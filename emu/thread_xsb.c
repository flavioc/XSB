#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#include "xsb_debug.h"
#include "xsb_config.h"

#include "basictypes.h"
#include "basicdefs.h"

#include "cell_xsb.h"
#include "register.h"
#include "context.h"
#include "cinterf.h"
#include "error_xsb.h"

#ifndef SYSTEM_FLAGS
#include "flags_xsb.h"
#endif

#include "flag_defs_xsb.h"
#include "deref.h"
#include "ptoc_tag_xsb_i.h"
#include "thread_xsb.h"
#include "rw_lock.h"

#ifdef MULTI_THREAD
#include <errno.h>

int emuloop(CTXTdeclc byte *startaddr);
void cleanup_machine(CTXTdecl);
void init_machine(CTXTdecl);
Cell copy_term_from_thread( th_context *th, th_context *from, Cell arg1 );


#ifdef WIN_NT
typedef pthread_t* pthread_t_p;
#else
typedef pthread_t pthread_t_p;
#endif

typedef struct
{	pthread_t_p	ptid;
	int		valid;
	int		detached ;
} xsb_thread_t ;

static xsb_thread_t th_vec[MAX_THREADS];
static xsb_thread_t *th_next = th_vec;

pthread_mutex_t sys_mut[MAX_SYS_MUTEXES] ;

pthread_mutex_t th_mutex = PTHREAD_MUTEX_INITIALIZER;

// pthread_t is a pointer in Unix, structure in Windows libraries
#ifdef WIN_NT
#define P_PTHREAD_T_P &tid
#define P_PTHREAD_T *tid
#else
#define P_PTHREAD_T_P tid
#define P_PTHREAD_T tid
#endif

static int th_find( pthread_t_p tid )
{
	xsb_thread_t *pos;

	for( pos = th_vec ; pos < th_next ; pos++ )
#ifdef WIN_NT
	  if( pos->valid && pthread_equal( P_PTHREAD_T, *(pos->ptid) ) )
			return pos - th_vec ;
#else
	  if( pos->valid && pthread_equal( P_PTHREAD_T, pos->ptid ) )
			return pos - th_vec ;
#endif
	return -1 ;
}

static int th_new( pthread_t_p t )
{
	xsb_thread_t *pos ;
	int i ;

	if( (i = th_find(t)) != -1 )
		return i ;

	for( pos = th_vec ; pos < th_vec + MAX_THREADS ; pos++ )
		if( !pos->valid ) 
			break;

	if( pos - th_vec >= MAX_THREADS )
		xsb_abort("to many threads");
	else if( pos == th_next )
		th_next++ ;

	pos->valid = 1;
	pos->ptid = t;
	pos->detached = 0;
	return pos - th_vec ;
}

static pthread_t_p th_get( int i )
{
	if( th_vec[i].valid )
		return th_vec[i].ptid ;
	else
		return (pthread_t_p)0 ;
}

static void th_delete( int i )
{
	th_vec[i].valid = 0;
}

void init_system_threads( void )
{
  pthread_t tid = pthread_self();
  th_new(P_PTHREAD_T_P) ;
}

void init_system_mutexes( void )
{
	int i ;
	pthread_mutexattr_t attr_rec ;
	pthread_mutexattr_t attr_std ;

/* make system mutex recursive, for there are recursive prolog calls	*/
/* to stuff that must be execute in mutual exclusion			*/

	pthread_mutexattr_init( &attr_rec ) ;
	if( pthread_mutexattr_settype( &attr_rec, PTHREAD_MUTEX_RECURSIVE_NP )<0 )
		xsb_abort( "error initializaing mutexes" ) ;

	pthread_mutexattr_init( &attr_std ) ;

	for( i = 0; i <=  LAST_REC_MUTEX ; i++ )
		pthread_mutex_init( &sys_mut[i], &attr_rec ) ;

	for( i = LAST_REC_MUTEX + 1 ; i < MAX_SYS_MUTEXES ; i++ )
		pthread_mutex_init( &sys_mut[i], &attr_std ) ;

	rw_lock_init(&trie_rw_lock);
}

static void *xsb_thread_run( void *arg )
{
        pthread_t tid;
	th_context *ctxt = (th_context *)arg ;

	pthread_mutex_lock( &th_mutex );
	tid = pthread_self();
	th_new( P_PTHREAD_T_P ) ;
	pthread_mutex_unlock( &th_mutex );
	emuloop( ctxt, get_ep((Psc)flags[THREAD_RUN]) ) ;

	/* execution shouldn't arrive here */
	xsb_bug( "emuloop returned from thread" );

	return NULL ;
}

static int xsb_thread_create(th_context *th)
{
	int rc ;
	Cell goal ;
	th_context *new_th ;
	pthread_t thr ;
	Integer id ;

	goal = ptoc_tag(th, 2) ;
	new_th = malloc(sizeof(th_context)) ;

	init_machine(new_th) ;
	new_th->_reg[1] = copy_term_from_thread(new_th, th, goal) ;

	flags[NUM_THREADS]++ ;

	rc = pthread_create( &thr, NULL, &xsb_thread_run, (void *)new_th ) ;

	pthread_mutex_lock( &th_mutex );
#ifdef WIN_NT
	id = th_new( &thr ) ;
#else
	id = th_new( thr ) ;
#endif
	pthread_mutex_unlock( &th_mutex );

	ctop_int( th, 3, id ) ;
	return rc ;
}

#endif /* MULTI_THREAD */

int xsb_thread_self()
{
#ifdef MULTI_THREAD
	int id;
        pthread_t tid = pthread_self();

        pthread_mutex_lock( &th_mutex );
        id = th_find( P_PTHREAD_T_P ) ;
        pthread_mutex_unlock( &th_mutex );
	return id;
#else
	return 0;
#endif
}

xsbBool xsb_thread_request( CTXTdecl ) 
{
	Integer request_num = ptoc_int(CTXTc 1) ;
#ifdef MULTI_THREAD
	Integer id, rval;
	pthread_t_p tid ;
	pthread_t tid2;
	int i;
	Integer rc ;
	xsbBool success = TRUE ;

	switch( request_num )
	{
		case XSB_THREAD_CREATE:
			rc = xsb_thread_create(th) ;
			break ;

		case XSB_THREAD_EXIT:
			rval = ptoc_int(CTXTc 2 ) ;
			cleanup_machine(CTXT) ;
			free( th ) ;
			flags[NUM_THREADS]-- ;
			pthread_mutex_lock( &th_mutex );
			tid2 = pthread_self();
#ifdef WIN_NT
			i = th_find( &tid2 ) ;
#else
			i = th_find( tid2 ) ;
#endif
			if( th_vec[i].detached )
				th_delete(i);
			pthread_mutex_unlock( &th_mutex );
			pthread_exit((void *) rval ) ;
			rc = 0 ; /* keep compiler happy */
			break ;

		case XSB_THREAD_JOIN:
			id = ptoc_int( CTXTc 2 ) ;
			pthread_mutex_lock( &th_mutex );
			tid = th_get( id ) ;
			pthread_mutex_unlock( &th_mutex );
			if( tid == (pthread_t_p)0 )
			        xsb_abort( "thread join - invalid thread id" );
			rc = pthread_join(P_PTHREAD_T, (void **)&rval ) ;
			pthread_mutex_lock( &th_mutex );
			th_delete(id);
			pthread_mutex_unlock( &th_mutex );
			ctop_int( CTXTc 3, rval ) ;
			break ;

		case XSB_THREAD_DETACH:
			id = ptoc_int( CTXTc 2 ) ;
			pthread_mutex_lock( &th_mutex );
			tid = th_get( id ) ;
			if( tid == (pthread_t_p)0 )
				xsb_abort( "thread detach - invalid thread id" );
			pthread_mutex_unlock( &th_mutex );
			rc = pthread_detach(P_PTHREAD_T) ;
			th_vec[id].detached = 1;
			break ;

		case XSB_THREAD_SELF:
			rc = id = xsb_thread_self() ;
			ctop_int( CTXTc 2, id ) ;
			break ;

		case XSB_MUTEX_INIT:
		{
			Integer arg = ptoc_int(CTXTc 2) ;
			pthread_mutexattr_t attr ;
			id = (Integer) malloc( sizeof(pthread_mutex_t) ) ;
        		pthread_mutexattr_init( &attr ) ;
			switch(arg)
			{
				case XSB_FAST_MUTEX:
        				pthread_mutexattr_settype( &attr, 
						PTHREAD_MUTEX_FAST_NP ) ;
					break ;
				case XSB_RECURSIVE_MUTEX:
        				pthread_mutexattr_settype( &attr, 
						PTHREAD_MUTEX_RECURSIVE_NP ) ;
					break ;
				case XSB_ERRORCHECK_MUTEX:
        				pthread_mutexattr_settype( &attr, 
						PTHREAD_MUTEX_ERRORCHECK_NP ) ;
					break ;
				default:
        				pthread_mutexattr_settype( &attr, 
						PTHREAD_MUTEX_FAST_NP ) ;
					break ;
			}
			rc = pthread_mutex_init( (pthread_mutex_t *)id, &attr ) ;
			ctop_int( CTXTc 3, id ) ;
			break ;
		}
		case XSB_MUTEX_LOCK:
			id = ptoc_int(CTXTc 2) ;
#ifdef DEBUG_MUTEXES
			fprintf( stddbg, "LOCK(%x)\n", id ) ;
#endif
			rc = pthread_mutex_lock( (pthread_mutex_t *)id ) ;
			break ;

		case XSB_MUTEX_TRYLOCK:
			id = ptoc_int(CTXTc 2) ;
			rc = pthread_mutex_trylock( (pthread_mutex_t *)id ) ;
			success = ( rc != EBUSY ) ;
			break ;

		case XSB_MUTEX_UNLOCK:
			id = ptoc_int(CTXTc 2) ;
#ifdef DEBUG_MUTEXES
			fprintf( stddbg, "UNLOCK(%x)\n", id ) ;
#endif
			rc = pthread_mutex_unlock( (pthread_mutex_t *)id ) ;
			break ;

		case XSB_MUTEX_DESTROY:
			id = ptoc_int(CTXTc 2) ;
			rc = pthread_mutex_destroy( (pthread_mutex_t *)id ) ;
			free( (pthread_mutex_t *)id ) ;
			break ;

		case XSB_SYS_MUTEX_LOCK:
			id = ptoc_int(CTXTc 2) ;
#ifdef DEBUG_MUTEXES
			fprintf( stddbg, "S LOCK(%ld)\n", (long)id ) ;
#endif
			rc = pthread_mutex_lock( &sys_mut[id] ) ;
#ifdef DEBUG_MUTEXES
			fprintf( stddbg, "RC=%ld\n", (long)rc ) ;
#endif
			break ;
		case XSB_SYS_MUTEX_UNLOCK:
			id = ptoc_int(CTXTc 2) ;
#ifdef DEBUG_MUTEXES
			fprintf( stddbg, "S UNLOCK(%ld)\n", (long)id ) ;
#endif
			rc = pthread_mutex_unlock( &sys_mut[id] ) ;
#ifdef DEBUG_MUTEXES
			fprintf( stddbg, "RC=%ld\n", (long)rc ) ;
#endif
			break ;

		case XSB_ENSURE_ONE_THREAD:
			ENSURE_ONE_THREAD() ;
			rc = 0 ;
			break ;

		default:
			rc = 0 ; /* Keep compiler happy */
			xsb_abort( "invalid thread operation requested" );
			break ;
	}
	ctop_int( CTXTc 5, rc ) ;
	return success ;
#else
        switch( request_num )
        {
                
		case XSB_THREAD_SELF:
			ctop_int( CTXTc 2, xsb_thread_self() ) ;
			break;
                case XSB_SYS_MUTEX_LOCK:
                case XSB_SYS_MUTEX_UNLOCK:
			break ;
		case XSB_ENSURE_ONE_THREAD:
			break ;
		default:
			xsb_abort( "thread primitives not compiled" ) ;
			break ;
	}
	
	ctop_int( CTXTc 5, 0 ) ;
	return TRUE ;
#endif
}

xsbBool mt_random_request( CTXTdecl )
{

  Integer request_num = ptoc_int(CTXTc 1) ;

  switch( request_num )
    {
    case INIT_MT_RANDOM:
     SRANDOM_CALL(time(0)); 
      break;

    case MT_RANDOM:
            ctop_int(CTXTc 2,RANDOM_CALL());
      break;

    case MT_RANDOM_INTERVAL:
      {
	UInteger rval;
	UInteger scale = ptoc_int(CTXTc 2);
	UInteger interval = ((unsigned long) pow(2,32) - 1) / scale;
	printf("max %lx\n",((unsigned long) pow(2,32)-1));
	printf("int %x scale %x s1 %d ex %x\n",
	       interval,scale,scale,16);
	rval = RANDOM_CALL(); 
	printf("rval %x \n",rval);
	ctop_int(CTXTc 3,floor(rval / interval));
	break;
      }

    default: 
      xsb_abort( "Improper case for mt_rand" ) ;
    }
  return TRUE ;
}

