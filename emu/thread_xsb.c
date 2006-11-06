/* File:      thread_xsb.c
** Author(s): Marques
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
** 
** XSB is free software; you can redistribute it and/or modify it under the
** terms of the GNU Library General Public License as published by the Free
** Software Foundation; either version 2 of the License, or (at your option)
** any later version.
** 
** XSB is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
** FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
** more details.
** 
** You should have received a copy of the GNU Library General Public License
** along with XSB; if not, write to the Free Software Foundation,
** Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
**
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <signal.h>

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

#include "io_defs_xsb.h"
#include "io_builtins_xsb.h"
#include "token_xsb.h"
#include "flag_defs_xsb.h"
#include "deref.h"
#include "ptoc_tag_xsb_i.h"
#include "thread_xsb.h"
#include "rw_lock.h"
#include "memory_xsb.h"
#include "sig_xsb.h"

#ifdef MULTI_THREAD

/* different things are included based on context.h -- easiest to
   include this conditionally. */
#include "macro_xsb.h"

#include <errno.h>

int emuloop(CTXTdeclc byte *startaddr);
void cleanup_thread_structures(CTXTdecl);
void init_machine(CTXTdeclc int, int, int, int);
void set_init_glstack_size(int);
void set_init_tcpstack_size(int);
void set_init_pdl_size(int);
void set_init_complstack_size(int);
Cell copy_term_from_thread( th_context *th, th_context *from, Cell arg1 );

/* Used to create detached thread -- process global. */
pthread_attr_t detached_attr_gl;
/* Used to create thread with reasonable stack size -- process global. */
pthread_attr_t normal_attr_gl;


pthread_mutexattr_t attr_rec_gl ;

typedef struct
{	
	pthread_t	tid;
#ifdef WIN_NT
	pthread_t *	tid_addr;
#endif
	int		valid;
	int		detached ;
	th_context *	ctxt ;
} xsb_thread_t ;

static xsb_thread_t th_vec[MAX_THREADS];
static xsb_thread_t *th_next = th_vec;

extern void release_private_dynamic_resources(CTXTdecl);
extern void release_private_tabling_resources(CTXTdecl);

//extern void thread_free_dyn_blks(CTXTdecl);
//extern void thread_free_tab_blks(CTXTdecl);
extern void delete_predicate_table(CTXTdeclc TIFptr);

MutexFrame sys_mut[MAX_SYS_MUTEXES] ;

pthread_mutex_t th_mutex = PTHREAD_MUTEX_INITIALIZER;

pthread_mutex_t completing_mut;
pthread_cond_t completing_cond;

// pthread_t is a pointer in Unix, structure in Windows libraries
#ifdef WIN_NT
#define P_PTHREAD_T_P &tid
#define P_PTHREAD_T *tid
#else
#define P_PTHREAD_T_P tid
#define P_PTHREAD_T tid
#endif

char *mutex_names[] = {
"mutex_dynamic","mutex_io","mutex_table","mutex_trie","mutex_symbol",
"mutex_flags"," mutex_load_undef","mutex_delay","mutex_sys_system","unused",
"unused","unused","unused","unused","unused",
"mutex_string","mutex_atom_buf","mutex_sm","mutex_stacks","mutex_sockets",
"mutex_mem","mutex_odbc","mutex_gentag","mutex_dispbkhdr","unused",
"unused","unused","unused","unused","unused",
"mutex_console","mutex_user1","mutex_user2","mutex_user3","mutex_user4",
"mutex_user5","mutex_user6","mutex_user7","mutex_user9","mutex_user9"};


/*-------------------------------------------------------------------------*/
/* General Routines */

th_context *find_context( int id )
{
	if (th_vec[id].valid)
		return th_vec[id].ctxt;
	else
		return NULL;
}

/*-------------------------------------------------------------------------*/
/* Thread Creation, Destruction, etc. */

/* finds thread in the thread-vector, returning its index if found, -1
   otherwise */
static int th_find( pthread_t_p tid )
{
	xsb_thread_t *pos;

	for( pos = th_vec ; pos < th_next ; pos++ )
	  if( pos->valid && pthread_equal( P_PTHREAD_T, pos->tid ) )
			return pos - th_vec ;
	return -1 ;
}

/* On normal termination, returns xsb_thread_id for a (usu. newly
   created) thread */
static int th_new( pthread_t_p t, th_context *ctxt )
{
	xsb_thread_t *pos ;
	int i ;
	if( (i = th_find(t)) != -1 )
		return i ;
	for( pos = th_vec ; pos < th_vec + MAX_THREADS ; pos++ )
		if( !pos->valid ) 
			break;

	if( pos - th_vec >= MAX_THREADS )
		xsb_abort("[THREAD] Too many threads");
	else if( pos == th_next )
		th_next++ ;

	pos->ctxt = ctxt ;
	pos->valid = 1;
#ifdef WIN_NT
	pos->tid = *t;
	pos->tid_addr = t;
#else
	pos->tid = t;
#endif
	pos->detached = 0;
	return pos - th_vec ;
}

static pthread_t_p th_get( int i )
{
	if( th_vec[i].valid )
#ifdef WIN_NT
	  return th_vec[i].tid_addr;
#else
	  return th_vec[i].tid ;
#endif
	else
	  return (pthread_t_p)0 ;
}

static void th_delete( int i )
{
#ifdef WIN_NT
	mem_dealloc(th_vec[i].tid_addr,sizeof(pthread_t),THREAD_SPACE);
#endif	
	th_vec[i].valid = 0;
}

/* calls _$thread_run/1 in thread.P */
static void *xsb_thread_run( void *arg )
{
        pthread_t tid;
	th_context *ctxt = (th_context *)arg ;

	pthread_mutex_lock( &th_mutex );
	tid = pthread_self();
/* if the xsb thread id was just created we need to re-initialize it on the
   thread context */
	ctxt->tid = th_new( P_PTHREAD_T_P, ctxt ) ;
	pthread_mutex_unlock( &th_mutex );
	emuloop( ctxt, get_ep((Psc)flags[THREAD_RUN]) ) ;

	/* execution shouldn't arrive here */
	xsb_bug( "emuloop returned from thread" );

	return NULL ;
}

static void copy_pflags( th_context *to, th_context *from )
{
	int i ;

	for( i = 0; i < MAX_PRIVATE_FLAGS; i++ )
		to->_pflags[i] = from->_pflags[i] ;
}

static int xsb_thread_create(th_context *th)
{
  int rc, is_detached ;
  Cell goal ;
  th_context *new_th_ctxt ;
  pthread_t_p thr ;
  Integer id ;
       
  goal = ptoc_tag(th, 2) ;
  new_th_ctxt = mem_alloc(sizeof(th_context),THREAD_SPACE) ;

  copy_pflags(new_th_ctxt, th) ;
  init_machine(new_th_ctxt,ptoc_int(CTXTc 4),ptoc_int(CTXTc 5),
	       ptoc_int(CTXTc 6),ptoc_int(CTXTc 7)) ;
  new_th_ctxt->_reg[1] = copy_term_from_thread(new_th_ctxt, th, goal) ;

  flags[NUM_THREADS]++ ;

  is_detached = ptoc_int(CTXTc 8);

#ifdef WIN_NT
  thr = mem_alloc(sizeof(pthread_t),THREAD_SPACE);
  if (is_detached) { /* set detached */
    rc = pthread_create(thr, &detached_attr_gl, &xsb_thread_run, 
			 (void *)new_th_ctxt ) ;
  }
  else {
    rc = pthread_create(thr, &normal_attr_gl, &xsb_thread_run, (void *)new_th_ctxt ) ;
  }

#else

  if (is_detached) { /* set detached */
    rc = pthread_create( &thr, &detached_attr_gl, &xsb_thread_run, 
			 (void *)new_th_ctxt ) ;
  }
  else {
    rc = pthread_create( &thr, &normal_attr_gl, &xsb_thread_run, (void *)new_th_ctxt ) ;
  }

#endif

  if (rc == EAGAIN) {
    xsb_resource_error(th,"system threads","xsb_thread_create",2);
  } else {
    if (rc != 0) 
      xsb_abort("Failure to create thread: error %d\n",rc);
  }

/* This repetition of the call to th_new is need for concurrency reasons */
  pthread_mutex_lock( &th_mutex );
  id = th_new( thr, new_th_ctxt ) ;

  if (is_detached) th_vec[id].detached = 1;

  pthread_mutex_unlock( &th_mutex );

  ctop_int( th, 3, id ) ;
  return rc ;
}  /* xsb_thread_create */

/*-------------------------------------------------------------------------*/
/* System Initialization Stuff */

void init_system_threads( th_context *ctxt )
{
  pthread_t tid = pthread_self();
  th_new(P_PTHREAD_T_P, ctxt) ;
}

/* * * * * * */
void init_system_mutexes( void )
{
	int i ;
	//	pthread_mutexattr_t attr_rec ;
	pthread_mutexattr_t attr_std ;

/* make system mutex recursive, for there are recursive prolog calls	*/
/* to stuff that must be executed in mutual exclusion			*/

	pthread_mutexattr_init( &attr_rec_gl ) ;
	if( pthread_mutexattr_settype( &attr_rec_gl,
				       PTHREAD_MUTEX_RECURSIVE_NP )<0 )
		xsb_abort( "[THREAD] Error initializing mutexes" ) ;

	pthread_mutexattr_init( &attr_std ) ;

	for( i = 0; i <=  LAST_REC_MUTEX ; i++ ) {
	  pthread_mutex_init( MUTARRAY_MUTEX(i), &attr_rec_gl ) ;
	  MUTARRAY_OWNER(i) = -1;
	}
	for( i = LAST_REC_MUTEX + 1 ; i < MAX_SYS_MUTEXES ; i++ ) {
	  pthread_mutex_init( MUTARRAY_MUTEX(i), &attr_std ) ;
	  MUTARRAY_OWNER(i) = -1;
	}

#ifdef MULTI_THREAD_RWL
	rw_lock_init(&trie_rw_lock);
#endif

	pthread_mutex_init( &completing_mut, &attr_std );
	pthread_cond_init( &completing_cond, NULL );
}

/*-------------------------------------------------------------------------*/
/* Routines for dynamic user mutexes. It may at first seem strange to
   have MUTEX_DYNMUT guard the creation and deletion of mutexes, but
   since the mutexes are held in a doubly linked list that is
   traversed by various routines, we have to be careful.  However
   mutex handling that affects a single user mutex (locking,
   unlocking) does not need to synchronize with MUTEX_DYNMUT.  */

/* Used only for sys mutexes -- I may fold this into mutex statistics
   at some point.  */
void print_mutex_use() {
  int i;

  printf("Mutexes used since last statistics:\n");
  for (i = 0; i < MAX_SYS_MUTEXES; i++) {
    if (sys_mut[i].num_locks > 0) 
      printf("Mutex %s (%d): %d\n",mutex_names[i],i,sys_mut[i].num_locks);
  }
  for (i = 0; i < MAX_SYS_MUTEXES; i++) {
    sys_mut[i].num_locks = 0;
  }
}

DynMutPtr dynmut_chain_begin = NULL;

DynMutPtr create_new_dynMutFrame() {
  DynMutPtr new_dynmut = mem_alloc(sizeof(DynMutexFrame),THREAD_SPACE) ;
  pthread_mutex_init( &(new_dynmut->th_mutex), &attr_rec_gl ) ;
  new_dynmut->num_locks = 0;
  new_dynmut->owner = -1;
  if (dynmut_chain_begin != NULL) 
    (*dynmut_chain_begin).prev_dynmut = new_dynmut;
  new_dynmut->next_dynmut = dynmut_chain_begin;
  new_dynmut->prev_dynmut = NULL;
  dynmut_chain_begin = new_dynmut;
  return new_dynmut;
}

void delete_dynMutFrame(DynMutPtr old_dynmut) {
  if (old_dynmut->prev_dynmut != NULL)
    (old_dynmut->prev_dynmut)->next_dynmut = old_dynmut->next_dynmut;
  if (old_dynmut->next_dynmut != NULL)
    (old_dynmut->next_dynmut)->prev_dynmut = old_dynmut->prev_dynmut;
  if (dynmut_chain_begin == old_dynmut)
    dynmut_chain_begin = old_dynmut->next_dynmut;
  mem_dealloc(old_dynmut,sizeof(DynMutexFrame),THREAD_SPACE) ;
}
  
/* Need to make sure that owner is not over-written falsely.  This one
   is for user mutexes.*/
void unlock_mutex(CTXTdeclc DynMutPtr id) {

  Integer rc ;

  if ( id->owner == xsb_thread_id) 
    id->owner = -1;
  
  rc = pthread_mutex_unlock( &(id->th_mutex) ) ;
  if (rc == EINVAL) {
    xsb_permission_error(CTXTc "unlock mutex","invalid mutex",
			 xsb_thread_id,"xsb_mutex_unlock",2); 
  } else if (rc == EPERM) { 
    xsb_permission_error(CTXTc "unlock mutex",
			 "mutex not held by thread",
			 xsb_thread_id,"xsb_mutex_unlock",2); 
  } 
}

/* This just unlocks the user mutexes held by a thread -- the name
   comes from the ISO document. */
void mutex_unlock_all(CTXTdecl) {
  DynMutPtr dmp = dynmut_chain_begin;
  while (dmp != NULL) {
    if (dmp-> owner == xsb_thread_id) {
      printf("unlocking %p\n",dmp);
      unlock_mutex(CTXTc dmp);
    }
    dmp = dmp->next_dynmut;
  }
}

/* Unlocks all system and user mutexes held by a thread */
void release_held_mutexes(CTXTdecl) {
  int i;

  //  printf("releasing held mutexes\n");
  for( i = 0; i <=  LAST_REC_MUTEX ; i++ ) {
    if ( MUTARRAY_OWNER(i) == xsb_thread_id) {
      pthread_mutex_unlock( MUTARRAY_MUTEX(i)) ;
    }
  }
  for( i = LAST_REC_MUTEX + 1 ; i < MAX_SYS_MUTEXES ; i++ ) {
    if ( MUTARRAY_OWNER(i) == xsb_thread_id) {
      pthread_mutex_unlock( MUTARRAY_MUTEX(i)) ;
    }
    pthread_mutex_unlock( MUTARRAY_MUTEX(i)) ;
  }
  for( i = 0; i < MAX_OPEN_FILES; i++ )
	if( OPENFILES_MUTEX_OWNER(i) == xsb_thread_id )
		pthread_mutex_unlock(OPENFILES_MUTEX(i));

  mutex_unlock_all(CTXT);
}

void close_str(CTXTdecl)
{
  int i;
  for( i = 0; i < MAXIOSTRS; i++ )
	if( iostrs[i] && iostrs[i]->owner == xsb_thread_id )
		strclose( iostrdecode(i) ) ;
}


#else /* Not MULTI_THREAD */

void print_mutex_use() {
  xsb_abort("This engine is not configured for mutex profiling.");
}

#endif /* MULTI_THREAD */

/* TLS: should probably rewrite and move. */
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

/*-------------------------------------------------------------------------*/
/* Thread Requests  */

extern void release_private_tabling_resources(CTXTdecl);
extern void abolish_private_tables(CTXTdecl);
extern void abolish_shared_tables(CTXTdecl);

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

	  /* TLS: replaced thread_free_tab_blks() by
	     thread_free_private_tabling_resources, which sets
	     appropriate tifs to 0, but doesn't use
	     delete_predicate_table -- rather it deallocates the
	     structure managers directly.  */

	case XSB_THREAD_EXIT:
	  rval = ptoc_int(CTXTc 2 ) ;
	  release_held_mutexes(CTXT);
	  release_private_tabling_resources(CTXT);
	  release_private_dynamic_resources(CTXT);
	  close_str(CTXT) ;
	  cleanup_thread_structures(CTXT) ;
	  mem_dealloc(th,sizeof(th_context),THREAD_SPACE) ;
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

	case XSB_THREAD_JOIN: {
	  id = ptoc_int( CTXTc 2 ) ;
	  pthread_mutex_lock( &th_mutex );
	  tid = th_get( id ) ;
	  pthread_mutex_unlock( &th_mutex );
	  if( tid == (pthread_t_p)0 )
	    xsb_existence_error(CTXTc "thread",reg[2],"xsb_thread_join",1,1); 
	  rc = pthread_join(P_PTHREAD_T, (void **)&rval ) ;
	  if (rc != 0) {
	    if (rc == EINVAL) { /* pthread found, but not joinable */
	      xsb_permission_error(CTXTc "thread_join","non-joinable thread",
				   reg[2],"xsb_thread_join",1); 
	    } else {
	      if (rc == ESRCH)  { /* no such pthread found */
		xsb_existence_error(CTXTc "thread",reg[2],
				    "xsb_thread_join",1,1); 
	      }
	    }
	  }

	  pthread_mutex_lock( &th_mutex );
	  th_delete(id);
	  pthread_mutex_unlock( &th_mutex );
	  ctop_int( CTXTc 3, rval ) ;
	  break ;
	}

	case XSB_THREAD_DETACH:
	  id = ptoc_int( CTXTc 2 ) ;
	  pthread_mutex_lock( &th_mutex );
	  tid = th_get( id ) ;
	  if( tid == (pthread_t_p)0 )
	    xsb_abort( "[THREAD] Thread detach - invalid thread id" );
	  pthread_mutex_unlock( &th_mutex );
	  rc = PTHREAD_DETACH( tid ) ;
	  if (rc == EINVAL) { /* pthread found, but not joinable */
	    xsb_permission_error(CTXTc "thread_detach","non-joinable thread",
				 reg[2],"xsb_thread_detach",1); 
	  } else {
	    if (rc == ESRCH)  { /* no such pthread found */
	      xsb_existence_error(CTXTc "thread",reg[2],
				  "xsb_thread_detach",1,1); 
	    }
	  }
	  th_vec[id].detached = 1;
	  break ;

       case XSB_THREAD_SELF:
	 rc = id = xsb_thread_self() ;
	 ctop_int( CTXTc 2, id ) ;
	 break ;

 	case XSB_MUTEX_INIT:		
	  ctop_int(CTXTc 2, (prolog_int) create_new_dynMutFrame());
	  break;

 /* TLS: obsolete old form
| 	case XSB_MUTEX_INIT:		{
| 	  Integer arg = ptoc_int(CTXTc 2) ;
| 	  pthread_mutexattr_t attr ;
| 	  id = (Integer) mem_alloc( sizeof(pthread_mutex_t),THREAD_SPACE ) ;
| 	  pthread_mutexattr_init( &attr ) ;
| 	  switch(arg)
| 	    {
| 	    case XSB_FAST_MUTEX:
| 	      pthread_mutexattr_settype( &attr, 
| 					 PTHREAD_MUTEX_FAST_NP ) ;
| 	      break ;
| 	    case XSB_RECURSIVE_MUTEX:
| 	      pthread_mutexattr_settype( &attr, 
| 					 PTHREAD_MUTEX_RECURSIVE_NP ) ;
| 	      break ;
| 	    case XSB_ERRORCHECK_MUTEX:
| 	      pthread_mutexattr_settype( &attr, 
| 					 PTHREAD_MUTEX_ERRORCHECK_NP ) ;
| 	      break ;
| 	    default:
| 	      pthread_mutexattr_settype( &attr, 
| 					 PTHREAD_MUTEX_FAST_NP ) ;
| 	      break ;
| 	    }
| 	  rc = pthread_mutex_init( (pthread_mutex_t *)id, &attr ) ;
| 	  if (rc == ENOMEM) {
| 	    xsb_resource_error(th,"memory","xsb_mutex_init",2);
| 	  }
| 	  break ;
| 	}
	  */
	case XSB_MUTEX_LOCK: {
	  DynMutPtr id = (DynMutPtr) ptoc_int(CTXTc 2) ;
	  rc = pthread_mutex_lock( &(id->th_mutex) ) ;
	  id->num_locks++;
	  id->owner = xsb_thread_id;
	  if (rc == EINVAL) {
	    xsb_permission_error(CTXTc "lock mutex","invalid mutex",
				 reg[2],"xsb_mutex_lock",2); 
	  } else if (rc == EDEADLK) { 
	    xsb_permission_error(CTXTc "lock mutex","deadlocking mutex",
				 reg[2],"xsb_mutex_lock",2); 
	  } 
	  break ;
	}

	case XSB_MUTEX_TRYLOCK: {
	  DynMutPtr id = (DynMutPtr) ptoc_int(CTXTc 2) ;
	  rc = pthread_mutex_trylock( &(id->th_mutex) ) ;
	  if (rc == EINVAL) {
	    xsb_permission_error(CTXTc "lock mutex","invalid mutex",
				 reg[2],"xsb_mutex_lock",2); 
	  } else success = ( rc != EBUSY ) ;
	  break ;
	}

	case XSB_MUTEX_UNLOCK: {
	  DynMutPtr id = (DynMutPtr) ptoc_int(CTXTc 2) ;
	  unlock_mutex(CTXTc id);
	  break ;
	}

	case XSB_MUTEX_DESTROY: {
	  DynMutPtr id = (DynMutPtr) ptoc_int(CTXTc 2) ;
	  rc = pthread_mutex_destroy( &(id->th_mutex) ) ;
	  if (rc == EINVAL) {
	    xsb_permission_error(CTXTc "destroy mutex","invalid mutex",
				 reg[2],"xsb_mutex_destroy",1); 
	  } else {
	    if (rc == EBUSY) { 
	      xsb_permission_error(CTXTc "destroy mutex","busy mutex",
				   reg[2],"xsb_mutex_destroy",1); 
	    } else 
	      delete_dynMutFrame(id);
	  }
	  break ;
	}
	case XSB_SYS_MUTEX_LOCK:
	  id = ptoc_int(CTXTc 2) ;
#ifdef DEBUG_MUTEXES
	  fprintf( stddbg, "S LOCK(%ld)\n", (long)id ) ;
#endif
	  rc = pthread_mutex_lock( MUTARRAY_MUTEX(id) ) ;
#ifdef DEBUG_MUTEXES
	  fprintf( stddbg, "RC=%ld\n", (long)rc ) ;
#endif
	  break ;
	case XSB_SYS_MUTEX_UNLOCK:
	  id = ptoc_int(CTXTc 2) ;
	  rc = pthread_mutex_unlock( MUTARRAY_MUTEX(id) ) ;
	  break ;

	case XSB_ENSURE_ONE_THREAD:
	  ENSURE_ONE_THREAD() ;
	  rc = 0 ;
	  break ;

	  /*TLS: I should make the configuration check for existence
	    of sched_yield somehow. */
	case XSB_THREAD_YIELD:
#if !defined(SOLARIS)
	  rc = sched_yield();
	  if (rc == ENOSYS) /* Need support for POSIX 1b for this */
#endif
	    xsb_abort("Real-time extensions not supported on this platform");
	  break;

	case XSB_SHOW_MUTEXES: 
	  printf("mutex owners\n");
	  for (i = 0; i < MAX_SYS_MUTEXES; i++) {
	    if (sys_mut[i].owner > 0) 
	      printf("Mutex %s (%d): %d\n",mutex_names[i],i,sys_mut[i].owner);
	  }
	  rc = 0;
	  break;

	case XSB_SET_INIT_GLSTACK_SIZE:
	  i = ptoc_int(CTXTc 2) ;
	  set_init_glstack_size(i);
	  rc = 0;
	  break;
	case XSB_SET_INIT_TCPSTACK_SIZE:
	  i = ptoc_int(CTXTc 2) ;
	  set_init_tcpstack_size(i);
	  rc = 0;
	  break;
	case XSB_SET_INIT_PDL_SIZE:
	  i = ptoc_int(CTXTc 2) ;
	  set_init_pdl_size(i);
	  rc = 0;
	  break;
	case XSB_SET_INIT_COMPLSTACK_SIZE:
	  i = ptoc_int(CTXTc 2) ;
	  set_init_complstack_size(i);
	  rc = 0;
	  break;

	  /* TLS: may generalize -- right now, just detached/joinable */
	case XSB_THREAD_PROPERTY: 
	  ctop_int(CTXTc 3, th_vec[ ptoc_int(CTXTc 2) ].detached);
	  break;

	  /* for now, one interrupt, but possibly we should allow
	     users to define others  */
	case XSB_THREAD_INTERRUPT: {
	  th_context *	ctxt_ptr ;

	  i = ptoc_int(CTXTc 2);
	  if (th_vec[i].valid) {
	    ctxt_ptr = th_vec[i].ctxt;
	    ctxt_ptr->_asynint_val |= THREADINT_MARK;
#ifdef WIN_NT
	    PTHREAD_KILL( th_vec[i].tid_addr, SIGINT );
#else
	    PTHREAD_KILL( th_vec[i].tid, SIGINT );
#endif
	  } else {
	    bld_int(reg+2,i);
	    xsb_permission_error(CTXTc "thread_interrupt","invalid_thread",
				   reg[2],"xsb_thread_interrupt",1); 
	  }
	  break;
	}

	case ABOLISH_PRIVATE_TABLES: {
	  abolish_private_tables(CTXT);
	  break;
	}

	case ABOLISH_SHARED_TABLES: {
	  abolish_shared_tables(CTXT);
	  break;
	}

	case GET_FIRST_MUTEX_PROPERTY: {
	  if (dynmut_chain_begin != NULL) {
	    ctop_int(CTXTc 2 , (prolog_int) dynmut_chain_begin);
	    ctop_int(CTXTc 3 , (*dynmut_chain_begin).num_locks);
	    ctop_int(CTXTc 4 , (*dynmut_chain_begin).owner);
	    ctop_int(CTXTc 5 , (prolog_int) (*dynmut_chain_begin).next_dynmut);
	  }
	  else {
	    ctop_int(CTXTc 2 , 0);
	    ctop_int(CTXTc 3 , 0);
	    ctop_int(CTXTc 4 , 0);
	    ctop_int(CTXTc 5 , 0);
	  }
	  break;
	}

	case GET_NEXT_MUTEX_PROPERTY: {
	  DynMutPtr dmp = (DynMutPtr) ptoc_int(CTXTc 2);
	  ctop_int(CTXTc 3 , (*dmp).num_locks);
	  ctop_int(CTXTc 4 , (*dmp).owner);
	  ctop_int(CTXTc 5 , (prolog_int) (*dmp).next_dynmut);
	  break;
	}

	case MUTEX_UNLOCK_ALL: 
	  mutex_unlock_all(CTXT);
	  break;

	default:
	  rc = 0 ; /* Keep compiler happy */
	  xsb_abort( "[THREAD] Invalid thread operation requested %d",request_num);
	  break ;
	}
	//	ctop_int( CTXTc 5, rc ) ;
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
		case XSB_THREAD_YIELD:
	       		break ;
		default:
			xsb_abort( "[THREAD] Thread primitives not compiled" ) ;
			break ;
	}
	
	ctop_int( CTXTc 5, 0 ) ;
	return TRUE ;
#endif /* MULTI_THREAD */
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
	//	printf("max %lx\n",((unsigned long) pow(2,32)-1));
	//	printf("int %x scale %x s1 %d ex %x\n", interval,scale,scale,16);
	rval = RANDOM_CALL(); 
	//	printf("rval %x \n",rval);
	ctop_int(CTXTc 3,(Integer)floor(rval / interval));
	break;
      }

    default: 
      xsb_abort( "[THREAD] Improper case for mt_rand" ) ;
    }
  return TRUE ;
}

/*
static void show_policy(void) {
  int my_policy;
  struct sched_param my_param;
  int status, min_priority, max_priority;

  if ((status = pthread_getschedparam(pthread_self(),&my_policy,&my_param))) {
    xsb_abort("bad scheduling status");
  }
  printf("thread routine running at %s/%d\n",
	 (my_policy == SCHED_FIFO ? "FIFO" 
	  : (my_policy == SCHED_RR ? "RR"
	     : (my_policy == SCHED_OTHER ? "OTHER"
		: "unknown") )),my_param.sched_priority);

  printf("min %d max %d\n",sched_get_priority_min(my_policy),
	 sched_get_priority_max(my_policy));
  
}
*/

