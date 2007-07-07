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
#include <string.h>

#include "xsb_debug.h"
#include "xsb_config.h"

#ifndef WIN_NT
#include <unistd.h>
#endif


#include "basictypes.h"
#include "basicdefs.h"
#include "auxlry.h"

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
Cell copy_term_from_thread( th_context *th, th_context *from, Cell arg1 );
int copy_term_to_message_queue(th_context *th, Cell arg1);

/* Used to create detached thread -- process global. */
pthread_attr_t detached_attr_gl;
/* Used to create thread with reasonable stack size -- process global. */
pthread_attr_t normal_attr_gl;

pthread_mutexattr_t attr_rec_gl ;

typedef struct xsb_thread_s
{	
	pthread_t		tid;
	struct xsb_thread_s	*next_entry,	/* either next free slot or next thread */
				*prev_entry ;	/* only valid for slots used for threads */
	unsigned int		incarn : 12;
	unsigned int		valid : 1;
	unsigned int		detached : 1;
	unsigned int		exited : 1;
	unsigned int		status : 3;
        unsigned int            aliased : 1;
	th_context *		ctxt ;
} xsb_thread_t ;

#define VALID_THREAD(tid)	( tid >= 0 &&\
				th_vec[THREAD_ENTRY(tid)].incarn == THREAD_INCARN(tid)\
				&& th_vec[THREAD_ENTRY(tid)].valid )

static xsb_thread_t *th_vec;
static xsb_thread_t *th_first_free, *th_last_free, *th_first_thread;

extern void findall_clean_all(CTXTdecl);
extern void release_private_dynamic_resources(CTXTdecl);
extern void release_private_tabling_resources(CTXTdecl);

//extern void thread_free_dyn_blks(CTXTdecl);
//extern void thread_free_tab_blks(CTXTdecl);
extern void delete_predicate_table(CTXTdeclc TIFptr);

MutexFrame sys_mut[MAX_SYS_MUTEXES] ;

pthread_mutex_t th_mutex = PTHREAD_MUTEX_INITIALIZER;

pthread_mutex_t completing_mut;
pthread_cond_t completing_cond;

counter max_threads_sofar;

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
"mutex_flags"," mutex_load_undef","mutex_delay","mutex_sys_system",
"unused", "unused","unused","unused",
"mutex_cons_list", "mutex_compl", 
"mutex_string","mutex_call_trie","mutex_sm","mutex_threads", "mutex_sockets",
"mutex_mem","mutex_odbc","mutex_gentag","mutex_dispbkhdr","unused",
"unused","unused","unused","unused","unused",
"mutex_console","mutex_user1","mutex_user2","mutex_user3","mutex_user4",
"mutex_user5","mutex_user6","mutex_user7","mutex_user9","mutex_user9"};


/*-------------------------------------------------------------------------*/
/* General Routines */

int thread_exited(int tid) {
  return th_vec[tid].exited;
}

th_context *find_context( int id )
{
	if ( th_vec[THREAD_ENTRY(id)].incarn == THREAD_INCARN(id) )
		return th_vec[THREAD_ENTRY(id)].ctxt;
	else
		return NULL;
}

int valid_tid( int t )
{
	return VALID_THREAD(t) ;
}

#ifdef SHARED_COMPL_TABLES
int get_waiting_for_tid( int t )
{
	int wtid;
	th_context *ctxt ;

	pthread_mutex_lock( &th_mutex ) ;
        SYS_MUTEX_INCR( MUTEX_THREADS ) ;
	if( !VALID_THREAD(t) )
		ctxt = NULL;
	else
		ctxt = th_vec[THREAD_ENTRY(t)].ctxt ;
	if( ctxt )
		wtid = ctxt->waiting_for_tid ;
	else
		wtid = -1 ;
	pthread_mutex_unlock( &th_mutex ) ;

	return wtid ;
}
#endif

static void init_thread_table(void)
{
	int i ;

	th_vec = mem_calloc(max_threads_glc, sizeof(xsb_thread_t), OTHER_SPACE);

	for( i = 0; i < max_threads_glc; i++ )
	{
		th_vec[i].incarn = INC_MASK_RIGHT;	/* Effectively -1 */
		th_vec[i].valid = FALSE;
		th_vec[i].next_entry = &th_vec[i+1];
		th_vec[i].ctxt = NULL;
	}
	th_first_free = &th_vec[0];
	th_last_free = &th_vec[max_threads_glc-1];
	th_last_free->next_entry = NULL;
	th_first_thread = NULL;
}

/*-------------------------------------------------------------------------*/
/* Thread Creation, Destruction, etc. */

/* finds thread in the thread-vector, returning its index if found, -1
   otherwise */
static int th_find( pthread_t_p tid )
{
	xsb_thread_t *pos;

	pos = th_first_thread;

	while( pos )
		if( pthread_equal( P_PTHREAD_T, pos->tid ) )
			return pos - th_vec ;
		else
			pos = pos->next_entry;

	return -1 ;
}

/* On normal termination, returns xsb_thread_id for a (usu. newly
   created) thread.

   Need to ensure this is called with thread mutex locked (except for
   initialization)*/
static int th_new( th_context *ctxt, int is_detached, int is_aliased )
{
	xsb_thread_t *pos ;

	/* get entry from free list */
	if( !th_first_free )
		return -1;

	pos = th_first_free ;
	th_first_free = th_first_free->next_entry ;

	/* add new entry to thread list */
	/* keep it ordered in the same way as the table */
	if ( th_first_thread == NULL || th_first_thread > pos )
	{	/* insert at head */
		if( th_first_thread != NULL )
			th_first_thread->prev_entry = pos ;
		pos->next_entry = th_first_thread ;
		th_first_thread = pos ;
		pos->prev_entry = NULL ;
	}
	else
	{	xsb_thread_t *p;

		p = th_first_thread ;
		/* if p->next_entry == NULL the cycle stops */
		while( pos < p->next_entry )
			p = p->next_entry ;

		/* p->next_entry is where we want to insert the entry */
		pos->prev_entry = p;
		pos->next_entry = p->next_entry;
		p->next_entry = pos;
		if( pos->next_entry != NULL )
			pos->next_entry->prev_entry = pos;
	}
	pos->ctxt = ctxt ;
	pos->incarn = (pos->incarn+1) & INC_MASK_RIGHT;
	pos->detached = is_detached;
	pos->exited = FALSE;
	pos->valid = FALSE;
	pos->aliased = is_aliased;
	pos->status = THREAD_RUNNING;
	return pos - th_vec ;
}

static pthread_t_p th_get( int i )
{
	int pos;
	unsigned int incarn ;

	if( i < 0 )
		return (pthread_t_p)0 ;

	pos    = THREAD_ENTRY(i) ;
	incarn = THREAD_INCARN(i) ;

	if( th_vec[pos].incarn == incarn && th_vec[pos].valid )
#ifdef WIN_NT
	  return &th_vec[pos].tid;
#else
	  return th_vec[pos].tid ;
#endif
	else
	  return (pthread_t_p)0 ;
}

static void th_delete( int i )
{
	/* delete from thread list */
	if( th_vec[i].prev_entry != NULL )
		th_vec[i].prev_entry->next_entry = th_vec[i].next_entry ;
	if( th_vec[i].next_entry != NULL )
		th_vec[i].next_entry->prev_entry = th_vec[i].prev_entry ;
	if( th_first_thread == &th_vec[i] )
		th_first_thread = th_vec[i].next_entry ;

	/* add to free list */
	if (th_first_free == NULL )
		th_first_free = th_last_free = &th_vec[i];
	else
	{	/* add new hole at tail to minimise re-use of slots */
		th_last_free->next_entry = &th_vec[i];
		th_last_free = &th_vec[i];
		th_last_free->next_entry = NULL;
	}
	th_vec[i].valid = FALSE;
}

/* calls _$thread_run/1 in thread.P */
static void *xsb_thread_run( void *arg )
{
        pthread_t tid;
	th_context *ctxt = (th_context *)arg ;
        int pos = THREAD_ENTRY(ctxt->tid) ;

	//	printf("pos %d ctxt %p reg1 %x\n",pos,ctxt,ctxt->_reg[1]);

	pthread_mutex_lock( &th_mutex );
        SYS_MUTEX_INCR( MUTEX_THREADS ) ;
	tid = pthread_self();
/* if the xsb thread id was just created we need to re-initialize 
   thread pthread id on the thread table */
        th_vec[pos].tid = tid ;
        th_vec[pos].valid = TRUE ;
	pthread_mutex_unlock( &th_mutex );

	emuloop( ctxt, get_ep((Psc)flags[THREAD_RUN]) ) ;

	/* execution shouldn't arrive here */
	xsb_bug( "emuloop returned from thread" );

	return NULL ;
}

/*----------------------------------------------------------------------------------*/

/* calls _$thread_run/1 in thread.P */
static void *ccall_xsb_thread_run( void *arg )
{
        pthread_t tid;
	th_context *ctxt = (th_context *)arg ;
        int pos = THREAD_ENTRY(ctxt->tid) ;

	pthread_mutex_lock( &th_mutex );
        SYS_MUTEX_INCR( MUTEX_THREADS ) ;
	tid = pthread_self();
/* if the xsb thread id was just created we need to re-initialize 
   thread pthread id on the thread table */
        th_vec[pos].tid = tid ;
        th_vec[pos].valid = TRUE ;
        pthread_mutex_unlock( &th_mutex );

	pthread_mutex_lock( &ctxt->_xsb_synch_mut ) ;

	emuloop( ctxt, get_ep(ccall_psc)) ;
	printf("exiting emuloop\n");

	printf("exiting thread\n");

	return NULL ;
}

static void copy_pflags( th_context *to, th_context *from )
{
	int i ;

	for( i = 0; i < MAX_PRIVATE_FLAGS; i++ )
		to->_pflags[i] = from->_pflags[i] ;
}

/*-------------------*/

#define increment_thread_nums \
  flags[NUM_THREADS]++ ; \
  max_threads_sofar = xsb_max( max_threads_sofar, flags[NUM_THREADS] ); 

#define decrement_thread_nums   flags[NUM_THREADS]-- ; 

static Integer xsb_thread_setup(th_context *th, int is_detached, int is_aliased) {
  th_context *new_th_ctxt ;
  Integer pos;
  Integer id;

  new_th_ctxt = mem_alloc(sizeof(th_context),THREAD_SPACE) ;

  pthread_mutex_lock( &th_mutex );
  SYS_MUTEX_INCR( MUTEX_THREADS ) ;
  id = pos = th_new( new_th_ctxt, is_detached, is_aliased );
  if (pos < 0) 
  {     pthread_mutex_unlock( &th_mutex );
        mem_dealloc(new_th_ctxt,sizeof(th_context),THREAD_SPACE);
        xsb_resource_error(CTXTc "maximum threads","thread_create",3);
  }
  increment_thread_nums;
  pthread_mutex_unlock( &th_mutex );

  SET_THREAD_INCARN(id, th_vec[pos].incarn ) ;
  new_th_ctxt->tid = id ;
  //  printf("id is %d ctxt is %p\n",id,new_th_ctxt);
  ctop_int( th, 3, id ) ;

  return pos;
}

static int xsb_thread_create_1(th_context *th, Cell goal, int glsize, int tcsize,
			       int complsize, int pdlsize, int is_detached, int pos){
  int rc;
  pthread_t *thr ;
  th_context *new_th_ctxt = th_vec[pos].ctxt;
  Integer id = new_th_ctxt->tid;

  thr = &th_vec[pos].tid ;
  copy_pflags(new_th_ctxt, th) ;
  init_machine(new_th_ctxt,glsize,tcsize,complsize,pdlsize);
  new_th_ctxt->_reg[1] = copy_term_from_thread(new_th_ctxt, th, goal) ;
  new_th_ctxt->tid = id ;
  if (is_detached) { /* set detached */
    rc = pthread_create(thr, &detached_attr_gl, &xsb_thread_run, 
			 (void *)new_th_ctxt ) ;
  }
  else {
    rc = pthread_create(thr, &normal_attr_gl, &xsb_thread_run, (void *)new_th_ctxt ) ;
  }
  th_vec[pos].valid = TRUE ;

  //  printf("creating %p %p\n",thr,th_vec[pos].tid);
  if (rc == EAGAIN) {
    decrement_thread_nums;
    cleanup_thread_structures(new_th_ctxt) ;
    th_delete(pos);
    mem_dealloc(new_th_ctxt,sizeof(th_context),THREAD_SPACE) ;
    xsb_resource_error(th,"system threads","xsb_thread_create",2);
  } else {
    if (rc != 0) {
      decrement_thread_nums;
      cleanup_thread_structures(new_th_ctxt) ;
      th_delete(pos);
      mem_dealloc(new_th_ctxt,sizeof(th_context),THREAD_SPACE) ;
      xsb_abort("[THREAD] Failure to create thread: error %d\n",rc);
    }
  }

  return rc ;
}  /* xsb_thread_create */

static int xsb_thread_create(th_context *th, int glsize, int tcsize, int complsize,int pdlsize,
			     int is_detached, int is_aliased) {
  Cell goal ;
  Integer pos ;
       
  goal = iso_ptoc_callable(th, 2,"thread_create/[2,3]");

  pos = xsb_thread_setup(th,  is_detached, is_aliased);
  return xsb_thread_create_1(th,goal, glsize, tcsize, complsize,pdlsize,is_detached,pos);
}


/*-------------------*/

call_conv int xsb_ccall_thread_create(th_context *th,th_context **thread_return)
{
  int rc ;
  th_context *new_th_ctxt ;
  pthread_t *thr ;
  Integer id, pos ;
       
  new_th_ctxt = mem_alloc(sizeof(th_context),THREAD_SPACE) ;

  pthread_mutex_lock( &th_mutex );
  SYS_MUTEX_INCR( MUTEX_THREADS ) ;
  id = pos = th_new( new_th_ctxt, 0, 0 );
  if (pos < 0) 
  {     pthread_mutex_unlock( &th_mutex );
        xsb_resource_error(CTXTc "maximum threads","thread_create",3);
  }
  flags[NUM_THREADS]++ ;
  max_threads_sofar = xsb_max( max_threads_sofar, flags[NUM_THREADS] );

  pthread_mutex_unlock( &th_mutex );

  new_th_ctxt->_xsb_ready = 0;  
  pthread_mutex_init( &new_th_ctxt->_xsb_synch_mut, NULL ) ;
  pthread_mutex_lock(&(new_th_ctxt->_xsb_synch_mut));

  copy_pflags(new_th_ctxt, th) ;

  init_machine(new_th_ctxt,0,0,0,0);

  SET_THREAD_INCARN(id, th_vec[pos].incarn ) ;
  new_th_ctxt->tid = id ;

  pthread_cond_init( &new_th_ctxt->_xsb_started_cond, NULL ) ;
  pthread_cond_init( &new_th_ctxt->_xsb_done_cond, NULL ) ;
  pthread_mutex_init( &new_th_ctxt->_xsb_ready_mut, NULL ) ;
  pthread_mutex_init( &new_th_ctxt->_xsb_query_mut, NULL ) ;
  new_th_ctxt->_xsb_inquery = 0;

  *thread_return = new_th_ctxt;

  thr = &th_vec[pos].tid ;
  rc = pthread_create(thr, &normal_attr_gl, &ccall_xsb_thread_run, (void *)new_th_ctxt ) ;
  th_vec[pos].valid = TRUE ;

  if (rc == EAGAIN) {
    xsb_resource_error(th,"system threads","xsb_thread_create",2);
  } else {
    if (rc != 0) 
      xsb_abort("[THREAD] Failure to create thread: error %d\n",rc);
  }

  while (!(new_th_ctxt->_xsb_ready))
	pthread_cond_wait( &new_th_ctxt->_xsb_done_cond, 
			   &new_th_ctxt->_xsb_synch_mut  );
  pthread_mutex_unlock( &new_th_ctxt->_xsb_synch_mut ) ;
  return rc ;
}  /* xsb_thread_create */

/*-------------------------------------------------------------------------*/
/* System Initialization Stuff */

void init_system_threads( th_context *ctxt )
{
  pthread_t tid = pthread_self();
  int id, pos;

  /* this should build an invalid thread id */
  init_thread_table();
  id = pos = th_new(ctxt, 0, 0) ;
  th_vec[pos].tid = tid ;
  th_vec[pos].valid = TRUE ;
  if( pos != 0 )
    SET_THREAD_INCARN(id, th_vec[pos].incarn ) ;
  ctxt->tid = id ;
  if( id != 0 )
	xsb_abort( "[THREAD] Error initializing thread table" );

  max_threads_sofar = 1 ;
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

/* Add new dynmutframe to beginning of chain */

DynMutPtr create_new_dynMutFrame() {
  DynMutPtr new_dynmut = mem_alloc(sizeof(DynMutexFrame),THREAD_SPACE) ;
  pthread_mutex_init( &(new_dynmut->th_mutex), &attr_rec_gl ) ;
  new_dynmut->num_locks = 0;
  new_dynmut->owner = -1;
  new_dynmut->next_dynmut = dynmut_chain_begin;
  if (dynmut_chain_begin != NULL) 
    (*dynmut_chain_begin).prev_dynmut = new_dynmut;
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
  xsb_abort("[THREAD] This engine is not configured for mutex profiling.");
}

#endif /* MULTI_THREAD */

int xsb_thread_self()
{
#ifdef MULTI_THREAD
	int pos, id;
        pthread_t tid = pthread_self();

        pthread_mutex_lock( &th_mutex );
        SYS_MUTEX_INCR( MUTEX_THREADS ) ;
        id = pos = th_find( P_PTHREAD_T_P ) ;
        pthread_mutex_unlock( &th_mutex );

	if( pos >= 0 )
		SET_THREAD_INCARN( id, th_vec[pos].incarn ) ;
#ifdef DEBUG
	else 
		raise( SIGSEGV ) ;
#endif

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
extern void abolish_all_private_tables(CTXTdecl);
extern void abolish_all_shared_tables(CTXTdecl);

xsbBool xsb_thread_request( CTXTdecl ) 
{
	Integer request_num = ptoc_int(CTXTc 1) ;
#ifdef MULTI_THREAD
	Integer id, rval;
	pthread_t_p tid ;
	int i;
	Integer rc ;
	xsbBool success = TRUE ;

	switch( request_num )
	{
	    /* Flags use default values, params have explicit
	       parameters sent in */
	case XSB_THREAD_CREATE_FLAGS:
	  iso_check_var(th, 3,"thread_create/[2,3]"); // should check here, rather than at end
	  rc = xsb_thread_create(th,flags[THREAD_GLSIZE],flags[THREAD_TCPSIZE],flags[THREAD_COMPLSIZE],
				   flags[THREAD_PDLSIZE],flags[THREAD_DETACHED],0) ;
	  break ;

	case XSB_THREAD_CREATE_PARAMS:
	  iso_check_var(th, 3,"thread_create/[2,3]"); // should check here, rather than at end
	  id = xsb_thread_create(th,ptoc_int(CTXTc 4),ptoc_int(CTXTc 5),
				 ptoc_int(CTXTc 6),ptoc_int(CTXTc 7), ptoc_int(CTXTc 8), 0);
	  break ;

	case XSB_THREAD_SETUP:
	  iso_check_var(th, 3,"thread_create/[2,3]"); // should check here, rather than at end
	  id = xsb_thread_setup(th, ptoc_int(CTXTc 8), 1);
	  ctop_int( CTXTc 9, id) ;
	  break;

	case XSB_THREAD_CREATE_ALIAS:
	  // 1: Request_num, 2: Goal, 3: _, 4: Glsize, 5: Tcpsize, 6: Complsize, 7: pdlsize, 8: detached, 9: Pos
	    rc = xsb_thread_create_1(th, iso_ptoc_callable(CTXTc 2,"thread_create/3"),ptoc_int(CTXTc 4),
				   ptoc_int(CTXTc 5),ptoc_int(CTXTc 6),ptoc_int(CTXTc 7), ptoc_int(CTXTc 8), 
				   ptoc_int(CTXTc 9));
	  break;

	  /* TLS: replaced thread_free_tab_blks() by
	     thread_free_private_tabling_resources, which sets
	     appropriate tifs to 0, but doesn't use
	     delete_predicate_table -- rather it deallocates the
	     structure managers directly.  */

	case XSB_THREAD_EXIT: {
	  int retract_aliases = 0;

	  rval = iso_ptoc_int(CTXTc 2, "thread_exit/1" ) ;
	  release_held_mutexes(CTXT);
	  release_private_tabling_resources(CTXT);
	  abolish_private_wfs_space(CTXT);
	  release_private_dynamic_resources(CTXT);
	  findall_clean_all(CTXT);
	  close_str(CTXT) ;
	  cleanup_thread_structures(CTXT) ;
	  pthread_mutex_lock( &th_mutex );
          SYS_MUTEX_INCR( MUTEX_THREADS ) ;
          i = THREAD_ENTRY( th->tid ) ;
	  th_vec[i].ctxt = NULL;
	  if( i >= 0 ) {
	    if ( th_vec[i].detached ) {
	      if (th_vec[i].aliased) 
		retract_aliases = 1;
	      th_delete(i);
	    }
	    else {
	      th_vec[i].exited = TRUE;
	      th_vec[i].status = rval;
	    }
	  }
	  pthread_mutex_unlock( &th_mutex );
	  if( i == -1 )
		xsb_abort("[THREAD] Couldn't find thread in thread table!") ;
	  mem_dealloc(th,sizeof(th_context),THREAD_SPACE) ;
	  flags[NUM_THREADS]-- ;
	  pthread_exit((void *) rval ) ;
	  ctop_int(CTXTc 3,retract_aliases);
	  rc = 0 ; /* keep compiler happy */
	  break ;
	}

	case XSB_THREAD_JOIN: {
	  id = iso_ptoc_int( CTXTc 2 ,"thread_join/[1,2]") ;
          iso_check_var(th, 3,"thread_join/[1,1]"); 
	  pthread_mutex_lock( &th_mutex );
          SYS_MUTEX_INCR( MUTEX_THREADS ) ;
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

	  ctop_int( CTXTc 4, th_vec[THREAD_ENTRY(id)].aliased) ;
	  pthread_mutex_lock( &th_mutex );
          SYS_MUTEX_INCR( MUTEX_THREADS ) ;
	  th_delete(THREAD_ENTRY(id));
	  pthread_mutex_unlock( &th_mutex );
	  ctop_int( CTXTc 3, rval ) ;
	  break ;
	}

	case XSB_THREAD_DETACH: {
	  int retract_aliases = 0;
	  int retract_exitball = 0;
	  
	  id = iso_ptoc_int( CTXTc 2 ,"thread_detach/1") ;

	  pthread_mutex_lock( &th_mutex );
          SYS_MUTEX_INCR( MUTEX_THREADS ) ;
	  tid = th_get( id ) ;
	  pthread_mutex_unlock( &th_mutex );

	  if( tid == (pthread_t_p)0 )
	    xsb_abort("[THREAD] Thread detach - invalid thread id" );
	  rc = PTHREAD_DETACH( tid ) ;
	  if (rc == EINVAL) { /* pthread found, but not joinable */
	    xsb_permission_error(CTXTc "thread_detach","thread",reg[2],"thread_detach",1); 
	  } else {
	    if (rc == ESRCH)  { /* no such pthread found */
	      xsb_existence_error(CTXTc "thread",reg[2], "thread_detach",1,1); 
	    }
	  }

	  id = THREAD_ENTRY(id) ;
	  pthread_mutex_lock( &th_mutex );
          SYS_MUTEX_INCR( MUTEX_THREADS ) ;
	  if ( th_vec[id].exited ) {
	    if (th_vec[THREAD_ENTRY(id)].detached == FALSE 
		&& th_vec[THREAD_ENTRY(id)].aliased == TRUE ) retract_aliases = 1;
	    if (th_vec[THREAD_ENTRY(id)].status > THREAD_FAILED) retract_exitball = 1;
	    th_delete(id) ;
	  }
	  else 
	    th_vec[THREAD_ENTRY(id)].detached = TRUE;
	  pthread_mutex_unlock( &th_mutex );

	  ctop_int(CTXTc 3,retract_aliases);
	  ctop_int(CTXTc 4,retract_exitball);

	  break ;
	}

       case XSB_THREAD_SELF:
	 rc = id = th->tid ;
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
	    xsb_abort("[THREAD] Real-time extensions not supported on this platform");
	  break;

	case XSB_SHOW_MUTEXES: 
	  printf("mutex owners\n");
	  for (i = 0; i < MAX_SYS_MUTEXES; i++) {
	    if (sys_mut[i].owner > 0) 
	      printf("Mutex %s (%d): %d\n",mutex_names[i],i,sys_mut[i].owner);
	  }
	  rc = 0;
	  break;

	  /* TLS: may generalize -- right now, just detached/joinable */
	case XSB_THREAD_PROPERTY: 
	  i = iso_ptoc_int(CTXTc 2,"thread_property/2");
	  if( !VALID_THREAD(i) )
		xsb_abort( "[THREAD] Invalid Thread Id" ) ;
	  ctop_int(CTXTc 3, th_vec[ THREAD_ENTRY(i) ].detached);
	  break;

	  /* for now, one interrupt, but possibly we should allow
	     users to define others  */
	case XSB_THREAD_INTERRUPT: {
	  th_context *	ctxt_ptr ;

	  i = ptoc_int(CTXTc 2);
	  if( VALID_THREAD(i) ) {
	    pthread_mutex_lock( &th_mutex ) ;
            SYS_MUTEX_INCR( MUTEX_THREADS ) ;
	    ctxt_ptr = th_vec[THREAD_ENTRY(i)].ctxt;
	    if( ctxt_ptr )
	    {	ctxt_ptr->_asynint_val |= THREADINT_MARK;
#ifdef WIN_NT
	    	PTHREAD_KILL( &th_vec[THREAD_ENTRY(i)].tid, SIGINT );
#else
	    	PTHREAD_KILL( th_vec[THREAD_ENTRY(i)].tid, SIGINT );
#endif
	    }
	    pthread_mutex_unlock( &th_mutex ) ;
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

	case ABOLISH_ALL_PRIVATE_TABLES: {
	  abolish_all_private_tables(CTXT);
	  break;
	}

	case ABOLISH_ALL_SHARED_TABLES: {
	  abolish_all_shared_tables(CTXT);
	  break;
	}

	case SET_XSB_READY: {
	  unlock_xsb_ready("set_xsb_ready");
	  break;
	}

	case MESSAGE_QUEUE_CREATE: {
	  XSB_MQ_Ptr xsb_mq;
	  int declared_size;

	  xsb_mq = (XSB_MQ_Ptr) mem_alloc(sizeof(XSB_MQ),THREAD_SPACE);
	  xsb_mq->first_message = 0;
	  xsb_mq->last_message = 0;
	  xsb_mq->size = 0;
	  if ((declared_size = ptoc_int(CTXTc 3)) == 0)
	    xsb_mq->max_size = DEFAULT_MQ_SIZE;
	  else xsb_mq->max_size = declared_size;

	  pthread_mutex_init(&xsb_mq->mq_mutex, NULL ) ;
	  pthread_cond_init( &xsb_mq->mq_has_free_cells, NULL );
	  pthread_cond_init( &xsb_mq->mq_has_messages, NULL );

	  ctop_int(CTXTc 2,(int) xsb_mq);
	  break;
	}

	case THREAD_SEND_MESSAGE: {
	  XSB_MQ_Ptr message_queue = (XSB_MQ_Ptr) ptoc_int(CTXTc 2);
	  MQ_Cell_Ptr this_cell;

	  /*   int i; CPtr BuffPtr;
	  BuffPtr = (CPtr) asrtBuff-> Buff;
	  for (i = 0  ; i < (asrtBuff->Size)/sizeof(CPtr) ; i++) 
	    printf("asrtBuf (%d): %x\n",i,BuffPtr[i]);	  */

	  pthread_mutex_lock(&message_queue->mq_mutex);
	  while (message_queue->size >= message_queue->max_size) {
	    pthread_cond_wait(&message_queue->mq_has_free_cells,&message_queue->mq_mutex);
	  }
	  
	  this_cell = mem_alloc(asrtBuff->Size+sizeof(MQ_Cell),THREAD_SPACE);
	  this_cell->prev = message_queue->last_message;
	  this_cell->next = 0;
	  this_cell->size = asrtBuff->Size+sizeof(MQ_Cell);
	  /* Moves assert buffer to word just after MQ_Cell */
	  memmove(this_cell+1,asrtBuff->Buff,asrtBuff->Size); 

	  /*	  BuffPtr = (CPtr) this_cell->message;
	  for (i = 0  ; i < (asrtBuff->Size)/sizeof(CPtr) ; i++) 
	    printf("msgBuf (%d): %x\n",i,BuffPtr[i]);	  */
	  if (message_queue->last_message) 
	    (message_queue->last_message)->next = this_cell;
	  message_queue->last_message = this_cell;
	  message_queue->size++;
	  
	  if (!message_queue->first_message) {
	    message_queue->first_message = this_cell;
	  }

	  pthread_mutex_unlock(&message_queue->mq_mutex);
	  /* Need to broadcast whenever you add a new message as
	     threads may be waiting at the end of a non-full queue*/
	  pthread_cond_broadcast(&message_queue->mq_has_messages);
	  break;
	}

case THREAD_TRY_MESSAGE: {	 
	  XSB_MQ_Ptr message_queue = (XSB_MQ_Ptr) ptoc_int(CTXTc 2);

	  pthread_mutex_lock(&message_queue->mq_mutex);
	  while (!message_queue->first_message) {
	    pthread_cond_wait(&message_queue->mq_has_messages,&message_queue->mq_mutex);
	  }
	  current_mq_cell = message_queue->first_message;
	  pcreg =  (byte *)(current_mq_cell+1);
	  break;
	}

  /* THREAD_RETRY_MESSAGE will have the lock as set up by THREAD_TRY_MESSAGE (or by
     succeeding out of pthread_cond_wait() The lock will be unlocked
     either by THREAD_ACCEPT_MESSAGE or by suspending in
     pthread_cond_wait() */
case THREAD_RETRY_MESSAGE: {	 
	  XSB_MQ_Ptr message_queue = (XSB_MQ_Ptr) ptoc_int(CTXTc 2);

	  /* If current_mq_cell is last message, the thread has made a
	     traversal through the queue without finding a term that
	     unifies.  It gives up the lock and goes to sleep.  Before
	     it wakes again, another thread may have changed the queue
	     substantially -- so its old state (current_mq_cell) is
	     invalid.  This cell may even have been reclaimed.  Thus,
	     there is no way of relating our old position to the new
	     queue.  All you can do is start again from the beginning
	     (checking, of course, that there is a beginning) */

	  if (current_mq_cell == message_queue->last_message) {
	      pthread_cond_wait(&message_queue->mq_has_messages,&message_queue->mq_mutex);
	    while (!message_queue->first_message) {
	      pthread_cond_wait(&message_queue->mq_has_messages,&message_queue->mq_mutex);
	    }
	    current_mq_cell = message_queue->first_message;
	  } 
	  else current_mq_cell = current_mq_cell->next;
	  pcreg = (byte *) (current_mq_cell+1); // offset for compiled code.
	  break;
	}

  /* Broadcasts whenever it goes from "full" to "not_full" so that
     writers will be awakened. */	
case THREAD_ACCEPT_MESSAGE: {	 
  XSB_MQ_Ptr message_queue = (XSB_MQ_Ptr) ptoc_int(CTXTc 2);

  /* Take MQ_Cell out of chain, and clear its contents */
  if (message_queue->first_message == current_mq_cell) 
      message_queue->first_message = current_mq_cell->next;

  if (message_queue->last_message == current_mq_cell) 
      message_queue->last_message = current_mq_cell->prev;

  if (current_mq_cell->prev) 
    (current_mq_cell->prev)->next = (current_mq_cell->next);

  if (current_mq_cell->next) 
    (current_mq_cell->next)->prev = (current_mq_cell->prev);

  mem_dealloc(current_mq_cell,current_mq_cell->size,THREAD_SPACE);

  message_queue->size--;
  if (message_queue->size+1 == message_queue->max_size) {
    pthread_mutex_unlock(&message_queue->mq_mutex);
    pthread_cond_broadcast(&message_queue->mq_has_free_cells);
    }
  else   pthread_mutex_unlock(&message_queue->mq_mutex);
  break;
 }

	case PRINT_MESSAGE_QUEUE: {
	  XSB_MQ_Ptr xsb_mq = (XSB_MQ_Ptr) ptoc_int(CTXTc 2);
  	  MQ_Cell_Ptr cur_cell;

	  printf("first message %p last_message %p size %d\n",
		 xsb_mq->first_message,xsb_mq->last_message,xsb_mq->size);
	  cur_cell = xsb_mq->first_message;
	  while (cur_cell != 0) {
	    printf("cell %p next %p, prev %p, size %d\n",
		   cur_cell,cur_cell->next,cur_cell->prev,cur_cell->size);
	    cur_cell = cur_cell->next;
	  }
	  break;
	}

	case XSB_USLEEP: {
	  usleep(iso_ptoc_int_arg(CTXTc 2,"usleep/1",1));
	  break;
	}

        case XSB_CHECK_ALIASES_ON_EXIT: {
	  int i = THREAD_ENTRY( th->tid ) ;
	  if (th_vec[i].detached && th_vec[i].aliased)
	    ctop_int(CTXTc 2,1);
	  else 
	    ctop_int(CTXTc 2,0);
	  if (th_vec[i].detached)
	    ctop_int(CTXTc 3,1);
	  else 
	    ctop_int(CTXTc 3,0);
	  break;
	}

	case XSB_RECLAIM_THREAD_SETUP:
	    decrement_thread_nums;
	    th_delete(ptoc_int(CTXTc 2));
	    rc = 0;
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
			ctop_int( CTXTc 2, 0 ) ;
			break;
                case XSB_SYS_MUTEX_LOCK:
                case XSB_SYS_MUTEX_UNLOCK:
			break ;
		case XSB_ENSURE_ONE_THREAD:
			break ;
		case XSB_THREAD_YIELD:
	       		break ;
	case SET_XSB_READY:
	  break;
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
    xsb_abort("[THREAD] bad scheduling status");
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

