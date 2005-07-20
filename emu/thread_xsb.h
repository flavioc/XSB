

#ifndef __THREAD_XSB_H__

#define __THREAD_XSB_H__

#include "context.h"
#include "thread_defs_xsb.h"
#include "basictypes.h"

xsbBool xsb_thread_request( CTXTdecl ) ;
xsbBool mt_random_request( CTXTdecl ) ;
int xsb_thread_self() ;


#ifdef MULTI_THREAD

#include <pthread.h>

#ifdef WIN_NT
typedef pthread_t* pthread_t_p;
#define PTHREAD_CREATE(a,b,c,d) pthread_create(&a,b,c,d);
#define PTHREAD_DETACH(a) pthread_detach(*a);
#define PTHREAD_CANCEL(a) pthread_cancel(*a);
#else
typedef pthread_t pthread_t_p;
#define PTHREAD_CREATE(a,b,c,d) pthread_create(a,b,c,d);
#define PTHREAD_DETACH(a) pthread_detach(a);
#define PTHREAD_CANCEL(a) pthread_cancel(a);
#endif

extern pthread_mutex_t sys_mut[] ;

extern pthread_mutex_t completing_mut;
extern pthread_cond_t completing_cond;

#define SYS_MUTEX_LOCK( M ) ( pthread_mutex_lock( &sys_mut[(M)] ) )
#define SYS_MUTEX_UNLOCK( M ) ( pthread_mutex_unlock( &sys_mut[(M)] ) )
#else
#define SYS_MUTEX_LOCK( M ) 
#define SYS_MUTEX_UNLOCK( M )
#endif

#ifdef MULTI_THREAD
void init_system_mutexes( void ) ;
void init_system_threads( th_context * ctxt ) ;

th_context *find_context( int tid );
#endif

#define ENSURE_ONE_THREAD()					\
{	if( flags[NUM_THREADS] > 1 ) 				\
		xsb_abort( "more than one thread running" ) ; 	\
}


/*
  TLS: the mt engine does not yet work for enable no cygwin, but this
   allows random and srandom to be used.  They're both defined in
   stdlib.h as they should be, but Windows calls them rand and srand.
*/

#if defined(WIN_NT)

#define RANDOM_CALL rand
#define SRANDOM_CALL srand

#else

#define RANDOM_CALL random
#define SRANDOM_CALL srandom

#endif


/* TLS: for Cygwin, these constants must be re-defined */

#if defined(DARWIN) || defined(FREEBSD)

#define PTHREAD_MUTEX_RECURSIVE_NP PTHREAD_MUTEX_RECURSIVE
#define PTHREAD_MUTEX_ERRORCHECK_NP PTHREAD_MUTEX_ERRORCHECK
#define PTHREAD_MUTEX_FAST_NP PTHREAD_MUTEX_NORMAL

#endif

#endif















