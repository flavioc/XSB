

#ifndef __THREAD_XSB_H__

#define __THREAD_XSB_H__

#include "thread_defs_xsb.h"

xsbBool xsb_thread_request( CTXTdecl ) ;
xsbBool mt_random_request( CTXTdecl ) ;
int xsb_thread_self() ;


#ifdef MULTI_THREAD

#include <pthread.h>

extern pthread_mutex_t sys_mut[] ;

#define SYS_MUTEX_LOCK( M ) ( pthread_mutex_lock( &sys_mut[(M)] ) )
#define SYS_MUTEX_UNLOCK( M ) ( pthread_mutex_unlock( &sys_mut[(M)] ) )
#else
#define SYS_MUTEX_LOCK( M ) 
#define SYS_MUTEX_UNLOCK( M )
#endif

#ifdef MULTI_THREAD
void init_system_mutexes( void ) ;
void init_system_threads( void ) ;
#endif

#define ENSURE_ONE_THREAD()					\
{	if( flags[NUM_THREADS] > 1 ) 				\
		xsb_abort( "more then one thread running" ) ; 	\
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

#if defined(CYGWIN) || defined(DARWIN)

#define PTHREAD_MUTEX_RECURSIVE_NP PTHREAD_MUTEX_RECURSIVE
#define PTHREAD_MUTEX_ERRORCHECK_NP PTHREAD_MUTEX_ERRORCHECK
#define PTHREAD_MUTEX_FAST_NP PTHREAD_MUTEX_NORMAL

#endif

#endif
