

#ifndef __THREAD_XSB_H__

#define __THREAD_XSB_H__

#include "thread_defs_xsb.h"

xsbBool xsb_thread_request( CTXTdecl ) ;
xsbBool mt_random_request( CTXTdecl ) ;


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

#endif
