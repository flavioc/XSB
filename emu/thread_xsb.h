/* File:      thread_xsb.h
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
#define PTHREAD_KILL(a,s) pthread_kill(*a,s);
#else
typedef pthread_t pthread_t_p;
#define PTHREAD_CREATE(a,b,c,d) pthread_create(a,b,c,d);
#define PTHREAD_DETACH(a) pthread_detach(a);
#define PTHREAD_CANCEL(a) pthread_cancel(a);
#define PTHREAD_KILL(a,s) pthread_kill(a,s);
#endif

typedef struct Mutex_Frame {
  pthread_mutex_t th_mutex; 
  int num_locks;
  int owner;
} MutexFrame;

typedef struct Dynamic_Mutex_Frame *DynMutPtr;
typedef struct Dynamic_Mutex_Frame {
  pthread_mutex_t th_mutex; 
  int             num_locks;
  int             owner;
  DynMutPtr       next_dynmut;
  DynMutPtr       prev_dynmut;
} DynMutexFrame;

extern MutexFrame sys_mut[MAX_SYS_MUTEXES];

#define MUTARRAY_MUTEX(i) &(sys_mut[(i)].th_mutex)
#define MUTARRAY_NUMLOCKS(i) sys_mut[(i)].num_locks
#define MUTARRAY_OWNER(i) sys_mut[(i)].owner

extern void print_mutex_use(void);
extern void release_held_mutexes(CTXTdecl);

extern pthread_mutex_t completing_mut;
extern pthread_cond_t completing_cond;

#define PROFILE_MUTEXES 1
#ifdef PROFILE_MUTEXES

#define SYS_MUTEX_LOCK( M )   {pthread_mutex_lock(MUTARRAY_MUTEX(M));	      \
                               MUTARRAY_OWNER(M) = xsb_thread_id;	      \
                               MUTARRAY_NUMLOCKS(M)++; }

#define SYS_MUTEX_LOCK_NOERROR( M )   {pthread_mutex_lock(MUTARRAY_MUTEX(M));  \
                                       MUTARRAY_NUMLOCKS(M)++; }
#else

#define SYS_MUTEX_LOCK( M )   {pthread_mutex_lock( MUTARRAY_MUTEX(M));	      \
                               MUTARRAY_OWNER(M) = xsb_thread_id; }

#define SYS_MUTEX_LOCK_NOERROR( M )   {pthread_mutex_lock(MUTARRAY_MUTEX(M)); }

#endif /* PROFILE_MUTEXES */

#define SYS_MUTEX_UNLOCK( M )   {pthread_mutex_unlock( MUTARRAY_MUTEX(M) );        \
                                 MUTARRAY_OWNER(M) = -1; }

#define SYS_MUTEX_UNLOCK_NOERROR( M )   {pthread_mutex_unlock( MUTARRAY_MUTEX(M) );        \
                                         MUTARRAY_OWNER(M) = -1; }
#else
#define SYS_MUTEX_LOCK( M ) 
#define SYS_MUTEX_LOCK_NOERROR( M ) 
#define SYS_MUTEX_UNLOCK( M )
#define SYS_MUTEX_UNLOCK_NOERROR( M )
#endif /* MULTI_THREAD */

#ifdef MULTI_THREAD
void init_system_mutexes( void ) ;
void init_system_threads( th_context * ctxt ) ;

th_context *find_context( int tid );
#endif

#define ENSURE_ONE_THREAD()						\
  { if( flags[NUM_THREADS] > 1 )					\
      xsb_abort( "Operation is permitted only when a single thread is active" ) ; \
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

#if defined(DARWIN) || defined(FREEBSD) || defined(SOLARIS)

#define PTHREAD_MUTEX_RECURSIVE_NP PTHREAD_MUTEX_RECURSIVE
#define PTHREAD_MUTEX_ERRORCHECK_NP PTHREAD_MUTEX_ERRORCHECK
#define PTHREAD_MUTEX_FAST_NP PTHREAD_MUTEX_NORMAL

#endif

#endif















