/* File:      timer_xsb.c
** Author(s): Songmei Yu, kifer
** Contact:   xsb-contact@cs.sunysb.edu
**
** Copyright (C) The Research Foundation of SUNY, 1999
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
** $Id$
** 
*/



#include "xsb_config.h"

#ifdef WIN_NT
#include <windows.h>
#include <winuser.h>
#else /* UNIX */
#include <sys/types.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#endif

#include <stdio.h>

#include "cell_xsb.h"
#include "error_xsb.h"
#include "setjmp_xsb.h"
#include "timer_xsb.h"
#include "basicdefs.h"

/* To set a timeout for a function call such as: 
  ... ...
  int foo(char *X, int Y, long Z);
  ... ...
 
   do the following steps:

	       1: Define a structure

	          struct xsb_timeout {
		     long parent_thread;
		     ???  return_value;
		     ???  arg1;
		     .....
		     ???  arg_n;
		  }

		  that will be used to hold the parameters of foo().
		  The member PARENT_THREAD is mandatory and must be the
		  first in the structure.
		  The other members of the structure are optional and they are
		  intended to represent the return value and the arguments of
		  foo().
		  Next, make a wrapper, say

		      void new_foo(xsbTimeout *pptr)

		  The type xsbTimeout is a typedef to struct xsb_timeout.
		  It is defined in timer_xsb.h.
		  In our case (given the declaration of foo() above), the
		  xsb_timeout structure will look as follows: 
	   
		   struct xsb_timeout {
                       long parent_thread;
		       int  return_value;
		       char *X;
		       int   Y;
		       long  Z;
		   }
   		
	       Then the wrapper will look as follows:

               void new_foo(xsbTimeout *pptr) {
		   pptr->return_value = foo(pptr->X, pptr->Y, pptr->Z);

		   NOTIFY_PARENT_THREAD(pptr);
	       }
    
       step 2: Instead of calling foo() directly, now call the generic timeout
               control function MAKE_TIMED_CALL:  
   
   	       int make_timed_call(xsbTimeout*, void (*)(xsbTimeout*));

	       For instance, 
   
	       xsbTimeout *pptr = NEW_TIMEOUT_OBJECT; // defined in timer_xsb.h
	       int timeout_flag;

	       // timeout is set in Prolog using set_timer/1 call
	       if (CHECK_TIMER_SET) {
		  timeout=make_timed_call(pptr, new_foo);
		  if (timeout_flag == TIMER_SETUP_ERR) {
		     // problem setting up timer (Windows only)
	          } else if (timeout_flag) {
	            // timeout happened
		    .....
		  }
	       } else {
	         // timer is not set
		 new_foo(pptr); 
	       }

       step 3: Free the xsbTimeout object when appropriate.
*/

#ifdef WIN_NT
static int exitFlag = STILL_WAITING;
static long timedThread;
HANDLE sockEvent = NULL;
#endif

struct xsb_timeout {
  long parent_thread;
};


#ifdef WIN_NT
VOID CALLBACK xsb_timer_handler(HWND wind, UINT msg, UINT eventid, DWORD time)
{
  if (exitFlag == STILL_WAITING)
    exitFlag = TIMED_OUT; /* tell the timed thread to quit */
  TerminateThread((HANDLE)timedThread, 1);
}

int message_pump()
{
  MSG msg;

  if ((xsb_timer_id = SetTimer(NULL,
			       0,
			       /* set timeout period */
			       (UINT)((int)flags[SYS_TIMER] * 1000),
			       (TIMERPROC)xsb_timer_handler))
      == 0) {
    xsb_error("SOCKET_REQUEST: Can't create timer: %d\n", GetLastError());
    return TIMER_SETUP_ERR;
  }

  exitFlag=STILL_WAITING;
  while ((exitFlag==STILL_WAITING) && GetMessage(&msg,NULL,0,0)) {
    DispatchMessage(&msg);
    if (msg.wParam == NORMAL_TERMINATION)
      break;
  }

  if (xsb_timer_id != 0)
    TURNOFFALARM;

  if (exitFlag == TIMED_OUT) 
    return TRUE;  /* timed out */
  else
    return FALSE;  /* not timed out */
}

#else  /* UNIX */

/* SIGALRM handler for controlling time outs */
void xsb_timer_handler(int signo)
{ 
  siglongjmp(xsb_timer_env,1);
}

#endif

/* the following function is a general format for timeout control. it takes 
   function calls which need timeout control as argument and controls the
   timeout for different platform */ 
int make_timed_call(xsbTimeout *pptr, void (*fptr)(xsbTimeout *))
{
#ifdef WIN_NT   
  int return_msg; /* message_pump() return value */
#endif

  SETALARM;     /* specify the timer handler in Unix;
		   Noop in Windows (done in SetTimer) */
#ifdef WIN_NT
  /* create a concurrent timed thread; 
     pptr points to the procedure to be timed */  
  pptr->parent_thread=(long)GetCurrentThreadId();
  if((timedThread = _beginthread(fptr,0,(void*)(pptr)))==-1) { 
    xsb_error("SOCKET_REQUEST: Can't create concurrent timer thread\n");
    return TIMER_SETUP_ERR;
  }
  /* OP_TIMED_OUT returns TRUE/FALSE/TIMER_SETUP_ERR */
  if ((return_msg = OP_TIMED_OUT) == TIMER_SETUP_ERR)
    return TIMER_SETUP_ERR;  
  else if (!return_msg) { /* no timeout */
    TURNOFFALARM;
    return FALSE;
  } else { /* timeout */
    TURNOFFALARM;
    return TRUE;
  }
#else /* UNIX */
  if ( !OP_TIMED_OUT ) { /* no timeout */
    SET_TIMER; /* specify the timeout period */       
    (*fptr)(pptr); /* procedure call that needs timeout control */
    TURNOFFALARM;
    return FALSE;
  } else {  /* timeout */
    TURNOFFALARM;
    return TRUE;
  }

#endif

}
