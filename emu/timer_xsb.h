/* File:      timer_xsb.h
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


#ifndef CONFIG_INCLUDED
#error "File configs/config.h must be included before this file"
#endif


#include <signal.h>
#include <setjmp.h>

#include "flags_xsb.h"
#include "timer_defs_xsb.h"

#ifdef WIN_NT
extern jmp_buf xsb_timer_env;
#else
extern sigjmp_buf xsb_timer_env;
#endif


#ifdef WIN_NT
VOID CALLBACK xsb_timer_handler(HWND wind, UINT msg, UINT eventid, DWORD time);
UINT xsb_timer_id;
#else
extern void xsb_timer_handler(int signo);
#endif


#ifdef WIN_NT
#define SETALARM            ;
#define TURNOFFALARM        KillTimer(NULL,xsb_timer_id); flags[SYS_TIMER] = 0
#define CHECK_TIMER_SET     (flags[SYS_TIMER] > 0)
#define SET_TIMER       \
   xsb_timer_id = SetTimer(NULL,0,(UINT)(1000*(int)flags[SYS_TIMER]), \
				  (TIMERPROC)xsb_timer_handler)
#define OP_TIMED_OUT        (setjmp(xsb_timer_env) != 0)

#else  /* Unix */

/* set timer */
#define SETALARM     	    (signal(SIGALRM, xsb_timer_handler))	
/* turn off the timer */
#define TURNOFFALARM        alarm(0); flags[SYS_TIMER] = 0
#define CHECK_TIMER_SET     (flags[SYS_TIMER] > 0)
#define SET_TIMER           alarm(flags[SYS_TIMER])
#define OP_TIMED_OUT        (sigsetjmp(xsb_timer_env,1) != 0)
#endif
