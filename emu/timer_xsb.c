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
#endif

#include "setjmp_xsb.h" 


/* To set a timeout, do this:
#include "xsb_config.h"
#include "timer_xsb.h"

   if (CHECK_TIMER_SET) {
      // if timeout was set in Prolog using set_timeout/1
      SETALARM;
      if (!TIMERJMP) {
	SET_TIMER;
	.... actions if NO timeout...;
        TURNOFFALARM;
      } else {
        .... actions if timeout .....
        TURNOFFALARM;
      }
   } else {
     .... actions when timeout is not set ....
   }
*/


#ifdef WIN_NT
jmp_buf xsb_timer_env;
#else
sigjmp_buf xsb_timer_env;
#endif



#ifdef WIN_NT
VOID CALLBACK xsb_timer_handler(HWND wind, UINT msg, UINT eventid, DWORD time)
{
  longjmp(xsb_timer_env,1);
}
#else
/* SIGALRM handler for controlling time outs */
void xsb_timer_handler(int signo)
{ 
  siglongjmp(xsb_timer_env,1);
}
#endif
