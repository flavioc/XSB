/* File:      thread.H
** Author(s): R. Marques
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
** $Id$
** 
*/

:- import thread_request/5 from machine.
:- import stat_set_flag/2, term_psc/2 from machine.

:- import call/1 from standard.
:- import writeln/2 from standard.
:- import catch/3 from standard.

:- import check_callable/3, check_integer/3, check_var/3,
          default_error_handler/1,domain_error/5,
          instantiation_error/3 from error_handler.
:- import integer/1 from standard.

:-export xsb_thread_create/3, xsb_thread_exit/2, xsb_thread_join/3,
	xsb_thread_detach/2, xsb_thread_self/2.

:-export xsb_mutex_init/2, xsb_mutex_init_np/3, 
	 xsb_mutex_lock/2, xsb_mutex_trylock/2,
	 xsb_mutex_unlock/2, xsb_mutex_destroy/2.

:- export xsb_sys_mutex_lock/1, xsb_sys_mutex_unlock/1.

:- export xsb_user_mutex_lock/1, xsb_user_mutex_unlock/1.

:- export xsb_thread_yield/0, xsb_thread_yield/1.

:- export xsb_show_mutexes/1.

:- export xsb_set_init_glstack_size/1, xsb_set_init_tcpstack_size/1,
	  xsb_set_init_pdl_size/1, xsb_set_init_complstack_size/1.

/* Shortcuts */

:- export xsb_thread_create/1, xsb_thread_create/2,
	  xsb_thread_exit/0, xsb_thread_exit/1,
	  xsb_thread_join/2, xsb_thread_join/1,
	  xsb_thread_detach/1,
	  xsb_thread_self/1.

:-export xsb_mutex_init/1, xsb_mutex_lock/1, xsb_mutex_trylock/1,
         xsb_mutex_unlock/1, xsb_mutex_destroy/1, xsb_mutex_init_np/2.

/* Higher level */

:- export par_execute/1, n_par_execute/2.

:- export exmut_execute/2, console_exmut/1.





