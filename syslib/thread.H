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
:- import writeln/1 from standard.

:- import append/3 from basics.
:- import assert/1, (dynamic)/1 from assert.
:- import (table)/1, functor/3, (=..)/2, name/2 from standard.
:- import abolish_table_call/1, abolish_table_pred/1, tnot/1 from tables.
:- import abolish/1 from assert.

:-export xsb_thread_create/3, xsb_thread_exit/2, xsb_thread_join/3,
	xsb_thread_detach/2, xsb_thread_self/2.

:-export xsb_mutex_init/2, xsb_mutex_init_np/3, 
	 xsb_mutex_lock/2, xsb_mutex_trylock/2,
	 xsb_mutex_unlock/2, xsb_mutex_destroy/2.

:- export xsb_sys_mutex_lock/1, xsb_sys_mutex_unlock/1.

:- export xsb_user_mutex_lock/1, xsb_user_mutex_unlock/1.

/* Shortcuts */

:- export xsb_thread_create/1, xsb_thread_create/2,
	  xsb_thread_exit/0, xsb_thread_exit/1,
	  xsb_thread_join/2, xsb_thread_join/1,
	  xsb_thread_detach/1,
	  xsb_thread_self/1.

:-export xsb_mutex_init/1, xsb_mutex_lock/1, xsb_mutex_trylock/1,
         xsb_mutex_unlock/1, xsb_mutex_destroy/1, xsb_mutex_init_np/2.

/* Thread specific tables */

:- export ts_assert/1, ts_call/1, ts_tnot/1. % ts_abolish_table_pred/1.
:- export ts_init_tables/0, ts_abolish_tables/0, ts_abolish/0.
:- export ts_abolish_table_call/1.

/* Higher level */

:- export par_execute/1, n_par_execute/2.

:- export exmut_execute/2, console_exmut/1.





