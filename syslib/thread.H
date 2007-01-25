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

:- export thread_cancel/1,
          thread_create/2,thread_create/3,
          thread_detach/1,
	  thread_exit/1,
          thread_exit/0,
	  thread_join/2, 
          thread_join/1,
          thread_property/2,
   thread_self/1,set_xsb_ready/0,
   thread_yield/0.

:- export abolish_private_tables/0,abolish_shared_tables/0.
:- export abolish_all_private_tables/0,abolish_all_shared_tables/0.

/* All obsolete -- try not to use */
:- export xsb_thread_cancel/1,
          xsb_thread_create/2,xsb_thread_create/3, 
          xsb_thread_detach/1,
	  xsb_thread_exit/1,  
          xsb_thread_exit/0,  % obsolescent
	  xsb_thread_join/2, 
          xsb_thread_join/1,  % obsolescent
          xsb_thread_property/2,
          xsb_thread_self/1,
          xsb_thread_yield/0.

:- export xsb_sys_mutex_lock/1, xsb_sys_mutex_unlock/1.
:- export xsb_show_mutexes/0.

%%:- export get_first_mutex_property/4, get_next_mutex_property/4.

:- import retractall/1 from assert. 
 
:- import check_callable/3, 
          check_integer/3, check_nonvar/3, check_var/3,
          default_error_handler/1,domain_error/5,
          instantiation_error/3, misc_error/1, permission_error/4
      from error_handler.

:- import thread_request/8 from machine.
:- import stat_set_flag/2, term_psc/2 from machine.

:- import call/1, catch/3, integer/1 from standard.

 :- import '_$thread_exit_ball'/2 from usermod.

/* Higher level */
:- export par_execute/1, n_par_execute/2.

%:- export xsb_user_mutex_lock/1, xsb_user_mutex_unlock/1.
%:- export xsb_set_init_glstack_size/1, xsb_set_init_tcpstack_size/1,
%	  xsb_set_init_pdl_size/1, xsb_set_init_complstack_size/1.
%:-export xsb_mutex_trylock/1,xsb_mutex_unlock/1, xsb_mutex_destroy/1.

%:- export exmut_execute/2, console_exmut/1.


