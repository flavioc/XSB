/* File:      assert.H
** Author(s): Kostis Sagonas, David S. Warren
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
** Copyright (C) ECRC, Germany, 1990
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


:- export convert_to_dyna/3,/* incremental evaluation */ assert0/1, assert/1, asserta/1, assertz/1, 
	  asserti/2, assertai/2, assertzi/2, assert/3.
:- export retract/1, retractall/1, abolish/1, abolish/2. 
:- export retract0/1.	% used in standard.P (for multifile)
:- export retract_nr/1, reclaim_space/1.
:- export retract_last_fact/1.
:- export clause0/2, clause/2.
:- export (dynamic)/1.
:- export t_assert/2.
   :- export code_to_buff/2. % for message queues
:- export exchange_definitions/2.

:- import append/3, memberchk/2 from basics.

:- import default_assert_size/1 from config.

:- import db_retract/4, db_retractall/3, db_abolish/1, db_clause/4,
  db_retract_nr/4, db_reclaim_space/2
     from dbclause.

:- import instantiation_error/3 , permission_error/4, 
             type_error/4 from error_handler.

:- import '_$index'/3, '_$trie_asserted'/1, retract_trie_index/1,
	  retract_index/1, retract_table/1, mpa_to_skel/2  from file_op.
:- import psc_ep/2, psc_type/2, psc_set_type/2, psc_arity/2,
	  conpsc/2, assert_code_to_buff/1, assert_buff_to_clref/6,
          psc_set_ep/2, unload_seg/1, db_build_prref/3, db_get_prref/2,
	  code_call/3, db_retract0/3, term_set_arg/4
     from machine.
:- import db_get_last_clause/4 from machine.

:- import '_$multifile_retract'/3 from parse.

 :- import (=..)/2, functor/3, integer/1, callable/1, arg/3,
         goal_cut_trans/3, goal_cut_untrans/2, abort/1, check_arity_integer/3,
         is_most_general_term/1, atomic/1, warning/1
     from standard.

:- import standard_symbol/3, standard_dynamic_symbol/2 from std_xsb.

:- import xsb_sys_mutex_lock/1, xsb_sys_mutex_unlock/1 from thread.

:- import call_xsb_hook/3, xsb_assert_hook/1, xsb_retract_hook/1 from xsb_hook.

:- import obsolete/2 from obsolete.
