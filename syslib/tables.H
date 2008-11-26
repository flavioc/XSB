/* File:      tables.H
** Author(s): Terry Swift, Prasad Rao, Kostis Sagonas
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


:- export
      abolish_all_tables/0,
      abolish_table_call/1,       abolish_table_call/2, 
      invalidate_tables_for/2,
      abolish_table_pred/1,      abolish_table_pred/2,
      abolish_module_tables/1,
      delete_return/2,
      force_truth_value/2,
      get_call/3,
      get_calls/3,
      get_calls_for_table/2,
      get_lastnode_cs_retskel/4, /* because it is used in intern.P */
      get_producer_call/3,
      get_ptcp/1,
      get_residual/2,
      get_returns/2,
      get_returns/3,
      get_returns_and_dls/3, 
      get_returns_for_call/2,
      is_incomplete/2,
      sk_not/1,
      table_state/2,
      table_state/4,
      tfindall/3,
      tnot/1,
      't not'/1,
      (trie_dynamic)/1,
      trie_assert/1,
      trie_assert/2,
      trie_retract/1,
      trie_retract_nr/1,
      trie_retract_all/1,
      (use_subsumptive_tabling)/1,
      (use_variant_tabling)/1.

   :- export find_components/1, find_forward_dependencies/1, %%%find_backward_dependencies/1,
   find_answers/1.

:- export '_$$get_returns'/3.

:- import findall/3
     from setof.
:- import add_trie_index/1
     from file_op.
:- import copy_term/2, ground/1, member/2
     from basics.
:- import (dynamic)/1, retract/1, retract_nr/1, retractall/1, t_assert/2
     from assert.
:- import (=..)/2, abort/1, call/1, functor/3, is_attv/1, tcall/1
     from standard.
   :- import domain_error/5, check_nonvar/3, type_error/4, table_error/1 from error_handler.

:- import abolish_table_info/0, delete_attributes/1, table_inspection_function/5,
   term_psc/2, term_new/2 from machine.

:- import numbervars/3, unnumbervars/2
     from num_vars.

:- import invalidate_table_for/2 from usermod.

/* ------------------- end of file tables.H --------------------------- */
