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


:- export abolish_all_tables/0, abolish_table_call/1, abolish_table_pred/1,
	  (use_variant_tabling)/1, (use_subsumptive_tabling)/1,
	  table_state/2, table_state/4,
	  get_calls_for_table/2, get_returns_for_call/2,
	  tfindall/3, tnot/1, 't not'/1, sk_not/1,
	  delete_return/2,
	  get_call/3, get_calls/3, get_delay_lists/3,
          get_returns/2, get_returns/3,
	  get_residual/2,
	  get_lastnode_cs_retskel/4, /* because it is used in intern.P */
	  force_truth_value/2,
	  (trie_dynamic)/1, trie_assert/1, trie_assert/2,
	  trie_retract/1, trie_retract_nr/1, trie_retract_all/1.

:- import findall/3 from setof.
:- import add_trie_index/1 from file_op.
:- import ground/1, member/2, copy_term/2 from basics.
:- import retract/1, retract_nr/1, retractall/1,
	  (dynamic)/1, t_assert/2 from assert.
:- import call/1, tcall/1, abort/1, functor/3 from standard.  
:- import abolish_table_info/0, is_incomplete/4, get_ptcp/1, slg_not/1,
	  get_subgoal_ptr/2 from machine.
:- import numbervars/3, unnumbervars/2 from num_vars.
:- import (table)/1 from usermod.

/* ------------------- end of file tables.H --------------------------- */
