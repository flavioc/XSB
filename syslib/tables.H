/* File:      tables.H
** Author(s): Swift, Rao, Sagonas
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
        abolish_table_call/1,
	abolish_table_pred/1,
	etnot/1,
	get_returns_for_call/2,
	get_calls_for_table/3,
	robust_term_psc/2,
	set_pil_on/0, set_pil_off/0, /* convenience */
	set_prof_on/1, set_prof_off/0, /* convenience */
	set_xtrace_on/0, set_xtrace_off/0, /* convenience */
	show_table/1,
	from_table/1,
	show_table0/1, 		% debugging.
	output_table/2,		% debugging.
	table_state/2,
	tfindall/3, tnot/1, 't not'/1,
	zero_out_profile_table/0, write_out_profile_table/0,
	make_list/4, %should not be used
	nth_var_reg/2,
	call_psc_tip/3,
	delete_return/2,
	not_most_general/1,
	get_calls/3,
	get_returns/2,
	get_skel/2,
	get_returns/3,
	get_residual/2,
	get_delay_lists/2,
	delete_predicate_table/1,
	is_trie_asserted/1,
	(trie_dynamic)/1,
	trie_assert/1,
	trie_assert/2,
	trie_retract/1,
	trie_retract_nr/1,
	trie_retract_all/1,
	trie_delete_branch/2,
	abolish_trie_asserted/1,
	breg_retskel/4,	
	tip_struct_disp/2,
	call_struct_disp/2,
	tip_call_trie_root/2,
	subgp_call_trie_leaf/2,
	get_subg_ptr/3,
	get_subg_ptr/2,
	trie_node_element/7.


:- import 
	abolish_table_info/0,
	buff_set_word/3, buff_word/3,
	call_hash/5, conpsc/2, cut_if_leader/3, slg_not/1,
	is_incomplete/5, get_osp_breg/2, get_ptcp/1, get_subgoal_ptr/2,
	print_predicate_table/1,
        psc_tabled/2,
	stat_set_flag/3, 
	term_type/2, tip_prop/4, get_emu_dependent_const/2,
	conpsc/2, buff_byte/3, buff_set_byte/3, 
	zero_out_profile/0,write_out_profile/0
	from machine.

:- import nl/0, ttywritenl/1, call/1, tcall/1,
	  warning/1, tell/1, told/0, alt_write_canonical/1,
	  abort/1, (=..)/2, write/1, writeln/1, arg/3, functor/3
	  from standard.  

:- import numbervars/3 from num_vars.
:- import ground/1, copy_term/2, member/2 from basics.
:- import variant/2 from subsumes.
:- import retract/1, retract_nr/1, retractall/1, 
          (dynamic)/1, t_assert/2 from assert.

:- import add_trie_index/1 from file_op.

:- import findall/3 from setof.

