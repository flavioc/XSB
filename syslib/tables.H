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


:- export abolish_all_tables/0, abolish_table_call/1, abolish_table_pred/1,
	  get_calls_for_table/2, get_returns_for_call/2,
	  table_state/2, output_table/2,		% debugging.
	  tfindall/3, tnot/1, 't not'/1, etnot/1,
	  delete_return/2,
	  get_call/3, get_calls/3, get_returns/2, get_returns/3,
	  get_residual/2,
	  get_lastnode_and_retskel/3, /* because it is used in intern.P */
	  breg_retskel/4.

:- export is_trie_asserted/1, (trie_dynamic)/1, trie_assert/1, trie_assert/2,
	  trie_retract/1, trie_retract_nr/1, trie_retract_all/1,
	  abolish_trie_asserted/1.

:- export set_pil_on/0, set_pil_off/0, /* convenience */
	  set_prof_on/1, set_prof_off/0, /* convenience */
	  zero_out_profile_table/0, write_out_profile_table/0.

:- import abolish_table_info/0, buff_set_word/3, buff_word/3,
	  call_hash/5, conpsc/2, cut_if_leader/3, slg_not/1,
	  is_incomplete/4, get_osp_breg/2, get_ptcp/1, get_subgoal_ptr/2,
          psc_tabled/2, stat_set_flag/3, tip_prop/4, get_emu_dependent_const/2,
	  conpsc/2, buff_byte/3, buff_set_byte/3, 
	  zero_out_profile/0, write_out_profile/0
	from machine.

:- import nl/0, call/1, tcall/1, tell/1, told/0, alt_write_canonical/1,
	  abort/1, writeln/1, functor/3
        from standard.  

:- import findall/3 from setof.
:- import numbervars/3 from num_vars.
:- import add_trie_index/1 from file_op.
:- import ground/1, member/2 from basics.
:- import retract/1, retract_nr/1, retractall/1, 
          (dynamic)/1, t_assert/2 from assert.

/* ------------------- end of file tables.H --------------------------- */
