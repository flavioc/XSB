/* File:      consult.H
** Author(s): Kostis F. Sagonas, David S. Warren
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


:- export consult/1, consult/2, '.'/2, reconsult/1, reconsult/2,
	  compile/1, compile/2, search_module/6,
	  load_dyn/1, load_dyn/2, 
	  load_dync/1, load_dync/2, read_canonical/1,
	  ensure_dyn_loaded/1, ensure_dync_loaded/1,
	  cvt_canonical/2.

:- local load_dyn_retracted/5, load_dyn_trie_retracted/1,
	 load_dyn_file_loaded/2, load_dyn_pred_loaded/4, load_dyn_file/1.

:- import libpath/1 from loader.
:- import compile/4 from compile.
:- import compile_cH/5 from foreign.
:- import str_cat/3, str_sub/2 from string.
:- import default_assert_size/1 from config.
:- import predicate_property/2, current_input_port/1 from curr_sym.
:- import file_write0/2, file_nl/1 from xsb_writ.
:- import rm/1, sys_link/3, process_control/2 from shell.
:- import abort/1, call/1, file_exists/1, atom/1, repeat/0,
	  seeing/1, see/1, seen/0, call/1, goal_cut_trans/3,
	  expand_term/2, read/1, op/3, (table)/1, (index)/3, (index)/2,
	  (=..)/2, writeln/1, write_canonical/1, tell/1,
	  told/0, telling/1, functor/3, cputime/1
     from standard.
:- import code_load/3, code_call/3, file_read_canonical/3,
	  stat_flag/2,
	  expand_filename/2, tilde_expand_filename/2, unload_seg/1,
	  buff_word/3, assert_code_to_buff/1, assert_buff_to_clref/6,
	  conpsc/2, psc_prop/2, psc_tabled/2, psc_type/2, psc_name/2,
	  psc_arity/2, existing_file_extension/2, almost_search_module/5
     from machine.
:- import file_time/2, file_close/1 from file_io.
:- import asserta/1, (dynamic)/1, retractall/1, assert/1, retract/1, t_assert/2
     from assert.
:- import warning/1, time_message/2 from auxlry.
:- import append/3, memberchk/2 from basics.
:- import '_$index'/3, '_$trie_asserted'/1 from file_op.
:- import add_hilog_symbol/1 from hilogsym.
:- import get_p_mod/3, change_multifile_directive/3, open_or_xpp_file/2,
	  '_$multifile'/1, '_$multifile1'/1,
	  '_$multifile2'/3, '_$apply_arity'/1
     from parse.
:- import conset/2, conget/2  from prof_sbp.
:- import xsb_configuration/2 from xsb_configuration.
