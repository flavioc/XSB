/* File:      parse.H
** Author(s): Saumya Debray, Kostis F. Sagonas
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


:- export parse/4, get_symtab/2, parse_preddef/4.
:- export get_p_mod/3, change_multifile_directive/3, 
	open_or_cpp_file/2, xsb_cpp_include_dir/1,
	'_$multifile'/1, '_$multifile1'/1,
	'_$multifile2'/3, '_$apply_arity'/1,
	'_$multifile_retract'/3.

:- import stat_flag/2, stat_set_flag/2, slash/1 from machine.
:- import closetail/1 from listutil.
:- import inline_predicate/2 from builtin.
:- import singleton_check/2 from singlton.
:- import file_read/3, vv/2 from xsb_read.
:- import add_hilog_symbol/1 from hilogsym.
:- import otherwise/0, conset/2, append/3, conget/2 from prof_sbp.
:- import member1/2, error/1, warning/1, message/1, setoptions1/1 from auxlry.
:- import sym_insert/5, sym_propin/2, sym_complete/1,
	  add_new_clause/2, clause_listify/2
     from symtab.
:- import functor/3, seen/0, file_exists/1, '=..'/2, atom/1, integer/1, 
	  op/3, (index)/3, real/1, see/1, expand_term/2, prolog_arglist/4,
	  name/2, seeing/1, is_most_general_term/1
     from standard.
:- import retract/1, retractall/1, assert/1, (dynamic)/1 from assert.
:- import add_table/1, '_$index'/3 from file_op.

:- import spawn_process/5, process_control/2 from shell.
:- import xsb_configuration/2 from xsb_configuration.

:- import fmt_write_string/3, file_close/1 from file_io.
:- import windows_os_loader/0 from loader.

:- import findall/3 from setof.
:- import current_input_fd/1 from curr_sym.
