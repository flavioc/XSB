/* File:      flrutils.H
**
** Author(s): Guizhen Yang
**
** Contact:   xsb-contact@cs.sunysb.edu
**
** Copyright (C) The Research Foundation of SUNY, 1999-2001
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


:- import
	file_exists/1,
	cputime/1,
	integer/1,
	atom/1
   from standard.

:- import member/2 from basics.

:- import assert/1, retractall/1 from assert.

:- import compile/2, consult/1 from consult.

:- import library_directory/1 from usermod.

:- import sort/2, findall/3 from setof.

:- import xsb_configuration/2 from xsb_configuration.

:- import xpp_options/1 from parse.

:- import current_output_port/1, current_predicate/1 from curr_sym.

:- import package_configuration/2 from packaging.

:- import conset/2 from gensym.

:- import
	slash/1,
	is_absolute_filename/1,
	parse_filename/4,
	stat_set_flag/2
   from machine.

:- import
	file_time/2,
	fmt_write_string/3,
	fmt_write/3,
	file_close/1,
	print_file_contents/1,
	path_sysop/2
   from file_io.

:- import process_control/2 from shell.

:- import xpp_process_file/3 from parse.

:- import bootstrap_flora/0 from flora2.
:- bootstrap_flora.

:- import
	flora_reset_lexer/0,
	flora_tokens/2,
	flora_token_text/6,
	flora_symbol_token_struct/6,
	flora_rule_delimeter_struct/6,
	flora_blank_line/1
   from flrlexer.

:- import flora_compose/3 from flrcomposer.

:- import flora_reset_operator/0 from flroperator.

:- import flora_reset_arguments/0 from flrsignature.

:- import flora_reset_prolog/0 from flrprolog.

:- import flora_reset_firstorder/0 from flrnohilog.

:- import flora_parse/3 from flrparser.

:- import
	flora_reset_compiler/0,
	flora_compile/4
   from flrcompiler.

:- import
	flora_divide_program/3,
	flora_extern_code/2,
	flora_intern_code/4
   from flrcoder.

:- import plg2hlg/3 from prolog2hilog.

:- import
	flora_stderr_nl/0,
	flora_stderr_string/1,
	flora_stderr_string/2,
	flora_stdwarn_string/1,
	flora_stdwarn_string/2,
	flora_stdmsg_nl/0,
	flora_error_heading/0,
	flora_error_indentline/0,
	flora_warning_indentline/0,
	flora_error_line/1,
	flora_error_line/2,
	flora_warning_line/1,
	flora_warning_line/2,
	flora_message_line/1,
	flora_message_line/2
   from flrprint.

:- import flora_stderr_write/1 from flrdisplay.

:- import flora_include/2 from flrlibman.

:- import flora_wspredicate/4 from flrwrapper.

:- dynamic flora_maxerr(_).

:- index(flora_maxerr/1,trie).

:- export
	flMaxerr/1,
	flHalt/0,
	flora_compile_internal/1,
	flora_compile_internal/2,
	flora_load_module_internal/1,
	flora_load_module_internal/2,
	flP2H/2,
	flora_locate_file/3,
	flora_FLH_filename/2,
	flloadtrailer/1,
	flloadtrailer/2,
	flsetuptrailer/1,
	flsetuppatch/0,
	flora_end_of_input/1,
	flora_not_end_of_input/1,
	flora_shell_command_line/3,
	flora_good_command/1,
	flora_bad_command/1,
	flora_noop_command/1,
	flora_check_filename/1,
	flora_set_xpp_options/0,
	flora_clear_xpp_options/0,
	flora_list2conjunct/2,
	flora_check_workspace/1.
