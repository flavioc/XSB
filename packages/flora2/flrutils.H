/* File:      flrutils.H
**
** Author(s): Guizhen Yang
**
** Contact:   flora-users@lists.sourceforge.net
**
** Copyright (C) The Research Foundation of SUNY, 1999-2001
** 
** FLORA-2 is free software; you can redistribute it and/or modify it under the
** terms of the GNU Library General Public License as published by the Free
** Software Foundation; either version 2 of the License, or (at your option)
** any later version.
** 
** FLORA-2 is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
** FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
** more details.
** 
** You should have received a copy of the GNU Library General Public License
** along with FLORA-2; if not, write to the Free Software Foundation,
** Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
**
** $Id$
**
*/


:- compiler_options([xpp_on]).

#include "flora_porting.flh"

:- import
	flora_slash/1,
	flora_cputime/1,
	flora_set_counter/2,
	flora_concat_atoms/2
   from flrporting.

:- import format/2 from format.

:- import member/2, append/3 from basics.

:- import
	current_loaded_file/1
    from consult.

:- import library_directory/1 from usermod.

:- import xsb_configuration/2 from xsb_configuration.

:- import xpp_options/1, xpp_process_file/3 from parse.

:- import current_predicate/1 from curr_sym.

:- import package_configuration/2 from packaging.

:- import
	parse_filename/4,
	stat_flag/2,
	stat_set_flag/2
   from machine.

:- import
	file_close/1,
	print_file_contents/1,
	copyIOport/2,
	path_sysop/2,
	path_sysop/3
   from file_io.

:- import process_control/2 from shell.

:- import xpp_process_file/3 from parse.

:- import 
	FLORA_DB_INSERT_PREDICATE/3,
	FLORA_DB_COMMIT_PREDICATE/1,
	FLORA_DB_RECLAIM_SPACE_PREDICATE/1
   from storage.

:- import bootstrap_flora/0 from flora2.

:- import
	flora_reset_lexer/0,
	flora_reset_lexer/1,
	flora_tokens/2,
	flora_token_text/6,
	flora_symbol_token_struct/6,
	flora_rule_delimeter_struct/6,
	flora_blank_line/1,
	flora_current_compile_filename/1
   from flrlexer.

:- import
	flora_reset_composer/0,
	flora_compose/3,
	flora_nth_token/2
   from flrcomposer.

:- import flora_reset_operator/0 from flroperator.

:- import flora_reset_arguments/0 from flrarguments.

:- import flora_reset_prolog/0 from flrprolog.

:- import flora_reset_firstorder/0 from flrfirstorder.

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

%% support for undefinedness
:- import 
	flora_debug_code/2
   from flrundefined.

:- import plg2hlg/3 from prolog2hilog.

:- import
	flora_stderr_nl/0,
	flora_stderr_string/1,
	flora_stderr_string/2,
	flora_stdwarn_nl/0,
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

:- import
	flora_include/2,
	flora_sysmod_file/3
   from flrlibman.

:- import prettyprint_stdin/0 from flrprettyprint.

:- import
	flora_firstorder_user_module_predicate_symbol/3,
	flora_module_name/3,
	flora_module_name_error/1,
	flora_fdb_storage_name/2,
	%% support for undefinedness
	flora_fld_storage_name/2
   from flrwrapper.

:- import check_for_dependencies/2 from flrdependency.

:- dynamic flora_maxerr(_).
:- dynamic flora_xpp_extra_options/1.
:- dynamic flora_module_registry/1.

:- dynamic flora_debug_module_registry/1.
:- dynamic flora_in_debug_mode/1.

:- dynamic flora_storage_registry/1.

:- index(flora_maxerr/1,trie).
:- index(flora_debug_module_registry/1,trie).
:- index(flora_storage_registry/1,trie).
:- index(flora_module_registry/1,trie).
:- index(flora_in_debug_mode/1,trie).

:- export
	flMaxerr/1,
	flHalt/0,
	flora_compile_internal/1,
	flora_compile_internal/2,
	flora_compile_system_module/1,
	flora_load_module_internal/1,
	flora_load_module_internal/2,
	flP2H/2,
	flDump/1,
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
	flora_set_xpp_options_for_compile/0,
	flora_set_xpp_options_for_compile/1,
	flora_set_xpp_options_for_compile/2,
	flora_set_xpp_options_for_read/0,
	flora_set_xpp_options_for_read/1,
	flora_set_xpp_options_for_read/2,
	flora_clear_xpp_options/0,
	flora_list2conjunct/2,
	flora_check_workspace/1,
	flora_module_registry/1,
	%% support for undefinedness
	flora_debug_module_registry/1,
	flora_in_debug_mode/1,

	flora_storage_registry/1,
	flora_reclaim_storage_space/0,
	flora_commit_storage/0,
	flora_load_dyn_fdb/2,
	%% support for undefinedness
	flora_load_dyn_fld/2,

	flora_read_canonical_and_insert/2,
	flora_compile_system_module_by_ID/1,
	flora_load_system_module_internal/1,
	flora_measure_time/2,
	flCompileInclude/1.
