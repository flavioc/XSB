/* File:      flrwrapper.H
**
** Author(s): Guizhen Yang
**
** Contact:   xsb-contact@cs.sunysb.edu
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
**
*/


:- import assert/1, retractall/1 from assert.

:- import fmt_write_string/3 from file_io.

:- import str_sub/3, substring/4 from string.

:- dynamic flora_flogic_fact_wrapper(_,_).
:- dynamic flora_current_flogic_module(_).

:- index(flora_flogic_fact_wrapper/2,trie).
:- index(flora_current_flogic_module/1,trie).

:- import flora_error_line/2 from flrprint.

:- export
	flora_user_module_predicate/4,
	flora_system_module_predicate/4,
	flora_module_predicate/4,

	flora_firstorder_user_module_predicate/4,
	flora_firstorder_system_module_predicate/4,
	flora_firstorder_module_predicate/4,

	flora_user_module_predicate_symbol/3,
	flora_system_module_predicate_symbol/3,
	flora_module_predicate_symbol/3,

	flora_hilog_user_module_predicate_symbol/2,
	flora_hilog_system_module_predicate_symbol/2,
	flora_hilog_module_predicate_symbol/2,

	flora_firstorder_user_module_predicate_symbol/3,
	flora_firstorder_system_module_predicate_symbol/3,
	flora_firstorder_module_predicate_symbol/3,

	flora_module_name/3,
	flora_check_module_name/1,
	flora_module_name_error/1,

	flora_user_storage_name/2,
	flora_system_storage_name/2,
	flora_storage_name/2,
	flora_user_fld_storage_name/2,
	flora_system_fld_storage_name/2,
	flora_fld_storage_name/2,
	flora_is_flogic_base/1,

	flora_setup_flogic_fact_wrapper/1,
	flora_flogic_fact_swrapper/2,

	flora_decode_predicate/6,
	flora_decode_module_name/2,
	flora_is_flogic_wrapper/3,

	flora_is_dynamic_flogic_base/2.
