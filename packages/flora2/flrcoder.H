/* File:      flrcoder.H
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
** $Id$
**
*/


:- import write_canonical/1, writeln/1 from standard.

:- import append/3 from basics.

:- import eval/2 from eval.

:- import str_cat/3 from string.

:- import
	flora_firstorderdef/2,
	flora_flogicstaticdef/2
   from flrfirstorder.

:- import flora_prlgdef/2 from flrprolog.

:- import
	flora_user_module_predicate/4,
	flora_user_module_predicate_symbol/3,
	flora_system_module_predicate/4,
	flora_firstorder_user_module_predicate_symbol/3,
	flora_firstorder_user_module_predicate/4,
	flora_firstorder_system_module_predicate/4,
	flora_user_storage_name/2,
	%% Storage for skeletons of defined predicates
	flora_user_fld_storage_name/2
   from flrwrapper.

%% Undefinedness support
:- dynamic using_debug_prefix(_).

:- export
	flora_divide_program/3,
	flora_extern_code/2,
	flora_intern_code/4.

:- export
	is_prrule/3,
	is_prfact/2,
	is_prdirective/2,
	is_prtable/3,
	is_prhilogtable/2,
	is_prthismodule/1,
	is_prthisstorage/1,
	is_pratom/3,
	is_prnumber/3,
	is_prvariable/3,
	is_prstring/3,
	is_prlist/4,
	is_prstorage/3,
	is_skolem/5,
	is_prterm/4,
	is_goallogic/4,
	is_prcall/2,
	is_pratomlit/3,
	is_prtermlit/4,
	is_prworkspace/3,
	is_florasyslib/5,
	is_prologterm/4,
	is_prologliblit/4,
	is_prnewpred/4,
	is_prnewoid/3,
	is_flogic/4,
	is_fllibdb/1.
