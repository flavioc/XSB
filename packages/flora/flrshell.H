/* File:      flrshell.H
**
** Author(s): Guizhen Yang
** Contact:   xsb-contact@cs.sunysb.edu
**
** Copyright (C) The Research Foundation of SUNY, 1999
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

:- compiler_options([xpp_on]).

#mode save
#mode nostring "\!#'"
#define STD_FPREFIX(X)    '_$_$_flora_''X'
#mode restore


:- import
	write/1, writeln/1, repeat/0, atom_chars/2, abort/0,
	cputime/1, push_abort_cutpoint/0, pop_abort_cutpoint/0,
	file_exists/1
   from standard.

:- import member/2, ground/1 from basics.

:- import trie_assert/1, trie_retract_all/1 from tables.

:- import slash/1 from machine.

:- import fmt_write_string/3 from file_io.

:- import file_write/2, file_nl/1 from xsb_writ.

:- import current_predicate/1 from curr_sym.

:- import consult/2, compile/1, compile/2, search_module/6 from consult.

:- import conget/2, conset/2 from prof_sbp.

:- import
	bootstrap_package/2, package_configuration/2, unload_package/1
   from packaging.

:- import bootstrap_flora/0 from flora.

:- import
	flora_read_parse_compile/6,
	flora_flcompile/5,
	STD_FPREFIX(loadlist)/1,
	STD_FPREFIX(dynloadlist)/1,
	flora_check_filename/1,
	flora_discard_tokens/1,
	flcompile/1, flcompile/2, maxerr/1, flconsult/1, flconsult/2,
	flload/1, dynload/1, dyncompile/1, dyncompile/2, dynconsult/1,
	dynconsult/2, STD_FPREFIX(warning_nl)/1,
	STD_FPREFIX(warning_nl)/2,
	flora_display_msg/1, flora_display_msg/2
   from flrutils.

:- import glob_directory/4 from wildmatch.

:- import STD_FPREFIX(debugger_data_loaded)/0 from usermod.

%%:- import STD_FPREFIX(trace)/0, STD_FPREFIX(notrace)/0 from flrdebugger.

:- dynamic STD_FPREFIX(switch)/1, STD_FPREFIX(debugger_data_loaded)/0.
