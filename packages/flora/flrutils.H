/* File:      flrutils.H
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


:- import
	put/1, write/1, tab/1, writeln/1, write_canonical/1, display/1,
	atom_codes/2, see/1, seen/0, tell/1, told/0, seeing/1, telling/1,
	file_exists/1, cputime/1, var/1
   from standard.

:- import append/3, member/2 from basics.

:- import
	consult/1, consult/2, compile/2, load_dyn/1
   from consult.

:- import sort/2, findall/3 from setof.

:- import package_configuration/2 from packaging.

:- import bootstrap_flora/0 from flora.

:- import library_directory/1 from usermod.

:- import trie_assert/1, trie_retract_all/1 from tables.

:- import slash/1, is_absolute_filename/1, parse_filename/4 from machine.

:- import fmt_write_string/3, file_time/2 from file_io.

%%:- import unix/1 from unix.

:- import read_tokens/2 from flrtokens.

:- import rule/3 from flrparser.

:- import set_load_mode/1, compile_flogic/3 from flrcompiler.

:- dynamic flora_maxerr(_).

:- export 
	'_$_$_flora_flcompile'/5,
	'_$_$_flora_read_parse_compile'/6,
	'_$_$_flora_loadlist'/1,
	'_$_$_flora_dynloadlist'/1,
	'_$_$_flora_check_filename'/1,
	'_$_$_write_flora_warnheader'/0,
	'_$_$_flora_discard_tokens'/1,
	flcompile/1, flcompile/2, flconsult/1, flconsult/2, dyncompile/1,
	dyncompile/2, dynconsult/1, dynconsult/2, flload/1, dynload/1,
	maxerr/1, flora_prefix/1, flora_word/2, terminate_list/1,
	flora_member/2, displayln_errmsg/1, displayln_warning/1,
	displayln/0, displayln/1, flora_msgheader/0.

