/* File:      flrwrapper.H
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
**
*/


:- import assert/1, retractall/1 from assert.

:- import fmt_write_string/3 from file_io.

:- import str_sub/3, substring/4, str_length/2 from string.

:- import eval/2 from eval.

:- import reverse/2 from basics.

:- dynamic flora_flogic_bswrapper(_,_).
:- dynamic flora_flogic_workspace(_).

:- index(flora_flogic_bswrapper/2,trie).
:- index(flora_flogic_workspace/1,trie).

:- import flora_check_workspace/1 from flrutils.

:- export
	flora_setup_flogic_bswrapper/1,
	flora_flogic_bswrapper/2,
	flora_flogic_predicate/4,
	flora_hilog_predicate/2,
	flora_nohilog_predicate/3,
	flora_wspredicate/4,
	flora_is_general_predicate/2,
	flora_storage_name/2.
