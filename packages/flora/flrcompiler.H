/* File:      flrcompiler.H
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
/* For CPP
    #define STD_FPREFIX(X)    '_$_$_flora_' ## X
*/
#define STD_FPREFIX(X)    '_$_$_flora_''X'


:- import
	member/2, append/3, length/2
   from basics.


:- import fmt_write_string/3 from file_io.

:- import trie_dynamic/1, trie_assert/1, trie_retract/1,
	  trie_retract_all/1
   from tables.

:- import
	conget/2, conset/2
   from prof_sbp.

:- import flora_set/1 from flrparser.

:- import
	flora_member/2, flora_word/2, terminate_list/1, 
	flora_error_nl/1, STD_FPREFIX(warning_nl)/1,
	STD_FPREFIX(warning_nl)/2
   from flrutils.

:- dynamic compiler_options(_).

:- trie_dynamic(compiler_options(_)).

:- dynamic load_mode(_).

:- trie_dynamic(load_mode(_)).

:- export init_flrcompiler/0, set_load_mode/1, compile_flogic/3.

