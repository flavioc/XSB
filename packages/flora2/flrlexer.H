/* File:      flrlexer.H
**
** Author(s): Guizhen Yang
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



:- import get0/1 from standard.

:- import append/3 from basics.

:- import conget/2, conset/2 from gensym.

:- import fmt_write_string/3 from file_io.

:- import parse_filename/4 from machine.

:- dynamic flora_file_stack/2.
:- index(flora_file_stack/2,trie).


:- export
	flora_reset_lexer/0,
	flora_reset_lexer/1,
	flora_reset_charpos/0,
	flora_tokens/2,
	flora_token_text/6,
	flora_symbol_token_struct/6,
	flora_rule_delimeter_struct/6,
	flora_blank_line/1,
	flora_begin_file/1,
	flora_end_file/1,
	flora_current_compile_filename/1.

