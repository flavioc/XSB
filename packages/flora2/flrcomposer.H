/* File:      flrcomposer.H
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


:- dynamic indexed_token(_,_).
:- dynamic composing_error(_,_).
:- dynamic composing_error(_,_,_).

:- index(indexed_token/2,trie).
:- index(composing_error/2,trie).
:- index(composing_error/3,trie).

:- import findall/3 from setof.

:- import retractall/1, assert/1 from assert.

:- import eval/2 from eval.

:- import conget/2, conset/2 from gensym.

:- import flora_opdef/3 from flroperator.

:- export
	flora_reset_composer/0,
	flora_compose/3,
	flora_nth_token/2.
