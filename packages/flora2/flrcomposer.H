/* File:      flrcomposer.H
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
**
*/


:- dynamic indexed_token(_,_,_).
:- dynamic composing_error(_,_).
:- dynamic composing_error(_,_,_).

:- index(indexed_token/3,trie).
:- index(composing_error/2,trie).
:- index(composing_error/3,trie).

:- import eval/2 from eval.

:- import 
	flora_set_counter/2,
	flora_get_counter/2
   from flrporting.

:- import flora_opdef/3 from flroperator.

:- export
	flora_reset_composer/0,
	flora_compose/3,
	flora_nth_token/2,
	flora_token_rulenum/2.
