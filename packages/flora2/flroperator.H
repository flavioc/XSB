/* File:      flroperator.H
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


:- dynamic flora_dynop(_,_,_).

:- dynamic flora_shop(_,_,_).

:- index(flora_dynop/3,trie).

:- index(flora_shop/3,trie).

:- index flora_op/3-3.

:- import assert/1, retractall/1 from assert.

:- import conget/2 from gensym.

:- export
	flora_opdef/3,
	flora_conflict_operator/3,
	flora_reset_operator/0,
	flora_define_operator/3,
	flora_shreset_operator/0,
	flora_shdefine_operator/3.
