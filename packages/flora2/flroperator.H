/* File:      flroperator.H
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


:- dynamic flora_dynop(_,_,_).

:- dynamic flora_shop(_,_,_).

:- index(flora_dynop/3,trie).

:- index(flora_shop/3,trie).

:- index flora_op/3-3.

:- import flora_get_counter/2 from flrporting.

:- export
	flora_opdef/3,
	flora_conflict_operator/3,
	flora_reset_operator/0,
	flora_define_operator/3,
	flora_shreset_operator/0,
	flora_shdefine_operator/3.
