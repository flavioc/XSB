/* File:      flrfirstorder.H
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
** $Id$
**
*/


:- index flora_firstorder/2-1.

:- dynamic flora_dynfirstorder(_,_).

:- dynamic flora_shfirstorder(_,_).

:- dynamic flora_dynflogicstatic(_,_).

:- index(flora_dynfirstorder/2,trie).

:- index(flora_shfirstorder/2,trie).

:- index(flora_dynflogicstatic/2,trie).

:- import flora_get_counter/2 from flrporting.

:- export
	flora_firstorderdef/2,
	flora_reset_firstorder/0,
	flora_define_firstorder/2,
	flora_shreset_firstorder/0,
	flora_shdefine_firstorder/2,
	flora_flogicstaticdef/2,
	flora_define_flogicstatic/2.
