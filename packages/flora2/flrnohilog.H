/* File:      flrnohilog.H
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


:- index flora_nohilog/2-1.

:- dynamic flora_dynnohilog(_,_).

:- dynamic flora_shnohilog(_,_).

:- index(flora_dynnohilog/2,trie).

:- index(flora_shnohilog/2,trie).

:- import assert/1, retractall/1 from assert.

:- import conget/2 from gensym.

:- export flora_nohilogdef/2,
          flora_reset_nohilog/0,
	  flora_define_nohilog/2,
          flora_shreset_nohilog/0,
	  flora_shdefine_nohilog/2.
