/* File:      flrcompiler.H
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
** $Id$
**
*/


:- import member/2, append/3, length/2 from basics.

:- import assert/1, retractall/1 from assert.

:- import sort/2, keysort/2, findall/3 from setof.

:- import conset/2, conget/2 from gensym.

:- import str_cat/3, str_sub/3 from string.

:- import flora_define_operator/3 from flroperator.

:- import flora_argdef/3, flora_define_arguments/3 from flrsignature.

:- import flora_define_prolog/2 from flrprolog.

:- import flora_define_firstorder/2 from flrnohilog.

:- export flora_reset_compiler/0, flora_compile/4.

