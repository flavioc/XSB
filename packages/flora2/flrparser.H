/* File:      flrparser.H
**
** Author(s): Guizhen Yang
**
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


:- import integer/1 from standard.

:- import append/3 from basics.

:- import conget/2 from gensym.

:- import
	flora_begin_file/1,
	flora_end_file/1,
	flora_reset_charpos/0
   from flrlexer.

:- import flora_conflict_operator/3 from flroperator.

:- import flora_nodefp/2 from flrnodefp.

:- import flora_nowsp/2 from flrnowsp.

:- import flora_prlgdef/2 from flrprolog.

:- import flora_argdef/3, flora_conflict_arguments/3 from flrarguments.

:- import flora_system_module/1 from flrlibman.

:- export flora_parse/3.
