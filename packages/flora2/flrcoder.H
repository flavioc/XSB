/* File:      flrcoder.H
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


:- import write_canonical/1, writeln/1 from standard.

:- import append/3 from basics.

:- import eval/2 from eval.

:- import str_cat/3 from string.

:- import
	flora_firstorderdef/2,
	flora_flogicstaticdef/2
   from flrnohilog.

:- import flora_prlgdef/2 from flrprolog.

:- import
	flora_user_module_predicate/4,
	flora_user_module_predicate_symbol/3,
	flora_system_module_predicate/4,
	flora_firstorder_user_module_predicate_symbol/3,
	flora_firstorder_user_module_predicate/4,
	flora_firstorder_system_module_predicate/4,
	flora_user_storage_name/2
   from flrwrapper.

:- export
	flora_divide_program/3,
	flora_extern_code/2,
	flora_intern_code/4.
