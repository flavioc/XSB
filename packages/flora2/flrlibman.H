/* File:      flrlibman.H
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


:- import flora_syslib_loaded/1 from usermod.
:- dynamic flora_syslib_loaded(_).
:- index flora_syslib_loaded/1-1.

:- import flora_sysmod_loaded/1 from usermod.
:- dynamic flora_sysmod_loaded(_).
:- index flora_sysmod_loaded/1-1.

:- import consult/1 from consult.

:- import assert/1 from assert.

:- import package_configuration/2 from packaging.

:- import fmt_write_string/3 from file_io.

:- import slash/1 from machine.

:- import bootstrap_flora/0 from flora2.
:- bootstrap_flora.

:- index flora_sysmod_file/3-1.
:- index flora_library_file/3-1.

:- import flora_error_line/1,
	flora_error_line/2
   from flrprint.

:- import flora_load_system_module_internal/1 from flrutils.

:- export
	flora_load_library/1,
	flora_include/2,
	flora_sysmod_file/3,
	flora_system_module/1,
	flora_load_system_module/1.
