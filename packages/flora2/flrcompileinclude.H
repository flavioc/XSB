/* File:      flrcompileinclude.P -- Workspace wrapper for .fli files
**
** Author(s): kifer
**
** Contact:   xsb-contact@cs.sunysb.edu
**
** Copyright (C) The Research Foundation of SUNY, 2001
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


:- import
	parse_filename/4,
	stat_set_flag/2
   from machine.
:- import 
	storage_find_fact/2,
	storage_insert_fact/3
   from storage.
:- import ','/2 from standard.
:- import
	flora_set_xpp_options/0,
	flora_clear_xpp_options/0,
	flora_check_filename/1,
	flora_locate_file/3,
	flora_FLH_filename/2
   from flrutils.
:- import
	flora_stderr_nl/0,
	flora_error_line/2
   from flrprint.
:- import member/2 from basics.
:- import process_control/2 from shell.
:- import xpp_process_file/3 from parse.
:- import
	fmt_write_string/3,
	path_sysop/2
   from file_io.

:- export flCompileInclude/1.

