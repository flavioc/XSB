/* File:      flrcompileinclude.P -- Workspace wrapper for .fli files
**
** Author(s): kifer
**
** Contact:   flora-users@lists.sourceforge.net
**
** Copyright (C) The Research Foundation of SUNY, 2001
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


:- compiler_options([xpp_on]).

#include "flora_porting.flh"

:- import
	parse_filename/4,
	stat_set_flag/2
   from machine.

:- import ','/2 from standard.

:- import
	flora_concat_items/2,
	flora_concat_atoms/2
   from flrporting.

:- import
	flora_set_xpp_options_for_read/0,
	flora_clear_xpp_options/0,
	flora_check_filename/1,
	flora_locate_file/3
   from flrutils.

:- import
	flora_stderr_nl/0,
	flora_error_line/2
   from flrprint.


:- import process_control/2 from shell.

:- import xpp_process_file/3 from parse.

:- import bootstrap_flora/0 from flora2.

:- export flWrapAround/1.

