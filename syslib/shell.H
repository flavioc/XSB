/* File:      shell.H
** Author(s): Kostis F. Sagonas, Jiyang Xu
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
** Copyright (C) ECRC, Germany, 1990
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


:- export cd/1, ls/0, rename/2, rm/1, edit/1,
	shell/5, shell/2, shell/1,
	spawn_process/5, get_process_table/1, process_status/2, 
	process_control/2.
:- export sys_chdir/2, sys_access/3, sys_stat/3, sys_link/3.

:- import atom/1, abort/1 from standard.
:- import sys_system/7, expand_filename/2, sys_syscall/4
   from machine.

