/* File:      unix.H
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


:- export unix/2, cd/1, unix/1, ls/0, rename/2, rm/1, edit/1,
	spawn_process/3.
:- export sys_chdir/2, sys_access/3, sys_stat/3, sys_link/3.

:- import str_cat/3 from string.
:- import atom/1, abort/1 from standard.
:- import psc_name/2, psc_set_type/2, psc_set_prop/2, conpsc/2,
	psc_type/2, psc_prop/2,
	sys_system/4, term_psc/2, term_type/2, expand_filename/2
    from machine.

