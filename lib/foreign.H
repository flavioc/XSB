/* File:      foreign.H
** Author(s): Kostis Sagonas, Jiyang Xu
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


:- export compile_cH/5.

:- import shell/1 from shell.
:- import str_sub/2 from string.
:- import memberchk/2 from basics.
:- import write_int/1 from prof_sbp.
:- import attach/2, length/2 from listutil.
:- import hdr_ext/1, obj_ext/1 from curr_sym.
:- import message/1, warning/1, time_message/2 from auxlry.
:- import str_len/2, str_cat/3 from machine.
:- import read/1, see/1, seen/0, write/1, put/1, tell/1, told/0,
	  atom/1, file_exists/1, cputime/1, abort/1, seeing/1,
          telling/1
     from standard.
:- import xsb_configuration/2 from xsb_configuration.
:- import write_header/0, process_from/2 from wrapping.
:- import file_close/1 from file_io.
:- import stat_flag/2 from machine.
:- import process_control/2 from shell.
:- import conset/2, conget/2 from prof_sbp.
:- import open_or_xpp_file/2 from parse.
:- import repeat/0 from standard.
