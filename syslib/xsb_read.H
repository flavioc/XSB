/* File:      xsb_read.H
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


:- export file_read/2, file_read/3, string_as/1, vv/2, 'syntax errors'/0.
:- export file_read_foe/3.
:- export fmt_read/4, read_line/2.


:- import '_$op'/3 from file_op.
:- import str_tolist/2 from string.
:- import file_write0/2, file_nl/1 from xsb_writ.
:- import atom/1, repeat/0, abort/1, callable/1, compound/1 from standard.
:- import '_$hilog_symbol'/1 from hilogsym.
:- import file_put/2, term_new/2, term_set_arg/4, term_type/2, 
	  file_gettoken/5, psc_insert/4, stat_set_flag/3, conname/2, 
	  conpsc/2, str_insert/2, term_psc/2, psc_set_prop/3, psc_prop/2,
     	  stat_flag/2, fmt_read/5, file_read_line/3
     from machine.


/* -------------------- end of file file_read.H ----------------------- */
