/* File:      string.H
** Author(s): Xu, Swift, Sagonas
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


:- export concat_atom/2, concat_atom/3, str_cat/3, str_sub/2, str_sub/3,
   str_fromlist/2, str_length/2, str_substring/4, str_substring/2, 
   str_tolist/2, sublist/2.

:- import atom/1 from standard.
:- import buff_set_byte/3, buff_alloc/3, buff_byte/3, buff_dealloc/4,
	  str_insert/2, str_len/2 from machine.
%:- import c_min_ed_dis/3,c_substring/3 from xeddis.

/* ---------------------- end of file string.H ------------------------ */
