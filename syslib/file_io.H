/* File:      file_io.H
** Author(s): kifer
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1999
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
	stat_flag/2, file_function/7,
    	file_stat/3, formatted_io/5, intern_string/2
      from machine.

:- import abort/1, atom_codes/2,
	compound/1, callable/1
      from standard.

:- import file_write/2 from xsb_writ.

:- export fmt_write_string/3, fmt_write/2, fmt_read/3,
	  fmt_write/3, fmt_read/4,
	  print_file_contents/1,
	  print_file_contents/2,
	  file_flush/2, file_seek/4, file_truncate/3,
	  file_pos/2, file_size/2, file_time/2,
	  file_open/3, file_close/1, file_get/2, file_put/2,
	  file_getbuf/4,
	  file_getbuf_atom/4, file_getbuf_atom/3,
	  file_getbuf_list/4, file_getbuf_list/3,
	  file_putbuf/5, file_putbuf/4,
	  file_write_line/2, file_write_line/3,
      	  file_read_line/3,
	  file_read_line_atom/2, file_read_line_atom/3,
	  file_read_line_list/2, file_read_line_list/3,
	  file_reopen/4, file_clone/3.
