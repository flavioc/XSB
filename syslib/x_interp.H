/* File:      x_interp.H
** Author(s): David Warren, Terrance Swift, Jiyang Xu
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


:- export break/0, prompt/3.



:- import set_inthandler/2 from loader.

:- import
       abort/0, call_c/1, call_expose/1, goal_cut_trans/3, repeat/0,
       see/1, seen/0, call/1
   from standard.

:- import
       psc_type/2, psc_set_type/3, psc_prop/2, psc_set_prop/3,
       psc_arity/2, term_psc/2, term_arg/3, term_new/2,
       term_type/2, conpsc/2, conname/2, file_get/2,
       stat_flag/2, stat_set_flag/3, str_insert/2, is_xwammode/1,
       close_open_tables/0, trimcore/0, 
       file_read_line/2, slash/1, fmt_write_string/3,
       intern_string/2, file_open/3, file_close/1
   from machine.

:- import file_write/2, file_nl/1 from xsb_writ.
:- import file_read/3, vv/2, file_read/2 from xsb_read.

:- import numbervars/1 from num_vars.

:- import xsb_configuration/2 from configuration.
:- import [] / 0 from usermod.	       % just to make usermod typed "module"
