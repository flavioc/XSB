/* File:      error_handler.H
** Author(s): Swift
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

:- export domain_error/4, existence_error/4,
              instantiation_error/2,  % obsolescent
              instantiation_error/3,
               permission_error/4, type_error/4, 
              misc_error/1,
              check_atom/3,
              check_ground/3,
              check_nonvar_list/3, check_nonvar/3, 
              check_stream/3, 
              check_var/3, 
              default_error_handler/1,
	      default_sys_error_handler/1,
              get_sys_error_description/2,
              get_sys_error_description/3,
              print_backtrace/1,
              get_backtrace/2.

:- import (dynamic)/1 from assert.
 :- import ground/1 from basics.
%%:- import reset_undef_pred_handlers/0 from loader.
:- import close_open_tables/0, psc_data/2, psc_name/2, psc_arity/2,
             xsb_backtrace/1 from machine.
:- import atom/1, messageln/2, throw/1 from standard.
:- import concat_atom/2 from string.

