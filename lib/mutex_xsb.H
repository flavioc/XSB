/* File:      xsb_mutex.H
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
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
** 
*/

:- export mutex_create/1, mutex_lock/1, mutex_unlock/1,
          mutex_trylock/1, mutex_destroy/1, with_mutex/2.
:- export mutex_unlock_all, mutex_property/2.

:- import assert/1, (dynamic)/1, retractall/1 from assert.
:- import check_atom/3, existence_error/4, permission_error/4, type_error/4 
          from error_handler.
:- import gensym/2 from gensym.
:- import thread_request/8 from machine.
:- import catch/3, (index)/2, throw/1 from standard.


