/* File:      file_op.H 
** Author(s): Kostis F. Sagonas, David S. Warren
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


:- export '_$op'/3, add_ops/3, op_priority/1, op_specifier/1.
:- export add_index/3, add_trie_index/1,  '_$index'/3 , '_$trie_asserted'/1.
:- export add_table/1, '_$table'/1.
:- export force_file_op_loaded/0.
:- export dyn_op/3.	% make it local (yup, strange, ain't it.)

:- import default_assert_size/1 from config.
:- import integer/1, functor/3 from standard.
:- import assert/1, assertai/2, retract/1, retractall/1, (dynamic)/1 from assert.

:- index '_$op1'/3-3.

/*
:- export fx/0, fy/0, yf/0, xf/0, xfx/0, xfy/0, yfx/0, yfy/0.
*/

/* --------------------- end of file file_op.H ------------------------ */
