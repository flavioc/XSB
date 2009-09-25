/* File:      cp_opt.H
** Author(s): Terrance Swift
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
** $Id$
** 
*/


:- export cp_opt/2.
:- export aggregation_transform/2.

:- import nthmember/3 from listutil.
:- import parse_preddef/4 from parse.
:- import have_hardcut/2 from preproc.
:- import index_split/2 from tp_index.
:- import member/2, append/3, length/2, copy_term/2 from basics.
:- import functor/3, arg/3, atom_codes/2, '=..'/2 from standard.
:- import sym_type/2, sym_name/3, sym_index/2, sym_insert/5 from symtab.
:- import warning/1 from auxlry.
