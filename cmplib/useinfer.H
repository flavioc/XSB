/* File:      useinfer.H
** Author(s): Kostis Sagonas, Jiyang Xu, David S. Warren
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


:- export useinfer/3.

:- import standard_metapredicate/3 from std_xsb.
:- import member/2, ith/3 from basics.
:- import warning/1, option/1 from auxlry.
:- import standard_symbol/3 from std_xsb.
:- import sym_propin/2, sym_prop/2, sym_insert/5, sym_scan/3,
	  extract_symlist/3, sym_env/2, sym_name/3
    from symtab.

