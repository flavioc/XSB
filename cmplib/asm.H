/* File:      asm.H
** Author(s): Saumya Debray, Kostis Sagonas, Terrance Swift
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


:- export asm_genheader/4, asm_PIL/4, asm_magic/1.

:- import insert/2 from listutil.
:- import asm_opt/5 from asm_opt.
:- import asm_inst/4 from asm_inst.
:- import term_hash/3, str_len/2 from machine.
:- import ll_arg/3, write_int/1 from prof_sbp.
:- import functor/3, write/1, put/1, tell/1 from standard.
:- import asm_gendata/1, asm_genindex/2, asm_gentext/2, asm_putlength/1
     from asmpass2.
:- import sym_name/3, sym_count/2, sym_propin/2, sort_by_index/2 from symtab.

