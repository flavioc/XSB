/* File:      symtab.H
** Author(s): Kostis F. Sagonas, Jiyang Xu, David S. Warren
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


:- export sym_name/3, sym_offset/2, sym_propin/2, sym_prop/2,
	  extract_symlist/3, sym_empty/1, sym_env/2, sym_category/2, 
	  sym_type/2, sym_insert/5, sym_scan/3, sym_gen/2, 
	  sym_count/2, sort_by_index/2, sym_complete/1,
	  sym_index/2, sym_pass_thru/1, typ_prop/1, env_prop/1, dispose_symtab/1.
:- export add_new_clause/2, clause_listify/2.

:- import 'table #'/0 from compile.
:- import error/1, member1/2 from auxlry.
:- import integer/1, functor/3 from standard.
:- import membercheck/2, member2/2, closetail/1 from listutil.
:- import append/3, member/2, ll_arg/3, conget/2, conset/2 from prof_sbp.
:- import buff_alloc/3, buff_word/3, buff_set_word/3, term_hash/3,
	  backtrackable_buff_set_word/4, buff_dealloc/4 
     from machine.
:- import copy_term/2 from basics.

