/* File:      tp_cond.H
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


:- export tcond/10, tvar_hold/3, force_tp_cond_loaded/0.

:- import warning/1 from auxlry.
:- import gennum/1 from prof_sbp.
:- import vprag_loc/2, vprag_type/2 from varproc.
:- import eval_relop/11, arithrelop/1 from tp_eval.
:- import geninl_load_lhs/7, geninline/9 from tpinline.
:- import term_comp_op/1, term_comp_tab/4 from tp_comp.
:- import release_if_done/6, alloc_reg/3, release_if_done0/5, hold/3,
	  getreg/2, putvarinst/5
     from tp_var.
