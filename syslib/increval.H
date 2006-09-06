/* File:      increval.H
** Author(s): Diptikalyan Saha, C. R. Ramakrishnan
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
** $$
** 
*/
:- import
	term_psc/2
	from machine.
:- import
assert/1,retract/1,retractall/1 
	from assert.
:- import 
   length/2, append/3, member/2
         from basics.
:- import 
         findall/3
         from setof.

:- import get_calls/3, get_returns/2, get_call/3, abolish_table_pred/1 
         from tables.

:- import '\='/2,str_cat/3,'=..'/2,call/1,cputime/1,writeln/1 from standard.



:- export
   update/1,
   incr_statistics/0,
   get_call_graph/0,
   incr_assert/1,
   incr_retract/1,
   invalidate_call/1,
   print_call/1,
   psc_set_incr/2,
   imm_depends/2,
   trans_depends/2.

   
