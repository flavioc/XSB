/* File:      gensym.H 
** Author(s): Kostis F, Sagonas, Jiyang Xu 
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1999
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


:- export gensym/2, gennum/1, prepare/0.
:- export copyterm/2, conlength/2.
:- export conset/2, conget/2, coninc/1.
:- export newcounter/1, inccounter/1, getcounter/1.

:- import append/3 from basics.
:- import dict_lookup/3 from auxlry.
:- import psc_set_prop/2, conpsc/2, conname/2,psc_prop/2, str_len/2,
 	  psc_set_type/2
     from machine.
:- import name/2, arg/3, functor/3, atomic/1,
	  integer/1, atom_codes/2
     from standard.
