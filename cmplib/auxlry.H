/* File:      auxlry.H
** Author(s): Saumya Debray, Kostis F. Sagonas
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


:- export member1/2, message/1, warning/1, error/1, time_message/2, 
	  setoptions/1, subsumes/2, dict_lookup/3, option/1,
	  debug_writenl/1, verbo_time_message/2, subsumed_member/2,
	  freeze/1, melt/2, writetop/2.

:- import numbervars/3 from num_vars.
:- import 'error occurred'/0 from compile.
:- import member/2, conget/2, conset/2 from prof_sbp.
:- import ttywritenl/1, cputime/1, arg/3, functor/3, atomic/1,
	  tell/1, nl/0, write/1, '=..'/2, telling/1
     from standard.

/* ------------------------ end of file auxlry.H ---------------------- */
