/* File:      basics.H
** Author(s): David S. Warren, Kostis F. Sagonas
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


:- export append/3, copy_term/2, flatten/2, ground/1, ith/3, length/2, 
	  member/2, memberchk/2, subset/2, reverse/2, select/3.
:- export log_ith/3, log_ith_bound/3.

:- import term_type/2 from machine.
:- import integer/1, is_list/1 from standard.
