/* File:      numbervars.H
** Author(s): Kostis F. Sagonas, Jiyang Xu, David S. Warren
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


:- export numbervars/1, numbervars/3.
:- export unnumbervars/2, unnumbervars/3.

:- import '>'/2, '=<'/2, '>='/2, '=:='/2, is/2 from standard.
:- import term_psc/2, term_type/2, psc_arity/2, term_arg/3, term_new/2
        from machine.
:- import log_ith/3 from basics.
