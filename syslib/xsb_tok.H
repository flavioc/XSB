/* File:      xsb_tok.H
** Author(s): Saumya K. Debray, Jiyang Xu, Kostis F. Sagonas
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



:- export read_next_token/4, char_blank/1.


:- import psc_name/2, file_get/2, term_psc/2, 
	  stat_flag/2, buff_word/3, buff_set_word/3, buff_byte/3
     from machine.
:- import str_fromlist/2 from string.
:- import file_write0/2, file_nl/1 from xsb_writ.


/* ------------------- end of file file_token.H ------------------- */
