/* File:      xsb_writ.H
** Author(s): Kostis F. Sagonas, Kostis F. Sagonas
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


:- export file_write/2, file_write0/2, file_write_prolog/2, 
	  file_write_quoted/2, file_write_canonical/2, fmt_write/2,
	  fmt_write_string/3, alt_file_write_canonical/2, file_nl/1.

:- import '_$op'/3 from file_op.
:- import atom/1, integer/1, hilog_op/3, proper_hilog/1, 
     	  abort/1, callable/1
   from standard.

:- import file_put/2, term_psc/2, psc_arity/2, psc_name/2, term_arg/3, 
	  term_type/2, file_puttoken/3, file_writequoted/2,
 	  is_backslashing_os/0, fmt_write/3, stat_flag/2
     from machine.


/* ------------------- end of file file_write.H ----------------------- */
