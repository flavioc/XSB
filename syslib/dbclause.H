/* File:      dbclause.H
** Author(s): Kostis Sagonas, David S. Warren
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

	  
:- export db_retract/3, db_retract_nr/3, db_reclaim_space/2.
:- export db_clause/4, db_retractall/2, db_abolish/1, '_$clause_int'/1.
:- export db_get_clauses_in_list/8.

:- import asserti/2, (dynamic)/1 from assert.
:- import abort/1, atom/1, call/1, repeat/0, is_most_general_term/1
     from standard.
:- import set_inthandler/2, '_$load_undef'/1 from loader.
:- import stat_set_flag/2, code_call/3, conname/2,
          gen_retract_all/1, buff_word/3, buff_cell/3, buff_alloc/2, 
	  buff_set_byte/3, buff_set_cell/4, buff_set_var/4, buff_byte/3,
          db_get_clause/11, db_retract0/2, db_remove_prref/1, db_reclaim0/2
     from machine.

