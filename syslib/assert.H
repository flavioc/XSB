/* File:      assert.H
** Author(s): Kostis F. Sagonas, David S. Warren
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



:- export assert0/1, assert/1, asserta/1, assertz/1, 
	  asserti/2, assertai/2, assertzi/2.
:- export retract/1, retractall/1, abolish/1, abolish/2. 
:- export retract0/1.	% used in standard.P (for multifile)
:- export retract_nr/1, reclaim_space/1.
:- export clause0/2, clause/2.
:- export (dynamic)/1.
:- export t_assert/2.

:- import (=..)/2, functor/3, integer/1, callable/1,
	  goal_cut_trans/3, goal_cut_untrans/2, abort/1, arity_integer/1
     from standard.

:- import db_new_prref/3 from db.
:- import db_retract/3, db_retractall/2, db_abolish/2, db_clause/4,
	  db_retract_nr/3, db_reclaim_space/2
     from dbclause.
:- import '_$index'/3, '_$trie_asserted'/1  from file_op.
:- import psc_prop/2, psc_type/2, psc_set_type/3, psc_arity/2, buff_byte/3,
	  buff_word/3, conpsc/2,
	  assert_code_to_buff/2, assert_buff_to_clref/7,
	  compiled_to_dynamic/2,
	  psc_tabled/2, psc_set_prop/3, unload_seg/1
     from machine.

:- import standard_symbol/3 from std_sbp.

:- import default_assert_size/1 from config.
:- import '_$multifile_retract'/3 from parse.
