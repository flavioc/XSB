/* File:      debugger.H
** Author(s): David S. Warren
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


/* Trace and Debug package */

:- export debug/0, nodebug/0, dump/0, trace_stack_max/0, notrace_stack_max/0,
	  (spy)/1, (nospy)/1, trace/0, notrace/0, piltrace/0, hitrace/0,
	  nosimtrace/0, debugging/0.
:- export '_$deb_trace'/1, '_$deb_spy'/1, '_b_SPY_condition'/1,
	show/1, leash/1.

:- import set_inthandler/2, '_$load_undef'/1, load_pred/1
     from loader.
:- import file_write/2 from xsb_writ.
:- import =.. /2, functor/3, atom/1, warning/1, abort/0, abort/1, (import)/1
     from standard.
:- import (dynamic)/1, assert0/1, retract/1, retractall/1 from assert.
:- import break/0, prompt/3 from x_interp.
:- import term_psc/2, psc_insert/4, stat_flag/2, stat_set_flag/3, 
	  psc_type/2, psc_prop/2, file_get/2, file_put/2, 
	  buff_byte/3, buff_set_byte/3, conname/2, code_call/3,
	  psc_arity/2, psc_name/2, term_arg/3, term_type/2, term_new/2
     from machine.
:- import standard_symbol/3 from std_sbp.
:- import predicate_property/2, current_predicate/1 from curr_sym.
:- import findall/3 from setof.

% := hide debug/0.


/* --------------------- end of file debugger.H ----------------------- */
