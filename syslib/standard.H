/* File:      standard.H
** Author(s): Sagonas, Warren, Kifer, Cui, Demoen, Swift and others
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
*/


/* ------------------------------ */
/*  exported built-in predicates  */
/* ------------------------------ */


:- export 
	arg/3,		arg0/3,		atom/1,		
            atom_concat/3,   atomic/1,
	goal_arg/3,	atom_chars/2,   atom_codes/2,
	call/1,		call_expose/1,	callable/1,
	close/1,	compare/3,	compound/1,	cputime/1,	
            display/1, now/1, datime/1,
            error_write/1, error_writeln/1,
	expand_term/2,	file_exists/1,	fail/0,		fail_if/1,
	fileerrors/0,	float/1,	functor/3,
	get/1,		get0/1,		
        get_char/1,	get_char/2,     get_code/1,	get_code/2,
        halt/0,		
	ceil/2, floor/2, round/2,
	(hilog)/1,	hilog_arg/3,	hilog_functor/3,
	(index)/3,	(index)/2,      (table)/1,
	integer/1,	is/2,		is_attv/1,	is_list/1,
	is_most_general_term/1,		
	listing/0,	listing/1,	name/2,		nl/0, nl/1,
	nofileerrors/0, nonvar/1,	(not)/1,	number/1,
	number_chars/2, number_codes/2, number_digits/2,
  once/1,		op/3,		open/3,		open/4,
%	print/1,	
            otherwise/0,  proper_hilog/1,	
            put/1,          put_char/1,     put_char/2,     put_code/1,
            put_code/2,
            read/1,		read/2,             read_term/3,    read_term/2,
            real/1,		repeat/0,
	see/1,		seeing/1,	seen/0,
            set_input/1,    set_output/1,  
	demand_once/1,
	skip/1,		statistics/1,	statistics/0,	structure/1,
	tab/1,		tell/1,		tell/2,		telling/1,	told/0,
	ttywrite/1,	ttywritenl/1,   ttywrite/2,	ttywritenl/2, 
	var/1,		walltime/1,	true/0,
	write/1,	write/2,	write_prolog/1,	write_prolog/2,
	writeln/1,	writeln/2,	writeq/1,	writeq/2,
	write_canonical/1,  write_canonical/2,
	('\+')/1,	'=:='/2,	'=\='/2,	'<'/2,
	'>'/2,		'=<'/2,		'>='/2,		'='/2,
	'\='/2,		'=..'/2,	'^=..'/2,	'=='/2, 	'@='/2,
	'\=='/2,	'@<'/2,		'@>'/2,		'@=<'/2,
	'@>='/2,	';'/2,		','/2,		'->'/2,
	':'/2,		'_$cutto'/1,	'_$savecp'/1.	


/* --------------------------------------------------------------- */
/*  exported predicates used in other modules (but not "standard")  */
/* --------------------------------------------------------------- */

:- export
        call_c/1,	        
        catch/3,                throw/1,        '$$exception_ball'/1,
        goal_cut_trans/3,	goal_cut_untrans/2,
	tcall/1,	arity_integer/1,	prolog_arglist/4,
	message/1,      message/2, messageln/2,
        warning/1,      abort/1,	abort/0,
	set_timer/1,
	(import)/1,	(multifile)/1.


:- import (dynamic)/1, asserta/1 from assert.
:- import member/2,append/3 from basics.
:- import consult/1, reconsult/1 from consult.
:- import predicate_property/2 from curr_sym.
:- import dcg/2 from dcg.
:- import eval/2 from eval.
:- import file_open/3, file_close/1, file_get/2, file_put/2,
  check_stream_or_alias/4, check_open_stream_or_alias/5, add_alias/2
     from file_io.
:- import add_ops/3, op_priority/1, op_specifier/1, add_index/3,
          add_trie_index/1, add_table/1
     from file_op.
:- import '_$hilog_symbol'/1, add_hilog_symbol/1 from hilogsym.
:- import retract0/1, assertz/1, clause/2, retractall/1 from assert.
:- import domain_error/4, check_atom/3, check_nonvar/3,type_error/4,
  permission_error/4, instantiation_error/3
     from error_handler.
:- import  check_nonvar_list/3 from error_handler.
:- import '_$load_undef'/1 from loader.
:- import windows_os_loader/0 from loader.

:- import call0/1, conname/2,
	  expand_filename/2, machine_file_exists/1, 
	  psc_arity/2, psc_import/3, psc_insert/4,
	  psc_name/2, psc_type/2,
	  stat_set_flag/2, stat_flag/2, term_compare/3,
	  stat_sta/1, stat_cputime/1, stat_walltime/1,
          term_psc/2, term_new/2, term_new_mod/3, term_type/2  %%, close_open_tables/0
	  from machine.
:- import '$$set_scope_marker'/0, '$$unwind_stack'/0, 
	  '$$clean_up_block'/0,
	  xsb_backtrace/1
     from machine. 
:- import numbervars/1 from num_vars.
:- import calculate_singleton/3 from singlton.
:- import concat_atom/2 from string.
:- import file_read/2, file_read/3 from xsb_read.
:- import file_write0/2, file_write/2, file_write_prolog/2, 
	  file_write_quoted/2, file_write_canonical/2, file_nl/1
     from xsb_writ.



/* --------------------- end of file standard.H ----------------------- */
