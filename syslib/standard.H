/* File:      standard.H
** Author(s): Kostis F. Sagonas, Jiyang Xu 
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


/* ------------------------------ */
/*  exported built-in predicates  */
/* ------------------------------ */


:- export 
	arg/3,		arg0/3,		atom/1,		atomic/1,
	atom_chars/2,
	cputime/1,	call/1,		call_expose/1,	callable/1,
	close/1,	compare/3,	compound/1,	display/1,
	expand_term/2,	file_exists/1,	fail/0,		fail_if/1,
	fileerrors/0,	float/1,	(fun)/1,	functor/3,
	get/1,		get0/1,		halt/0,		
	(hilog)/1,	hilog_arg/3,	hilog_functor/3,hilog_op/3,
	(index)/3,	(index)/2,      (table)/1,
	integer/1,	is/2,		is_list/1,	proper_hilog/1,
        is_string/1,
	listing/0,	listing/1,	name/2,		nl/0, nl/1,
	nofileerrors/0, nonvar/1,	(not)/1,	number/1,
	number_chars/2,
	once/1,		op/3,		open/3,          otherwise/0,
	print/1,	put/1,
	read/1,		read/2,         real/1,		repeat/0,
	see/1,		seeing/1,	seen/0,
	skip/1,		statistics/1,	statistics/0,	structure/1,
	tab/1,		tell/1,		telling/1,	told/0,
	ttywrite/1,	ttywritenl/1,
	true/0, 	var/1,		
	write/1,	write/2,	write_prolog/1,	write_prolog/2,
	writeln/1,	writeln/2,	writeq/1,	writeq/2,
	write_canonical/1,  write_canonical/2,  alt_write_canonical/1,
	('\+')/1,	'=:='/2,	'=\='/2,	'<'/2,
	'>'/2,		'=<'/2,		'>='/2,		'='/2,
	'\='/2,		'=..'/2,	'^=..'/2,	'=='/2,
	'\=='/2,	'@<'/2,		'@>'/2,		'@=<'/2,
	'@>='/2,	';'/2,		','/2,		'->'/2,
	'_$cutto'/1,	'_$savecp'/1.	


/* --------------------------------------------------------------- */
/*  exported predicates used in other modules (but not built-ins)  */
/* --------------------------------------------------------------- */

:- export
	call_c/1,	goal_cut_trans/3,	goal_cut_untrans/2,
	tcall/1,	arity_integer/1,	prolog_arglist/4,
	warning/1,	abort/1,		abort/0,
	(import)/1,	(multifile)/1.


/* The following predicates are directly imported from other modules */

:- import '_$load_undef'/1 from loader.
:- import file_put/2, psc_set_prop/3, psc_name/2,
	  term_psc/2, term_new/2, psc_arity/2, term_type/2, psc_prop/2,
	  file_open/3, psc_type/2, stat_set_flag/3, file_close/1,
	  file_get/2, stat_flag/2, psc_set_type/3, term_compare/3,
	  stat_sta/1, conname/2, conpsc/2, stat_cputime/1, 
	  expand_filename/2, machine_file_exists/1, psc_insert/3,
	  psc_import/3, call0/1, close_open_tables/0, is_backslashing_os/0
     from machine.
:- import str_tolist/2, str_fromlist/2 from string.
:- import file_write0/2, file_write/2, file_write_prolog/2, 
	  file_write_quoted/2, file_write_canonical/2, 
	  alt_file_write_canonical/2, file_nl/1
     from xsb_writ.
:- import char_blank/1 from xsb_tok.
:- import file_read/2 from xsb_read.
:- import eval/2 from eval.
:- import '_$op'/3, add_ops/3, op_priority/1, op_specifier/1, add_index/3,
           add_trie_index/1,
	  add_table/1
     from file_op.
:- import '_$hilog_symbol'/1, add_hilog_symbol/1 from hilogsym.
:- import dcg/2 from dcg.
:- import retract0/1, assertz/1, clause/2 from assert.
:- import numbervars/1 from num_vars.
:- import predicate_property/2 from curr_sym.

/* --------------------- end of file standard.H ----------------------- */
