/* File:      machine.H
** Author(s): Kostis F. Sagonas, Jiyang Xu David S. Warren, Terrance Swift
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


/*======================================================================*/
/* This file contains definitions for predicates implemented by the	*/
/* underline machine (hard builtins).					*/
/* Please keep this format for easier diffing.				*/
/*======================================================================*/

:- export
	psc_name/2, 
	psc_arity/2, 
	psc_type/2, 
	psc_prop/2, 
	psc_set_type/2, 
	psc_set_prop/2,
	term_psc/2, 
	term_type/2, 
	term_compare/3, 
	term_new/2,
	term_arg/3, 
	term_set_arg/4, 
	term_hash/3,
	stat_flag/2, 
	stat_set_flag/3, 
	stat_sta/1,
	buff_alloc/2, 
	buff_word/3, 
	buff_set_word/3,
	buff_dealloc/3, 
	buff_cell/3, 
	buff_set_cell/4, 
	buff_set_var/4,
	buff_byte/3, 
	buff_set_byte/3, 
	file_puttoken/3,
	code_call/3, 
	call0/1,
	code_load/2, 
	code_load/3, 
	load_seg/5, 
	file_gettoken/5,
	stat_cputime/1, 
	copy_term0/2,
	str_insert/2,
        str_sub/2,
        str_sub/3,
        dirname_canonic/2,
	psc_insert/4, 
	psc_import/3, 
	psc_insertmod/3,
	file_getbuf/4, 
	file_putbuf/4,
	str_len/2, 
	str_cat/3, 
	str_cmp/3, 
	str_hsh/4,
	unload_seg/1, 
	load_obj/4, 
	expand_filename/2,
        tilde_expand_filename/2,
        is_absolute_filename/1,
        parse_filename/4,
	getenv/2, 
	sys_syscall/4,
	sys_system/2, 
	sys_gethost/2, 
	sys_errno/1, 
	file_stat/3,
	file_writequoted/2,
	fast_ground/1,
        intern_string/2,
 	psc_env/2, 
	psc_spy/2, 
	psc_tabled/2, 
	tip_prop/4,
	is_incomplete/4,
	get_osp_breg/2,
	cut_if_leader/3,
	get_ptcp/1,
	get_subgoal_ptr/2,
	dereference_the_bucket/2,
	pair_psc/2, 
	pair_next/2, 
	next_bucket/2,
	slg_not/1,
	is_xwammode/1, 
	close_open_tables/0, 
	print_predicate_table/1, 
	abolish_table_info/0,
	zero_out_profile/0, 
	write_out_profile/0,
	assert_code_to_buff/2,
	assert_buff_to_clref/7,
        formatted_io/5,
	file_read_canonical/3,
	gen_retract_all/1,
	compiled_to_dynamic/2,
	db_get_clause/8,
	db_retract0/2,
	db_build_prref/3,
	db_remove_prref/1,
	get_emu_dependent_const/2,
	oracle_query/7,
	odbc_exec_query/5,
	private_builtin/11,
	trimcore/0,
	print_ls/0, print_tr/0, print_heap/2, print_cp/0, print_regs/0,
	print_all_stacks/0, expand_heap/0, mark_heap/1, gc_heap/0,
	'$$findall_init'/2,
	'$$findall_add'/3,
	'$$findall_get_solutions'/4,
	socket_request/6.

:- export 
	file_function/7,
	psc_insert/3, psc_set_env/2, conname/2, conpsc/2, 
	machine_file_exists/1, backtrackable_buff_set_word/4,
  	is_backslashing_os/0, is_pcdos/0, slash/1.

/* --------------------- end of file machine.H ------------------------ */

