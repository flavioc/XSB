/* File:      inst.c
** Author(s): Warren, Swift, Xu, Sagonas, Freire, Johnson
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


#include "configs/config.h"
#include "debugs/debug.h"
#include "auxlry.h"
#include "cell.h"
#include "inst.h"
#include "subinst.h"

Cell subinst_table[BUILTIN_TBL_SZ][2];
Cell builtin_table[BUILTIN_TBL_SZ][2];

#define set_builtin_table(inst, instr) \
        builtin_table[inst][0] = (Cell)(instr);

#ifdef PROFILE
Cell inst_table[BUILTIN_TBL_SZ][6];

#define set_inst(inst, instr, op1type, op2type, op3type,op4type) \
        inst_table[inst][0] = (Cell)(instr); \
	inst_table[inst][1] = op1type; \
	inst_table[inst][2] = op2type; \
	inst_table[inst][3] = op3type; \
	inst_table[inst][4] = op4type; \
	inst_table[inst][5] = 0

#define profile_inst(inst) \
       inst_table[inst][5] = inst_table[inst][5] + 1

#define set_subinst_table(inst,instr) \
        subinst_table[inst][0] = (Cell)(instr); \
	subinst_table[inst][1] = 0
#else
Cell inst_table[BUILTIN_TBL_SZ][5];

#define set_inst(inst, instr, op1type, op2type, op3type,op4type) \
        inst_table[inst][0] = (Cell)(instr); \
	inst_table[inst][1] = op1type; \
	inst_table[inst][2] = op2type; \
	inst_table[inst][3] = op3type; \
	inst_table[inst][4] = op4type
#endif

/*----------------------------------------------------------------------*/

void init_inst_table_1(void)
{
    set_inst(getpvar, "getpvar",            P,  V, R,X);
    set_inst(getpval, "getpval",            P,  V, R,X);
    set_inst(getstrv, "getstrv",            PP, V, S,X);
    set_inst(gettval, "gettval",            P,  R, R,X);
    set_inst(getcon, "getcon",             PP, R, C,X);
    set_inst(getnil, "getnil",             PP, R, X,X);
    set_inst(getstr, "getstr",             PP, R, S,X);
    set_inst(getlist, "getlist",            PP, R, X,X);
    set_inst(unipvar, "unipvar",            PP, V, X,X);
    set_inst(unipval, "unipval",            PP, V, X,X);
    set_inst(unitvar, "unitvar",            PP, R, X,X);
    set_inst(unitval, "unitval",            PP, R, X,X);
    set_inst(unicon, "unicon",             PPP,C, X,X);
    set_inst(uninil, "uninil",             PPP,X, X,X);
    set_inst(getnumcon, "getnumcon",          PP, R, N,X);
    set_inst(putnumcon, "putnumcon",          PP, R, N,X);
    set_inst(putpvar, "putpvar",            P,  V, R,X);
    set_inst(putpval, "putpval",            P,  V, R,X);
    set_inst(puttvar, "puttvar",            P,  R, R,X);
    set_inst(putstrv, "putstrv",            PP, V, S,X);
    set_inst(putcon, "putcon",             PP, R, C,X);
    set_inst(putnil, "putnil",             PP, R, X,X);
    set_inst(putstr, "putstr",             PP, R, S,X);
    set_inst(putlist, "putlist",            PP, R, X,X);
    set_inst(bldpvar, "bldpvar",            PP, V, X,X);
    set_inst(bldpval, "bldpval",            PP, V, X,X);
}

void init_inst_table_2(void)
{
    set_inst(bldtvar, "bldtvar",            PP, R, X,X);
    set_inst(bldtval, "bldtval",            PP, R, X,X);
    set_inst(bldcon, "bldcon",             PPP,C, X,X);
    set_inst(bldnil, "bldnil",             PPP,X, X,X);
    set_inst(uninumcon, "uninumcon",          PPP,N, X,X);
    set_inst(bldnumcon, "bldnumcon",          PPP,N, X,X);
    set_inst(getlist_tvar_tvar, "getlist_tvar_tvar",  R,  R, R,X);

    set_inst(trie_no_cp_str, "trie_no_cp_str", X, X, X,X);
    set_inst(trie_try_str, "trie_try_str", X, X, X,X);
    set_inst(trie_retry_str, "trie_retry_str", X,X,X,X);
    set_inst(trie_trust_str, "trie_trust_str", X, X, X,X);
    set_inst(trie_no_cp_numcon, "trie_no_cp_numcon", X, X, X,X);
    set_inst(trie_try_numcon, "trie_try_numcon", X, X, X,X);
    set_inst(trie_retry_numcon, "trie_retry_numcon", X,X,X,X);
    set_inst(trie_trust_numcon, "trie_trust_numcon", X, X, X,X);
    set_inst(trie_no_cp_var, "trie_no_cp_var", X, X, X,X);
    set_inst(trie_try_var, "trie_try_var", X, X, X,X);
    set_inst(trie_retry_var, "trie_retry_var", X,X,X,X);
    set_inst(trie_trust_var, "trie_trust_var", X, X, X,X);
    set_inst(trie_no_cp_val, "trie_no_cp_val", X, X, X,X);
    set_inst(trie_try_val, "trie_try_val", X, X, X,X);
    set_inst(trie_retry_val, "trie_retry_val", X,X,X,X);
    set_inst(trie_trust_val, "trie_trust_val", X, X, X,X);
    set_inst(trie_no_cp_list, "trie_no_cp_list", X, X, X,X);
    set_inst(trie_try_list, "trie_try_list", X, X, X,X);
    set_inst(trie_retry_list, "trie_retry_list", X,X,X,X);
    set_inst(trie_trust_list, "trie_trust_list", X, X, X,X);
    set_inst(trie_proceed, "trie_proceed", X,X,X,X);
    set_inst(hash_opcode, "hash_opcode", X,X,X,X);
    set_inst(hash_handle, "hash_handle", X,X,X,X);
    set_inst(trie_assert_inst,"trie_assert_inst",X,X,X,X);
    set_inst(trie_no_cp_numcon_succ, "trie_no_cp_numcon_succ", X, X, X,X);
    set_inst(trie_try_numcon_succ, "trie_try_numcon_succ", X, X, X,X);
    set_inst(trie_retry_numcon_succ, "trie_retry_numcon_succ", X,X,X,X);
    set_inst(trie_trust_numcon_succ, "trie_trust_numcon_succ", X, X, X,X);

    set_inst(getfloat, "getfloat",           PP, R, F,X);
    set_inst(putfloat, "putfloat",           PP, R, F,X);
    set_inst(unifloat, "unifloat",           PPP,F, X,X);
    set_inst(bldfloat, "bldfloat",           PPP,F, X,X);
    set_inst(trymeelse, "trymeelse",          PP, A, L,X);
    set_inst(retrymeelse, "retrymeelse",        PP, A, L,X);
    set_inst(trustmeelsefail, "trustmeelsefail",    PP, A, X,X);
    set_inst(try, "try",                  PP, A, L,X);
    set_inst(retry, "retry",              PP, A, L,X);
    set_inst(trust, "trust",              PP, A, L,X);
    set_inst(getpbreg, "getpbreg",        PP, V, X,X);
}

void init_inst_table_3(void)
{
    set_inst(gettbreg, "gettbreg",           PP, R, X,X);
    set_inst(putpbreg, "putpbreg",           PP, V, X,X);
    set_inst(puttbreg, "puttbreg",           PP, R, X,X);
    set_inst(jumptbreg, "jumptbreg",         PP, R, L,X);

    set_inst(getVn, "getVn",                 PP, V, X,X);
    set_inst(test_heap, "test_heap",         PP, A, N,X);

    set_inst(switchonterm, "switchonterm",       PPR,L, L,X);
    set_inst(switchonbound, "switchonbound",      PPR,I, I,X);
    set_inst(switchon3bound, "switchon3bound",      RRR,I, I,X);
    set_inst(trymeorelse, "trymeorelse",          PP, A, L,X);
    set_inst(retrymeorelse, "retrymeorelse",        PP, A, L,X);
    set_inst(trustmeorelsefail, "trustmeorelsefail",   PP, A, X,X);
    set_inst(dyntrustmeelsefail, "dyntrustmeelsefail", PP, A, L,X); 
    set_inst(tableretry, "tableretry",         PP, A, L,X);
    set_inst(tabletry, "tabletry",           PP, A, L,T); 
    set_inst(tabletrust, "tabletrust",         PP, A, L,X); 
    set_inst(tabletrysingle, "tabletrysingle",       PP, A, L,T); 
    set_inst(answer_return, "answer_return",         PPP, X, X,X); 

    set_inst(check_complete, "check_complete",     PPP, X, X,X); 
    set_inst(resume_compl_suspension, "resume_compl_suspension", PPP, X, X,X); 
    set_inst(new_answer_dealloc, "new_answer_dealloc", P, A, R, X);
    set_inst(term_comp, "term_comp",     R,  R, R,X);
    set_inst(movreg, "movreg",           P,  R, R,X);
    set_inst(negate, "negate",           PP, R, X,X);
    set_inst(and, "and",                 P,  R, R,X);
    set_inst(or, "or",                   P,  R, R,X);
    set_inst(lshiftl, "lshiftl",         P,  R, R,X);
    set_inst(lshiftr, "lshiftr",         P,  R, R,X);
    set_inst(addreg, "addreg",           P,  R, R,X);
    set_inst(subreg, "subreg",           P,  R, R,X);
    set_inst(mulreg, "mulreg",           P,  R, R,X);
    set_inst(divreg, "divreg",           P,  R, R,X);
}

void init_inst_table_4(void)
{
    set_inst(idivreg, "idivreg",         P,  R, R,X);
    set_inst(int_test_z, "int_test_z",	 PP, R, N,L);
    set_inst(int_test_nz, "int_test_nz", PP, R, N,L);
    set_inst(putdval, "putdval",         P,  V, R,X);
    set_inst(putuval, "putuval",         P,  V, R,X);
    set_inst(allocate_gc, "allocate_gc", P,  A, A,X);
    set_inst(call, "call",               PP, A, S,X);
    set_inst(allocate, "allocate",       PPP,X, X,X);
    set_inst(deallocate, "deallocate",   PPP,X, X,X);
    set_inst(proceed, "proceed",         PPP,X, X,X);
    set_inst(execute, "execute",         PPP,S, X,X);
    set_inst(calld, "calld",             PP, A, L,X); /* diff from compiler */
    set_inst(jump, "jump",               PPP,L, X,X);
    set_inst(jumpz, "jumpz",             PP, R, L,X);
    set_inst(jumpnz, "jumpnz",           PP, R, L,X);
    set_inst(jumplt, "jumplt",           PP, R, L,X);
    set_inst(jumple, "jumple",           PP, R, L,X);
    set_inst(jumpgt, "jumpgt",           PP, R, L,X);
    set_inst(jumpge, "jumpge",           PP, R, L,X);
    set_inst(cases, "cases",             A,  N, N,X); /* not used in emulator */
    set_inst(fail, "fail",               PPP,X, X,X);
    set_inst(noop, "noop",               PP, A, X,X);
    set_inst(halt, "halt",               PPP,X, X,X);
    set_inst(builtin, "builtin",         PP, A, X,X);
    set_inst(unifunc, "unifunc",	 PPP,X, X,X);
    set_inst(userfunc, "userfunc",       PP, A, S,X);
}

/*----------------------------------------------------------------------*/

#ifdef PROFILE /* for profiling only */
void init_subinst_table(void)
{
   set_subinst_table(OUTER_FIXPOINT,"outer_fixpoint");
   set_subinst_table(ITER_FIXPOINT,"inner_fixpoint");
   set_subinst_table(SCHED_ANSWERS,"sched_answers");
   set_subinst_table(SIMPL_REMOVE_DL,"simpl_remove_dl");
   set_subinst_table(SIMPL_REMOVE_DE,"simpl_remove_de");
   set_subinst_table(NEW_ANSWER_SIMPL_POS_UNS,"new_answer_simpl_pos_uns");
   set_subinst_table(NEW_ANSWER_SIMPL_POS_UNC,"new_answer_simpl_pos_unc");
   set_subinst_table(NEW_ANSWER_SIMPL_NEG_FAIL,"new_answer_simpl_neg_fail");
   set_subinst_table(NEW_ANSWER_SIMPL_NEG_SUC,"new_answer_simpl_neg_succ");
}
#endif

/*----------------------------------------------------------------------*/

void init_builtin_table(void)
{
  int i;

  for (i = 0; i < BUILTIN_TBL_SZ ; i++) builtin_table[i][1] = 0;
  set_builtin_table(PSC_NAME, "psc_name");
  set_builtin_table(PSC_ARITY, "psc_arity");
  set_builtin_table(PSC_TYPE, "psc_type");
  set_builtin_table(PSC_PROP, "psc_prop");
  set_builtin_table(PSC_SET_TYPE, "psc_set_type");
  set_builtin_table(PSC_SET_PROP, "psc_set_prop");
  set_builtin_table(FILE_OPEN, "file_open");
  set_builtin_table(FILE_CLOSE, "file_close");
  set_builtin_table(FILE_GET, "file_get");
  set_builtin_table(FILE_PUT, "file_put");
  set_builtin_table(TERM_PSC, "term_psc");
  set_builtin_table(TERM_TYPE, "term_type");
  set_builtin_table(TERM_COMPARE, "term_compare");
  set_builtin_table(TERM_NEW, "term_new");
  set_builtin_table(TERM_ARG, "term_arg");
  set_builtin_table(TERM_SET_ARG, "term_set_arg");
  set_builtin_table(STAT_FLAG, "stat_flag");
  set_builtin_table(STAT_SET_FLAG, "stat_set_flag");
  set_builtin_table(BUFF_ALLOC, "buff_alloc");
  set_builtin_table(BUFF_WORD, "buff_word");
  set_builtin_table(BUFF_SET_WORD, "buff_set_word");
  set_builtin_table(BUFF_BYTE, "buff_byte");
  set_builtin_table(BUFF_SET_BYTE, "buff_set_byte");
  set_builtin_table(CODE_CALL, "code_call");
  set_builtin_table(STR_LEN, "str_len");
  set_builtin_table(STR_CAT, "str_cat");
  set_builtin_table(STR_CMP, "str_cmp");
  set_builtin_table(STR_HSH, "str_hsh");
  set_builtin_table(STR_INSERT, "str_insert");
  set_builtin_table(STR_SUB, "str_sub");
  set_builtin_table(DIRNAME_CANONIC, "dirname_canonic");
  set_builtin_table(CALL0, "call0");
  set_builtin_table(STAT_STA, "stat_sta");
  set_builtin_table(STAT_CPUTIME, "stat_cputime");
  set_builtin_table(CODE_LOAD, "code_load");
  set_builtin_table(BUFF_SET_VAR, "buff_set_var");
  set_builtin_table(BUFF_DEALLOC, "buff_dealloc");
  set_builtin_table(BUFF_CELL, "buff_cell");
  set_builtin_table(BUFF_SET_CELL, "buff_set_cell");
  set_builtin_table(COPY_TERM,"copy_term");
  set_builtin_table(PSC_INSERT, "psc_insert");
  set_builtin_table(PSC_IMPORT, "psc_import");
  set_builtin_table(FILE_GETBUF, "file_getbuf");
  set_builtin_table(FILE_PUTBUF, "file_putbuf");
  set_builtin_table(PSC_INSERTMOD, "psc_insertmod");
  set_builtin_table(LOAD_SEG, "load_seg");
  set_builtin_table(FILE_GETTOKEN, "file_gettoken");
  set_builtin_table(FILE_PUTTOKEN, "file_puttoken");
  set_builtin_table(TERM_HASH, "term_hash");
  set_builtin_table(UNLOAD_SEG, "unload_seg");
  set_builtin_table(LOAD_OBJ, "load_obj");
  set_builtin_table(GETENV, "getenv");
  set_builtin_table(SYS_SYSCALL, "sys_syscall");
  set_builtin_table(SYS_SYSTEM, "sys_system");
  set_builtin_table(SYS_GETHOST, "sys_gethost");
  set_builtin_table(SYS_ERRNO, "sys_errno");
  set_builtin_table(FILE_STAT, "file_stat");
  set_builtin_table(FILE_WRITEQUOTED, "file_writequoted");
  set_builtin_table(FAST_GROUND, "fast_ground");
  set_builtin_table(FILE_POS, "file_pos");

  set_builtin_table(INTERN_STRING, "intern_string");
  set_builtin_table(EXPAND_FILENAME, "expand_filename");
  set_builtin_table(TILDE_EXPAND_FILENAME, "tilde_expand_filename");
  set_builtin_table(IS_ABSOLUTE_FILENAME, "is_absolute_filename");
  set_builtin_table(PARSE_FILENAME, "parse_filename");

  set_builtin_table(PSC_ENV, "psc_env");
  set_builtin_table(PSC_SPY, "psc_spy");
  set_builtin_table(PSC_TABLED, "psc_tabled");
  set_builtin_table(TIP_PROP, "tip_prop");
  set_builtin_table(IS_INCOMPLETE, "is_incomplete");
  set_builtin_table(GET_OSP_BREG, "get_osp_breg");
  set_builtin_table(CUT_IF_LEADER, "cut_if_leader");
  set_builtin_table(GET_PTCP, "get_ptcp");
  set_builtin_table(GET_SUBGOAL_PTR, "get_subgoal_ptr");
  set_builtin_table(DEREFERENCE_THE_BUCKET, "dereference_the_bucket");
  set_builtin_table(PAIR_PSC, "pair_psc");
  set_builtin_table(PAIR_NEXT, "pair_next");
  set_builtin_table(NEXT_BUCKET, "next_bucket");
  set_builtin_table(SLG_NOT, "slg_not");
  set_builtin_table(IS_XWAMMODE, "is_xwammode");
  set_builtin_table(CLOSE_OPEN_TABLES, "close_open_tables");
  set_builtin_table(PRINT_PREDICATE_TABLE, "print_predicate_table");

  set_builtin_table(ABOLISH_TABLE_INFO, "abolish_table_info");
  set_builtin_table(ZERO_OUT_PROFILE, "zero_out_profile");
  set_builtin_table(WRITE_OUT_PROFILE, "write_out_profile");
  set_builtin_table(ASSERT_CODE_TO_BUFF, "assert_code_to_buff");
  set_builtin_table(ASSERT_BUFF_TO_CLREF, "assert_buff_to_clref");
  set_builtin_table(FMT_WRITE, "fmt_write");
  set_builtin_table(SLASH_BUILTIN, "slash");
  set_builtin_table(FMT_WRITE_STRING, "fmt_write_string");
  set_builtin_table(FILE_READ_LINE, "file_read_line");
  set_builtin_table(FMT_READ, "fmt_read");
  set_builtin_table(FILE_READ_CANONICAL, "file_read_canonical");
  set_builtin_table(GEN_RETRACT_ALL, "gen_retract_all");
  set_builtin_table(COMPILED_TO_DYNAMIC, "compiled_to_dynamic");
  set_builtin_table(DB_RETRACT0, "db_retract0");
  set_builtin_table(DB_GET_CLAUSE, "db_get_clause");
  set_builtin_table(DB_BUILD_PRREF, "db_build_prref");
  set_builtin_table(DB_REMOVE_PRREF, "db_remove_prref");

  set_builtin_table(TRIE_NODE_ELEMENT, "trie_node_element");
  set_builtin_table(PROLOG_NEWNODE, "prolog_newnode");

  set_builtin_table(TABLE_STATUS, "table_status");
  set_builtin_table(GET_DELAY_LISTS, "get_delay_lists");
  set_builtin_table(DELETE_PREDICATE_TABLE, "delete_predicate_table");

  set_builtin_table(TRIE_ASSERT, "trie_assert");
  set_builtin_table(TRIE_RETRACT, "trie_retract");
  set_builtin_table(TRIE_RETRACT_SAFE, "trie_retract_safe");
  set_builtin_table(TRIE_DELETE_TERM, "trie_delete_term");
  set_builtin_table(TRIE_GET_RETURN, "trie_get_return");
  set_builtin_table(TRIE_GET_CALL, "trie_get_call");
  set_builtin_table(GET_LASTNODE_CS_RETSKEL, "get_lastnode_cs_retskel");
  set_builtin_table(CONSTRUCT_RET_FOR_CALL, "construct_ret_for_call");
  set_builtin_table(BREG_RETSKEL,"breg_retskel");

  set_builtin_table(GET_EMU_DEPENDENT_CONST, "get_emu_dependent_const");
  set_builtin_table(TRIMCORE, "trimcore");

  set_builtin_table(VAR, "var");
  set_builtin_table(NONVAR, "nonvar");
  set_builtin_table(ATOM, "atom");
  set_builtin_table(INTEGER, "integer");
  set_builtin_table(REAL, "real");
  set_builtin_table(NUMBER, "number");
  set_builtin_table(ATOMIC, "atomic");
  set_builtin_table(COMPOUND, "compound");
  set_builtin_table(CALLABLE, "callable");
  set_builtin_table(IS_LIST, "is_list");

  set_builtin_table(FUNCTOR, "functor");
  set_builtin_table(ARG, "arg");
  set_builtin_table(UNIV, "univ");
  set_builtin_table(MY_HiLog_FUNCTOR, "hilog_functor");
  set_builtin_table(HiLog_ARG, "hilog_arg");
  set_builtin_table(HiLog_UNIV, "hilog_univ");
  set_builtin_table(MY_COPY_TERM, "my_copy_term");
  set_builtin_table(MY_NAME, "my_name");
  set_builtin_table(ATOM_CHARS, "atom_chars");
  set_builtin_table(NUMBER_CHARS, "number_chars");

  set_builtin_table(PUT, "put");
  set_builtin_table(TAB, "tab");
  set_builtin_table(SORT, "sort");
  set_builtin_table(KEYSORT, "keysort");

  set_builtin_table(ORACLE_QUERY, "oracle_query");
  set_builtin_table(ODBC_QUERY, "odbc_query");

  set_builtin_table(PRINT_LS, "print_ls");
  set_builtin_table(PRINT_TR, "print_tr");
  set_builtin_table(PRINT_HEAP, "print_heap");
  set_builtin_table(PRINT_CP, "print_cp");
  set_builtin_table(PRINT_REGS, "print_regs");
  set_builtin_table(PRINT_ALL_STACKS, "print_all_stacks");
  set_builtin_table(MARK_HEAP, "mark_heap");
  set_builtin_table(GC_HEAP, "gc_heap");
  set_builtin_table(FINDALL_INIT, "$$findall_init");
  set_builtin_table(FINDALL_ADD, "$$findall_add");
  set_builtin_table(FINDALL_GET_SOLS, "$$findall_get_solutions");

#ifdef HAVE_SOCKET
  set_builtin_table(SOCKET_REQUEST, "socket_request");
#endif

  set_builtin_table(JAVA_INTERRUPT, "setupJavaInterrupt");
}

/*----------------------------------------------------------------------*/

void init_inst_table(void)
{
    init_inst_table_1();
    init_inst_table_2();
    init_inst_table_3();
    init_inst_table_4();
    init_builtin_table();
#ifdef PROFILE
    init_subinst_table();
#endif
}

