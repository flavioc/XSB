/* File:      inst_xsb.c
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


#include "xsb_config.h"
#include "xsb_debug.h"

#include "auxlry.h"
#include "cell_xsb.h"
#include "inst_xsb.h"
#include "subinst.h"

extern void init_builtin_table(void);

Cell subinst_table[BUILTIN_TBL_SZ][2];

#ifdef PROFILE
Cell inst_table[BUILTIN_TBL_SZ][6];

#define set_inst(inst, instr, op1type, op2type, op3type, op4type) \
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

#define set_inst(inst, instr, op1type, op2type, op3type, op4type) \
        inst_table[inst][0] = (Cell)(instr); \
	inst_table[inst][1] = op1type; \
	inst_table[inst][2] = op2type; \
	inst_table[inst][3] = op3type; \
	inst_table[inst][4] = op4type
#endif

/*----------------------------------------------------------------------*/

int sizeof_inst(byte inst)
{ /* returns the size of an instruction in bytes */
  int i, size;

  size = 1;
  for (i=2; i <= 4 && inst_table[inst][i] != X; i++) {
    size++;
  }
  size *= sizeof(byte *); /* works for both 32 & 64 bit machines */
  return size;
}

/*----------------------------------------------------------------------*/

static void init_inst_table_1(void)
{
  set_inst(getpvar, "getpvar",           P,  V, R,X);
  set_inst(getpval, "getpval",           P,  V, R,X);
  set_inst(getstrv, "getstrv",           PP, V, S,X);
  set_inst(gettval, "gettval",           P,  R, R,X);
  set_inst(getcon, "getcon",             PP, R, C,X);
  set_inst(getnil, "getnil",             PP, R, X,X);
  set_inst(getstr, "getstr",             PP, R, S,X);
  set_inst(getlist, "getlist",           PP, R, X,X);
  set_inst(unipvar, "unipvar",           PP, V, X,X);
  set_inst(unipval, "unipval",           PP, V, X,X);
  set_inst(unitvar, "unitvar",           PP, R, X,X);
  set_inst(unitval, "unitval",           PP, R, X,X);
  set_inst(unicon, "unicon",             PPP,C, X,X);
  set_inst(uninil, "uninil",             PPP,X, X,X);
  set_inst(getnumcon, "getnumcon",       PP, R, N,X);
  set_inst(putnumcon, "putnumcon",       PP, R, N,X);
  set_inst(putpvar, "putpvar",           P,  V, R,X);
  set_inst(putpval, "putpval",           P,  V, R,X);
  set_inst(puttvar, "puttvar",           P,  R, R,X);
  set_inst(putstrv, "putstrv",           PP, V, S,X);
  set_inst(putcon, "putcon",             PP, R, C,X);
  set_inst(putnil, "putnil",             PP, R, X,X);
  set_inst(putstr, "putstr",             PP, R, S,X);
  set_inst(putlist, "putlist",           PP, R, X,X);
  set_inst(bldpvar, "bldpvar",           PP, V, X,X);
  set_inst(bldpval, "bldpval",           PP, V, X,X);
  set_inst(bldtvar, "bldtvar",           PP, R, X,X);
  set_inst(bldtval, "bldtval",           PP, R, X,X);
  set_inst(bldcon, "bldcon",             PPP,C, X,X);
  set_inst(bldnil, "bldnil",             PPP,X, X,X);
  set_inst(uninumcon, "uninumcon",       PPP,N, X,X);
  set_inst(bldnumcon, "bldnumcon",       PPP,N, X,X);
  set_inst(getattv, "getattv",           PP, R, X,X);
  set_inst(putattv, "putattv",           PP, R, X,X);
  set_inst(getlist_tvar_tvar, "getlist_tvar_tvar", R, R, R,X);
}

static void init_inst_table_2(void)
{
  /* the following are generated dynamically */
  set_inst(trie_root, "trie_root", X, X, X,X);
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
  set_inst(trie_trust_list, "trie_trust_list", X,X,X,X);
  set_inst(trie_proceed, "trie_proceed", X,X,X,X);
  set_inst(hash_opcode, "hash_opcode", X,X,X,X);
  set_inst(hash_handle, "hash_handle", X,X,X,X);
  set_inst(trie_assert_inst,"trie_assert_inst",X,X,X,X);
  set_inst(trie_proceed, "trie_proceed", X,X,X,X);
  set_inst(trie_no_cp_numcon_succ, "trie_no_cp_numcon_succ", X, X, X,X);
  set_inst(trie_try_numcon_succ, "trie_try_numcon_succ", X, X, X,X);
  set_inst(trie_retry_numcon_succ, "trie_retry_numcon_succ", X,X,X,X);
  set_inst(trie_trust_numcon_succ, "trie_trust_numcon_succ", X, X, X,X);

  set_inst(getfloat, "getfloat",           PP, R, F,X);
  set_inst(putfloat, "putfloat",           PP, R, F,X);
  set_inst(unifloat, "unifloat",           PPP,F, X,X);
  set_inst(bldfloat, "bldfloat",           PPP,F, X,X);
  set_inst(trymeelse, "trymeelse",         PP, A, L,X);
  set_inst(retrymeelse, "retrymeelse",     PP, A, L,X);
  set_inst(trustmeelsefail, "trustmeelsefail",    PP, A, X,X);
  set_inst(try, "try",                  PP, A, L,X);
  set_inst(retry, "retry",              PP, A, L,X);
  set_inst(trust, "trust",              PP, A, L,X);
  set_inst(getpbreg, "getpbreg",        PP, V, X,X);
}

static void init_inst_table_3(void)
{
  set_inst(gettbreg, "gettbreg",           PP, R, X,X);
  set_inst(putpbreg, "putpbreg",           PP, V, X,X);
  set_inst(puttbreg, "puttbreg",           PP, R, X,X);
  set_inst(jumptbreg, "jumptbreg",         PP, R, L,X);
  set_inst(getVn, "getVn",                 PP, V, X,X);
  set_inst(test_heap, "test_heap",         PP, A, N,X);
  set_inst(switchonterm, "switchonterm",     PPR, L, L,X);
  set_inst(switchonbound, "switchonbound",   PPR, I, I,X);
  set_inst(switchon3bound, "switchon3bound", RRR, I, I,X);
  set_inst(trymeorelse, "trymeorelse",        PP, A, L,X);
  set_inst(retrymeorelse, "retrymeorelse",    PP, A, L,X);
  set_inst(trustmeorelsefail, "trustmeorelsefail",   PP, A, X,X);
  set_inst(dyntrustmeelsefail, "dyntrustmeelsefail", PP, A, L,X); 
  set_inst(tableretry, "tableretry",          PP, A, L,X);
  set_inst(tabletry, "tabletry",              PP, A, L,T); 
  set_inst(tabletrust, "tabletrust",          PP, A, L,X); 
  set_inst(tabletrysingle, "tabletrysingle",  PP, A, L,T);

  /* the following are generated dynamically */
  set_inst(answer_return, "answer_return",   PPP, X, X,X); 
  set_inst(check_complete, "check_complete", PPP, X, X,X); 
  set_inst(resume_compl_suspension, "resume_compl_suspension", PPP, X, X,X); 

  set_inst(check_interrupt, "check_interrupt",       PP, A, S,X);
  set_inst(new_answer_dealloc, "new_answer_dealloc", P, A, A, X);
  set_inst(term_comp, "term_comp",     R,  R, R,X);
  set_inst(movreg, "movreg",           P,  R, R,X);
  set_inst(negate, "negate",           PP, R, X,X);
  set_inst(and, "and",                 P,  R, R,X);
  set_inst(or, "or",                   P,  R, R,X);
  set_inst(logshiftl, "logshiftl",     P,  R, R,X);
  set_inst(logshiftr, "logshiftr",     P,  R, R,X);
  set_inst(addreg, "addreg",           P,  R, R,X);
  set_inst(subreg, "subreg",           P,  R, R,X);
  set_inst(mulreg, "mulreg",           P,  R, R,X);
  set_inst(divreg, "divreg",           P,  R, R,X);
}

static void init_inst_table_4(void)
{
  set_inst(idivreg, "idivreg",         P,  R, R,X);
  set_inst(int_test_z, "int_test_z",   PP, R, N,L);
  set_inst(int_test_nz, "int_test_nz", PP, R, N,L);
  set_inst(putdval, "putdval",         P,  V, R,X);
  set_inst(putuval, "putuval",         P,  V, R,X);
  set_inst(call_forn, "call_forn",     PPP,L, X,X);
  set_inst(load_pred, "load_pred",     PPP,S, X,X);
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
  set_inst(unifunc, "unifunc",	       PPP,X, X,X);
  set_inst(reset, "reset",	       PPP,X, X,X);
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

/*----------------------------------------------------------------------*/
