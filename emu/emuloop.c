/* File:      emuloop.c
** Author(s): Warren, Swift, Xu, Sagonas, Johnson
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

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>

#ifdef FOREIGN
#ifndef SOLARIS
#ifndef FOREIGN_WIN32
#include <sys/un.h>
#endif
#endif
#endif

#include "auxlry.h"
#include "cell_xsb.h"
#include "register.h"
#include "error_xsb.h"
#include "inst_xsb.h"
#include "psc_xsb.h"
#include "deref.h"
#include "memory_xsb.h"
#include "heap_xsb.h"
#include "sig_xsb.h"
#include "context.h"
#include "emudef.h"
#include "loader_xsb.h"
#include "binding.h"
#include "flags_xsb.h"
#include "trie_internals.h"
#include "choice.h"
#include "sw_envs.h"
#include "macro_xsb.h"
#include "tables.h"
#include "subinst.h"
#include "scc_xsb.h"
#include "subp.h"
#include "tr_utils.h"
#include "cut_xsb.h"
#include "export.h"
#include "orient_xsb.h"
#include "io_builtins_xsb.h"
#include "unify_xsb.h"
#include "emuloop_aux.h"
#include "remove_unf.h"
#include "thread_xsb.h"
#include "rw_lock.h"
#include "debug_xsb.h"

#include "hash_xsb.h"
/*
 * Variable ans_var_pos_reg is a pointer to substitution factor of an
 * answer in the heap.  It is used and set in function
 * variant_answer_search().  The name of this variable is from VarPosReg, a
 * variable used in variant_call_search() to save the substitution factor
 * of the call.
 */
#ifndef MULTI_THREAD
CPtr	ans_var_pos_reg;
#endif

/*----------------------------------------------------------------------*/

#include "tr_delay.h"
#include "tr_code_xsb_i.h"

/*----------------------------------------------------------------------*/
/* indirect threading-related stuff                                     */

#ifdef DEBUG_VM

#define XSB_Debug_Instr                                    \
   if (flags[PIL_TRACE]) {                                 \
      debug_inst(CTXTc lpcreg, ereg);                      \
   }                                                       \
   xctr++;

#else

#define XSB_Debug_Instr

#endif

#ifdef PROFILE

#define XSB_Profile_Instr                                     \
    if (flags[PROFFLAG]) {                                    \
      inst_table[(int) *(lpcreg)][sizeof(Cell)+1]             \
        = inst_table[(int) *(lpcreg)][sizeof(Cell)+1] + 1;    \
      if (flags[PROFFLAG] > 1 && (int) *lpcreg == builtin)    \
        builtin_table[(int) *(lpcreg+3)][1] =                 \
  	  builtin_table[(int) *(lpcreg+3)][1] + 1;            \
    } 

#else

#define XSB_Profile_Instr

#endif

#define handle_xsb_profile_interrupt 				\
    if (asynint_val && (asynint_val & PROFINT_MARK)) {		\
      asynint_val &= ~PROFINT_MARK;				\
      log_prog_ctr(lpcreg);					\
    }								\

/* lfcastro: with INSN_BLOCKS, we use a block for each WAM instruction, 
   and define temporary variables locally; otherwise, temp variables are 
   global to the emuloop function */

#ifdef INSN_BLOCKS

#define Def1op          register Cell op1;
#define Def2ops         register Cell op1, op2;
#define Def3ops         register Cell op1,op2; register CPtr op3;
#define DefOps13        register Cell op1; register CPtr op3;

#define DefGlobOps

#else

#define Def1op
#define Def2ops
#define Def3ops
#define DefOps13

#define DefGlobOps register Cell op1,op2; register CPtr op3;

#endif

/* lfcastro: with JUMPTABLE_EMULOOP, we use GCC's first-order labels to
   create a jumptable for the WAM instructions of emuloop(); otherwise 
   a switch statement is used. */

#ifdef JUMPTABLE_EMULOOP

static void *instr_addr[256];

#define XSB_End_Instr()                                      \
                   XSB_Debug_Instr                           \
                   XSB_Profile_Instr                         \
		   goto *instr_addr[(byte)*lpcreg];          \
		   }


#define XSB_Next_Instr()                                     \
                   do {                                      \
                      XSB_Debug_Instr                        \
                      XSB_Profile_Instr                      \
                      goto *instr_addr[(byte)*lpcreg];       \
                   } while(0)


#define XSB_Start_Instr_Chained(Instr,Label)                 \
        Label: 

#define XSB_Start_Instr(Instr,Label)                         \
        Label: {
		   


#else /* no threading */

#define XSB_Next_Instr()              goto contcase

#define XSB_End_Instr()               goto contcase; }

#define XSB_Start_Instr_Chained(Instr,Label)                 \
        case Instr:

#define XSB_Start_Instr(Instr,Label)                         \
        case Instr: { 

#endif

/*----------------------------------------------------------------------*/

#define get_axx         (lpcreg[1])
#define get_vxx         (ereg-(Cell)lpcreg[1])
#define get_rxx         (rreg+lpcreg[1])

#define get_xax         (lpcreg[2])
#define get_xvx         (ereg-(Cell)lpcreg[2])
#define get_xrx         (rreg+lpcreg[2])

#define get_xxa         (lpcreg[3])
#define get_xxv         (ereg-(Cell)lpcreg[3])
#define get_xxr         (rreg+lpcreg[3])

#define get_xxxl        (*(CPtr)(lpcreg+sizeof(Cell)))
#define get_xxxs        (*(CPtr)(lpcreg+sizeof(Cell)))
#define get_xxxc        (*(CPtr)(lpcreg+sizeof(Cell)))
#define get_xxxn        (*(CPtr)(lpcreg+sizeof(Cell)))
#define get_xxxg        (*(CPtr)(lpcreg+sizeof(Cell)))
#define get_xxxi        (*(CPtr)(lpcreg+sizeof(Cell)))
#define get_xxxf        (*(CPtr)(lpcreg+sizeof(Cell)))

#define get_xxxxi       (*(CPtr)(lpcreg+sizeof(Cell)*2))
#define get_xxxxl       (*(CPtr)(lpcreg+sizeof(Cell)*2))

#define Op1(Expr)       op1 = (Cell)Expr
#define Op2(Expr)       op2 = (Cell)Expr
#define Op3(Expr)       op3 = (CPtr)Expr

#define Register(Expr)  (cell(Expr))
#define Variable(Expr)  (cell(Expr))

#define size_none       0
#define size_xxx        1
#define size_xxxX       2
#define size_xxxXX      3

#define ADVANCE_PC(InstrSize)  (lpcreg += InstrSize*sizeof(Cell))

/* Be sure that flag only has the following two values.	*/

#define WRITE		1
#define READFLAG	0

#ifdef USE_BP_LPCREG
#define POST_LPCREG_DECL asm ("bp")
#else
#define POST_LPCREG_DECL
#endif

/*----------------------------------------------------------------------*/
/* The following macros work for all CPs.  Make sure this remains	*/
/* the case...								*/
/*----------------------------------------------------------------------*/

#define Fail1 lpcreg = cp_pcreg(breg);

#define restore_trail_condition_registers(breg) \
      if (*breg != (Cell) &check_complete_inst) { \
	ebreg = cp_ebreg(breg); \
	hbreg = cp_hreg(breg); \
      } 

/*----------------------------------------------------------------------*/

extern int  builtin_call(CTXTdeclc byte), unifunc_call(CTXTdeclc int, CPtr);
extern Cell builtin_table[BUILTIN_TBL_SZ][2];
extern Pair build_call(CTXTdeclc Psc);

extern void log_prog_ctr(byte *);
extern long prof_flag;

#ifdef DEBUG_VM
extern void debug_inst(CTXTdeclc byte *, CPtr);
#endif

/**static int  (*dyn_pred)(); unused-remove soon**/

#ifndef MULTI_THREAD
xsbBool neg_delay;
int  xwammode, level_num;
#endif

#ifdef DEBUG_VM
int  xctr;
#endif

/*----------------------------------------------------------------------*/

#include "schedrev_xsb_i.h"

#ifndef LOCAL_EVAL 
#include "wfs_xsb_i.h" 
#endif 
#include "complete_local.h"

/*----------------------------------------------------------------------*/

/* place for a meaningful message when segfault is detected */
char *xsb_default_segfault_msg =
     "\n++Memory violation occurred during evaluation.\n++Please report this problem using the XSB bug tracking system accessible from\n++\t http://sourceforge.net/projects/xsb\n++Please supply the steps necessary to reproduce the bug.\n";
char *xsb_segfault_message;
jmp_buf xsb_abort_fallback_environment;

/*======================================================================*/
/* the main emulator loop.						*/
/*======================================================================*/

/*
 * The WAM instructions are aligned with word (4 bytes on 32-bit machines,
 * or 8-byte on 64-bit machines), the shortest instructions (like fail)
 * take one word, and the longest ones take three words (like
 * switchon3bound).  If an instruction takes more than one word, then the
 * 2nd (or 3rd) word always contains an operand that takes one word.  The
 * one-word operands can be (see file emu/inst_xsb.h):
 *
 * 	L - label
 * 	S - structure symbol
 * 	C - constant symbol
 * 	N - number
 * 	G - string
 * 	I - 2nd & 3rd arguments of switchonbound
 * 	F - floating point number
 *
 * The opcode of all instructions takes the first byte in the first word.
 * The rest 3 bytes contain operands that needs only one byte.  These
 * one-byte operands can be:
 *
 * 	P - pad, not used
 * 	A - one byte number
 * 	V - variable offset
 * 	R - register number
 *
 * (In 64-bit machines there are 4 bytes of extra padding space for each 
 *  instruction)
 */

int emuloop(CTXTdeclc byte *startaddr)
{
  register CPtr rreg;
  register byte *lpcreg POST_LPCREG_DECL;
  DefGlobOps
  byte flag = READFLAG;  	/* read/write mode flag */
  int  restore_type;	/* 0 for retry restore; 1 for trust restore */ 

#if (defined(GC) && defined(GC_TEST))
/* Used only in the garbage collection test; does not affect emulator o/w */
#define GC_INFERENCES 66 /* make sure the garbage collection test is hard */
  static int infcounter = 0;
#endif

  xsb_segfault_message = xsb_default_segfault_msg;
  rreg = reg; /* for SUN */

#ifdef JUMPTABLE_EMULOOP

#define XSB_INST(INum,Instr,Label,d1,d2,d3,d4) \
        instr_addr[INum] = && Label
#include "xsb_inst_list.h"

#endif

  if ((lpcreg = (byte *) setjmp(xsb_abort_fallback_environment))) {
    /*
    * Short circuit untrailing to avoid possible seg faults in
    * switch_envs.
    */
    trreg = cp_trreg(breg);
    /* Restore the default signal handling */
    signal(SIGSEGV, xsb_default_segfault_handler);
   } else 
    lpcreg = startaddr;  /* first instruction of entire engine */

#ifdef JUMPTABLE_EMULOOP
  XSB_Next_Instr();
#else

contcase:     /* the main loop */

#ifdef DEBUG_VM
  if (flags[PIL_TRACE]) debug_inst(CTXTc lpcreg, ereg);
  xctr++;
#endif
#ifdef PROFILE
  if (flags[PROFFLAG]) {
    inst_table[(int) *(lpcreg)][sizeof(Cell)+1]
      = inst_table[(int) *(lpcreg)][sizeof(Cell)+1] + 1;
    if (flags[PROFFLAG] > 1 && (int) *lpcreg == builtin) 
      builtin_table[(int) *(lpcreg+3)][1] = 
	builtin_table[(int) *(lpcreg+3)][1] + 1;
  }
#endif

  switch (*lpcreg) {
#endif
    
  XSB_Start_Instr(getpvar,_getpvar)  /* PVR */
    Def2ops
    Op1(Variable(get_xvx));
    Op2(Register(get_xxr));
    ADVANCE_PC(size_xxx);
   /* trailing is needed here because this instruction can also be
       generated *after* the occurrence of the first call - kostis */
    bind_copy((CPtr)op1, op2);      /* In WAM bld_copy() */
  XSB_End_Instr()

  XSB_Start_Instr(getpval,_getpval) /* PVR */
    Def2ops
    Op1(Variable(get_xvx));
    Op2(Register(get_xxr));
    ADVANCE_PC(size_xxx);
    unify_xsb(_getpval);
  XSB_End_Instr()

  XSB_Start_Instr(getstrv,_getstrv) /* PPV-S */
    Def2ops
    Op1(Variable(get_xxv));
    Op2(get_xxxs);
    ADVANCE_PC(size_xxxX);
    nunify_with_str(op1,op2);
  XSB_End_Instr()

  XSB_Start_Instr(gettval,_gettval) /* PRR */
    Def2ops
    Op1(Register(get_xrx));
    Op2(Register(get_xxr));
    ADVANCE_PC(size_xxx);
    unify_xsb(_gettval);
  XSB_End_Instr()

  XSB_Start_Instr(getcon,_getcon) /* PPR-C */
    Def2ops
    Op1(Register(get_xxr));
    Op2(get_xxxc);
    ADVANCE_PC(size_xxxX);
    nunify_with_con(op1,op2);
  XSB_End_Instr()

  XSB_Start_Instr(getnil,_getnil) /* PPR */
    Def1op
    Op1(Register(get_xxr));
    ADVANCE_PC(size_xxx);
    nunify_with_nil(op1);
  XSB_End_Instr()	

  XSB_Start_Instr(getstr,_getstr) /* PPR-S */
    Def2ops
    Op1(Register(get_xxr));
    Op2(get_xxxs);
    ADVANCE_PC(size_xxxX);
    nunify_with_str(op1,op2);
  XSB_End_Instr()

  XSB_Start_Instr(getlist,_getlist) /* PPR */
    Def1op
    Op1(Register(get_xxr));
    ADVANCE_PC(size_xxx);
    nunify_with_list_sym(op1);
  XSB_End_Instr()

  XSB_Start_Instr(getattv,_getattv) /* PPR */
    Def1op
    Op1(Register(get_xxr));
    ADVANCE_PC(size_xxx);
    nunify_with_attv(op1);
  XSB_End_Instr()

/* TLS: Need trailing here: for a full explanation, see "A Note on
   Trailing in the SLGWAM on my web page. */
  XSB_Start_Instr(unipvar,_unipvar) /* PPV */
    Def1op
    Op1(get_xxv);
    ADVANCE_PC(size_xxx);
    if (!flag) {	/* if (flag == READ) */
      /* also introduce trailing here - bmd & kostis
         was: bld_copy((CPtr)op1, *(sreg++)); */
      bind_copy((CPtr)op1, *(sreg));
      sreg++;
    } else {
      bind_ref((CPtr)op1, hreg);
      new_heap_free(hreg);
    }
  XSB_End_Instr()

  XSB_Start_Instr(unipval,_unipval) /* PPV */
    Def2ops
    Op1(Variable(get_xxv));
    ADVANCE_PC(size_xxx);
    if (flag) { /* if (flag == WRITE) */
      nbldval(op1); 
    } 
    else {
      op2 = *(sreg++);
      unify_xsb(_unipval);
    } 
  XSB_End_Instr()

  XSB_Start_Instr(unitvar,_unitvar) /* PPR */
    Def1op
    Op1(get_xxr);
    ADVANCE_PC(size_xxx);
    if (!flag) {	/* if (flag == READ) */
      bld_copy((CPtr)op1, *(sreg++));
    }
    else {
      bld_ref((CPtr)op1, hreg);
      new_heap_free(hreg);
    }
  XSB_End_Instr()

  XSB_Start_Instr(uniavar,_uniavar) /* PPP */
    ADVANCE_PC(size_xxx);
    if (!flag) {	/* if (flag == READ) */
      sreg++;
    }
    else {
      new_heap_free(hreg);
    }
  XSB_End_Instr()

  XSB_Start_Instr(unitval,_unitval) /* PPR */
    Def2ops
    Op1(Register(get_xxr));
    ADVANCE_PC(size_xxx);
    if (flag) { /* if (flag == WRITE) */
      nbldval(op1); 
      XSB_Next_Instr();
    }
    else {
      op2 = *(sreg++);
      unify_xsb(_unitval);
    } 
  XSB_End_Instr()

  XSB_Start_Instr(unicon,_unicon) /* PPP-C */
    Def2ops
    Op2(get_xxxc);
    ADVANCE_PC(size_xxxX);
    if (flag) {	/* if (flag == WRITE) */
      new_heap_string(hreg, (char *)op2);
    }
    else {  
      /* op2 already set */
      op1 = *(sreg++);
      nunify_with_con(op1,op2);
    }
  XSB_End_Instr()

  XSB_Start_Instr(uninil,_uninil) /* PPP */
    Def1op
    ADVANCE_PC(size_xxx);
    if (flag) {	/* if (flag == WRITE) */
      new_heap_nil(hreg);
    }
    else {
      op1 = *(sreg++);
      nunify_with_nil(op1);
    }
  XSB_End_Instr()

  XSB_Start_Instr(getnumcon,_getnumcon) /* PPR-B */
    Def2ops
    Op1(Register(get_xxr));
    Op2(get_xxxn);
    ADVANCE_PC(size_xxxX);
    nunify_with_num(op1,op2);
  XSB_End_Instr()

  XSB_Start_Instr(getfloat,_getfloat) /* PPR-F */
    Def2ops
    Op1(Register(get_xxr));
    Op2(get_xxxn);
    ADVANCE_PC(size_xxxX);
    nunify_with_float(op1,op2);
  XSB_End_Instr()

  XSB_Start_Instr(putnumcon,_putnumcon) /* PPR-B */
    Def2ops
    Op1(get_xxr);
/*      Op2(get_xxxn); */
    op2 = *(pw)(lpcreg+sizeof(Cell));
    ADVANCE_PC(size_xxxX);
    bld_int_tagged((CPtr)op1, op2);
  XSB_End_Instr()

  XSB_Start_Instr(putfloat,_putfloat) /* PPR-F */
    Def2ops
    Op1(get_xxr);
    Op2(get_xxxn);
    ADVANCE_PC(size_xxxX);
    bld_float_tagged((CPtr)op1, op2);
  XSB_End_Instr()

  XSB_Start_Instr(putpvar,_putpvar) /* PVR */
    Def2ops
    Op1(get_xvx);
    Op2(get_xxr);
    ADVANCE_PC(size_xxx);
    bld_free((CPtr)op1);
    bld_ref((CPtr)op2, (CPtr)op1);
  XSB_End_Instr()

  XSB_Start_Instr(putpval,_putpval) /* PVR */
    DefOps13
    Op1(get_xvx);
    Op3(get_xxr);
    ADVANCE_PC(size_xxx);
    bld_copy(op3, *((CPtr)op1));
  XSB_End_Instr()

  XSB_Start_Instr(puttvar,_puttvar) /* PRR */
    Def2ops
    Op1(get_xrx);
    Op2(get_xxr);
    ADVANCE_PC(size_xxx);
    bld_ref((CPtr)op1, hreg);
    bld_ref((CPtr)op2, hreg);
    new_heap_free(hreg); 
  XSB_End_Instr()

/* TLS: Need trailing here: for a full explanation, see "A Note on
   Trailing in the SLGWAM on my web page. */
  XSB_Start_Instr(putstrv,_putstrv) /*  PPV-S */
    Def2ops
    Op1(get_xxv);
    Op2(get_xxxs);
    ADVANCE_PC(size_xxxX);
    bind_cs((CPtr)op1, (Pair)hreg);
    new_heap_functor(hreg, (Psc)op2); 
  XSB_End_Instr()

  XSB_Start_Instr(putcon,_putcon) /* PPR-C */
    Def2ops
    Op1(get_xxr);
    Op2(get_xxxc);
    ADVANCE_PC(size_xxxX);
    bld_string((CPtr)op1, (char *)op2);
  XSB_End_Instr()

  XSB_Start_Instr(putnil,_putnil) /* PPR */
    Def1op
    Op1(get_xxr);
    ADVANCE_PC(size_xxx);
    bld_nil((CPtr)op1);
  XSB_End_Instr()

/* doc tls -- differs from putstrv since it pulls from a register.
   Thus the variable is already initialized.  */
  XSB_Start_Instr(putstr,_putstr) /* PPR-S */
    Def2ops
    Op1(get_xxr);
    Op2(get_xxxs);
    ADVANCE_PC(size_xxxX);
    bld_cs((CPtr)op1, (Pair)hreg);
    new_heap_functor(hreg, (Psc)op2); 
  XSB_End_Instr()

  XSB_Start_Instr(putlist,_putlist) /* PPR */
    Def1op
    Op1(get_xxr);
    ADVANCE_PC(size_xxx);
    bld_list((CPtr)op1, hreg);
  XSB_End_Instr()

  XSB_Start_Instr(putattv,_putattv) /* PPR */
    Def1op
    Op1(get_xxr);
    ADVANCE_PC(size_xxx);
    bld_attv((CPtr)op1, hreg);
    new_heap_free(hreg);
  XSB_End_Instr()

/* TLS: Need trailing here: for a full explanation, see "A Note on
   Trailing in the SLGWAM on my web page. */
  XSB_Start_Instr(bldpvar,_bldpvar) /* PPV */
    Def1op
    Op1(get_xxv);
    ADVANCE_PC(size_xxx);
    bind_ref((CPtr)op1, hreg); /* trailing is needed: if o/w see ai_tests */
    new_heap_free(hreg);
  XSB_End_Instr()

  XSB_Start_Instr(bldpval,_bldpval) /* PPV */
    Def1op
    Op1(Variable(get_xxv));
    ADVANCE_PC(size_xxx);
    nbldval(op1);
  XSB_End_Instr()

  XSB_Start_Instr(bldtvar,_bldtvar) /* PPR */
    Def1op
    Op1(get_xxr);
    ADVANCE_PC(size_xxx);
    bld_ref((CPtr)op1, hreg);
    new_heap_free(hreg);
  XSB_End_Instr()

  XSB_Start_Instr(bldavar,_bldavar) /* PPR */
    ADVANCE_PC(size_xxx);
    new_heap_free(hreg);
  XSB_End_Instr()

  XSB_Start_Instr(bldtval,_bldtval) /* PPR */
    Def1op
    Op1(Register(get_xxr));
    ADVANCE_PC(size_xxx);
    nbldval(op1);
  XSB_End_Instr()

  XSB_Start_Instr(bldcon,_bldcon) /* PPP-C */
    Def1op
    Op1(get_xxxc);
    ADVANCE_PC(size_xxxX);
    new_heap_string(hreg, (char *)op1);
  XSB_End_Instr()

  XSB_Start_Instr(bldnil,_bldnil) /* PPP */
    ADVANCE_PC(size_xxx);
    new_heap_nil(hreg);
  XSB_End_Instr()

  XSB_Start_Instr(getlist_tvar_tvar,_getlist_tvar_tvar) /* RRR */
    Def3ops
    Op1(Register(get_rxx));
    Op2(get_xrx);
    Op3(get_xxr);
    ADVANCE_PC(size_xxx);
    XSB_Deref(op1);
    if (islist(op1)) {
      sreg = clref_val(op1);
      op1 = (Cell)op2;
      bld_ref((CPtr)op1, *(sreg));
      op1 = (Cell)op3;
      bld_ref((CPtr)op1, *(sreg+1));
    } else if (isref(op1)) {
      bind_list((CPtr)(op1), hreg);
      op1 = (Cell)op2;
      bld_ref((CPtr)op1, hreg);
      new_heap_free(hreg);
      op1 = (Cell)op3;
      bld_ref((CPtr)op1, hreg);
      new_heap_free(hreg);
     } else if (isattv(op1)) {
      attv_dbgmsg(">>>> getlist_tvar_tvar: ATTV interrupt needed\n");
      add_interrupt(CTXTc op1, makelist(hreg));
      op1 = (Cell)op2;
      bld_ref((CPtr)op1, hreg);
      new_heap_free(hreg);
      op1 = (Cell)op3;
      bld_ref((CPtr)op1, hreg);
      new_heap_free(hreg);
    }
    else Fail1;
  XSB_End_Instr()	/* end getlist_tvar_tvar */

  XSB_Start_Instr(uninumcon,_uninumcon) /* PPP-B */
    Def2ops
    Op2(get_xxxn); /* num in op2 */
    ADVANCE_PC(size_xxxX);
    if (flag) {	/* if (flag == WRITE) */
      new_heap_num(hreg, (Integer)op2);
    }
    else {  /* op2 set */
      op1 = *(sreg++);
      nunify_with_num(op1,op2);
    }
  XSB_End_Instr()

  XSB_Start_Instr(unifloat,_unifloat) /* PPPF */
    Def2ops
    Op2(get_xxxf); /* num in op2 */
    ADVANCE_PC(size_xxxX);
    if (flag) {	/* if (flag == WRITE) */
      new_heap_float(hreg, op2);
    }
    else {  /* op2 set */
      op1 = cell(sreg++);
      nunify_with_float(op1,op2);
    }
  XSB_End_Instr()

  XSB_Start_Instr(bldnumcon,_bldnumcon) /* PPP-B */
    Def1op
    Op1(get_xxxn);  /* num to op2 */
    ADVANCE_PC(size_xxxX);
    new_heap_num(hreg, (Integer)op1);
  XSB_End_Instr()

  XSB_Start_Instr(bldfloat,_bldfloat) /* PPP-F */
    Def1op
    Op1(get_xxxf); /* num to op2 */
    ADVANCE_PC(size_xxxX);
    new_heap_float(hreg, op1);
  XSB_End_Instr()

  XSB_Start_Instr(trymeelse,_trymeelse) /* PPA-L */
    Def2ops
    Op1(get_xxa);
    Op2(get_xxxl);
#if 0
    { 
      Psc mypsc = *(CPtr)(cpreg-4);
      if (mypsc)
	if (get_type(mypsc) == T_PRED) {
	  fprintf(stddbg,"creating_cp(trymeelse(%s/%d), %p).\n",
		  get_name(mypsc), get_arity(mypsc), breg);
	}
    }
#endif
    ADVANCE_PC(size_xxxX);
    SUBTRYME
  XSB_End_Instr()

  XSB_Start_Instr(retrymeelse,_retrymeelse) /* PPA-L */
    Def1op
    Op1(get_xxa);
    cp_pcreg(breg) = (byte *)get_xxxl;
    restore_type = 0;
    ADVANCE_PC(size_xxxX);
    RESTORE_SUB
  XSB_End_Instr()

  XSB_Start_Instr(trustmeelsefail,_trustmeelsefail) /* PPA */
    Def1op
    Op1(get_xxa);
    restore_type = 1;
    handle_xsb_profile_interrupt;
    ADVANCE_PC(size_xxx);
    RESTORE_SUB
  XSB_End_Instr()

  XSB_Start_Instr(try,_try) /* PPA-L */
    Def2ops
    Op1(get_xxa);
    op2 = (Cell)((Cell)lpcreg + sizeof(Cell)*2);
#if 0
    { 
      Psc mypsc = *(CPtr)(cpreg-4);
      if (mypsc)
	if (get_type(mypsc) == T_PRED) {
	  fprintf(stddbg,"creating_cp(try(%s/%d), %p).\n",
		  get_name(mypsc), get_arity(mypsc), breg);
	}
    }
#endif
    lpcreg = *(pb *)(lpcreg+sizeof(Cell)); /* = *(pointer to byte pointer) */
    SUBTRYME
  XSB_End_Instr()

  XSB_Start_Instr(retry,_retry) /* PPA-L */
    Def1op
    Op1(get_xxa);
    cp_pcreg(breg) = lpcreg+sizeof(Cell)*2;
    lpcreg = *(pb *)(lpcreg+sizeof(Cell));
    restore_type = 0;
    RESTORE_SUB
  XSB_End_Instr()

  XSB_Start_Instr(trust,_trust) /* PPA-L */
    Def1op
    Op1(get_xxa);
    handle_xsb_profile_interrupt;
    lpcreg = *(pb *)(lpcreg+sizeof(Cell));
    restore_type = 1;
    RESTORE_SUB
  XSB_End_Instr()

  XSB_Start_Instr(getVn,_getVn) /* PPV */
    Def1op
    Op1(get_xxv);
    ADVANCE_PC(size_xxx);
    cell((CPtr)op1) = (Cell)tcp_subgoal_ptr(breg);
  XSB_End_Instr()

  XSB_Start_Instr(getpbreg,_getpbreg) /* PPV */
    Def1op
    Op1(get_xxv);
    ADVANCE_PC(size_xxx);
    bld_int((CPtr)op1, ((pb)tcpstack.high - (pb)breg));
  XSB_End_Instr()

  XSB_Start_Instr(gettbreg,_gettbreg) /* PPR */
    Def1op
    Op1(get_xxr);
    ADVANCE_PC(size_xxx);
    bld_int((CPtr)op1, ((pb)tcpstack.high - (pb)breg));
  XSB_End_Instr()

  XSB_Start_Instr(putpbreg,_putpbreg) /* PPV */
    Def1op
    Op1(Variable(get_xxv));
    ADVANCE_PC(size_xxx);
    cut_code(op1);
  XSB_End_Instr()

  XSB_Start_Instr(puttbreg,_puttbreg) /* PPR */
    Def1op
    Op1(Register(get_xxr));
    ADVANCE_PC(size_xxx);
    cut_code(op1);
  XSB_End_Instr()

  XSB_Start_Instr(jumptbreg,_jumptbreg) /* PPR-L */	/* ??? */
    Def1op
    Op1(get_xxr);
    bld_int((CPtr)op1, ((pb)tcpstack.high - (pb)breg));
    lpcreg = *(byte **)(lpcreg+sizeof(Cell));
  XSB_End_Instr()

  XSB_Start_Instr(test_heap,_test_heap) /* PPA-N */
    Def2ops
    Op1(get_xxa); /* op1 = the arity of the procedure */
    Op2(get_xxxn);
    ADVANCE_PC(size_xxxX);
#ifdef GC_TEST
    if ((infcounter++ > GC_INFERENCES) || ((ereg - hreg) < (long)op2))
      {
	infcounter = 0;
        fprintf(stddbg, ".");
#else
    if ((ereg - hreg) < (long)op2)
      {
#endif
        if (gc_heap(CTXTc op1)) { /* garbage collection potentially modifies hreg */
	  if ((ereg - hreg) < (long)op2) {
	    if (flags[STACK_REALLOC]) {
	      if (glstack_realloc(CTXTc resize_stack(glstack.size,(op2*sizeof(Cell))),op1) != 0) {
		xsb_basic_abort(local_global_exception);
	      }
	    } else {
	      xsb_warn("Reallocation is turned OFF !");
              xsb_basic_abort(local_global_exception);
	    }
	  }
	}
	/* are there any localy cached quantities that must be reinstalled ? */
      }
  XSB_End_Instr()

  XSB_Start_Instr(switchonterm,_switchonterm) /* PPR-L-L */
    Def1op
    Op1(Register(get_xxr));
    XSB_Deref(op1);
    switch (cell_tag(op1)) {
    case XSB_INT:
    case XSB_STRING:
    case XSB_FLOAT:
      lpcreg = *(pb *)(lpcreg+sizeof(Cell));	    
      break;
    case XSB_FREE:
    case XSB_REF1:
    case XSB_ATTV:
      ADVANCE_PC(size_xxxXX);
      break;
    case XSB_STRUCT:
      if (get_arity(get_str_psc(op1)) == 0) {
	lpcreg = *(pb *)(lpcreg+sizeof(Cell));
	break;
      }
    case XSB_LIST:	/* include structure case here */
      lpcreg = *(pb *)(lpcreg+sizeof(Cell)*2); 
      break;
    }
  XSB_End_Instr()

  XSB_Start_Instr(switchonbound,_switchonbound) /* PPR-L-L */
    Def3ops
    /* op1 is register, op2 is hash table offset, op3 is modulus */
    Op1(get_xxr);
    XSB_Deref(op1);
    switch (cell_tag(op1)) {
    case XSB_STRUCT:
      op1 = (Cell)get_str_psc(op1);
      break;
    case XSB_STRING:	/* We should change the compiler to avoid this test */
      op1 = (Cell)(isnil(op1) ? 0 : string_val(op1));
      break;
    case XSB_INT: 
    case XSB_FLOAT:	/* Yes, use int_val to avoid conversion problem */
      op1 = (Cell)int_val(op1);
      break;
    case XSB_LIST:
      op1 = (Cell)(list_str); 
      break;
    case XSB_FREE:
    case XSB_REF1:
    case XSB_ATTV:
      lpcreg += 3 * sizeof(Cell);
      XSB_Next_Instr();
    }
    op2 = (Cell)(*(byte **)(lpcreg+sizeof(Cell)));
    op3 = *(CPtr *)(lpcreg+sizeof(Cell)*2);
    /* doc tls -- op2 + (op1%size)*4 */
    lpcreg =
      *(byte **)((byte *)op2 + ihash((Cell)op1, (Cell)op3) * sizeof(Cell));
  XSB_End_Instr()

  XSB_Start_Instr(switchon3bound,_switchon3bound) /* RRR-L-L */
    Def3ops
    int  i, j = 0;
    int indexreg[3];
    Cell opa[3]; 
    /* op1 is register contents, op2 is hash table offset, op3 is modulus */
    indexreg[0] = get_axx;
    indexreg[1] = get_xax;
    indexreg[2] = get_xxa;

    if (*lpcreg == 0) { opa[0] = 0; }
    else opa[0] = Register((rreg + (indexreg[0] & 0x7f)));
    opa[1] = Register((rreg + (indexreg[1] & 0x7f)));
    opa[2] = Register((rreg + (indexreg[2] & 0x7f)));
    op2 = (Cell)(*(byte **)(lpcreg+sizeof(Cell)));
    op3 = *(CPtr *)(lpcreg+sizeof(Cell)*2); 
    /* This is not a good way to do this, but until we put retract into C,
       or add new builtins, it will have to do. */
    for (i = 0; i <= 2; i++) {
      if (opa[i] != 0) {
        if (indexreg[i] > 0x80) {
          int k, depth = 0;
          Cell *stk[MAXTOINDEX];
          int argsleft[MAXTOINDEX];
          stk[0] = &opa[i];
          argsleft[0] = 1;

          for (k = MAXTOINDEX; k > 0; k--) {
            if (depth < 0) break;
            op1 = *stk[depth];
            argsleft[depth]--;
            if (argsleft[depth] <= 0) depth--;
            else stk[depth]++;
	    XSB_Deref(op1);
	    switch (cell_tag(op1)) {
	    case XSB_FREE:
	    case XSB_REF1:
	    case XSB_ATTV:
	      ADVANCE_PC(size_xxxXX);
	      XSB_Next_Instr();
	    case XSB_INT: 
	    case XSB_FLOAT:	/* Yes, use int_val to avoid conversion problem */
	      op1 = (Cell)int_val(op1);
	      break;
	    case XSB_LIST:
              depth++;
              argsleft[depth] = 2;
              stk[depth] = clref_val(op1);
	      op1 = (Cell)(list_str); 
	      break;
	    case XSB_STRUCT:
	      depth++;
              argsleft[depth] = get_arity(get_str_psc(op1));
              stk[depth] = clref_val(op1)+1;
	      op1 = (Cell)get_str_psc(op1);
	      break;
	    case XSB_STRING:
	      op1 = (Cell)string_val(op1);
	      break;
            }
	    j = (j<<1) + ihash((Cell)op1, (Cell)op3);
          }
      } else {
	op1 = opa[i];
	XSB_Deref(op1);
	switch (cell_tag(op1)) {
	case XSB_FREE:
	case XSB_REF1:
	case XSB_ATTV:
	  ADVANCE_PC(size_xxxXX);
	  XSB_Next_Instr();
	case XSB_INT: 
	case XSB_FLOAT:	/* Yes, use int_val to avoid conversion problem */
	  op1 = (Cell)int_val(op1);
	  break;
	case XSB_LIST:
	  op1 = (Cell)(list_str); 
	  break;
	case XSB_STRUCT:
	  op1 = (Cell)get_str_psc(op1);
	  break;
	case XSB_STRING:
	  op1 = (Cell)string_val(op1);
	  break;
	default:
	  xsb_error("Illegal operand in switchon3bound");
	  break;
        }
	j = (j<<1) + ihash((Cell)op1, (Cell)op3);
      }
      }
    }
    lpcreg = *(byte **)((byte *)op2 + ((j % (Cell)op3) * sizeof(Cell)));
  XSB_End_Instr()

  XSB_Start_Instr(trymeorelse,_trymeorelse) /* PPA-L */
    Def2ops
    Op1(0);
    Op2(get_xxxl);
#if 0
    { 
      Psc mypsc = *(CPtr)(cpreg-4);
      if (mypsc)
	if (get_type(mypsc) == T_PRED) {
	  fprintf(stddbg,"creating_cp(trymeorelse(%s/%d), %p).\n",
		  get_name(mypsc), get_arity(mypsc), breg);
	}
    }
#endif
    ADVANCE_PC(size_xxxX);
    cpreg = lpcreg; /* Another use of cpreg for inline try's for disjunctions */
    SUBTRYME
  XSB_End_Instr()

  XSB_Start_Instr(retrymeorelse,_retrymeorelse) /* PPA-L */
    Def1op
    Op1(0);
    cp_pcreg(breg) = *(byte **)(lpcreg+sizeof(Cell));
    ADVANCE_PC(size_xxxX);
    restore_type = 0;
    RESTORE_SUB
  XSB_End_Instr()

  XSB_Start_Instr(trustmeorelsefail,_trustmeorelsefail) /* PPA */
    Def1op
    Op1(0);
    handle_xsb_profile_interrupt;
    ADVANCE_PC(size_xxx);
    restore_type = 1;
    RESTORE_SUB
  XSB_End_Instr()

  XSB_Start_Instr(dyntrustmeelsefail,_dyntrustmeelsefail) /* PPA-L, second word ignored */
    Def1op
    Op1(get_xxa);
    handle_xsb_profile_interrupt;
    ADVANCE_PC(size_xxxX);
    restore_type = 1;
    RESTORE_SUB
  XSB_End_Instr()

/*----------------------------------------------------------------------*/

#include "slginsts_xsb_i.h"

#include "tc_insts_xsb_i.h"

/*----------------------------------------------------------------------*/

  XSB_Start_Instr(term_comp,_term_comp) /* RRR */
    Def3ops
    Op1(get_rxx);
    Op2(get_xrx);
    Op3(get_xxr);
    ADVANCE_PC(size_xxx);
    bld_int(op3, compare(CTXTc (void *)op1, (void *)op2));
  XSB_End_Instr()

  XSB_Start_Instr(movreg,_movreg) /* PRR */
    Def2ops
    Op1(get_xrx);
    Op2(get_xxr);
    ADVANCE_PC(size_xxx);
    bld_copy((CPtr) op2, *((CPtr)op1));
  XSB_End_Instr()

#define ARITHPROC(OP, STROP)                                            \
    Op1(Register(get_xrx));                                             \
    Op3(get_xxr);                                                       \
    ADVANCE_PC(size_xxx);                                               \
    op2 = *(op3);							\
    XSB_Deref(op1);	       						\
    XSB_Deref(op2);		       					\
    if (isinteger(op1)) {						\
	if (isinteger(op2)) {						\
            Integer temp = int_val(op2) OP int_val(op1);                \
	    bld_oint(op3, temp);                         }              \
	else if (isfloat(op2)) {					\
            Float temp = float_val(op2) OP (Float)int_val(op1);         \
	    bld_float(op3, temp); }	                                \
        else if (isboxedinteger(op2)) {                                 \
            Integer temp = boxedint_val(op2) OP int_val(op1);           \
            bld_oint(op3, temp); }                                      \
	else { arithmetic_abort(CTXTc op2, STROP, op1); }               \
    } else if (isfloat(op1)) {						\
	if (isfloat(op2)) {						\
            Float temp = float_val(op2) OP float_val(op1);              \
	    bld_float(op3, temp); }		                        \
	else if (isinteger(op2)) {					\
            Float temp = (Float)int_val(op2) OP float_val(op1);         \
	    bld_float(op3, temp); }	                                \
        else if (isboxedinteger(op2)) {                                 \
            Float temp = (Float)boxedint_val(op2) OP float_val(op1);    \
	    bld_float(op3, temp); }                                     \
	else { arithmetic_abort(CTXTc op2, STROP, op1); } 	        \
    } else if (isboxedinteger(op1)) {                                   \
	if (isinteger(op2)) {						\
            Integer temp = int_val(op2) OP boxedint_val(op1);           \
	    bld_oint(op3, temp); }                                      \
        else if (isboxedinteger(op2)) {                                 \
            Integer temp = boxedint_val(op2) OP boxedint_val(op1);      \
            bld_oint(op3, temp); }                                      \
	else if (isfloat(op2)) {					\
            Float temp = float_val(op2) OP (Float)boxedint_val(op1);    \
	    bld_float(op3, temp); }                                     \
	else { arithmetic_abort(CTXTc op2, STROP, op1); }               \
    } else { arithmetic_abort(CTXTc op2, STROP, op1); }


  XSB_Start_Instr(addreg,_addreg) /* PRR */
    Def3ops
    ARITHPROC(+, "+");
  XSB_End_Instr() 

  XSB_Start_Instr(subreg,_subreg) /* PRR */
    Def3ops
    ARITHPROC(-, "-");
  XSB_End_Instr() 

  XSB_Start_Instr(mulreg,_mulreg) /* PRR */
    Def3ops
    ARITHPROC(*, "*");
  XSB_End_Instr() 

   /* TLS: cant use ARITHPROC because int/int -> float */
  XSB_Start_Instr(divreg,_divreg) /* PRR */
    Def3ops
    Op1(Register(get_xrx));
    Op3(get_xxr);
    ADVANCE_PC(size_xxx);
    op2 = *(op3);
    XSB_Deref(op1);
    XSB_Deref(op2);
    if (isinteger(op1)) {
      if (isinteger(op2)) {
        Float temp = (Float)int_val(op2)/(Float)int_val(op1);
	bld_float(op3, temp); }
      else if (isfloat(op2)) {
        Float temp = float_val(op2)/(Float)int_val(op1);
	bld_float(op3, temp); }
      else if (isboxedinteger(op2)) {
        Float temp = (Float)boxedint_val(op2)/(Float)int_val(op1);
	bld_float(op3, temp); }
      else { arithmetic_abort(CTXTc op2, "/", op1); }
    } else if (isfloat(op1)) {
      if (isfloat(op2)) {
        Float temp = float_val(op2)/float_val(op1);
	bld_float(op3, temp); }
      else if (isinteger(op2)) {
        Float temp = (Float)int_val(op2)/float_val(op1);
	bld_float(op3, temp); }
      else if (isboxedinteger(op2)) {
        Float temp = (Float)boxedint_val(op2)/float_val(op1);
	bld_float(op3, temp); }
      else { arithmetic_abort(CTXTc op2, "/", op1); }
    } else if (isboxedinteger(op1)) {
      if (isinteger(op2)) {
        Float temp = (Float)int_val(op2) / (Float)boxedint_val(op1);
        bld_float(op3, temp); }
      else if (isboxedinteger(op2)) {
        Integer temp = (Integer) ((Float)boxedint_val(op2) / (Float)boxedint_val(op1));
        bld_float(op3, temp); }
      else if (isfloat(op2)) {
        Float temp = (Float)float_val(op2) / (Float)boxedint_val(op1);
	bld_float(op3, temp); }
      else { arithmetic_abort(CTXTc op2, "/", op1); }
    } else { arithmetic_abort(CTXTc op2, "/", op1); }
  XSB_End_Instr() 

  XSB_Start_Instr(idivreg,_idivreg) /* PRR */
    Def3ops
    Op1(Register(get_xrx));
    Op3(get_xxr);
    ADVANCE_PC(size_xxx);
    op2 = *(op3);
    XSB_Deref(op1);
    XSB_Deref(op2);
      if (isinteger(op1)) {
        if (int_val(op1) != 0) {
          if (isinteger(op2)) {
            Integer temp = int_val(op2) / int_val(op1);
            bld_oint(op3, temp); 
          } else if (isboxedinteger(op2)) {
            Integer temp = boxedint_val(op2) / int_val(op1);
            bld_oint(op3, temp); 
          } else { arithmetic_abort(CTXTc op2, "//", op1); }
        } else {
	  err_handle(CTXTc ZERO_DIVIDE, 2,
		     "arithmetic expression involving is/2 or eval/2",
		     2, "non-zero number", op1);
	  lpcreg = pcreg;
        }
      } else if (isboxedinteger(op1)) {
        if (isinteger(op2)) {
          Integer temp = int_val(op2) / boxedint_val(op1);
          bld_oint(op3, temp);
        } else if (isboxedinteger(op2)) {
          Integer temp = boxedint_val(op2) / boxedint_val(op1);
          bld_oint(op3, temp);
        }
      }
    else { arithmetic_abort(CTXTc op2, "//", op1); }
  XSB_End_Instr() 

  XSB_Start_Instr(int_test_z,_int_test_z)   /* PPR-B-L */
    Def3ops
    Op1(Register(get_xxr));
    Op2(get_xxxn);
    Op3(get_xxxxl);
    ADVANCE_PC(size_xxxXX);
    XSB_Deref(op1); 
    if (isnumber(op1)) {
      if (op1 == op2)
	lpcreg = (byte *)op3;
    }
    else if (isboxedinteger(op1)) {
       if (oint_val(op1) == oint_val(op2))
          lpcreg = (byte *)op3;
    }	  
    else {
      arithmetic_comp_abort(CTXTc op1, "=\\=", op2);
    }
  XSB_End_Instr()

  XSB_Start_Instr(int_test_nz,_int_test_nz)   /* PPR-B-L */
    Def3ops
    Op1(Register(get_xxr));
    Op2(get_xxxn);
    Op3(get_xxxxl);
    ADVANCE_PC(size_xxxXX);
    XSB_Deref(op1); 
    if (isnumber(op1)) {
      if (op1 != op2)
	lpcreg = (byte *) op3;
    }
    else if (isboxedinteger(op1)) {
       if (oint_val(op1) != oint_val(op2))
          lpcreg = (byte *)op3;
    }	  
    else {
      arithmetic_comp_abort(CTXTc op1, "=:=", op2);
    }
  XSB_End_Instr()

  XSB_Start_Instr(fun_test_ne,_fun_test_ne)   /* PRR-L */
    Def3ops
    Op1(Register(get_xrx));
    Op2(Register(get_xxr));
    Op3(get_xxxl);
    ADVANCE_PC(size_xxxX);
    XSB_Deref(op1);
    XSB_Deref(op2);
    if (isconstr(op1)) {
      if (!isconstr(op2) || get_str_psc(op1) != get_str_psc(op2)) 
        lpcreg = (byte *) op3;
    } else if (islist(op1)) {
      if (!islist(op2)) lpcreg = (byte *) op3;
    } else if (op1 != op2) lpcreg = (byte *) op3;
  XSB_End_Instr()

     /* TLS: so much work for such a little function! */
  XSB_Start_Instr(minreg,_minreg) /* PRR */
    Def3ops
    Op1(Register(get_xrx));
    Op3(get_xxr);
    ADVANCE_PC(size_xxx);
    op2 = *(op3);
    XSB_Deref(op1);
    XSB_Deref(op2);
    if (isinteger(op1)) {
         if (isinteger(op2)) {
              if (int_val(op2) < int_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
         if (isboxedinteger(op2)) {
              if (boxedint_val(op2) < int_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
         if (isfloat(op2)) {
              if (float_val(op2) < int_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
    } 
    else if (isboxedinteger(op1)) {
         if (isinteger(op2)) {
              if (int_val(op2) < boxedint_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
         if (isboxedinteger(op2)) {
              if (boxedint_val(op2) < boxedint_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
         if (isfloat(op2)) {
              if (float_val(op2) < boxedint_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
    } 
    else if (isfloat(op1)) {
         if (isinteger(op2)) {
              if (int_val(op2) < float_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
         if (isboxedinteger(op2)) {
              if (boxedint_val(op2) < float_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
         if (isfloat(op2)) {
              if (float_val(op2) < float_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
    } 
   else { arithmetic_abort(CTXTc op2, "min", op1); }
  XSB_End_Instr() 

     /* TLS: so much work for such a little function! */
  XSB_Start_Instr(maxreg,_maxreg) /* PRR */
    Def3ops
    Op1(Register(get_xrx));
    Op3(get_xxr);
    ADVANCE_PC(size_xxx);
    op2 = *(op3);
    XSB_Deref(op1);
    XSB_Deref(op2);
    if (isinteger(op1)) {
         if (isinteger(op2)) {
              if (int_val(op2) > int_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
         if (isboxedinteger(op2)) {
              if (boxedint_val(op2) > int_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
         if (isfloat(op2)) {
              if (float_val(op2) > int_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
    } 
    else if (isboxedinteger(op1)) {
         if (isinteger(op2)) {
              if (int_val(op2) > boxedint_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
         if (isboxedinteger(op2)) {
              if (boxedint_val(op2) > boxedint_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
         if (isfloat(op2)) {
              if (float_val(op2) > boxedint_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
    } 
    else if (isfloat(op1)) {
         if (isinteger(op2)) {
              if (int_val(op2) > float_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
         if (isboxedinteger(op2)) {
              if (boxedint_val(op2) > float_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
         if (isfloat(op2)) {
              if (float_val(op2) > float_val(op1))  bld_copy(op3,op2); else bld_copy(op3,op1);
          }
    } 
   else { arithmetic_abort(CTXTc op2, "min", op1); }
  XSB_End_Instr() 


  XSB_Start_Instr(putdval,_putdval) /* PVR */
    Def2ops
    Op1(Variable(get_xvx));
    Op2(get_xxr);
    ADVANCE_PC(size_xxx);
    XSB_Deref(op1);
    bld_copy((CPtr)op2, op1);
  XSB_End_Instr()

  XSB_Start_Instr(putuval,_putuval) /* PVR */
    Def2ops
    Op1(Variable(get_xvx));
    Op2(get_xxr);
    ADVANCE_PC(size_xxx);
    XSB_Deref(op1);
    if (isnonvar(op1) || ((CPtr)(op1) < hreg) || ((CPtr)(op1) >= ereg)) {
      bld_copy((CPtr)op2, op1);
    } else {
      bld_ref((CPtr)op2, hreg);
      bind_ref((CPtr)(op1), hreg);
      new_heap_free(hreg);
    } 
  XSB_End_Instr()

  /*
   * Instruction `check_interrupt' is used before `new_answer_dealloc' to
   * handle the pending attv interrupts.  It is similar to `call' but the
   * second argument (S) is not used currently.
   */
  XSB_Start_Instr(check_interrupt,_check_interrupt)  /* PPA-S */
    Def1op
    
    Op1(get_xxxs);
    ADVANCE_PC(size_xxxX);
    if (int_val(cell(interrupt_reg)) > 0) {
      cpreg = lpcreg;
      bld_cs(reg + 2, hreg);	/* see subp.c: build_call() */
      new_heap_functor(hreg, true_psc);
      bld_copy(reg + 1, build_interrupt_chain(CTXT));
      lpcreg = get_ep((Psc) flags[MYSIG_ATTV + INT_HANDLERS_FLAGS_START]);
    }
  XSB_End_Instr()

  XSB_Start_Instr(call,_call)  /* PPA-S */
    Def1op
    Psc psc;

    Op1(get_xxxs); /* the first arg is used later by alloc */
    ADVANCE_PC(size_xxxX);
    cpreg = lpcreg;
    psc = (Psc)op1;
#ifdef CP_DEBUG
    pscreg = psc;
#endif
    call_sub(psc);
  XSB_End_Instr()

  XSB_Start_Instr(call_forn,_call_forn)  /* PPP-L, maybe use userfun instr? */
    Def1op
    Op1(get_xxxl);
    ADVANCE_PC(size_xxxX);
    if (((PFI)op1)())  /* call foreign function */
      lpcreg = cpreg;
    else Fail1;
  XSB_End_Instr()

  XSB_Start_Instr(load_pred,_load_pred) /* PPP-S */
    Def1op
    Psc psc;
    
    Op1(get_xxxs);
    SYS_MUTEX_LOCK(MUTEX_LOAD_UNDEF);
    ADVANCE_PC(size_xxxX);
    psc = (Psc)op1;
    /* check env or type to give (better) error msgs? */
    switch (get_type(psc)) {
    case T_PRED:
    case T_DYNA:
    case T_FORN:
#ifndef MULTI_THREAD
      xsb_abort("[EMULOOP] Trying to load an already loaded pred");
#else
      /* predicate was loaded by another thread */
      /* fprintf(stderr,"Predicate loaded by other thread\n");
         fflush(stderr);
       */	
      SYS_MUTEX_UNLOCK(MUTEX_LOAD_UNDEF);
      lpcreg = get_ep(psc);             /* new ep of predicate */
      break;
#endif
    default:
      /* xsb_dbgmsg("loading module %s for %s/%d\n",
	 get_name(get_data(psc)),get_name(psc),get_arity(psc)); */
      bld_cs(reg+1, build_call(CTXTc psc));   /* put call-term in r1 */
      /* get psc of undef handler */
      psc = (Psc)flags[MYSIG_UNDEF+INT_HANDLERS_FLAGS_START];
      bld_int(reg+2, MYSIG_UNDEF);      /* undef-pred code */
      lpcreg = get_ep(psc);             /* ep of undef handler */
      break;
    }
  XSB_End_Instr()

  XSB_Start_Instr(allocate_gc,_allocate_gc) /* PAA */
    Def3ops
    Op2(get_xax);
    Op3((CPtr) (Cell)get_xxa);
    ADVANCE_PC(size_xxx);
    if (efreg_on_top(ereg))
      op1 = (Cell)(efreg-1);
    else {
      if (ereg_on_top(ereg)) op1 = (Cell)(ereg - *(cpreg-2*sizeof(Cell)+3));
      else op1 = (Cell)(ebreg-1);
    }
    *(CPtr *)((CPtr) op1) = ereg;
    *((byte **) (CPtr)op1-1) = cpreg;
    ereg = (CPtr)op1; 
    {/* initialize all permanent variables not in the first chunk to unbound */
      int  i = ((Cell)op3) - op2;
      CPtr p = ((CPtr)op1) - op2;
      while (i--) {
	bld_free(p);
        p--;
      }
    }
  XSB_End_Instr()

/* This is obsolete and is only kept for backwards compatibility for < 2.0 */
  XSB_Start_Instr(allocate,_allocate) /* PPP */
    Def1op
    ADVANCE_PC(size_xxx);
    if (efreg_on_top(ereg))
      op1 = (Cell)(efreg-1);
    else {
      if (ereg_on_top(ereg)) op1 = (Cell)(ereg - *(cpreg-2*sizeof(Cell)+3));
      else op1 = (Cell)(ebreg-1);
    }
    *(CPtr *)((CPtr) op1) = ereg;
    *((byte **) (CPtr)op1-1) = cpreg;
    ereg = (CPtr)op1; 
    { /* for old object files initialize pessimisticly but safely */
      int  i = 256;
      CPtr p = ((CPtr)op1)-2;
      while (i--) {
	bld_free(p);
        p--;
      }
    }
  XSB_End_Instr()

  XSB_Start_Instr(deallocate,_deallocate) /* PPP */
    ADVANCE_PC(size_xxx);
    cpreg = *((byte **)ereg-1);
    ereg = *(CPtr *)ereg;
  XSB_End_Instr()

  XSB_Start_Instr(proceed,_proceed)  /* PPP */
     proceed_sub;
  XSB_End_Instr()

  XSB_Start_Instr(xsb_execute,_xsb_execute) /* PPP-S */
    Def1op
    Psc psc;

    Op1(get_xxxs);
    ADVANCE_PC(size_xxxX);
    psc = (Psc)op1;
#ifdef CP_DEBUG
    pscreg = psc;
#endif
    call_sub(psc);
  XSB_End_Instr()

  XSB_Start_Instr(jump,_jump)   /* PPP-L */
    lpcreg = (byte *)get_xxxl;
  XSB_End_Instr()

  XSB_Start_Instr(jumpz,_jumpz)   /* PPR-L */
    Def1op
    Op1(Register(get_xxr));
    if (int_val(op1) == 0)
       lpcreg = (byte *)get_xxxl;
    else
         ADVANCE_PC(size_xxxX);
  XSB_End_Instr()

  XSB_Start_Instr(jumpnz,_jumpnz)    /* PPR-L */
    Def1op
    Op1(Register(get_xxr));
    if (oint_val(op1) != 0)
      lpcreg = (byte *)get_xxxl;
    else ADVANCE_PC(size_xxxX);;
  XSB_End_Instr()

  XSB_Start_Instr(jumplt,_jumplt)    /* PPR-L */
    Def1op
    Op1(Register(get_xxr));
    if (isinteger(op1)) {
      if (int_val(op1) < 0) lpcreg = (byte *)get_xxxl;
      else {ADVANCE_PC(size_xxxX);}
    } else if (isfloat(op1)) {
      if (float_val(op1) < 0.0) lpcreg = (byte *)get_xxxl;
      else {ADVANCE_PC(size_xxxX);}
    } else if (isboxedinteger(op1)) {
      if (boxedint_val(op1) < 0) lpcreg = (byte *)get_xxxl;
      else {ADVANCE_PC(size_xxxX);}
    }
  XSB_End_Instr() 

  XSB_Start_Instr(jumple,_jumple)    /* PPR-L */
    Def1op
    Op1(Register(get_xxr));
    if (isinteger(op1)) {
      if (int_val(op1) <= 0) lpcreg = (byte *)get_xxxl;
      else {ADVANCE_PC(size_xxxX);}
    } else if (isfloat(op1)) {
      if (float_val(op1) <= 0.0) lpcreg = (byte *)get_xxxl;
      else {ADVANCE_PC(size_xxxX);}
    } else if (isboxedinteger(op1)) {
      if (boxedint_val(op1) <= 0) lpcreg = (byte *)get_xxxl;
      else {ADVANCE_PC(size_xxxX);}
    }
  XSB_End_Instr() 

  XSB_Start_Instr(jumpgt,_jumpgt)    /* PPR-L */
    Def1op
    Op1(Register(get_xxr));
    if (isinteger(op1)) {
      if (int_val(op1) > 0) lpcreg = (byte *)get_xxxl;
      else {ADVANCE_PC(size_xxxX);}
    } else if (isfloat(op1)) {
      if (float_val(op1) > 0.0) lpcreg = (byte *)get_xxxl;
      else {ADVANCE_PC(size_xxxX);}
    } else if (isboxedinteger(op1)) {
      if (boxedint_val(op1) > 0) lpcreg = (byte *)get_xxxl;
      else {ADVANCE_PC(size_xxxX);}
    }
  XSB_End_Instr()

  XSB_Start_Instr(jumpge,_jumpge)    /* PPR-L */
    Def1op
    Op1(Register(get_xxr));
    if (isinteger(op1)) {
      if (int_val(op1) >= 0) lpcreg = (byte *)get_xxxl;
      else {ADVANCE_PC(size_xxxX);}
    } else if (isfloat(op1)) {
      if (float_val(op1) >= 0.0) lpcreg = (byte *)get_xxxl;
      else {ADVANCE_PC(size_xxxX);}
    } else if (isboxedinteger(op1)) {
      if (boxedint_val(op1) >= 0) lpcreg = (byte *)get_xxxl;
      else {ADVANCE_PC(size_xxxX);}
    }
  XSB_End_Instr() 

  XSB_Start_Instr(fail,_fail)    /* PPP */
    Fail1; 
  XSB_End_Instr()

  XSB_Start_Instr(noop,_noop)  /* PPA */
    Def1op
    Op1(get_xxa);
    ADVANCE_PC(size_xxx);
    lpcreg += (int)op1;
    lpcreg += (int)op1;
  XSB_End_Instr()

  XSB_Start_Instr(halt,_halt)  /* PPP */
    ADVANCE_PC(size_xxx);
    pcreg = lpcreg; 
    inst_begin = lpcreg;  /* hack for the moment to make this a ``creturn'' */
    return(0);	/* not "goto contcase"! */
  XSB_End_Instr()

  XSB_Start_Instr(builtin,_builtin)
    Def1op
    Op1(get_xxa);
    ADVANCE_PC(size_xxx);
    pcreg=lpcreg; 
    if (builtin_call(CTXTc (byte)(op1))) {lpcreg=pcreg;}
    else Fail1;
  XSB_End_Instr()

  XSB_Start_Instr(unifunc,_unifunc)   /* PAR */
    Def2ops
    Op1(get_xax);
    Op2(get_xxr);
    ADVANCE_PC(size_xxx);
    if (unifunc_call(CTXTc (int)(op1), (CPtr)op2) == 0) {
      xsb_error("Error in unary function call");
      Fail1;
    }
  XSB_End_Instr()

  XSB_Start_Instr(calld,_calld)   /* PPA-L */
    ADVANCE_PC(size_xxx); /* this is ok */
    cpreg = lpcreg+sizeof(Cell); 
    /*check_glstack_overflow(MAX_ARITY, lpcreg,OVERFLOW_MARGIN);  try eliminating?? */
    handle_xsb_profile_interrupt;
    lpcreg = *(pb *)lpcreg;
  XSB_End_Instr()

  XSB_Start_Instr(logshiftr,_logshiftr)  /* PRR */
    Def3ops
    Op1(Register(get_xrx));
    Op3(get_xxr);
    ADVANCE_PC(size_xxx);
    op2 = *(op3);
    XSB_Deref(op1); 
    XSB_Deref(op2);
    if (isinteger(op1)) {
      if (isinteger(op2)) {
        Integer temp = int_val(op2) >> int_val(op1);
        bld_oint(op3, temp); 
      }
      else if (isboxedinteger(op2)) {
        Integer temp = boxedint_val(op2) >> int_val(op1);
        bld_oint(op3, temp); 
      }
      else {arithmetic_abort(CTXTc op2, "'>>'", op1);}
    }
    else if (isboxedinteger(op1)) {
      if (isinteger(op2)) {
        Integer temp = int_val(op2) >> boxedint_val(op1);
        bld_oint(op3, temp); 
      }
      else if (isboxedinteger(op2)) {
        Integer temp = boxedint_val(op2) >> boxedint_val(op1);
        bld_oint(op3, temp); 
      }
      else {arithmetic_abort(CTXTc op2, "'>>'", op1);}
    }
    else {arithmetic_abort(CTXTc op2, "'>>'", op1);}
  XSB_End_Instr() 

  XSB_Start_Instr(logshiftl,_logshiftl)   /* PRR */
    Def3ops
    Op1(Register(get_xrx));
    Op3(get_xxr);
    ADVANCE_PC(size_xxx);
    op2 = *(op3);
    XSB_Deref(op1); 
    XSB_Deref(op2);
    if (isinteger(op1)) {
      if (isinteger(op2)) {
        Integer temp = int_val(op2) << int_val(op1);
        bld_oint(op3, temp); 
      }
      else if (isboxedinteger(op2)) {
        Integer temp = boxedint_val(op2) << int_val(op1);
        bld_oint(op3, temp); 
      }
      else {arithmetic_abort(CTXTc op2, "'<<'", op1);}
    }
    else if (isboxedinteger(op1)) {
      if (isinteger(op2)) {
        Integer temp = int_val(op2) << boxedint_val(op1);
        bld_oint(op3, temp); 
      }
      else if (isboxedinteger(op2)) {
        Integer temp = boxedint_val(op2) << boxedint_val(op1);
        bld_oint(op3, temp); 
      }
      else {arithmetic_abort(CTXTc op2, "'<<'", op1);}
    }
    else {arithmetic_abort(CTXTc op2, "'<<'", op1);}
  XSB_End_Instr() 

  XSB_Start_Instr(or,_or)   /* PRR */
    Def3ops
    Op1(Register(get_xrx));
    Op3(get_xxr);
    ADVANCE_PC(size_xxx);
    op2 = *(op3);
    XSB_Deref(op1); 
    XSB_Deref(op2);
    if (isinteger(op1)) {
      if (isinteger(op2)) {
        Integer temp = (int_val(op2)) | (int_val(op1));
        bld_oint(op3, temp); 
      }
      else if (isboxedinteger(op2)) {
        Integer temp = (boxedint_val(op2)) | (int_val(op1));
        bld_oint(op3, temp);
      }
      else {arithmetic_abort(CTXTc op2, "'\\/'", op1);}
    }
    else if (isboxedinteger(op1)) {
      if (isinteger(op2)) {
        Integer temp = (int_val(op2)) | (boxedint_val(op1));
        bld_oint(op3, temp); 
      }
      else if (isboxedinteger(op2)) {
        Integer temp = (boxedint_val(op2)) | (boxedint_val(op1));
        bld_oint(op3, temp); 
      }
      else {arithmetic_abort(CTXTc op2, "'\\/'", op1);}
    }
    else {arithmetic_abort(CTXTc op2, "'\\/'", op1);}
/**    if (!isinteger(op1) || !isinteger(op2)) {
      arithmetic_abort(CTXTc op2, "'\\/'", op1);
    }
    else { bld_oint(op3, int_val(op2) | int_val(op1)); } ***/
  XSB_End_Instr() 

  XSB_Start_Instr(and,_and)   /* PRR */
    Def3ops
    Op1(Register(get_xrx));
    Op3(get_xxr);
    ADVANCE_PC(size_xxx);
    op2 = *(op3);
    XSB_Deref(op1); 
    XSB_Deref(op2);
    if (isinteger(op1)) {
      if (isinteger(op2)) {
        Integer temp = (int_val(op2)) & (int_val(op1));
        bld_oint(op3, temp); 
      }
      else if (isboxedinteger(op2)) {
        Integer temp = (boxedint_val(op2)) & (int_val(op1));
        bld_oint(op3, temp);
      }
      else {arithmetic_abort(CTXTc op2, "'/\\'", op1);}
    }
    else if (isboxedinteger(op1)) {
      if (isinteger(op2)) {
        Integer temp = (int_val(op2)) & (boxedint_val(op1));
        bld_oint(op3, temp); 
      }
      else if (isboxedinteger(op2)) {
        Integer temp = (boxedint_val(op2)) & (boxedint_val(op1));
        bld_oint(op3, temp); 
      }
      else {arithmetic_abort(CTXTc op2, "'/\\'", op1);}
    }
    else {arithmetic_abort(CTXTc op2, "'/\\'", op1);}

/**    if (!isinteger(op1) || !isinteger(op2)) {
      arithmetic_abort(CTXTc op2, "'/\\'", op1);
    }
    else { bld_oint(op3, int_val(op2) & int_val(op1)); } **/
  XSB_End_Instr() 

  XSB_Start_Instr(negate,_negate)   /* PPR */
    DefOps13
    Op3(get_xxr);
    ADVANCE_PC(size_xxx);
    op1 = *(op3);
    XSB_Deref(op1);
    if (isinteger(op1)) { bld_int(op3, ~(int_val(op1))); }
    else if (isboxedinteger(op1)) { 
      Integer temp = ~(boxedint_val(op1));
      bld_oint(op3, temp); 
    }
    else { arithmetic_abort1(CTXTc "'\\'", op1); }
  XSB_End_Instr() 

#ifndef JUMPTABLE_EMULOOP
  default: {
    char message[80];
    sprintf(message, "Illegal opcode hex %x", *lpcreg); 
    xsb_exit(message);
  }
} /* end of switch */
#else
  _no_inst:
    {
      char message[80];
      sprintf(message, "Illegal opcode hex %x", *lpcreg);
      xsb_exit(message);
    }
#endif

return 0;

} /* end of emuloop() */

/*======================================================================*/
/*======================================================================*/

DllExport int call_conv xsb(CTXTdeclc int flag, int argc, char *argv[])
{ 
   char *startup_file;
   FILE *fd;
   unsigned int magic_num;
   static double realtime;	/* To retain its value across invocations */

   extern void dis(xsbBool);
   extern char *init_para(int, char **);
   extern void perform_IO_Redirect(int, char **);
   extern void init_machine(CTXTdecl), init_symbols(void);
#ifdef FOREIGN
#ifndef FOREIGN_ELF
#ifndef FOREIGN_WIN32
   extern char tfile[];
#endif
#endif
#endif

   if (flag == 0) {  /* initialize xsb */
     /* Set the name of the executable to the real name.
	The name of the executable could have been set in cinterf.c:xsb_init
	if XSB is called from C. In this case, we don't want `executable'
	to be overwritten, so we check if it is initialized. */

	perform_IO_Redirect(argc, argv);

#ifdef SIMPLESCALAR
     strcpy(executable,argv[0]);
#else
     if (executable[0] == '\0')
       xsb_executable_full_path(argv[0]);
#endif

     /* set install_dir, xsb_config_file and user_home */
     set_install_dir();
     set_config_file();
     set_user_home();

     realtime = real_time();
     setbuf(stdout, NULL);
     startup_file = init_para(argc, argv);	/* init parameters */

     init_machine(CTXT);	/* init space, regs, stacks */
     init_inst_table();		/* init table of instruction types */
     init_symbols();		/* preset a few symbols in PSC table */
     init_interrupt();		/* catch ^C interrupt signal */

     /* "b" does nothing, but POSIX allows it */
     fd = fopen(startup_file, "rb");

     if (!fd) {
       char message[256];
       sprintf(message, "The startup file, %s, could not be found!",
	       startup_file);
       xsb_exit(message);
     }
     magic_num = read_magic(fd);
     fclose(fd);
     if (magic_num == 0x11121307 || magic_num == 0x11121305)
       inst_begin = loader(startup_file,0);
     else
       xsb_exit("Incorrect startup file format");

     if (!inst_begin)
       xsb_exit("Error in loading startup file");

     if (xsb_mode == DISASSEMBLE) {
       dis(1);
       exit(0);
     }

     /* do it after initialization, so that typing 
	xsb -v or xsb -h won't create .xsb directory */
     set_xsbinfo_dir();

     return(0);

   } else if (flag == 1) {  /* continue execution */

     return(emuloop(CTXTc inst_begin));

   } else if (flag == 2) {  /* shutdown xsb */

#ifdef FOREIGN
#ifndef FOREIGN_ELF
#ifndef FOREIGN_WIN32
     if (fopen(tfile, "r")) unlink(tfile);
#endif
#endif
#endif

     if (xsb_mode != C_CALLING_XSB) {
       realtime = real_time() - realtime;
       fprintf(stdmsg, "\nEnd XSB (cputime %.2f secs, elapsetime ",
	       cpu_time());
       if (realtime < 600.0)
	 fprintf(stdmsg, "%.2f secs)\n", realtime);
       else
	 fprintf(stdmsg, "%.2f mins)\n", realtime/60.0);
     }
     return(0);
   }
   return(1);
}  /* end of xsb() */

/*======================================================================*/
