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

#include "configs/xsb_config.h"
#include "debugs/xsb_debug.h"

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

/*
 * Variable ans_var_pos_reg is a pointer to substitution factor of an
 * answer in the heap.  It is used and set in function
 * variant_answer_search().  The name of this variable is from VarPosReg, a
 * variable used in variant_call_search() to save the substitution factor
 * of the call.
 */
CPtr	ans_var_pos_reg;

/*----------------------------------------------------------------------*/

#include "tr_delay.h"
#include "tr_code_xsb_i.h"

#ifdef CHAT
#include "chat.h"
#endif

/*----------------------------------------------------------------------*/

#define pad		(lpcreg++)
#define ppad		(lpcreg+=2)
#define pppad		(lpcreg+=3)
#define opregaddr	(rreg+(*lpcreg++))
#define opvaraddr	(ereg-(Cell)(*lpcreg++))

#ifdef BITS64
#define pad64		(lpcreg += 4)
#else
#define pad64
#endif

#define opreg		cell(opregaddr)
#define opvar		cell(opvaraddr)
#define op1byte		op1 = (Cell)(*lpcreg++)
#define op2byte		op2 = (Cell)(*lpcreg++)
#define op3byte		op3 = (CPtr)((int)(*lpcreg++))
#define op2word		op2 = (Cell)(*(CPtr)lpcreg); lpcreg+=sizeof(Cell)
#define op3word		op3 = *(CPtr)lpcreg; lpcreg+=sizeof(Cell)

#define ADVANCE_PC	(lpcreg+=sizeof(Cell))

/* Be sure that flag only has the following two values.	*/

#define WRITE		1
#define READFLAG	0

/*----------------------------------------------------------------------*/
/* The following macros work for all CPs.  Make sure this remains	*/
/* the case...								*/
/*----------------------------------------------------------------------*/

#define Fail1 lpcreg = cp_pcreg(breg);

/* why the test on pcheck_complete_inst in the following non-CHAT macro ? */

#ifdef CHAT
#define restore_trail_condition_registers(BREG) \
      ebreg = cp_ebreg(BREG); \
      hbreg = cp_hreg(BREG);
#else
#define restore_trail_condition_registers(breg) \
      if (*breg != (Cell) &check_complete_inst) { \
	ebreg = cp_ebreg(breg); \
	hbreg = cp_hreg(breg); \
      } 
#endif

/*----------------------------------------------------------------------*/

extern int  builtin_call(int), unifunc_call(int, CPtr);
extern Cell builtin_table[BUILTIN_TBL_SZ][2];
extern Pair build_call(Psc);

#ifdef DEBUG
extern void debug_inst(byte *, CPtr);
extern void print_completion_stack(void);
extern void print_subgoal(FILE *, SGFrame);
extern void print_delay_list(FILE *, CPtr);
extern void printterm(Cell, byte, int);
#endif

/**static int  (*dyn_pred)(); unused-remove soon**/

bool neg_delay;
int  xwammode, level_num;

#ifdef DEBUG
int  xctr;
#endif

/*----------------------------------------------------------------------*/

#ifdef CHAT
#include "chatsched_xsb_i.h"
#else
#include "schedrev_xsb_i.h"
#endif

#ifndef LOCAL_EVAL 
#include "wfs_xsb_i.h" 
#endif 

/*----------------------------------------------------------------------*/

/* place for a meaningful message when segfault is detected */
char *xsb_default_segfault_msg =
     "\n++Memory violation occurred during evaluation.\n++When reporting this XSB bug to xsb-development@lists.sourceforge.net,\n++please supply the steps necessary to reproduce the bug.\n";
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

static int emuloop(byte *startaddr)
{
  register byte *lpcreg;
  register CPtr rreg;
  register Cell op1, op2;	/* (*CPtr) */
  CPtr op3, xtemp1, xtemp2;
  byte flag = READFLAG;  	/* read/write mode flag */
  int  restore_type;	/* 0 for retry restore; 1 for trust restore */ 

#if (defined(GC) && defined(GC_TEST))
/* Used only in the garbage collection test; does not affect emulator o/w */
#define GC_INFERENCES 66 /* make sure the garbage collection test is hard */
  static int infcounter = 0;
#endif

  xsb_segfault_message = xsb_default_segfault_msg;
  rreg = reg; /* for SUN */
  op1 = op2 = (Cell) NULL;
  lpcreg = (pb)&reset_inst;  /* start by initializing abort handler */

contcase:     /* the main loop */

#ifdef DEBUG
  if (flags[PIL_TRACE]) debug_inst(lpcreg, ereg);
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
  
  switch (*lpcreg++) {
    
  case getpvar:  /* PVR */
    pad;
    op1 = (Cell)(opvaraddr);
    /* trailing is needed here because this instruction can also be
       generated *after* the occurrence of the first call - kostis */
    op2 = opreg; 
    bind_copy((CPtr)op1, op2);      /* In WAM bld_copy() */
    pad64;
    goto contcase;
    
  case getpval: /* PVR */
    pad; op1 = opvar; op2 = opreg;
    pad64;
    goto nunify;

  case getstrv: /* PPV-S */
    ppad; op1 = opvar; pad64; op2word;
    nunify_with_str(op1,op2);
    goto contcase;

  case gettval: /* PRR */
    pad; op1 = opreg; op2 = opreg;
    pad64;
    goto nunify;

  case getcon: /* PPR-C */
    ppad; op1 = opreg; pad64; op2word;
    nunify_with_con(op1,op2);
    goto contcase;

  case getnil: /* PPR */
    ppad; op1 = opreg;
    pad64;
    nunify_with_nil(op1);
    goto contcase;	

  case getstr: /* PPR-S */
    ppad; op1 = opreg; pad64; op2word;
    nunify_with_str(op1,op2);
    goto contcase;

  case getlist: /* PPR */
    ppad; op1 = opreg;
    pad64;
    nunify_with_list_sym(op1);
    goto contcase;

  case getattv: /* PPR */
    ppad; op1 = opreg;
    pad64;
    nunify_with_attv(op1);
    goto contcase;

/* tls 12/8/92 */
  case unipvar: /* PPV */
    ppad; op1 = (Cell)(opvaraddr);
    pad64;
    if (flag) {	/* if (flag == WRITE) */
      bind_ref((CPtr)op1, hreg);
      new_heap_free(hreg);
    } else {
      /* also introduce trailing here - bmd & kostis
         was: bld_copy((CPtr)op1, *(sreg++)); */
      bind_copy((CPtr)op1, *(sreg));
      sreg++;
    }
    goto contcase;

  case unipval: /* PPV */
    ppad; op1 = opvar;
    pad64;
    if (flag) { /* if (flag == WRITE) */
      nbldval(op1); 
    } 
    else {
      op2 = *(sreg++);
      goto nunify;
    } 
    goto contcase;

  case unitvar: /* PPR */
    ppad; op1 = (Cell)(opregaddr);
    pad64;
    if (flag) {	/* if (flag == WRITE) */
      bld_ref((CPtr)op1, hreg);
      new_heap_free(hreg);
    }
    else {
      bld_copy((CPtr)op1, *(sreg++));
    }
    goto contcase;

  case unitval: /* PPR */
    ppad; op1 = opreg;
    pad64;
    if (flag) { /* if (flag == WRITE) */
      nbldval(op1); 
      goto contcase;
    }
    else {
      op2 = *(sreg++);
      goto nunify;
    } 

  case unicon: /* PPP-C */
    pppad; pad64; op2word;
    if (flag) {	/* if (flag == WRITE) */
      new_heap_string(hreg, (char *)op2);
    }
    else {  /* op2 already set */
      op1 = *(sreg++);
      nunify_with_con(op1,op2);
    }
    goto contcase;

  case uninil: /* PPP */
    pppad;
    pad64;
    if (flag) {	/* if (flag == WRITE) */
      new_heap_nil(hreg);
    }
    else {
      op1 = *(sreg++);
      nunify_with_nil(op1);
    }
    goto contcase;

  case getnumcon: /* PPR-N */
    ppad; op1 = opreg; pad64; op2word;
    nunify_with_num(op1,op2);
    goto contcase;

  case getfloat: /* PPR-N */
    ppad; op1 = opreg; pad64; op2word;
    nunify_with_float(op1,op2);
    goto contcase;

  case putnumcon: /* PPR-N */
    ppad; op1 = (Cell)(opregaddr);
    pad64;
    op2 = *(pw)lpcreg; ADVANCE_PC;
    bld_int((CPtr)op1, op2);
    goto contcase;

  case putfloat: /* PPR-N */
    ppad; op1 = (Cell)(opregaddr);
    pad64;
    bld_float((CPtr)op1, asfloat(*(pw)lpcreg));
    ADVANCE_PC;
    goto contcase;

  case putpvar: /* PVR */
    pad;
    op1 = (Cell)(opvaraddr);
    bld_free((CPtr)op1);
    op2 = (Cell)(opregaddr);
    pad64;
    bld_ref((CPtr)op2, (CPtr)op1);
    goto contcase;

  case putpval: /* PVR */
    pad; op1 = (Cell)(opvaraddr);
    bld_copy(opregaddr, *((CPtr)op1));
    pad64;
    goto contcase;

  case puttvar: /* PRR */
    pad; op1 = (Cell)(opregaddr); op2 = (Cell)(opregaddr);
    pad64;
    bld_ref((CPtr)op1, hreg);
    bld_ref((CPtr)op2, hreg);
    new_heap_free(hreg); 
    goto contcase;

/* tls 12/8/92 */
  case putstrv: /*  PPV-S */
    ppad; op1 = (Cell)(opvaraddr);
    pad64;
    bind_cs((CPtr)op1, (Pair)hreg);
    new_heap_functor(hreg, *(Psc *)lpcreg); ADVANCE_PC;
    goto contcase;

  case putcon: /* PPR-C */
    ppad; op1 = (Cell)(opregaddr);
    pad64;
    bld_string((CPtr)op1, *(char **)lpcreg); ADVANCE_PC;
    goto contcase;
    
  case putnil: /* PPR */
    ppad; op1 = (Cell)(opregaddr);
    pad64;
    bld_nil((CPtr)op1);
    goto contcase;
    
/* doc tls -- differs from putstrv since it pulls from a register */
  case putstr: /* PPR-S */
    ppad; op1 = (Cell)(opregaddr);
    pad64;
    bld_cs((CPtr)op1, (Pair)hreg);
    new_heap_functor(hreg, *(Psc *)lpcreg); ADVANCE_PC;
    goto contcase;

  case putlist: /* PPR */
    ppad; op1 = (Cell)(opregaddr);
    pad64;
    bld_list((CPtr)op1, hreg);
    goto contcase;

  case putattv: /* PPR */
    ppad; op1 = (Cell)(opregaddr);
    pad64;
    bld_attv((CPtr)op1, hreg);
    new_heap_free(hreg);
    goto contcase;

  case bldpvar: /* PPV */
    ppad; op1 = (Cell)(opvaraddr);
    pad64;
    /* tls 12/8/92 */
    bind_ref((CPtr)op1, hreg); /* trailing is needed: if o/w see ai_tests */
    new_heap_free(hreg);
    goto contcase;
    
  case bldpval: /* PPV */
    ppad; op1 = opvar;
    pad64;
    nbldval(op1);
    goto contcase;
    
  case bldtvar: /* PPR */
    ppad; op1 = (Cell)(opregaddr);
    pad64;
    bld_ref((CPtr)op1, hreg);
    new_heap_free(hreg);
    goto contcase;
    
  case bldtval: /* PPR */
    ppad; op1 = opreg;
    pad64;
    nbldval(op1);
    goto contcase;
    
  case bldcon: /* PPP-C */
    pppad;
    pad64;
    new_heap_string(hreg, *(char **)lpcreg);
    ADVANCE_PC;
    goto contcase;
    
  case bldnil: /* PPP */
    pppad;
    pad64;
    new_heap_nil(hreg);
    goto contcase;
    
  case getlist_tvar_tvar: /* RRR */
    op1 = opreg;
    deref(op1);
    if (isref(op1)) {
      bind_list((CPtr)(op1), hreg);
      op1 = (Cell)(opregaddr);
      bld_ref((CPtr)op1, hreg);
      new_heap_free(hreg);
      op1 = (Cell)(opregaddr);
      pad64;
      bld_ref((CPtr)op1, hreg);
      new_heap_free(hreg);
    } else if (islist(op1)) {
      sreg = clref_val(op1);
      op1 = (Cell)(opregaddr);
      bld_ref((CPtr)op1, *(sreg));
      op1 = (Cell)(opregaddr);
      pad64;
      bld_ref((CPtr)op1, *(sreg+1));
    }
    else Fail1;
    goto contcase;	/* end getlist_tvar_tvar */

  case uninumcon: /* PPP-N */
    pppad; pad64; op2word; /* num in op2 */
    if (flag) {	/* if (flag == WRITE) */
      new_heap_num(hreg, (Integer)op2);
    }
    else {  /* op2 set */
      op1 = *(sreg++);
      nunify_with_num(op1,op2);
    }
    goto contcase;

  case unifloat: /* PPPF */
    pppad; pad64; op2word; /* num in op2 */
    if (flag) {	/* if (flag == WRITE) */
      new_heap_float(hreg, asfloat(op2));
    }
    else {  /* op2 set */
      op1 = cell(sreg++);
      nunify_with_float(op1,op2);
    }
    goto contcase;
    
  case bldnumcon: /* PPP-N */
    pppad; pad64; op2word; /* num to op2 */
    new_heap_num(hreg, (Integer)op2);
    goto contcase;

  case bldfloat: /* PPP-F */
    pppad; pad64; op2word; /* num to op2 */
    new_heap_float(hreg, asfloat(op2));
    goto contcase;

  case trymeelse: /* PPA-L */
    ppad; op1byte; pad64; op2word;
    goto subtryme;

  case retrymeelse: /* PPA-L */
    ppad; op1byte;
    pad64;
    cp_pcreg(breg) = *(byte **)lpcreg;
    ADVANCE_PC;
    restore_type = 0;
    goto restore_sub;

  case trustmeelsefail: /* PPA */
    ppad; op1byte;
    pad64;
    restore_type = 1;
    goto restore_sub;

  case try: /* PPA-L */
    ppad; op1byte;
    pad64;
    op2 = (Cell)((Cell)lpcreg + sizeof(Cell));
    lpcreg = *(pb *)lpcreg; /* = *(pointer to byte pointer) */
    goto subtryme;

  case retry: /* PPA-L */
    ppad; op1byte;
    pad64;
    cp_pcreg(breg) = lpcreg+sizeof(Cell);
    lpcreg = *(pb *)lpcreg;
    restore_type = 0;
    goto restore_sub;

  case trust: /* PPA-L */
    ppad; op1byte;
    pad64;
    lpcreg = *(pb *)lpcreg;
    restore_type = 1;
    goto restore_sub;

  case getVn: /* PPV */
    ppad; op1 = (Cell)(opvaraddr);
    pad64;
    cell((CPtr)op1) = (Cell)tcp_subgoal_ptr(breg);
    goto contcase;

  case getpbreg: /* PPV */
    ppad; op1 = (Cell)(opvaraddr);
    pad64;
    bld_int((CPtr)op1, ((pb)tcpstack.high - (pb)breg));
    goto contcase;

  case gettbreg: /* PPR */
    ppad; op1 = (Cell)(opregaddr);
    pad64;
    bld_int((CPtr)op1, ((pb)tcpstack.high - (pb)breg));
    goto contcase;

  case putpbreg: /* PPV */
    ppad; op1 = opvar;
    pad64;
    cut_code(op1);

  case puttbreg: /* PPR */
    ppad; op1 = opreg;
    pad64;
    cut_code(op1);

  case jumptbreg: /* PPR-L */	/* ??? */
    ppad; op1 = (Cell)(opregaddr);
    pad64;
    bld_int((CPtr)op1, ((pb)tcpstack.high - (pb)breg));
    lpcreg = *(byte **)lpcreg;
    goto contcase;

  case test_heap: /* PPA-N */
    ppad;
    op1byte;  /* op1 = the arity of the procedure */
    pad64;
    op2 = *(pw)lpcreg; lpcreg+=4;
    pad64;
#ifdef GC_TEST
    if ((infcounter++ > GC_INFERENCES) || ((ereg - hreg) < (long)op2))
      {
	infcounter = 0;
        fprintf(stddbg, ".");
#else
    if ((ereg - hreg) < (long)op2)
      {
#endif
        if (gc_heap(op1)) { /* garbage collection potentially modifies hreg */
	  if ((ereg - hreg) < (long)op2) {
	    if (flags[STACK_REALLOC]) {
	      if (glstack_realloc(resize_stack(glstack.size,0),op1) != 0) {
		local_global_exception(lpcreg);
		goto contcase;
	      }
	    } else {
	      xsb_warn("Reallocation is turned OFF !");
	      local_global_exception(lpcreg);
	      goto contcase;
	    }
	  }
	}
	/* are there any localy cached quantities that must be reinstalled ? */
      }
    goto contcase;

  case switchonterm: /* PPR-L-L */
    ppad; 
    op1 = opreg;
    pad64;
    deref(op1);
    switch (cell_tag(op1)) {
    case FREE:
    case REF1:
    case ATTV:
      lpcreg += 2 * sizeof(Cell);
      break;
    case INT:
    case STRING:
    case FLOAT:
      lpcreg = *(pb *)lpcreg;	    
      break;
    case CS:
      if (get_arity(get_str_psc(op1)) == 0) {
	lpcreg = *(pb *)lpcreg;
	break;
      }
    case LIST:	/* include structure case here */
      lpcreg += sizeof(Cell); lpcreg = *(pb *)lpcreg; 
      break;
    }
    goto contcase;

  case switchonbound: /* PPR-L-L */
    /* op1 is register, op2 is hash table offset, op3 is modulus */
    ppad; 
    op1 = opreg;
    pad64;
    deref(op1);
    switch (cell_tag(op1)) {
    case FREE:
    case REF1:
    case ATTV:
      lpcreg += 2 * sizeof(Cell);
      goto sotd2;
    case INT: 
    case FLOAT:	/* Yes, use int_val to avoid conversion problem */
      op1 = (Cell)int_val(op1);
      break;
    case LIST:
      op1 = (Cell)(list_str); 
      break;
    case CS:
      op1 = (Cell)get_str_psc(op1);
      break;
    case STRING:	/* We should change the compiler to avoid this test */
      op1 = (Cell)(isnil(op1) ? 0 : string_val(op1));
      break;
    }
    op2 = (Cell)(*(byte **)(lpcreg));
    lpcreg += sizeof(Cell);
    op3 = *(CPtr *)lpcreg;
    /* doc tls -- op2 + (op1%size)*4 */
    lpcreg =
      *(byte **)((byte *)op2 + ihash((Cell)op1, (Cell)op3) * sizeof(Cell));
  sotd2: goto contcase;
      
  case switchon3bound: /* RRR-L-L */
  {
    int  i, j = 0;
    Cell opa[3]; 
    /* op1 is register, op2 is hash table offset, op3 is modulus */
    if (*lpcreg == 0) { lpcreg++; opa[0] = 0; }
    else opa[0] = (Cell)opreg;
    opa[1] = (Cell)opreg;
    opa[2] = (Cell)opreg;
    pad64;
    op2 = (Cell)(*(byte **)(lpcreg)); lpcreg += sizeof(Cell);
    op3 = *(CPtr *)lpcreg; 
    /* This is not a good way to do this, but until we put retract into C,
       or add new builtins, it will have to do. */
    for (i = 0; i <= 2; i++) {
      if (opa[i] != 0) {
	op1 = opa[i];
	deref(op1);
	switch (cell_tag(op1)) {
	case FREE:
	case REF1:
	case ATTV:
	  lpcreg += sizeof(Cell);
	  goto sob3d2;
	case INT: 
	case FLOAT:	/* Yes, use int_val to avoid conversion problem */
	  op1 = (Cell)int_val(op1);
	  break;
	case LIST:
	  op1 = (Cell)(list_str); 
	  break;
	case CS:
	  op1 = (Cell)get_str_psc(op1);
	  break;
	case STRING:
	  op1 = (Cell)string_val(op1);
	  break;
	default:
	  xsb_error("Illegal operand in switchon3bound");
	  break;
	}
	j = (j<<1) + ihash((Cell)op1, (Cell)op3);
      }
    }
    lpcreg = *(byte **)((byte *)op2 + ((j % (Cell)op3) * sizeof(Cell)));
  sob3d2: goto contcase;
  }

  case trymeorelse: /* PPA-L */
    pppad;
    pad64;
    op1 = 0;
    op2word;
    cpreg = lpcreg;
    goto subtryme;

  case retrymeorelse: /* PPA-L */
    pppad;
    pad64;
    op1 = 0;
    cp_pcreg(breg) = *(byte **)lpcreg;
    ADVANCE_PC;
    cpreg = lpcreg;
    restore_type = 0;
    goto restore_sub;

  case trustmeorelsefail: /* PPA */
    pppad;
    pad64;
    op1 = 0;
    cpreg = lpcreg+sizeof(Cell);
    restore_type = 1;
    goto restore_sub;

  case dyntrustmeelsefail: /* PPA-L, second word ignored */
    ppad; op1byte; 
    pad64;
    ADVANCE_PC;
    restore_type = 1;
    goto restore_sub;

/*----------------------------------------------------------------------*/

#include "slginsts_xsb_i.h"
#include "tc_insts_xsb_i.h"

/*----------------------------------------------------------------------*/

  case term_comp: /* RRR */
    op1 = (Cell)*(opregaddr);
    op2 = (Cell)*(opregaddr);
    bld_int(opregaddr, compare(op1, op2));
    pad64;
    goto contcase;

  case movreg: /* PRR */
    pad;
    op1 = (Cell)(opregaddr);
    bld_copy(opregaddr, *((CPtr)op1));
    pad64;
    goto contcase;

#define ARITHPROC(OP, STROP) \
    pad;								\
    op1 = opreg;							\
    op3 = opregaddr;							\
    pad64;								\
    op2 = *(op3);							\
    deref(op1);								\
    deref(op2);								\
    if (isinteger(op1)) {						\
	if (isinteger(op2)) {						\
	    bld_int(op3, int_val(op2) OP int_val(op1));	}		\
	else if (isfloat(op2)) {					\
	    bld_float(op3, float_val(op2) OP (Float)int_val(op1)); }	\
	else { arithmetic_abort(op2, STROP, op1); }                     \
    } else if (isfloat(op1)) {						\
	if (isfloat(op2)) {						\
	    bld_float(op3, float_val(op2) OP float_val(op1)); }		\
	else if (isinteger(op2)) {					\
	    bld_float(op3, (Float)int_val(op2) OP float_val(op1)); }	\
	else { arithmetic_abort(op2, STROP, op1); } 	                \
    } else { arithmetic_abort(op2, STROP, op1); }

  case addreg: /* PRR */
    ARITHPROC(+, "+");
    goto contcase; 

  case subreg: /* PRR */
    pad;
    op1 = opreg;
    op3 = opregaddr;							
    pad64;
    op2 = *(op3);
    deref(op1);
    deref(op2);
    if (isinteger(op1)) {						
      if (isinteger(op2)) {
	bld_int(op3, int_val(op2) - int_val(op1)); }
      else if (isfloat(op2)) {
	bld_float(op3, float_val(op2) - (Float)int_val(op1)); }
      else { arithmetic_abort(op2, "-", op1); }
    } else if (isfloat(op1)) {
      if (isfloat(op2)) {
	bld_float(op3, float_val(op2) - float_val(op1)); }
      else if (isinteger(op2)) {
	bld_float(op3, (Float)int_val(op2) - float_val(op1)); }
      else arithmetic_abort(op2, "-", op1);
    }
    else arithmetic_abort(op2, "-", op1);
    goto contcase; 

  case mulreg: /* PRR */
    ARITHPROC(*, "*");
    goto contcase; 

  case divreg: /* PRR */
    pad;
    op1 = opreg;
    op3 = opregaddr;
    pad64;
    op2 = *(op3);
    deref(op1);
    deref(op2);
    if (isinteger(op1)) {
      if (isinteger(op2)) {
	bld_float(op3, (Float)int_val(op2)/(Float)int_val(op1)); }
      else if (isfloat(op2)) {
	bld_float(op3, float_val(op2)/(Float)int_val(op1)); }
      else { arithmetic_abort(op2, "/", op1); }
    } else if (isfloat(op1)) {
      if (isfloat(op2)) {
	bld_float(op3, float_val(op2)/float_val(op1)); }
      else if (isinteger(op2)) {
	bld_float(op3, (Float)int_val(op2)/float_val(op1)); }
      else { arithmetic_abort(op2, "/", op1); }
    } else { arithmetic_abort(op2, "/", op1); }
    goto contcase; 

  case idivreg: /* PRR */
    pad;
    op1 = opreg;
    op3 = opregaddr;
    pad64;
    op2 = *(op3);
    deref(op1);
    deref(op2);
    if (isinteger(op1) && isinteger(op2)) {
      if (int_val(op1) != 0) { bld_int(op3, int_val(op2) / int_val(op1)); }
      else {
	err_handle(ZERO_DIVIDE, 2,
		   "arithmetic expression involving is/2 or eval/2",
		   2, "non-zero number", op1);
	lpcreg = pcreg;
      }
    }
    else { arithmetic_abort(op2, "//", op1); }
    goto contcase; 

  case int_test_z:   /* PPR-N-L */
    ppad;
    op1 = opreg; pad64;
    deref(op1); op2word;
    if (isnumber(op1)) {
      if ((int_val(op1) - (Integer)op2) == 0)
	lpcreg = *(byte **)lpcreg;
      else ADVANCE_PC;
    }
    else {
      ADVANCE_PC;
      arithmetic_comp_abort(op1, "=/=", op2);
    }
    goto contcase;

  case int_test_nz:   /* PPR-N-L */
    ppad;
    op1 = opreg; pad64;
    deref(op1); op2word;
    if (isnumber(op1)) {
      if ((int_val(op1) - (Integer)op2) != 0)
	lpcreg = *(byte **)lpcreg;
      else ADVANCE_PC;
    }
    else {
      ADVANCE_PC;
      arithmetic_comp_abort(op1, "=:=", op2);
    }
    goto contcase;

  case putdval: /* PVR */
    pad; 
    op1 = opvar;
    deref(op1);
    op2 = (Cell)(opregaddr);
    pad64;
    bld_copy((CPtr)op2, op1);
    goto contcase;

  case putuval: /* PVR */
    pad;
    op1 = opvar;
    op2 = (Cell)(opregaddr);
    pad64;
    deref(op1);
    if (isnonvar(op1) || ((CPtr)(op1) < hreg) || ((CPtr)(op1) >= ereg)) {
      bld_copy((CPtr)op2, op1);
    } else {
      bld_ref((CPtr)op2, hreg);
      bind_ref((CPtr)(op1), hreg);
      new_heap_free(hreg);
    } 
    goto contcase;

  /*
   * Instruction `check_interrupt' is used before `new_answer_dealloc' to
   * handle the pending attv interrupts.  It is similar to `call' but the
   * second argument (S) is not used currently.
   */
  case check_interrupt: { /* PPA-S */
    Pair true_pair;
    int new_indicator;

    pppad; pad64; op2word;
    if (int_val(cell(interrupt_reg)) > 0) {
      cpreg = lpcreg;
      true_pair = insert("true", 0, global_mod, &new_indicator);
      bld_cs(reg + 2, hreg);	/* see subp.c: build_call() */
      new_heap_functor(hreg, pair_psc(true_pair));
      bld_copy(reg + 1, build_interrupt_chain());
      lpcreg = get_ep((Psc) flags[MYSIG_ATTV + 32]);
    }
    goto contcase;
  }

  case call: { /* PPA-S */
    Psc psc;

    pppad; pad64; op2word;	/* the first arg is used later by alloc */
    cpreg = lpcreg;
    psc = (Psc)op2;
    call_sub(psc);
    goto contcase;
  }

  case call_forn: { /* PPP-L, maybe use userfun instr? */
    pppad; pad64; op2word;
    if (((PFI)op2)())  /* call foreign function */
      lpcreg = cpreg;
    else Fail1;
    goto contcase;
  }

  case load_pred: { /* PPP-S */
    Psc psc;
    
    pppad; pad64; op2word;
    psc = (Psc)op2;
    /* check env or type to give (better) error msgs? */
    switch (get_type(psc)) {
    case T_PRED:
    case T_DYNA:
      xsb_abort("System Error: trying to load an already loaded pred");
    default:
      /* printf("loading module %s for %s/%d\n",
	 get_name(get_data(psc)),get_name(psc),get_arity(psc)); */
      bld_cs(reg+1, build_call(psc));   /* put call-term in r1 */
      psc = (Psc)flags[MYSIG_UNDEF+32]; /* get psc of undef handler */
      bld_int(reg+2, MYSIG_UNDEF);      /* undef-pred code */
      lpcreg = get_ep(psc);             /* ep of undef handler */
      break;
    }
    goto contcase;
  }

  case allocate_gc: /* PAA */
    pad; op2byte; op3byte;
    pad64;
#if (!defined(CHAT))
    if (efreg_on_top(ereg))
      op1 = (Cell)(efreg-1);
    else {
#endif
      if (ereg_on_top(ereg)) op1 = (Cell)(ereg - *(cpreg-2*sizeof(Cell)+3));
      else op1 = (Cell)(ebreg-1);
#if (!defined(CHAT))
    }
#endif
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
    goto contcase;

/* This is obsolete and is only kept for backwards compatibility for < 2.0 */
  case allocate: /* PPP */
    pppad; 
    pad64;
#if (!defined(CHAT))
    if (efreg_on_top(ereg))
      op1 = (Cell)(efreg-1);
    else {
#endif
      if (ereg_on_top(ereg)) op1 = (Cell)(ereg - *(cpreg-2*sizeof(Cell)+3));
      else op1 = (Cell)(ebreg-1);
#if (!defined(CHAT))
    }
#endif
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
    goto contcase;

  case deallocate: /* PPP */
    pppad; 
    pad64;
    cpreg = *((byte **)ereg-1);
    ereg = *(CPtr *)ereg;
    goto contcase;

  case proceed:  /* PPP */
    lpcreg = cpreg;
    goto contcase;

  case execute: { /* PPP-S */
    Psc psc;

    pppad; pad64; op2word;
    psc = (Psc)op2;
    call_sub(psc);
    goto contcase;
  }

  case jump:   /* PPP-L */
    pppad;
    pad64;
    lpcreg = *(byte **)lpcreg;
    goto contcase;

  case jumpz:   /* PPR-L */
    ppad; op1 = opreg;
    pad64;
    if (int_val(op1) == 0)
      lpcreg = *(byte **)lpcreg;
    else ADVANCE_PC;
    goto contcase;

  case jumpnz:    /* PPR-L */
    ppad; op1 = opreg;
    pad64;
    if (int_val(op1) != 0)
      lpcreg = *(byte **)lpcreg;
    else ADVANCE_PC;
    goto contcase;

  case jumplt:    /* PPR-L */
    ppad; op1 = opreg;
    pad64;
    if ((isinteger(op1) && int_val(op1) < 0) ||
	(isfloat(op1) && float_val(op1) < 0.0))
      lpcreg = *(byte **)lpcreg;
    else ADVANCE_PC;
    goto contcase; 

  case jumple:    /* PPR-L */
    ppad; op1 = opreg;
    pad64;
    if ((isinteger(op1) && int_val(op1) <= 0) ||
	(isfloat(op1) && float_val(op1) <= 0.0))
      lpcreg = *(byte **)lpcreg;
    else ADVANCE_PC;
    goto contcase; 

  case jumpgt:    /* PPR-L */
    ppad; op1 = opreg;
    pad64;
    if ((isinteger(op1) && int_val(op1) > 0) ||
	(isfloat(op1) && float_val(op1) > 0.0))
      lpcreg = *(byte **)lpcreg;
    else ADVANCE_PC;
    goto contcase;

  case jumpge:    /* PPR-L */
    ppad; op1 = opreg;
    pad64;
    if ((isinteger(op1) && int_val(op1) >= 0) ||
	(isfloat(op1) && float_val(op1) >= 0.0))
      lpcreg = *(byte **)lpcreg;
    else ADVANCE_PC;
    goto contcase; 

  case fail:    /* PPP */
    Fail1; 
    goto contcase;

  case noop:  /* PPA */
    ppad; op1byte;
    pad64;
    lpcreg += (int)op1;
    lpcreg += (int)op1;
    goto contcase;

  case halt:  /* PPP */
    pppad;
    pad64;
    pcreg = lpcreg; 
    inst_begin = lpcreg;  /* hack for the moment to make this a ``creturn'' */
    return(0);	/* not "goto contcase"! */

  case builtin:
    ppad; op1byte; pad64; pcreg=lpcreg; 
    if (builtin_call((int)(op1))) {lpcreg=pcreg;}
    else Fail1;
    goto contcase;

  case unifunc:   /* PAR */
    pad;
    op1byte;
    if (unifunc_call((int)(op1), opregaddr) == 0) {
      xsb_error("Error in unary function call");
      Fail1;
    }
    pad64;
    goto contcase;

  case calld:   /* PPA-L */
    pppad;
    pad64;
    cpreg = lpcreg+sizeof(Cell); 
    check_glstack_overflow(MAX_ARITY, lpcreg, OVERFLOW_MARGIN, goto contcase);
    lpcreg = *(pb *)lpcreg;
    goto contcase;

  case logshiftr:  /* PRR */
    pad;
    op1 = opreg;
    op3 = opregaddr;
    pad64; 
    op2 = *(op3);
    deref(op1); 
    deref(op2);
    if (!isinteger(op1) || !isinteger(op2)) {
      arithmetic_abort(op2, "'>>'", op1);
    }
    else { bld_int(op3, int_val(op2) >> int_val(op1)); }
    goto contcase; 

  case logshiftl:   /* PRR */
    pad;
    op1 = opreg;
    op3 = opregaddr;
    pad64;
    op2 = *(op3);
    deref(op1); 
    deref(op2);
    if (!isinteger(op1) || !isinteger(op2)) {
      arithmetic_abort(op2, "'<<'", op1);
    }
    else { bld_int(op3, int_val(op2) << int_val(op1)); }
    goto contcase; 

  case or:   /* PRR */
    pad;
    op1 = opreg;
    op3 = opregaddr;
    pad64;
    op2 = *(op3);
    deref(op1); 
    deref(op2);
    if (!isinteger(op1) || !isinteger(op2)) {
      arithmetic_abort(op2, "'\\/'", op1);
    }
    else { bld_int(op3, int_val(op2) | int_val(op1)); }
    goto contcase; 

  case and:   /* PRR */
    pad;
    op1 = opreg;
    op3 = opregaddr;
    pad64;
    op2 = *(op3);
    deref(op1); 
    deref(op2);
    if (!isinteger(op1) || !isinteger(op2)) {
      arithmetic_abort(op2, "'/\\'", op1);
    }
    else { bld_int(op3, int_val(op2) & int_val(op1)); }
    goto contcase; 

  case negate:   /* PPR */
    ppad;
    op3 = opregaddr;
    pad64;
    op2 = *(op3);
    deref(op2);
    if (!isinteger(op2)) { arithmetic_abort1("'\\'", op2); }
    else { bld_int(op3, ~(int_val(op2))); }
    goto contcase; 

  case reset:  /* PPP */
    /*
     * This instruction is added just to provide a fall back point for
     * xsb_abort() or xsb_segfault_catcher().  It is called only once, and 
     * then control goes to startaddr.
     */
    if ((lpcreg = (byte *) setjmp(xsb_abort_fallback_environment))) {
      /*
       * Short circuit untrailing to avoid possible seg faults in
       * switch_envs.
       */
      trreg = cp_trreg(breg);
      /* Restore the default signal handling */
      signal(SIGSEGV, xsb_default_segfault_handler);
    }
    else lpcreg = startaddr;  /* first instruction of entire engine */
    goto contcase;

  default: {
    char message[80];
    sprintf(message, "Illegal opcode hex %x", *--lpcreg); 
    xsb_exit(message);
  }
} /* end of switch */


/*======================================================================*/
/* unification routines							*/
/*======================================================================*/

#define IFTHEN_SUCCEED  goto contcase
#define IFTHEN_FAILED	{Fail1 ; goto contcase ;}

nunify: /* ( op1, op2 ) */
/* word op1, op2 */
#include "unify_xsb_i.h"

    goto contcase;  /* end of nunify */

/*======================================================================*/

subtryme:
{
  register CPtr cps_top;	/* cps_top only needed for efficiency */

  save_find_locx(ereg);		/* sets ebreg to the top of the E-stack	*/
  check_tcpstack_overflow;
  cps_top = top_of_cpstack;
  save_registers(cps_top, (Cell)op1, rreg);
  save_choicepoint(cps_top, ereg, (byte *)op2, breg);
  breg = cps_top;
  hbreg = hreg;
  goto contcase;
} /* end of subtryme */

/*----------------------------------------------------------------------*/

restore_sub:
{
  register CPtr tbreg;

  tbreg = breg;
  /*   switch_envs(tbreg); */
  undo_bindings(tbreg);
  ptcpreg = cp_ptcp(tbreg);
  delayreg = cp_pdreg(tbreg);
  restore_some_wamregs(tbreg, ereg);
  restore_registers(tbreg, (int)op1, rreg);
  if (restore_type == 1) { /* trust */
    breg = cp_prevbreg(breg); 
    restore_trail_condition_registers(breg);
  }
  goto contcase;
} /* end of restore_sub */

/*----------------------------------------------------------------------*/

table_restore_sub:
{
  register CPtr tbreg;

  tbreg = breg;
  switch_envs(tbreg);
  /* This CP should be used for the dependency graph */
  ptcpreg = tcp_subgoal_ptr(tbreg);
  delayreg = NULL;
  restore_some_wamregs(tbreg, ereg);
  table_restore_registers(tbreg, (int)op1, rreg);
  if (restore_type == 1) { 
    xtemp1 = tcp_prevbreg(breg); 
    restore_trail_condition_registers(xtemp1);
  }
  goto contcase;
} /* end of table_restore_sub */

/*----------------------------------------------------------------------*/

} /* end of emuloop() */

/*======================================================================*/
/*======================================================================*/

DllExport int call_conv xsb(int flag, int argc, char *argv[])
{ 
   char *startup_file;
   FILE *fd;
   unsigned int magic_num;
   static double realtime;	/* To retain its value across invocations */

   extern void dis(int);
   extern char *init_para(int, char **);
   extern void init_machine(void), init_symbols(void);
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
     if (executable[0] == '\0')
       xsb_executable_full_path(argv[0]);

     /* set install_dir, xsb_config_file and user_home */
     set_install_dir();
     set_config_file();
     set_user_home();

     realtime = real_time();
     setbuf(stdout, NULL);
     startup_file = init_para(argc, argv);	/* init parameters */
     init_machine();		/* init space, regs, stacks */
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

     return(emuloop(inst_begin));

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
