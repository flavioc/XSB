/* File:      biassert.c
** Author(s): David S. Warren, Jiyang Xu
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

/* Special debug includes */
#include "debugs/debug_biassert.h"

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <setjmp.h>
#include <stdlib.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "cinterf.h"
#include "memory_xsb.h"
#include "psc_xsb.h"
#include "heap_xsb.h"
#include "register.h"
#include "flags_xsb.h"
#include "inst_xsb.h"
#include "token_xsb.h"
#include "loader_xsb.h"
#include "trie_internals.h"
#include "choice.h"
#include "macro_xsb.h"
#include "tr_utils.h"
#include "trassert.h"

/* --- routines used from other files ---------------------------------	*/

extern Cell val_to_hash(Cell);

extern TIFptr first_tip;
extern TIFptr last_tip;

/*======================================================================*/
/* dbgen_inst: Generate an instruction in the buffer.			*/
/*======================================================================*/

#define write_word(Buff,Loc,w) { *(CPtr)((pb)Buff + *(Loc)) = (Cell)(w); *(Loc) += 4; \
				pad64bits(Loc); }
#define write_byte(Buff,Loc,w) { *(pb)((pb)Buff + *(Loc)) = (byte)(w); *(Loc) += 1; }

#ifdef BITS64
#define pad64bits(Loc)	{ *(Loc) += 4; }
#else
#define pad64bits(Loc)	{}
#endif

#ifdef ASSERTDEBUG
void dbgen_printinst3(Opcode, Arg1, Arg2, Arg3)
{
  switch (Opcode) {
  case getlist_tvar_tvar:
    printf("getlist_tvar_tvar - %ld %ld %ld\n",(long)Arg1,(long)Arg2,(long)Arg3); break;
  case switchonbound:
    printf("switchonbound - %ld %ld %ld\n",(long)Arg1,(long)Arg2,(long)Arg3); break;
  case switchon3bound:
    printf("switchon3bound - %ld %ld %ld\n",(long)Arg1,(long)Arg2,(long)Arg3); break;
  default: xsb_dbgmsg("Unknown instruction in assert %d",
		      Opcode);
  }
}

void dbgen_printinst(Opcode, Arg1, Arg2)
{
  switch (Opcode) {
  case getpvar:	/* PRV */
    printf("getpvar - %d %d\n", Arg1, Arg2); break;
  case getpval:	/* PRV */
    printf("getpval - %d %d\n", Arg1, Arg2); break;
  case putpvar:	/* PRV */
    printf("putpvar - %d %d\n", Arg1, Arg2); break;
  case putpval:	/* PRV */
    printf("putpval - %d %d\n", Arg1, Arg2); break;
  case gettval:	/* PRR */
    printf("gettval - %d %d\n", Arg1, Arg2); break;
  case puttvar:	/* PRR */
    printf("puttvar - %d %d\n", Arg1, Arg2); break;
  case movreg:	/* PRR */
    printf("movreg - %d %d\n", Arg1, Arg2); break;
  case unipvar:	/* PPV */
    printf("unipvar - - %d\n", Arg1); break;
  case unipval:	/* PPV */
    printf("unipval - - %d\n", Arg1); break;
  case bldpvar:	/* PPV */
    printf("bldpvar - - %d\n", Arg1); break;
  case bldpval:	/* PPV */
    printf("bldpval - - %d\n", Arg1); break;
  case unitvar:	/* PPR */
    printf("unitvar - - %d\n", Arg1); break;
  case unitval:	/* PPR */
    printf("unitval - - %d\n", Arg1); break;
  case bldtvar:	/* PPR */
    printf("bldtvar - - %d\n", Arg1); break;
  case bldtval:	/* PPR */
    printf("bldtval - - %d\n", Arg1); break;
  case putlist:	/* PPR */
    printf("putlist - - %d\n", Arg1); break;
  case getlist:	/* PPR */
    printf("getlist - - %d\n", Arg1); break;
  case putcon:
    printf("putcon - - %d 0x%x\n", Arg1, Arg2); break;
  case putnumcon:
    printf("putnumcon - - %d 0x%x\n", Arg1, Arg2); break;
  case putfloat:
    printf("putfloat - - %d %f (0x%x)\n", Arg1, Arg2, Arg2); break;
  case getcon:
    printf("getcon - - %d 0x%x\n", Arg1, Arg2); break;
  case getnumcon:
    printf("getnumcon - - %d 0x%x\n", Arg1, Arg2); break;
  case getfloat:
    printf("getfloat - - %d %f (0x%x)\n", Arg1, Arg2, Arg2); break;
  case putstr:
    printf("putstr - - %d 0x%x\n", Arg1, Arg2); break;
  case getstr:
    printf("getstr - - %d 0x%x\n", Arg1, Arg2); break;
  case putnil:
    printf("putnil - - %d\n", Arg1); break;
  case getnil:
    printf("getnil - - %d\n", Arg1); break;
  case bldcon:
    printf("bldcon - - - 0x%x\n", Arg1); break;
  case bldnumcon:
    printf("bldnumcon - - - 0x%x\n", Arg1); break;
  case bldfloat:
    printf("bldfloat - - - 0x%x\n", Arg1); break;
  case unicon:
    printf("unicon - - - 0x%x\n", Arg1); break;
  case uninumcon:
    printf("uninumcon - - - 0x%x\n", Arg1); break;
  case unifloat:
    printf("unifloat - - - 0x%x\n", Arg1); break;
  case execute:
    printf("execute - - - 0x%x\n", Arg1); break;
  case bldnil:
    printf("bldnil - - -\n"); break;
  case uninil:
    printf("uninil - - -\n"); break;
  case proceed:
    printf("proceed - - -\n"); break;
  case noop:
    printf("noop - - -\n"); break;
  case jumptbreg:
    printf("jumptbreg - - %d 0x%x\n", Arg1, Arg2); break;
  case test_heap:
    printf("test_heap - - %d %d\n", Arg1, Arg2); break;
  case dyntrustmeelsefail:
    printf("dyntrustmeelsefail - - %d 0x%x\n", Arg1, Arg2); break;
  case retrymeelse:
    printf("retrymeelse - - %d 0x%x\n", Arg1, Arg2); break;
  case trymeelse:
    printf("trymeelse - - %d 0x%x\n", Arg1, Arg2); break;
  case jump:
    printf("jump - - - 0x%x\n", Arg1); break;
  case fail:
    printf("fail - - -\n"); break;
  default: xsb_dbgmsg("Unknown instruction in assert %d",
		      Opcode);
  }
}

#define dbgen_printinst3_macro(Opcode, Arg1, Arg2, Arg3) \
	dbgen_printinst3(Opcode, Arg1, Arg2, Arg3)

#define dbgen_printinst_macro(Opcode, Arg1, Arg2) \
	dbgen_printinst(Opcode, Arg1, Arg2)

#else  /* ASSERTDEBUG */

#define dbgen_printinst3_macro(Opcode, Arg1, Arg2, Arg3)
#define dbgen_printinst_macro(Opcode, Arg1, Arg2)

#endif /* ASSERTDEBUG */


#define dbgen_inst3_tv(Opcode,Arg1,Arg2,Arg3,Buff,Loc) {	\
  dbgen_printinst3_macro(Opcode, Arg1, Arg2, Arg3);		\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,Arg1);	\
  write_byte(Buff,Loc,Arg2); write_byte(Buff,Loc,Arg3);		\
  pad64bits(Loc);						\
}

#define dbgen_inst3_sob(Opcode,Arg1,Arg2,Arg3,Buff,Loc) {	\
  dbgen_printinst3_macro(Opcode, Arg1, Arg2, Arg3);		\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,Arg1>>16);	\
  write_byte(Buff,Loc,Arg1>>8); write_byte(Buff,Loc,Arg1);	\
  pad64bits(Loc);						\
  write_word(Buff,Loc,Arg2); write_word(Buff,Loc,Arg3);		\
}

#define dbgen_inst_pvv(Opcode,Arg1,Arg2,Buff,Loc) {	\
  dbgen_printinst_macro(Opcode, Arg1, Arg2);		\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,0);	\
  write_byte(Buff,Loc,Arg1); write_byte(Buff,Loc,Arg2);	\
  pad64bits(Loc);					\
}

#define dbgen_inst_ppv(Opcode,Arg1,Buff,Loc) {		\
  dbgen_printinst_macro(Opcode, Arg1, 0);		\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,0);	\
  write_byte(Buff,Loc,0); write_byte(Buff,Loc,Arg1);	\
  pad64bits(Loc);					\
}

#define dbgen_inst_ppvw(Opcode,Arg1,Arg2,Buff,Loc) {	\
  dbgen_printinst_macro(Opcode, Arg1, Arg2);		\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,0);	\
  write_byte(Buff,Loc,0); write_byte(Buff,Loc,Arg1);	\
  pad64bits(Loc);					\
  write_word(Buff,Loc,Arg2);				\
}

#define dbgen_inst_ppvww(Opcode,Arg1,Arg2,Arg3,Buff,Loc) {	\
  dbgen_printinst_macro(Opcode, Arg1, Arg2);			\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,0);		\
  write_byte(Buff,Loc,0); write_byte(Buff,Loc,Arg1);		\
  pad64bits(Loc);						\
  write_word(Buff,Loc,Arg2);					\
  write_word(Buff,Loc,Arg3);					\
}

#define dbgen_inst_pppw(Opcode,Arg1,Buff,Loc) {		\
  dbgen_printinst_macro(Opcode, Arg1, 0);		\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,0);	\
  write_byte(Buff,Loc,0); write_byte(Buff,Loc,0);	\
  pad64bits(Loc);					\
  write_word(Buff,Loc,Arg1);				\
}

#define dbgen_inst_ppp(Opcode,Buff,Loc) {		\
  dbgen_printinst_macro(Opcode, 0, 0);			\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,0);	\
  write_byte(Buff,Loc,0); write_byte(Buff,Loc,0);	\
  pad64bits(Loc);					\
}

#define dbgen_instB3_tv(Opcode,Arg1,Arg2,Arg3) {		\
  dbgen_printinst3_macro(Opcode, Arg1, Arg2, Arg3);		\
  if (*Loc >= BLim) Buff = buff_realloc();			\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,Arg1);	\
  write_byte(Buff,Loc,Arg2); write_byte(Buff,Loc,Arg3);		\
  pad64bits(Loc);						\
}

#define dbgen_instB3_sob(Opcode,Arg1,Arg2,Arg3) {		\
  dbgen_printinst3_macro(Opcode, Arg1, Arg2, Arg3);		\
  if (*Loc >= BLim) Buff = buff_realloc();			\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,Arg1>>16);	\
  write_byte(Buff,Loc,Arg1>>8); write_byte(Buff,Loc,Arg1);	\
  pad64bits(Loc);						\
  write_word(Buff,Loc,Arg2); write_word(Buff,Loc,Arg3);		\
}

#define dbgen_instB_pvv(Opcode,Arg1,Arg2) {		\
  dbgen_printinst_macro(Opcode, Arg1, Arg2);		\
  if (*Loc >= BLim) Buff = buff_realloc();		\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,0);	\
  write_byte(Buff,Loc,Arg1); write_byte(Buff,Loc,Arg2);	\
  pad64bits(Loc);					\
}

#define dbgen_instB_ppv(Opcode,Arg1) {			\
  dbgen_printinst_macro(Opcode, Arg1,0);		\
  if (*Loc >= BLim) Buff = buff_realloc();		\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,0);	\
  write_byte(Buff,Loc,0); write_byte(Buff,Loc,Arg1);	\
  pad64bits(Loc);					\
}

#define dbgen_instB_ppvw(Opcode,Arg1,Arg2) {		\
  dbgen_printinst_macro(Opcode, Arg1, Arg2);		\
  if (*Loc >= BLim) Buff = buff_realloc();		\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,0);	\
  write_byte(Buff,Loc,0); write_byte(Buff,Loc,Arg1);	\
  pad64bits(Loc);					\
  write_word(Buff,Loc,Arg2);				\
}

#define dbgen_instB_pppw(Opcode,Arg1) {			\
  dbgen_printinst_macro(Opcode, Arg1, 0);		\
  if (*Loc >= BLim) Buff = buff_realloc();		\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,0);	\
  write_byte(Buff,Loc,0); write_byte(Buff,Loc,0);	\
  pad64bits(Loc);					\
  write_word(Buff,Loc,Arg1);				\
}

#define dbgen_instB_ppp(Opcode) {			\
  dbgen_printinst_macro(Opcode,0,0);			\
  if (*Loc >= BLim) Buff = buff_realloc();		\
  write_byte(Buff,Loc,Opcode); write_byte(Buff,Loc,0);	\
  write_byte(Buff,Loc,0); write_byte(Buff,Loc,0);	\
  pad64bits(Loc);					\
}


/*======================================================================*/
/* db_cmpl(+Clause, +Buffer, +Index, -Size)                             */
/*      Clause is a fact or rule.                                       */
/*      Buffer is the buffer where the code is put.                     */
/*      Index is the argument to index on (0 if none).                  */
/*      Size is the size of the compiled code                           */
/* The predicate will generate code for the given clause in the Buffer. */
/* The first 8 bytes are reserved for general chain.  If index is       */
/* requested, the 2nd 8 bytes are used for the buckete chain. See Code  */
/* below.                                                               */
/*======================================================================*/


static jmp_buf assertcmp_env;

struct flatten_elt {
	union {
		prolog_term term;
		Cell opcode;
	} v;
	int reg;
};

struct instruction {
	Cell opcode;
	Cell arg1;
	Cell arg2;
};

#define INST_QUEUE_SIZE	16384	/* was 1024 (which was too low) */
#define FLATTEN_STACK_SIZE 512	

static struct flatten_elt flatten_stack[FLATTEN_STACK_SIZE];
static struct instruction inst_queue[INST_QUEUE_SIZE];
static int flatten_stack_top;
static int inst_queue_top;
static int inst_queue_bottom;

static void assertcmp_throw(int num)
{
    longjmp(assertcmp_env, num);
}


#define ERR_FUNCTOR	1
#define ERR_REGISTER	2

static int arity(prolog_term T0)
{
  if (is_functor(T0)) return p2c_arity(T0);
  else if (is_list(T0)) return 2;
  else if (is_string(T0)) return 0;
  else assertcmp_throw(ERR_FUNCTOR);
  return -1;
}

static void assertcmp_printerror(int num)
{
    switch (num) {
    case ERR_FUNCTOR:
	xsb_abort("Assert: functor expected");
	break;
    case ERR_REGISTER:
	xsb_abort("Assert: need too many registers");
	break;
    default: 
	xsb_abort("Assert: error occured in assert_cmp");
    }
}

/* db_cmpl(Clause, Buff, Index, Size) */

static Integer p2c_float_as_int(prolog_term T0)
{
    union float_conv {
	Float f;
	Integer i;
    } float_conv;
    float_conv.f = float_val(T0);
    return float_conv.i;
}

static int is_frozen_var(prolog_term T0)
{
    if (is_functor(T0) && strcmp(p2c_functor(T0), "$assertVAR")==0 &&
	p2c_arity(T0) == 1) {
	T0 = p2p_arg(T0, 1);
	return int_val(T0);
    } else return 0;
}

static void flatten_stack_init(struct flatten_elt *flatten_stack)
{
    flatten_stack_top = 0;
}

static int flatten_stack_size(struct flatten_elt *flatten_stack)
{
    return flatten_stack_top;
}

static void flatten_stack_push(struct flatten_elt *flatten_stack,
			       int argno, Cell term)
{
    flatten_stack[flatten_stack_top].reg = argno;
    flatten_stack[flatten_stack_top].v.opcode = term;
    flatten_stack_top++;
    if (flatten_stack_top >= FLATTEN_STACK_SIZE)
      xsb_abort("flatten_stack overflow in assert");
}

static void flatten_stack_pop(struct flatten_elt *flatten_stack,
			      int *argnop, Cell *termp)
{
    flatten_stack_top--;
    *argnop = flatten_stack[flatten_stack_top].reg;
    *termp = flatten_stack[flatten_stack_top].v.opcode;
}

static void inst_queue_init(struct instruction *inst_queue)
{
    inst_queue_top = 0;
    inst_queue_bottom = 0;
}

static int inst_queue_empty(struct instruction *inst_queue)
{
    return (inst_queue_top == inst_queue_bottom);
}

static void inst_queue_push(struct instruction *inst_queue,
			    Cell opcode, Cell arg1, Cell arg2)
{
    inst_queue[inst_queue_top].opcode = opcode;
    inst_queue[inst_queue_top].arg1 = arg1;
    inst_queue[inst_queue_top].arg2 = arg2;
    inst_queue_top++;
    if (inst_queue_top >= INST_QUEUE_SIZE)
      xsb_abort("instruction queue overflow in assert");
}

static void inst_queue_pop(struct instruction *inst_queue,
			   Cell *opcodep, Cell *arg1p, Cell *arg2p)
{
    inst_queue_top--;
    *opcodep = inst_queue[inst_queue_top].opcode;
    *arg1p = inst_queue[inst_queue_top].arg1;
    *arg2p = inst_queue[inst_queue_top].arg2;
}

static void inst_queue_rem(struct instruction *inst_queue,
			   Cell *opcodep, Cell *arg1p, Cell *arg2p)
{
    *opcodep = inst_queue[inst_queue_bottom].opcode;
    *arg1p = inst_queue[inst_queue_bottom].arg1;
    *arg2p = inst_queue[inst_queue_bottom].arg2;
    inst_queue_bottom++;
}

typedef int *RegStat;
	/* 0 - all rest registers are free */
	/* >0 - next free register */
	/* -1 used for real var */
	/* -2 used for introduced var */

#define RVAR -1
#define TVAR -2

static int RegArray[MAX_REGS];
static int RegArrayInit[MAX_REGS];
static int FreeReg;

static RegStat reg_init(int Size)
{
    int i;

    FreeReg = Size+1;
    for (i=0; i<FreeReg; i++) RegArray[i] = RVAR;
    RegArray[FreeReg] = 0;
    return RegArray;
}

/* Type: RVAR=-1 - used for real var; TVAR=-2 - used for introduced var */
static int reg_get(RegStat Reg, int Type)
{
    int new_reg;

    new_reg = FreeReg;
    if (RegArray[FreeReg]==0) {
	FreeReg++;
	if (FreeReg >= MAX_REGS) {
	  assertcmp_throw(ERR_REGISTER);
	}
	RegArray[FreeReg] = 0;
    } else FreeReg = RegArray[FreeReg];
    RegArray[new_reg] = Type;
    RegArrayInit[new_reg] = 0;	/* register is not initialized */
    return new_reg;
}

static void reg_release(int R0)
{
    if (RegArray[R0]==TVAR) {
	RegArray[R0] = FreeReg;
	FreeReg = R0;
    }
}

#ifndef max
#define max(p1,p2) ((p1)>=(p2)?(p1):(p2))
#endif
#ifndef min
#define min(p1,p2) ((p1)<=(p2)?(p1):(p2))
#endif

static char *Buff = NULL;
static int Buff_size = 512;
static int *Loc;
static int BLim = 0;
static int Size;

static char *buff_realloc(void)
{
  /*  xsb_dbgmsg("Enter buff_realloc(%d) %X", Buff_size,Buff); */
  Buff_size = Buff_size + Buff_size;
  if (Buff == NULL) Buff = malloc(Buff_size);
  else Buff = realloc(Buff,Buff_size);
  BLim = Buff_size-16;
  /*  xsb_dbgmsg("Leave buff_realloc(%d) %X", Buff_size,Buff); */
  return(Buff);
}

/*----------------------------------------------------------------------*/
/*  Function prototypes.						*/
/*----------------------------------------------------------------------*/

static void db_putterm(int, prolog_term, RegStat);
static void db_genmvs(struct instruction *, RegStat);
static void db_gentopinst(prolog_term, int, RegStat);
static void db_genterms(struct instruction *, RegStat);
static void db_geninst(prolog_term, RegStat, struct instruction *);
static void db_bldsubs(prolog_term, RegStat, struct flatten_elt *);
static void db_genaput(prolog_term, int, struct instruction *, RegStat);

/*======================================================================*/
/*  The following code compiles a clause into a local buffer.  It	*/
/*  treats all rules as though they had a single literal on their	*/
/*  right-hand-side.  Thus it compiles a clause with more than one 	*/
/*  literal on the right-hand-side as a call to the predicate ,/2.	*/
/*======================================================================*/

int assert_code_to_buff(/* Clause */)
{
  prolog_term Clause;
  prolog_term Head, Body;
  int Location;
  int Loc_size;
  RegStat Reg;
  int Arity;
  int has_body;
  int Argno;
  int v;
  Pair sym;

  Clause = reg_term(1);
  /* set catcher */
  if ((Argno = setjmp(assertcmp_env))) {
    assertcmp_printerror(Argno);
    return FALSE;
  }
  if (isconstr(Clause) && strcmp(p2c_functor(Clause),":-")==0 &&
      get_arity(get_str_psc(Clause))==2) {
    Head = p2p_arg(Clause, 1);
    Body = p2p_arg(Clause, 2);
    if (isstring(Body)) {
      sym = insert(string_val(Body),0,(Psc)flags[CURRENT_MODULE],&v);
      Body = makecs(hreg);
      new_heap_functor(hreg,sym->psc_ptr);
    }
    has_body = 1;
  } else {
    Head = Clause;
    Body = (prolog_term) NULL;
    has_body = 0;
  }
  Arity = arity(Head);
  Location = 0;
  Loc = &Location;
  dbgen_instB_ppvw(test_heap,Arity,0);  /* size will be backpatched */
  Loc_size = *Loc - sizeof(Cell);
  if (has_body) Reg = reg_init(max(Arity,(int)get_arity(get_str_psc(Body))));
  else Reg = reg_init(Arity);
  for (Argno = 1; Argno <= Arity; Argno++) {
    db_gentopinst(p2p_arg(Head,Argno),Argno,Reg);
  }
  if (has_body) {
    inst_queue_init(inst_queue);
    for (Argno=1; Argno<=arity(Body); Argno++) {
      db_genaput(p2p_arg(Body,Argno),Argno,inst_queue,Reg);
    }
    db_genmvs(inst_queue,Reg);
    dbgen_instB_pppw(execute, get_str_psc(Body));
  } else dbgen_instB_ppp(proceed);
  Size = *Loc;
  write_word(Buff,&Loc_size,(Size/sizeof(Cell)));  /* backpatch max heap needed*/
  return TRUE;
}

static void db_gentopinst(prolog_term T0, int Argno, RegStat Reg)
{
  int Rt;
  
  if (is_int(T0)) {
    dbgen_instB_ppvw(getnumcon, Argno, int_val(T0)); /* getnumcon */
  } else if (is_string(T0)) {
    dbgen_instB_ppvw(getcon, Argno, (Cell)string_val(T0));  /* getcon */
  } else if (is_float(T0)) {
    dbgen_instB_ppvw(getfloat, Argno, p2c_float_as_int(T0)); /* getfloat */
  } else if (is_var(T0)) {
    c2p_functor("$assertVAR", 1, T0);
    T0 = p2p_arg(T0, 1);
    c2p_int(Argno, T0);
    RegArrayInit[Argno] = 1;	/* Reg is initted */
  } else if (is_nil(T0)) {
    dbgen_instB_ppv(getnil, Argno);	/* getnil */
  } else if ((Rt = is_frozen_var(T0))) {
    dbgen_instB_pvv(gettval, Rt, Argno);	/* gettval */
  } else {
    inst_queue_init(inst_queue);
    inst_queue_push(inst_queue, Argno, T0, 0);
    db_genterms(inst_queue, Reg);
  }
}

static void db_genterms(struct instruction *inst_queue,
			RegStat Reg)
{
  prolog_term T0, T1, T2;
  Cell Argno;
  
  while (!inst_queue_empty(inst_queue)) {
    inst_queue_pop(inst_queue, &Argno, &T0, &T1);
    RegArrayInit[Argno] = 1;	/* Reg is initted */
    if (is_list(T0)) {
      T1 = p2p_car(T0);
      T2 = p2p_cdr(T0);
      if (is_var(T1) && is_var(T2) && T1!=T2 /* not same var */) {
	int Rt1, Rt2;
	c2p_functor("$assertVAR", 1, T1);
	T1 = p2p_arg(T1, 1);
	Rt1 = reg_get(Reg, RVAR);
	c2p_int(Rt1, T1);
	c2p_functor("$assertVAR", 1, T2);
	T2 = p2p_arg(T2, 1);
	Rt2 = reg_get(Reg, RVAR);
	c2p_int(Rt2, T2);
	dbgen_instB3_tv(getlist_tvar_tvar, Argno, Rt1, Rt2);
	RegArrayInit[Rt1] = 1;	/* Reg is initted */
	RegArrayInit[Rt2] = 1;	/* Reg is initted */
	reg_release(Argno);
      } else {
	dbgen_instB_ppv(getlist, Argno);    /* getlist */
	reg_release(Argno);
	db_geninst(p2p_car(T0), Reg, inst_queue);
	db_geninst(p2p_cdr(T0), Reg, inst_queue);
      }
    } else {
      dbgen_instB_ppvw(getstr, Argno, get_str_psc(T0));   /* getstr */
      reg_release(Argno);
      for (Argno=1; Argno <= (int)get_arity(get_str_psc(T0)); Argno++) {
	db_geninst(p2p_arg(T0,Argno), Reg, inst_queue);
      }
    }
  }
}

static void db_geninst(prolog_term Sub, RegStat Reg,
		       struct instruction *inst_queue)
{
  int Rt;
  
  if (is_int(Sub)) {
    dbgen_instB_pppw(uninumcon, int_val(Sub));
  } else if (is_string(Sub)) {
    dbgen_instB_pppw(unicon, (Cell)p2c_string(Sub));
  } else if (is_nil(Sub)) {
    dbgen_instB_ppp(uninil);
  } else if (is_float(Sub)) {
    dbgen_instB_pppw(unifloat, p2c_float_as_int(Sub));
  } else if (is_var(Sub)) {
    c2p_functor("$assertVAR", 1, Sub);
    Sub = p2p_arg(Sub, 1);
    Rt = reg_get(Reg, RVAR);
    c2p_int(Rt, Sub);
    dbgen_instB_ppv(unitvar, Rt);
    RegArrayInit[Rt] = 1;  /* reg is inited */
  } else if ((Rt = is_frozen_var(Sub))) {
    dbgen_instB_ppv(unitval, Rt);
  } else {
    Rt = reg_get(Reg, TVAR);
    dbgen_instB_ppv(unitvar, Rt);
    RegArrayInit[Rt] = 1;  /* reg is inited */
    inst_queue_push(inst_queue, Rt, Sub, 0);
  }
}

static void db_genaput(prolog_term T0, int Argno,
		       struct instruction *inst_queue,
		       RegStat Reg)
{
  int Rt;

  if (is_var(T0)) {
    c2p_functor("$assertVAR", 1, T0);
    T0 = p2p_arg(T0, 1);
    Rt = reg_get(Reg, RVAR);
    c2p_int(Rt, T0);  /* used to be TempVar???? */
    dbgen_instB_pvv(puttvar, Rt, Rt);
    RegArrayInit[Rt] = 1;  /* reg is inited */
    inst_queue_push(inst_queue, movreg, Rt, Argno);
  } else if ((Rt = is_frozen_var(T0))) {
    inst_queue_push(inst_queue, movreg, Rt, Argno);
  } else if (is_int(T0)) {
    inst_queue_push(inst_queue, putnumcon, int_val(T0), Argno);
  } else if (is_float(T0)) {
    inst_queue_push(inst_queue, putnumcon, p2c_float_as_int(T0), Argno);
  } else if (is_nil(T0)) {
    inst_queue_push(inst_queue, putnil, 0, Argno);
  } else if (is_string(T0)) {
    inst_queue_push(inst_queue, putcon, (Cell)p2c_string(T0), Argno);
  } else {  /* structure */
    Rt = reg_get(Reg, TVAR);
    inst_queue_push(inst_queue, movreg, Rt, Argno);
    flatten_stack_init(flatten_stack);
    db_putterm(Rt,T0,Reg);
  }
}

static void db_putterm(int Rt, prolog_term T0,
		       RegStat Reg)
{
  int Argno;
  int BldOpcode;
  Cell Arg1;
  int stack_size;
  
  stack_size = flatten_stack_size(flatten_stack);
  if (is_list(T0)) {
    db_bldsubs(p2p_cdr(T0),Reg,flatten_stack);
    db_bldsubs(p2p_car(T0),Reg,flatten_stack);
    dbgen_instB_ppv(putlist, Rt);			/* putlist */
  } else { /* structure */
    for (Argno=get_arity(get_str_psc(T0)); Argno>=1; Argno--)
      db_bldsubs(p2p_arg(T0,Argno),Reg,flatten_stack);
    dbgen_instB_ppvw(putstr, Rt, get_str_psc(T0));	/* putstr */
  }
  RegArrayInit[Rt] = 1;	/* in either case, reg is inited */
  while (flatten_stack_size(flatten_stack)>stack_size) {
    flatten_stack_pop(flatten_stack, &BldOpcode, &Arg1);	
    /* be careful about order!!*/
    switch (BldOpcode) {
    case bldpvar:
      if (RegArrayInit[Arg1]) {
	dbgen_instB_ppv(bldpval, Arg1); break;
      } else {
	RegArrayInit[Arg1] = 1;
	dbgen_instB_ppv(bldpvar, Arg1); break;
      }
    case bldtvar:
      if (RegArrayInit[Arg1]) {
	dbgen_instB_ppv(bldtval, Arg1);
      } else {
	RegArrayInit[Arg1] = 1;
	dbgen_instB_ppv(bldtvar, Arg1);
      }
      break;
    case bldcon:
      dbgen_instB_pppw(bldcon, Arg1); break;
    case bldnumcon:
      dbgen_instB_pppw(bldnumcon, Arg1); break;
    case bldfloat:
      dbgen_instB_pppw(bldfloat, Arg1); break;
    case bldnil:
      dbgen_instB_ppp(bldnil); break;
    default: xsb_dbgmsg("Incorrect bld instruction in assert %d", 
			BldOpcode);
    }
  }
}

static void db_bldsubs(prolog_term Sub, RegStat Reg,
		       struct flatten_elt *flatten_stack)
{
  int Rt;
  
  if (is_string(Sub)) {
    flatten_stack_push(flatten_stack,bldcon,(Cell)string_val(Sub)); /* bldcon */
  } else if (is_int(Sub)) {               /* bldnumcon(Sub) */
    flatten_stack_push(flatten_stack, bldnumcon, int_val(Sub));
  } else if (is_float(Sub)) {             /* bldfloat(Sub) */
    flatten_stack_push(flatten_stack, bldfloat, p2c_float_as_int(Sub));
  } else if (is_var(Sub)) {
    c2p_functor("$assertVAR", 1, Sub);
    Sub = p2p_arg(Sub, 1);
    Rt = reg_get(Reg, RVAR);
    c2p_int(Rt, Sub);
    flatten_stack_push(flatten_stack, bldtvar, Rt);    /* bldtvar(Ri) */
  } else if (is_nil(Sub)) {
    flatten_stack_push(flatten_stack, bldnil, 0);      /* bldnil */
  } else if ((Rt = is_frozen_var(Sub))) {
    flatten_stack_push(flatten_stack, bldtvar, Rt);
  } else {
    Rt = reg_get(Reg, TVAR);
    flatten_stack_push(flatten_stack, bldtvar, Rt);
    db_putterm(Rt,Sub,Reg);
  }
}

static bool target_is_not_source(int Reg)
{
  int i;
  
  for (i=inst_queue_bottom; i<inst_queue_top; i++) {
    if (inst_queue[i].opcode==movreg && inst_queue[i].arg1 == Reg)
      return FALSE;
  }
  return TRUE;
}

static bool source_is_not_target(int Reg)
{
  int i;
  
  for (i=inst_queue_bottom; i<inst_queue_top; i++) {
    if (inst_queue[i].arg2 == Reg) return FALSE;
  }
  return TRUE;
}

/* this is a simple routine to generate a series of instructions to
   load a series of registers with constants or from other registers.
   It is given a list of Source,Target pairs.  Target is always a
   register  number.  Source may be a putcon(con), putnumcon(num),
   puttvar(reg), puttvar(Var), or movreg(reg).  The registers  can
   overlap in any way.  db_genmvs tries to generate a reasonably efficient
   series of instructions to load the indicated registers with the
   indicated values.  */ 

static void db_genmvs(struct instruction *inst_queue, RegStat Reg)
{
  Cell Opcode, Arg, T0, R0;
  
  /* pay attention to the ordering, must be a QUEUE !!!!! */
  while (!inst_queue_empty(inst_queue)) {
    inst_queue_rem(inst_queue, &Opcode, &Arg, &T0);	/* T0: target reg */
    switch (Opcode) {
    case puttvar:  
      dbgen_instB_pvv(Opcode, Arg, T0);
      break;
    case putnil:
      if (target_is_not_source(T0))
	{dbgen_instB_ppv(Opcode, T0);}
      else inst_queue_push(inst_queue, Opcode, Arg, T0);
      break;
    case putcon:
    case putnumcon:
      if (target_is_not_source(T0))
	{dbgen_instB_ppvw(Opcode, T0, Arg);}
      else inst_queue_push(inst_queue, Opcode, Arg, T0);
      break;
    case movreg:
      if (Arg==T0) break;
      else if (target_is_not_source(T0)) {
	dbgen_instB_pvv(movreg, Arg, T0); /* movreg */
	reg_release(Arg);
      } else if (source_is_not_target(Arg)) /* assume target is source */
	inst_queue_push(inst_queue, movreg, Arg, T0);
      /* delay the instruction at the end */
      /* else if (Arg>T0) dbgen_instB_pvv(movreg,Arg,T0); movreg */
      else {
	R0 = reg_get(Reg, TVAR);
	dbgen_instB_pvv(movreg, Arg, R0); /* movreg */
	reg_release(Arg);
	inst_queue_push(inst_queue, movreg, R0, T0);
	/* dbgen_instB_pvv(movreg, R0, T0); */ /* movreg */
      }
      break;
    }
  }
}

/*======================================================================*/
/*	The following byte offsets are valid for 32 bit architectures	*/
/*	For 64 bit architecture multiply everithing by 2		*/
/*======================================================================*/

/*======================================================================*/
/* assert_buff_to_clref(+Arg,+Arity,+Prref,+AZ,+Index,+HashTabSize)	*/
/*	allocates a Clref, copies the byte-code for the clause from	*/
/*	an internal buffer into it, and adds to to the chains.		*/
/*	The arguments are:						*/
/*	Arg:   The argument value of the indexed arg (ignored if no ind)*/
/*	Arity: the number of registers to save in a choice point.	*/
/*		Note the Arity is one more than the original arity, to  */
/*		hold the cut address.					*/
/*	Prref: predicate reference to which to add the asserted fact	*/
/*	AZ:   0 - inserted as the first clause; 1 - as the last clause	*/
/*	Index:  0 if no index is to be built, or n if an index		*/
/*		on the nth argument of the fact is to be used		*/
/*	HashTabSize:  The size of the hash table to create if one must	*/
/*		be created for this clause (the SOB record)		*/
/*======================================================================*/

/*======================================================================*/
/* Formats for dynamic code:						*/
/* PSC rec point to:							*/
/*	PrRef:								*/
/*		0: BC instruction: fail (if empty),			*/
/*			jump and save breg (if nonempty)		*/
/*		4: Addr of first Clref on ALL chain			*/
/*		8: Addr of last Clref on ALL chain			*/
/*									*/
/* PrRef's point to chain of clRef's (one of 3 types):			*/
/* (the -8 location stores length of buff + flag indicating ClRef type	*/
/*	ClRef2 (for compiled code):					*/
/*		-8: length of buffer (+2)				*/
/*		-4: Addr of previous ClRef (or PrRef)			*/
/*		0: Try-type instruction, for chain			*/
/*		4: (cont) Addr of next ClRef on chain			*/
/*		8: jump							*/
/*		12: Addr of compiled code				*/
/*	ClRef0 (for unindexed asserted code):				*/
/*		-8: length of buffer (+0)				*/
/*		-4: Addr of previous ClRef (or PrRef)			*/
/*		0: Try-type instruction, for chain			*/
/*		4: (cont) Addr of next ClRef on chain			*/
/*		8+: BC for asserted clause				*/
/*	ClRef1 (for group of indexed clauses, aka SOB record):		*/
/*		-8: length of buffer (+1)				*/
/*		-4: Addr of previous ClRef (or PrRef)			*/
/*		0: Try-type instruction, for chain			*/
/*		4: (cont) Addr of next ClRef on chain			*/
/*		8: BC switch-on-bound instruction (drop thru if var)	*/
/*		11: (cont) arg to index on				*/
/*		12: (cont) address of Hash Table			*/
/*		16: (cont) size of Hash Table				*/
/*		20: BC jump to	(or fail if empty)			*/
/*		24: (cont) Addr of first ClRefI on all subchain		*/
/*		    or to ClRef1 for next index				*/
/*		28: Addr of last ClRefI on all subchain			*/
/*              32: Number of clauses accessible thru this hash table   */
/*		36+: Hash Table						*/
/*									*/
/* ClRef1's point to indexed clauses, each represented by a ClRefI:	*/
/*	ClRefI (for an indexed clause):					*/
/*		-8: length of buffer (+3)				*/
/*		-4: Addr of previous ClRefI on all chain		*/
/*		0: Try-type instruction, for all subchain		*/
/*		4: (cont) Addr of next ClRefI on all subchain		*/
/*            For each index we have the following four fields:         */
/*		8: BC noop(14) to skip next NI*8-2 bytes		*/
/*		12: Addr of previous ClRefI on bucket chain		*/
/*		16: Try-type instruction, for hash bucket subchain	*/
/*		20: (cont) Addr of next ClRefI in bucket		*/
/*		24: BC noop(6) to skip next (NI-1)*8-2 bytes		*/
/*		28: Addr of previous ClRefI on bucket chain		*/
/*		32: Try-type instruction, for hash bucket subchain	*/
/*		34: (cont) Addr of next ClRefI in bucket		*/
/*	   NI*16+8: BC for asserted code				*/
/*									*/
/*======================================================================*/

/* Predicate References */

typedef struct PrRef
{	Cell	Instr ;
	struct ClRefHdr *FirstClRef ;
	struct ClRefHdr *LastClRef ;
}	*PrRef, PrRefData ;

#define PredOpCode(P)		(cell_opcode(&(P)->Instr))

/* Clause References */

typedef struct ClRefHdr
{	unsigned long buflen ;
	struct ClRefHdr *prev ;
/*	Cell Data[];	*/
}	*ClRef, ClRefData, ClRefHdr ;

typedef ClRef SOBRef ;

#define ClRefAddr(Cl)		((CPtr)((ClRef)(Cl)-1))
#define ClRefSize(Cl)		(((ClRef)(Cl))[-1].buflen & ~0x3)
#define ClRefType(Cl)		(((ClRef)(Cl))[-1].buflen & 0x3)
#define SetClRefSize(Cl,len)	(((ClRef)(Cl))[-1].buflen |= \
		(((ClRef)(Cl))[-1].buflen & 0x3) | ((len) & ~0x3))
#define SetClRefType(Cl,type)	(((ClRef)(Cl))[-1].buflen = \
		(((ClRef)(Cl))[-1].buflen & ~0x3) | ((type) & 0x3))
#define ClRefPrev(Cl)		(((ClRef)(Cl))[-1].prev)
#define ClRefWord(Cl,pos)	(((CPtr)(Cl))[(pos)])

#define SetClRefPrev(Cl,Prv)	(((ClRef)(Cl))[-1].prev = (ClRef)(Prv))

/* Clause types */

#define UNINDEXED_CL	0
#define SOB_RECORD	1
#define COMPILED_CL	2
#define INDEXED_CL	3

#define MakeClRef(ptr,Type,NCells)\
{	long sz = (((NCells)*sizeof(Cell)+sizeof(ClRefHdr) + 7) & ~0x7);\
	(ptr) = (ClRef)mem_alloc(sz);\
	(ptr)->buflen = ((Type)&3)+(sz&~3);\
	(ptr)++;\
}

/* Clause common fields */

#define ClRefTryInstr(Cl)	(ClRefWord((Cl),0))
#define ClRefTryOpCode(Cl)	(cell_opcode(&ClRefTryInstr(Cl)))
#define ClRefNext(Cl)		((ClRef)ClRefWord((Cl),1))


#define SetClRefNext(Cl,Nxt)	(ClRefWord((Cl),1)=(Cell)(Nxt))

/* First byte code in clause at word 2  - Jump/SOB/etc */
#define ClRefEntryPoint(Cl)	(&ClRefWord((Cl),2))

/* For compiled clause */
#define ClRefCompiledCode(Cl)	(ClRefWord((Cl),3))

/* For indexed clause group (SOBblock) */

#define ClRefSOBInstr(Cl)	(ClRefWord((Cl),2))
#define ClRefHashSize(Cl)	(ClRefWord((Cl),4))
#define ClRefJumpInstr(Cl)	(ClRefWord((Cl),5))
#define ClRefFirstIndex(Cl)	(ClRefWord((Cl),6))
#define ClRefLastIndex(Cl)	(ClRefWord((Cl),7))
#define ClRefNumClauses(Cl)	(ClRefWord((Cl),8))
#define ClRefHashTable(Cl)	(&ClRefWord((Cl),9))
#define ClRefHashBucket(Cl,b)	((CPtr)(ClRefHashTable(Cl)[(b)]))

#define ClRefSOBArg(Cl,n)	(cell_operandn(&ClRefWord((Cl),2),(n)))

/* Get the PrRef field of a SOB */
#define ClRefPrRef(Cl)		((PrRef)&ClRefWord((Cl),5))

/* Get the ClRef containing the PrRef */
#define PrRefClRef(Pr)		((ClRef)((CPtr)(Pr)-5))

#define ClRefUpSOB(Cl)		(PrRefClRef(ClRefNext(Cl)))

/* For Indexed clause index table */

#define ClRefNumInds(Cl)\
	 ( (cell_operand3(&ClRefWord((Cl),2))/(sizeof(Cell)/2) + 1)/4 )
#define ClRefIndPtr(Cl,Ind)	(&ClRefWord((Cl),(Ind)*4))

#define IndPtrClRef(IP,Ind)	((ClRef)((CPtr)(IP)-(Ind)*4))
#define IndRefNoop(IndPtr)	((IndPtr)[-2])
#define IndRefPrev(IndPtr)	(((CPtr *)(IndPtr))[-1])
#define IndRefTryInstr(IndPtr)	((IndPtr)[0])
#define IndRefNext(IndPtr)	(((CPtr *)(IndPtr))[1])

#define IC_CELLS(NI)		(4*(NI)+2)

#define ClRefIEntryPoint(Cl,NI)	(&ClRefWord((Cl),IC_CELLS(NI)))

/* First word of code in an (un)indexed clause */
#define ClRefEntryAny(Cl) 						\
	((CPtr)								\
		((ClRefType(Cl)&1) == UNINDEXED_CL ? ClRefEntryPoint(Cl) :\
				ClRefIEntryPoint((Cl),ClRefNumInds(Cl))	\
	))

#define ClRefNotRetracted(Cl) (cell_opcode(ClRefEntryAny(Cl))!=fail)

static void db_addbuff(byte, ClRef, PrRef, int, int); 
static void db_addbuff_i(int, ClRef, PrRef, int, int *, int, prolog_term, int);


/* Used by assert & retract to get through the SOBs */

static int Index[20], NI ;

static void get_indexes( prolog_term prolog_ind )
{
  if (is_int(prolog_ind)) {
    Index[1] = int_val(prolog_ind);
    if (Index[1] == 0) NI = 0; else NI = 1;
  } else {
    for (NI = 0; !is_nil(prolog_ind); prolog_ind = p2p_cdr(prolog_ind)) {
      NI++;
      Index[NI] = int_val(p2p_car(prolog_ind));
    }
  }
}

bool assert_buff_to_clref(/*Head,Arity,Prref,AZ,Indexes,HashTabSize,Clref*/)
{
  byte Arity;
  int AZ, HashTabSize;
  ClRef Clause;
  PrRef Pred ;
  int Location, *Loc, Inum;
  prolog_term Head ;

  Head = reg_term(1);
  Arity = ptoc_int(2);
  Pred = (PrRef)ptoc_int(3);
  AZ = ptoc_int(4);
  get_indexes( reg_term(5) ) ;
  HashTabSize = ptoc_int(6);

#ifdef ASSERTDEBUG
  printf("Now add clref to chain:\n");
#endif /* ASSERTDEBUG */

  MakeClRef( Clause,
	     (NI>0) ? INDEXED_CL : UNINDEXED_CL,
	     IC_CELLS(NI) + ((Size+0xf)&~0x7)/sizeof(Cell) ) ;

  Location = 0; Loc = &Location;
  dbgen_inst_ppv(noop,sizeof(Cell)/2,Clause,Loc);    /* will become try */
  write_word(Clause,Loc,0);
  for (Inum = NI; Inum > 0; Inum--) {
    /* put template code for chaining buffers from hash tables  */
    dbgen_inst_ppv(noop,(4*Inum-1)*sizeof(Cell)/2,Clause,Loc);  /* noop(6) */
    write_word(Clause,Loc,0);
    dbgen_inst_ppv(noop,sizeof(Cell)/2,Clause,Loc);             /* noop(2) */
    write_word(Clause,Loc,0);
  }

/* Buff is a global variable used to communicate from assert_code_to_buff
   to assert_buff_to_clref through PROLOG calls */

  memmove(((pb)Clause)+Location,Buff,Size); /* fill in clause with code from Buff */
  /* ctop_int(7, (Integer)Clause);  DO NOT RETURN ANYTHING */
  
  if (NI <= 0) db_addbuff(Arity,Clause,Pred,AZ,1);
  else db_addbuff_i(Arity,Clause,Pred,AZ,Index,NI,Head,HashTabSize);
  return TRUE;
}

/* add NewClause to beginning of try-retry chain beginning with FirstClause */
static void prefix_to_chain(byte Arity, ClRef FirstClause, ClRef NewClause)
{
  int Loc = 0;
  
  if (ClRefTryOpCode(FirstClause) == noop)
  {  dbgen_inst_ppvw(dyntrustmeelsefail,Arity,ClRefNext(FirstClause),
		     FirstClause,&Loc); }
  else if (ClRefTryOpCode(FirstClause) == trymeelse)
  {  dbgen_inst_ppvw(retrymeelse,Arity,ClRefNext(FirstClause),
		     FirstClause,&Loc);}
  else xsb_dbgmsg("***Error 1 in assert: 0x%x",
		  ClRefTryOpCode(FirstClause));

  ClRefPrev(NewClause)   = ClRefPrev(FirstClause);
  ClRefPrev(FirstClause) = NewClause;

  Loc = 0;
  dbgen_inst_ppvw(trymeelse,Arity,FirstClause,NewClause,&Loc);
}

/* add NewClause after LastClause on try-retry chain */
static void append_to_chain(byte Arity, ClRef LastClause, ClRef NewClause)
{
  int Loc = 0;
  dbgen_inst_ppvw(dyntrustmeelsefail,Arity,ClRefNext(LastClause),
		  NewClause,&Loc);

  Loc = 0;
  if (ClRefTryOpCode(LastClause) == noop)
  {  dbgen_inst_ppvw(trymeelse,Arity,NewClause,
		     LastClause,&Loc);  }
  else if (ClRefTryOpCode(LastClause) == dyntrustmeelsefail)
  {  dbgen_inst_ppvw(retrymeelse,Arity,NewClause,
		     LastClause,&Loc);  }
  else xsb_dbgmsg("***Error 2 in assert: 0x%x",
		  ClRefTryOpCode(LastClause));

  SetClRefPrev(NewClause, LastClause);
}

/* add Clause to end of Pred */
static void db_addbuff(byte Arity, ClRef Clause, PrRef Pred, int AZ, int Inum) 
{
  int Loc; 
  ClRef LastClause ;
  
  if (PredOpCode(Pred) == fail) {
    Loc = 0;
    if (Inum > 1) {dbgen_inst_pppw(jump,Clause,Pred,&Loc);}
    else {dbgen_inst_ppvw(jumptbreg,Arity,Clause,Pred,&Loc);}
    Pred->LastClRef = Clause ;
    SetClRefPrev(Clause, Pred) ;
    Loc = 0;
    dbgen_inst_ppv(noop,sizeof(Cell)/2,Clause,&Loc);
    SetClRefNext(Clause, Pred) ;
  } else if ( PredOpCode(Pred) == jumptbreg || PredOpCode(Pred) == jump ) {
    if (AZ == 0) {
      prefix_to_chain(Arity, Pred->FirstClRef, Clause);
      Pred->FirstClRef = Clause ;
    } else {
      LastClause = Pred->LastClRef ;
      append_to_chain(Arity,LastClause,Clause);
      Pred->LastClRef = Clause ;
    }
  } else xsb_dbgmsg("***Error 3 in assert");
}

static int hash_resize( PrRef Pred, SOBRef SOBrec, int OldTabSize )
{
   int ThisTabSize ;

/* xsb_dbgmsg("SOB - %p, with %d cls",
	      SOBrec, ClRefNumClauses(SOBrec) ) ;
*/
   /* Compute number of clauses */
   if( PredOpCode(Pred) != fail && ClRefType(SOBrec) == SOB_RECORD )
   {    ThisTabSize = ClRefHashSize(SOBrec) ;
        if (ClRefNumClauses(SOBrec)+ClRefNumClauses(SOBrec)/2 >= ThisTabSize)
            ThisTabSize = 2*ThisTabSize+1 ;
	return max(ThisTabSize, OldTabSize) ;
    }
    else return OldTabSize ;
}

static int can_hash(int Ind, prolog_term Head )
{
  int i, j ;
  if (Ind < 256) {  /* handle usual case specially */
    return !is_var(p2p_arg(Head,Ind)) ;
  } else {
    for (i = 2; i >= 0; i--) {
      j = (Ind >> (i*8)) & 0xff;
      if (j > 0 && is_var(p2p_arg(Head,j)) ) return 0 ;
    }
  }
  return TRUE;
}

static int hash_val(int Ind, prolog_term Head, int TabSize )
/* assumes we can hash to this Ind */
{
  int Hashval = 0 ;
  int i, j ;
  prolog_term Arg ;

  if (Ind < 256) {  /* handle usual case specially */
    Arg = p2p_arg(Head,Ind) ;
    /* The following line is a hack and should be taken out
     * when the compiler change for indexing []/0 is made. */
    if (is_nil(Arg)) Hashval = ihash(0, TabSize);
    else Hashval = ihash(val_to_hash(Arg), TabSize);
  } else {   /* handle joint indexes */
    for (i = 2; i >= 0; i--) {
      j = (Ind >> (i*8)) & 0xff;
      if (j > 0) {
        Arg = p2p_arg(Head,j);
        Hashval += Hashval + ihash(val_to_hash(Arg), TabSize);
    } }
    Hashval %= TabSize;
  }
  return Hashval ;
}

static SOBRef new_SOBblock(int ThisTabSize, int Ind )
{
   int i, Loc ;
   SOBRef NewSOB ;

   /* get NEW SOB block */
   MakeClRef(NewSOB,SOB_RECORD,9+ThisTabSize);
/*   xsb_dbgmsg("New SOB %p, size = %d", NewSOB, ThisTabSize); */
   Loc = 0 ;
   dbgen_inst3_sob( Ind>255 ? switchon3bound : switchonbound,
 	  Ind,ClRefHashTable(NewSOB),ThisTabSize,&ClRefSOBInstr(NewSOB),&Loc);
   /* set the PrRef inside SOB */
   Loc = 0 ;
   dbgen_inst_ppp(fail,&ClRefJumpInstr(NewSOB),&Loc);
   ClRefFirstIndex(NewSOB) = (Cell)&ClRefJumpInstr(NewSOB) ;
   ClRefLastIndex( NewSOB) = (Cell)&ClRefJumpInstr(NewSOB) ;
   ClRefNumClauses(NewSOB) = 0 ;
      
   /* Initialize hash table */
   for (i = 0; i < ThisTabSize; i++)
      ClRefHashTable(NewSOB)[i] = (Cell)&fail_inst ;

   return NewSOB ;
}

static void addto_hashchain( int AZ, int Hashval, SOBRef SOBrec, CPtr NewInd,
			     int Arity )
{
    CPtr *Bucketaddr = (CPtr *) (ClRefHashTable(SOBrec) + Hashval);
    CPtr OldInd = *Bucketaddr ;
    int Loc ;

    if ((pb)OldInd == (pb)&fail_inst) { /* empty bucket, add first clause */
      *Bucketaddr = NewInd ;
      IndRefPrev(NewInd) = (CPtr) Bucketaddr ;
      IndRefNext(NewInd) = (CPtr) SOBrec ;
    } else if (AZ == 0) { /* add at beginning */
      *Bucketaddr = NewInd ;
      IndRefPrev(NewInd) = (CPtr) Bucketaddr ;
      Loc = 0;
      if (cell_opcode(OldInd) == noop)
      {  dbgen_inst_ppvw(dyntrustmeelsefail,Arity,IndRefNext(OldInd),
			 OldInd,&Loc); }
      else
      {  dbgen_inst_ppvw(retrymeelse,Arity,IndRefNext(OldInd),
			 OldInd,&Loc); }
      Loc = 0;
      dbgen_inst_ppvw(trymeelse,Arity,OldInd,NewInd,&Loc);
      IndRefPrev(OldInd) = NewInd;
    } else { /* AZ == 1 add at end */
      Loc = 0;
      if (cell_opcode(OldInd) == noop)
      {  dbgen_inst_ppvw(trymeelse,Arity,NewInd,OldInd,&Loc); }
      else {
        while (cell_opcode(OldInd) != dyntrustmeelsefail)
          OldInd = IndRefNext(OldInd);
        dbgen_inst_ppvw(retrymeelse,Arity,NewInd,OldInd,&Loc);
      }
      Loc = 0;
      dbgen_inst_ppvw(dyntrustmeelsefail,Arity, SOBrec, NewInd,&Loc);
      IndRefPrev(NewInd) = OldInd ;
    }
}

static void addto_allchain( int AZ, ClRef Clause, SOBRef SOBrec, int Arity)
{
  ClRef Last, First ;
  int Loc ;

  /* add code buff to all chain */
  if (PredOpCode(ClRefPrRef(SOBrec)) == fail) { /* insert first clrefI into SOB buff */
    Loc = 0 ;
    dbgen_inst_pppw(jump,Clause,ClRefPrRef(SOBrec),&Loc);
    ClRefLastIndex(SOBrec) = (Cell) Clause ;
    ClRefPrev(Clause) = SOBrec ;
    Loc = 0;
    dbgen_inst_ppv(noop,sizeof(Cell)/2,Clause,&Loc);
    SetClRefNext(Clause, SOBrec);
  } else if (AZ == 0) {  /* add at beginning */
    First = (ClRef) ClRefFirstIndex(SOBrec);
    prefix_to_chain(Arity,First,Clause);
    ClRefPrev(First) = Clause;
    ClRefFirstIndex(SOBrec) = (Cell) Clause;
  } else {  /* add at end */
    Last = (ClRef) ClRefLastIndex(SOBrec);
    append_to_chain(Arity, Last, Clause);
    ClRefPrev(Clause) = Last ;
    ClRefLastIndex(SOBrec) = (Cell) Clause;
  }
}

/* adds an indexed buffer to an index chain */
static void db_addbuff_i(int Arity, ClRef Clause, PrRef Pred, int AZ,
			 int *Index, int NI, prolog_term Head, int HashTabSize)
{ SOBRef SOBbuff ;
  int Inum, Ind;
  int ThisTabSize, Hashval;

  SOBbuff = AZ == 0 ? Pred->FirstClRef : Pred->LastClRef ;
  HashTabSize = ThisTabSize = hash_resize(Pred, SOBbuff, HashTabSize);
  
  for (Inum = 1; Inum <= NI; Inum++) {
    SOBbuff = AZ == 0 ? Pred->FirstClRef : Pred->LastClRef ;
    Ind = Index[Inum];
    if( can_hash(Ind, Head) )
	Hashval = hash_val(Ind, Head, ThisTabSize) ;
    else
    {   Hashval = 0; 
	ThisTabSize = 1;
    }
    if (PredOpCode(Pred) == fail || ClRefType(SOBbuff) != SOB_RECORD
	|| ClRefHashSize(SOBbuff) != ThisTabSize
	|| ClRefSOBArg(SOBbuff,1) != (byte)(Ind>>16)  /* for byte-back */
	|| ClRefSOBArg(SOBbuff,2) != (byte)(Ind>>8)
	|| ClRefSOBArg(SOBbuff,3) != (byte)Ind) {
      SOBbuff = new_SOBblock(ThisTabSize,Ind);
      /* add new SOB block */
      db_addbuff(Arity,SOBbuff,Pred,AZ,Inum);
    }
    ClRefNumClauses(SOBbuff)++ ;
    Pred = ClRefPrRef(SOBbuff) ;
    addto_hashchain(AZ, Hashval, SOBbuff, ClRefIndPtr(Clause,Inum), Arity);
  }
  addto_allchain( AZ, Clause, SOBbuff, Arity ) ;
}

/** The following macros traverse the SOB chains/trees
 ** and pick the first (next) clause since a given sob
 **/

/* Check if a clause with head H is in the hash table of a SOB */
/* The indexing Level is used to adjust the returned clause    */
/* pointer to the beginning of the clause		       */

#define CheckSOBClause(H, Ind, SOB, Level )			\
{    int h, t ;							\
     ClRef cl ;				    			\
     t = ClRefHashSize(SOB); 					\
     h = hash_val( (Ind), (H), t ) ;				\
     cl = (ClRef) ClRefHashTable(SOB)[h] ;			\
     if ((pb)cl != (pb)&fail_inst)				\
	return IndPtrClRef(cl,Level) ;				\
}

/* This macro finds the next SOB to search in the indexing tree */
/* the ordering is down, left, (up+)left, the down and left	*/
/* parts being performed by the next macro.			*/
/* It returns 0 if the root (prref) is ever reached.		*/
/* Compiled clauses may be intermixed with dynamic ones, so 	*/
/* that possibility must be checked.				*/

#define NextSOB(s,cur_level)					\
{   while( ClRefTryOpCode(s) == dyntrustmeelsefail		\
	|| ClRefTryOpCode(s) == noop ) /* end of sob chain */	\
	if( cur_level-- == 1 ) /* root of sob tree */		\
		return 0 ;					\
	else	s = ClRefUpSOB(s) ; /* go up */			\
    s = ClRefNext(s) ; /* follow sob chain */			\
    if( ClRefType(s) != SOB_RECORD ) return s;			\
}

/* s points to first SOB of the index chain			*/
/* look for Head/Ind in all SOB chains for this index level	*/
/* if needed go up to look in next sob chain(s)			*/

#define FirstClauseSOB(s,i,l,Head,Ind)				\
{   for(;;)							\
	if( i < l ) /* sob node */				\
	{   s = ClRefPrRef(s)->FirstClRef; /* go down */	\
	    i++ ;						\
	}							\
	else /* i == l -> sob leaf */				\
	{   CheckSOBClause(Head,Ind,s,i) ;			\
	    NextSOB(s,i) ;					\
	}							\
}

static ClRef first_clref( PrRef Pred, prolog_term Head,
			  int *ILevel, int *Index )
{   SOBRef s ;	/* working SOB */
    int i, l ;  /* index depth */
    int Ind = 0;   /* indexing sob instruction argument JF: init */

    if( PredOpCode(Pred) == fail )
	return 0 ;

    /* first findout what index shall we use */
    *Index = *ILevel = 0 ;
    for( s = Pred->FirstClRef, i = 1;
	 ClRefType(s) == SOB_RECORD;
	 s = (Pred=ClRefPrRef(s))->FirstClRef, i++ )
    {
	Ind = ((ClRefSOBArg(s,1) << 8) | ClRefSOBArg(s,2) ) << 8 |
		ClRefSOBArg(s,3) ;
	if( can_hash(Ind,Head) )
	{   *Index = Ind ; *ILevel = i ;
	    /* This is the index we're going to use */
	    break ;
	}
    }
    if( *ILevel == 0 )	/* It's not indexable, so s points to first clause */
	return s ;	/* in all chain of first SOB at lowest level */
    else
    {	i = l = *ILevel ;
	FirstClauseSOB(s,i,l,Head,Ind) ;
    }
}

static ClRef next_clref( PrRef Pred, ClRef Clause, prolog_term Head,
			 int IndexLevel, int Ind )
{   SOBRef s ;	/* working SOB */
    int ni ;	/* number of indexes */
    int i ;	/* how deep is s in the indexing trees (0->Prref/ni->leaf) */
    CPtr PI ;	/* working index pointer */

    if( ClRefType(Clause) != INDEXED_CL ) {	/* mixed clause types */
	if( ClRefTryOpCode(Clause) == dyntrustmeelsefail
	    || ClRefTryOpCode(Clause) == noop )
	  return 0 ;
	else if( ClRefType(ClRefNext(Clause)) != SOB_RECORD )
	  return ClRefNext(Clause) ;
	else /* should do as in cl_ref_first -- to index */
	     /* when first clause COMPILED */
	{   s = ClRefNext(Clause) ; 
	    if( IndexLevel == 0 ) /* goto first cl in all chain */
	    {	while( ClRefType(s) == SOB_RECORD )
		    s = ClRefPrRef(s)->FirstClRef ;
	 	return s ;
	    }
	    else
	    {   for( i = 1 ; i < IndexLevel ; i++ )
		    s = ClRefPrRef(s)->FirstClRef ; /* all the way down */
		CheckSOBClause(Head,Ind,s,IndexLevel) ;
		NextSOB(s,i) ;
		FirstClauseSOB(s,i,IndexLevel,Head,Ind) ;
	    }
	}
    }
    else if( IndexLevel == 0 ) { /* look in all chain */
	if( ClRefTryOpCode(Clause) == trymeelse || /* mid chain */
	    ClRefTryOpCode(Clause) == retrymeelse ) 
	    return ClRefNext(Clause) ;
        else /* INDEXED_CL, look on next SOB */
	{   ni = i = ClRefNumInds(Clause);
				      /* all chain is on lowest index chain */
	    s = ClRefNext(Clause);    /* s = current SOB */
	    NextSOB(s,i);
	    /* all leaf SOBs have non empty all chains */
	    while( i++ < ni )
		s = ClRefPrRef(s)->FirstClRef ;
	    return ClRefPrRef(s)->FirstClRef ;
	}
    }
    else	/* look in appropriate hash chain */
    {	PI = ClRefIndPtr(Clause,IndexLevel) ;
	if( cell_opcode(PI) == trymeelse || /* mid chain */
	    cell_opcode(PI) == retrymeelse ) 
	    return IndPtrClRef(IndRefNext(PI),IndexLevel) ;
	else /* end of chain */
	{   i = IndexLevel ;
	    s = (SOBRef)IndRefNext(PI) ; /* s = current SOB */
	    NextSOB(s,i) ;
	    FirstClauseSOB(s,i,IndexLevel,Head,Ind) ;
	}
    }
}
/*
#define RETRACT_DEBUG
#define RETRACT_GC_DEBUG
*/
/* Generic macro that deletes an element from a chain		*
 * Made possible because of the design of all chains containing	*
 * the three words:						*
 * 		(-1)Prev - (0)TryInstr - (1)Next		*
 * Index Rec Macros are used, although any kind of chain can be *
 * handled							*
 * Args are PC    - pointer to Chain element			*
 *          Displ - value to use as noop arg for begging of BC  *
 * return position of element just deleted		        */

#define delete_from_chain( c, PC, Displ )                               \
{   switch( c )                                                         \
    {   case noop: /* uniq */                                           \
            break ;                                                     \
        case trymeelse: /* first */                                     \
            IndRefPrev(IndRefNext(PC)) = IndRefPrev(PC) ;               \
            if( cell_opcode(IndRefNext(PC)) == retrymeelse )            \
                cell_opcode(IndRefNext(PC)) = trymeelse ;               \
            else /* dyntrustme */                                       \
            {   cell_opcode(IndRefNext(PC)) = noop ;                    \
                cell_operand3(IndRefNext(PC)) = (Displ) ;               \
            }                                                           \
            break ;                                                     \
        case retrymeelse: /* mid */                                     \
            IndRefPrev(IndRefNext(PC)) = IndRefPrev(PC) ;               \
            IndRefNext(IndRefPrev(PC)) = IndRefNext(PC) ;               \
            break ;                                                     \
        case dyntrustmeelsefail: /* last */                             \
            IndRefNext(IndRefPrev(PC)) = IndRefNext(PC) ;               \
            if( cell_opcode(IndRefPrev(PC)) == retrymeelse )            \
                cell_opcode(IndRefPrev(PC)) = dyntrustmeelsefail ;      \
            else /* trymeelse */                                        \
            {   cell_opcode(IndRefPrev(PC)) = noop ;                    \
                cell_operand3(IndRefPrev(PC)) = (Displ) ;               \
            }                                                           \
            break ;                                                     \
        default:                                                        \
            xsb_exit("error removing a clause: %x",c) ;                 \
            break ;                                                     \
    }                                                                   \
}

/* delete from an hash chain */

static void delete_from_hashchain( ClRef Clause, int Ind, int NI )
{  
    CPtr PI = ClRefIndPtr(Clause,Ind) ;
    byte c = cell_opcode(PI) ;

    delete_from_chain(c,PI,((NI-Ind)*4+1)*sizeof(Cell)/2) ;

    if( cell_opcode(PI) == noop )
        *IndRefPrev(PI) = (Cell) &fail_inst ;
    else if( cell_opcode(PI) == trymeelse )
        *IndRefPrev(PI) = (Cell) IndRefNext(PI) ;
}

/* delete from the chain pointed by a prref - a all chain or a sob chain */
/* Works for SOBs and all sorts of clauses */

static void delete_from_allchain( ClRef Clause )
{  
    PrRef Pred ;
    byte c = ClRefTryOpCode(Clause) ;

    delete_from_chain( c, (CPtr)Clause, sizeof(Cell)/2 ) ;

    switch( c )
    {   case noop:
            Pred = ClRefPrRef(ClRefPrev(Clause)) ;
            PredOpCode(Pred) = fail ;
            Pred->FirstClRef = Pred->LastClRef = (ClRef) Pred ;
            break ;
        case trymeelse:
            Pred = ClRefPrRef(ClRefPrev(Clause)) ;
            Pred->FirstClRef = ClRefNext(Clause) ;
            break ;
        case dyntrustmeelsefail:
            Pred = ClRefPrRef(ClRefNext(Clause)) ;
            Pred->LastClRef = ClRefPrev(Clause) ;
            break ;
     }
}

static void delete_from_sobchain(ClRef Clause)
{  
    PrRef Pred ;
    byte c = ClRefTryOpCode(Clause) ;

    delete_from_chain( c, (CPtr)Clause, sizeof(Cell)/2 ) ;

    switch( c )
    {   case noop:
            Pred = (PrRef)ClRefPrev(Clause) ;
            PredOpCode(Pred) = fail ;
            Pred->FirstClRef = Pred->LastClRef = (ClRef) Pred ;
            break ;
        case trymeelse:
            Pred = (PrRef)ClRefPrev(Clause) ;
            Pred->FirstClRef = ClRefNext(Clause) ;
            break ;
        case dyntrustmeelsefail:
            Pred = (PrRef)ClRefNext(Clause) ;
            Pred->LastClRef = ClRefPrev(Clause) ;
            break ;
     }
}

/* circular buffer for retracted clauses */
#define MAX_RETRACTED_CLAUSES   25

ClRef retracted_buffer[MAX_RETRACTED_CLAUSES+1]  ;
ClRef *OldestCl = retracted_buffer, *NewestCl = retracted_buffer;

#define next_in_buffer(pCl) ((pCl)>=(retracted_buffer+MAX_RETRACTED_CLAUSES)\
                                ? retracted_buffer : (pCl) + 1)

#define retract_buffer_empty()  (NewestCl == OldestCl)
#define retract_buffer_full()   (OldestCl == next_in_buffer(NewestCl))

#define insert_in_buffer(Cl) (*NewestCl = (Cl),\
                                NewestCl = next_in_buffer(NewestCl))
#define remove_from_buffer() (OldestCl = next_in_buffer(OldestCl))

/* Insert in retract buffer and remove old clauses */

#define delete_clause(Clause)\
{   if( retract_buffer_full() )\
    {   ClRef ClauseToDelete = *OldestCl;\
	remove_from_buffer();\
	really_delete_clause(ClauseToDelete);\
    }\
    insert_in_buffer(Clause);\
}

static int really_delete_clause(ClRef Clause)
{
#ifdef RETRACT_DEBUG
            xsb_dbgmsg("Really deleting clause(%p) op(%x) type(%d)",
		       Clause, ClRefTryOpCode(Clause), ClRefType(Clause) ) ;
#endif
    switch( ClRefType(Clause) )
    {
        case UNINDEXED_CL:
	  delete_from_sobchain(Clause) ;
	  break ;

        case INDEXED_CL:
        {   int i ;
            SOBRef sob ;
            CPtr IP ;

            NI = ClRefNumInds(Clause) ;
#ifdef RETRACT_DEBUG
            xsb_dbgmsg("Really deleting clause (%p) size %d indexes %d",
		       Clause, ClRefSize(Clause), NI ) ;
#endif
            delete_from_allchain(Clause) ;

            /* remove it from index chains */
            for( i = NI; i >= 1; i-- )
            {	IP = ClRefIndPtr(Clause, i) ;
                while(  cell_opcode(IP) != dyntrustmeelsefail &&
                        cell_opcode(IP) != noop )
                	IP = IndRefNext(IP) ;
                /* last pointer in index chain points to indexing SOB */
                sob = (SOBRef)IndRefNext(IP) ;
#ifdef RETRACT_DEBUG
                xsb_dbgmsg("SOB(%d) - hash size %d - %d clauses",
			   i, ClRefHashSize(sob), ClRefNumClauses(sob) ) ;
                xsb_dbgmsg("Addr %p : prev %p : next %p",
			   sob, ClRefNext(sob), ClRefPrev(sob) ) ;
#endif
                delete_from_hashchain(Clause,i,NI) ;
                if( --ClRefNumClauses(sob) == 0 )
                {
#ifdef RETRACT_DEBUG
                    xsb_dbgmsg("deleting sob - %p", sob ) ;
#endif
                    delete_from_sobchain(sob) ;
                }
            }
            break ;
        }
        case COMPILED_CL:
	  return(0);  /* error message already given */
        case SOB_RECORD:
        default :
	  xsb_exit( "retract internal error!" ) ;
    }
    mem_dealloc((pb)ClRefAddr(Clause), ClRefSize(Clause));
    return TRUE ;
}

static int retract_clause( ClRef Clause, int retract_nr )
{
#ifdef RETRACT_DEBUG
            xsb_dbgmsg("Retract clause(%p) op(%x) type(%d)",
		       Clause, ClRefTryOpCode(Clause), ClRefType(Clause) ) ;
#endif
    switch( ClRefType(Clause) )
    {
        case UNINDEXED_CL:
	    /* set fail for retract_nr AND protection */
	  if (cell_opcode(ClRefEntryPoint(Clause)) == fail &&
	      cell_operand1(ClRefEntryPoint(Clause)) == 66) 
	    retract_nr = 1;  /* previously scheduled for deletion */
	  else {
	    cell_opcode(ClRefEntryPoint(Clause)) = fail ;
	    cell_operand1(ClRefEntryPoint(Clause)) = 66;
	  }
	  break ;
        case INDEXED_CL:
	  if (cell_opcode(ClRefIEntryPoint(Clause,ClRefNumInds(Clause))) == fail 
	      && cell_operand1(ClRefIEntryPoint(Clause,ClRefNumInds(Clause))) == 66)
	    retract_nr = 1;  /* previously scheduled for deletion */
	  else {
	    cell_opcode(ClRefIEntryPoint(Clause,ClRefNumInds(Clause))) = fail ;
	    cell_operand1(ClRefIEntryPoint(Clause,ClRefNumInds(Clause))) = 66;
	  }
	  break ;
        case COMPILED_CL:
	  return 0 ; /* cannot retract compiled code */
        case SOB_RECORD:
	  xsb_exit( "retracting indexing record!" ) ;
	  break ;
        default :
	  xsb_exit( "retract internal error!" ) ;
	  break ;
    }
    if (!retract_nr) {
#ifdef RETRACT_DEBUG
      xsb_dbgmsg("Inserting clause in delete buffer(%p) op(%x) type(%d)",
		 Clause, ClRefTryOpCode(Clause), ClRefType(Clause) ) ;
#endif
      delete_clause(Clause) ;
    }
    return TRUE ;
}

/***
 *** Entry points for CLAUSE/RETRACT predicates
 ***/

/* db_get_clause
 * gets next clause from predicate
 * Arg 1 is the previous ClRef, or 0 if this is the first call.
 * Args 2 and 3 must initially be variables; they get set by the first
 *   call and those values should continue to be passed for each 
 *   subsequent call.
 * Arg 4 is the Prref (predicate handle)
 * Arg 5 is a prolog term that matches the head of the clause
 * Arg 6 is 0 for "normal" clauses, 1 for clauses that consist of a fail 
 * instruction (generated by retract_nr and to be passed to reclaim space)
 * Arg 7 returns the clause address
 * Arg 8 returns the clause type
 * Arg 9 returns the jump point into the code
 */

bool db_get_clause( /*+CC, ?CI, ?CIL, +PrRef, +Head, +Failed, -Clause, -Type, -EntryPoint*/ )
{
  PrRef Pred = (PrRef)ptoc_int(4);
  int IndexLevel, IndexArg, ni ;
  ClRef Clause ;
  prolog_term Head = reg_term(5);
  CPtr EntryPoint = 0;
  Integer failed = ptoc_int(6) ;

#ifdef RETRACT_GC_DEBUG
    xsb_dbgmsg("GET CLAUSE P-%p(%x) C-%p(%x) F-%p L-%p",
	       Pred, *(pb)Pred, ptoc_int(1),
	       ptoc_int(1) ? *(pb)(ptoc_int(1)) : 0,
	       Pred->FirstClRef, Pred->LastClRef ) ;
#endif

    if( cell_opcode((CPtr)Pred) == tabletrysingle )
	/* Tabled pred, fetch real prref */
    	Pred = (PrRef)((CPtr *)Pred)[6] ;
    
    if( Pred->LastClRef == (ClRef)Pred )
    {	Clause = 0 ;
	goto set_outputs;
    }

    Clause = (ClRef)ptoc_int(1);
    if (Clause == 0)
    {   Clause = first_clref( Pred, Head, &IndexLevel, &IndexArg ) ;
	ctop_int(2,IndexLevel);
	ctop_int(3,IndexArg);
    }
    else
    {	IndexLevel = ptoc_int(2);
	IndexArg   = ptoc_int(3);
	do /* loop until a clause is found:
		Retracted if looking for failed; 
		Not Retracted if looking for not failed */
	    Clause = next_clref( Pred, Clause, Head, IndexLevel, IndexArg );
	while(Clause && ClRefNotRetracted(Clause)==failed ) ;
    }

set_outputs:
    if( Clause != 0 ) {
	if( ClRefType(Clause) == SOB_RECORD )
	    xsb_exit("Error in get clause");
	else if( ClRefType(Clause) != INDEXED_CL )
	  { EntryPoint = ClRefEntryPoint(Clause) ;}
	else /* ClRefType(Clause) == INDEXED_CL */
	  { ni = ClRefNumInds(Clause) ;
	    EntryPoint = ClRefIEntryPoint(Clause,ni) ;
	  }
    }
    else
      EntryPoint = 0 ;

#ifdef RETRACT_GC_DEBUG
    xsb_dbgmsg("GOT CLAUSE C-%p(%x)", Clause, Clause ? *(pb)Clause : 0 ) ;
#endif

    ctop_int( 7, (Integer)Clause ) ;
    ctop_int( 8, Clause != 0 ? (Integer)ClRefType(Clause) : 4 ) ;
    ctop_int( 9, (Integer)EntryPoint ) ;
    return TRUE ;
}

bool db_reclaim0( /* CLRef, Type */ )
{
  ClRef Clause = (ClRef)ptoc_int(1) ;

  return retract_clause( Clause, 0 ) ;
}

bool db_retract0( /* ClRef, retract_nr */ )
{
  ClRef Clause = (ClRef)ptoc_int(1) ;
  int retract_nr = (int)ptoc_int(2) ;

  return retract_clause( Clause, retract_nr ) ;
}

/*----------------------------------------------------------------------
  in the following, the number 8 denotes the size (in cells) of the
  following fixed sequence of instructions:
         <tabletrysingle, allocate_gc, getVn, calld, new_answer_dealloc>
  that gets generated as an entry point clause for all dynamic tabled
  predicates.
  ----------------------------------------------------------------------*/
#define FIXED_BLOCK_SIZE_FOR_TABLED_PRED     (8 * sizeof(Cell))

bool db_build_prref( /* PSC, Tabled?, -PrRef */ )
{
  CPtr p, tp;
  TIFptr tip;
  int Loc;
  Psc psc = (Psc)ptoc_int(1);
  Integer Arity = get_arity(psc);
  Integer Tabled = ptoc_int(2);

  /* moved this functionality from Prolog (very ugly and error-prone) to C */
  set_type(psc, T_DYNA);
  set_env(psc, T_VISIBLE);
    
  p = (CPtr)mem_alloc(4*sizeof(Cell));
  Loc = 0 ;
  dbgen_inst_ppp(fail,p,&Loc) ;
  p[2] = (Cell)p ;
  if ( Tabled )
    {
      New_TIF(tip,psc);
      if (first_tip == 0) first_tip = tip;
      else TIF_NextTIF(last_tip) = tip;
      last_tip = tip;
      tp  = (CPtr)mem_alloc(FIXED_BLOCK_SIZE_FOR_TABLED_PRED) ;
      Loc = 0 ;
      dbgen_inst_ppvww(tabletrysingle,Arity,(tp+3),tip,tp,&Loc) ;
      dbgen_inst_pvv(allocate_gc,3,3,tp,&Loc) ;
      dbgen_inst_ppv(getVn,2,tp,&Loc) ;  /* was getpbreg */
      dbgen_inst_ppvw(calld,3,p,tp,&Loc) ;
      dbgen_inst_pvv(new_answer_dealloc,Arity,2,tp,&Loc) ;
      set_ep(psc, (pb)tp);
    }
  else set_ep(psc, (pb)p);

  ctop_int(3,(Integer)p) ;
  return TRUE ;
}

bool db_remove_prref( /* PrRef */ ) 
{
  CPtr *p = (CPtr *)ptoc_int(1) ;

#ifdef RETRACT_GC_DEBUG
  xsb_dbgmsg("DEL Prref %p", p ) ;
#endif

  if ( *(pb)p == tabletrysingle )
    {
      mem_dealloc((pb)p, FIXED_BLOCK_SIZE_FOR_TABLED_PRED) ;
    }
  return TRUE ;
}

/*----------------------------------------------------------------------*/
/* some stuff for trie_assert                                           */
/*----------------------------------------------------------------------*/

#define clref_fld(x) ((CPtr) *(x +1))
#define next_clref(x) ((CPtr) *(x +1))
#define last_clref(PRREF)  ((CPtr)((PrRef)(PRREF))->LastClRef)
#define try_type_instr_fld(x)  (ClRefTryOpCode(x))
#define code_to_run(x)   (cell_opcode(ClRefEntryPoint(x)))
#define first_instr_to_run(x)  (cell_opcode(ClRefWord(x,3)))

/*----------------------------------------------------------------------*/

static inline int clref_trie_asserted(CPtr Clref) {
  return((code_to_run(Clref) == jump) && 
	 (first_instr_to_run(Clref) == trie_assert_inst));
}
/*----------------------------------------------------------------------*/

static void abolish_trie_asserted_stuff(CPtr b) {

   BTNptr pRoot;
   
   pRoot = (BTNptr)*(b + 3);
   switch_to_trie_assert;
   delete_trie(pRoot);
   switch_from_trie_assert;
   *(b + 3) = (Cell) 0;
   /* shouldn't we change one of the instr fields too? */
}

/*----------------------------------------------------------------------*/

static int another_buff(Cell Instr)
{
  int op = cell_opcode(&Instr) ;
  return op != noop && op != dyntrustmeelsefail && op != fail ;
}

/*======================================================================*/
/* The following routine deletes all clauses from a prref.  It is the	*/
/* equivalent of retractall(p(_,_,_,..,_). It is given the address of	*/
/* a buffer and frees it and all buffers it points to.			*/
/*======================================================================*/

int gen_retract_all(/* R1: + buff */)
{
  ClRef buffers_to_free[200];
  int btop = 0;
  ClRef buffer ;

  buffers_to_free[btop++] = (ClRef)ptoc_int(1);
  while (btop > 0) {
    buffer = buffers_to_free[--btop];
    switch (ClRefType(buffer)) {
    case SOB_RECORD: 
      if (another_buff(ClRefJumpInstr(buffer)))
	  buffers_to_free[btop++] = (ClRef) ClRefFirstIndex(buffer);
      if (another_buff(ClRefTryInstr(buffer)))
	  buffers_to_free[btop++] = ClRefNext(buffer);
      break ;
    case UNINDEXED_CL: 
    case INDEXED_CL:
      if (another_buff(ClRefTryInstr(buffer)))
	  buffers_to_free[btop++] = ClRefNext(buffer);
	  if( ClRefNotRetracted(buffer) )
		retract_clause(buffer,0) ;
      break;
    case COMPILED_CL:
      {
	if (clref_trie_asserted((CPtr) buffer)) {
	  abolish_trie_asserted_stuff((CPtr) buffer);
        }
        else
	  unload_seg((pseg)ClRefCompiledCode(buffer));
	if (another_buff(ClRefTryInstr(buffer)))
	  buffers_to_free[btop++] = ClRefNext(buffer);
/*	mem_dealloc((pb)ClRefAddr(buffer),ClRefSize(buffer)); */ /*??rfm*/
	break;
      }
    }
  }
  return TRUE;
}

/*---------------------------------------------------------------*/

static inline CPtr trie_asserted_clref(CPtr prref)
{
  CPtr Clref;

  Clref = last_clref(prref);
  if (try_type_instr_fld(prref) != fail) {
    if ((code_to_run(Clref) == jump) &&
	(first_instr_to_run(Clref) == trie_assert_inst))
      return Clref;
  }
  return NULL;
}

/*---------------------------------------------------------------*/

#ifdef DEBUG_T
static void print_bytes(CPtr x, int lo, int hi)
{
  int i;

  printf("addr %p ---------------------------------\n",x);
  for (i = lo; i <= hi ; i++) {
    printf(" i = %d 4*i = %d  x[i] = %x \n",i,4*i, (int)*(x+i));
  }
  printf("Instr = %s ---code to run %s----\n",
	 (char *)inst_table[try_type_instr_fld(x)][0],
	 (char *)inst_table[code_to_run(x)][0] );
}
#endif

/*----------------------------------------------------------------*/

int trie_assert(void)
{
  Cell Clause;
  Psc  psc;
  CPtr Prref;
#ifdef DEBUG_T
  int  Arity;
#endif
  CPtr Trie_Asserted_Clref = NULL;
  BTNptr inst_node_ptr;
  int  found = 1;


  Clause = reg_term(1);
  psc    = (Psc)ptoc_int(2);
  Prref  = (CPtr)ptoc_int(4);

#ifdef DEBUG_T
  Arity  = ptoc_int(3);
  printf("Prref bytes\n");
  print_bytes(Prref,-2,2);
  printf("Clause :");
  printterm(Clause,1,24);
  printf(" Arity %d ", Arity);
  printf(" Psc   %d ",(int)psc);
  printf(" Prref %d ",(int)Prref);
  printf("\n");
#endif

  Trie_Asserted_Clref = trie_asserted_clref(Prref);

#ifdef ASSERTDEBUG
  printf(" Trie_Asserted_Clref %p \n",Trie_Asserted_Clref);
#endif

  switch_to_trie_assert;
  
  if(Trie_Asserted_Clref == NULL){
    /*
     * Allocate the trie node as in old trie assert: put it in a clref
     * block and pray.
     */
    Trie_Asserted_Clref = ((CPtr)mem_alloc(6*sizeof(Cell))) + 2;
    *(Trie_Asserted_Clref-2) = 6*sizeof(Cell)+2; /* store size, encode type */
    *(byte *)(Trie_Asserted_Clref +2) = jump;

    inst_node_ptr = newBasicTrie(psc,ASSERT_TRIE_TT);
    Instr(inst_node_ptr) = trie_assert_inst;

    *(Trie_Asserted_Clref +3) = (Cell)inst_node_ptr;

    db_addbuff(get_arity(psc) + 1,(ClRef)Trie_Asserted_Clref,(PrRef)Prref,1,2);
  }
  else
    inst_node_ptr = (BTNptr)*(Trie_Asserted_Clref +3);

  one_term_chk_ins((CPtr)Clause,inst_node_ptr,&found);

  switch_from_trie_assert;	
  ctop_int(5,found);
  return TRUE;
}
/*-----------------------------------------------------------------*/

int trie_retract(void)
{
  CPtr Clref;
  BTNptr inst_node_ptr;

  Clref = (CPtr)ptoc_int(1);
  if (Clref == NULL) {
    Last_Nod_Sav = NULL;
    return TRUE;
  }
  else if (Last_Nod_Sav == NULL) {
    xsb_dbgmsg("Last_Nod_Sav is NULL ");
    return FALSE;
  }
  else {
    inst_node_ptr = (BTNptr)*(Clref +3);
#ifdef DEBUG_T
    printf(" Deleting from Instrn Node %p\n",  inst_node_ptr );
    printf(" Before: Child of Instrn Node %p\n", Child(inst_node_ptr));
#endif
    switch_to_trie_assert;
    delete_branch(Last_Nod_Sav, &(Child(inst_node_ptr)));
    switch_from_trie_assert;
#ifdef DEBUG_T
    printf(" After : Child of Instrn Node %p\n", Child(inst_node_ptr));
#endif
    return TRUE;
  }
}

/*-----------------------------------------------------------------*/

/* Only mark the nodes in the branch as deleted. */

int trie_retract_safe(void)
{ 
  if (Last_Nod_Sav == NULL)
    return FALSE;
  else {
    safe_delete_branch(Last_Nod_Sav);
    return TRUE;
  }
}

/*-----------------------------------------------------------------*/
