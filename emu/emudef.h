/* File:      emudef.h
** Author(s): Warren, Swift, Xu, Sagonas
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



/* Argument Registers
   ------------------ */
Cell reg[MAX_REGS];


/* Special Registers
   ----------------- */
CPtr ereg;		/* last activation record       */
CPtr breg;		/* last choice point            */
CPtr hreg;		/* top of heap                  */
CPtr *trreg;		/* top of trail stack           */
CPtr hbreg;		/* heap back track point        */
CPtr sreg;		/* current build or unify field */
byte *cpreg;		/* return point register        */
byte *pcreg;		/* program counter              */
CPtr ebreg;		/* breg into environment stack	*/

#if (!defined(CHAT))
CPtr efreg;
CPtr bfreg;
CPtr hfreg;
CPtr *trfreg;
#endif
CPtr pdlreg;
CPtr openreg;
CPtr root_address;

CPtr ptcpreg;
CPtr delayreg;

/*
 * Ptr to the beginning of instr. array
 */ 
byte *inst_begin;

byte call_intercept = 0;	/* hitrace or trace_sta for efficiency */

char *nil_sym;
Pair list_str;

Psc list_psc, comma_psc;
Psc tnot_psc, delay_psc;

/*
 * Ret PSC's are used to store substitution factors for subgoal calls
 * or answers.  A psc with a new arity will be created when needed,
 * except that ret_psc[0] stores the pointer to STRING "ret" and is
 * initialized when the system is started.
 */
Psc ret_psc[MAX_ARITY];

char *list_dot;

int asynint_val = 0;
int *asynint_ptr = &asynint_val;

/* Replacements for labelled code in emusubs.i */

#define nunify_with_nil(op) \
    deref(op); \
    if (isref(op)) {  \
	/* op is FREE */ \
	bind_nil((CPtr)(op)); \
    } \
    else if (isnil(op)) goto contcase; /* op == [] */ \
    else Fail1;	/* op is LIST, INT, or FLOAT */ 

/*======================================================================*/

#define nunify_with_con(OP1,OP2) \
    deref(OP1); \
    if (isref(OP1)) {  \
	/* op1 is FREE */ \
	bind_string((CPtr)(OP1), (char *)OP2); \
    } \
    else if (isstring(OP1)) { \
        if (string_val(OP1) == (char *)OP2) goto contcase; else Fail1; \
    } \
    else Fail1;

/*======================================================================*/

#define nunify_with_num(OP1,OP2) \
    /* op1 is general, op2 has number (untagged) */ \
    deref(OP1); \
    if (isref(OP1)) {  \
	/* op1 is FREE */\
	bind_int((CPtr)(OP1), (Integer)OP2);\
    } \
    else if (isinteger(OP1)) {\
       if (int_val(OP1) == (Integer)OP2) goto contcase; else Fail1;\
    }\
    else Fail1;	/* op1 is STRING, FLOAT, CS, or LIST */

/*======================================================================*/

#define nunify_with_float(OP1,OP2) \
    deref(OP1); \
    if (isref(OP1)) {  \
        /* op1 is FREE */ \
	bind_float(vptr(op1), asfloat(op2)); \
    } \
    else if (isfloat(op1)) { \
       if (float_val(op1) == asfloat(op2)) goto contcase; else Fail1; \
    } \
    else Fail1;	/* op1 is INT, STRING, CS, or LIST */ 

/*======================================================================*/

#define nunify_with_str(OP1,OP2) \
    /* struct psc_rec *str_ptr; using op2 */ \
    deref(OP1); \
    if (isref(OP1)) {  \
	/* op1 is FREE */ \
	bind_cs((CPtr)(OP1), (Pair)hreg); \
	new_heap_functor(hreg, (Psc)OP2); \
	flag = WRITE; \
    } \
    else if (isconstr(OP1)) {	 \
	OP1 = (Cell)(cs_val(OP1)); \
	if (*((Psc *)OP1) == (Psc)OP2) { \
	    flag = READFLAG; \
	    sreg = (CPtr)OP1 + 1; \
	} \
	else Fail1; \
     } \
     else Fail1;

/*======================================================================*/

#define nunify_with_list_sym(OP1) \
    deref(OP1); \
    if (isref(OP1)) {  \
	/* op1 is FREE */\
	bind_list((CPtr)(OP1), hreg);\
	flag = WRITE;\
    }\
    else if (islist(OP1)) {	\
	sreg = clref_val(OP1);\
	flag = READFLAG;\
    }\
    else Fail1;

/*======================================================================*/

#define obtain_ep(PSC) dyn_pred = (PFI)get_ep(PSC);

#define call_sub(PSC) \
  if (*asynint_ptr > 0) {                /* interrupt detected */ \
    if (*asynint_ptr == KEYINT_MARK) { \
      synint_proc(PSC, MYSIG_KEYB, lpcreg-2*sizeof(Cell));\
      lpcreg = pcreg; \
    } \
    else \
      lpcreg = (byte *)get_ep(PSC); \
    *asynint_ptr = 0; \
  } \
  else \
    switch (get_type(PSC)) { \
    case T_PRED: \
    case T_FUNC: \
    case T_DYNA: \
      lpcreg = (pb)get_ep(PSC); \
      check_glstack_overflow(get_arity(PSC),lpcreg,OVERFLOW_MARGIN); \
      break; \
    case T_FORN: \
      obtain_ep(PSC);\
      /* only call the predicate in case it won't be called in intercept() */ \
      if (!call_intercept || \
          !(flags[DEBUG_ON] && !flags[HIDE_STATE] && \
               (get_spy(psc) || flags[TRACE]) \
         ) ) \
      { dyn_pred(); \
        lpcreg = cpreg;  /* "proceed" */ \
      } \
      break; \
    case T_UDEF: \
    case T_UFUN: \
    default: \
      PSC = synint_proc(PSC, MYSIG_UNDEF, lpcreg-2*sizeof(Cell)); \
      if (!PSC) \
	lpcreg = pcreg; \
      else \
	lpcreg = get_ep(PSC); \
      break; \
    } \
  if (call_intercept) {               /* for debugging or for statistics */ \
    pcreg = lpcreg; \
    intercept(PSC); \
    lpcreg = pcreg; \
  }

