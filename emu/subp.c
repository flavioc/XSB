/* File:      subp.c
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


/* xsb_config.h must be the first #include.  Pls don't move it! */
#include "xsb_config.h"
#include "xsb_debug.h"

#include "debugs/debug_attv.h"

#include <stdio.h>
#include <signal.h>

#ifdef WIN_NT
#include <windows.h>
#include <process.h>	/* _beginthread, _endthread */
#include <stddef.h>
#include <stdlib.h>
#include <winsock.h>
#include <io.h>
#include <string.h>
#endif

#include "auxlry.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "psc_xsb.h"
#include "memory_xsb.h"
#include "register.h"
#include "heap_xsb.h"
#include "deref.h"
#include "flags_xsb.h"
#include "binding.h"
#include "trie_internals.h"
#include "trassert.h"
#include "choice.h"
#include "token_xsb.h"
#include "sig_xsb.h"
#include "inst_xsb.h"
#include "macro_xsb.h"
#include "table_stats.h"
#include "unify_xsb.h"
#include "debug_xsb.h"
#include "hash_xsb.h"

/*======================================================================*/
/*======================================================================*/

/* attv_dbgmsg() is used in unify_xsb_i.h */

#undef IFTHEN_FAILED
#define IFTHEN_FAILED	return 0
#undef IFTHEN_SUCCEED
#define IFTHEN_SUCCEED	return 1

double realtime_count;

extern int asynint_val;	/* 0 - no interrupt (or being processed) */
extern int asynint_code;	/* 0 means keyboard interrupt */

extern void dis(xsbBool), debug_call(Psc);
extern void total_stat(double);
extern void perproc_stat(void), perproc_reset_stat(void), reset_stat_total(void); 

#ifdef LINUX
static struct sigaction act, oact;
#endif

void (*xsb_default_segfault_handler)(int); /* where the previous value of the
					     SIGSEGV/SIGBUS handler is saved */

Cell attv_interrupts[20480][2];

/*
 * Put an attv interrupt into the interrupt chain. op1 is the related
 * attv, and op2 is the value (see verify_attributes/2).
 */

void add_interrupt(Cell op1, Cell op2) {
  int num;

#ifndef PRE_IMAGE_TRAIL
#error "PRE_IMAGE_TRAIL has to be defined for add_interrupt() !"
#else
  num = int_val(cell(interrupt_reg));
  /**printf("interrupt count = %d\n",num);**/
  push_pre_image_trail(&(attv_interrupts[num][0]), op1);
  attv_interrupts[num][0] = op1;
  push_pre_image_trail(&(attv_interrupts[num][1]), op2);
  attv_interrupts[num][1] = op2;
  num++;
  push_pre_image_trail(interrupt_reg, makeint(num));
  bld_int(interrupt_reg, num);

#endif
}


Cell build_interrupt_chain(void) {
  Cell head;
  CPtr tmp = &head;
  int num, i;

  num = int_val(cell(interrupt_reg));
  for (i = 0; i < num; i++) {
    bld_list(tmp, hreg);
    sreg = hreg + 2;
    bld_list(hreg, sreg); hreg++;
    if (i == (num - 1)) {
      bind_nil(hreg);
    }
    else
      tmp = hreg;
    bld_copy(sreg, attv_interrupts[i][0]); sreg++;
    bld_copy(sreg, attv_interrupts[i][1]); sreg++;
    hreg = sreg;
  }

#ifndef PRE_IMAGE_TRAIL
#error "PRE_IMAGE_TRAIL has to be defined for synint_proc() !"
#else
  /* Reset the interrupt counter to 0 for further attv interrupts. */
  push_pre_image_trail(interrupt_reg, makeint(0));
#endif

  bld_int(interrupt_reg, 0);

  return head;
}

/*======================================================================*/
/*  Unification routines.						*/
/*======================================================================*/

xsbBool unify(Cell rop1, Cell rop2)
{ /* begin unify */
  register Cell op1, op2;

  op1 = rop1; op2 = rop2;

/*----------------------------------------*/
  unify_xsb(unify);
  /* unify_xsb_i already ends with this statement
     IFTHEN_SUCCEED;
  */
/*----------------------------------------*/

}  /* end of unify */

/*======================================================================*/
/*  Determining whether two terms are identical.			*/
/*======================================================================*/

xsbBool are_identical_terms(Cell term1, Cell term2) {

  XSB_Deref(term1);
  XSB_Deref(term2);
  
  if ( term1 == term2 )
    return TRUE;

  if ( cell_tag(term1) != cell_tag(term2) )
    return FALSE;

  if ( cell_tag(term1) == XSB_STRUCT ) {
    CPtr cptr1 = clref_val(term1);
    CPtr cptr2 = clref_val(term2);
    Psc psc1 = (Psc)*cptr1;
    int i;

    if ( psc1 != (Psc)*cptr2 )
      return FALSE;

    for ( cptr1++, cptr2++, i = 0;  i < (int)get_arity(psc1);  i++ )
      if ( ! are_identical_terms(*cptr1,*cptr2) ) 
	return FALSE;

    return TRUE;
  }
  else if ( cell_tag(term1) == XSB_LIST ) {
    CPtr cptr1 = clref_val(term1);
    CPtr cptr2 = clref_val(term2);

    if ( are_identical_terms(*cptr1, *cptr2) &&
	 are_identical_terms(*(cptr1 + 1), *(cptr2 + 1)) )
      return TRUE;
    else
      return FALSE;
  }
  else
    return FALSE;
}

/*======================================================================*/
/*  Print statistics and measurements.					*/
/*======================================================================*/

/*
 * Called through builtins statistics/1 and statistics/0.
 * ( statistics :- statistics(1). )
 */
void print_statistics(int amount) {

  switch (amount) {
  case 0:		    /* Reset Statistical Parameters */
    realtime_count = real_time();
    perproc_reset_stat();	/* reset op-counts, starting time, and 'tds'
				   struct variable (all 0's) */
    reset_stat_total(); 	/* reset 'ttt' struct variable (all 0's) */
    xsb_mesg("Statistics is reset.");
    break;
  case 1:		    /* Print Stack Usage and CPUtime: */
    perproc_stat();		/* move max usage into 'ttt' struct variable */
    total_stat(real_time()-realtime_count);   /* print */
    reset_stat_total(); 	/* reset 'ttt' struct variable (all 0's) */
    break;
  case 2:		    /* Print Detailed Table Usage */
    print_detailed_tablespace_stats();
    break;
  case 3:		    /* Print Detailed Table, Stack, and CPUtime */
    perproc_stat();
    total_stat(real_time()-realtime_count);
    reset_stat_total();
    print_detailed_tablespace_stats();
    print_detailed_subsumption_stats();
    break;
  case 5:
    dis(0); 
    break;		/* output memory image; for debugging */
  case 6:
    dis(1); 
    break;		/* output memory image; for debugging */
#ifdef CP_DEBUG
  case 7:
    print_cp_backtrace();
    break;
#endif
  case 8:
    symbol_table_stats();
    string_table_stats();
    break;
  }
}

/*======================================================================*/
/*======================================================================*/

static void default_inthandler(int intcode, byte *cur_inst)
{
  char message[80];

  switch (intcode) {
  case MYSIG_UNDEF:
    xsb_exit("Undefined predicate; exiting by the default handler.");
    break;
  case MYSIG_KEYB:
    xsb_exit("Keyboard interrupt; exiting by the default handler.");
    break;
  case MYSIG_PSC:
    break;
  default:
    sprintf(message,
	    "Unknown interrupt (%d) occured; exiting by the default handler", 
	    intcode);
    xsb_exit(message);
    break;
  }
}

/*======================================================================*/
/* builds the current call onto the heap and returns a pointer to it.	*/
/*======================================================================*/

Pair build_call(Psc psc)
{
  register Cell arg;
  register Pair callstr;
  register int i;

  callstr = (Pair)hreg; /* save addr of new structure rec */
  new_heap_functor(hreg, psc); /* set str psc ptr */
  for (i=1; i <= (int)get_arity(psc); i++) {
    arg = cell(reg+i);
    nbldval(arg);
  }
  return callstr;
}

/*======================================================================*/
/* set interrupt code in reg 2 and return ep of interrupt handler.	*/
/* the returned value is normally assigned to pcreg, so this is like	*/
/* raising a trap.							*/
/* Note that the interrupt handlers referred to by flags array values   */
/* are set up on the Prolog side via set_inthandler/2                   */
/*======================================================================*/

Psc synint_proc(Psc psc, int intcode, byte *cur_inst)
{
  if (flags[intcode+INT_HANDLERS_FLAGS_START]==(Cell)0) {
    /* default hard handler */
    default_inthandler(intcode, cur_inst);
    psc = 0;
  } else {				/* call Prolog handler */
    switch (intcode) {
    case MYSIG_UNDEF:		/*  0 */
    case MYSIG_KEYB:		/*  1 */
    case MYSIG_SPY:		/*  3 */
    case MYSIG_TRACE:		/*  4 */
    case MYSIG_CLAUSE:		/* 16 */
      if (psc) bld_cs(reg+1, build_call(psc));
      psc = (Psc)flags[intcode+INT_HANDLERS_FLAGS_START];
      bld_int(reg+2, asynint_code);
      pcreg = get_ep(psc);
      break;
    case MYSIG_ATTV:		/*  8 */
      /* the old call must be built first */
      if (psc)
	bld_cs(reg+2, build_call(psc));
      psc = (Psc)flags[intcode+INT_HANDLERS_FLAGS_START];
      /*
       * Pass the interrupt chain to reg 1.  The counter of attv
       * interrupts (stored in *interrupt_reg) will be reset to 0 in
       * build_interrupt_chain()).
       */
      bld_copy(reg + 1, build_interrupt_chain());
      /* bld_int(reg + 3, intcode); */	/* Not really needed */
      pcreg = get_ep(psc);
      break;
    default:
      xsb_abort("Unknown intcode in synint_proc()");
    } /* switch */
  }
  return psc;
}

void init_interrupt(void);

/* TLS: 2/02 removed "inline static" modifiers so that this function
   can be called from interprolog_callback.c */
void keyint_proc(int sig)
{
#ifndef LINUX
  init_interrupt();  /* reset interrupt, if using signal */
#endif
  if (asynint_val & KEYINT_MARK) {
    xsb_abort("unhandled keyboard interrupt");
  } else {
    asynint_val |= KEYINT_MARK;
    asynint_code = 0;
  }
}

void init_interrupt(void)
{
#if (defined(LINUX))
  act.sa_handler = keyint_proc;
  sigemptyset(&act.sa_mask); 
  act.sa_flags = 0;
  sigaction(SIGINT, &act, &oact);
#else
  signal(SIGINT, keyint_proc); 
#endif

#if (defined(DEBUG_VERBOSE) || defined(DEBUG_VM) || defined(DEBUG_ASSERTIONS))
  /* Don't handle SIGSEGV/SIGBUS if configured with DEBUG */
  xsb_default_segfault_handler = SIG_DFL;
#else 
  xsb_default_segfault_handler = xsb_segfault_quitter;
#endif

#ifdef SIGBUS
  signal(SIGBUS, xsb_default_segfault_handler);
#endif
  signal(SIGSEGV, xsb_default_segfault_handler);
}


/*
 * Maintains max stack usage when "-s" option is given at startup.
 */
void intercept(Psc psc) {

  if (flags[CLAUSE_INT])
    synint_proc(psc, MYSIG_CLAUSE, pcreg-2*sizeof(Cell));
  else if (flags[DEBUG_ON] && !flags[HIDE_STATE]) {
    if (get_spy(psc)) { /* spy'ed pred, interrupted */
      synint_proc(psc, MYSIG_SPY, pcreg-2*sizeof(Cell));
      flags[HIDE_STATE]++; /* hide interrupt handler */
    }
    else if (flags[TRACE]) {
      synint_proc(psc, MYSIG_TRACE, pcreg-2*sizeof(Cell));
      flags[HIDE_STATE]++; /* hide interrupt handler */
    }
  }
  if (flags[HITRACE])
    debug_call(psc);

  if (flags[TRACE_STA]) {
    unsigned long  byte_size;

    byte_size = (top_of_heap - (CPtr)(glstack.low) + 1) * sizeof(Cell);
    if ( byte_size > tds.maxgstack_count )
      tds.maxgstack_count = byte_size;

    byte_size = ((CPtr)glstack.high - top_of_localstk) * sizeof(Cell);
    if ( byte_size > tds.maxlstack_count )
      tds.maxlstack_count = byte_size;

    byte_size = (top_of_trail - (CPtr *)tcpstack.low + 1) * sizeof(CPtr);
    if ( byte_size > tds.maxtrail_count )
      tds.maxtrail_count = byte_size;

    byte_size = ((CPtr)tcpstack.high - top_of_cpstack) * sizeof(Cell);
    if ( byte_size > tds.maxcpstack_count )
      tds.maxcpstack_count = byte_size;

    byte_size = ((CPtr)complstack.high - top_of_complstk) * sizeof(Cell);
    if ( byte_size > tds.maxopenstack_count )
      tds.maxopenstack_count = byte_size;

    if ((unsigned long)level_num > tds.maxlevel_num)
      tds.maxlevel_num = level_num;
  }
}

/*======================================================================*/
/* floating point conversions						*/
/*======================================================================*/

/* lose some precision in conversions from 32 bit formats */
#ifdef BITS64
#define FLOAT_MASK 0xfffffffffffffff0
#else
#define FLOAT_MASK 0xfffffff0
#endif

static union float_conv {
  Float f;
  Cell i;
} float_conv;

Float getfloatval(Cell w)
{
  float_conv.i = w & FLOAT_MASK;
  return float_conv.f;
}

Cell makefloat(Float f)
{
  float_conv.f = f;
  return ( float_conv.i & FLOAT_MASK ) | XSB_FLOAT;
}

Float asfloat(Cell w)
{
  float_conv.i = w;
  return float_conv.f;
}

static inline int sign(Float num)
{
  if (num==0.0) return 0;
  else if (num>0.0) return 1;
  else return -1;
}

/*======================================================================*/
/* compare(V1, V2)							*/
/*	compares two terms; returns zero if V1=V2, a positive value	*/
/*	if V1>V2 and a negative value if V1<V2.  Term comparison is	*/
/*	done according to the ISO standard total order of Prolog	*/
/*	terms which is as follows:					*/
/*									*/
/*	    variables < floats < integers < atoms < compound terms	*/
/*									*/
/*	A list is compared as an ordinary compound term with arity	*/
/*	2 and functor '.'.						*/
/*									*/
/*	This function was rewritten from scratch by Kostis so that	*/
/*	it is independent of the relative order of tag encoding.	*/
/*	However, it should ONLY be used to compare terms that appear	*/
/*	in the above ordering list.					*/
/*======================================================================*/

int compare(const void * v1, const void * v2)
{
  int comp;
  CPtr cptr1, cptr2;
  Cell val1 = (Cell) v1 ;
  Cell val2 = (Cell) v2 ;

  XSB_Deref(val2);		/* val2 is not in register! */
  XSB_Deref(val1);		/* val1 is not in register! */
  if (val1 == val2) return 0;
  switch(cell_tag(val1)) {
  case XSB_FREE:
  case XSB_REF1:
    if (isattv(val2))
      return vptr(val1) - (CPtr)dec_addr(val2);
    else if (isnonvar(val2)) return -1;
    else { /* in case there exist local stack variables in the	  */
	   /* comparison, globalize them to guarantee that their  */
	   /* order is retained as long as nobody "touches" them  */
	   /* in the future -- without copying garbage collection */
      if ((top_of_localstk <= vptr(val1)) &&
	  (vptr(val1) <= (CPtr)glstack.high-1)) {
	bld_free(hreg);
	bind_ref(vptr(val1), hreg);
	hreg++;
	val1 = follow(val1);	/* deref again */
      }
      if ((top_of_localstk <= vptr(val2)) &&
	  (vptr(val2) <= (CPtr)glstack.high-1)) {
	bld_free(hreg);
	bind_ref(vptr(val2), hreg);
	hreg++;
	val2 = follow(val2);	/* deref again */
      }
      return vptr(val1) - vptr(val2);
    }
  case XSB_FLOAT:
    if (isref(val2) || isattv(val2)) return 1;
    else if (isfloat(val2)) 
      return sign(float_val(val1) - float_val(val2));
    else return -1;
  case XSB_INT:
    if (isref(val2) || isfloat(val2) || isattv(val2)) return 1;
    else if (isinteger(val2)) 
      return int_val(val1) - int_val(val2);
    else if (isboxedinteger(val2))
      return int_val(val1) - boxedint_val(val2);
    else return -1;
  case XSB_STRING:
    if (isref(val2) || isfloat(val2) || isinteger(val2) || isattv(val2)) 
      return 1;
    else if (isstring(val2)) {
      return strcmp(string_val(val1), string_val(val2));
    }
    else return -1;
  case XSB_STRUCT:
    if (isboxedinteger(val1)) {
      if (isref(val2) || isfloat(val2) || isattv(val2)) return 1;
      else if (isinteger(val2)) 
	return boxedint_val(val1) - int_val(val2);
      else if (isboxedinteger(val2))
	return boxedint_val(val1) - boxedint_val(val2);
      else return -1;
    } else if (cell_tag(val2) != XSB_STRUCT && cell_tag(val2) != XSB_LIST) return 1;
    else {
      int arity1, arity2;
      Psc ptr1 = get_str_psc(val1);
      Psc ptr2 = get_str_psc(val2);

      arity1 = get_arity(ptr1);
      if (islist(val2)) arity2 = 2; 
      else arity2 = get_arity(ptr2);
      if (arity1 != arity2) return arity1-arity2;
      if (islist(val2)) comp = strcmp(get_name(ptr1), ".");
      else comp = strcmp(get_name(ptr1), get_name(ptr2));
      if (comp || (arity1 == 0)) return comp;
      cptr1 = clref_val(val1);
      cptr2 = clref_val(val2);
      for (arity2 = 1; arity2 <= arity1; arity2++) {
	if (islist(val2))
	  comp = compare((void*)cell(cptr1+arity2), (void*)cell(cptr2+arity2-1));  
	else
	  comp = compare((void*)cell(cptr1+arity2), (void*)cell(cptr2+arity2));
	if (comp) break;
      }
      return comp;
    }
    break;
  case XSB_LIST:
    if (cell_tag(val2) != XSB_STRUCT && cell_tag(val2) != XSB_LIST) return 1;
    else if (isconstr(val2)) return -(compare((void*)val2, (void*)val1));
    else {	/* Here we are comparing two list structures. */
      cptr1 = clref_val(val1);
      cptr2 = clref_val(val2);
      comp = compare((void*)cell(cptr1), (void*)cell(cptr2));
      if (comp) return comp;
      return compare((void*)cell(cptr1+1), (void*)cell(cptr2+1));
    }
    break;
  case XSB_ATTV:
    if (isattv(val2))
      return (CPtr)dec_addr(val1) - (CPtr)dec_addr(val2);
    else if (isref(val2))
      return (CPtr)dec_addr(val1) - vptr(val2);
    else
      return -1;
  default:
    xsb_abort("Compare (unknown tag %ld); returning 0", cell_tag(val1));
    return 0;
  }
}

/*======================================================================*/
/* key_compare(V1, V2)							*/
/*	compares the keys of two terms of the form Key-Value; returns	*/
/*	zero if Key1=Key2, a positive value if Key1>Key2 and a negative */
/*	value if Key1<Key2.  Term comparison is done according to the	*/
/*	standard total order of Prolog terms (see compare()).		*/
/*======================================================================*/

int key_compare(const void * t1, const void * t2)
{
  Cell term1 = (Cell) t1 ;
  Cell term2 = (Cell) t2 ;

  XSB_Deref(term1);		/* term1 is not in register! */
  XSB_Deref(term2);		/* term2 is not in register! */
  return compare((void*)cell(clref_val(term1)+1), (void*)cell(clref_val(term2)+1));
}

/*======================================================================*/
/* print an atom, quote it if necessary.				*/
/*======================================================================*/

void print_qatom(FILE *file, char *string)
{
  char *s;
  int need_quote = 0, type;

  if (intype(*string) != LOWER) need_quote = 1;
  else {
    s = string;    
    while (*s) {
      type = intype(*s);
      if (type != LOWER && type != UPPER && type != DIGIT && type != BREAK) {
	need_quote = 1; break; 
      }
      s++;
    }
  }
  if (need_quote) fprintf(file, "'%s'", string);
  else fprintf(file, "%s", string);
}

/*======================================================================*/
/* print an operator.							*/
/*======================================================================*/

void print_op(FILE *file, char *string, int pos)
{
  char *s;
  int need_blank = 0;

  s = string;
  while (*s) { 
    if (intype(*s) != SIGN) { need_blank = 1; break;} 
    s++;
  }
  if (need_blank) {
    switch (pos) {
      case 1: print_qatom(file, string); putc(' ', file); break;
      case 2: putc(' ', file);
	      print_qatom(file, string); putc(' ', file); break;
      case 3: putc(' ', file); print_qatom(file, string); break;
    }
  } else fprintf(file, "%s", string);
}

/* ----- The following is also called from the Prolog level ----------- */

void remove_open_tables_reset_freezes(void)
{
  if (xwammode) {
    remove_open_tables();
    reset_freeze_registers;
  }
}

/* ----- C level exception handlers ----------------------------------- */

/*
 * Returns the Breg offset stored in the Psc record of "_$abort_cutpoint"
 */
static Cell abort_cp_offset(void)
{
  int  is_new;
  Pair abort_pair;

  abort_pair = insert("_$abort_cutpoint", 0, global_mod, &is_new);
  if (is_new) {
    xsb_exit("Abort cut point could not be found");
    return 0;
  }
  else
    return (Cell) get_data(pair_psc(abort_pair));
}

byte *exception_handler(char *string)
{
  fprintf(stdfdbk, "%s! Aborting...\n", string);
  breg = (CPtr)(tcpstack.high - abort_cp_offset());
  hbreg = cp_hreg(breg);
  ebreg = cp_ebreg(breg);
  switch_from_trie_assert;	/* ensure table-trie space is default */
  remove_open_tables_reset_freezes();
  return cp_pcreg(breg); 
}

/* SIGSEGV/SIGBUS handler that catches segfaults; used unless 
   configured with DEBUG */ 
void xsb_segfault_catcher(int err)
{
  char *tmp_message = xsb_segfault_message;
  xsb_segfault_message = xsb_default_segfault_msg; /* restore default */
  longjmp(xsb_abort_fallback_environment,
	  (Integer) exception_handler(tmp_message));
}

void xsb_segfault_quitter(int err)
{
  xsb_exit(xsb_segfault_message);
}

#ifdef WIN_NT
/* Our separate thread */
void checkJavaInterrupt(void *info)
{
  char ch;
  SOCKET intSocket = (SOCKET)info;
  xsb_dbgmsg((LOG_DEBUG, "Thread started on socket %ld",(int)intSocket));
  while(1){
    if (1!=recv(intSocket,&ch,1,0)) {
      xsb_warn("Problem handling interrupt from Java");
    }
    else 
      xsb_mesg("--- Java interrupt detected");
    /* Avoid those annoying lags? */
    fflush(stdout);
    fflush(stderr);
    fflush(stdmsg);
    fflush(stdwarn);
    fflush(stddbg);
    keyint_proc(SIGINT); /* Do XSB's "interrupt" thing */
  }
}

xsbBool startInterruptThread(SOCKET intSocket)
{
  xsb_mesg("Beginning interrupt thread on socket %ld",(int)intSocket);
#ifdef _MT
  _beginthread( checkJavaInterrupt, 0, (void*)intSocket );
#endif
  return 1;
}
#endif
