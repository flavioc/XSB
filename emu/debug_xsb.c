/* File:      debug_xsb.c
** Author(s): Xu, Swift, Sagonas, Johnson, Freire
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

#include "auxlry.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "binding.h"
#include "psc_xsb.h"
#include "memory_xsb.h"
#include "flags_xsb.h"
#include "register.h"
#include "deref.h"
#include "trie_internals.h"
#include "choice.h"
#include "macro_xsb.h"
#include "inst_xsb.h"

#ifdef DEBUG
#include "subp.h"
#endif

/*----------------------------------------------------------------------*/

int call_step = 0;
int hitrace_suspend = 0;

#ifdef DEBUG
int pil_step = 1;
int compl_step = 0;
int debug_ctr = 0;
int print_hide = 0;
int memory_watch_flag = 0;
int register_watch_flag = 0;
#endif

/*----------------------------------------------------------------------*/

extern int  xctr;

/*----------------------------------------------------------------------*/

#ifdef DEBUG
#define STRIDESIZE     30

#define EOS	"--------------------BOTTOM_OF_STACK--------------------\n"
#define EOFR	"--------------------------------------------\n"
#define EOSUBG	"------------------------------------------------------------\n"

#define print_subg_header(SUBG) { \
    fprintf(stddbg, "=== Frame for "); print_subgoal(stddbg, SUBG); \
    if (is_completed(SUBG)) fprintf(stddbg, " (completed) ===\n"); \
    else fprintf(stddbg, " (incomplete) ===\n"); }
#endif

/*----------------------------------------------------------------------*/

static int count_producer_subgoals(void)
{
  int i;
  TIFptr tif;
  VariantSF temp_ptr;

  i = 0;
  for ( tif = tif_list.first;  IsNonNULL(tif);  tif = TIF_NextTIF(tif) )
    for ( temp_ptr = TIF_Subgoals(tif);  IsNonNULL(temp_ptr);
	  temp_ptr = (VariantSF)subg_next_subgoal(temp_ptr) )
      i ++;
  return(i);
}

/*======================================================================*/
/*======================================================================*/

#ifdef DEBUG
static void debug_interact(void);
#endif

/*======================================================================*/
/*  The following are possibly used both for tracing and by XSB		*/
/*  developers during debugging.					*/
/*======================================================================*/

CPtr decode_ptr(Cell cell) {
  return ( clref_val(cell) );
}


int decode_int(Cell cell) {
  return ( int_val(cell) );
}


int decode_tag(Cell cell) {
  return ( cell_tag(cell) );
}

/*----------------------------------------------------------------------*/

#define CAR		1
#define CDR		0

static void print_term(FILE *fp, Cell term, byte car, int level)
{
  unsigned short i, arity;
  Psc psc;
  CPtr cptr;

  level--;
  if (level < 0) {
    fprintf(fp, "...");
    return;
  }
  printderef(term);
  switch (cell_tag(term)) {
  case XSB_FREE:
  case XSB_REF1:
    fprintf(fp, "_%p", vptr(term));
    return;
  case XSB_ATTV:
    fprintf(fp, "_%p", (CPtr)dec_addr(term));
    return;
  case XSB_STRUCT:
    psc = get_str_psc(term);
    fprintf(fp, "%s", get_name(psc));
    arity = get_arity(psc);
    if ( arity == 0 )   /* constant */
      return;
    /* structure */
    fprintf(fp, "(");
    cptr = clref_val(term);
    for ( i = 1; i <= arity; i++ ) {
      print_term(fp, cell(cptr+i), CAR, level);
      if ( i < arity )
	fprintf(fp, ",");
    }
    fprintf(fp, ")");
    return;
  case XSB_STRING:
    fprintf(fp, "\"%s\"", string_val(term));
    break;
  case XSB_INT:
    fprintf(fp, "%ld", (long)int_val(term));
    return;
  case XSB_FLOAT:
    fprintf(fp, "%f", float_val(term));
    return;
  case XSB_LIST:
    cptr = clref_val(term);
    if ( car )
      fprintf(fp, "[");
    print_term(fp, cell(cptr), CAR, level);
    term = cell(cptr+1);
    XSB_Deref(term);
    switch (cell_tag(term)) {
    case XSB_FREE:
    case XSB_REF1: 
    case XSB_ATTV:
      goto vertbar;
    case XSB_LIST:
      fprintf(fp, ",");
      print_term(fp, term, CDR, level);
      return;
    case XSB_STRING:
      if (string_val(term) != nil_sym)
	goto vertbar;
      else {
	fprintf(fp, "]");
	return;
      }
    case XSB_STRUCT:
    case XSB_INT:
    case XSB_FLOAT:
    vertbar:
    fprintf(fp, "|");
    print_term(fp, term, CAR, level);
    fprintf(fp, "]");
    return;
    }
  }
}


void printterm(FILE *fp, Cell term, int depth) {

  print_term(fp, term, CAR, depth);
  fflush(fp); 
}

/*----------------------------------------------------------------------*/

static void print_call(Psc psc)
{
  int i, arity;

  arity = (int)get_arity(psc);
  fprintf(stddbg, "(w1) call: %s", get_name(psc));
  if (arity != 0) fprintf(stddbg, "(");
  for (i=1; i <= arity; i++) {
    printterm(stddbg, cell(reg+i), 3);
    if (i < arity) fprintf(stddbg, ",");
  }
  if (arity != 0) fprintf(stddbg, ")\n"); else fprintf(stddbg, "\n");
  fflush(stddbg);
}

/*----------------------------------------------------------------------*/

void debug_call(Psc psc)
{
  if (call_step || get_spy(psc)) {
    print_call(psc);
#ifdef DEBUG
    debug_interact();
#endif
  } else if (!hitrace_suspend) print_call(psc);
}

/*======================================================================*/
/*  The following till the end of file are used only by XSB developers	*/
/*  during internal system debugging.					*/
/*======================================================================*/

#ifdef DEBUG

extern void dis(xsbBool);
extern byte *print_inst(FILE *, byte *);

struct watch_struct {
  int  heap_flag;
  CPtr heap_val;
  int  stack_flag;
  CPtr stack_val;
  int  choice_flag;
  CPtr choice_val;
  int  trail_flag;
  CPtr trail_val;
} reg_watch, mem_watch;

/*======================================================================*/

void print_help(void)
{
  xsb_dbgmsg("      a r/v/d/a <addr>: inspect the content of the address");
  xsb_dbgmsg("      b <module> <name> <arity>: spy the predicate");
  xsb_dbgmsg("      B <num>: print detailed Prolog choice points from the top");
  xsb_dbgmsg("\tof the choice point stack with <num>-Cell overlap");
  xsb_dbgmsg("      c <num>: print top of choice point stack with <num> overlap");
#if (!defined(CHAT))
  xsb_dbgmsg("      C <num>: print choice point stack (around bfreg) with <num> overlap");
#endif
  xsb_dbgmsg("      d: print disassembled code for module");
  xsb_dbgmsg("      D: print current value of delay list (pointed by delayreg)");
  xsb_dbgmsg("      e <size>: expand trail/cp stack to <size> K-byte blocks");
  xsb_dbgmsg("      E <num>: print top of environment (local) stack with <num> overlap");
  xsb_dbgmsg("      g: leap to the next check_complete instruction");
  xsb_dbgmsg("      G: same as 'g', but does not print intermediate info");
  xsb_dbgmsg("      h: help");
  xsb_dbgmsg("      H <num>: print top of heap with <num> overlap");
  xsb_dbgmsg("      k <int>: print and skip <int> instructions");
  xsb_dbgmsg("      K <int>: skip <int> instructions");
  xsb_dbgmsg("      l: leap to the next spy point");
  xsb_dbgmsg("      L: same as 'l', but does not print intermediate info");
  xsb_dbgmsg("      M: print statistics");
  xsb_dbgmsg("      n: leap to the next call");
  xsb_dbgmsg("      N: nodebugging, continue to the end");
  xsb_dbgmsg("      o: print completion stack");
  xsb_dbgmsg("      P: print PDLSTK");
  xsb_dbgmsg("      q: quit XSB");
  xsb_dbgmsg("      r <num>: print register <num> as term");
  xsb_dbgmsg("      R <num>: print register <num> as ptr");
  xsb_dbgmsg("      s: step (execute a single instruction)");
  xsb_dbgmsg("      S: print status registers");
  xsb_dbgmsg("      T <num>: print top of trail with <num> overlap");
  xsb_dbgmsg("      u <name> <arity>: unspy the predicate");
  xsb_dbgmsg("      v <num>: print variable <num>");
  xsb_dbgmsg("      w <stack> <val>: watch <stack> register for <val>");
  xsb_dbgmsg("      W <stack> <val>: watch memory area of <stack> for <val>");
  xsb_dbgmsg("      x: exit XSB");
  xsb_dbgmsg("      1: print top of (persistent) subgoal stack");
  xsb_dbgmsg("      2 <num>: print val of table pointer");
  xsb_dbgmsg("      ?: help");
}

/*--------------------------------------------------------------------------*/

/*
 * Tries to make the interface more robust by cleaning-up any extra user
 * input supplied to a prompt.  Place a call to this function after any
 * input scan which doesn't take the whole input line (ie. which isn't a
 * `scanf("%s", &array);').
 */
static void skip_to_nl(void)
{
  char c;

  do {
    c = getchar();
  } while (c != '\n');
}

/*----------------------------------------------------------------------*/ 

static void set_register_watch(int num1, CPtr num2)
{
  register_watch_flag = 1;
  switch (num1) {
  case 1: 
    reg_watch.heap_flag = 1;
    reg_watch.heap_val = num2;
    break;
  case 2: 
    reg_watch.stack_flag = 1;
    reg_watch.stack_val = num2;
    break;
  case 3: 
    reg_watch.choice_flag = 1;
    reg_watch.choice_val = num2;
    break;
  case 4: 
    reg_watch.trail_flag = 1;
    reg_watch.trail_val = num2;
    break;
  }
}

/*----------------------------------------------------------------------*/ 

static void set_memory_watch(int num1, int num2)
{
  memory_watch_flag = 1;
  switch (num1) {
  case 1: 
    mem_watch.heap_flag = num2;
    mem_watch.heap_val = *(CPtr *) num2;
    break;
  case 2: 
    mem_watch.stack_flag = num2;
    mem_watch.stack_val = *(CPtr *) num2;
    break;
  case 3: 
    mem_watch.choice_flag = num2;
    mem_watch.choice_val = *(CPtr *) num2;
    break;
  case 4: 
    mem_watch.trail_flag = num2;
    mem_watch.trail_val = *(CPtr *) num2;
    break;
  }
}

/*----------------------------------------------------------------------*/

static void monitor_register_watch(void)
{
  if (reg_watch.heap_flag) 
    if (reg_watch.heap_val == hreg)
      xsb_dbgmsg("!!! hreg == %p, %d", hreg, xctr);
  if (reg_watch.stack_flag) 
    if (reg_watch.stack_val == ereg)
      xsb_dbgmsg("!!! ereg == %p, %d", ereg, xctr);
  if (reg_watch.choice_flag) 
    if (reg_watch.choice_val == breg)
      xsb_dbgmsg("!!! breg == %p, %d", breg, xctr);
  if (reg_watch.trail_flag) 
    if ((CPtr *) reg_watch.trail_val == trreg)
      xsb_dbgmsg("!!! trreg == %p, %d", trreg, xctr);
}

/*----------------------------------------------------------------------*/

static void monitor_memory_watch(void)
{
  if (mem_watch.heap_flag)
    if (*(CPtr *) mem_watch.heap_flag != mem_watch.heap_val) {
      xsb_dbgmsg("Heap watch val %x was %p is now %lx, xctr %d",
		 mem_watch.heap_flag, mem_watch.heap_val,
		 *(CPtr) mem_watch.heap_flag, xctr);
      debug_ctr = 0;
      mem_watch.heap_val = *(CPtr *) mem_watch.heap_flag;
    }
  if (mem_watch.stack_flag)
    if (*(CPtr *) mem_watch.stack_flag != mem_watch.stack_val) {
      xsb_dbgmsg("Stack watch val %x was %p is now %lx, xctr %d",
		 mem_watch.stack_flag,mem_watch.stack_val,
		 *(CPtr) mem_watch.stack_flag,xctr);
      debug_ctr = 0;
      mem_watch.stack_val = *(CPtr *) mem_watch.stack_flag;
    }
  if (mem_watch.choice_flag)
    if (*(CPtr *) mem_watch.choice_flag != mem_watch.choice_val) {
      xsb_dbgmsg("Choice watch val %x was %p is now %lx, xctr %d",
		 mem_watch.choice_flag,mem_watch.choice_val,
		 *(CPtr) mem_watch.choice_flag,xctr);
      debug_ctr = 0;
      mem_watch.choice_val = *(CPtr *) mem_watch.choice_flag;
    }
  if (mem_watch.trail_flag)
    if (*(CPtr *) mem_watch.trail_flag != mem_watch.trail_val) {
      xsb_dbgmsg("Trail watch val %x was %p is now %lx, xctr %d",
		 mem_watch.trail_flag,mem_watch.trail_val,
		 *(CPtr) mem_watch.trail_flag,xctr);
      debug_ctr = 0;
      mem_watch.trail_val = *(CPtr *) mem_watch.trail_flag;
    }
}

/*----- For table debugging --------------------------------------------*/ 

static Cell cell_array[500];

static void print_term_of_subgoal(FILE *fp, int *i)
{
  Cell term;
  int  j, args;

  term = cell_array[*i];
  switch (cell_tag(term)) {
  case XSB_TrieVar:
    fprintf(fp, "_v%d", (int_val(term) & 0xffff));
    break;
  case XSB_STRUCT:
    args = get_arity((Psc)cs_val(term));
    fprintf(fp, "%s", get_name((Psc)cs_val(term)));
    if (args > 0) fprintf(fp, "(");
    for (j = args; j > 0; j--) {
      (*i)--;
      print_term_of_subgoal(fp, i);
      if (j > 1) fprintf(fp, ",");
    }
    if (args > 0) fprintf(fp, ")");
    break;
  case XSB_LIST:	/* the list [a,b(1),c] is stored as . . . [] c b 1 a */
    /*
     * This is the old version, which does not work anymore.
     *
     * if (isnil(cell_array[(*i)-1])) {
     *   (*i) -= 2;
     *   print_term_of_subgoal(fp, i);
     *   fprintf(fp, "]");
     * } else xsb_error("Non-single element list in print_subgoal()");
     */
    fprintf(fp, "[");
    (*i)--;
    print_term_of_subgoal(fp, i);
    (*i)--;
    print_term_of_subgoal(fp, i);
    fprintf(fp, "]");
    break;
  case XSB_STRING:
    fprintf(fp, "%s", string_val(term));
    break;
  case XSB_INT:
    fprintf(fp, "%d", int_val(term));
    break;
  case XSB_FLOAT:
    fprintf(fp, "%.5g", float_val(term));
    break;
  default:
    xsb_error("Term with unknown tag (%d) in print_subgoal()",
	      (int)cell_tag(term));
    break;
  }
}

/*----------------------------------------------------------------------*/

void print_subgoal(FILE *fp, VariantSF subg)
{
  BTNptr leaf;
  int  i = 0;
  Psc  psc = TIF_PSC(subg_tif_ptr(subg));

  for (leaf = subg_leaf_ptr(subg); leaf != NULL; leaf = Parent(leaf)) {
    cell_array[i++] = BTN_Symbol(leaf);
  }
  fprintf(fp, "%s", get_name(psc));
  if (get_arity(psc) > 0) {
    fprintf(fp, "(");
    for (i = i-2; i >= 0 ; i--) {
      print_term_of_subgoal(fp, &i);
      if (i > 0) fprintf(fp, ", ");
    }
    fprintf(fp, ")");
  }
}

/*----------------------------------------------------------------------*/

static void print_delay_element(FILE *fp, Cell del_elem)
{
  Psc  psc = 0;
  CPtr cptr;
  int arity, i;
  Cell tmp_cell;
  char *name;

  if ((psc = get_str_psc(del_elem)) == delay_psc) {
    fprintf(fp, "%s(", get_name(psc));
    cptr = (CPtr)cs_val(del_elem);
    tmp_cell = cell(cptr + 1);
    print_subgoal(fp, (VariantSF) addr_val(tmp_cell)); fprintf(fp, ",");
    tmp_cell = cell(cptr + 2);
    fprintf(fp, "%p", (BTNptr) addr_val(tmp_cell)); fprintf(fp, ",");
    tmp_cell = cell(cptr + 3);
    if (isinteger(tmp_cell)) {
      fprintf(fp, "NEG");
    }
    else {
      if (isstring(tmp_cell)) {
	arity = 0;
	name = string_val(tmp_cell);
      }
      else {
	psc = get_str_psc(cell(cptr + 3));
	arity = get_arity(psc);
	name = get_name(psc);
      }
      fprintf(fp, "%s/%d(", name, arity);
      if (arity > 0) {
	cptr = (CPtr) cs_val(cell(cptr + 3));
	for (i = 0; i < arity; i++)
	  printterm(fp, cell(cptr + 1 + i), 25);
      }
    }
    fprintf(fp, ")");
  }
  else {
    xsb_abort("Unknown delay list element in print_delay_element()");
  }
}

/*----------------------------------------------------------------------*/

void print_delay_list(FILE *fp, CPtr dlist)
{
  CPtr cptr;
  
  if (dlist == NULL) {
    fprintf(fp, "[]"); fflush(fp);
  } else {
    if (islist(dlist) || isnil(dlist)) {
      fprintf(fp, "["); cptr = dlist;
      while (islist(cptr)) {
	cptr = clref_val(cptr);
	print_delay_element(fp, cell(cptr));
	cptr = (CPtr)cell(cptr+1);
	if (islist(cptr)) fprintf(fp, ", ");
      }
      if (isnil(cptr)) {
	fprintf(fp, "]"); fflush(fp);
      } else {
	xsb_abort("Delay list with unknown tail type in print_delay_list()");
      }
    } else {
      xsb_abort("Delay list with unknown type in print_delay_list()");
    }
  }
}

/*----------------------------------------------------------------------*/

void debug_inst(byte *lpcreg, CPtr le_reg)
{
  if (!print_hide) {
    fprintf(stddbg, "xctr %d ",xctr);
    print_inst(stddbg, lpcreg);
  }
  if (register_watch_flag) monitor_register_watch();
  if (memory_watch_flag) monitor_memory_watch();
  if (pil_step && debug_ctr == 0) {
    print_hide = 0;
    pcreg = lpcreg; ereg = le_reg;
    debug_interact();
  } else { 
    if (debug_ctr > 0) debug_ctr--;
    else 
      if (call_step == 1 && *lpcreg == call) {
	pil_step = 1; debug_interact();
      }
    if (compl_step == 1 && *lpcreg == check_complete) {
      pil_step = 1; debug_interact();
    }
  }
}

/*----------------------------------------------------------------------*/ 

static void print_cell(char *addrtype, CPtr addr, Cell term, char *more_info)
{
  switch (cell_tag(term)) {
  case XSB_REF:
  case XSB_REF1:
    fprintf(stddbg, "%s %p: XSB_REF (tag=%ld), value=0x%p",
	    addrtype, addr, cell_tag(term), ref_val(term));
    break;
  case XSB_ATTV:
    fprintf(stddbg, "%s %p: XSB_ATTV (tag=%ld), value=0x%p",
	    addrtype, (CPtr)dec_addr(cell(addr)),
	    cell_tag(term), ref_val(term));
    break;
  case XSB_STRUCT: 
    if (addr == (CPtr)dec_addr(term) || (CPtr)dec_addr(term) == NULL) {
      fprintf(stddbg, "Possible source of core dump\n");
      fprintf(stddbg, "%s %p: XSB_STRUCT, value=0x%p, hexval=0x%p", 
	      addrtype, addr, cs_val(term), ref_val(term));
    }	else {
      fprintf(stddbg, "%s %p: XSB_STRUCT, value=0x%p, hexval=0x%p (%s/%d)", 
	      addrtype, addr, cs_val(term), ref_val(term),
	      get_name((struct psc_rec *) follow(cs_val(term))),
	      get_arity((struct psc_rec *) follow(cs_val(term))));
    }
    break;
  case XSB_INT:
    fprintf(stddbg, "%s %p: XSB_INT, value=%d  hexval=0x%p",
	    addrtype, addr, int_val(term), ref_val(term));
    break;
  case XSB_STRING:
    if (isnil(term)) 
      fprintf(stddbg, "%s %p: XSB_STRING, hexval=0x%p\t ([])", 
	      addrtype, addr, ref_val(term));
    else
      fprintf(stddbg, "%s %p: XSB_STRING, hexval=0x%p\t (%s)", 
	      addrtype, addr, ref_val(term), string_val(term));
    break;
  case XSB_FLOAT:
    fprintf(stddbg, "%s %p: XSB_FLOAT, value=%f, hexval=0x%lx", 
	    addrtype, addr, float_val(term), dec_addr(term));
    break;
  case XSB_LIST:
    fprintf(stddbg, "%s %p: XSB_LIST, clref=%p, hex=%p",
	    addrtype, addr, clref_val(term), ref_val(term));
    break;
  default:
    fprintf(stddbg, "%s %p: tag=%ld, hex=0x%p, cval=%d", 
	    addrtype, addr, cell_tag(term), ref_val(term), int_val(term));
    break;
  }
  
  if (more_info != NULL)
    fprintf(stddbg, ",\t(%s)\n", more_info);
  else fprintf(stddbg, "\n");
}

/*----------------------------------------------------------------------*/ 

static void print_cp_cell(char *addrtype, CPtr addr, Cell term)
{
  if ((ref_val(term) != NULL) && (cell_tag(term) == term)) {
    fprintf(stddbg, "NULL cell in %s %p: tag=%ld, value=0x%p\n",
	    addrtype, addr, cell_tag(term), ref_val(term));
  } else {  
    switch (cell_tag(term)) {
    case XSB_REF: 
    case XSB_REF1:
      fprintf(stddbg, "%s %p: XSB_REF (tag=%ld), value=0x%p\n",
	      addrtype, addr, cell_tag(term), ref_val(term));
      break;
    case XSB_ATTV:
      fprintf(stddbg, "%s %p: XSB_ATTV (tag=%ld), value=0x%p\n",
	      addrtype, (CPtr)dec_addr(cell(addr)),
	      cell_tag(term), ref_val(term));
      break;
    case XSB_STRUCT:
      fprintf(stddbg, "%s %p: XSB_STRUCT, value=0x%p, hexval=0x%p (%s/%d)\n", 
	      addrtype, addr, cs_val(term), ref_val(term),
	      get_name((struct psc_rec *) follow(cs_val(term))),
	      get_arity((struct psc_rec *) follow(cs_val(term))));
      break;
    case XSB_INT:
      fprintf(stddbg, "%s %p: XSB_INT, value=%d, hexval=0x%p\n",
	      addrtype, addr, int_val(term), ref_val(term));
      break;
    case XSB_STRING:
      fprintf(stddbg, "%s %p: XSB_STRING, hexval=0x%p (%s)\n", 
	      addrtype, addr, ref_val(term), string_val(term));
      break;
    case XSB_FLOAT:
      fprintf(stddbg, "%s %p: XSB_FLOAT, value=%f, hexval=0x%lx\n", 
	      addrtype, addr, float_val(term), dec_addr(term));
      break;
    case XSB_LIST:
      fprintf(stddbg, "%s %p: XSB_LIST, value=%p\n",
	      addrtype, addr, ref_val(term));
      break;
    default:
      fprintf(stddbg, "%s %p: tag=%ld, value=0x%p\n", 
	      addrtype, addr, cell_tag(term), ref_val(term));
      break;
    }
  }
}

/*----------------------------------------------------------------------*/ 

/*
 * Local Stack grows from high to low memory.
 */
static void print_local_stack(int overlap)
{
  int i;
  CPtr cell_ptr,
    local_stack_bottom = (CPtr) glstack.high;
  char ans = 'y';

  if (ereg_on_top(ereg)) {
    cell_ptr = ereg;
    fprintf(stddbg, "ereg on top\n");
  }
  else {
    cell_ptr = ebreg;
    fprintf(stddbg, "ebreg on top\n");
  }
  for (i = -overlap; i < 0; i++) {
#if (!defined(CHAT))
    if ( cell_ptr+i == efreg ) fprintf(stddbg, "efreg\n");
#endif
    print_cp_cell("Local Stack", cell_ptr+i, cell(cell_ptr+i));
  }
  fprintf(stddbg, "top\n");
  do {
    for (i=0; (i < STRIDESIZE) && (cell_ptr < local_stack_bottom); i++) {
      if (cell_ptr == ebreg)
	fprintf(stddbg, "ebreg\n");
      if (cell_ptr == ereg)
	fprintf(stddbg, "ereg\n");
#if (!defined(CHAT))
      if (cell_ptr == efreg) fprintf(stddbg, "efreg\n");
#endif
      print_cp_cell("Local Stack", cell_ptr, cell(cell_ptr));
      cell_ptr++;
    }
    if (cell_ptr < local_stack_bottom) {
      fprintf(stddbg, "more (y/n)?  ");
      scanf("%c", &ans);
      skip_to_nl();
    }
    else {
      fprintf(stddbg, EOS);
      ans = 'n';
    }
  } while (ans == 'y');
}

/*----------------------------------------------------------------------*/ 

static void print_trail(int overlap)		/* trail grows up */
{
  int  i, offset=0;
  char ans = 'y';
  CPtr *temp;

#ifdef CHAT
  temp = trreg;
#else
  if (trfreg > trreg) temp = trfreg;  else temp = trreg;
#endif
  for (i = overlap; (i > 0); i--)
    {
      if ( (temp + i) == trreg ) xsb_dbgmsg("trreg");
#if (!defined(CHAT))
      if ( (temp + i) == trfreg ) xsb_dbgmsg("trfreg");
#endif
      print_cell("Trail", (CPtr)(temp+i), cell((CPtr)(temp+i)), NULL);
    }
  while (ans == 'y' && temp-offset >= (CPtr *) tcpstack.low) { 
    for (i = 0
	   ; (i <= STRIDESIZE && temp-(offset+i) >= (CPtr *)tcpstack.low)
	   ; i++ )      {
      if ( (temp - (offset+i)) == trreg ) xsb_dbgmsg("trreg");
#if (!defined(CHAT))
      if ( (temp - (offset+i)) == trfreg ) xsb_dbgmsg("trfreg");
#endif
      print_cell("Trail", (CPtr)(temp-(offset+i)),
		 cell((CPtr)(temp-(offset+i))), NULL);
      if ( (temp-(offset+i)) == (CPtr *) tcpstack.low ) 
	xsb_dbgmsg("bottom");
    }
    offset += STRIDESIZE;
    fprintf(stddbg, "more (y/n)?  ");
    scanf("%c", &ans);
    skip_to_nl();
  }
}

/*----------------------------------------------------------------------*/ 

#if (!defined(CHAT))

static void print_freeze_choice_points(int overlap)	/* CPs grow down */
{
  int i,last = 0;
  char ans = 'y';
 
  for (i = -overlap; i < 0 ; i++) {
    print_cp_cell("CP stack", bfreg+i, cell(bfreg+i));
    if ( (bfreg + i) == breg ) xsb_dbgmsg("breg");
  }
  xsb_dbgmsg("bfreg");
  for (i = 0; (i <= STRIDESIZE && bfreg+i<=(CPtr)tcpstack.high); i++){
    if ( (bfreg + i) == breg ) xsb_dbgmsg("breg");
    print_cp_cell("CP stack", bfreg+i, cell(bfreg+i));
    if ( (bfreg + i) == (CPtr) tcpstack.high ) fprintf(stddbg, EOS);
  }
  fprintf(stddbg, "more (y/n)?  ");
  scanf("%c", &ans);
  skip_to_nl();
  while (ans == 'y' && bfreg+last < (CPtr) tcpstack.high ) { 
    last = last+STRIDESIZE;
    for ( i = last
	    ; (i <= last+STRIDESIZE && bfreg+i <= (CPtr) tcpstack.high) 
	    ; i++ ) {
      if ( (bfreg + i) == breg ) xsb_dbgmsg("breg");
      print_cp_cell("CP stack", bfreg+i, cell(bfreg+i));
      if ( (bfreg + i) == (CPtr) tcpstack.high ) fprintf(stddbg, EOS);
    }
    fprintf(stddbg, "more (y/n)?  ");
    scanf("%c", &ans);
    skip_to_nl();
  }
}

#endif

/*----------------------------------------------------------------------*/ 

/*
 *  analyze choice point frame (in, out, out)
 */

static void analyze_cpf(CPtr cpf_addr, int *length, int *cpf_type)
{

  /*
   *  Tests to determine the type of choice point frame go here.
   */

  /* For now, we'll just assume that we have only standard CP Frames. */
  /* The very bottom of the CP stack contains a standard choice point frame
     whose dynamic link points to itself. */

  *cpf_type = STANDARD_CP_FRAME;
  if (cpf_addr == cp_prevbreg(cpf_addr))
    *length = CP_SIZE;
  else
    *length = cp_prevbreg(cpf_addr) - cpf_addr;
}

/*----------------------------------------------------------------------*/ 

/*
 *  print choice point frame (in, in, in)
 */

static void print_common_cpf_part(CPtr cpf_addr) {
  char *s = "   CP stack ";

  xsb_dbgmsg("%s%p:\tptr to next clause:\t0x%p", s,
	     &(cp_pcreg(cpf_addr)), cp_pcreg(cpf_addr));
  xsb_dbgmsg("%s%p:\tprev env cap (ebreg):\t0x%p", s,
	     &(cp_ebreg(cpf_addr)), cp_ebreg(cpf_addr));
  xsb_dbgmsg("%s%p:\ttop of heap:\t\t0x%p", s,
	     &(cp_hreg(cpf_addr)), cp_hreg(cpf_addr));
  xsb_dbgmsg("%s%p:\ttop of trail:\t\t0x%p", s,
	     &(cp_trreg(cpf_addr)), cp_trreg(cpf_addr));
  xsb_dbgmsg("%s%p:\tcontinuation pointer:\t0x%p", s,
	     &(cp_cpreg(cpf_addr)), cp_cpreg(cpf_addr));
  xsb_dbgmsg("%s%p:\ttop of local stack:\t0x%p", s,
	     &(cp_ereg(cpf_addr)), cp_ereg(cpf_addr));
  xsb_dbgmsg("%s%p:\tdynamic link:\t\t0x%p", s,
	     &(cp_prevbreg(cpf_addr)), cp_prevbreg(cpf_addr));
  xsb_dbgmsg("%s%p:\tparent subgoal dreg:\t0x%p", s,
	     &(cp_pdreg(cpf_addr)), cp_pdreg(cpf_addr));
}

static void print_cpf(CPtr cpf_addr, int length, int cpf_type) {

  CPtr arg;
  int i, num_of_args;
  char *s = "   CP stack ";

  switch (cpf_type) {
  case STANDARD_CP_FRAME:
    xsb_dbgmsg("Standard Choice Point Frame:");
    print_common_cpf_part(cpf_addr);

    num_of_args = length - CP_SIZE;
    for (i = 1, arg = cpf_addr + CP_SIZE; i <= num_of_args; i++, arg++)
      xsb_dbgmsg("%s%p:\tpredicate arg #%d:\t0x%p",
		 s, arg, i, ref_val(*arg));
    break;
  case GENERATOR_CP_FRAME:
    xsb_dbgmsg("Generator Choice Point Frame:");
    print_common_cpf_part(cpf_addr);
    xsb_dbgmsg("%s%p:\tparent tabled CP:\t0x%p", s,
	       &(tcp_ptcp(cpf_addr)), tcp_ptcp(cpf_addr));
    xsb_dbgmsg("%s%p:\tsubgoal frame ptr:\t0x%p", s,
	       &(tcp_subgoal_ptr(cpf_addr)), tcp_subgoal_ptr(cpf_addr));
#if (!defined(CHAT))
    xsb_dbgmsg("%s%p:\tCh P  freeze register:\t0x%p", s,
	       &(tcp_bfreg(cpf_addr)), tcp_bfreg(cpf_addr));
    xsb_dbgmsg("%s%p:\tHeap  freeze register:\t0x%p", s,
	       &(tcp_hfreg(cpf_addr)), tcp_hfreg(cpf_addr));
    xsb_dbgmsg("%s%p:\tTrail freeze register:\t0x%p", s,
	       &(tcp_trfreg(cpf_addr)), tcp_trfreg(cpf_addr));
    xsb_dbgmsg("%s%p:\tLo St freeze register:\t0x%p", s,
	       &(tcp_efreg(cpf_addr)), tcp_efreg(cpf_addr));
#ifdef LOCAL_EVAL
    xsb_dbgmsg("%s%p:\ttag used for local eval:\t0x%d", s,
	       &(tcp_tag(cpf_addr)), tcp_tag(cpf_addr));
    xsb_dbgmsg("%s%p:\tlocal eval trie_return:\t0x%p", s,
	       &(tcp_trie_return(cpf_addr)), tcp_trie_return(cpf_addr));
    xsb_dbgmsg("%s%p:\tsubst factor arity:\t0x%d", s,
	       &(tcp_arity(cpf_addr)), tcp_arity(cpf_addr));
#endif
#endif
    num_of_args = length - TCP_SIZE;
    for (i = 1, arg = cpf_addr + TCP_SIZE; i <= num_of_args; i++, arg++)
      xsb_dbgmsg("%s%p:\tpredicate arg #%d:\t0x%p",
		 s, arg, i, ref_val(*arg));
    break;
  default:
    xsb_error("CP Type %d not handled yet...", cpf_type);
    break;
  }
}

/*----------------------------------------------------------------------*/ 

/*
 * Choice point stack grows from high to low memory.
 */

static void print_cpfs(int overlap)
{
  int i, frames = 2;
  char ans = 'y';
  CPtr  cpf,            /* ptr to choice point frame */
    cp_stack_bottom = (CPtr) tcpstack.high;
  int  length,          /* needed to increment the stack pointer, hence
                           analyze_cpf must supply it */
    type;

  for (i = -overlap ; (i < 0) ; i++) {
#if (!defined(CHAT))
    if ((breg+i) == bfreg) xsb_dbgmsg("bfreg");
#endif
    print_cp_cell("   CP stack", breg+i, cell(breg+i));
  }
  xsb_dbgmsg("breg");
  cpf = breg;
  do {
    for (i = 0; (i < frames) && (cpf < cp_stack_bottom); i++) {
#if (!defined(CHAT))
      if ( cpf == bfreg )
	xsb_dbgmsg("bfreg");
#endif
      analyze_cpf(cpf, &length, &type);
      print_cpf(cpf, length, type);
      cpf = cpf + length;
    }
    if (cpf < cp_stack_bottom) {
      fprintf(stddbg, "more (y/n)?  ");
      scanf("%c", &ans);
      skip_to_nl();
    }
    else {
      fprintf(stddbg, EOS);
      ans = 'n';
    }
  } while (ans == 'y');
}

/*----------------------------------------------------------------------*/ 

static void print_choice_points(int overlap)
{
  int i, last = 0;
  char ans = 'y';
  CPtr cp_stack_bottom = (CPtr)tcpstack.high;
 
  for (i = -overlap ; (i < 0) ; i++) {
#if (!defined(CHAT))
    if ((breg+i) == bfreg) xsb_dbgmsg("bfreg");
#endif
    print_cp_cell("CP stack", breg+i, cell(breg+i));
  }
  xsb_dbgmsg("breg");
  do {
    for (i = last;
	 (i <= last + STRIDESIZE) && (breg+i <= cp_stack_bottom);
	 i++) {
#if (!defined(CHAT))
      if ( (breg + i) == bfreg ) xsb_dbgmsg("bfreg");
#endif
      print_cp_cell("CP stack", breg+i, cell(breg+i));
      if ( (breg + i) == cp_stack_bottom ) fprintf(stddbg, EOS);
    }
    if (breg+i < cp_stack_bottom) {
      last = last + STRIDESIZE;
      fprintf(stddbg, "more (y/n)?  ");
      scanf("%c", &ans);
      skip_to_nl();
    }
    else
      ans = 'n';
  } while (ans == 'y');
}

/*----------------------------------------------------------------------*/ 

/* Needs to change when new xwam stacks are introduced.  */
static void print_heap(int overlap)	/* Heap grows up */
{
  int i, offset = 0;
  char ans = 'y';

  for (i = overlap; (i > 0); i--) {
    print_cell("Heap", hreg+i, cell(hreg+i), NULL);
  }
  xsb_dbgmsg("hreg");
  while (ans == 'y' && hreg-i > (CPtr) glstack.low) {
    for (i = 0
	   ;(i <= STRIDESIZE && hreg-(offset+i) >= (CPtr) glstack.low) 
	   ; i++) {
#if (!defined(CHAT))
      if ( (hreg - (offset+i)) == hfreg ) xsb_dbgmsg("hfreg");
#endif
      if ( (hreg - (offset+i)) == hbreg ) xsb_dbgmsg("hbreg");
      print_cell("Heap", hreg-(offset+i), cell(hreg-(offset+i)), NULL);
      if ( (hreg-(offset+i)) == (CPtr) glstack.low ) 
	xsb_dbgmsg("bottom");
    }
    if ( (hreg-(offset+i)) != (CPtr) glstack.low ) {
      offset += STRIDESIZE;
      fprintf(stddbg, "more (y/n)?  ");
      scanf("%c", &ans);
      skip_to_nl();
    }
  }
}

/*----- For table debugging --------------------------------------------*/ 

static char *compl_stk_frame_field[] = {
  "subgoal_ptr", "level_num",
#ifdef CHAT
  "tcp-hreg", "tcp-pdreg", "tcp-ptcp", "cons_copy_list",
#endif
  "del_ret_list", "visited", "DG_edges", "DGT_edges"
};

/*----------------------------------------------------------------------*/

void print_completion_stack(void)
{
  int i = 0;
  EPtr eptr;
  VariantSF subg;
  CPtr temp = openreg;

  fprintf(stddbg,"openreg -> ");
  while (temp < COMPLSTACKBOTTOM) {
    if ((i % COMPLFRAMESIZE) == 0) {
      fprintf(stddbg,EOFR);	/* end of frame */
      subg = (VariantSF) *temp;
      print_subg_header(subg);
    }
    fprintf(stddbg,"Completion Stack %p: %lx\t(%s)",
	    temp, *temp, compl_stk_frame_field[(i % COMPLFRAMESIZE)]);
    if ((i % COMPLFRAMESIZE) >= COMPLFRAMESIZE-2) {
      for (eptr = (EPtr)*temp; eptr != NULL; eptr = next_edge(eptr)) {
	fprintf(stddbg," --> %p", edge_to_node(eptr));
      }
    }
    fprintf(stddbg,"\n");
    temp++; i++;
  }
  fprintf(stddbg, EOS);
}

/*----------------------------------------------------------------------*/

static void print_pdlstack(void)
{
  CPtr temp = pdlreg;

  while (temp <= (CPtr)(pdl.high) - 1) {
    xsb_dbgmsg("pdlstk %p: %lx", temp, *temp);
    temp++;
  }
}
 
/*-------------------------------------------------------------------------*/

/*
 *			TableInfoFrame Printing
 *			-----------------------
 */

char *stringTabledEvalMethod(TabledEvalMethod method) {

  switch(method) {
  case VARIANT_TEM:
    return ("variant");
    break;
  case SUBSUMPTIVE_TEM:
    return ("subsumption");
    break;
  default:
    return ("unknown");
    break;
  }
}

/*
 * Run the doubly-linked list of Subgoal Frames to the end, then back to
 * the beginning -- which may lie beyond `dll' -- counting the number of
 * frames encountered in each direction.
 */

void subg_dll_length(VariantSF dll, counter *forward, counter *back) {

  VariantSF cur, prev;
  counter f, b;

  /* Count the number of frames on the chain from `dll' forward. */
  f = 0;
  for ( prev = NULL, cur = dll;
	IsNonNULL(cur);
	prev = cur, cur = subg_next_subgoal(cur) )
    f++;

  /* Count the number of frames on the chain from the end to the beginning */
  b = 0;
  for ( cur = prev;  IsNonNULL(cur);  cur = subg_prev_subgoal(cur) )
    b++;

  *forward = f;
  *back = b;
}


void printTIF(TIFptr tif) {

  counter forward, back;

  printf("TableInfoFrame  %p\n"
	 "{ psc_ptr = %p  (%s/%d)\n"
	 "  method = %s\n"
	 "  call_trie = %p\n"
	 "  subgoals = %p  ",
	 tif,
	 TIF_PSC(tif), get_name(TIF_PSC(tif)), get_arity(TIF_PSC(tif)),
	 stringTabledEvalMethod(TIF_EvalMethod(tif)),
	 TIF_CallTrie(tif),
	 TIF_Subgoals(tif));
  subg_dll_length(TIF_Subgoals(tif),&forward,&back);
  if ( forward == back )
    printf("(%d total)", forward);
  else
    printf("(chain length mismatch: %d forward, %d back)", forward, back);
  printf("\n  next_tif = %p }\n", TIF_NextTIF(tif));
}

/*----------------------------------------------------------------------*/ 

/*
 *			Subgoal Frame Printing
 *			----------------------
 */

char *stringSubgoalFrameType(byte type) {

  switch(type) {
  case VARIANT_PRODUCER_SFT:
    return("variant");
    break;
  case SUBSUMPTIVE_PRODUCER_SFT:
    return("subsumptive producer");
    break;
  case SUBSUMED_CONSUMER_SFT:
    return("subsumptive consumer");
    break;
  default:
    return("unknown");
    break;
  }
}

void print_tables(void)
{
  int i = 0;
  char ans = 'y';
  TIFptr tif;
  VariantSF subg;
  SubConsSF cons;

  i = count_producer_subgoals();
  xsb_dbgmsg("\t There are %d producer subgoal structures...", i);

  i = 0;
  for ( tif = tif_list.first;  IsNonNULL(tif) && (ans == 'y');
	tif = TIF_NextTIF(tif) ) {
    fprintf(stddbg,EOSUBG);
    printTIF(tif);
    subg = TIF_Subgoals(tif);
    while ( IsNonNULL(subg) && (ans == 'y') ) {
      i++;
      print_subg_header(subg);
      fprintf(stddbg, "%p:\n", subg);
      xsb_dbgmsg("  sf_type = %s,  is_complete = %s,  is_reclaimed = %s,",
		 stringSubgoalFrameType(subg_sf_type(subg)),
		 (subg_is_complete(subg) ? "YES" : "NO"),
		 (subg_is_reclaimed(subg) ? "YES" : "NO"));
      xsb_dbgmsg("  tif_ptr = %p,  leaf_ptr = %p,  ans_root_ptr = %p,\n"
		 "  ans_list_ptr = %p,   ans_list_tail = %p,\n"
		 "  next_subgoal = %p,  prev_subgoal = %p,  cp_ptr = %p",
		 subg_tif_ptr(subg), subg_leaf_ptr(subg),
		 subg_ans_root_ptr(subg),
		 subg_ans_list_ptr(subg), subg_ans_list_tail(subg),
		 subg_next_subgoal(subg), subg_prev_subgoal(subg), 
		 subg_cp_ptr(subg));
#ifndef CHAT
      xsb_dbgmsg("  asf_list_ptr = %p,", subg_asf_list_ptr(subg));
#endif
      xsb_dbgmsg("  compl_stk_ptr = %p,  compl_susp_ptr = %p,"
		 "  nde_list = %p",
		 subg_compl_stack_ptr(subg), subg_compl_susp_ptr(subg),
		 subg_nde_list(subg));
      if ( IsSubProdSF(subg) ) {
	xsb_dbgmsg("  consumers = %p", subg_consumers(subg));
	for ( cons = subg_consumers(subg);  IsNonNULL(cons);
	      cons = conssf_consumers(cons) )
	  xsb_dbgmsg("Consumer  %p\n"
		     "  sf_type = %11s,  tif_ptr = %p,         leaf_ptr = %p\n"
		     "  producer = %10p,  ans_list_ptr = %p,"
		     "  ans_list_tail = %p\n"
		     "  ts = %ul,  consumers = %p",
		     cons, subg_sf_type(cons), subg_tif_ptr(cons),
		     subg_leaf_ptr(cons), conssf_producer(cons),
		     subg_ans_list_ptr(cons), subg_ans_list_tail(cons),
		     conssf_timestamp(cons), conssf_consumers(cons));
      }
      subg = subg_next_subgoal(subg);
      if (subg != NULL)
	fprintf(stddbg, EOSUBG);
      if (i == 10) {
	fprintf(stddbg, "more (y/n)?  ");
	scanf("%c", &ans);
	skip_to_nl();
	i = 0;
      }
    }
  }
  fprintf(stddbg, EOS);
}

/*----------------------------------------------------------------------*/ 

static void print_status(void)
{
  xsb_dbgmsg("     ereg: 0x%p", ereg);
  xsb_dbgmsg("    ebreg: 0x%p", ebreg);
  xsb_dbgmsg("     breg: 0x%p", breg);
  xsb_dbgmsg("     hreg: 0x%p", hreg);
  xsb_dbgmsg("    hbreg: 0x%p", hbreg);
  xsb_dbgmsg("    trreg: 0x%p", trreg);
  xsb_dbgmsg("    cpreg: 0x%p", cpreg);
  xsb_dbgmsg("    pcreg: 0x%p", pcreg);

#if (!defined(CHAT))
  xsb_dbgmsg("    efreg: 0x%p", efreg);
  xsb_dbgmsg("    bfreg: 0x%p", bfreg);
  xsb_dbgmsg("    hfreg: 0x%p", hfreg);
  xsb_dbgmsg("   trfreg: 0x%p", trfreg);
#endif
  xsb_dbgmsg("   pdlreg: 0x%p", pdlreg);
  xsb_dbgmsg("  ptcpreg: 0x%p", ptcpreg);
  xsb_dbgmsg(" delayreg: 0x%p", delayreg);
  xsb_dbgmsg("neg_delay: %s", (neg_delay == FALSE) ? "False" : "True");
  xsb_dbgmsg("   level#: %d", level_num);

  xsb_dbgmsg("\nPDL");
  xsb_dbgmsg("\tlow:       %p", pdl.low);
  xsb_dbgmsg("\thigh:      %p", pdl.high);
  xsb_dbgmsg("\tsize:      %ld", pdl.size); /* JF: long */
  xsb_dbgmsg("\tinit size: %ld", pdl.init_size); /* JF: long */

  xsb_dbgmsg("\nGlobal / Local Stack");
  xsb_dbgmsg("\tlow:       %p", glstack.low);
  xsb_dbgmsg("\thigh:      %p", glstack.high);
  xsb_dbgmsg("\tsize:      %ld", glstack.size); /* JF: long */
  xsb_dbgmsg("\tinit size: %ld", glstack.init_size); /* JF: long */

  xsb_dbgmsg("\nTrail / Choice Point Stack");
  xsb_dbgmsg("\tlow:       %p", tcpstack.low);
  xsb_dbgmsg("\thigh:      %p", tcpstack.high);
  xsb_dbgmsg("\tsize:      %ld", tcpstack.size); /* JF: long */
  xsb_dbgmsg("\tinit size: %ld", tcpstack.init_size); /* JF: long */

  xsb_dbgmsg("\nCompletion Stack");
  xsb_dbgmsg("\tlow:       %p", complstack.low);
  xsb_dbgmsg("\thigh:      %p", complstack.high);
  xsb_dbgmsg("\tsize:      %ld", complstack.size); /* JF: long */
  xsb_dbgmsg("\tinit size: %ld", complstack.init_size); /* JF: long */
}

/*----------------------------------------------------------------------*/ 

void pofsprint(CPtr base, int arity)
{     
  CPtr arg_ptr = base;

  fprintf(stddbg, "( ");
  for (arg_ptr = base - 1; arg_ptr >= base - arity; arg_ptr--) {
    printterm(stddbg, (Cell)arg_ptr, 8);
    if (arg_ptr != base - arity)
      fprintf(stddbg, ",");
  }
  fprintf(stddbg, ")\n");
}

/*----------------------------------------------------------------------*/ 

static void debug_interact(void)
{
  char command, mod[32], name[32];
  int num, num1;
  Pair sym;

 again:
  fprintf(stddbg, "\n > ");  
  fflush(stddbg);
  scanf("%c", &command);
  switch (command) {
  case 'a':
    scanf("%s %x", name, &num);
    skip_to_nl();
    switch (name[0]) {
    case 'a':
      xsb_dbgmsg("0x%x: 0x%x", num, *(int *)num);
      break;
    case 'r':
      print_cell("Reg", (CPtr)num, cell(reg+num), NULL);
      break;
    case 'v':
      print_cell("Var", (CPtr)num, cell(ereg-num), NULL);
      break;
    case 'd':
      print_cell("Addr", (CPtr)num, cell((CPtr)(num)), NULL);
      break;
    }
    goto again;
  case 'b':
    scanf("%s %s %d", mod, name, &num);
    skip_to_nl();
    sym = insert_module(0, mod);
    sym = insert(name, num, sym->psc_ptr, &num);
    set_spy(sym->psc_ptr, 0x80);
    goto again;
  case 'B':
    scanf("%d", &num);
    skip_to_nl();
    print_cpfs(num);
    goto again;
  case 'c':
    scanf("%d", &num);
    skip_to_nl();
    print_choice_points(num);
    goto again;
#if (!defined(CHAT))
  case 'C':
    scanf("%d", &num);
    skip_to_nl();
    print_freeze_choice_points(num);
    skip_to_nl();
    goto again;
#endif
  case 'd':
    skip_to_nl();
    dis(1);
    goto again;
  case 'D':
    skip_to_nl();
    fprintf(stddbg, "Delay List = ");
    print_delay_list(stddbg, delayreg);
    fprintf(stddbg, "\n");
    goto again;
  case 'e':
    scanf("%d", &num);
    skip_to_nl();
    tcpstack_realloc(num);
    goto again;
  case 'E':
    scanf("%d", &num);
    skip_to_nl();
    print_local_stack(num);
    goto again;
  case 'g':
    skip_to_nl();
    pil_step = hitrace_suspend = call_step = 0;
    compl_step = 1;
    break;
  case 'G':
    skip_to_nl();
    print_hide = hitrace_suspend = compl_step = 1;
    pil_step = call_step = 0;
    break;
  case 'h':
  case '?':
    skip_to_nl();
    print_help();
    goto again;
  case 'H':
    scanf("%d", &num);
    skip_to_nl();
    print_heap(num);
    goto again;
  case 'k':
    scanf("%d", &num);
    skip_to_nl();
    debug_ctr = num;
    flags[PIL_TRACE] = 1;
    break;
  case 'K':
    scanf("%d", &num);
    skip_to_nl();
    debug_ctr = num;
    print_hide = flags[PIL_TRACE] = 1;
    break;
  case 'l':
    skip_to_nl();
    pil_step = call_step = hitrace_suspend = 0;
    break;
  case 'L':
    skip_to_nl();
    pil_step = flags[PIL_TRACE] = call_step = 0; 
    print_hide = hitrace_suspend = 1;
    break;
  case 'M':
    skip_to_nl();
    print_statistics(1);
    goto again;
  case 'n':
    skip_to_nl();
    pil_step = hitrace_suspend = 0;
    call_step = 1;
    break;
  case 'N':
    skip_to_nl();
    pil_step = flags[PIL_TRACE] = flags[HITRACE] = call_step = 0;
    print_hide = 1;
    break;
  case 'o':
    skip_to_nl();
    print_completion_stack();
    goto again;
  case 'P':
    skip_to_nl();
    print_pdlstack();
    goto again;
  case 'q':
  case 'x':
    xsb_exit("Debugging aborted by the user");
    break;
  case 'r':
    scanf("%d", &num);
    skip_to_nl();
    fprintf(stddbg, "Reg[%d] = ", num);
    printterm(stddbg, cell(reg+num), 8);
    fprintf(stddbg, "\n"); 
    fprintf(stddbg, "%lx\n",*(reg+num));
    goto again;
  case 'R':
    scanf("%d", &num);
    skip_to_nl(); 
    fprintf(stddbg, "Reg[%d] = %lx\n",num,*(reg+num));
    goto again;
  case 's':
    skip_to_nl();
    pil_step = 1;
    flags[PIL_TRACE] = 1;
    hitrace_suspend = 0;
    break;
  case 'S':
    skip_to_nl(); 
    print_status();
    goto again;
  case 'T': 
    scanf("%d", &num);
    skip_to_nl();
    print_trail(num);
    goto again;
  case 'u':
    scanf("%s %s %d", mod, name, &num);
    skip_to_nl();
    sym = insert_module(0, mod);
    sym = insert(name, num, sym->psc_ptr, &num);
    set_spy(sym->psc_ptr, 0x00);
    goto again;
  case 'v':
    scanf("%d", &num);
    skip_to_nl();
    fprintf(stddbg, "Var[%d] = ", num);
    printterm(stddbg, cell(ereg-num), 8);
    fprintf(stddbg, "\n");
    goto again;
  case 'w':
    scanf("%d %x", &num1, &num);
    skip_to_nl();
    set_register_watch(num1, (CPtr)num);
    goto again;      
  case 'W':
    scanf("%x %x", &num1, &num);
    skip_to_nl();
    set_memory_watch(num1, num);
    goto again;      
  case '1':
    skip_to_nl();
    print_tables();
    goto again;
  case '2':
    scanf("%d",&num);
    skip_to_nl();
    fprintf(stddbg, "tabptr: 0x%p tabptrval: 0x%lx\n",
	    ((CPtr) (pdl.low)) + num,
	    *(((CPtr) (pdl.low)) + num));
    goto again;
  case '\n':
    break;
  default:
    skip_to_nl();
    fprintf(stddbg, "Unknown command\n");
    goto again;
  }
  return;
}


#endif	/* DEBUG */
