/* File:      debug.c
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


#include <stdio.h>

#include "configs/config.h"
#include "debugs/debug.h"

#include "auxlry.h"
#include "cell.h"
#include "xsberror.h"
#include "binding.h"
#include "psc.h"
#include "memory.h"
#include "flags.h"
#include "register.h"
#include "deref.h"
#include "tries.h"
#include "choice.h"
#include "xmacro.h"
#include "inst.h"

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

#define CAR		1
#define CDR		0

#define outfile stdout

#ifdef DEBUG
#define STRIDESIZE     30
#define SG_STRIDESIZE	7*CALLSTRUCTSIZE

#define EOS	"--------------------BOTTOM_OF_STACK--------------------\n"
#define EOFR	"--------------------------------------------\n"
#define EOSUBG	"------------------------------------------------------------\n"

#define print_subg_header(SUBG) { \
    fprintf(outfile, "=== Frame for "); print_subgoal(outfile, SUBG); \
    if (is_completed(SUBG)) fprintf(outfile, " (completed) ===\n"); \
    else fprintf(outfile, " (incomplete) ===\n"); }
#endif

/*----------------------------------------------------------------------*/

int count_subgoals(void)
{
  int i;
  SGFrame temp_ptr;

  i = 0;
  temp_ptr = subg_structure_list;
  while(temp_ptr != NULL){
    i ++;
    temp_ptr = subg_next_subgoal(temp_ptr);
  }
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

void printterm(Cell term, byte car, int level)
{
    unsigned short i, arity1;
    struct psc_rec *psc_ptr1;
    CPtr cptr1;

    if (level-- < 0) { fprintf(outfile, "..."); return; }
    printderef(term);
    switch (cell_tag(term)) {
      case FREE:  case REF1:
	fprintf(outfile, "_%p", vptr(term));
	return;
      case CS:
	psc_ptr1 = get_str_psc(term);
	fflush(outfile); 
	fprintf(outfile, "%s", get_name(psc_ptr1));
	if ( get_arity(psc_ptr1) == 0 ) return;   /* constant */
	/* structure */
	fprintf(outfile, "(");
	arity1 = get_arity(psc_ptr1);
	    cptr1 = (CPtr)cs_val(term);
	    for ( i = 1; i <= arity1; i++ ) {
		printterm(cell(cptr1+i), CAR, level);
                if (i<arity1) fprintf(outfile, ",");
	    }
	    fprintf(outfile, ")");
            /* fflush(outfile); */
            return;
      case STRING:
	fprintf(outfile, "\"%s\"", string_val(term));
	break;
      case INT:
	fprintf(outfile, "%d", int_val(term));
	return;
      case FLOAT:
	fprintf(outfile, "%f", float_val(term));
	return;
      case LIST:
	cptr1 = clref_val(term);
	if ( car ) fprintf(outfile, "[");
	printterm(cell(cptr1), CAR, level);
	term = cell(cptr1+1);
	free_deref(term);
	switch (cell_tag(term)) {
	        case FREE:
	        case REF1: 
	            goto vertbar;
                case LIST:
		    fprintf(outfile, ",");
		    printterm(term, CDR, level);
		    return;
	        case STRING:
		    if (string_val(term) != nil_sym) goto vertbar;
		    else { fprintf(outfile, "]"); return; }
                case CS:
	        case INT:
		case FLOAT:
		    vertbar:
		    fprintf(outfile, "|");
		    printterm(term, CAR, level);
		    fprintf(outfile, "]");
		    /* fflush(outfile); */
		    return;
	}
    }
}

static void print_call(Psc psc)
{
  int i, arity;

  arity = (int)get_arity(psc);
  fprintf(outfile, "(w1) call: %s", get_name(psc));
  if (arity != 0) fprintf(outfile, "(");
  for (i=1; i <= arity; i++) {
    printterm(cell(reg+i), 1, 3);
    if (i < arity) fprintf(outfile, ",");
  }
  if (arity != 0) fprintf(outfile, ")\n"); else fprintf(outfile, "\n");
  fflush(outfile);
}

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

extern void dis(bool);
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
  printf("      a r/v/d/a <addr>: inspect the content of the address\n");
  printf("      b <module> <name> <arity>: spy the predicate\n");
  printf("      B <num>: print detailed Prolog choice points from the top\n");
  printf("\tof the choice point stack with <num>-Cell overlap\n");
  printf("      c <num>: print top of choice point stack with <num> overlap\n");
  printf("      C <num>: print choice point stack (around bfreg) with <num> overlap\n");
  printf("      d: print disassembled code for module\n");
  printf("      D: print current value of delay list (pointed by delayreg)\n");
  printf("      e <size>: expand trail/cp stack to <size> K-byte blocks\n");
  printf("      E <num>: print top of environment (local) stack with <num> overlap\n");
  printf("      g: leap to the next check_complete instruction\n");
  printf("      G: same as 'g', but does not print intermediate info\n");
  printf("      h: help\n");
  printf("      H <num>: print top of heap with <num> overlap\n");
  printf("      k <int>: print and skip <int> instructions\n");
  printf("      K <int>: skip <int> instructions\n");
  printf("      l: leap to the next spy point\n");
  printf("      L: same as 'l', but does not print intermediate info\n");
  printf("      M: print statistics\n");
  printf("      n: leap to the next call\n");
  printf("      N: nodebugging, continue to the end\n");
  printf("      o: print completion stack\n");
  printf("      P: print PDLSTK\n");
  printf("      q: quit XSB\n");
  printf("      r <num>: print register <num> as term\n");
  printf("      R <num>: print register <num> as ptr\n");
  printf("      s: step (execute a single instruction)\n");
  printf("      S: print status registers\n");
  printf("      T <num>: print top of trail with <num> overlap\n");
  printf("      u <name> <arity>: unspy the predicate\n");
  printf("      v <num>: print variable <num>\n");
  printf("      w <stack> <val>: watch <stack> register for <val>\n");
  printf("      W <stack> <val>: watch memory area of <stack> for <val>\n");
  printf("      x: exit XSB\n");
  printf("      1: print top of (persistent) subgoal stack\n");
  printf("      2 <num>: print val of table pointer\n");
  printf("      ?: help\n");
}

/*--------------------------------------------------------------------------*/

/*
 * Tries to make the interface more robust by cleaning-up any extra user
 * input supplied to a prompt.  Place a call to this function after any
 * input scan which doesn't take the whole input line (ie. which isn't a
 * `scanf("%s", &array);').
 */
void skip_to_nl(void)
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
      printf("!!! hreg == %p, %d\n", hreg, xctr);
  if (reg_watch.stack_flag) 
    if (reg_watch.stack_val == ereg)
      printf("!!! ereg == %p, %d\n", ereg, xctr);
  if (reg_watch.choice_flag) 
    if (reg_watch.choice_val == breg)
      printf("!!! breg == %p, %d\n", breg, xctr);
  if (reg_watch.trail_flag) 
    if ((CPtr *) reg_watch.trail_val == trreg)
      printf("!!! trreg == %p, %d\n", trreg, xctr);
}

/*----------------------------------------------------------------------*/

static void monitor_memory_watch(void)
{
  if (mem_watch.heap_flag)
    if (*(CPtr *) mem_watch.heap_flag != mem_watch.heap_val) {
      printf("Heap watch val %x was %p is now %lx, xctr %d\n",
	     mem_watch.heap_flag, mem_watch.heap_val,
	     *(CPtr) mem_watch.heap_flag, xctr);
      debug_ctr = 0;
      mem_watch.heap_val = *(CPtr *) mem_watch.heap_flag;
    }
  if (mem_watch.stack_flag)
    if (*(CPtr *) mem_watch.stack_flag != mem_watch.stack_val) {
      printf("Stack watch val %x was %p is now %lx, xctr %d\n",
	     mem_watch.stack_flag,mem_watch.stack_val,
	     *(CPtr) mem_watch.stack_flag,xctr);
      debug_ctr = 0;
      mem_watch.stack_val = *(CPtr *) mem_watch.stack_flag;
    }
  if (mem_watch.choice_flag)
    if (*(CPtr *) mem_watch.choice_flag != mem_watch.choice_val) {
      printf("Choice watch val %x was %p is now %lx, xctr %d\n",
	     mem_watch.choice_flag,mem_watch.choice_val,
	     *(CPtr) mem_watch.choice_flag,xctr);
      debug_ctr = 0;
      mem_watch.choice_val = *(CPtr *) mem_watch.choice_flag;
    }
  if (mem_watch.trail_flag)
    if (*(CPtr *) mem_watch.trail_flag != mem_watch.trail_val) {
      printf("Trail watch val %x was %p is now %lx, xctr %d\n",
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
  case CS:
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
  case LIST:	/* the list [a,b(1),c] is stored as . . . [] c b 1 a */
    fprintf(fp, "[");
    if (isnil(cell_array[(*i)-1])) {
      (*i) -= 2;
      print_term_of_subgoal(fp, i);
      fprintf(fp, "]");
    } else fprintf(stderr, "Non-single element list in print_subgoal()\n");
    break;
  case STRING:
    fprintf(fp, "%s", string_val(term));
    break;
  case INT:
    fprintf(fp, "%d", int_val(term));
    break;
  case FLOAT:
    fprintf(fp, "%.5g", float_val(term));
    break;
  case TrieVar:
    fprintf(fp, "_v%d", (int_val(term) & 0xffff));
    break;
    /* pvr 21.june -6 ESCAPE removed  , Unused2 converted to TrieVar */
  default:
    fprintf(stderr, "Term with unknown tag (%d) in print_subgoal()\n",
	    (int)cell_tag(term));
    break;
  }
}

/*----------------------------------------------------------------------*/

void print_subgoal(FILE *fp, SGFrame subg)
{
  NODEptr leaf;
  int  i = 0;
  Psc  psc = ti_psc_ptr(subg_tip_ptr(subg));

  for (leaf = subg_leaf_ptr(subg); leaf != NOPAR; leaf = unftag(Parent(leaf))) {
    cell_array[i++] = Atom(leaf);
  }
  fprintf(fp, "%s", get_name(psc));
  if (get_arity(psc) > 0) fprintf(fp, "(");
  for (i--; i >= 0 ; i--) {
    print_term_of_subgoal(fp, &i);
    if (i > 0) fprintf(fp, ", ");
  }
  if (get_arity(psc) > 0) fprintf(fp, ")");
}

/*----------------------------------------------------------------------*/

static void print_delay_element(FILE *fp, Cell del_elem)
{
  Psc  psc;
  CPtr cptr;

  if ((psc = get_str_psc(del_elem)) == delay_psc) {
    fprintf(fp, "%s(", get_name(psc));
    cptr = (CPtr)cs_val(del_elem);
    print_subgoal(fp, (SGFrame)cell(cptr+1)); fprintf(fp, ",");
    fprintf(fp, "%p", (SGFrame)cell(cptr+2)); fprintf(fp, ",");
    if (cell(cptr+3) == NEG_DELAY) fprintf(fp, "NEG");
    else fprintf(fp, "POS");
    fprintf(fp, ")");
  } else {
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
    printf("xctr %d ",xctr);
    print_inst(outfile, lpcreg);
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

void debug_subinst(byte *lpcreg, CPtr le_reg, char *Inst, CPtr arg1, CPtr arg2)
{
  if (!print_hide) {
    printf("xctr %d ",xctr);
    printf("%s, %p, %p\n", Inst, arg1, arg2);
  }
  if (register_watch_flag) monitor_register_watch();
  if (memory_watch_flag) monitor_memory_watch();
  if (pil_step && debug_ctr == 0) {
    print_hide = 0;
    pcreg = lpcreg; 
    ereg = le_reg;
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
    case REF: case REF1:
      printf("%s %p: REF (tag=%ld), value=0x%p",
	     addrtype, addr, cell_tag(term), ref_val(term));
      break;
    case CS: 
      if (addr == (CPtr)dec_addr(term) || (CPtr)dec_addr(term) == NULL) {
	printf("Possible source of core dump\n");
	printf("%s %p: CS, value=0x%p, hexval=0x%p", 
	   addrtype, addr, cs_val(term), ref_val(term));
      }	else {
	printf("%s %p: CS, value=0x%p, hexval=0x%p (%s/%d)", 
	       addrtype, addr, cs_val(term), ref_val(term),
	       get_name((struct psc_rec *) follow(cs_val(term))),
	       get_arity((struct psc_rec *) follow(cs_val(term))));
      }
      break;
    case INT:
      printf("%s %p: INT, value=%d  hexval=0x%p",
	     addrtype, addr, int_val(term), ref_val(term));
      break;
    case STRING:
      if (isnil(term)) 
	printf("%s %p: STRING, hexval=0x%p\t ([])", 
	       addrtype, addr, ref_val(term));
      else
	printf("%s %p: STRING, hexval=0x%p\t (%s)", 
	       addrtype, addr, ref_val(term), string_val(term));
      break;
    case FLOAT:
      printf("%s %p: FLOAT, value=%f, hexval=0x%lx", 
	     addrtype, addr, float_val(term), dec_addr(term));
      break;
    case LIST:
      printf("%s %p: LIST, clref=%p, hex=%p",
	     addrtype, addr, clref_val(term), ref_val(term));
      break;
    default:
      printf("%s %p: tag=%ld, hex=0x%p, cval=%d", 
	     addrtype, addr, cell_tag(term), ref_val(term), int_val(term));
      break;
  }

  if (more_info != NULL) printf(",\t(%s)\n", more_info); else printf("\n");
}

/*----------------------------------------------------------------------*/ 

static void print_cp_cell(char *addrtype, CPtr addr, Cell term)
{
  if ((ref_val(term) != NULL) && (cell_tag(term) == term)) {
    printf("NULL cell in %s %p: tag=%ld, value=0x%p\n",
	   addrtype, addr, cell_tag(term), ref_val(term));
  } else {  
    switch (cell_tag(term)) {
      case REF: case REF1:
	printf("%s %p: REF (tag=%ld), value=0x%p\n",
	       addrtype, addr, cell_tag(term), ref_val(term));
	break;
      case CS:
	printf("%s %p: CS, value=0x%p, hexval=0x%p (%s/%d)\n", 
	       addrtype, addr, cs_val(term), ref_val(term),
	       get_name((struct psc_rec *) follow(cs_val(term))),
	       get_arity((struct psc_rec *) follow(cs_val(term))));
	break;
      case INT:
	printf("%s %p: INT, value=%d, hexval=0x%p\n",
	       addrtype, addr, int_val(term), ref_val(term));
	break;
      case STRING:
	printf("%s %p: STRING, hexval=0x%p (%s)\n", 
	       addrtype, addr, ref_val(term), string_val(term));
	break;
      case FLOAT:
	printf("%s %p: FLOAT, value=%f, hexval=0x%lx\n", 
	       addrtype, addr, float_val(term), dec_addr(term));
	break;
      case LIST:
	printf("%s %p: LIST, value=%p\n", addrtype, addr, ref_val(term));
	break;
      default:
	printf("%s %p: tag=%ld, value=0x%p\n", 
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
    printf("ereg on top\n");
  }
  else {
    cell_ptr = ebreg;
    printf("ebreg on top\n");
  }
  for (i = -overlap; i < 0; i++) {
    if ( cell_ptr+i == efreg ) printf("efreg\n");
    print_cp_cell("Local Stack", cell_ptr+i, cell(cell_ptr+i));
  }
  printf("top\n");
  do {
    for (i=0; (i < STRIDESIZE) && (cell_ptr < local_stack_bottom); i++) {
      if (cell_ptr == ebreg)
	printf("ebreg\n");
      if (cell_ptr == ereg)
	printf("ereg\n");
      if (cell_ptr == efreg)
	printf("efreg\n");
      print_cp_cell("Local Stack", cell_ptr, cell(cell_ptr));
      cell_ptr++;
    }
    if (cell_ptr < local_stack_bottom) {
      printf("more (y/n)?  ");
      scanf("%c", &ans);
      skip_to_nl();
    }
    else {
      printf(EOS);
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

  if (trfreg > trreg) temp = trfreg;  else temp = trreg;
  for (i = overlap; (i > 0); i--)
    {
      if ( (temp + i) == trreg ) printf("trreg\n");
      if ( (temp + i) == trfreg ) printf("trfreg\n");
      print_cell("Trail", (CPtr)(temp+i), cell((CPtr)(temp+i)), NULL);
    }
  while (ans == 'y' && temp-offset >= (CPtr *) tcpstack.low) { 
    for (i = 0
	   ; (i <= STRIDESIZE && temp-(offset+i) >= (CPtr *)tcpstack.low)
	   ; i++ )      {
	if ( (temp - (offset+i)) == trreg ) printf("trreg\n");
	if ( (temp - (offset+i)) == trfreg ) printf("trfreg\n");
	print_cell("Trail", (CPtr)(temp-(offset+i)),
		   cell((CPtr)(temp-(offset+i))), NULL);
	if ( (temp-(offset+i)) == (CPtr *) tcpstack.low ) 
	  printf("bottom\n");
      }
      offset += STRIDESIZE;
      printf("more (y/n)?  ");
      scanf("%c", &ans);
      skip_to_nl();
    }
}

/*----------------------------------------------------------------------*/ 

static void print_freeze_choice_points(int overlap)	/* CPs grow down */
{
  int i,last = 0;
  char ans = 'y';
 
  for (i = -overlap; i < 0 ; i++) {
    print_cp_cell("CP stack", bfreg+i, cell(bfreg+i));
    if ( (bfreg + i) == breg ) printf("breg\n");
  }
  printf("bfreg\n");
  for (i = 0; (i <= STRIDESIZE && bfreg+i<=(CPtr)tcpstack.high); i++){
    if ( (bfreg + i) == breg ) printf("breg\n");
    print_cp_cell("CP stack", bfreg+i, cell(bfreg+i));
    if ( (bfreg + i) == (CPtr) tcpstack.high ) printf(EOS);
  }
  printf("more (y/n)?  ");
  scanf("%c", &ans);
  skip_to_nl();
  while (ans == 'y' && bfreg+last < (CPtr) tcpstack.high ) { 
    last = last+STRIDESIZE;
    for ( i = last
	    ; (i <= last+STRIDESIZE && bfreg+i <= (CPtr) tcpstack.high) 
	    ; i++ ) {
      if ( (bfreg + i) == breg ) printf("breg\n");
      print_cp_cell("CP stack", bfreg+i, cell(bfreg+i));
      if ( (bfreg + i) == (CPtr) tcpstack.high ) printf(EOS);
    }
    printf("more (y/n)?  ");
    scanf("%c", &ans);
    skip_to_nl();
  }
}

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

/*static*/ void print_cpf(CPtr cpf_addr, int length, int cpf_type) {

  CPtr arg;
  int i, num_of_args;
  char *s = "   CP stack ";


  switch (cpf_type) {
  case STANDARD_CP_FRAME:
    printf("Standard Choice Point Frame:\n");
    printf("%s%p:\tptr to next clause:\t0x%p\n", s,
	   &(cp_pcreg(cpf_addr)), cp_pcreg(cpf_addr));
    printf("%s%p:\tprev env cap (ebreg):\t0x%p\n", s,
	   &(cp_ebreg(cpf_addr)), cp_ebreg(cpf_addr));
    printf("%s%p:\ttop of heap:\t\t0x%p\n", s,
	   &(cp_hreg(cpf_addr)), cp_hreg(cpf_addr));
    printf("%s%p:\ttop of trail:\t\t0x%p\n", s,
	   &(cp_trreg(cpf_addr)), cp_trreg(cpf_addr));
    printf("%s%p:\tcontinuation pointer:\t0x%p\n", s,
	   &(cp_cpreg(cpf_addr)), cp_cpreg(cpf_addr));
    printf("%s%p:\ttop of local stack:\t0x%p\n", s,
	   &(cp_ereg(cpf_addr)), cp_ereg(cpf_addr));
    printf("%s%p:\tdynamic link:\t\t0x%p\n", s,
	   &(cp_prevbreg(cpf_addr)), cp_prevbreg(cpf_addr));
    printf("%s%p:\tparent tabled CP:\t0x%p\n", s,
	   &(cp_ptcp(cpf_addr)), cp_ptcp(cpf_addr));
    printf("%s%p:\tparent subgoal delayreg:\t0x%p\n", s,
	   &(cp_pdreg(cpf_addr)), cp_pdreg(cpf_addr));

    num_of_args = length - CP_SIZE;
    for (i = 1, arg = cpf_addr + CP_SIZE; i <= num_of_args; i++, arg++)
      printf("%s%p:\tpredicate arg #%d:\t0x%p\n", s, arg, i, ref_val(*arg));
    break;

  default:
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
    if ((breg+i) == bfreg) printf("bfreg\n");
    print_cp_cell("   CP stack", breg+i, cell(breg+i));
  }
  printf("breg\n");
  cpf = breg;
  do {
    for (i = 0; (i < frames) && (cpf < cp_stack_bottom); i++) {
      if ( cpf == bfreg )
	printf("bfreg\n");
      analyze_cpf(cpf, &length, &type);
      print_cpf(cpf, length, type);
      cpf = cpf + length;
    }
    if (cpf < cp_stack_bottom) {
      printf("more (y/n)?  ");
      scanf("%c", &ans);
      skip_to_nl();
    }
    else {
      printf(EOS);
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
    if ((breg+i) == bfreg) printf("bfreg\n");
    print_cp_cell("CP stack", breg+i, cell(breg+i));
  }
  printf("breg\n");
  do {
    for (i = last;
	 (i <= last + STRIDESIZE) && (breg+i <= cp_stack_bottom);
	 i++) {
      if ( (breg + i) == bfreg ) printf("bfreg\n");
      print_cp_cell("CP stack", breg+i, cell(breg+i));
      if ( (breg + i) == cp_stack_bottom ) printf(EOS);
    }
    if (breg+i < cp_stack_bottom) {
      last = last + STRIDESIZE;
      printf("more (y/n)?  ");
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
  printf("hreg\n");
  while (ans == 'y' && hreg-i > (CPtr) glstack.low) {
    for (i = 0
	   ;(i <= STRIDESIZE && hreg-(offset+i) >= (CPtr) glstack.low) 
	   ; i++) {
      if ( (hreg - (offset+i)) == hfreg ) printf("hfreg\n");
      if ( (hreg - (offset+i)) == hbreg ) printf("hbreg\n");
      print_cell("Heap", hreg-(offset+i), cell(hreg-(offset+i)), NULL);
      if ( (hreg-(offset+i)) == (CPtr) glstack.low ) 
	printf("bottom\n");
    }
    if ( (hreg-(offset+i)) != (CPtr) glstack.low ) {
      offset += STRIDESIZE;
      printf("more (y/n)?  ");
      scanf("%c", &ans);
      skip_to_nl();
    }
  }
}

/*----- For table debugging --------------------------------------------*/ 

static char *compl_stk_frame_field[] = {
  "subgoal_ptr", "level_num", "del_ret_list",
  "visited", "DG_edges", "DGT_edges"
};

/*----------------------------------------------------------------------*/

void print_completion_stack(void)
{
  int i = 0;
  EPtr eptr;
  SGFrame subg;
  CPtr temp = openreg;

  printf("openreg -> ");
  while (temp < COMPLSTACKBOTTOM) {
    if ((i % COMPLFRAMESIZE) == 0) {
      printf(EOFR);	/* end of frame */
      subg = (SGFrame) *temp;
      print_subg_header(subg);
    }
    printf("Completion Stack %p: %lx\t(%s)",
	   temp, *temp, compl_stk_frame_field[(i % COMPLFRAMESIZE)]);
    if ((i % COMPLFRAMESIZE) >= COMPLFRAMESIZE-2) {
      for (eptr = (EPtr)*temp; eptr != NULL; eptr=next_edge(eptr)) {
	printf(" --> %p", edge_to_node(eptr));
      }
    }
    printf("\n");
    temp++; i++;
  }
  printf(EOS);
}

/*----------------------------------------------------------------------*/

static void print_pdlstack(void)
{
  CPtr temp = pdlreg;

  while (temp <= (CPtr)(pdl.high) - 1) {
    printf("pdlstk %p: %lx\n", temp, *temp);
    temp++;
  }
}
 
/*----------------------------------------------------------------------*/ 

/*** Commented Kostis didn't update this, He should get back to it....
static void print_tables(void)
{
  int i = 0;
  char ans = 'y';
  SGFrame subg;
  NIDEs nide;

  i = count_subgoals();
  printf("\t There are %d subgoal structures...\n", i); if (i) printf(EOSUBG);
  subg = subg_structure_list;
  i = 0;
  while ((subg != NULL) && (ans == 'y')) {
    i++;
    print_subg_header(subg);
    printf("%p:", subg);
    printf("\tnext_subg = %6p,   ans_root_ptr = %6p,    asf_list_ptr = %p,\n",
	   subg_next_subgoal(subg), subg_ans_root_ptr(subg),
	   subg_asf_list_ptr(subg));
    printf("\t  tip_ptr = %6p,  compl_stk_ptr = %6p,  compl_susp_ptr = %p,\n",
	   subg_tip_ptr(subg), subg_compl_stack_ptr(subg),
	   subg_compl_susp_ptr(subg));
    printf("\t ans_list = %6p,       leaf_ptr = %6p,          cp_ptr = %p\n",
	   subg_answers(subg), subg_leaf_ptr(subg), subg_cp_ptr(subg));
    printf("\tnide_list");
    for (nide = subg_nide_list(subg); nide != NULL; nide = ides_next_ide(nide))
      printf(" --> %p", ides_ide(nide));
    if (subg_nide_list(subg) == NULL) printf(" = NULL\n"); else printf("\n");
    subg = subg_next_subgoal(subg);
    if (subg != NULL) printf(EOSUBG);
    if (i == 10) {
      printf("more (y/n)?  ");
      scanf("%c", &ans);
      skip_to_nl();
      i = 0;
    }
  }
  printf(EOS);
}
*** end of commented out part *****/

/*----------------------------------------------------------------------*/ 

static void print_status(void)
{
  printf("     ereg: 0x%p\n", ereg);
  printf("    ebreg: 0x%p\n", ebreg);
  printf("     breg: 0x%p\n", breg);
  printf("     hreg: 0x%p\n", hreg);
  printf("    hbreg: 0x%p\n", hbreg);
  printf("    trreg: 0x%p\n", trreg);
  printf("    cpreg: 0x%p\n", cpreg);
  printf("    pcreg: 0x%p\n", pcreg);

  printf("    efreg: 0x%p\n", efreg);
  printf("    bfreg: 0x%p\n", bfreg);
  printf("    hfreg: 0x%p\n", hfreg);
  printf("   trfreg: 0x%p\n", trfreg);
  printf("   pdlreg: 0x%p\n", pdlreg);
  printf("  ptcpreg: 0x%p\n", ptcpreg);
  printf(" delayreg: 0x%p\n", delayreg);
  printf("neg_delay: %s\n", (neg_delay == FALSE) ? "False" : "True");
  printf("   level#: %d\n", level_num);

  printf("\nPDL\n");
  printf("\tlow:       %p\n", pdl.low);
  printf("\thigh:      %p\n", pdl.high);
  printf("\tsize:      %ld\n", pdl.size); /* JF: long */
  printf("\tinit size: %ld\n", pdl.init_size); /* JF: long */

  printf("\nGlobal / Local Stack\n");
  printf("\tlow:       %p\n", glstack.low);
  printf("\thigh:      %p\n", glstack.high);
  printf("\tsize:      %ld\n", glstack.size); /* JF: long */
  printf("\tinit size: %ld\n", glstack.init_size); /* JF: long */

  printf("\nTrail / Choice Point Stack\n");
  printf("\tlow:       %p\n", tcpstack.low);
  printf("\thigh:      %p\n", tcpstack.high);
  printf("\tsize:      %ld\n", tcpstack.size); /* JF: long */
  printf("\tinit size: %ld\n", tcpstack.init_size); /* JF: long */

  printf("\nCompletion Stack\n");
  printf("\tlow:       %p\n", complstack.low);
  printf("\thigh:      %p\n", complstack.high);
  printf("\tsize:      %ld\n", complstack.size); /* JF: long */
  printf("\tinit size: %ld\n", complstack.init_size); /* JF: long */
}

/*----------------------------------------------------------------------*/ 

void pofsprint(CPtr base, int arity)
{     
  CPtr arg_ptr = base;

  printf("( ");
  for (arg_ptr = base - 1; arg_ptr >= base - arity; arg_ptr--) {
    printterm((Cell) arg_ptr, (byte) 1, 8);
    if (arg_ptr != base - arity)
      printf(",");
  }
  printf(")\n");
}

/*----------------------------------------------------------------------*/ 

static void debug_interact(void)
{
  char command, mod[32], name[32];
  int num, num1;
  Pair sym;

 again:
  printf("\n > ");  
  fflush(outfile);
  scanf("%c", &command);
  switch (command) {
  case 'a':
    scanf("%s %x", name, &num);
    skip_to_nl();
    switch (name[0]) {
    case 'a':
      printf("0x%x: 0x%x\n", num, *(int *)num);
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
    sym = (Pair)insert_module(0, mod);
    sym = (Pair)insert(name, num, sym->psc_ptr, &num);
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
  case 'C':
    scanf("%d", &num);
    skip_to_nl();
    print_freeze_choice_points(num);
    skip_to_nl();
    goto again;
  case 'd':
    skip_to_nl();
    dis(1);
    goto again;
  case 'D':
    skip_to_nl();
    fprintf(outfile, "Delay List = ");
    print_delay_list(outfile, delayreg);
    fprintf(outfile, "\n");
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
    printf("Reg[%d] = ", num);
    printterm(cell(reg+num), 1, 8);
    printf("\n"); 
    printf("%lx\n",*(reg+num));
    goto again;
  case 'R':
    scanf("%d", &num);
    skip_to_nl(); 
    printf("Reg[%d] = %lx\n",num,*(reg+num));
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
    sym = (Pair)insert_module(0, mod);
    sym = (Pair)insert(name,num, sym->psc_ptr, &num);
    set_spy(sym->psc_ptr, 0x00);
    goto again;
  case 'v':
    scanf("%d", &num);
    skip_to_nl();
    printf("Var[%d] = ", num);
    printterm(cell(ereg-num), 1, 8);
    printf("\n");
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
    /*    print_tables(); */
    goto again;
  case '2':
    scanf("%d",&num);
    skip_to_nl();
    printf("tabptr: 0x%p tabptrval: 0x%lx\n",
	   ((CPtr) (pdl.low)) + num,
	   *(((CPtr) (pdl.low)) + num));
    goto again;
  case '\n':
    break;
  default:
    skip_to_nl();
    printf("Unknown command\n");
    goto again;
  }
  return;
}


#endif	/* DEBUG */
