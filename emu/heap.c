/* File:      heap.c
** Author(s): Bart Demoen
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


/*
This module provides

	reallocation of the heap/environment area
		void glstack_realloc(new_size,arity)
		originally written by E. Johnson in file
		memory.c, but completely redone by Bart Demoen

	heap garbage collector
		int gc_heap(arity)


	printing routines for some areas
		print_heap
		print_ls
		print_cp
		print_regs
		print_tr
		print_all: does all of the above
	some - maybe all - of these were somewhere in the system already
		but didn't comply to my needs

*/


/* configs/config.h must be the first #include.  Please don't move it. */
#include "configs/config.h"
#include "debugs/debug.h"

#include <string.h>
#include <stdlib.h>
#include <malloc.h>
#include <sys/stat.h>
/* special.h must be included after sys/stat.h */
#include "configs/special.h"

#include "auxlry.h"
#include "cell.h"
#include "memory.h"
#include "inst.h"

/* For Reallocation Routines
   ------------------------- */
#include <stdio.h>     /* printf */

#include "register.h"  /* breg, trreg */
#include "tries.h"     /* needed by "choice.h" */
#include "choice.h"    /* choice point structures and macros */
#include "xsberror.h"  /* xsb_exit() */
#include "psc.h"       /* needed by "xmacro.h" */
#include "xmacro.h"    /* Completion Stack and Subgoal Frame def's */


static int printnum = 0 ;
static CPtr heap_bot,heap_top,ls_bot,ls_top,tr_bot,tr_top,cp_bot,cp_top ;
static FILE *where ;
static int num_gc = 0 ;
static int print_at = 0 ; /* at the print_at-th gc, the stacks are printed */
static int print_anyway = 0 ;

#define print_on_gc ((print_at == num_gc) || print_anyway)


static char *heap_marks = NULL ;
static char *ls_marks = NULL ;
static char *cp_marks = NULL ;
static char *tr_marks = NULL ;

#define MARKED 1
#define CHAIN_BIT 4                            

#define h_marked(i) (heap_marks[i])
#define h_mark(i) heap_marks[i] |= MARKED

/*
static void h_mark(int i)
{
	if ((num_gc == 12))
		{ printf("%d\n",i) ;
		  if (i == 53801) exit(0) ;
		}
	heap_marks[i] |= MARKED ;

}
*/

#define ls_marked(i) (ls_marks[i])
#define ls_mark(i) ls_marks[i] |= MARKED

/*
 top_of_localstk is only right after the execution of a call/fail instruction.
 the over approximation made here is fine for stack expansion
*/
#define stack_boundaries \
  heap_top = hreg; \
  ls_top = top_of_localstk - 1024 ; \
  if (ls_top < heap_top) ls_top = heap_top ; \
  heap_bot = (CPtr)glstack.low ; \
  ls_bot = (CPtr)glstack.high - 1 ; \
  tr_top = (CPtr)(top_of_trail) ; \
  tr_bot = (CPtr)tcpstack.low ; \
  cp_bot = (CPtr)tcpstack.high - 1 ; \
  cp_top = (bfreg < breg) ? bfreg : breg ; /* is this correct ? */

#define FROM_NOWHERE 0
#define FROM_LS 1
#define FROM_CP 2
#define FROM_TR 3
#define FROM_AREG 4
#define FROM_HEAP 5

#define TO_NOWHERE 0
#define TO_LS 1
#define TO_CP 2
#define TO_TR 3
#define TO_AREG 4
#define TO_HEAP 5

#define realloc_ref(cell_pr, cell_val)                          \
{       if (heap_bot <= cell_val)                               \
        {   if( cell_val < heap_top)                            \
                *cell_ptr = cell_val + heap_offset ;            \
            else if( cell_val <= ls_bot )                       \
                 *cell_ptr = cell_val + local_offset ;          \
}       }

#define reallocate_heap_or_ls_pointer(cell_ptr) 		\
    cell_val = (Cell)*cell_ptr ; 				\
    switch (cell_tag(cell_val)) 				\
    { case REF: case REF1 : 					\
        realloc_ref(cell_ptr,(CPtr)cell_val);                   \
        break ; /* end case FREE or REF */ 			\
      case CS : 						\
        if( heap_bot <= (clref_val(cell_val)) &&		\
                        (clref_val(cell_val)) < heap_top )      \
            *cell_ptr = 					\
		(CPtr)makecs((Cell)(clref_val(cell_val)+heap_offset)) ;\
        break ; 						\
      case LIST : 						\
        if( heap_bot <= (clref_val(cell_val)) &&		\
		        (clref_val(cell_val)) < heap_top )	\
            *cell_ptr =						\
		(CPtr)makelist((Cell)(clref_val(cell_val)+heap_offset));\
        break ; 						\
      default : /* no need to reallocate */ 			\
        break ; 						\
    }

void print_heap(int, int,int) ;
void print_all() ;

static CPtr pointer_from_cell(Cell cell, int *tag, int *whereto)
{ int t ;
  CPtr retp ;

      *tag = t = cell_tag(cell) ;
      switch (t)
	{ case REF : case REF1:
		retp = (CPtr)cell ;
		break ;
	  case LIST :
		retp = clref_val(cell) ;
		break ;
	  case CS :
		retp = ((CPtr)(cs_val(cell))) ;
		break ;
	  default :
		*whereto = TO_NOWHERE ;
		return((CPtr)cell) ;
	}

      if ((heap_bot <= retp) && (retp < heap_top)) *whereto = TO_HEAP ;
	else
      if ((tr_bot <= retp) && (retp <= tr_top)) *whereto = TO_TR ;
        else
      if ((ls_top <= retp) && (retp <= ls_bot)) *whereto = TO_LS ;
        else
      if ((cp_top <= retp) && (retp <= cp_bot)) *whereto = TO_CP ;
        else *whereto = TO_NOWHERE ;
      return(retp) ;

} /* pointer_from_cell */

static char * pr_h_marked(CPtr cell_ptr)
{ int i ;
  i = cell_ptr - heap_bot ;
  if (heap_marks == NULL) return("not_m") ;
  if (h_marked(i) == MARKED) return("marked") ;
  if (h_marked(i) == CHAIN_BIT) return("chained") ;
  if (h_marked(i) == (CHAIN_BIT | MARKED)) return("chained+marked") ;
  return("not_m") ;
} /* pr_h_marked */

static char * pr_ls_marked(CPtr cell_ptr) 
{ int i ; 
  i = cell_ptr - ls_top ;
  if (ls_marks == NULL) return("not_m") ;
  if (ls_marked(i) == MARKED) return("marked") ;
  if (ls_marked(i) == CHAIN_BIT) return("chained") ;
  if (ls_marked(i) == (CHAIN_BIT | MARKED)) return("chained+marked") ;
  return("not_m") ; 
} /* pr_h_marked */ 

/* mark_cell keeps an explicit stack
   marking without using such a stack, as in SICStus, should not be considered
   it is nice, but slower and more prone to errors
   recursive marking is the only aternative in my opinion, but one can
   construct too easely examples that overflow the C-stack
*/

#define MAXS 3700
#define push_to_mark(p) mark_stack[mark_top++] = p
#define mark_overflow (mark_top >= MAXS)

static int mark_cell(CPtr cell_ptr)
{ CPtr p ;
  Cell cell_val ;
  int i, m, arity ;
  CPtr mark_stack[MAXS+MAX_ARITY+1] ;
  int mark_top = 0 ;

    m = 0 ;
mark_more:
    if ((heap_bot > cell_ptr) || (cell_ptr >= heap_top)) /* defensive marking */
		goto pop_more ;
    i = cell_ptr - heap_bot ;
    if (h_marked(i)) goto pop_more ;
    h_mark(i) ;
    m++ ;

    cell_val = *cell_ptr; p = (CPtr)cell_val ;
    switch (cell_tag(cell_val))
    { case REF : case REF1 :
        if (p == cell_ptr) goto pop_more ;
	cell_ptr = p ;
	goto mark_more ;

      case CS :                     
        cell_ptr = ((CPtr)(cs_val(cell_val))) ;
	i = cell_ptr - heap_bot ;
	if (h_marked(i)) goto pop_more ;
	h_mark(i) ; m++ ;
	cell_val = *cell_ptr;
	arity = get_arity((Psc)(cell_val)) ;
	p = ++cell_ptr ;
	if (mark_overflow)
		{ while (--arity)
			{ m += mark_cell(++p) ; }
		}
	else while (--arity) push_to_mark(++p) ;
	goto mark_more ;

      case LIST :
        cell_ptr = clref_val(cell_val) ;
        if (mark_overflow)
        	{ m += mark_cell(cell_ptr+1) ; }
        else push_to_mark(cell_ptr+1) ;
	goto mark_more ;

      case INT :
      case FLOAT :
      case STRING :
	goto pop_more ;

      default :
        fprintf(stderr,"Unknown tag on heap during marking %ld\n",(Cell)cell_val) ; 
	return(0) ;
    }
pop_more:
    if (mark_top--)
    	{ cell_ptr = mark_stack[mark_top] ; goto mark_more ; }
    return(m) ;

} /* mark_cell */

static int mark_root(Cell cell_val)
{ int m, i, arity ;
  CPtr cell_ptr,p ;
  int tag, whereto ;
  Cell v ;

/* this is one of the places to be defensive while marking: and uninitialised */
/* cell in the ls, can point to a Psc; the danger is not in following the Psc */
/* and marking something outside of the heap: mark_cell takes care of that;   */
/* the dangerous thing is to mark the cell with the Psc on the heap without   */
/* marking all its arguments */

    if (cell_val == 0) return(0) ;
    switch (cell_tag(cell_val))
    { case REF : case REF1 :
    	v = *(CPtr)cell_val ;
    	p = pointer_from_cell(v,&tag,&whereto) ;
    	switch (tag)
    	{ case REF : case REF1 :
    		if (whereto != TO_HEAP) return(0) ;
    		break ;
    	}
    	return(mark_cell((CPtr)cell_val)) ;

      case CS : 
        cell_ptr = ((CPtr)(cs_val(cell_val))) ;
        if ((heap_bot > cell_ptr) || (cell_ptr > heap_top)) return(0) ;
        i = cell_ptr - heap_bot ; 
        if (h_marked(i)) return(0) ; 
	/* now check that at i, there is a Psc */
	v = *cell_ptr ;
	p = pointer_from_cell(v,&tag,&whereto) ;
	switch (tag)
	{ case REF: case REF1 :
		if (whereto != TO_NOWHERE) return(0) ;
		break ;
	}
        h_mark(i) ; m = 1 ; 
        cell_val = *cell_ptr;
        arity = get_arity((Psc)(cell_val)) ;
        while (arity--) m += mark_cell(++cell_ptr) ;
        return(m) ;

      case LIST :
	/* the 2 cells will be marked iff neither of them is a Psc */
        cell_ptr = clref_val(cell_val) ;
        if ((heap_bot > cell_ptr) || (cell_ptr > heap_top)) return(0) ;
	v = *cell_ptr ;
	p = pointer_from_cell(v,&tag,&whereto) ;
	switch (tag)
	{ case REF: case REF1 :
		if (whereto != TO_HEAP) return(0) ;
		break ;
	}
	v = *(++cell_ptr) ;
	p = pointer_from_cell(v,&tag,&whereto) ;
	switch (tag)
	{ case REF: case REF1 :
		if (whereto != TO_HEAP) return(0) ;
		break ;
	}
        m = mark_cell(cell_ptr) ;
        cell_ptr-- ; 
        m += mark_cell(cell_ptr) ;
        return(m) ;

      default : return(0) ;
    }

} /* mark_root */

static int mark_region(CPtr beginp, CPtr endp)
{ int marked = 0 ;

  while (beginp <= endp)
    marked += mark_root(*(beginp++)) ;

  return(marked) ;
} /* mark_region */

static int mark_query()
{ int yvar, i, total_marked = 0 ;
  CPtr b,e,tr,h,a, trailed_cell ;
  byte *cp;

  b = breg ;
  e = ereg ;
  tr = (CPtr)trreg ;
  h = hreg ;
  cp = cpreg ;

  while (1)
    { while ((e < ls_bot) && (cp != NULL))
      { if (ls_marked(e - ls_top)) break ;
        ls_mark(e - ls_top) ;
        yvar = *(cp-5) - 1 ;
        total_marked += mark_region(e-yvar,e-2) ;
	i = (e-2) - ls_top ;
	while (yvar-- > 1) ls_mark(i--) ;
        cp = (byte *)e[-1] ;
        e = (CPtr)e[0] ;
      }
      if (b >= (cp_bot-CP_SIZE)) return(total_marked) ;
      a = cp_hreg(b) ;
      i = a - heap_bot ;
      if (! h_marked(i)) /* h from choicepoint should point to something that
      				is marked; if not, mark it now and set it
      				to something reasonable - int(666) is ok
      				although a bit scary :-)
      			  */
          { h_mark(i) ;
            total_marked++ ;
            *a = makeint(666) ;
          }
      a = tr ;
      tr = (CPtr)cp_trreg(b) ;
      while (a > tr)
        { /* a[0]a == prev cell; a[-1] == new value; a[-2] == & trailed cell */
          trailed_cell = (CPtr)a[-2] ;
          if ((heap_bot <= trailed_cell) && (trailed_cell < heap_top))
          	{ i = trailed_cell - heap_bot ;
          	  if (! h_marked(i))
          		{ h_mark(i) ;
          		  total_marked++ ;
          		  *trailed_cell = (Cell)trailed_cell ; /* early reset */
          		  a[-1] = (Cell)trailed_cell ;
          		}
          	}
          else
          /* it must be a ls pointer, but since the trail is traversed
             context free, one has to take into account between_h_ls
          */
          if ((ls_top <= trailed_cell) && (trailed_cell <= ls_bot))
          	{ i = trailed_cell - ls_top ;
          	  if (! ls_marked(i))
          		{ ls_mark(i) ; /* do not update total_marked */
          		  *trailed_cell = (Cell)trailed_cell ; /* early reset */
          		  a[-1] = (Cell)trailed_cell ;
          		}
         	}

          a = (CPtr)a[0] ;
        }

#ifdef MARK_QUERY_NEEDED
      if (*b == (Cell)pretry_active_inst)/* not nice neither correct :-) */
      	   total_marked += mark_region(b+CP_SIZE,(CPtr)nlcp_prevbreg(b)-1) ;
      else total_marked += mark_region(b+CP_SIZE,(CPtr)cp_prevbreg(b)-1) ;
#endif

      e = cp_ereg(b) ;
      cp = cp_cpreg(b) ;
      b = cp_prevbreg(b) ;
    }


} /* mark_query */

/*
	mark_frozen_parts is not yet completely understood by the author
	one way to do it is
		for each active subgoal
			re-install it
			do usual marking
			de-install it
	I had hoped that this would be possible reusing code from XSB
	because that would allow early reset also inside the frozen parts
	but it turns out that this *-installing business is partly hidden
	and als relies on invariants which are not true at the moment of gc;
	for instance, the top choicepoint is not an ASF, neither is it
	more recent than any environment

	so, I have decided for the moment to implement this:
		for each active subgoal
			mark its frozen tr part
			mark its frozen ls part
			mark its frozen choicepoint
	and forget about early reset at the moment - seems like this is the
	only thing that is lost

	another alternative - I will experiment with it first - is to just
	mark everything remaining ...
*/

static int mark_frozen_parts()
{ int m ;

	m = mark_region(tr_bot,tr_top) ;
	m += mark_region(ls_top,ls_bot) ;
	m += mark_region(cp_top,cp_bot) ;
	return(m) ;

} /* mark_frozen_parts */


int mark_heap(int arity)
{ int marked ;

  /* first make sure the heap pointer that might not point into heap, does */
  if (hreg == cp_hreg(breg))
	{ *hreg = makeint(666) ;
	  hreg++ ;
	}

  /* copy the aregs to the top of the heap */
  for (marked = 1; marked <= arity; marked++ )
  	*hreg++ = reg[marked] ;

  stack_boundaries ;

#ifdef STACKS_DEBUG
  if (print_on_gc) print_all() ;
#endif

  heap_marks = calloc(heap_top - heap_bot,1) ;
  ls_marks = calloc(ls_bot - ls_top + 1,1) ;
  cp_marks = calloc(cp_bot - cp_top + 1,1) ;
  tr_marks = calloc(tr_top - tr_bot + 1,1) ;

  if ((heap_marks == NULL) || (ls_marks == NULL)
  		|| (cp_marks == NULL) || (tr_marks == NULL))
	xsb_exit("Not enough core to mark heap") ;

  marked = mark_region(reg+1,reg+arity) + arity ;
  while (arity > 0)
  	h_mark((heap_top - arity--)-heap_bot) ;
  marked += mark_query() ;

  marked += mark_frozen_parts() ;

#ifdef STACKS_DEBUG
  if (print_on_gc) print_all() ;
#endif

  return(marked) ;
} /* mark_heap */

static char *code_to_string(byte *pc)
{
	return((char *)(inst_table[*pc][0])) ;
} /* code_to_string */

static void print_cell(CPtr cell_ptr,int fromwhere)
{ Integer index = 0 ;
  Cell cell_val ;
  CPtr p ;
  int whereto, tag ;
  char *s = 0 ;

    cell_val = *cell_ptr;
    index = (Integer)cell_val ;
    if ((-258 < index) && (index < 258))
	{ if (index == 0)  fprintf(where,"null,0).\n") ;
		else fprintf(where,"untagged,%ld).\n", (long) index) ;
	  return ;
	}
    if (fromwhere == FROM_CP) heap_top++ ;
    p = pointer_from_cell(cell_val,&tag,&whereto) ;
    if (fromwhere == FROM_CP) heap_top-- ;
    switch (whereto)
	{ case TO_HEAP : index = p - heap_bot ; s = "ref_heap" ; break ;
	  case TO_NOWHERE : index = p - heap_bot ; s = "ref_nowhere" ; break ;
	  case TO_LS : index = ls_bot - p ; s = "ref_ls" ; break ;
	  case TO_CP : index = cp_bot - p ; s = "ref_cp" ; break ;
	  case TO_TR : index = p - tr_bot ; s = "ref_tr" ; break ;
	}
    switch (tag)
    { case REF : case REF1 :
	if (p == NULL) fprintf(where,"null,0).\n") ;
	else
        if (p == cell_ptr) fprintf(where,"undef,_).\n") ;
        else
	{ switch (whereto)
	  { case TO_HEAP :
	   case TO_LS :
	   case TO_TR :
	   case TO_CP :
		fprintf(where,"%s,%ld).\n",s, (long) index) ;
		break ;
	   case TO_NOWHERE:
		if ((heap_top <= p) && (p < ls_top))
			{ index = (p-heap_bot) ;
			  fprintf(where,"between_h_ls,%ld/%ld,_) .\n",
				  (long) index, (long) (ls_bot-p)) ;
			}
		else
		if ((Integer)cell_val < 10000) 
			fprintf(where,"strange,%ld).\n", 
				(long)cell_val) ;
		else
		if (fromwhere == FROM_HEAP)
			fprintf(where,"funct,'%s'/%d).\n",
                        get_name((Psc)(cell_val)),
                        get_arity((Psc)(cell_val))) ;
		else
		if ((fromwhere == FROM_LS) || (fromwhere == FROM_CP))
			{ char *s ;
			  s = code_to_string((byte *)cell_val) ;
			  if (s == NULL)
			    fprintf(where,"code,%ld).\n",
				    (long)cell_val) ; 
			  else  
			    fprintf(where,"code,%ld-%s).\n",
				    (long)cell_val,s) ; 
			}
		else fprintf(where,"strange_ref,%ld).\n",(long)cell_val) ;
		break ;
	  }
	}
	break ;

      case CS :
        fprintf(where,"cs-%s,%ld).\n",s, (long) index) ;
        break ;

      case LIST :
        fprintf(where,"list-%s,%ld).\n",s, (long) index) ;
        break ;

      case INT :
        fprintf(where,"int  ,%ld).\n", (long) int_val(cell_val)) ;
        break ;

      case FLOAT :
        fprintf(where,"float,%.5g).\n", float_val((Integer)cell_val)) ;
        break ;

      case STRING :
        if (isnil(cell_val)) fprintf(where,"strin,[]) .\n") ;
        else fprintf(where,"strin,'%s').\n",string_val(cell_val)) ;
        break ;

      default :
        fprintf(where,"strange,%ld).\n",(long)cell_val) ; 
        break ;
    }

} /* print_cell */

void print_heap(int start, int end, int add)
{
  CPtr startp, endp ;
  char buf[100] ;

  sprintf(buf,"HEAP%d",printnum) ;
  printnum += add ;
  where = fopen(buf,"w") ;
  stack_boundaries ;

  if (start < 0) start = 0 ;
  startp = heap_bot + start ;
  endp = heap_bot + end ;
  if (endp > heap_top) endp = heap_top ;

  while ( startp < endp )
  { fprintf(where,"heap(%6d,%s,",start,pr_h_marked(startp)) ;
    print_cell(startp,FROM_HEAP) ;
    startp++ ; start++ ;
  }

  fclose(where) ;
} /* print_heap */

void print_ls(int add)
{
  CPtr startp, endp ;
  char buf[100] ;
  int start ;

  sprintf(buf,"LS%d",printnum) ;
  printnum += add ;
  where = fopen(buf,"w") ;
  stack_boundaries ;

  start = 1 ;
  startp = ls_bot - 1 ;
  endp = ls_top ;

  while ( startp >= endp )
  { fprintf(where,"ls(%6d,%s,",start,pr_ls_marked(startp)) ;
    print_cell(startp,FROM_LS) ;
    startp-- ; start++ ;
  }

  fclose(where) ;
} /*print_ls */

void print_cp(int add)
{
  CPtr startp, endp ;
  char buf[100] ;
  int start ;

  sprintf(buf,"CP%d",printnum) ;
  printnum += add ;
  where = fopen(buf,"w") ;
  stack_boundaries ;

/*
fprintf(where," retry_active_inst = %d\n", pretry_active_inst);
fprintf(where," completion_suspension_inst = %d\n", pcompletion_suspension_inst);
fprintf(where," check_complete_inst = %d\n", pcheck_complete_inst);  
fprintf(where," hash_handle_inst = %d\n",phash_handle_inst); 
fprintf(where," fail_inst = %d\n", pfail_inst);
fprintf(where," halt_inst = %d\n", phalt_inst);
fprintf(where," proceed_inst = %d\n", pproceed_inst);
*/

  start = 0 ;
  startp = cp_bot ;
  endp = cp_top ;

  while ( startp >= endp )
  { fprintf(where,"cp(%6d,",start) ;
    print_cell(startp,FROM_CP) ;
    startp-- ; start++ ;
  }

  fclose(where) ;
} /*print_cp */

void print_tr(int add)
{
  CPtr startp, endp ;
  int start ;

  char buf[100] ;

  sprintf(buf,"TRAIL%d",printnum) ;
  printnum += add ;
  where = fopen(buf,"w") ;
  stack_boundaries ;

  startp = tr_bot ;
  endp = tr_top ;
  start = 0 ;

  while ( startp <= endp )
  { fprintf(where,"trail(%6d,",start) ;
    print_cell(startp,FROM_TR) ;
    startp++ ; start++ ;
  }

  fclose(where) ;
} /*print_tr */

void print_regs(int a,int add)
{
  CPtr startp, endp ;                                                     
  int start ;                                                             

  char buf[100] ;                                                         

  sprintf(buf,"REGS%d",printnum) ;                                       
  printnum += add ;
  where = fopen(buf,"w") ;                                                
  stack_boundaries ;      

  startp = reg+1 ;                                                       
  endp = reg+a ;                                                         
  start = 1 ;                                                             

  while ( startp <= endp )                                              
  { fprintf(where,"areg(%6d,",start) ;
    print_cell(startp,FROM_AREG) ;                              
    startp++ ; start++ ;                   
  }                                                                       

  fprintf(where,"trreg = %ld\n", (long) ((CPtr)trreg-tr_bot)) ;
  fprintf(where,"breg = %ld\n", (long) (cp_bot-breg)) ;
  fprintf(where,"hreg = %ld\n", (long) (hreg-heap_bot)) ;
  fprintf(where,"ereg = %ld\n", (long) (ls_bot-ereg)) ;

  fprintf(where,"trfreg = %ld\n", (long) ((CPtr)trfreg-tr_bot)) ;
  fprintf(where,"bfreg = %ld\n", (long) (cp_bot-bfreg)) ;
  fprintf(where,"hfreg = %ld\n", (long) (hfreg-heap_bot)) ;
  fprintf(where,"efreg = %ld\n", (long) (ls_bot-efreg)) ;
  fprintf(where,"ptcpreg - %ld\n",(Cell)ptcpreg) ;

  fprintf(where,"ebreg = %ld\n", (long) (ls_bot-ebreg)) ;
  fprintf(where,"hbreg = %ld\n", (long) (hbreg-heap_bot)) ;

  fprintf(where,"cpreg - %ld\n",(Cell)cpreg) ;
  fprintf(where,"pcreg - %ld\n",(Cell)pcreg) ;


  fclose(where) ;

} /* print_regs */


void print_all()
{
printnum++ ;
print_heap(0,200000,0) ;
print_ls(0) ;
print_tr(0) ;
print_cp(0) ;
print_regs(10,0) ;
} /* print_all */

/*
        new_size = new size of heap + environmentstack
        arity = number of argument registers in use at moment of call

        assumption: the argument registers in use are
                        reg+1 up to reg+arity included

        if you call glstack_realloc with new_size == the current size,
                you will get a reallocated area !

        Re-allocate the space for the Global and Local Stacks' data area
        to "new_size" K-byte blocks.


        Optimizations:
                if the heap hasn't been moved, then there is no need to change:
                        o pointers INTO the heap;
                        o pointers IN the heap (because there shouldn't be
                                any pointing into the local stack).
*/

void glstack_realloc(int new_size, int arity)
{ CPtr new_heap_bot ;       /* bottom of new Global Stack area */
  CPtr new_ls_bot ;         /* bottom of new Local Stack area */

  long heap_offset ;         /* offsets between the old and new */
  long local_offset ;        /* stack bottoms, measured in Cells */

  CPtr *cell_ptr ;
  Cell cell_val ;

  long new_size_in_bytes, new_size_in_cells ; /* what a mess ! */
#ifdef STACKS_DEBUG
  long expandtime ;
#endif

  if (new_size <= glstack.size) return ;

  new_size_in_bytes = new_size*K ;
  new_size_in_cells = new_size_in_bytes/sizeof(Cell) ;
  		/* and let's hope K stays divisible by sizeof(Cell) */

#ifdef STACKS_DEBUG
  expandtime = 1000*cpu_time() ;
/* print_anyway = 1 ; */
  if (print_on_gc) print_all() ;
#endif

  stack_boundaries ;

  /* Expand the data area and push the Local Stack to the high end. */

  new_heap_bot = (CPtr)realloc(heap_bot, new_size_in_bytes);
  if (new_heap_bot == NULL)
      xsb_exit("Not enough core to resize the Heap and Local Stack!");
  heap_offset = new_heap_bot - heap_bot ;
  new_ls_bot = new_heap_bot + new_size_in_cells - 1 ;
  local_offset = new_ls_bot - ls_bot ;
  bcopy(ls_top + heap_offset,              /* move from */
         ls_top + local_offset,             /* move to */
         (ls_bot - ls_top + 1)*sizeof(Cell) );      /* number of bytes */

  /* Update the Heap links */
  for (cell_ptr = (CPtr *)(heap_top + heap_offset);
       cell_ptr-- > (CPtr *)new_heap_bot;
      )
  { reallocate_heap_or_ls_pointer(cell_ptr) ; }

  /* Update the pointers in the Local Stack */
  for (cell_ptr = (CPtr *)(ls_top + local_offset);
       cell_ptr <= (CPtr *)new_ls_bot;
       cell_ptr++)
  { reallocate_heap_or_ls_pointer(cell_ptr) ; }

  /* Update the trailed variable pointers */
  for (cell_ptr = (CPtr *)top_of_trail - 1;
       cell_ptr > (CPtr *)tcpstack.low;
       cell_ptr = cell_ptr - 2)
  { /* first the value */
    reallocate_heap_or_ls_pointer(cell_ptr) ;

    /* now the address */
    cell_ptr-- ;
    cell_val = (Cell)*cell_ptr ;
    realloc_ref(cell_ptr,(CPtr)cell_val) ;
  }

  /* Update the CP Stack pointers */
  for (cell_ptr = (CPtr *)top_of_cpstack;
       cell_ptr < (CPtr *)tcpstack.high;
       cell_ptr++)
  { reallocate_heap_or_ls_pointer(cell_ptr) ; }

  /* Update the argument registers */
  while (arity)
  { cell_ptr = (CPtr *)(reg+arity) ;
    reallocate_heap_or_ls_pointer(cell_ptr) ;
    arity-- ;    
  }

  /* Update the var_regs array */
  for( arity = 0 ; arity < MaxTrieRegs ; arity++ )
  { cell_ptr = (CPtr *)(var_regs+arity) ;
    reallocate_heap_or_ls_pointer(cell_ptr) ;
  }

  hreg = (CPtr)hreg + heap_offset ;
  hbreg = (CPtr)hbreg + heap_offset ;
  hfreg = (CPtr)hfreg + heap_offset ;
  ereg = (CPtr)ereg + local_offset ;
  ebreg = (CPtr)ebreg + local_offset ;
  efreg = (CPtr)efreg + local_offset ;

  if( islist(delayreg) )
      delayreg = (CPtr)makelist(clref_val(delayreg) + heap_offset);

  /* Update the system variables */
  glstack.low = (byte *)new_heap_bot ;
  glstack.high = (byte *)(new_ls_bot + 1) ;
  glstack.size = new_size ;

#ifdef STACKS_DEBUG
  expandtime = 1000*cpu_time() - expandtime;

  fprintf(stderr,"Heap/Local Stack expansion - finito in %ld msecs\n\n",
          expandtime) ;

/*  print_all() ; */
#endif

} /* glstack_realloc */


/* from here to end of sweep_heap is code taken to some extent from
   BinProlog and adapted to XSB - especially what concerns the
   environments, but the BinProlog was also written originally
   and solely by Bart Demoen
*/


#define h_set_chained(p)	heap_marks[(p-heap_bot)] |= CHAIN_BIT
#define h_set_unchained(p)	heap_marks[(p-heap_bot)] &= ~CHAIN_BIT
#define h_is_chained(p)		(heap_marks[(p-heap_bot)] & CHAIN_BIT)
#define h_clear_mark(p)		 heap_marks[(p-heap_bot)] &= ~MARKED

#define ls_set_chained(p)        ls_marks[(p-ls_top)] |= CHAIN_BIT 
#define ls_set_unchained(p)      ls_marks[(p-ls_top)] &= ~CHAIN_BIT
#define ls_is_chained(p)         (ls_marks[(p-ls_top)] & CHAIN_BIT)
#define ls_clear_mark(p)	 ls_marks[(p-ls_top)] = 0

#define cp_set_chained(p)        cp_marks[(p-cp_top)] |= CHAIN_BIT
#define cp_set_unchained(p)      cp_marks[(p-cp_top)] &= ~CHAIN_BIT
#define cp_is_chained(p)         (cp_marks[(p-cp_top)] & CHAIN_BIT)

#define tr_set_chained(p)        tr_marks[(p-tr_bot)] |= CHAIN_BIT 
#define tr_set_unchained(p)      tr_marks[(p-tr_bot)] &= ~CHAIN_BIT
#define tr_is_chained(p)         (tr_marks[(p-tr_bot)] & CHAIN_BIT)

static void unchain(CPtr hptr, CPtr destination)
{ CPtr start,pointsto ;
  int whereto, tag ;
  int continue_after_this = 0 ;

/* hptr is a pointer to the heap and is chained */
/* the whole chain is unchained, i.e.
	the end of the chain is put in the beginning and
	all other chained elements (up to end included) are made
		to point to the destination
	we have to make sure that the tags are ok and that the chain tags
		are switched off
   I have implemented a version which can be optimised, but it shows
   all intermediate steps as the previous chaining steps - except for
   the chain bit of hptr
*/

        h_set_unchained(hptr) ;

   do
   {
        start = (CPtr)(*hptr) ;
	/* start is for sure a pointer - possibly with a tag */
	pointsto = pointer_from_cell((Cell)start,&tag,&whereto) ;
	if (pointsto == NULL) xsb_exit("pointsto error during unchaining") ;
	switch (whereto)
	{ case TO_HEAP : continue_after_this = h_is_chained(pointsto) ;
			h_set_unchained(pointsto) ;
			break ;
	  case TO_LS : continue_after_this = ls_is_chained(pointsto) ;
			ls_set_unchained(pointsto) ;
			break ;
	  case TO_TR : continue_after_this = tr_is_chained(pointsto) ;
			tr_set_unchained(pointsto) ;
			break ;
	  case TO_CP : continue_after_this = cp_is_chained(pointsto) ;
			cp_set_unchained(pointsto) ;
			break ;
	  default : xsb_exit("whereto error during unchaining") ;
	}
	*hptr = *pointsto ;
	switch (tag)
	{ case REF: case REF1:
		*pointsto = (Cell)destination ;
		break ;
	  case CS :
		*pointsto = makecs((Cell)destination) ;
		break ;
	  case LIST :
	  	*pointsto = makelist((Cell)destination) ;
	  	break ;
	  default : xsb_exit("tag error during unchaining") ;
	}
   }
   while (continue_after_this) ;

} /* unchain */

static void swap_with_tag(CPtr p, CPtr q, int tag)
{ /* p points to a cell with contents a tagged pointer
     make *q = p + tag, but maybe shift p
  */
   *p = *q ;
   switch (tag)
    {
      case REF : case REF1 :
        *q = (Cell)p ;
	break ;
      case CS :
	*q = makecs((Cell)p) ;
	break ;
      case LIST :
	*q = makelist((Cell)p) ;
	break ;
      default : xsb_exit("error during swap_with_tag") ;
    }
} /* swap_with_tag */

/*
	sweep_heap: implements a sliding collector for the heap
	see: Algorithm of Morris / ACM paper by Appleby et al.
	num_marked = number of marked heap cells
	the relevant argument registers have been moved to the top of the heap
		prior to marking
*/

static CPtr sweep_heap(int num_marked)
{ int whereto, tag ;
  CPtr p, contents, q ;

        /* chain external (to heap) pointers */      

                /* chain argument registers */
                /* will be automatic as aregisters were copied to the heap */

                /* chain trail */
		/* more precise traversal of trail possible */

                { CPtr endtr ;
                  endtr = tr_top ;
                  for (p = tr_bot; p <= endtr ; p++ )
                  { contents = (CPtr)(*p) ;
                    q = pointer_from_cell((Cell)contents,&tag,&whereto) ;
                    if (whereto != TO_HEAP) continue ;
		    if (! h_marked(q-heap_bot)) continue ;
                    if (h_is_chained(q)) tr_set_chained(p) ;
                    h_set_chained(q) ;
                    swap_with_tag(p,q,tag) ;
                  }
                }

                /* chain choicepoints */
		/* more precise traversal of choicepoints possible */

                { CPtr endcp ;
                  endcp = cp_top ;
                  for (p = cp_bot; p >= endcp ; p-- )
                  { contents = (CPtr)(*p) ;
                    q = pointer_from_cell((Cell)contents,&tag,&whereto) ;
                    if (whereto != TO_HEAP) continue ;
		    if (! h_marked(q-heap_bot)) continue ;
                    if (h_is_chained(q)) cp_set_chained(p) ;
                    h_set_chained(q) ;
                    swap_with_tag(p,q,tag) ;
                  }
                }

		/* chain local stack */
		/* more precise traversal of local stack possible */

                { CPtr endls ;
                  endls = ls_top ;
                  for (p = ls_bot; p >= endls ; p-- )
                  { if (! ls_marked(p-ls_top)) continue ;
	            ls_clear_mark(p) ; /* chain bit cannot be on yet */
		    contents = (CPtr)(*p) ;
                    q = pointer_from_cell((Cell)contents,&tag,&whereto) ;
                    if (whereto != TO_HEAP) continue ;
		    if (! h_marked(q-heap_bot)) continue ;
                    if (h_is_chained(q)) ls_set_chained(p) ;
                    h_set_chained(q) ;
                    swap_with_tag(p,q,tag) ;
                  }
                }

#ifdef STACKS_DEBUG
    if (print_on_gc) print_all() ;
#endif

{ CPtr destination, hptr ;
  long garbage = 0 ;
  int index ;

        /* one phase upwards - from top of heap to bottom of heap */

        index = heap_top - heap_bot ;
	destination = heap_bot + num_marked - 1 ;
        for (hptr = heap_top - 1 ; hptr >= heap_bot ; hptr--)
        { if (h_marked(hptr - heap_bot))
                { /* boxing */
                  if (garbage)
                        { *(hptr+1) = (Cell)garbage ;
                          garbage = 0 ;
                        }
                  if (h_is_chained(hptr))
                        { unchain(hptr,destination) ; }
		  p = pointer_from_cell(*hptr,&tag,&whereto) ;            
		  if ((whereto == TO_HEAP) && (p < hptr))
                        { swap_with_tag(hptr,p,tag) ;
			  if (h_is_chained(p))
				h_set_chained(hptr) ;
			  else h_set_chained(p) ;
                        }
                  destination-- ;
                }
          else garbage++ ;
	  index-- ;
        }

/*
if (print_on_gc) print_all() ;
*/

        /* one phase downwards - from bottom of heap to top of heap */

        index = 0 ;
	destination = heap_bot ;
        for (hptr = heap_bot ; hptr < heap_top ;)
        { if (h_marked(hptr - heap_bot))
                { if (h_is_chained(hptr))
                        { unchain(hptr,destination) ; }
                  if ((Cell)(hptr) == *hptr) /* UNDEF */
                        *destination = (long)destination ;
                  else
		  { p = pointer_from_cell(*hptr,&tag,&whereto) ;
                    *destination = *hptr ;
		    if ((whereto == TO_HEAP) && (p > hptr))
                        { swap_with_tag(destination,p,tag) ;
                          if (h_is_chained(p))           
                                h_set_chained(destination) ;   
                          else h_set_chained(p) ;
                        }
		  }
                  h_clear_mark(hptr) ;
                  hptr++ ; destination++ ;
		  index++ ;
                }
          else { index += (long)*hptr ; hptr += (long)*hptr ; }
        }
}

        return(heap_bot + num_marked) ;

} /* sweep_heap */

static void check_zero(char *b, int l, char *s)
{ int i = 0 ;
  while (l--)
    if (*b++)
    { fprintf(stderr,"%s - left marker - %d - %d - %d\n",s,*(--b),i,l) ;
      return ;
    }
	else i++ ;

} /* check_zero */

static int total_time_gc = 0 ;

int gc_heap(int arity)
{ int marked, marktime, sweeptime ;

num_gc++ ;

  fprintf(stderr,"Heap gc - arity = %d - used = %ld - left = %ld - #gc = %d\n",
	  arity, (long) (hreg-(CPtr)glstack.low), (long) (ereg-hreg), num_gc) ;
  marktime = 1000*cpu_time() ;
  marked = mark_heap(arity) ;
  sweeptime = 1000*cpu_time() ;
  marktime = sweeptime - marktime ;
  fprintf(stderr,
  	"Heap gc - marking finished - #marked = %d - start compact\n",marked) ;
  hreg = sweep_heap(marked) ;
  /* copy the aregs from the top of the heap back */
  for (marked = 1; marked <= arity; marked++)
  	reg[marked] = hreg[marked - arity - 1] ;
  hreg -= arity ;
  hbreg = cp_hreg(breg);


  sweeptime =  1000*cpu_time() - sweeptime ;

if (print_on_gc) print_all() ;

  total_time_gc += marktime + sweeptime ;
  fprintf(stderr,
  	"Heap gc end - mark time = %d; sweep time = %d; total = %d\n\n",
			marktime, sweeptime,total_time_gc) ;
  /* get rid of the marking areas - if they exist */
  if (heap_marks) { check_zero(heap_marks,(heap_top-heap_bot),"heap") ;
                    free(heap_marks) ;
		    heap_marks = NULL ;
                  }
  if (tr_marks)   { check_zero(tr_marks,(tr_top-tr_bot + 1),"tr") ;
                    free(tr_marks) ;
		    tr_marks = NULL ;
                  }
  if (ls_marks)   { check_zero(ls_marks,(ls_bot - ls_top + 1),"ls") ;
                    free(ls_marks) ;
		    ls_marks = NULL ;
                  }
  if (cp_marks)   { check_zero(cp_marks,(cp_bot - cp_top + 1),"cp") ;
                    free(cp_marks) ;
		    cp_marks = NULL ;
                  }

  return(TRUE) ;
} /* gc_heap */
