/* File:      heap_xsb.c
** Author(s): Bart Demoen, Kostis Sagonas
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1998
** Copyright (C) K.U. Leuven, 1998-1999
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


/*************************************************************************
 * This module provides:

	reallocation of the heap/environment area
	-----------------------------------------
	Function glstack_realloc(new_size,arity)
		originally written by E. Johnson in file
		memory_xsb.c, but completely redone by Bart Demoen

	heap garbage collection
	-----------------------
	Function gc_heap(arity) - currently only supported for CHAT
	   To understand the usefulness logic, see paper:
		B. Demoen and K. Sagonas.
		Memory Management for Prolog with Tabling.
		in Proceedings of the 1998 ACM SIGPLAN International
		Symposium on Memory Management, Vancouver, B.C., Canada,
		Oct. 1998. ACM Press. p. 97-106

	   To understand the implementation and for additional information see:
		B. Demoen and K. Sagonas.
		Heap Garbage Collection in XSB: Practice and Experience.
		CW report 272, September 1998; finished February 1999.


	Function slide_heap() implements a sliding collector a la Morris
	   It was mostly written by Bart Demoen
	       for a Prolog specific one see paper:
	       K. Appleby, M. Carlsson, S. Haridi, and D. Sahlin.
	       Garbage Collection for Prolog Based on WAM.
	       Communications of the ACM, 31(6):719--741, June 1988.


	Function copy_heap() implements a copying collector a la Cheney
	   It was mostly written by Kostis Sagonas
               for a Prolog specific one see paper:
	       J. Bevemyr and T. Lindgren.
	       A Simple and Efficient Copying Garbage Collector for Prolog.
	       In M. Hermenegildo and J. Penjam, editors,
	       Proceedings of the Sixth International Symposium on
	       Programming Language Implementation and Logic Programming,
	       number 844 in LNCS, pages 88--101, Madrid, Spain, Sept.  1994.
	       Springer-Verlag.


	printing routines for some areas
		print_heap
		print_ls
		print_cp
		print_regs
		print_tr
		print_all_stacks: does all of the above
	some - maybe all - of these were somewhere in the system already
		but weren't entirely what we needed


Todo:
        adapt the garbage collectors to SLG-WAM
	provide a decent user interface to the garbage collector
	integrate with compiler

****************************************************************************/

/* configs/config.h must be the first #include.  Please don't move it. */
#include "configs/config.h"
#include "debugs/debug.h"

#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "memory_xsb.h"
#include "inst_xsb.h"

/* For Reallocation Routines
   ------------------------- */
#include <stdio.h>     /* printf */

#include "register.h"  /* breg, trreg */
#include "psc_xsb.h"       /* needed by "tries.h" and "macro_xsb.h" */
#include "tries.h"     /* needed by "choice.h" */
#include "choice.h"    /* choice point structures and macros */
#include "error_xsb.h"  /* xsb_exit() and friends */
#include "macro_xsb.h"    /* Completion Stack and Subgoal Frame def's */
#include "realloc.h"   /* Heap - ls reallocation macros */
#include "chat.h"      /* CHAT related declarations */
#include "flags_xsb.h"     /* for checking whether functionality is enabled */
#include "heap_xsb.h"
#include "io_builtins_xsb.h"

/*=========================================================================*/

/* to choose between copying or sliding collector:
   its value is determined based on the the value of flags[GARBAGE_COLLECT] */
static bool slide;

/* max value of active delay register fields in CHAT areas.  it should
   not be bigger than the margin (2nd arg) of gc_heap instruction minus
   stuff (like aregs) which are put on heap in the beginning of a sliding
   collection. and it should be used only for a sliding collector. */
#ifdef CHAT
#define MAX_DREG_MARKS 1000
#endif


#ifdef GC
/* measuring fragmentation without collection - it also sets slide = 0 */
static const int fragmentation_only = 0;
#endif
		      
/* choose to do early reset or not */
#define EARLY_RESET 1


/* expresses how often early reset of a trailed heap cell occurs */
static int heap_early_reset;

/* expresses how often early reset of a trailed local stack cell occurs */
static int ls_early_reset;


/* ways to count gc and control the output during a gc */
static int printnum = 0 ;
static int num_gc = 0 ;

#ifdef DEBUG
static int print_at = 0 ; /* at the print_at-th gc, the stacks are printed */
static int print_after = 0 ; /* if non zero, print all after this numgc */
static int print_anyway = 0 ;

#define print_on_gc \
        ((print_at == num_gc) \
	 || ((print_after > 0) && (print_after <= num_gc)) \
	 || print_anyway)
#else
#define print_on_gc 0
#endif

/* if SAFE_GC is defined, some more checks are made after gargage collection */
/*#define SAFE_GC*/

/* if VERBOSE_GC is defined, garbage collection prints its statistics */
/*#define VERBOSE_GC*/


/*---------------------------------------------------------------------------*/
/* global variables for top and bottom of some areas + macro to compute them */
/*---------------------------------------------------------------------------*/

static CPtr heap_bot,heap_top,
            ls_bot,ls_top,
            tr_bot,tr_top,
            cp_bot,cp_top,
            compl_top,compl_bot;

#define stack_boundaries \
  heap_top = hreg; \
  ls_top = top_of_localstk ; \
  if (ls_top < heap_top) xsb_exit("Heap and local stack are clobbered"); \
  heap_bot = (CPtr)glstack.low ; \
  ls_bot = (CPtr)glstack.high - 1 ; \
  tr_top = (CPtr)(top_of_trail) - 1 ; \
  tr_bot = (CPtr)tcpstack.low ; \
  cp_bot = (CPtr)tcpstack.high - 1 ; \
  cp_top = top_of_cpstack ; \
  compl_top = (CPtr)complstack.low ; /* NOT top_of_complstk !!! */\
  compl_bot = (CPtr)complstack.high ;

#define points_into_heap(p)  ((heap_bot <= p) && (p < heap_top))
#define points_into_ls(p)    ((ls_top <= p) && (p <= ls_bot))
#define points_into_cp(p)    ((cp_top <= p) && (p <= cp_bot))
#define points_into_tr(p)    ((tr_bot <= p) && (p <= tr_top))
#define points_into_compl(p) ((compl_top <= p) && (p <= compl_bot))

/*----------------------------------------------------------------------*/
/* marker bits in different areas: the mark bit for the CHAT areas is   */
/* in the CHAT areas themselves.                                        */
/*----------------------------------------------------------------------*/

static char *heap_marks  = NULL ;
static char *ls_marks    = NULL ;
/* the following areas are just chain bits and are used only in sliding */
static char *cp_marks    = NULL ;
static char *tr_marks    = NULL ;
#ifdef CHAT
static char *compl_marks = NULL ;
#endif

#define MARKED    1
#define CHAIN_BIT 4                            

#define h_marked(i)        (heap_marks[i])
#define h_mark(i)          heap_marks[i] |= MARKED
#define h_clear_mark(i)	   heap_marks[i] &= ~MARKED

#define ls_marked(i)       (ls_marks[i])
#define ls_mark(i)         ls_marks[i] |= MARKED
#define ls_clear_mark(i)   ls_marks[i] = 0

/*=========================================================================*/

/* in the absence of serious bugs, the test is an invariant of the WAM */
#ifdef DEBUG
#define testreturnit(retp)   if (points_into_heap(retp)) return(retp)
#else
#define testreturnit(retp)   return(retp)
#endif

#ifdef GC
static inline CPtr hp_pointer_from_cell(Cell cell, int *tag)
{
    int t;
    CPtr retp;

    t = cell_tag(cell) ;

    /* the use of if-tests rather than a switch is for efficiency ! */
    /* as this function is very heavily used - do not modify */
    if (t == LIST)
      {
	*tag = LIST;
	retp = clref_val(cell);
	testreturnit(retp);
      }
    if (t == CS)
      {
	*tag = CS;
	retp = (CPtr)(cs_val(cell));
	testreturnit(retp);
      }
    if ((t == REF) || (t == REF1))
      {
	*tag = t;
	retp = (CPtr)cell ;
	if (points_into_heap(retp)) return(retp);
      }
    if (t == ATTV)
      {
	xsb_abort("case ATTV in hp_pointer_from_cell() is not implemented yet");
      }

    return NULL;
} /* hp_pointer_from_cell */
#endif

static CPtr pointer_from_cell(Cell cell, int *tag, int *whereto)
{ int t ;
  CPtr retp ;

      *tag = t = cell_tag(cell) ;
      switch (t)
	{ case REF: case REF1:
	    retp = (CPtr)cell ;
	    break ;
	  case ATTV:
	    retp = (CPtr)cell;
	    xsb_abort("case ATTV in pointer_from_cell is not implemented yet");
	    break;
	  case LIST:
	    retp = clref_val(cell) ;
	    break ;
	  case CS:
	    retp = ((CPtr)(cs_val(cell))) ;
	    break ;
	  default:
	    *whereto = TO_NOWHERE ;
	    return((CPtr)cell) ;
	}

      if (points_into_heap(retp)) *whereto = TO_HEAP ;
	else
      if (points_into_tr(retp)) *whereto = TO_TR ;
        else
      if (points_into_ls(retp)) *whereto = TO_LS ;
        else
      if (points_into_cp(retp)) *whereto = TO_CP ;
        else
      if (points_into_compl(retp)) *whereto = TO_COMPL ;
        else *whereto = TO_NOWHERE ;
      return(retp) ;

} /* pointer_from_cell */

/*-------------------------------------------------------------------------*/

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
} /* pr_ls_marked */ 

static char * pr_cp_marked(CPtr cell_ptr) 
{ int i ; 
  i = cell_ptr - cp_top ;
  if (cp_marks == NULL) return("not_m") ;
  if (cp_marks[i]) return("chained") ;
  return("not_m") ; 
} /* pr_cp_marked */ 

static char * pr_tr_marked(CPtr cell_ptr) 
{ int i ; 
  i = cell_ptr - tr_bot ;
  if (tr_marks == NULL) return("not_m") ;
  if (tr_marks[i]) return("chained") ;
  return("not_m") ; 
} /* pr_tr_marked */ 

/*-------------------------------------------------------------------------*/

/* Function mark_cell() keeps an explicit stack to perform marking.
   Marking without using such a stack, as in SICStus, should not be
   considered.  It is nice, but slower and more prone to errors.
   Recursive marking is the only alternative in my opinion, but one can
   construct too easily examples that overflow the C-stack - Bart Demoen.
*/

#define MAXS 3700
#define push_to_mark(p) mark_stack[mark_top++] = p
#define mark_overflow   (mark_top >= MAXS)

static int mark_cell(CPtr cell_ptr)
{
  CPtr p ;
  Cell cell_val ;
  int  i, m, arity, tag ;
  int  mark_top = 0 ;
  CPtr mark_stack[MAXS+MAX_ARITY+1] ;

    m = 0 ;
mark_more:
    if (!points_into_heap(cell_ptr)) /* defensive marking */
		goto pop_more ;
safe_mark_more:
    i = cell_ptr - heap_bot ;
    if (h_marked(i)) goto pop_more ;
    h_mark(i) ;
    m++ ;

    cell_val = *cell_ptr;
    tag = cell_tag(cell_val);

    if (tag == LIST)
      { cell_ptr = clref_val(cell_val) ;
        if (mark_overflow)
	  { m += mark_cell(cell_ptr+1) ; }
        else push_to_mark(cell_ptr+1) ;
        goto safe_mark_more ;
      }

    if (tag == CS)
      { p = (CPtr)cell_val ;
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
      }

    if ((tag == REF) || (tag == REF1))
      { p = (CPtr)cell_val ;
        if (p == cell_ptr) goto pop_more ;
	cell_ptr = p ;
	goto mark_more ;
      }

    /*    if (tag == STRUCT)
      { xsb_dbgmsg("Unknown tag on heap during marking %ld", cell_val) ;
        return(0) ;
      } */

pop_more:
    if (mark_top--)
    	{ cell_ptr = mark_stack[mark_top] ; goto mark_more ; }
    return(m) ;

} /* mark_cell */

/*----------------------------------------------------------------------*/

static int mark_root(Cell cell_val)
{
  int m, i, arity ;
  CPtr cell_ptr;
  int tag, whereto ;
  Cell v ;

/* this is one of the places to be defensive while marking: an uninitialised */
/* cell in the ls can point to a Psc; the danger is not in following the Psc */
/* and mark something outside of the heap: mark_cell takes care of that; the */
/* dangerous thing is to mark the cell with the Psc on the heap without      */
/* marking all its arguments */

    if (cell_val == 0) return(0) ;
    switch (cell_tag(cell_val))
    { case REF : case REF1 :
    	v = *(CPtr)cell_val ;
    	pointer_from_cell(v,&tag,&whereto) ;
    	switch (tag)
    	{ case REF : case REF1 :
    		if (whereto != TO_HEAP) return(0) ;
    		break ;
    	}
    	return(mark_cell((CPtr)cell_val)) ;

      case CS : 
        cell_ptr = ((CPtr)(cs_val(cell_val))) ;
        if (!points_into_heap(cell_ptr)) return(0) ;
        i = cell_ptr - heap_bot ; 
        if (h_marked(i)) return(0) ; 
	/* now check that at i, there is a Psc */
	v = *cell_ptr ;
	pointer_from_cell(v,&tag,&whereto) ;
	/* v must be a PSC - the following tries to test this */
	switch (tag)
	{ case REF: case REF1 :
		if (whereto != TO_NOWHERE) return(0) ;
		break ;
		/* default: return(0); */
	}
        h_mark(i) ; m = 1 ; 
        cell_val = *cell_ptr;
        arity = get_arity((Psc)(cell_val)) ;
        while (arity--) m += mark_cell(++cell_ptr) ;
        return(m) ;

      case LIST :
	/* the 2 cells will be marked iff neither of them is a Psc */
        cell_ptr = clref_val(cell_val) ;
        if (!points_into_heap(cell_ptr)) return(0) ;
	v = *cell_ptr ;
	pointer_from_cell(v,&tag,&whereto) ;
	switch (tag)
	{ case REF: case REF1 :
		if (whereto != TO_HEAP) return(0) ;
		break ;
	}
	v = *(++cell_ptr) ;
	pointer_from_cell(v,&tag,&whereto) ;
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

/*----------------------------------------------------------------------*/

static inline int mark_region(CPtr beginp, CPtr endp)
{ int marked = 0 ;

  while (beginp <= endp)
    marked += mark_root(*(beginp++)) ;

  return(marked) ;
} /* mark_region */

/*----------------------------------------------------------------------*/

static int mark_query(void)
{
  int yvar, i, total_marked = 0 ;
  CPtr b,e,*tr,a,d, trailed_cell ;
  byte *cp;

  b = breg ;
  e = ereg ;
  tr = trreg ;
  cp = cpreg ;

  while (1)
    {
      while ((e < ls_bot) && (cp != NULL))
      {
	if (ls_marked(e - ls_top)) break ;
        ls_mark(e - ls_top) ;
        yvar = *(cp-2*sizeof(Cell)+3) - 1 ;
        total_marked += mark_region(e-yvar,e-2) ;
	i = (e-2) - ls_top ;
	while (yvar-- > 1) { ls_mark(i--); }
        cp = (byte *)e[-1] ;
        e = (CPtr)e[0] ;
      }
      if (b >= (cp_bot-CP_SIZE)) return(total_marked) ;
      a = (CPtr)tr ;
      tr = cp_trreg(b) ;
      while (a-- > (CPtr)tr)
        { /* a[0] == & trailed cell */
          trailed_cell = (CPtr)a[0] ;
          if (points_into_heap(trailed_cell))
	    { i = trailed_cell - heap_bot ;
	      if (! h_marked(i))
		{
#if (EARLY_RESET == 1)
		  {
		    h_mark(i) ;
		    total_marked++ ;
		    bld_free(trailed_cell); /* early reset */
		    /* could do trail compaction now or later */
		    heap_early_reset++;
		  }
#else
		  {
		    total_marked += mark_root((Cell)trailed_cell);
		  }
#endif
		}
	    }
          else
	    /* it must be a ls pointer, but for safety
	       we take into account between_h_ls */
	    if (points_into_ls(trailed_cell))
	      { i = trailed_cell - ls_top ;
          	if (! ls_marked(i))
		  {
#if (EARLY_RESET == 1)
		    {
		      /* don't ls_mark(i) because we early reset
			 so, it is not a heap pointer
			 but marking would be correct */
		      bld_free(trailed_cell) ; /* early reset */
		      /* could do trail compaction now or later */
		      ls_early_reset++;
		    }
#else
		    { ls_mark(i) ;
		      total_marked += mark_region(trailed_cell,trailed_cell);
		    }
#endif
		  }
	      }
        }

      /* mark the arguments in the choicepoint */
      /* the choicepoint can be a consumer, a generator or ... */

      /* the code for non-tabled choice points is ok */
      /* for all other cps - check that
	   (1) the saved arguments are marked
	   (2) the substitution factor is marked
      */

      /* the following code does not work for SLG-WAM as is */

      if (is_generator_choicepoint(b))
	{ /* mark the arguments */
	  total_marked += mark_region(b+TCP_SIZE,tcp_prevbreg(b)-1);
	  /* mark the substitution factor later */
	}
      else if (is_consumer_choicepoint(b))
	{ /* mark substitution factor -- skip the number of SF vars */
	  /* substitution factor is in the choicepoint for consumers */
	  total_marked += mark_region(b+NLCPSIZE+1,nlcp_prevbreg(b)-1);
	  /* b+NLCPSIZE+cell(b+NLCPSIZE)); */
	}
#ifdef CHAT
      else if (is_compl_susp_frame(b))
	/* there is nothing to do in this case */ ;
#endif
      else { CPtr endregion, beginregion;
             endregion = cp_prevbreg(b)-1;
	     beginregion = b+CP_SIZE;
	     total_marked += mark_region(beginregion,endregion) ;
           }

      /* mark the delay list field of all choice points in CP stack too */
      if ((d = cp_pdreg(b)) != NULL) {
	total_marked += mark_root((Cell)d);
      }

      e = cp_ereg(b) ;
      cp = cp_cpreg(b) ;
      b = cp_prevbreg(b) ;
    }

} /* mark_query */

/*----------------------------------------------------------------------*/

#ifdef CHAT

static int chat_mark_trail(CPtr *tr, int trlen)
{
  int  i, j, m = 0;
  CPtr trailed_cell;

  /* mark trail: both address and value - do cautious early reset */

  while (trlen)
    { 
      if (trlen > sizeof(CPtr))
	{ trlen -= sizeof(CPtr); j = sizeof(CPtr)>>1 ; }
      else { j = trlen>>1; trlen = 0; }
      while (j--)
	{ 
	  /* mark address */
	  trailed_cell = *tr;
	  tr++;
	  if (points_into_heap(trailed_cell))
	    { i = trailed_cell - heap_bot ;
	      if (! h_marked(i))
		{ h_mark(i) ; /* it is correct to mark this cell, because the
				 value is also marked
				 avoiding to mark the cell would make chat
	                         trail compaction necessary
			      */
#if (EARLY_RESET == 1)
		  *tr = (CPtr)makeint(88888);
		  heap_early_reset++;
#endif
		  m++ ;
		}
	    }
	  else
	    /* it must be a ls pointer, but we check between_h_ls */
	    if (points_into_ls(trailed_cell))
	      { i = trailed_cell - ls_top ;
	        if (! ls_marked(i))
		  {
		    /* it is possible to come here:
		       in SLG-WAM (and CHAT) there might exist envs
		       that are not reachable from anywhere and some
		       CHAT trail pointer might point into it
		       as example (maybe not simplest):
		       :- table g/1.

		       g(X) :- a, g(Y).
		       g(_) :- gc_heap.

		       a :- b(X), p(X), use(X).
		       
		       p(17).
		       
		       use(_).
		       
		       b(_).
		       b(_) :- fail.
		    */
#if (EARLY_RESET == 1)
		    *tr = (CPtr)makeint(99999);
		    ls_early_reset++;
#endif
		  }
	      }

	  /* mark value */
	  m += mark_root((Cell)(*tr));
	  tr++;
	}
      tr++; /* skip chain bits */
    }

  return(m);
} /* chat_mark_trail */

static int chat_mark_region(CPtr b, int len)
{
  int j, m = 0;

  while (len)
    {
      if (len > sizeof(CPtr))
        { len -= sizeof(CPtr); j = sizeof(CPtr) ; }
      else { j = len; len = 0; }
      while (j--)
        {
          m += mark_root(*b);
          b++;
        }
      b++; /* skip chain bits */
    }

  return(m); 
} /* chat_mark_region */

/*-------------------------------------------------------------------------
chat_mark_frozen_parts: see ISMM'98 paper for more understanding
for CHAT it means:
         mark the environment chain starting from the consumer
	 then mark the trail in the chat areas of this consumer
	          but be careful with early reset
	 since the failure continuations of a consumer are always also
	 failure continuations of the active computation, there is no
	 need to mark the failure continuations of the consumer
  -------------------------------------------------------------------------*/

static int chat_mark_frozen_parts(int *avail_dreg_marks)
{
  int  m, i, trlen, yvar;
  chat_init_pheader initial_pheader;
  chat_incr_pheader pheader;
  CPtr b, e, d, *tr;
  byte *cp;

  m = 0;
  initial_pheader = chat_link_headers;
  if (initial_pheader != NULL)
  do
    {
      b = (CPtr)chat_get_args_start(initial_pheader);
      /* marking of argument registers from consumer */
      m += chat_mark_region(b,chat_get_nrargs(initial_pheader));

      /* marking of heap pointer from consumer is unnecessary */
      /* as consumer gets hreg from its scheduling generator  */

      /* mark from the consumer the environment chain */
      /* it is enough to do that until the e of the leader of the consumer */
      /* we do it all the way up for now */

      b = (CPtr)(&chat_get_cons_start(initial_pheader));
      e = cp_ereg(b);
      cp = cp_cpreg(b);

      /* mark the delay list field of choice points too */
      if ((d = cp_pdreg(b)) != NULL) {
	if (slide) {
	  if (*avail_dreg_marks > 0) {
	    *avail_dreg_marks -= 1;
	    *hreg = (Cell)d;
	    heap_top++;
	    m += mark_root((Cell)d)+1; /* +1 because of the following line */
	    h_mark(hreg-heap_bot) ;
	    hreg++;
	  } else xsb_exit("Fatal: no heap space to mark Dreg of CHAT areas");
	}
	else {
	  m += mark_root((Cell)d);
	}
      }

      while ((e < ls_bot) && (cp != NULL))
	{
	  if (ls_marked(e - ls_top)) break ;
          ls_mark(e - ls_top) ;
	  yvar = *(cp-2*sizeof(Cell)+3) - 1 ;
	  m += mark_region(e-yvar,e-2) ;
	  i = (e-2) - ls_top ;
	  while (yvar-- > 1) ls_mark(i--) ;
	  cp = (byte *)e[-1] ;
	  e = (CPtr)e[0] ;
	}

      initial_pheader = initial_pheader->next_header;
    }
  while (initial_pheader != chat_link_headers);

  initial_pheader = chat_link_headers;
  if (initial_pheader != NULL)
    do
      { /* now mark the CHAT trails */
	pheader = chat_get_father(initial_pheader);
	while ((pheader != NULL) && (! chat_area_imarked(pheader)))
	  {
	    chat_imark_area(pheader);
	    tr = chat_get_tr_start(pheader);
	    trlen = chat_get_tr_length(pheader);
	    m += chat_mark_trail(tr,trlen);

	    pheader = chat_get_ifather(pheader);
	  }
	initial_pheader = initial_pheader->next_header;
      }
    while (initial_pheader != chat_link_headers);

  return(m) ;

} /* chat_mark_frozen_parts */

/*-------------------------------------------------------------------------*/

static int chat_mark_substitution_factor(void)
{
  CPtr compl_fr;
  SGFrame subg_ptr;
  CPtr region_to_mark, d;
  int  CallNumVar, m = 0;

  /* mark the substitution factor starting from the completion stack */
  compl_fr = openreg;
  while (compl_fr != COMPLSTACKBOTTOM)
    {
      /* substitution factor is now in the heap for generators */
      region_to_mark = compl_hreg(compl_fr);
      CallNumVar = int_val(cell(region_to_mark))+1; /* mark the Num too */
      while (CallNumVar--)
	m += mark_cell(region_to_mark--);

      /* we also need to mark the Dreg fields of those generator
         choice points that have been reclaimed from the CP stack */
      subg_ptr = compl_subgoal_ptr(compl_fr);
      if ((subg_cp_ptr(subg_ptr) == NULL) /* not in the CP stack anymore */ &&
	  (! is_completed(subg_ptr))) {
	if ((d = compl_pdreg(compl_fr)) != NULL) {
	  m += mark_root((Cell)d);
	}
      }
      compl_fr = prev_compl_frame(compl_fr);
    }
  return(m) ;

} /* chat_mark_substitution_factor */

#endif

/*-------------------------------------------------------------------------*/

static int mark_hreg_from_choicepoints(void)
{
  CPtr b, bprev, h;
  int  i, m;

  /* this has to happen after all other marking ! */
  /* actually there is no need to do this for a copying collector */

  b = breg;
  bprev = 0;
  m = 0;
  while (b != bprev)
    { h = cp_hreg(b) ;
      i = h - heap_bot ;
      if (! h_marked(i)) /* h from choicepoint should point to something that
      				is marked; if not, mark it now and set it
      				to something reasonable - int(666) is ok
      				although a bit scary :-)
      			  */
	{
	  h_mark(i) ;
	  m++ ;
	  cell(h) = makeint(666) ;
	}
      bprev = b; b = cp_prevbreg(b);
    }

  return(m);
} /* mark_hreg_from_choicepoints */

/*-------------------------------------------------------------------------*/

#ifdef GC

static void adapt_hreg_from_choicepoints(CPtr h)
{ CPtr b, bprev;

  /* only after copying collection */

  b = breg;
  bprev = 0;
  while (b != bprev)
    { cp_hreg(b) = h;
      bprev = b; b = cp_prevbreg(b);
    }
} /* adapt_hreg_from_choicepoints */

#endif

/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/

int mark_heap(int arity, int *marked_dregs)
{
  int avail_dreg_marks = 0, marked = 0;

  /* the following seems unnecessary, but it is not !
     mark_heap() may be called directly and not only through gc_heap() */
  slide = (flags[GARBAGE_COLLECT] == SLIDING_GC) ;

  stack_boundaries ;

  if (print_on_gc) print_all_stacks();

  if (slide) {
#ifdef CHAT
    avail_dreg_marks = MAX_DREG_MARKS;
    /* here allocate EXACT amount for completion stack: not upper bound */
    compl_marks = calloc(compl_bot - top_of_complstk,1);
    if (! compl_marks)
      xsb_exit("Not enough core to allocate chain bits for completion stack");
#endif
    /* these areas are not used in a copying collector */
    cp_marks = calloc(cp_bot - cp_top + 1,1);
    tr_marks = calloc(tr_top - tr_bot + 1,1);
    if ((! cp_marks) || (! tr_marks))
      xsb_exit("Not enough core to perform garbage collection chaining phase");
  }

  heap_marks = calloc(heap_top - heap_bot + 2 + avail_dreg_marks,1);
  ls_marks   = calloc(ls_bot - ls_top + 1,1);
  if ((! heap_marks) || (! ls_marks))
    xsb_exit("Not enough core to perform garbage collection marking phase");
 
  heap_marks += 1; /* see its free; also note that heap_marks[-1] = 0 is
		      needed for copying garbage collection see copy_block() */

  marked = mark_region(reg+1,reg+arity);
  if (delayreg != NULL) {
    marked += mark_root((Cell)delayreg);
  }

  if (slide)
    { int put_on_heap;
      put_on_heap = arity;
      marked += put_on_heap;
      while (put_on_heap > 0)
	h_mark((heap_top - put_on_heap--)-heap_bot);
    }

#ifdef CHAT
  marked += chat_mark_substitution_factor();
#endif

  marked += mark_query();

#ifdef CHAT
  marked += chat_mark_frozen_parts(&avail_dreg_marks);
  if (slide) *marked_dregs = MAX_DREG_MARKS - avail_dreg_marks;
#endif

  marked += mark_hreg_from_choicepoints();

  if (print_on_gc) print_all_stacks();

  return marked ;
} /* mark_heap */

/*-------------------------------------------------------------------------*/

static char *code_to_string(byte *pc)
{
  return((char *)(inst_table[*pc][0])) ;
} /* code_to_string */

static void print_cell(FILE *where, CPtr cell_ptr, int fromwhere)
{
  Integer index = 0 ;
  Cell cell_val ;
  CPtr p ;
  int  whereto, tag ;
  char *s = 0 ;

  cell_val = cell(cell_ptr);

  if (cell_val == 0) { fprintf(where,"null,0).\n") ; return; }

  if (fromwhere == FROM_CP) heap_top++ ; /* because the hreg in a CP
					    can be equal to heap_top
					    and pointer_from_cell tests
					    for strict inequality */
  p = pointer_from_cell(cell_val,&tag,&whereto) ;
  if (fromwhere == FROM_CP) heap_top-- ;
  switch (whereto)
    { case TO_HEAP : index = p - heap_bot ; s = "ref_heap" ; break ;
      case TO_NOWHERE : index = (Integer)p ; s = "ref_nowhere" ; break ;
      case TO_LS : index = ls_bot - p ; s = "ref_ls" ; break ;
      case TO_CP : index = cp_bot - p ; s = "ref_cp" ; break ;
      case TO_TR : index = p - tr_bot ; s = "ref_tr" ; break ;
      case TO_COMPL : index = p - compl_bot ; s = "ref_compl" ; break ;
    }
  switch (tag)
    { case REF : case REF1 :
	if (p == NULL) fprintf(where,"null,0).\n") ;
	else
        if (p == cell_ptr) fprintf(where,"undef,_).\n") ;
        else
	{ switch (whereto)
	    {
	    case TO_HEAP :
	    case TO_LS :
	    case TO_TR :
	    case TO_CP :
	      fprintf(where,"%s,%ld).\n",s,(long)index) ;
	      break ;
	    case TO_COMPL :
	      fprintf(where,"%s,%ld).\n",s,(long)index) ;
	      break ;
	    case TO_NOWHERE:
	      if (points_into_heap(p))
		{
		  index = (p-heap_bot) ;
		  fprintf(where,"between_h_ls,%ld/%p,_) .\n",(long)index,p) ;
		}
	      else
		if ((Integer)cell_val < 10000)
		  fprintf(where,"strange,%ld).\n",(long)cell_val) ;
		else
		  if (fromwhere == FROM_HEAP)
		    fprintf(where,"funct,'%s'/%d).\n",
			    get_name((Psc)(cell_val)),
			    get_arity((Psc)(cell_val))) ;
		  else
		    if ((fromwhere == FROM_LS) || (fromwhere == FROM_CP))
		      {
			char *s ;
		        if ((tr_bot < (CPtr)cell_val) &&
			    ((CPtr)cell_val < cp_bot))
			  fprintf(where,"between_trail_cp,%ld).\n",
				  (long)cell_val) ;
			else
			  {
			    s = code_to_string((byte *)cell_val) ;
			    if (s == NULL)
			      fprintf(where,"dont_know,%ld).\n",cell_val) ;
			    else fprintf(where,"code,%ld-%s).\n",cell_val,s) ;
			  }
		      }
		    else fprintf(where,"strange_ref,%ld).\n",cell_val) ;
	      break ;
	    }
	}
	break ;

      case CS :
        if (whereto == TO_NOWHERE)
	  fprintf(where,"cs-%s,%lx).\n",s,(long)index) ;
	else
	  fprintf(where,"cs-%s,%ld).\n",s,(long)index) ;
        break ;

      case LIST :
        fprintf(where,"list-%s,%ld).\n",s,(long)index) ;
        break ;

      case INT :
        fprintf(where,"int  ,%ld).\n",(long)int_val(cell_val)) ;
        break ;

      case FLOAT :
        fprintf(where,"float,%.5g).\n",float_val((Integer)cell_val)) ;
        break ;

      case STRING :
	fprintf(where,"atom ,'%s').\n",string_val(cell_val)) ;
        break ;

      default :
        fprintf(where,"strange,%ld).\n",cell_val) ;
        break ;
    }
} /* print_cell */

void print_heap(int start, int end, int add)
{
  CPtr startp, endp ;
  char buf[100] ;
  FILE *where ;

  sprintf(buf,"HEAP%d",printnum) ;
  printnum += add ;
  where = fopen(buf,"w") ;
  if (! where)
    { xsb_dbgmsg("could not open HEAP%d",printnum);
      return;
    }
  stack_boundaries ;

  if (start < 0) start = 0 ;
  startp = heap_bot + start ;
  endp = heap_bot + end ;
  if (endp > heap_top) endp = heap_top ;

  while ( startp < endp )
  { fprintf(where,"heap('%p',%6d,%s,",startp,start,pr_h_marked(startp)) ;
    print_cell(where,startp,FROM_HEAP) ;
    startp++ ; start++ ;
  }

  fclose(where) ;
} /* print_heap */

void print_ls(int add)
{
  CPtr startp, endp ;
  char buf[100] ;
  int start ;
  FILE *where ;

  sprintf(buf,"LS%d",printnum) ;
  printnum += add ;
  where = fopen(buf,"w") ;
  if (! where)
    { xsb_dbgmsg("could not open LS%d", printnum);
      return;
    }
  stack_boundaries ;

  start = 1 ;
  startp = ls_bot - 1 ;
  endp = ls_top ;

  while ( startp >= endp )
  { fprintf(where,"ls(%6d,%s,",start,pr_ls_marked(startp)) ;
    print_cell(where,startp,FROM_LS) ;
    startp-- ; start++ ;
  }

  fclose(where) ;
} /* print_ls */

void print_cp(int add)
{
  CPtr startp, endp ;
  char buf[100] ;
  int  start ;
  FILE *where ;

  sprintf(buf,"CP%d",printnum) ;
  printnum += add ;
  where = fopen(buf,"w") ;
  if (! where)
    { xsb_dbgmsg("could not open CP%d", printnum);
      return;
    }
  stack_boundaries ;

  start = 0 ;
  startp = cp_bot ;
  endp = cp_top ;

  while ( startp >= endp )
  { fprintf(where,"cp('%p',%6d,%s,",startp,start,pr_cp_marked(startp)) ;
    print_cell(where,startp,FROM_CP) ;
    fflush(where);
    startp-- ; start++ ;
  }

  fclose(where) ;
} /* print_cp */

void print_tr(int add)
{
  CPtr startp, endp ;
  int  start ;
  FILE *where ;
  char buf[100] ;

  sprintf(buf,"TRAIL%d",printnum) ;
  printnum += add ;
  where = fopen(buf,"w") ;
  if (! where)
    { xsb_dbgmsg("could not open TRAIL%d",printnum);
      return;
    }
  stack_boundaries ;

  startp = tr_bot ;
  endp = tr_top ;
  start = 0 ;

  while ( startp <= endp )
  { fprintf(where,"trail(%6d,%s,",start,pr_tr_marked(startp)) ;
    print_cell(where,startp,FROM_TR) ;
    startp++ ; start++ ;
  }

  fclose(where) ;
} /* print_tr */

void print_regs(int a,int add)
{
  CPtr startp, endp ;                                                     
  int  start ;                                                             
  FILE *where ;
  char buf[100] ;                                                         

  sprintf(buf,"REGS%d",printnum) ;                                       
  printnum += add ;
  where = fopen(buf,"w") ;
  if (! where)
    { xsb_dbgmsg("could not open REGS%d",printnum);
      return;
    }
  stack_boundaries ;      

  startp = reg+1 ;                                                       
  endp = reg+a ;                                                         
  start = 1 ;                                                             

  while (startp <= endp)                                              
    { 
      fprintf(where,"areg(%6d,",start) ;
      print_cell(where,startp,FROM_AREG) ;                              
      startp++ ; start++ ;                   
    }

  fprintf(where,"trreg = %ld\n",(long)((CPtr)trreg-tr_bot)) ;
  fprintf(where,"breg = %ld\n",(long)(cp_bot-breg)) ;
  fprintf(where,"hreg = %ld\n",(long)(hreg-heap_bot)) ;
  fprintf(where,"ereg = %ld\n",(long)(ls_bot-ereg)) ;

#if (!defined(CHAT))
  fprintf(where,"trfreg = %ld\n",(long)((CPtr)trfreg-tr_bot)) ;
  fprintf(where,"bfreg = %ld\n",(long)(cp_bot-bfreg)) ;
  fprintf(where,"hfreg = %ld\n",(long)(hfreg-heap_bot)) ;
  fprintf(where,"efreg = %ld\n",(long)(ls_bot-efreg)) ;
#endif
  fprintf(where,"ptcpreg = %ld\n",(Cell)ptcpreg) ;

  fprintf(where,"ebreg = %ld\n",(long)(ls_bot-ebreg)) ;
  fprintf(where,"hbreg = %ld\n",(long)(hbreg-heap_bot)) ;

  fprintf(where,"cpreg = %ld\n",(Cell)cpreg) ;
  fprintf(where,"pcreg = %ld\n",(Cell)pcreg) ;

  if (delayreg)
    {
      fprintf(where,"delayreg(");
      print_cell(where,(CPtr)(&delayreg),FROM_AREG);
    }
  else fprintf(where,"delayreg = %ld\n",(Cell)delayreg);

  fclose(where) ;
} /* print_regs */

void print_chat(int add)
{
#ifdef CHAT
  CPtr startp;                                                     
  FILE *where;
  chat_init_pheader initial_pheader;
  chat_incr_pheader pheader;
  char buf[100];                                                         
  int  i, j, len;

  sprintf(buf,"CHAT%d",printnum) ;                                       
  printnum += add ;
  where = fopen(buf,"w") ;                                                
  if (! where)
    { xsb_dbgmsg("could not open CHAT%d",printnum);
      return;
    }
  stack_boundaries ;      

  initial_pheader = chat_link_headers;
  if (initial_pheader == NULL)
    { fprintf(where,"no CHAT areas\n");
      return;
    }

  do
    {
      CPtr b = (CPtr)(&chat_get_cons_start(initial_pheader));

      if (is_consumer_choicepoint(b)) fprintf(where,"CHAT area of a consumer");
      else if (is_compl_susp_frame(b))
	     fprintf(where,"CHAT area of a completion suspension");
           else fprintf(where,"CHAT area of UNKNOWN TYPE");
#ifdef DEBUG
      fprintf(where," (subgoal = ");
      print_subgoal(where, (SGFrame)nlcp_subgoal_ptr(b));
      fprintf(where,")");
#endif
      fprintf(where," @ %p:\n",initial_pheader);
      fprintf(where,"----------------------\n");
      fprintf(where,"Arguments\n");
      fprintf(where,"----------------------\n");

      startp = (CPtr)chat_get_args_start(initial_pheader);
      len = chat_get_nrargs(initial_pheader);
      i = 0;
      while (len)
	{ if (len > sizeof(CPtr))
	    { j = sizeof(CPtr); len -= sizeof(CPtr); }
	  else { j = len; len = 0; }
	  while (j--)
	    { fprintf(where,"chatargs('%p',%3d,%1d,",
		      startp,i,chat_is_chained(startp));
	      print_cell(where,startp,FROM_CP);
	      startp++ ; i++;
	    }
	  startp++;
	}

      fprintf(where,"Trail\n");
      pheader = chat_get_father(initial_pheader);
      while (pheader != NULL)
	{ fprintf(where,"increment %p marked = %d\n",
		  pheader,chat_area_imarked(pheader));
	  startp = (CPtr)chat_get_tr_start(pheader);
	  len = chat_get_tr_length(pheader);

	  i = 0;
	  while (len)
	    { if (len > sizeof(CPtr))
	      { j = sizeof(CPtr); len -= sizeof(CPtr); }
	    else { j = len; len = 0; }
	    while (j--)
	      { fprintf(where,"chattrail('%p',%3d,%1d,",
			startp,i,chat_is_chained(startp));
	        print_cell(where,startp,FROM_CP);
		startp++ ; i++;
	      }
	    startp++;
	    }

	  pheader = chat_get_ifather(pheader);
	}
      fprintf(where,"End CHAT area - %p\n\n\n",initial_pheader);
      initial_pheader = initial_pheader->next_header;
    }
  while (initial_pheader != chat_link_headers);

  fclose(where) ;
#endif
} /* print_chat */

void print_all_stacks(void)
{
    printnum++ ;
    print_regs(10,0) ;
    print_heap(0,200000,0) ;
    print_ls(0) ;
    print_tr(0) ;
    print_cp(0) ;
#ifdef CHAT
    print_chat(0);
#endif
} /* print_all_stacks */

/*==========================================================================
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


/*----------------------------------------------------------------------*/
/* reallocation for CHAT areas                                          */
/*----------------------------------------------------------------------*/

#ifdef CHAT

static void chat_relocate_region(CPtr *startp, int len,
				 int heap_offset, int local_offset)
{ int j;
  Cell cell_val;

  while (len)
    { 
      if (sizeof(CPtr) > len)
	       { j = len; len = 0; }
	  else { j = sizeof(CPtr); len -= sizeof(CPtr); }

      while (j--)
	{ reallocate_heap_or_ls_pointer(startp);
	  startp++;
	}
      startp++;
    }
} /* chat_relocate_region */

/*----------------------------------------------------------------------*/

static void chat_relocate_all(CPtr heap_bot, int heap_offset,
			      CPtr ls_bot, int local_offset)
{ chat_init_pheader initial_pheader;
  chat_incr_pheader pheader;
  CPtr *b, *tr;
  int  i, trlen;
  Cell cell_val;

  initial_pheader = chat_link_headers;
  if (initial_pheader != NULL)
  {
  do
    {
      /* relocate the saved consumer choice points -- this also */
      /* taes care of their dreg field.  A more refined traversal */
      /* of choice points is possible, but is it worth it ? */

      b = (CPtr *)(&chat_get_cons_start(initial_pheader));
      for (i = NLCPSIZE; i > 0; i--)
	{ reallocate_heap_or_ls_pointer(b);
	  b++;
	}

      /* relocate the argument registers from consumer */
      b = (CPtr *)chat_get_args_start(initial_pheader);
      chat_relocate_region(b,chat_get_nrargs(initial_pheader),heap_offset,local_offset);

      /* relocate the CHAT trail */
      /* this is more tricky than expected: the marks must be used:  */
      /* a relocated pointer might point to the old area !           */
      /* and the marks have to switched off as well at the end       */
      pheader = chat_get_father(initial_pheader);
      while ((pheader != NULL) && (! chat_area_imarked(pheader)))
	{
	  chat_imark_area(pheader);
	  tr = chat_get_tr_start(pheader);
	  trlen = chat_get_tr_length(pheader);
	  chat_relocate_region(tr,trlen,heap_offset,local_offset);

	  pheader = chat_get_ifather(pheader);
	}
      initial_pheader = initial_pheader->next_header;
    }
  while (initial_pheader != chat_link_headers);

  initial_pheader = chat_link_headers;
  do
    {
      /* here the marks are switched off */
      pheader = chat_get_father(initial_pheader);
      while ((pheader != NULL) && (chat_area_imarked(pheader)))
        {
          chat_iunmark_area(pheader);
          pheader = chat_get_ifather(pheader);
        }
      initial_pheader = initial_pheader->next_header;
    }
  while (initial_pheader != chat_link_headers);
  }

  /* now relocate pointers to the heap from the completion
   * stack: the SF and Dreg fields for generators */
  { CPtr compl_fr;
    CPtr *p;

    compl_fr = openreg;
    while (compl_fr != COMPLSTACKBOTTOM)
      { /* substitution factor is now in the heap for generators */
	p = (CPtr *)(&compl_hreg(compl_fr));
	reallocate_heap_or_ls_pointer(p);
	/* relocate Dreg field too: if non-null, it points to the heap */
	p = (CPtr *)(&compl_pdreg(compl_fr));
	reallocate_heap_or_ls_pointer(p);
	compl_fr = prev_compl_frame(compl_fr);
      }
  }

} /* chat_relocate_all */

#endif /* CHAT */

/*----------------------------------------------------------------------*/

bool glstack_realloc(int new_size, int arity)
{
  CPtr   new_heap_bot ;       /* bottom of new Global Stack area */
  CPtr   new_ls_bot ;         /* bottom of new Local Stack area */

  long   heap_offset ;        /* offsets between the old and new */
  long   local_offset ;       /* stack bottoms, measured in Cells */

  CPtr   *cell_ptr ;
  Cell   cell_val ;

  size_t new_size_in_bytes, new_size_in_cells ; /* what a mess ! */
  long   expandtime ;

  if (new_size <= glstack.size) return 0;
#ifdef STACKS_DEBUG
  xsb_dbgmsg("Heap/Local Stack expansion - new size = %d  arity = %d",
	     new_size,arity) ;
#endif

  expandtime = 1000*cpu_time() ;

  new_size_in_bytes = new_size*K ;
  new_size_in_cells = new_size_in_bytes/sizeof(Cell) ;
  		/* and let's hope K stays divisible by sizeof(Cell) */

  stack_boundaries ;

  /* Expand the data area and push the Local Stack to the high end. */

  new_heap_bot = (CPtr)realloc(heap_bot, new_size_in_bytes);
  if (new_heap_bot == NULL) {
    xsb_mesg("Not enough core to resize the Heap and Local Stack!");
    return 1; /* return an error output -- will be picked up later */
  }
  heap_offset = new_heap_bot - heap_bot ;
  new_ls_bot = new_heap_bot + new_size_in_cells - 1 ;
  local_offset = new_ls_bot - ls_bot ;
  memmove(ls_top + local_offset,             /* move to */
	  ls_top + heap_offset,              /* move from */
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
#ifdef WAM_TRAIL
  for (cell_ptr = (CPtr *)top_of_trail - 1;
       cell_ptr >= (CPtr *)tcpstack.low;  /* CHAT needs: >= */
       cell_ptr--)
  { /* the address is the only thing */
    cell_val = (Cell)*cell_ptr ;
    realloc_ref(cell_ptr,(CPtr)cell_val) ;
  }
#else
  for (cell_ptr = (CPtr *)top_of_trail - 1;
       cell_ptr > (CPtr *)tcpstack.low;
       cell_ptr = cell_ptr - 2)
  { /* first the value */
    reallocate_heap_or_ls_pointer(cell_ptr);
    /* now the address */
    cell_ptr-- ;
    cell_val = (Cell)*cell_ptr ;
    realloc_ref(cell_ptr,(CPtr)cell_val) ;
  }
#endif

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

#ifdef CHAT
  chat_relocate_all(heap_bot,heap_offset,ls_bot,local_offset); 
#endif 

  /* Update the system variables */
  glstack.low = (byte *)new_heap_bot ;
  glstack.high = (byte *)(new_ls_bot + 1) ;
  glstack.size = new_size ;

  hreg = (CPtr)hreg + heap_offset ;
  hbreg = (CPtr)hbreg + heap_offset ;
#if (!defined(CHAT))
  hfreg = (CPtr)hfreg + heap_offset ;
#endif
  ereg = (CPtr)ereg + local_offset ;
  ebreg = (CPtr)ebreg + local_offset ;
#if (!defined(CHAT))
  efreg = (CPtr)efreg + local_offset ;
#endif

  if (islist(delayreg))
    delayreg = (CPtr)makelist(clref_val(delayreg) + heap_offset);

  expandtime = 1000*cpu_time() - expandtime;

#ifdef STACKS_DEBUG
  xsb_dbgmsg("Heap/Local Stack expansion - finished in %ld msecs\n",
	     expandtime) ;
#endif

  return 0;
} /* glstack_realloc */

/*=======================================================================*/

/* from here to end of slide_heap is code taken to some extent from
   BinProlog and adapted to XSB - especially what concerns the
   environments
   the BinProlog garbage collector was also written originally by Bart Demoen
*/

#ifdef GC

#define h_set_chained(p)	 heap_marks[(p-heap_bot)] |= CHAIN_BIT
#define h_set_unchained(p)	 heap_marks[(p-heap_bot)] &= ~CHAIN_BIT
#define h_is_chained(p)		 (heap_marks[(p-heap_bot)] & CHAIN_BIT)

#define ls_set_chained(p)        ls_marks[(p-ls_top)] |= CHAIN_BIT 
#define ls_set_unchained(p)      ls_marks[(p-ls_top)] &= ~CHAIN_BIT
#define ls_is_chained(p)         (ls_marks[(p-ls_top)] & CHAIN_BIT)

#define cp_set_chained(p)        cp_marks[(p-cp_top)] |= CHAIN_BIT
#define cp_set_unchained(p)      cp_marks[(p-cp_top)] &= ~CHAIN_BIT
#define cp_is_chained(p)         (cp_marks[(p-cp_top)] & CHAIN_BIT)

#define tr_set_chained(p)        tr_marks[(p-tr_bot)] |= CHAIN_BIT 
#define tr_set_unchained(p)      tr_marks[(p-tr_bot)] &= ~CHAIN_BIT
#define tr_is_chained(p)         (tr_marks[(p-tr_bot)] & CHAIN_BIT)

#ifdef CHAT
#define compl_is_chained(p)      compl_marks[(compl_bot-p)]
#define compl_set_unchained(p)   compl_marks[(compl_bot-p)] = 0
#define compl_set_chained(p)     compl_marks[(compl_bot-p)] = 1
#endif

static void unchain(CPtr hptr, CPtr destination)
{
  CPtr start, pointsto ;
  int  whereto, tag ;
  int  continue_after_this = 0 ;

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
	{
	  case TO_HEAP :
	    continue_after_this = h_is_chained(pointsto) ;
	    h_set_unchained(pointsto) ;
	    break ;
	  case TO_LS :
	    continue_after_this = ls_is_chained(pointsto) ;
	    ls_set_unchained(pointsto) ;
	    break ;
	  case TO_TR :
	    continue_after_this = tr_is_chained(pointsto) ;
	    tr_set_unchained(pointsto) ;
	    break ;
	  case TO_CP :
	    continue_after_this = cp_is_chained(pointsto) ;
	    cp_set_unchained(pointsto) ;
	    break ;
#ifdef CHAT
	  case TO_COMPL :
	    continue_after_this = compl_is_chained(pointsto) ;
	    compl_set_unchained(pointsto) ;
	    break ;
#endif
	  default :
#ifdef CHAT
	    /* we count on it being into a CHAT area - trail or arguments */
	    continue_after_this = chat_is_chained(pointsto) ;
	    chat_set_unchained(pointsto);
#else
	    xsb_exit("pointsto wrong space error during unchaining");
#endif
	    break;
	}
      *hptr = *pointsto ;
      switch (tag)
	{
	  case REF: case REF1:
	    *pointsto = (Cell)destination ;
	    break ;
	  case CS :
	    *pointsto = makecs((Cell)destination) ;
	    break ;
	  case LIST :
	    *pointsto = makelist((Cell)destination) ;
	    break ;
	  default :
	    xsb_exit("tag error during unchaining") ;
	}
    }
  while (continue_after_this) ;

} /* unchain */

/*----------------------------------------------------------------------*/

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

#endif /* GC */

/*----------------------------------------------------------------------*/

#ifdef CHAT

#ifdef SAFE_GC
static void chat_check_zero_region(CPtr b, int len)
{ int j;

  /* no need to distinguish between trail or args  */

  while (len)
    { if (len > sizeof(CPtr))
           { len -= sizeof(CPtr); j = sizeof(CPtr); }
      else { j = len; len = 0; }
      while (j--)
	{
	  if (chat_is_chained(b))
	    xsb_dbgmsg("chain bit left in chat area");
	  b++;
	}
      b++; /* skipping the chain bits */
    } 
} /* chat_check_zero_region */
#endif

static void chat_check_zero(void)
{
#ifdef SAFE_GC
  chat_init_pheader initial_pheader;
  chat_incr_pheader pheader;
  int trlen;
  CPtr b,*tr;

  if (chat_link_headers == NULL) return;

  initial_pheader = chat_link_headers;
  do
    {
      b = (CPtr)chat_get_args_start(initial_pheader);
      chat_check_zero_region(b,chat_get_nrargs(initial_pheader));

      pheader = chat_get_father(initial_pheader);
      while (pheader != NULL)
	{ tr = chat_get_tr_start(pheader);
	  trlen = chat_get_tr_length(pheader);
	  chat_check_zero_region((CPtr)tr,trlen);
	  pheader = chat_get_ifather(pheader);
	}
      initial_pheader = initial_pheader->next_header;
    }
  while (initial_pheader != chat_link_headers);
#endif
} /* chat_check_zero */

/*----------------------------------------------------------------------*/

static void chat_chain_region(CPtr b, int len)
{ int whereto, tag, j ;
  Cell contents;
  CPtr q ;

  /* no need to distinguish between trail or args chaining */

  while (len)
    { if (len > sizeof(CPtr))
      { len -= sizeof(CPtr); j = sizeof(CPtr); }
      else { j = len; len = 0; }
      while (j--)
	{ contents = cell(b);
	  q = pointer_from_cell(contents,&tag,&whereto) ;
	  if (whereto != TO_HEAP) { b++; continue ; }
	  if (! h_marked(q-heap_bot)) { xsb_mesg("panic 17"); b++; continue ; }
	  if (h_is_chained(q)) chat_set_chained(b) ;
	  h_set_chained(q) ;
	  swap_with_tag(b,q,tag) ;
	  b++;
	}
      b++; /* skipping the chain bits */
    }
} /* chat_chain_region */

#endif

/*----------------------------------------------------------------------*/
/*
	slide_heap: implements a sliding collector for the heap
	see: Algorithm of Morris / ACM paper by Appleby et al.
	num_marked = number of marked heap cells
	the relevant argument registers have been moved to the top
	of the heap prior to marking
*/

#ifdef GC

static CPtr slide_heap(int num_marked)
{
  int  tag ;
  Cell contents;
  CPtr p, q ;

  /* chain external (to heap) pointers */      

    /* chain argument registers */
    /* will be automatic as aregisters were copied to the heap */

    /* chain trail */
    /* more precise traversal of trail possible */

    { CPtr endtr ;
      endtr = tr_top ;
      for (p = tr_bot; p <= endtr ; p++ )
	{ contents = cell(p) ;
	  q = hp_pointer_from_cell(contents,&tag) ;
	  if (!q) continue ;
	  if (! h_marked(q-heap_bot)) continue ;
	  if (h_is_chained(q)) tr_set_chained(p) ;
	  h_set_chained(q) ;
	  swap_with_tag(p,q,tag) ;
	}
    }

    /* chain choicepoints */
    /* more precise traversal of choice points possible */

    { CPtr endcp ;
      endcp = cp_top ;
      for (p = cp_bot; p >= endcp ; p--)
	{ contents = cell(p) ;
	  q = hp_pointer_from_cell(contents,&tag) ;
	  if (!q) continue ;
	  if (! h_marked(q-heap_bot))
	    { xsb_dbgmsg("not marked from cp"); continue ; }
	  if (h_is_chained(q)) cp_set_chained(p) ;
	  h_set_chained(q) ;
	  swap_with_tag(p,q,tag) ;
	}
    }

    /* chain the substitution factors reachable - for CHAT	  */
    /* substitution factors of generators are in the heap and are */
    /* pointed by a field of the completion stack; the substitution */
    /* factors of the consumers are in the choice point stack     */
    /* so they have just been chained already                     */
#ifdef CHAT
    { CPtr compl_fr;

      compl_fr = openreg;
      while (compl_fr != COMPLSTACKBOTTOM)
	{ /* substitution factor is now in the heap for generators */
	  p = (CPtr)(&compl_hreg(compl_fr));
	  contents = cell(p);
	  q = hp_pointer_from_cell(contents,&tag);
	  if (!q) xsb_dbgmsg("bad heap pointer during chaining SF");
	  if (! h_marked(q-heap_bot)) xsb_dbgmsg("chain SF problem");
	  if (h_is_chained(q)) compl_set_chained(p);
	  h_set_chained(q);
	  swap_with_tag(p,q,tag);

	  /* we also need to adapt the Dreg fields */
	  if (compl_pdreg(compl_fr) != NULL) {
	    p = (CPtr)(&(compl_pdreg(compl_fr)));
	    contents = cell(p);
	    q = hp_pointer_from_cell(contents,&tag);
	    if (!q)
	      xsb_dbgmsg("bad heap pointer during chaining Dreg");
	    if (! h_marked(q-heap_bot)) xsb_dbgmsg("chain Dreg problem");
	    if (h_is_chained(q)) compl_set_chained(p);
	    h_set_chained(q);
	    swap_with_tag(p,q,tag);
	  }
	  compl_fr = prev_compl_frame(compl_fr);
	}
    }
#endif

    /* chain local stack */
    /* more precise traversal of local stack possible */

    { CPtr endls ;
      endls = ls_top ;
      for (p = ls_bot; p >= endls ; p-- )
	{
	  if (! ls_marked(p-ls_top)) continue ;
	  ls_clear_mark((p-ls_top)) ; /* chain bit cannot be on yet */
	  contents = cell(p) ;
	  q = hp_pointer_from_cell(contents,&tag) ;
	  if (!q) continue ;
	  if (! h_marked(q-heap_bot)) continue ;
	  if (h_is_chained(q)) ls_set_chained(p) ;
	  h_set_chained(q) ;
	  swap_with_tag(p,q,tag) ;
	}
    }

#ifdef CHAT
    /* chain CHAT areas */

    if (chat_link_headers != NULL)
      { chat_init_pheader initial_pheader;
        chat_incr_pheader pheader;
	int trlen;
	CPtr b,*tr;

	initial_pheader = chat_link_headers;
	do
	  {
	    b = (CPtr)chat_get_args_start(initial_pheader);
	    /* chaining of argument registers from consumer */
	    chat_chain_region(b,chat_get_nrargs(initial_pheader));

	    /* chaining of heap pointer from consumer is unnecessary */

	    /* because of how chaining of ls is done above, no need to   */
	    /* do chaining of the ls that is reachable from the consumer */
	    /* revision might be needed                                  */

	    /* chaining of the CHAT trails */
	    /* during which the imarkbit is switched off */

	    pheader = chat_get_father(initial_pheader);
	    while ((pheader != NULL) && chat_area_imarked(pheader))
	      {
		chat_iunmark_area(pheader);
		tr = chat_get_tr_start(pheader);
		trlen = chat_get_tr_length(pheader);
		chat_chain_region((CPtr)tr,trlen);
		pheader = chat_get_ifather(pheader);
	      }
	    initial_pheader = initial_pheader->next_header;
	  }
	while (initial_pheader != chat_link_headers);
      }
#endif

    /* if (print_on_gc) print_all_stacks() ; */

  { CPtr destination, hptr ;
    long garbage = 0 ;
    int index ;

    /* one phase upwards - from top of heap to bottom of heap */

    index = heap_top - heap_bot ;
    destination = heap_bot + num_marked - 1 ;
    for (hptr = heap_top - 1 ; hptr >= heap_bot ; hptr--)
      {
	if (h_marked(hptr - heap_bot))
	{ /* boxing */
	  if (garbage)
	    { *(hptr+1) = makeint(garbage) ;
	      garbage = 0 ;
	    }
	  if (h_is_chained(hptr))
	    { unchain(hptr,destination) ; }
	  p = hp_pointer_from_cell(*hptr,&tag) ;            
	  if (p && (p < hptr))
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

    if (garbage)
      /* the first heap cell is not marked */
      *heap_bot = makeint(garbage) ;

    /* if (print_on_gc) print_all_stacks(); */

    /* one phase downwards - from bottom of heap to top of heap */
    index = 0 ;
    destination = heap_bot ;
    for (hptr = heap_bot ; hptr < heap_top ; )
      {
	if (h_marked(hptr - heap_bot))
	{
	  if (h_is_chained(hptr))
	    { unchain(hptr,destination) ; }
	  if ((Cell)(hptr) == *hptr) /* UNDEF */
	    bld_free(destination) ;
	  else
	    {
	      p = hp_pointer_from_cell(*hptr,&tag) ;
	      *destination = *hptr ;
	      if (p && (p > hptr))
		{
		  swap_with_tag(destination,p,tag) ;
		  if (h_is_chained(p))           
		    h_set_chained(destination) ;   
		  else h_set_chained(p) ;
		}
	    }
	  h_clear_mark((hptr-heap_bot)) ;
	  hptr++ ; destination++ ;
	  index++ ;
	}
      else
	{
	  garbage = int_val(cell(hptr)) ;
	  index += garbage ;
	  hptr += garbage ;
	}
      }
    if (destination != (heap_bot+num_marked))
      xsb_dbgmsg("bad size %p  %p",
		 destination,heap_bot+num_marked);
  }

    return(heap_bot + num_marked) ;

} /* slide_heap */

static void check_zero(char *b, int l, char *s)
{ 
#ifdef SAFE_GC
  int i = 0 ;
  while (l--)
  {
    if (*b++)
      xsb_dbgmsg("%s - left marker - %d - %d - %d", s,*(b-1),i,l) ;
    i++ ;
  }
#endif
} /* check_zero */

#endif

/*=======================================================================*/
/*  Heap collection by copying                                           */
/*  Marking is finished and new heap allocated                           */
/*  Idea is to copy a la Cheney and copy immediatly back to original     */
/*          heap - this allows release of the new heap                   */
/*          Cheney is slightly adapted to make it possible to have       */
/*          the copy back as a mem copy; so heap pointers during Cheney  */
/*          will already point to the final destination                  */
/*                                                                       */
/*  It is very similar to the BinProlog garbage collector.               */
/*=======================================================================*/

#ifdef GC

/*#define GC_DEBUG*/

#ifdef GC_DEBUG
static void CHECK(CPtr p)
{ CPtr q;
  q = (CPtr)(*p);
  if (((heap_bot - offset) <= q) && (q < next)) return;
  xsb_dbgmsg("really bad thing discovered");
} /* CHECK */
#define GCDBG(mes,val) /*if (num_gc == 61)*/ fprintf(stddbg,mes,val)
#else
#define CHECK(p)
#define GCDBG(mes,val)
#endif

/* the following variables are set by copy_heap() and used as */
/* globals in the two functions below.                        */

static int offset;
static CPtr scan, next;

#define adapt_external_heap_pointer(P,Q,TAG) \
    CHECK(Q);\
    GCDBG("Adapting %p ", P); GCDBG("with %p ", Q);\
    Q = (CPtr)((CPtr)(cell(Q))+offset); \
    if (TAG == REF || TAG == REF1) {\
      bld_ref(P, Q); \
    } else {\
      cell(P) = (Cell)(enc_addr(Q) | TAG); \
    } \
    GCDBG("to %lx\n", cell(P))

#define copy_block(HP,NEXT) /* make sure this macro does not modify HP ! */\
    i = HP-heap_bot; \
    while (h_marked(--i)) ; /* assumes that h_marked[-1] = 0 !!! */\
    /* while (--i >= 0 && h_marked(i)) ; otherwise */\
    p = heap_bot+i+1;\
    for (i = p-heap_bot; h_marked(i); p++, i++) { \
      h_clear_mark(i); \
      cell(NEXT) = cell(p); \
      cell(p) = (Cell)(NEXT); /* leave a forwarding pointer */\
      NEXT++; \
    }

static void find_and_copy_block(CPtr hp)
{
    int  i, tag;
    CPtr p, q, addr;

    /* copy the block into the new heap area */
    copy_block(hp,next);

    /* perform a Cheney scan: pointer "scan" chases the "next" pointer  */
    /* note that "next" is modified inside the for loop by copy_block() */
    for ( ; scan < next; scan++) {
      q = (CPtr)cell(scan);
      tag = cell_tag(q);
      switch (tag)
      { case REF : case REF1 :
	  if (points_into_heap(q)) {
	    GCDBG("Reference to heap with tag %d\n", tag);
#ifdef GC_DEBUG
	    fprintf(stddbg, "In adapting case for %p with %p (%lx)...",
		    scan, q, cell(q));
#endif
	   if (h_marked(q-heap_bot)) {
	     copy_block(q,next);
	   }
	   q = (CPtr)((CPtr)(cell(q))+offset);
	   GCDBG(" to be adapted to %p\n", q);
	   bld_ref(scan, q);
	  }
	  break;
        case CS :
	  addr = (CPtr)cs_val(q);
	  GCDBG("Structure pointing to %p found...\n", addr);
	  if (h_marked(addr-heap_bot)) { /* if structure not already copied */
	    copy_block(addr,next); /* this modifies *addr */
	  }
	  CHECK(addr);
	  GCDBG("*p = %lx ", cell(addr));
	  addr = (CPtr)((CPtr)(cell(addr))+offset);
	  GCDBG("q = %p ", addr);
	  bld_cs(scan, addr);
	  GCDBG("made to point to %lx\n", cell(scan));
          break;
        case LIST :
	  addr = clref_val(q);
	  GCDBG("List %p found... \n", addr);
	  if (h_marked(addr-heap_bot)) { /* if list head not already copied */
	    copy_block(addr,next); /* this modifies *addr */
	  }
	  CHECK(addr);
	  addr = (CPtr)((CPtr)(cell(addr))+offset);
	  bld_list(scan, addr);
          break;
        default :
	  break;
      }
    }
} /* find_and_copy_block */

#endif /* GC */

/*=======================================================================*/

#ifdef CHAT
static void chat_copy_region(CPtr p, int len)
{
  CPtr q;
  int  j, tag;
  Cell contents;

  while (len)
    {
      if (len > sizeof(CPtr))
        { len -= sizeof(CPtr); j = sizeof(CPtr) ; }
      else { j = len; len = 0; }
      while (j--)
        { contents = cell(p);
          q = hp_pointer_from_cell(contents,&tag) ;
          if (!q) { p++; continue ; }
          if (h_marked(q-heap_bot)) { find_and_copy_block(q); }
          adapt_external_heap_pointer(p,q,tag);
          p++;
        }
      p++; /* skip chain bits */
    }
} /* chat_copy_region */
#endif

/*=======================================================================*/

#ifdef GC

static CPtr copy_heap(int marked, CPtr begin_new_h, CPtr end_new_h, int arity)
{
    CPtr p, q;
    int  tag; 
    Cell contents;

    offset = heap_bot-begin_new_h;
    scan = next = begin_new_h; 

#ifdef GC_DEBUG
    xsb_dbgmsg("New heap space between %p and %p", begin_new_h,end_new_h);
#endif

  /* the order in which stuff is copied might be important                 */
  /* but like the price of a ticket from Seattle to New York: nobody knows */

  /* trail */
  /* more precise traversal of trail possible */

    { CPtr endtr ;
      endtr = tr_top ;
      for (p = tr_bot; p <= endtr; p++)
	{ contents = cell(p);
          q = hp_pointer_from_cell(contents,&tag) ;
	  if (!q) continue ;
	  if (h_marked(q-heap_bot)) { find_and_copy_block(q); }
	  adapt_external_heap_pointer(p,q,tag);
	}
    }

  /* choicepoints */
  /* a more precise traversal of choicepoints is possible */

    { CPtr endcp ;
      endcp = cp_top ;
      for (p = cp_bot; p >= endcp ; p--)
	{ contents = cell(p) ;
	  q = hp_pointer_from_cell(contents,&tag) ;
	  if (!q) continue ;
	  if (h_marked(q-heap_bot)) { find_and_copy_block(q); }
	  adapt_external_heap_pointer(p,q,tag);
	}
    }

#ifdef CHAT
  /* copy & adapt substitution factors reachable - they are pointed by a
     field of completion stack; the substitution factors of the consumers 
     are in the choice point stack and they have just been treated */

    { CPtr compl_fr;
      compl_fr = openreg;
      while (compl_fr != COMPLSTACKBOTTOM)
	{ /* CHAT stores the substitution factor of generators in the heap */
	  p = (CPtr)(&compl_hreg(compl_fr));
	  contents = cell(p) ;
	  q = hp_pointer_from_cell(contents,&tag) ;
	  if (!q)
	    xsb_dbgmsg("bad heap pointer during copying SF");
	  if (h_marked(q-heap_bot)) { find_and_copy_block(q); }
	  adapt_external_heap_pointer(p,q,tag);

	  /* we also need to adapt the Dreg fields */
	  if (compl_pdreg(compl_fr) != NULL) {
	    p = (CPtr)(&(compl_pdreg(compl_fr)));
	    contents = cell(p) ;
	    q = hp_pointer_from_cell(contents,&tag) ;
	    if (!q)
	      xsb_mesg("non null Dreg field in ComplStack points not in heap");
	    else
	      {
		if (h_marked(q-heap_bot)) { find_and_copy_block(q); }
		adapt_external_heap_pointer(p,q,tag);
	      }	    
	  }
	  compl_fr = prev_compl_frame(compl_fr);
	}
    }
#endif

  /* local stack */
  /* a more precise traversal of local stack is possible */

    { CPtr endls;
      endls = ls_top ;
      for (p = ls_bot; p >= endls ; p-- )
	{ if (! ls_marked(p-ls_top)) continue ;
          ls_clear_mark((p-ls_top)) ;
	  contents = cell(p) ;
	  q = hp_pointer_from_cell(contents,&tag) ;
	  if (!q) continue ;
	  if (h_marked(q-heap_bot)) { find_and_copy_block(q); }
	  adapt_external_heap_pointer(p,q,tag);
	}
    }

#ifdef CHAT
  /* CHAT areas */

    if (chat_link_headers != NULL)
    { chat_init_pheader initial_pheader;
      chat_incr_pheader pheader;
      int  trlen;
      CPtr b, *tr;

      initial_pheader = chat_link_headers;
      do
	{
	  b = (CPtr)chat_get_args_start(initial_pheader);
	  /* copying of argument registers from consumer */
	  chat_copy_region(b,chat_get_nrargs(initial_pheader));

	  /* copying of heap pointer from consumer is unnecessary */

	  /* because of how copying of ls is done above, no need to   */
	  /* do copying of the ls that is reachable from the consumer */
	  /* revision might be needed                                 */

	  /*----------------------------------------------------------*/
	  /* adapt the delay list field of choice points too */
	  b = (CPtr)(&chat_get_cons_start(initial_pheader));
	  if (cp_pdreg(b) != NULL) {
	    p = (CPtr)(&(cp_pdreg(b)));
	    contents = cell(p) ;
	    q = hp_pointer_from_cell(contents,&tag) ;
	    if (!q)
	      xsb_mesg("non null Dreg field in CP stack points not in heap");
	    else
	      {
		if (h_marked(q-heap_bot)) { find_and_copy_block(q); }
		adapt_external_heap_pointer(p,q,tag);
	      }
	  }
	  /*----------------------------------------------------------*/

	  /* copying of the CHAT trails */
	  /* during which the imarkbit is switched off */

	  pheader = chat_get_father(initial_pheader);
	  while ((pheader != NULL) && chat_area_imarked(pheader))
	  { chat_iunmark_area(pheader);
	    tr = chat_get_tr_start(pheader);
	    trlen = chat_get_tr_length(pheader);
	    chat_copy_region((CPtr)tr,trlen);
	    pheader = chat_get_ifather(pheader);
	  }
	  initial_pheader = initial_pheader->next_header;
	}
      while (initial_pheader != chat_link_headers);
    }
#endif

    /* now do the argument registers */

    { CPtr p;
      for (p = reg+1; arity-- > 0; p++)
        { contents = cell(p) ;
          q = hp_pointer_from_cell(contents,&tag) ;
          if (!q) continue ;
          if (h_marked(q-heap_bot)) { find_and_copy_block(q); }
          adapt_external_heap_pointer(p,q,tag);
        }
    }

    /* now do the delay register */

    { CPtr p;

      if (delayreg != NULL)
	{ 
	  p = (CPtr)(&delayreg);
	  contents = cell(p) ;
          q = hp_pointer_from_cell(contents,&tag) ;
          if (!q)
	    xsb_dbgmsg("non null delayreg points not in heap");
          else
	    {
	      if (h_marked(q-heap_bot)) { find_and_copy_block(q); }
	      adapt_external_heap_pointer(p,q,tag);
	    }
        }
    }

    if (next != end_new_h)
      xsb_dbgmsg("heap copy gc - inconsistent hreg: %d cells not copied...",
		 (end_new_h-next));

    memcpy((void *)heap_bot, (void *)begin_new_h, marked*sizeof(Cell));

    return(heap_bot+marked);
} /* copy_heap */

#endif /* GC */

/*======================================================================*/
/* global variables used for statistics.                                */
/*======================================================================*/

static int total_time_gc = 0 ;
static int total_collected = 0 ;

/*======================================================================*/
/* The main routine that performs garbage collection.                   */
/*======================================================================*/

int gc_heap(int arity)
{

#ifdef GC
  CPtr p;
  int  begin_marktime,
#ifdef VERBOSE_GC
    end_marktime, begin_slidetime, begin_copy_time,
#endif
    end_slidetime, end_copy_time;
  int  marked = 0, marked_dregs = 0, i;

  if (flags[GARBAGE_COLLECT] != NO_GC) {

    num_gc++ ;

    slide = (flags[GARBAGE_COLLECT] == SLIDING_GC) ;

    if (fragmentation_only) slide = FALSE;
    heap_early_reset = ls_early_reset = 0;

#ifdef VERBOSE_GC
    xsb_dbgmsg("Heap gc - arity = %d - used = %d - left = %d - #gc = %d",
	       arity,hreg+1-(CPtr)glstack.low,ereg-hreg,num_gc) ;

#endif
    begin_marktime = 1000*cpu_time() ;

    total_collected += hreg+1-(CPtr)glstack.low;

    /* make sure the top choice point heap pointer 
       that might not point into heap, does */
    if (hreg == cp_hreg(breg))
      {
	*hreg = makeint(666) ;
	hreg++ ;
      }

    /* copy the aregs to the top of the heap - only if sliding */
    /* just hope there is enough space */
    /* this happens best before the stack_boundaries are computed */
    if (slide)
      {
	if (delayreg != NULL) {
	  arity++;
	  reg[arity] = (Cell)delayreg;
	}
	for (i = 1; i <= arity; i++ )
	  *hreg++ = reg[i] ;
      }

    marked = mark_heap(arity, &marked_dregs) ;

#ifdef VERBOSE_GC
    end_marktime = 1000*cpu_time() ;
#endif

    if (fragmentation_only)
      {
	/* fragmentation is expressed as ratio not-marked/total heap in use
	   this is internal fragmentation only.  we print marked and total,
	   so that postprocessing can do what it wants with this info. */
	xsb_dbgmsg("marked_used_missed(%d,%d,%d,%d).",
		   marked,hreg+1-(CPtr)glstack.low,
		   heap_early_reset,ls_early_reset);

	/* get rid of the marking areas - if they exist */
	if (heap_marks)  { free((heap_marks-1)); heap_marks = NULL; }
	if (tr_marks)    { free(tr_marks); tr_marks = NULL; }
	if (ls_marks)    { free(ls_marks); ls_marks = NULL; }
	if (cp_marks)    { free(cp_marks); cp_marks = NULL; }
#ifdef CHAT
	if (compl_marks) { free(compl_marks); compl_marks = NULL; }
#endif
	return(TRUE);
      }

#ifdef VERBOSE_GC
    xsb_dbgmsg("Heap gc - marking finished - #marked = %d - start compact",
	       marked);
#endif

    total_collected -= marked;
    if (slide)
      {
#ifdef VERBOSE_GC
	begin_slidetime = end_marktime;
#endif
	hreg = slide_heap(marked) ;
	if (hreg != (heap_bot+marked))
	  xsb_dbgmsg("heap sliding gc - inconsistent hreg");
#ifdef CHAT
	hreg -= marked_dregs;
	{
	  CPtr b, p = hreg;
	  chat_init_pheader initial_pheader = chat_link_headers;
	  
	  if (initial_pheader != NULL)
	    do
	      { /* loop over CHAT areas to restore Dreg fields */
		b = (CPtr)(&chat_get_cons_start(initial_pheader));
		if (cp_pdreg(b) != NULL)
		  cp_pdreg(b) = (CPtr)*p++ ;
		initial_pheader = initial_pheader->next_header;
	      }
	    while (initial_pheader != chat_link_headers);
	}
#endif
	/* copy the aregs from the top of the heap back */
	hreg -= arity;
	hbreg = cp_hreg(breg);

	p = hreg;

	for (i = 1; i <= arity; i++)
	  reg[i] = *p++ ;
	if (delayreg != NULL)
	  delayreg = (CPtr)reg[arity--];

	end_slidetime = 1000*cpu_time();

	total_time_gc += (end_slidetime - begin_marktime);
#ifdef VERBOSE_GC
	xsb_dbgmsg("Heap gc end - mark time = %d; slide time = %d; total = %d\n",
		   (end_marktime - begin_marktime),
		   (end_slidetime - begin_slidetime),
		   total_time_gc) ;
#endif
      }
    else
      { /* else we call the copying collector a la Cheney */
	CPtr begin_new_heap, end_new_heap;

#ifdef VERBOSE_GC
	begin_copy_time = end_marktime;
#endif
	begin_new_heap = (CPtr)malloc(marked*sizeof(Cell));
	if (begin_new_heap == NULL)
	  xsb_exit("copying garbage collection could not allocate new heap");
	end_new_heap = begin_new_heap+marked;
	hreg = copy_heap(marked,begin_new_heap,end_new_heap,arity);
	free(begin_new_heap);
	adapt_hreg_from_choicepoints(hreg);
	hbreg = cp_hreg(breg);
	end_copy_time = 1000*cpu_time();

	total_time_gc += (end_copy_time - begin_marktime);
#ifdef VERBOSE_GC
	xsb_dbgmsg("Heap gc end - mark time = %d; copy_time = %d; total = %d\n",
		   (end_marktime - begin_marktime),
		   (end_copy_time - begin_copy_time),
		   total_time_gc) ;
#endif
      }

    if (print_on_gc) print_all_stacks();

    /* get rid of the marking areas - if they exist */
    if (heap_marks)  { check_zero(heap_marks,(heap_top - heap_bot),"heap") ;
                       free((heap_marks-1)) ; /* see its calloc */
		       heap_marks = NULL ;
                     }
    if (tr_marks)    { check_zero(tr_marks,(tr_top - tr_bot + 1),"tr") ;
                       free(tr_marks) ;
		       tr_marks = NULL ;
                     }
    if (ls_marks)    { check_zero(ls_marks,(ls_bot - ls_top + 1),"ls") ;
                       free(ls_marks) ;
		       ls_marks = NULL ;
                     }
    if (cp_marks)    { check_zero(cp_marks,(cp_bot - cp_top + 1),"cp") ;
                       free(cp_marks) ;
		       cp_marks = NULL ;
                     }
#ifdef CHAT
    if (compl_marks) { i = (compl_bot - top_of_complstk);
                       check_zero(compl_marks,i,"compl") ;
                       free(compl_marks) ;
		       compl_marks = NULL ;
                     }
    chat_check_zero();
#endif

#ifdef SAFE_GC
    p = hreg;
    while (p < heap_top)
      *p++ = 0;
#endif

  } /* if (flags[GARBAGE_COLLECT]) */
#else
  /* for non-CHAT, there is no gc, but stack expansion can be done */
#endif

  return(TRUE);

} /* gc_heap */

/*--------------------------------------------------------------------------*/

void print_gc_statistics(void)
{
  char *which = (slide) ? "sliding" : "copying" ;

  printf("  %4d heap garbage collections by %s: collected %d cells in %d millisecs\n\n",
	 num_gc, which, total_collected, total_time_gc);
}

/*--------------------------------------------------------------------------*/
