/* File:      gc_mark.h
** Author(s): Luis Castro, Bart Demoen, Kostis Sagonas
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

#ifdef INDIRECTION_SLIDE
#define TO_BUFFER(ptr) \
{ \
  if (slide_buffering) { \
    slide_buf[slide_top] = ptr; \
    slide_top++; \
    slide_buffering = slide_top <= slide_buf_size; \
  } \
}
#else
#define TO_BUFFER(ptr)
#endif

#ifdef GC_PROFILE
#define h_mark(i) \
do { \
  CPtr cell_ptr; int place;\
  place = i;\
  cell_ptr = (CPtr) heap_bot + place;\
  inspect_ptr(cell_ptr);  \
  heap_marks[place] |= MARKED;\
} while (0)
#else
#define h_mark(i)          heap_marks[i] |= MARKED
#endif

#define h_marked(i)        (heap_marks[i])
#define h_clear_mark(i)	   heap_marks[i] &= ~MARKED

#define ls_marked(i)       (ls_marks[i])
#ifdef GC_PROFILE
#define ls_mark(i)         \
do { \
  int tag, place; \
  CPtr ptr; \
  place = i; \
  ptr = (CPtr) ls_top + place; \
  tag = cell_tag(*ptr); \
  inspect_chain(ptr); \
  ls_marks[place] |= MARKED; \
} while (0)
#else
#define ls_mark(i)         ls_marks[i] |= MARKED
#endif
#define ls_clear_mark(i)   ls_marks[i] = 0

#define tr_marked(i)         (tr_marks[i])
#define tr_mark(i)           tr_marks[i] |= MARKED
#define tr_clear_mark(i)     tr_marks[i] &= ~MARKED
#define tr_mark_pre(i)       tr_marks[i] |= TRAIL_PRE
#define tr_clear_pre_mark(i) tr_marks[i] &= ~TRAIL_PRE
#define tr_pre_marked(i)     (tr_marks[i] & TRAIL_PRE)

#define cp_marked(i)       (cp_marks[i])
#define cp_mark(i)         cp_marks[i] |= MARKED
#define cp_clear_mark(i)   cp_marks[i] &= ~MARKED

/*=========================================================================*/

#ifdef GC
inline static CPtr hp_pointer_from_cell(Cell cell, int *tag)
{
  int t;
  CPtr retp;

  t = cell_tag(cell) ;

  /* the use of if-tests rather than a switch is for efficiency ! */
  /* as this function is very heavily used - do not modify */
  if (t == XSB_LIST)
    {
      *tag = XSB_LIST;
      retp = clref_val(cell);
      testreturnit(retp);
    }
  if (t == XSB_STRUCT)
    {
      *tag = XSB_STRUCT;
      retp = (CPtr)(cs_val(cell));
      testreturnit(retp);
    }
  if ((t == XSB_REF) || (t == XSB_REF1))
    {
      *tag = t;
      retp = (CPtr)cell ;
      if (points_into_heap(retp)) return(retp);
    }
  if (t == XSB_ATTV)
    {
      *tag = XSB_ATTV;
      retp = clref_val(cell);
      testreturnit(retp);
    }

  return NULL;
} /* hp_pointer_from_cell */
#endif

static CPtr pointer_from_cell(Cell cell, int *tag, int *whereto)
{ int t ;
 CPtr retp ;

 *tag = t = cell_tag(cell) ;
 switch (t)
   {
   case XSB_REF:
   case XSB_REF1: 
     retp = (CPtr)cell ;
     break ;
   case XSB_LIST: 
   case XSB_ATTV:
     retp = clref_val(cell) ;
     break ;
   case XSB_STRUCT:
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

inline static char * pr_h_marked(CPtr cell_ptr)
{ int i ;
 i = cell_ptr - heap_bot ;
 if (heap_marks == NULL) return("not_m") ;
 if (h_marked(i) == MARKED) return("marked") ;
 if (h_marked(i) == CHAIN_BIT) return("chained") ;
 if (h_marked(i) == (CHAIN_BIT | MARKED)) return("chained+marked") ;
 return("not_m") ;
} /* pr_h_marked */

inline static char * pr_ls_marked(CPtr cell_ptr) 
{ int i ; 
 i = cell_ptr - ls_top ;
 if (ls_marks == NULL) return("not_m") ;
 if (ls_marked(i) == MARKED) return("marked") ;
 if (ls_marked(i) == CHAIN_BIT) return("chained") ;
 if (ls_marked(i) == (CHAIN_BIT | MARKED)) return("chained+marked") ;
 return("not_m") ; 
} /* pr_ls_marked */ 

inline static char * pr_cp_marked(CPtr cell_ptr) 
{ int i ; 
 i = cell_ptr - cp_top ;
 if (cp_marks == NULL) return("not_m") ;
 if (cp_marked(i) == MARKED) return("marked") ;
 if (cp_marked(i) == CHAIN_BIT) return("chained") ;
 if (cp_marked(i) == (CHAIN_BIT | MARKED)) return("chained+marked") ;
 return("not_m") ; 
} /* pr_cp_marked */ 

inline static char * pr_tr_marked(CPtr cell_ptr) 
{ int i ; 
 i = cell_ptr - tr_bot ;
 if (tr_marks == NULL) return("not_m") ;
 if (tr_marked(i) == MARKED) return("marked") ;
 if (tr_marked(i) == CHAIN_BIT) return("chained") ;
 if (tr_marked(i) == (CHAIN_BIT | MARKED)) return("chained+marked") ;
 if (tr_marked(i) == (CHAIN_BIT | MARKED | TRAIL_PRE)) 
   return("chained+marked+pre");
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
  Integer i;
  int  m, arity, tag ;
  int  mark_top = 0 ;
  CPtr mark_stack[MAXS+MAX_ARITY+1] ;

  m = 0 ;
 mark_more:
  if (!points_into_heap(cell_ptr)) /* defensive marking */
    goto pop_more ;
 safe_mark_more:
  i = cell_ptr - heap_bot ;
  if (h_marked(i)) goto pop_more ;
  TO_BUFFER(cell_ptr);
  h_mark(i) ;
  m++ ;

  cell_val = *cell_ptr;
  tag = cell_tag(cell_val);


  if (tag == XSB_LIST || tag == XSB_ATTV)
    { cell_ptr = clref_val(cell_val) ;
    if (mark_overflow)
      { m += mark_cell(cell_ptr+1) ; }
    else push_to_mark(cell_ptr+1) ;
    goto safe_mark_more ;
    }

  if (tag == XSB_STRUCT)
    { p = (CPtr)cell_val ;
    cell_ptr = ((CPtr)(cs_val(cell_val))) ;
    i = cell_ptr - heap_bot ;
    if (h_marked(i)) goto pop_more ;
    TO_BUFFER(cell_ptr);
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

  if ((tag == XSB_REF) || (tag == XSB_REF1))
    { p = (CPtr)cell_val ;
    if (p == cell_ptr) goto pop_more ;
    cell_ptr = p ;
    goto mark_more ;
    }

 pop_more:
  if (mark_top--)
    { cell_ptr = mark_stack[mark_top] ; goto mark_more ; }
  return(m) ;

} /* mark_cell */

/*----------------------------------------------------------------------*/

static int mark_root(Cell cell_val)
{
  Integer i;
  int m, arity ;
  CPtr cell_ptr;
  int tag, whereto ;
  Cell v ;

  /* this is one of the places to be defensive while marking: an uninitialised
     cell in the ls can point to a Psc; the danger is not in following the Psc
     and mark something outside of the heap: mark_cell takes care of that; the
     dangerous thing is to mark the cell with the Psc on the heap without
     marking all its arguments */

  if (cell_val == 0) return(0) ;
  switch (cell_tag(cell_val))
    {
    case XSB_REF:
    case XSB_REF1:
      v = *(CPtr)cell_val ;
      pointer_from_cell(v,&tag,&whereto) ;
      switch (tag)
    	{ case XSB_REF: case XSB_REF1:
	  if (whereto != TO_HEAP) return(0) ;
	  break ;
    	}
      return(mark_cell((CPtr)cell_val)) ;

    case XSB_STRUCT : 
      cell_ptr = ((CPtr)(cs_val(cell_val))) ;
      if (!points_into_heap(cell_ptr)) return(0) ;
      i = cell_ptr - heap_bot ; 
      if (h_marked(i)) return(0) ; 
      /* now check that at i, there is a Psc */
      v = *cell_ptr ;
      pointer_from_cell(v,&tag,&whereto) ;
      /* v must be a PSC - the following tries to test this */
      switch (tag) {
      case XSB_REF: 
      case XSB_REF1 :
	if (whereto != TO_NOWHERE) return(0) ;
	break ;
	/* default: return(0); */
      }
      TO_BUFFER(cell_ptr);
      h_mark(i) ; m = 1 ; 
      cell_val = *cell_ptr;
      arity = get_arity((Psc)(cell_val)) ;
      while (arity--) m += mark_cell(++cell_ptr) ;
      return(m) ;
      
    case XSB_LIST: 
    case XSB_ATTV:
      /* the 2 cells will be marked iff neither of them is a Psc */
      cell_ptr = clref_val(cell_val) ;
      if (!points_into_heap(cell_ptr)) return(0) ;
      v = *cell_ptr ;
      pointer_from_cell(v,&tag,&whereto) ;
      switch (tag) {
      case XSB_REF:
      case XSB_REF1:
	if (whereto != TO_HEAP) return(0) ;
	break ;
      }
      v = *(++cell_ptr) ;
      pointer_from_cell(v,&tag,&whereto) ;
      switch (tag) {
      case XSB_REF:
      case XSB_REF1:
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

inline static int mark_region(CPtr beginp, CPtr endp)
{
  int marked = 0 ;

  while (beginp <= endp) {
    marked += mark_root(*(beginp++)) ;
  }

  return(marked) ;
} /* mark_region */

/*----------------------------------------------------------------------*/
inline static unsigned long mark_trail_section(CPtr begintr, CPtr endtr)
{
  CPtr a = begintr;
  CPtr trailed_cell;
  unsigned long i=0, marked=0;
#ifdef PRE_IMAGE_TRAIL
  CPtr pre_value = NULL;
#endif
  
  while (a > (CPtr)endtr)
    { 
      tr_mark(a - tr_bot); /* mark trail cell as visited */
      /* lfcastro -- needed for copying */
      tr_mark((a-tr_bot)-1);
      tr_mark((a-tr_bot)-2);

      trailed_cell = (CPtr) *(a-2);
#ifdef PRE_IMAGE_TRAIL
      if ((long) trailed_cell & PRE_IMAGE_MARK) {
	trailed_cell = (CPtr) ((Cell) trailed_cell & ~PRE_IMAGE_MARK);
	pre_value = (CPtr) *(a-3);
	tr_mark_pre((a-tr_bot)-2); /* mark somewhere else */
	*(a-2) = ((Cell)trailed_cell & ~PRE_IMAGE_MARK); /* and delete mark */
      /* lfcastro -- needed for copying */
	tr_mark((a-tr_bot)-3);
      }
#endif
      
      if (points_into_heap(trailed_cell))
	{ i = trailed_cell - heap_bot ;
	if (! h_marked(i))
	  {
#if (EARLY_RESET == 1)
	    {
	      /* instead of marking the word in the heap, 
		 we make the trail cell point to itself */
	      TO_BUFFER(trailed_cell);
	      h_mark(i) ;
	      marked++ ;
	      
#ifdef PRE_IMAGE_TRAIL
	      if (pre_value) 
		*trailed_cell = (Cell) pre_value;
	      else
#endif
		bld_free(trailed_cell); /* early reset */
	      
	      /* could do trail compaction now or later */
	      heap_early_reset++;
	    }
#else
	    {
	      marked += mark_root((Cell)trailed_cell);
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
#ifdef PRE_IMAGE_TRAIL
		if (pre_value)
		  *trailed_cell = (Cell) pre_value;
		else
#endif
		  bld_free(trailed_cell) ; /* early reset */
		
		/* could do trail compaction now or later */
		ls_early_reset++;
	      }
#else
	      { ls_mark(i) ;
	      marked += mark_region(trailed_cell, trailed_cell);
	      }
#endif
	    }
	  }
      
      /* mark the forward value */
      marked += mark_root((Cell) *(a-1));
      
#ifdef PRE_IMAGE_TRAIL
      if (pre_value) { 
	marked += mark_root((Cell) pre_value);
	pre_value = NULL;
      }
#endif

      /* stop if we're not going anywhere */
      if ((unsigned long) a == (unsigned long) *a)
	break;

      /* jump to previous cell */
      a = (CPtr) *a;
    }
  return marked;
}
/*----------------------------------------------------------------------*/

static int mark_query(CTXTdecl)
{
  Integer i;
  int yvar, total_marked = 0 ;
  CPtr b,e,*tr,a,d;
  byte *cp;
  int first_time;

  b = breg ;
  e = ereg ;
  tr = trreg ;
  cp = cpreg ;
  first_time = 1;

restart:
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
      if (b >= (cp_bot-CP_SIZE)) {
	return(total_marked) ;
      }
      a = (CPtr)tr ;
      tr = cp_trreg(b) ;

      /* the answer template is part of the forward computation for 
	 consumers, so it should be marked before the trail in order
	 to allow for early reset                      --lfcastro */
      
      if (is_generator_choicepoint(b)) {
	CPtr region;
	int at_size;
	region = (CPtr) tcp_template(b);
	at_size = (int_val(cell(region)) & 0xffff) + 1;
	while (at_size--)
	  total_marked += mark_cell(region--);
      } else if (is_consumer_choicepoint(b)) {
	CPtr region;
	int at_size;
	region = (CPtr) nlcp_template(b);
	at_size = (int_val(cell(region))&0xffff)+1;
	while (at_size--)
	  total_marked += mark_cell(region--);
      }

      /* mark the delay list field of all choice points in CP stack too */
      if ((d = cp_pdreg(b)) != NULL) {
	total_marked += mark_root((Cell)d);
      }

      total_marked += mark_trail_section(a,(CPtr) tr);

      /* mark the arguments in the choicepoint */
      /* the choicepoint can be a consumer, a generator or ... */

      /* the code for non-tabled choice points is ok */
      /* for all other cps - check that
	 (1) the saved arguments are marked
	 (2) the substitution factor is marked
      */

      if (is_generator_choicepoint(b))
	{ /* mark the arguments */
	  total_marked += mark_region(b+TCP_SIZE, tcp_prevtop(b)-1);
	}
      else if (is_consumer_choicepoint(b))
	{ /* mark substitution factor -- skip the number of SF vars */
	  /* substitution factor is in the choicepoint for consumers */
#ifdef SLG_GC
	  if (nlcp_prevtop(b) != b+NLCP_SIZE) {
	    /* this was a producer that was backtracked over --lfcastro */
	    /* mark the arguments, since chaining & copying consider them */
	    CPtr ptr;
	    for (ptr = b+NLCP_SIZE; ptr < nlcp_prevtop(b); ptr++)
	      *ptr = makeint(6660666);
/* 	    total_marked += mark_region(b+NLCP_SIZE, nlcp_prevtop(b)-1); */
	  } 

#endif
	}
      else if (is_compl_susp_frame(b)) 
	/* there is nothing to do in this case */ ;
      else { 
	CPtr endregion, beginregion;
	endregion = cp_prevtop(b)-1;
	beginregion = b+CP_SIZE;
	total_marked += mark_region(beginregion,endregion) ;

      }

      e = cp_ereg(b) ;
      cp = cp_cpreg(b) ;
#if defined(GC_PROFILE) && defined(CP_DEBUG)
      if (examine_data) {
	print_cpf_pred(b);
	active_cps++;
      }
#endif
      if (first_time) {
	first_time = 0;
	if (bfreg < breg) {
	  b = bfreg;
	  e = cp_ereg(b);
	  cp = cp_cpreg(b);
	  tr = cp_trreg(b);
	  goto restart;
	}
      }

      b = cp_prevtop(b);
    }

} /* mark_query */

/*----------------------------------------------------------------------*/

static int mark_hreg_from_choicepoints(CTXTdecl)
{
  CPtr b, bprev, h;
  Integer i;
  int  m;

  /* this has to happen after all other marking ! */
  /* actually there is no need to do this for a copying collector */

  b = (bfreg < breg ? bfreg : breg);
  bprev = 0;
  m = 0;
    while(1)
     {
      h = cp_hreg(b) ;
      i = h - heap_bot ;
      if (! h_marked(i)) /* h from choicepoint should point to something that
			    is marked; if not, mark it now and set it
			    to something reasonable - int(666) is ok
			    although a bit scary :-)
			 */
	{
	  cell(h) = makeint(666) ;
	  TO_BUFFER(h);
	  h_mark(i) ;
	  m++ ;
	}
#ifdef SLG_GC
      /* should mark hfreg for generators, too --lfcastro */
      if (is_generator_choicepoint(b)) {
	h = tcp_hfreg(b);
	i = h - heap_bot;
	if (! h_marked(i)) {
	  cell(h) = makeint(6660);
	  TO_BUFFER(h);
	  h_mark(i);
	  m++;
	}
      }
#endif
      bprev = b; 
    b = cp_prevtop(b);
    if (b >= (cp_bot-CP_SIZE))
      break;
  }
  return m;
} /* mark_hreg_from_choicepoints */

/*-------------------------------------------------------------------------*/

/**
 * mark_from_attv_array: marks reachable cells from the attributed variables 
 *                       interrupt chain.
 * 
 * 
 * Return value: number of marked cells.
 **/
static int mark_from_attv_array(CTXTdecl)
{
  int i,max;
  int m=0;

  max = int_val(cell(interrupt_reg));

  for (i=0; i<max; i++) {
    m += mark_cell((CPtr) attv_interrupts[i][0]);
    m += mark_cell((CPtr) attv_interrupts[i][1]);
  }
  return m;
}

/*-------------------------------------------------------------------------*/


int mark_heap(CTXTdeclc int arity, int *marked_dregs)
{
  int avail_dreg_marks = 0, marked = 0;

  /* the following seems unnecessary, but it is not !
     mark_heap() may be called directly and not only through gc_heap() */
  slide = (flags[GARBAGE_COLLECT] == SLIDING_GC) |
    (flags[GARBAGE_COLLECT] == INDIRECTION_SLIDE_GC);
  
  stack_boundaries ;
  
  if (print_on_gc) print_all_stacks(CTXTc arity);
  
  if (slide) {
#ifdef INDIRECTION_SLIDE
    /* space for keeping pointers to live data */
    slide_buf_size = (unsigned long) ((hreg+1-(CPtr)glstack.low)*0.2);
    slide_buf = (CPtr *) calloc(slide_buf_size+1, sizeof(CPtr));
    if (!slide_buf)
      xsb_exit("Not enough space to allocate slide_buf");
    slide_top=0;
    if (flags[GARBAGE_COLLECT] == INDIRECTION_SLIDE_GC)
      slide_buffering=1;
    else
      slide_buffering=0;
#endif
  }
  
#ifdef INDIRECTION_SLIDE
  else
    slide_buffering=0;
#endif
  
#ifdef SLG_GC
  cp_marks = (char *)calloc(cp_bot - cp_top + 1,1);
  tr_marks = (char *)calloc(tr_top - tr_bot + 1,1);
  if ((! cp_marks) || (! tr_marks))
    xsb_exit("Not enough core to perform garbage collection chaining phase");
#endif  
  heap_marks = (char * )calloc(heap_top - heap_bot + 2 + avail_dreg_marks,1);
  ls_marks   = (char * )calloc(ls_bot - ls_top + 1,1);
  if ((! heap_marks) || (! ls_marks))
    xsb_exit("Not enough core to perform garbage collection marking phase");
  
  heap_marks += 1; /* see its free; also note that heap_marks[-1] = 0 is
		      needed for copying garbage collection see copy_block() */
  
  /* start marking phase */
  marked = mark_region(reg+1,reg+arity);
  if (delayreg != NULL) {
    marked += mark_root((Cell)delayreg);
  }
  /* Heap[0] is a global variable */
  marked += mark_root((Cell)glstack.low);
  
  if (slide)
    { 
      int put_on_heap;
      put_on_heap = arity;
      marked += put_on_heap;
      while (put_on_heap > 0) {
#ifdef SLG_GC
	TO_BUFFER((heap_top-put_on_heap-1));
	h_mark((heap_top - 1 - put_on_heap--)-heap_bot);
#else
	TO_BUFFER((heap_top-put_on_heap));
	h_mark((heap_top - put_on_heap--)-heap_bot);
#endif
      }
    }

#ifdef SLG_GC
  /* hfreg's also kept in the heap so that it's automatically adjusted */
  /* only for sliding GC */
  if (slide) { 
    CPtr hfreg_in_heap;
    /* mark from hfreg */
    marked += mark_root((Cell)hfreg);
  
    hfreg_in_heap = heap_top - 1;
    TO_BUFFER(hfreg_in_heap);
    if (!h_marked(hfreg_in_heap - heap_bot)) {
      h_mark(hfreg_in_heap - heap_bot);
      marked++;
    }
  }
#endif


  marked += mark_query(CTXT);

  marked += mark_from_attv_array(CTXT);

  if (slide)
    marked += mark_hreg_from_choicepoints(CTXT);

  if (print_on_gc) print_all_stacks(CTXTc arity);

  return marked ;
} /* mark_heap */

