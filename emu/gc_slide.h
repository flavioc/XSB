/* File:      gc_slide.h
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
      switch (tag) {
      case XSB_REF: 
      case XSB_REF1:
	*pointsto = (Cell)destination ;
	break ;
      case XSB_STRUCT :
	*pointsto = makecs((Cell)destination) ;
	break ;
      case XSB_LIST :
	*pointsto = makelist((Cell)destination) ;
	break ;
      case XSB_ATTV :
	*pointsto = makeattv((Cell)destination);
	break;
      default :
	xsb_exit("tag error during unchaining") ;
      }
    }
  while (continue_after_this) ;

} /* unchain */

/*----------------------------------------------------------------------*/

inline static void swap_with_tag(CPtr p, CPtr q, int tag)
{ /* p points to a cell with contents a tagged pointer
     make *q = p + tag, but maybe shift p
  */
   *p = *q ;
   switch (tag) {
   case XSB_REF:
   case XSB_REF1:
     *q = (Cell)p ;
     break ;
   case XSB_STRUCT :
     *q = makecs((Cell)p) ;
     break ;
   case XSB_LIST :
     *q = makelist((Cell)p) ;
     break ;
   case XSB_ATTV :
     *q = makeattv((Cell)p);
     break;
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

#ifdef INDIRECTION_SLIDE

#define mem_swap(a,b) \
{ unsigned long temp; \
 temp = *a; \
 *a = *b; \
 *b = temp; \
}
#define push_sort_stack(X,Y) \
addr_stack[stack_index] = X;\
size_stack[stack_index] = Y;\
stack_index++
#define pop_sort_stack(X,Y)\
stack_index--; \
X = addr_stack[stack_index]; \
Y = size_stack[stack_index]
#define sort_stack_empty \
(stack_index == 0)

static void randomize_data(unsigned long *data, unsigned long size)
{
  unsigned long i,j;

  for (i=0; i<size; i++) {
    j = (unsigned long) rand()*(size-1)/RAND_MAX;
    mem_swap((data+i), (data+j));
  }
}

static void sort_buffer(unsigned long *indata, unsigned long insize)
{
  unsigned long *left, *right, *pivot;
  unsigned long *data, size;
  unsigned long *addr_stack[4000];
  unsigned long size_stack[4000];
  int stack_index=0;
  int leftsize;
#ifdef GC_PROFILE
  unsigned long begin_sorting, end_sorting;
#endif
  
  randomize_data(indata,insize);

#ifdef GC_PROFILE
  if (verbose_gc)
    begin_sorting = cpu_time();
#endif
  push_sort_stack(indata,insize);

  while (!sort_stack_empty) {
    
    pop_sort_stack(data,size);

    if (size < 1)
      continue;

    if (size == 1) {
      if (data[0] > data[1])
	mem_swap(data, (data+1));
      continue;
    }
    
    left = data;
    right = &data[size];
    
    pivot = &data[size/2];
    mem_swap(pivot, right);
    
    pivot = right;
    
    while (left < right) {
      while ((*left < *pivot) && (left < right)) 
	left++;
      while ((*right >= *pivot) && (left < right))
	right--;
      if (left < right) { 
	mem_swap(left,right);
	left++;
      }
    }
    if (right == data) {
      mem_swap(right, pivot);
      right++;
    }
    leftsize = right - data;
    if (leftsize >= 1)
      push_sort_stack(data,leftsize);
    if ((size-leftsize) >= 1)
      push_sort_stack(right,(size-leftsize));

  } 
#ifdef GC_PROFILE
  if (verbose_gc) {
    end_sorting = cpu_time();
    fprintf(stddbg,"{GC} Sorting took %f ms.\n", (double)
	    (end_sorting - begin_sorting)*1000/CLOCKS_PER_SEC);
  }
#endif
}

#endif

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
	  if (! h_marked(q-heap_bot)) {
	    continue ;
	  }
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
	    { xsb_dbgmsg("not marked from cp(%p)",p); continue ; }
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
	/* chain answer template special area */
	chat_chain_region(at_start, at_area_size);
      }
#endif

    /* if (print_on_gc) print_all_stacks() ; */

  { CPtr destination, hptr ;
    long garbage = 0 ;
    int index ;

    /* one phase upwards - from top of heap to bottom of heap */

    index = heap_top - heap_bot ;
    destination = heap_bot + num_marked - 1 ;
#ifdef INDIRECTION_SLIDE
    if (slide_buffering) {
      unsigned long i;
#ifdef GC_PROFILE
      if (verbose_gc) {
	fprintf(stddbg,"{GC} Using Fast-Slide scheme.\n");
      }
#endif
      /* sort the buffer */
      sort_buffer((unsigned long *)slide_buf, slide_top-1);

      /* upwards phase */
      for (i=slide_top; i > 0; i--) {
	hptr = slide_buf[i-1];

	if (h_is_chained(hptr)) {
	  unchain(hptr,destination);
	}
	p = hp_pointer_from_cell(*hptr,&tag);
	if (p &&(p<hptr)) {
	  swap_with_tag(hptr,p,tag);
	  if (h_is_chained(p))
	    h_set_chained(hptr);
	  else
	    h_set_chained(p);
	}
	destination--;
      }
    } else {
#ifdef GC_PROFILE
      if (verbose_gc && flags[GARBAGE_COLLECT]==INDIRECTION_SLIDE_GC)
	fprintf(stddbg,"{GC} Giving up Fast-Slide scheme.\n");
#endif
#endif /* INDIRECTION_SLIDE */
      for (hptr = heap_top - 1 ; hptr >= heap_bot ; hptr--) {
	if (h_marked(hptr - heap_bot)) {
	  /* boxing */
	  if (garbage) {
	    *(hptr+1) = makeint(garbage) ;
	    garbage = 0 ;
	  }
	  if (h_is_chained(hptr)) {
	    unchain(hptr,destination) ; 
	  }
	  p = hp_pointer_from_cell(*hptr,&tag) ;            
	  if (p && (p < hptr)) {
	    swap_with_tag(hptr,p,tag) ;
	    if (h_is_chained(p))
	      h_set_chained(hptr) ;
	    else 
	      h_set_chained(p) ;
	  }
	  destination-- ;
	}
	else 
	  garbage++ ;
	index-- ;
      }
#ifdef INDIRECTION_SLIDE
    }
    if (!slide_buffering)
#endif
    if (garbage)
      /* the first heap cell is not marked */
      *heap_bot = makeint(garbage) ;

    /* one phase downwards - from bottom of heap to top of heap */
    index = 0 ;
    destination = heap_bot ;

#ifdef INDIRECTION_SLIDE
    if (slide_buffering) {
      unsigned long i;
      for (i=0; i<slide_top; i++) {
	hptr = slide_buf[i];

	if (h_is_chained(hptr)) {
	  unchain(hptr,destination);
	}
	if ((Cell)(hptr) == *hptr) /* undef */
	  bld_free(destination);
	else {
	  p = hp_pointer_from_cell(*hptr,&tag);
	  *destination = *hptr;
	  if (p && (p > hptr)) {
	    swap_with_tag(destination,p,tag);
	    if (h_is_chained(p))
	      h_set_chained(destination);
	    else
	      h_set_chained(p);
	  }
	  h_clear_mark((hptr-heap_bot));
	}
	destination++;
      }	    
    } else {
#endif /* INDIRECTION_SLIDE */
      hptr = heap_bot;
      while (hptr < heap_top) {
	if (h_marked(hptr - heap_bot)) {
	  if (h_is_chained(hptr))
	    { unchain(hptr,destination) ; }
	  if ((Cell)(hptr) == *hptr) /* UNDEF */
	    bld_free(destination) ;
	  else {
	    p = hp_pointer_from_cell(*hptr,&tag) ;
	    *destination = *hptr ;
	    if (p && (p > hptr)) {
	      swap_with_tag(destination,p,tag) ;
	      if (h_is_chained(p))           
		h_set_chained(destination) ;   
	      else 
		h_set_chained(p) ;
	    }
	  }
	  h_clear_mark((hptr-heap_bot)) ;
	  hptr++ ; destination++ ;
	  index++ ;
	} else {
	  garbage = int_val(cell(hptr)) ;
	  index += garbage ;
	  hptr += garbage ;
	}
      }
      if (destination != (heap_bot+num_marked))
	xsb_dbgmsg("bad size %p  %p",
		   destination,heap_bot+num_marked);
#ifdef INDIRECTION_SLIDE
    }  
#endif
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
