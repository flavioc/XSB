/* File:      gc_copy.h
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

/* #define GC_DEBUG */

/* the following variables are set by copy_heap() and used as */
/* globals in the two functions below.                        */

static int offset;
static CPtr scan, next;


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

#define adapt_external_heap_pointer(P,Q,TAG) \
    CHECK(Q);\
    GCDBG("Adapting %p ", P); GCDBG("with %p ", Q);\
    Q = (CPtr)((CPtr)(cell(Q))+offset); \
    if (TAG == XSB_REF || TAG == XSB_REF1) {\
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
      switch (tag) {
      case XSB_REF: 
      case XSB_REF1:
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
      case XSB_STRUCT :
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
      case XSB_LIST :
	addr = clref_val(q);
	GCDBG("List %p found... \n", addr);
	if (h_marked(addr-heap_bot)) { /* if list head not already copied */
	  copy_block(addr,next); /* this modifies *addr */
	}
	CHECK(addr);
	addr = (CPtr)((CPtr)(cell(addr))+offset);
	bld_list(scan, addr);
	break;
      case XSB_ATTV:
	addr = clref_val(q);
	GCDBG("Attv %p found... \n", addr);
	if (h_marked(addr-heap_bot)) {
	  copy_block(addr,next);
	}
	CHECK(addr);
	addr = (CPtr)((CPtr)(cell(addr))+offset);
	bld_attv(scan, addr);
	break;
      default :
	break;
      }
    }
} /* find_and_copy_block */

#endif /* GC */

/*-------------------------------------------------------------------------*/
#ifdef GC

inline static void adapt_hreg_from_choicepoints(CPtr h)
{
  CPtr b, bprev;

  /* only after copying collection */
  b = breg;
  b = top_of_cpstack;
  bprev = 0;
  while (b != bprev) {
    cp_hreg(b) = h;
    bprev = b; 
    b = cp_prevbreg(b);
  }
} /* adapt_hreg_from_choicepoints */

#endif

#ifdef SLG_GC

inline static void adapt_hfreg_from_choicepoints(CPtr h)
{
  CPtr b, bprev;
  b = (bfreg < breg ? bfreg : breg);
  bprev = 0;
  while (1) {
    if (is_generator_choicepoint(b))
      tcp_hfreg(b) = h;
    cp_hreg(b) = h;
    b = cp_prevtop(b);
    if (b >= (cp_bot - CP_SIZE))
      break;
  }
}

#endif

/*=======================================================================*/

#ifdef CHAT
static inline void chat_copy_region(CPtr p, int len)
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
      for (p = tr_bot; p <= endtr; p++) {
	contents = cell(p);

	/* lfcastro -- experimental for copying */
#ifdef SLG_GC
	if (!tr_marked(p-tr_bot))
	  continue;
	tr_clear_mark(p-tr_bot);
#endif

	q = hp_pointer_from_cell(contents,&tag) ;
	if (!q) continue ;
	if (h_marked(q-heap_bot)) 
	  find_and_copy_block(q); 
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
      /* copy of answer template special area */
      chat_copy_region(at_start, at_area_size);
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

    if (next != end_new_h) { 
      xsb_dbgmsg("heap copy gc - inconsistent hreg: %d cells not copied. (num_gc=%d)\n",
		 (end_new_h-next),num_gc);
    }

    memcpy((void *)heap_bot, (void *)begin_new_h, marked*sizeof(Cell));

    return(heap_bot+marked);
} /* copy_heap */

#endif /* GC */

