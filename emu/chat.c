/* File:      chat.c
** Author(s): Bart Demoen, Kostis Sagonas
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) K.U. Leuven and the Research Foundation of SUNY, 1998
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
#include <stdlib.h>

#include "configs/config.h"
#include "debugs/debug.h"

#ifdef CHAT	/* have this here so that gcc is silent with -ansi */

#include "auxlry.h"
#include "cell.h"
#include "register.h"
#include "tries.h"
#include "choice.h"
#include "psc.h"
#include "xmacro.h"
#include "memory.h"
#include "xsberror.h"
#include "chat.h"
#include "sw_envs.h"
#include "binding.h"
#include "realloc.h"
#include "inst.h"

static int chat_total_malloced = 0;
static int chat_inuse = 0;
static int chat_malloc_high_mark = 0;
static int chat_number_saved_consumers = 0;
static int chat_number_incremental_saves = 0;
static int chat_area_sharing = 0;
static int chat_nr_of_restores = 0;
static int chat_restored_memory = 0;

chat_init_pheader chat_link_headers = 0;

#if (!defined(WAM_TRAIL))
#error "Sorry, it will not work without it, hence this error!"
#endif


/*----------------------------------------------------------------------

Copy area data structure:

         a header which consists of 2 times the information about

         - where to restore the piece of info: destination (*)
	 - where the piece of info is: start
	 - the length of the info: length

2 times, i.e. for the the trail(*) and the consumer choicepoint (*)

(*) is to be updated on gc or stack expansion

The header also contains a pointer to a data structure of similar
form, which denotes the other segments(s) to be restored when this
consumer is switch_env-ed to.

The pieces of info are in an amorphous area of type *CPtr[] (I think)

Type definitions are in chat.h

  ----------------------------------------------------------------------*/

#define chat_set_tr_length(phead,p)         chat_get_tr_length(phead) = p
#define chat_set_cons_length(phead,p)       chat_get_cons_length(phead) = p

#define chat_set_tr_start(phead,p)          chat_get_tr_start(phead) = p
#define chat_set_cons_start(phead,p)        chat_get_cons_start(phead) = p

#define chat_init_reference(phead)          (phead)->reference_counter = 0
#define chat_decr_reference(phead)          ((phead)->reference_counter)--
#define chat_freeable(phead)                (0 == (phead)->reference_counter)


#define chat_fill_prevb(phead,previousb)    nlcp_prevbreg((CPtr)(&chat_get_cons_start(phead))) = previousb
#define chat_fill_chat_area(phead)	    nlcp_chat_area((CPtr)(&chat_get_cons_start(phead))) = (CPtr) phead
#define chat_fill_prevcons(phead,prev_cons) nlcp_prevlookup((CPtr)(&chat_get_cons_start(phead))) = prev_cons


#define needed_size(basic_size) \
             (((basic_size + sizeof(CPtr) - 1)/sizeof(CPtr))*(sizeof(CPtr)+1) /* chain bit overhead */ \
             + sizeof(CPtr)) /* alignment overhead */


/*----------------------------------------------------------------------*/
/* Memory management routines for auxiliary areas.                      */
/*----------------------------------------------------------------------*/

#define cr_alloc_chunk_size   (1024 * sizeof(struct chat_root))

#define Free_CR(theCR) \
    cr_next(theCR) = free_crs; \
    free_crs = theCR

#define New_CR(theCR,theOldRoots,theNewRoot) \
    if (free_crs) {\
      theCR = free_crs; \
      free_crs = cr_next(free_crs); \
    } else { \
      if (free_cr_space < top_cr_space) { \
        theCR = free_cr_space++; \
      } else { \
        theCR = alloc_more_cr_space(); \
      } \
    } \
    /* fprintf(stderr,"NewCR(%p, %p, %p)\n", theCR,theOldRoots,theNewRoot);*/\
    cr_root_area(theCR) = theNewRoot; \
    cr_next(theCR) = (CRPtr)theOldRoots; \
    theOldRoots = (CPtr)theCR

char *cr_space_chunk_ptr = NULL;

static CRPtr free_crs = NULL, free_cr_space = NULL, top_cr_space = NULL;

void abolish_cr_space(void)
{
    char *t;

    while (cr_space_chunk_ptr) {
      t = *(char **)cr_space_chunk_ptr;
      free(cr_space_chunk_ptr);
      cr_space_chunk_ptr = t;
    }
    free_crs = free_cr_space = top_cr_space = NULL;
}

void reclaim_cr_space(TChoice tcp_ptr)
{
    CRPtr cr, cr_ptr;

    cr = (CRPtr)tcp_chat_roots(tcp_ptr);
    while (cr != NULL) {
      cr_ptr = cr;
      cr = cr_next(cr);
      Free_CR(cr_ptr);
    }
}

static CRPtr alloc_more_cr_space(void)
{
    char *t;

    if ((t = (char *)malloc(cr_alloc_chunk_size+sizeof(Cell))) == NULL)
      xsb_abort("No space to allocate more CHAT roots");
    *(char **)t = cr_space_chunk_ptr;
    cr_space_chunk_ptr = t;
    free_cr_space = (CRPtr)(t+sizeof(Cell));
    top_cr_space = (CRPtr)(t+cr_alloc_chunk_size+sizeof(Cell));
    return free_cr_space++;
}

/*----------------------------------------------------------------------*/
/* Auxiliary function to update statistics.                             */
/* -- does not take into account that CHAT areas are not only for       */
/*    consumers now... perhaps more errors.                             */
/*----------------------------------------------------------------------*/

static void chat_update_stats(incremental, nrarguments, size_tr)
{
    int i = 0;
    
    i = sizeof(incr_chat_header) + sizeof(CPtr)*needed_size(size_tr);
    if (!incremental)
      i += sizeof(init_chat_header) + sizeof(CPtr)*(needed_size(nrarguments));
    chat_total_malloced += i;
    chat_inuse += i;
    if (chat_inuse > chat_malloc_high_mark)
      chat_malloc_high_mark = chat_inuse;
    if (incremental) chat_number_incremental_saves++;
    else chat_number_saved_consumers++;
}

/*----------------------------------------------------------------------*/
/* to be called with phead pointing to the first in a chain of headers	*/
/* the "mode" argument specifies whether the freeing would be batched   */
/* (usually upon completion of subgoals) or just selective, i.e. some   */
/* chat areas, (on e.g. resuming chat areas of completion suspensions). */
/*----------------------------------------------------------------------*/

#define myfree(p) free(p)

#define BATCH_MODE     0
#define SELECTIVE_MODE 1

static void chat_free_chat_area(chat_init_pheader phead, bool mode)
{
  chat_incr_pheader p,q;

  chat_inuse -=  sizeof(init_chat_header)
                 + needed_size(chat_get_nrargs(phead))*sizeof(CPtr);
  p = chat_get_father(phead);
  myfree(chat_get_malloc_start(phead));

  if (phead == chat_link_headers)
    chat_link_headers = phead->next_header;
  if (phead == chat_link_headers)
    chat_link_headers = 0;
  else { chat_init_pheader prev = phead->prev_header;
         chat_init_pheader next = phead->next_header;
	 prev->next_header = next;
	 next->prev_header = prev;
	 phead->prev_header = phead->next_header = NULL;
       }

  myfree(phead);
  while (p)
    { chat_area_sharing++ ;
      chat_decr_reference(p);
      if (chat_freeable(p))
	{ q = p;
	  p = chat_get_ifather(q);
	  /* no need to do the following in BATCH_MODE: in this mode all the
	   * chat roots are also freed anyway; therefore no dangling pointers
	   * can occur (as long of course as this invariant is maintained) */
	  if (mode != BATCH_MODE) {
	    if (p == NULL) { /* q is a CHAT trail root: remove it as such */
	      /* unfortunately I do not really see any better way than to
	       * start searching for the generator CP at this point... */
	      CRPtr cr, prev;
	      CPtr  b = breg;
	      do {
		b = cp_prevbreg(b);
	      } while (!(is_generator_choicepoint(b)));
	      /* here b points to a leader TCP */
	      for (prev = NULL, cr = (CRPtr)tcp_chat_roots(b);
		   cr != NULL; prev = cr, cr = cr_next(cr)) {
		if (q == cr_root_area(cr)) {
		  /*	  fprintf(stderr, "!!!!  FOUND !!!!\n"); */
		  if (!prev)
		    tcp_chat_roots(b) = (CPtr)cr_next(cr);
		  else
		    cr_next(prev) = cr_next(cr);
		  Free_CR(cr);
		  break; /* Invariant: A chat root can only occur once */
		}
	      }
	    }
	  }
	  chat_inuse -= sizeof(incr_chat_header)
                 + needed_size(chat_get_tr_length(q))*sizeof(CPtr);
	  myfree(chat_get_imalloc_start(q));
	  myfree(q);
	}
      else return;
    }
} /* chat_free_chat_area */

/*----------------------------------------------------------------------*/
/* frees all Chat-areas of consumers of subgoal "subg_ptr"		*/
/*----------------------------------------------------------------------*/

void chat_free_cons_chat_areas(SGFrame subg_ptr)
{
    ComplStackFrame csf;
    chat_init_pheader chat_ptr, chat_ptr_tmp;

    csf = (ComplStackFrame)subg_compl_stack_ptr(subg_ptr);
    chat_ptr = (chat_init_pheader)compl_cons_copy_list(csf);
    while (chat_ptr != NULL) {
      chat_ptr_tmp = (chat_init_pheader)nlcp_prevlookup((CPtr)(&chat_get_cons_start(chat_ptr)));
      chat_free_chat_area(chat_ptr, BATCH_MODE);
      chat_ptr = chat_ptr_tmp;
    }
    compl_cons_copy_list(csf) = NULL; /* really forget about them */
}

/*----------------------------------------------------------------------*/
/* frees all Chat-areas of completion suspensions of subgoal "subg_ptr"	*/
/*----------------------------------------------------------------------*/

void chat_free_compl_susp_chat_areas(SGFrame subg_ptr)
{
    chat_init_pheader ptr;

    while ((ptr = (chat_init_pheader)subg_compl_susp_ptr(subg_ptr)) != NULL) {
      subg_compl_susp_ptr(subg_ptr) =
	csf_prevcsf((CPtr)(&chat_get_cons_start(ptr)));
      chat_free_chat_area(ptr, BATCH_MODE);
    }
}

/*----------------------------------------------------------------------*/
/* Routines that restore CHAT areas.                                    */
/*----------------------------------------------------------------------*/

static CPtr *chat_reinstall_oldbindings(chat_incr_pheader pheader,
					CPtr *where_tr)
{
    int  j;
    long size_tr = chat_get_tr_length(pheader);
    CPtr *chat_tr_area = chat_get_tr_start(pheader);

    while (size_tr)
      { 
	  if (sizeof(CPtr) > size_tr)
	       { j = size_tr>>1; size_tr = 0; }
	  else { j = sizeof(CPtr)>>1; size_tr -= sizeof(CPtr); }

	  while (j--)
	    { *(where_tr++) = *chat_tr_area;  /* imitates pushtrail0 */
	      *(*chat_tr_area) = (Cell)(*(chat_tr_area+1));
	      chat_tr_area += 2;
	    }
	  chat_tr_area++;
      }

    return where_tr;

} /* chat_reinstall_oldbindings */

/*----------------------------------------------------------------------*/
/* Restores an init-chat-area; both of a consumer and of a completion   */
/* suspension.  Please make sure this double use remains valid.         */
/*----------------------------------------------------------------------*/

#define CONSUMER_TYPE   0
#define COMPL_SUSP_TYPE 1

static void chat_restore_init_area(CPtr destination, int type,
				   chat_init_pheader phead)
{ int len, i;
  CPtr from; 

  *((NLChoice)destination) = chat_get_cons_start(phead);
  destination += NLCPSIZE;
  len = chat_get_nrargs(phead);
  if (type == CONSUMER_TYPE) {
    *destination = len; 
    destination++;
  }
  from = (CPtr)chat_get_args_start(phead);
  while (len)
    { if (len > sizeof(CPtr))
           { i = sizeof(CPtr); len -= sizeof(CPtr); }
      else { i = len; len = 0; }
      while (i--)
	*destination++ = *from++;
      from++; /* skipping the chain bit */
    }

} /* chat_restore_init_area */

/*----------------------------------------------------------------------*/

static void chat_reinstall_all_oldbindings(chat_incr_pheader pheader)
{
    chat_incr_pheader father;

    father = chat_get_ifather(pheader);
    if (father)
      chat_reinstall_all_oldbindings(father);
    else trreg = tcp_trreg(breg);

    /* the saved trail has to be used for restoring       */
    /* some values on the old heap and old local stack    */
    trreg = chat_reinstall_oldbindings(pheader, trreg);

} /* chat_reinstall_all_oldbindings */

/*----------------------------------------------------------------------*/

CPtr chat_restore_consumer(chat_init_pheader pheader)
{
    int nrarguments;
    CPtr consumer_breg;
    chat_incr_pheader father;

    father = chat_get_father(pheader);
    chat_reinstall_all_oldbindings(father);

    /* consumers are only scheduled by a leader         */
    /* the consumer is installed just below this leader */

    chat_fill_prevb(pheader, breg);
    nrarguments = chat_get_nrargs(pheader);
    consumer_breg = breg - NLCPSIZE - 1 - nrarguments;
    chat_restore_init_area(consumer_breg, CONSUMER_TYPE, pheader);
    nlcp_trreg(consumer_breg) = trreg;

    /* Reset the WAM registers that are not reset by the answer-return	*/
    /* instruction: trreg is reset as shown above; ptcpreg and delayreg */
    /* by answer-return explicitly; hreg, cpreg and ereg are reset by	*/
    /* the restore-some-wamregs; the remaining registers as below.	*/

    /* the reinstalled consumer choicepoint should get
       the h and eb from CP of the leader -- pointed by breg */

    hbreg = cp_hreg(consumer_breg) = cp_hreg(breg);
    ebreg = cp_ebreg(consumer_breg) = cp_ebreg(breg);

    chat_nr_of_restores++;

    return consumer_breg;

} /* chat_restore_consumer */

/*----------------------------------------------------------------------*/

CPtr chat_restore_compl_susp(chat_init_pheader pheader, CPtr h, CPtr eb)
{
    int nrarguments;
    CPtr compl_susp_breg;

#ifdef DEBUG
    fprintf(stderr, "called with eb = %d\n", ((CPtr)glstack.high - 1) - eb);
#endif
    chat_fill_prevb(pheader, breg);
#ifdef DEBUG
    if (is_generator_choicepoint(breg)) {
      SGFrame subg = (SGFrame)tcp_subgoal_ptr(breg);
      fprintf(stderr, "Subgoal = %p", subg);
      if (is_completed(subg)) {
	fprintf(stderr, " is completed; clobbering its TCP[next_clause]\n");
	/* tcp_pcreg(breg) = (pb)&fail_inst; --- csuses infinite loop */
      } else fprintf(stderr, "\n");
    }
#endif
    nrarguments = chat_get_nrargs(pheader);
    compl_susp_breg = breg - NLCPSIZE - nrarguments;
    chat_restore_init_area(compl_susp_breg, COMPL_SUSP_TYPE, pheader);

    /* propagate trreg of leader */
    csf_trreg(compl_susp_breg) = cp_trreg(breg);
    
    /* the reinstalled completion suspension choicepoint
       should protect heap and local stack */
    hbreg = cp_hreg(compl_susp_breg) = h;
    ebreg = cp_ebreg(compl_susp_breg) = eb;

    return compl_susp_breg;

} /* chat_restore_compl_susp */

/*----------------------------------------------------------------------*/

void chat_update_compl_susp(chat_init_pheader pheader)
{
  /* NOTE: this procedure is written this way for maintainability.  It is
   *       sub-optimal: some things related to the following declarations
   *       are done twice ! */
    CPtr hb_saved = csf_hreg(breg);
    CPtr eb_saved = csf_ebreg(breg);
    CPtr *tr_saved = csf_trreg(breg);

    chat_fill_prevb(pheader, csf_prev(breg));
    chat_restore_init_area(breg, COMPL_SUSP_TYPE, pheader);
  /* the following lines imitate some of chat_restore_compl_susp() */
    csf_trreg(breg) = tr_saved;
    csf_hreg(breg) = hb_saved;
    csf_ebreg(breg) = eb_saved;
}

/*----------------------------------------------------------------------*/

void chat_restore_compl_susp_trail(chat_init_pheader pheader)
{
  /*    CPtr *tr_start; */
    chat_incr_pheader father;

    father = chat_get_father(pheader);
  /* tr_start = tcp_trreg(csf_prev((CPtr)(&chat_get_cons_start(pheader)))); */
    chat_reinstall_all_oldbindings(father);

    /* this CHAT area is not needed anymore */
    chat_free_chat_area(pheader, SELECTIVE_MODE);

} /* chat_restore_compl_susp_trail */

/*----------------------------------------------------------------------*/
/* Routines that save states in CHAT areas.                             */
/*----------------------------------------------------------------------*/

static void chat_save_cons_arguments(chat_init_pheader pheader,
				     int nrarguments,
				     CPtr *startargs)
{ 
    int  j;
    long i;
    CPtr *p;

    i = needed_size(nrarguments);
    p = malloc(i*sizeof(CPtr));
    if (p == NULL)
      xsb_exit("chat_save_cons_arguments - malloc failed\n");
    chat_get_malloc_start(pheader) = p;
    chat_set_nrargs(pheader,nrarguments);

    i = (((long)p)/sizeof(CPtr)) % (sizeof(CPtr) + 1);
    if (i != 0)
      p += sizeof(CPtr) + 1 - i; /* now p should be aligned properly */
    chat_get_args_start(pheader) = p;

    while (nrarguments) {
      p[sizeof(CPtr)] = 0;

      if (sizeof(CPtr) >= nrarguments)
	   { j = nrarguments; nrarguments = 0; }
      else { j = sizeof(CPtr); nrarguments -= sizeof(CPtr); }

      while (j--)
	*(p++) = *(startargs++);
      p++;
    }
} /* chat_save_cons_arguments */

/*----------------------------------------------------------------------*/
/*
static void test_finiteness()
{
  chat_init_pheader initial_pheader;
  initial_pheader = chat_link_headers;
  if (initial_pheader != NULL)
  do
    {
      initial_pheader = initial_pheader->next_header;
    }
  while (initial_pheader != chat_link_headers);

} test_finiteness */

/*----------------------------------------------------------------------*/
/* The following function performs copy of the trail.    	        */
/*----------------------------------------------------------------------*/

static void chat_trail_copy(chat_incr_pheader pheader, CPtr *tr)
{
    int  j;
    long size_tr = chat_get_tr_length(pheader);
    CPtr *chat_tr_area = chat_get_tr_start(pheader);

    while (size_tr)
	{ chat_tr_area[sizeof(CPtr)] = 0;

	  if (sizeof(CPtr) > size_tr)
	       { j = size_tr>>1; size_tr = 0; }
	  else { j = sizeof(CPtr)>>1; size_tr -= sizeof(CPtr); }

	  while (j--)
	    { *(chat_tr_area++) = *tr;
	      *(chat_tr_area++) = (CPtr)(cell(*(tr++)));
	    }
	  chat_tr_area++;
	}
} /* chat_trail_copy */

/*------------------------------------------------------------------------*/
/* Allocates an incremental CHAT area for the trail and saves it there by */
/* copying.                                                               */
/*------------------------------------------------------------------------*/

static chat_incr_pheader chat_save_trail(chat_init_pheader pheader,
					 long size_tr, CPtr *dest_tr)
{
    long i;
    CPtr *mem_area;
    chat_incr_pheader incr_phead;

    incr_phead = (chat_incr_pheader)malloc(sizeof(incr_chat_header));
    if (incr_phead == NULL)
      xsb_exit("no room for incremental CHAT area");
    else {
      chat_init_reference(incr_phead);
      if (pheader) { /* then incremental */
        chat_set_father(pheader, incr_phead);
        chat_increment_reference(incr_phead);
      }

      chat_set_tr_length(incr_phead, size_tr);
      chat_init_imark_bit(incr_phead);
      chat_set_ifather(incr_phead, NULL);

      i = needed_size(size_tr);
      mem_area = malloc(i*sizeof(CPtr));
      if (mem_area == NULL)
	xsb_exit("CHAT trail allocation failure");
      chat_set_imalloc_start(incr_phead, mem_area);
      i = (((long)mem_area)/sizeof(CPtr)) % (sizeof(CPtr) + 1);
      if (i != 0)
	mem_area += sizeof(CPtr) + 1 - i; /* now should be aligned properly */

      chat_set_tr_start(incr_phead, mem_area);
      chat_trail_copy(incr_phead, (CPtr *)dest_tr);
    }

    return incr_phead;
}

/*------------------------------------------------------------------------*/
/* chat_save_consumer_state allocates a piece of memory to hold areas of  */
/* the required size for trail and consumer choicepoint                   */
/* the header of this area is filled in as well                           */
/*                                                                        */
/* chat_save_consumer_state is called when a consumer is "frozen"         */
/* see save_a_consumer_copy for meaning of the first argument             */
/*------------------------------------------------------------------------*/

static chat_init_pheader chat_save_consumer_state(int incremental,
						  long size_tr, long size_cons,
						  CPtr *dest_tr,CPtr dest_cons)
{
    CPtr *startargs;
    chat_init_pheader pheader, p;
    chat_incr_pheader incr_phead;
    int nrarguments;

    /* we need space for the chainbits - let sizecptr = sizeof(CPtr)    */
    /* for each CPtr we take one byte and we pack sizecptr such bytes   */
    /* in a CPtr the CHAT trail +cp will then be a block of sizecptr    */
    /* CPtrs followed by the CPtr with their chainbits                  */
    /* the only thing we want to rely on is that sizecptr is a multiple */
    /* of 2 as the trail is a series of CPtr pairs                      */
    /* we also want to let the CHAT trail start at an address that is a */
    /* multiple of sizecptr*(sizecptr+1)                                */
    /* in this way we can access the chainbit in constant time given a  */
    /* pointer to a CHAT entry                                          */
    /* this should be insensitive to 32-64-??? bit                      */

    /* for simplicity, keep the tr_start and cons_start both "aligned"  */

    if (! incremental) {
      /* if not incremental: alloc an initial header and fill it out */
      p = chat_link_headers;
      pheader = (chat_init_pheader)malloc(sizeof(init_chat_header));
      if (pheader == NULL) {
	xsb_exit("no room for initial CHAT area");
	return NULL;
      }
      if (p == NULL)
	chat_link_headers = pheader->prev_header = pheader->next_header = pheader;
      else {
	chat_init_pheader prev = p;
	chat_init_pheader next = p->next_header;
	next->prev_header = pheader; pheader->next_header = next;
	prev->next_header = pheader; pheader->prev_header = prev;
      }

      nrarguments = size_cons - NLCPSIZE - 1;
      startargs = (CPtr *)(breg + NLCPSIZE + 1);
      chat_get_cons_start(pheader) = *(NLChoice)breg; /* copy the cons CP */
      chat_save_cons_arguments(pheader,nrarguments,startargs);
    } else {
      pheader = NULL; nrarguments = 0;
    }

    incr_phead = chat_save_trail(pheader, size_tr, dest_tr);

    chat_update_stats(incremental, nrarguments, size_tr);

    if (! incremental)
      return pheader;
    return((chat_init_pheader)(incr_phead));

} /* chat_save_consumer_state */

/*----------------------------------------------------------------------*/
/* save_a_consumer_copy                                                 */
/*   argument 2: if 1 then we are about to backtrack over a generator   */
/*                         which is no longer a leader; we save a part  */
/*                         of a (virtual) consumer: not its cp          */
/*                         this copy has to be linked in with other     */
/*                         headers through the father_block_start       */
/*               if 0 then a new consumer state is copied               */
/*   modified so that it also incrementally updates the fields of       */
/*   choice points between the current top and the previous generator   */
/*----------------------------------------------------------------------*/

chat_init_pheader save_a_consumer_copy(SGFrame subg_ptr, int incremental)
{
#ifdef INCREMENTAL_CP_TRAVERSAL
    CPtr b, h, eb;
#else
    CPtr b;
#endif
    TChoice prev_tcp;
    chat_incr_pheader ip;
    chat_init_pheader pheader;
    long size_tr, size_cons;
    CPtr *dest_tr, dest_cons;
    CRPtr cr;

#ifdef INCREMENTAL_CP_TRAVERSAL
    if (incremental) {
      b = subg_cp_ptr(subg_ptr); h = cp_hreg(b); eb = cp_ebreg(b);
    } else {
      b = breg; h = hreg; eb = ebreg;
    }
    do {
      b = cp_prevbreg(b);
      cp_hreg(b) = h;
      cp_ebreg(b) = eb;
    } while (!(is_generator_choicepoint(b)));
    prev_tcp = (TChoice)b;
    /* The following is needed because restore_some_wamregs()
       resets from hbreg and not from cp_hreg(top_cp) */
    hbreg = h;
#else
    if (incremental) b = subg_cp_ptr(subg_ptr); else b = breg;
    for (prev_tcp = (TChoice)cp_prevbreg(b);
         !(is_generator_choicepoint(b));
         prev_cp = (TChoice)cp_prevbreg(prev_tcp)) ;
#endif
#ifdef Chat_DEBUG
    fprintf(stderr, "In save_a_cons_copy: leader tcp = %p\n", prev_tcp);
#endif

    dest_tr = tcp_trreg(prev_tcp);
    if (incremental)
         dest_cons = 0;
    else dest_cons = breg;

    size_tr = 2 * (trreg - (CPtr *)dest_tr) ;

    /* only one choicepoint needs to be saved: the     	*/
    /* consumer and the substitution factor below it	*/
    if (incremental)
         size_cons = 0;
    else size_cons = NLCPSIZE + *(breg+NLCPSIZE) + 1;

#ifdef Chat_DEBUG
    fprintf(stderr, "Trail to be copied from %p to %p (%ld cells)\n",
                    dest_tr,trreg-1, size_tr);
    fprintf(stderr, "Consumer to be copied from %p (%ld cells)\n",
                    breg, size_cons);
#endif

    pheader = chat_save_consumer_state(incremental,
				       size_tr,size_cons,
				       dest_tr,dest_cons);
    if (! pheader) {
      xsb_exit("No more rooms for CHATs");	/* :-) */
      return NULL;
    }

#ifdef Chat_DEBUG
    if (!incremental) {
      fprintf(stderr, "Found consumer of ");
      print_subgoal(stderr, subg_ptr); 
      fprintf(stderr, " Chat-saved in %p\n", pheader);
    }
#endif

    if (! incremental) {
      chat_fill_chat_area(pheader);
#ifdef Chat_DEBUG
      fprintf(stderr, "...Filling cons-copy-list of %p %p %p\n", subg_ptr,
		subg_compl_stack_ptr(subg_ptr),
		compl_cons_copy_list(subg_compl_stack_ptr(subg_ptr)));
#endif
      chat_fill_prevcons(pheader,compl_cons_copy_list(subg_compl_stack_ptr(subg_ptr)));
    }

    if (incremental) ip = (chat_incr_pheader)pheader;
    else ip = chat_get_father(pheader);

    New_CR(cr, tcp_chat_roots(prev_tcp), ip);

    return pheader;

} /* save_a_consumer_copy */

/*----------------------------------------------------------------------*/

#ifdef LOCAL_EVAL

chat_init_pheader save_a_consumer_for_generator(SGFrame subg_ptr)
{
    TChoice prev_tcp;
    int     subst_fact_var_num;
    CPtr    consumer, *cptr, compl_fr;
    chat_incr_pheader ip;
    chat_init_pheader pheader;
    long size_tr, size_cons;
    CPtr *dest_tr, dest_cons;
    CRPtr cr;

    for (prev_tcp = (TChoice)cp_prevbreg(breg);
	 !(is_generator_choicepoint(prev_tcp));
	 prev_tcp = (TChoice)cp_prevbreg(prev_tcp)) ;
#ifdef Chat_DEBUG
    fprintf(stderr, "In save_a_cons_4_gen: leader tcp = %p\n", prev_tcp);
#endif

    dest_tr = tcp_trreg(prev_tcp);
    dest_cons = breg;

    size_tr = 2 * (trreg - (CPtr *)dest_tr) ;

    compl_fr = subg_compl_stack_ptr(subg_ptr);
  /* only one choicepoint needs to be saved: 		*/
  /* the generator and the substitution factor for it	*/
    subst_fact_var_num = int_val(cell(compl_hreg(compl_fr)));
    size_cons = NLCPSIZE + subst_fact_var_num + 1;

#ifdef Chat_DEBUG
    fprintf(stderr, "Trail to be copied from %p to %p (%ld cells)\n",
                    dest_tr,trreg-1, size_tr);
    fprintf(stderr, "Consumer to be copied from %p (%ld cells)\n",
                    breg, size_cons);
#endif

    pheader = chat_save_consumer_state(CHAT_CONS_AREA,	/* hack */
				       size_tr,size_cons,
				       dest_tr,dest_cons);
    if (! pheader) {
      xsb_exit("No more rooms for CHATs");	/* :-) */
      return NULL;
    }

    consumer = (CPtr)(&chat_get_cons_start(pheader));
    nlcp_pcreg(consumer) = (pb) &answer_return_inst;
    nlcp_trie_return(consumer) = subg_ans_list_ptr(subg_ptr);

/* OLD CODE
    *(consumer+NLCPSIZE) = subst_fact_var_num;
    cptr = compl_hreg(compl_fr)-subst_fact_var_num;
    for (i = 1; i <= subst_fact_var_num; i++) {
      *(consumer+NLCPSIZE+i) = cell(cptr);
      cptr++;
    }
 */
    cptr = (CPtr *)(compl_hreg(compl_fr)-subst_fact_var_num);
    chat_save_cons_arguments(pheader,subst_fact_var_num,cptr);

    chat_fill_chat_area(pheader);
    chat_fill_prevcons(pheader,compl_cons_copy_list(compl_fr));

    ip = chat_get_father(pheader);

    New_CR(cr, tcp_chat_roots(prev_tcp), ip);

    { /* added BART - 9 nov 1998 */
      CPtr b, h, eb;

      b = breg; h = hreg; eb = ebreg;
      do {
	b = cp_prevbreg(b);
	cp_hreg(b) = h;
	cp_ebreg(b) = eb;
      } while (!(is_generator_choicepoint(b)));
      hbreg = h;
    }

    return pheader;

} /* save_a_consumer_for_generator */

#endif

/*----------------------------------------------------------------------*/
/* save_a_chat_compl_susp                                               */
/*----------------------------------------------------------------------*/

#if (!defined(INCREMENTAL_CP_TRAVERSAL))
#error "Function save_a_chat_compl_susp will not work on this configuration !"
#else

chat_init_pheader save_a_chat_compl_susp(int nrarguments, CPtr reg_base,
					 SGFrame subg_ptr, CPtr ptcp, byte *cp)
{
    CPtr b, h, eb;
    TChoice prev_tcp;
    chat_incr_pheader ip;
    chat_init_pheader pheader, p;
    long size_tr;
    CPtr *dest_tr, where;
    CRPtr cr;

    b = breg; h = hreg; eb = ebreg;
    do { /* perform CHAT-freeze till previous generator */
      cp_hreg(b) = h;
      cp_ebreg(b) = eb;
      if (is_generator_choicepoint(b)
	  /* THE FOLLOWING ADDED TEST IS A HORRIBLE TEMPORARY FIX */
	  && !is_completed(tcp_subgoal_ptr(b))
	  ) break; else b = cp_prevbreg(b);
    } while (TRUE);
    prev_tcp = (TChoice)b;
    /* The following is needed because restore_some_wamregs()
       resets from hbreg and not from cp_hreg(top_cp) */
    hbreg = h;
#ifdef DEBUG /* was Chat_DEBUG */
    fprintf(stderr, "In save_compl_susp: leader tcp = %p, tcp[eb] = %d\n",
	    prev_tcp, (((CPtr)glstack.high - 1) - (CPtr)(tcp_ebreg(prev_tcp))));
#endif

    p = chat_link_headers;
    pheader = (chat_init_pheader)malloc(sizeof(init_chat_header));
    if (! pheader) {
      xsb_exit("no room for initial CHAT area of completion suspension");
      return NULL;
    }
    if (p == NULL)
      chat_link_headers = pheader->prev_header = pheader->next_header = pheader;
    else {
      chat_init_pheader prev = p;
      chat_init_pheader next = p->next_header;
      next->prev_header = pheader; pheader->next_header = next;
      prev->next_header = pheader; pheader->prev_header = prev;
    }

    /* only one choicepoint needs to be saved: the	*/
    /* completion suspension and its argument registers	*/
    chat_save_cons_arguments(pheader, nrarguments, (CPtr *)reg_base);
    where = (CPtr)(&chat_get_cons_start(pheader));
    save_compl_susp_frame(where, subg_ptr, ptcp, cp);
    chat_fill_chat_area(pheader);

    dest_tr = tcp_trreg(prev_tcp);
    size_tr = 2 * (trreg - (CPtr *)dest_tr) ;
#ifdef DEBUG /* was Chat_DEBUG */
    fprintf(stderr, "Trail to be copied from %p to %p (%ld cells)\n",
                    dest_tr,trreg-1, size_tr);
#endif
    chat_save_trail(pheader, size_tr, dest_tr);

    chat_update_stats(CHAT_CONS_AREA, nrarguments, size_tr);

    ip = chat_get_father(pheader);
    New_CR(cr, tcp_chat_roots(prev_tcp), ip);

    return pheader;

} /* save_a_chat_compl_susp */

#endif /* INCREMENTAL_CP_TRAVERSAL */

/*----------------------------------------------------------------------*/
/* routines for memory statistics about CHAT areas                      */
/*----------------------------------------------------------------------*/

void reset_chat_statistics(void)
{
    chat_total_malloced = chat_malloc_high_mark = chat_inuse = 0;
    chat_number_saved_consumers = chat_number_incremental_saves = 0;
    chat_area_sharing = chat_nr_of_restores = chat_restored_memory = 0;
}

void print_chat_statistics(void)
{
  printf("  total size CHAT areas: %d b; high water mark: %d b; not freed: %d b\n",
	  chat_total_malloced,chat_malloc_high_mark,chat_inuse);
  printf("  suspensions saved: %d; increments saved: %d; shared increments: %d\n",
	  chat_number_saved_consumers,
	  chat_number_incremental_saves,
	  chat_area_sharing);
  printf("  number of restored suspensions: %d; total restored memory: %d\n",
	 chat_nr_of_restores,chat_restored_memory);
}


/*----------------------------------------------------------------------*/
/* some gc related stuff                                                */
/*----------------------------------------------------------------------*/

void chat_set_chained(CPtr p)
{ int i;
  char *pc;

  i = (((int)p)/sizeof(CPtr)) % (sizeof(CPtr) + 1);
  if (i >= sizeof(CPtr))
    fprintf(stderr,"alignment error during chat_set_chained\n");
  p += sizeof(CPtr)-i;
  pc = ((char *)p) + i;
  *pc = 1;
} /* chat_set_chained */

void chat_set_unchained(CPtr p)
{ int i;
  char *pc;

  i = (((int)p)/sizeof(CPtr)) % (sizeof(CPtr) + 1);
  if (i >= sizeof(CPtr))
    fprintf(stderr,"alignment error during chat_set_unchained\n");
  p += sizeof(CPtr)-i;
  pc = ((char *)p) + i;
  *pc = 0;
} /* chat_set_unchained */

int chat_is_chained(CPtr p)
{ int i;
  char *pc;

  i = (((int)p)/sizeof(CPtr)) % (sizeof(CPtr) + 1);
  if (i >= sizeof(CPtr))
    fprintf(stderr,"alignment error during chat_is_chained\n");
  p += sizeof(CPtr)-i;
  pc = ((char *)p) + i;
  return(((int)*pc));
} /* chat_is_chained */



#endif	/* CHAT */


/*------------------------- end of file chat.c --------------------------*/
