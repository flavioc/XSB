/*  -*-c-*-  Make sure this file comes up in the C mode of emacs */ 
/* File:      chatsched.i
** Author(s): Kostis Sagonas
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

/*----------------------------------------------------------------------*/
/* Reinstalls and schedules a consumer that has unresolved answers.	*/
/* In local evaluation, it also handles some generators as consumers.	*/
/* Returns NULL if there are no consumers that need scheduling.		*/
/*----------------------------------------------------------------------*/

static CPtr schedule_subgoal(SGFrame subg_ptr, CPtr compl_fr)
{
  CPtr consumer = NULL;
  chat_init_pheader chat_ptr = NULL;

#ifdef Chat_DEBUG
   fprintf(stderr,"scheduling answers for ");
#ifdef DEBUG
   print_subgoal(stderr, subg_ptr);
#endif
   fprintf(stderr, " (in %p) ",subg_ptr);
   if (has_answers(subg_ptr)) fprintf(stderr, "has answers");
   else fprintf(stderr, "NO answers\n");
#endif

  /* there are consumers and answers */  
  if ((has_answers(subg_ptr)) &&
      (chat_ptr = (chat_init_pheader)compl_cons_copy_list(compl_fr))) {
    consumer = (CPtr)(&chat_get_cons_start(chat_ptr));
    /* find the first consumer with unresolved answers, if any */
    while ((chat_ptr != NULL) && (nlcp_trie_return(consumer) != NULL) &&
	   (aln_next_aln(nlcp_trie_return(consumer)) == NULL)) {
      if ((chat_ptr = (chat_init_pheader)nlcp_prevlookup(consumer)) != NULL)
	consumer = (CPtr)(&chat_get_cons_start(chat_ptr));
    }
    if (chat_ptr != NULL) {
      /* Reinstall the consumer and return the value of breg */
#ifdef Chat_DEBUG
      fprintf(stderr,"restoring consumer...");
#endif
      consumer = chat_restore_consumer(chat_ptr);
#ifdef Chat_DEBUG
      fprintf(stderr,"done\n");
#endif
    } else {
      consumer = NULL;
    }
  } /* if there are answers and consumers */

  return consumer;
}

/*----------------------------------------------------------------------*/

static CPtr chat_fixpoint(SGFrame subg_ptr, TChoice leader_tcp)
{
  SGFrame currSubg;
  CPtr complFrame; /* completion stack frame for currSubg */
  CPtr sched_breg;

#ifdef Chat_DEBUG
  fprintf(stderr, "fixpoint subg_ptr = %p\n",subg_ptr);
#endif

  complFrame = openreg;
  /* for each subgoal in the ASCC, from youngest to leader (including) */
  while (complFrame <= subg_compl_stack_ptr(subg_ptr)) {
    currSubg = compl_subgoal_ptr(complFrame);

    /* if there are unresolved answers for currSubg */
    if ((sched_breg = schedule_subgoal(currSubg, complFrame))) {
/*
	fprintf(stderr, "returning %p\n", sched_breg);
*/
	return sched_breg;	/* very crude */
    } else { /* nothing to schedule for currSubg */
	complFrame = prev_compl_frame(complFrame);
    }
  }  /* while */

#ifdef Chat_DEBUG
  fprintf(stderr,"fixpoint returning null\n");
#endif

  return NULL;
}

/*----------------------------------------------------------------------*/
/* Makes an incremental CHAT copy area and links it to all consumer	*/
/* areas that need it -- in effect, chat areas have a tree form.       	*/
/*----------------------------------------------------------------------*/

static void chat_incremental_copy(SGFrame subg_ptr)
{
    CRPtr	      cr;
    TChoice	      tcp_ptr;
    chat_incr_pheader chat_area, inc_chat_area;
#ifdef LOCAL_EVAL
    chat_init_pheader chat_4_gen;

#ifdef Chat_DEBUG
    tcp_ptr = (TChoice)subg_cp_ptr(subg_ptr);
    fprintf(stderr, "**** for %p: ", tcp_ptr);
    for (cr = (CRPtr)tcp_chat_roots(tcp_ptr); cr != NULL; cr = cr_next(cr)) {
      fprintf(stderr, "CRarea = %p --> ", cr_root_area(cr));
    } fprintf(stderr, "\n");
#endif

    chat_4_gen = save_a_consumer_for_generator(subg_ptr);
    compl_cons_copy_list(subg_compl_stack_ptr(subg_ptr)) = (CPtr) chat_4_gen;
#endif

    tcp_ptr = (TChoice)subg_cp_ptr(subg_ptr);
    if ((cr = (CRPtr)tcp_chat_roots(tcp_ptr)) != NULL) {
#ifdef LOCAL_EVAL
      /* use the trail just saved for the generator as */
      /* incremental trail for all consumers that need it */
      inc_chat_area = chat_get_father(chat_4_gen);
#else
      inc_chat_area = (chat_incr_pheader)save_a_consumer_copy(subg_ptr,
                                                              CHAT_INCR_AREA);
#endif
      while (cr) {
	chat_area = cr_root_area(cr);
#ifdef DEBUG
	if (chat_area != inc_chat_area) { /* added PATCH */
#endif
	  chat_set_ifather(chat_area, inc_chat_area);
	  chat_increment_reference(inc_chat_area);
#ifdef DEBUG
	} else {
	  xsb_warn("Oh, oh: again we are in a loop...");
	  fprintf(stderr, "chat_area = %p, subgoal = %p\n",chat_area,subg_ptr);
	}
#endif
	cr = cr_next(cr);
      }
    }
    reclaim_cr_space(tcp_ptr);
}

/*----------------------------------------------------------------------*/
