/* File:      schedrev_xsb_i.h
** Author(s): Juliana Freire
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

/*-------------------------------------------------------------------------*/

/*
 * Schedules consumers to which at least some answers haven't been
 * returned.  Does so by linking them through their "previous breg" choice
 * point fields.  The value returned is the scheduling chain of such
 * consumers (NULL if no such consumers of this producer exist).
 */

#define ScheduleConsumer(Consumer,First,Last) {		\
   if ( IsNonNULL(Last) )				\
     nlcp_prevbreg(Last) = Consumer;			\
   else	 /* record first consumer to backtrack to */	\
     First = Consumer;					\
   Last = Consumer;					\
 }

static CPtr sched_answers(CTXTdeclc VariantSF producer_sf, CPtr producer_cpf,
			  xsbBool is_leader)
{

  CPtr first_sched_cons, last_sched_cons, consumer_cpf;


#ifdef PROFILE
  subinst_table[SCHED_ANSWERS][1]++;
#endif	

  first_sched_cons = last_sched_cons = NULL;
  consumer_cpf = subg_asf_list_ptr(producer_sf);

  /**** The producer has answers and consuming calls ****/  
  if ( has_answers(producer_sf) && IsNonNULL(consumer_cpf) ) {
    xsb_dbgmsg((LOG_SCHED, "SchedAnswers: consumer_cpf=%d,producer_sf=%d",
	       (int)consumer_cpf,(int)producer_sf));
    /**** Check each consumer for unresolved answers ****/
    if ( IsSubsumptiveProducer(producer_sf) )
      while ( IsNonNULL(consumer_cpf) ) {
	SubConsSF consumer_sf;
	ALNptr answer_continuation;
	BTNptr next_answer;

#define ANSWER_TEMPLATE nlcp_template(consumer_cpf)

	consumer_sf = (SubConsSF)nlcp_subgoal_ptr(consumer_cpf);

	table_pending_answer( nlcp_trie_return(consumer_cpf),
			      answer_continuation,
			      next_answer,
			      consumer_sf,
			      (SubProdSF)producer_sf,
			      ANSWER_TEMPLATE,
			      switch_envs(consumer_cpf),
			      TPA_NoOp );
	if ( IsNonNULL(answer_continuation) )
	  ScheduleConsumer(consumer_cpf,first_sched_cons,last_sched_cons);
	consumer_cpf = nlcp_prevlookup(consumer_cpf);
      }
    else
      while ( IsNonNULL(consumer_cpf) ) {
	if ( IsNonNULL(ALN_Next(nlcp_trie_return(consumer_cpf))) )
	  ScheduleConsumer(consumer_cpf,first_sched_cons,last_sched_cons);
	consumer_cpf = nlcp_prevlookup(consumer_cpf);
      }
    
    if ( IsNonNULL(last_sched_cons))
      nlcp_prevbreg(last_sched_cons) = breg;
  } /* if any answers and active nodes */

  xsb_dbgmsg((LOG_SCHED, "schedule active nodes: ccbreg=%d, breg=%d", 
	     (int)producer_cpf,(int)breg));
  xsb_dbgmsg((LOG_SCHED, "first active =%d, last=%d",
	     (int)first_sched_cons,(int)last_sched_cons));
  return first_sched_cons;
}

/*-------------------------------------------------------------------------*/

/* returns 0 if reached fixpoint, otherwise, returns the next breg 
 * for batched.  Essentially this routine performs a sched_answer()
 * for each subgoal in the (A)SCC except the leader.  This
 * sched_answer() has already been performed earlier in the
 * check_complete instruction.
 */

static CPtr find_fixpoint(CTXTdeclc VariantSF subg, CPtr producer_cpf) 
{

  VariantSF currSubg;
  CPtr complFrame; /* completion frame for currSubg */
  CPtr tcp; /* choice point for currSubg */
  CPtr sched_chain = 0, prev_sched = 0, tmp_sched = 0; /* build sched chain */

#ifdef PROFILE
  subinst_table[OUTER_FIXPOINT][1]++;
#endif
  complFrame = openreg;
  /* for each subgoal in the ASCC, from youngest to leader there is no
   * need to include the leader.  This is because sched_answers() is
   * done for each subgoal whenever it executes a check_complete
   * operation. Thus, scheduling for the leader has already been done.
   */
  while(complFrame < subg_compl_stack_ptr(subg)) {
#ifdef PROFILE
    subinst_table[ITER_FIXPOINT][1]++;
#endif
    currSubg = compl_subgoal_ptr(complFrame);
    /* check if all answers have been resolved for this subgoal */
    tcp = subg_cp_ptr(currSubg);

    /* if there are unresolved answers for currSubg */
    if ((tmp_sched = sched_answers(CTXTc currSubg, tcp, FALSE))) {
      if (prev_sched) { /* if there is a prev subgoal scheduled */
	/* link new node to the previous one */
	tcp_prevbreg(prev_sched) = tmp_sched;
      }
      else {
	sched_chain = tmp_sched; /* first node in the chain */
      }
      prev_sched = tcp;
    }
    else { /* no unresolved answers for currSubg */
      tcp_prevbreg(tcp) = producer_cpf;  /* backtrack to leader */
    }
    complFrame = prev_compl_frame(complFrame);	
  }  /* while */

  if (prev_sched)  /* if anything has been scheduled */
    /* the first generator should backtrack to leader */
    tcp_prevbreg(prev_sched) = producer_cpf;  

  return sched_chain;
}
