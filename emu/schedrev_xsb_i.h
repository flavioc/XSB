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

static CPtr sched_answers(VariantSF producer_sf, CPtr producer_cpf,
			  xsbBool is_leader) {

  CPtr first_sched_cons, last_sched_cons, consumer_cpf;


#ifdef PROFILE
  subinst_table[SCHED_ANSWERS][1]++;
#endif	

  first_sched_cons = last_sched_cons = NULL;
  consumer_cpf = subg_asf_list_ptr(producer_sf);

  /**** The producer has answers and consuming calls ****/  
  if ( has_answers(producer_sf) && IsNonNULL(consumer_cpf) ) {
#ifdef DEBUG_REV
    xsb_dbgmsg("SchedAnswers: consumer_cpf=%d,producer_sf=%d",
	       (int)consumer_cpf,(int)producer_sf);
#endif
    /**** Check each consumer for unresolved answers ****/
    if ( IsSubsumptiveProducer(producer_sf) )
      while ( IsNonNULL(consumer_cpf) ) {
	ALNptr answer_set = ALN_Next(nlcp_trie_return(consumer_cpf));
	SubConsSF consumer_sf = (SubConsSF)nlcp_subgoal_ptr(consumer_cpf);
	if ( IsNULL(answer_set) && ((VariantSF)consumer_sf != producer_sf) )
	  if ( MoreAnswersAvailable(consumer_sf,producer_sf) ) {
	    switch_envs(consumer_cpf);
	    answer_set =
	      table_identify_relevant_answers((SubProdSF)producer_sf,
					      consumer_sf,
					      consumer_cpf + NLCPSIZE);
	  }
	if ( IsNonNULL(answer_set) )
	  ScheduleConsumer(consumer_cpf,first_sched_cons,last_sched_cons);
	consumer_cpf = nlcp_prevlookup(consumer_cpf);
      }
    else
      while ( IsNonNULL(consumer_cpf) ) {
	if ( IsNonNULL(ALN_Next(nlcp_trie_return(consumer_cpf))) )
	  ScheduleConsumer(consumer_cpf,first_sched_cons,last_sched_cons);
	consumer_cpf = nlcp_prevlookup(consumer_cpf);
      }
    
    /**** The last consumer always backtracks to the producer ****/
    if ( IsNonNULL(last_sched_cons) )
      nlcp_prevbreg(last_sched_cons) = producer_cpf;
  } /* if any answers and active nodes */


#ifdef LOCAL_EVAL /*-----------------------------------------------------*/
  /* if leader, nothing needs to be done -- its answers will only be 
   * returned after the SCC is completed 
   * ow, schedules answers to the generator node, changing 
   * the flag of the TCP
   */
  if ( ! is_leader ) {
    /* if not leader and gen has not been scheduled before */
    if(tcp_trie_return(producer_cpf)==NULL) {
#ifdef DEBUG_REV
      xsb_dbgmsg("-----> LEADER became NON_LEADER, sched gen %d",
		 (int)producer_cpf);
#endif
      tcp_trie_return(producer_cpf) = subg_ans_list_ptr(producer_sf);
    }
    if(ALN_Next(tcp_trie_return(producer_cpf))) {
      /*so that checkcompl will ret ans*/
      tcp_tag(producer_cpf) = (int)RETRY_GEN_ACTIVE_TAG;
      if (!last_sched_cons)
	last_sched_cons = first_sched_cons = producer_cpf;
    }
  } /* if not leader */
#endif /* LOCAL_EVAL ----------------------------------------------------*/
  
#ifdef DEBUG_REV
  xsb_dbgmsg("schedule active nodes: ccbreg=%d, breg=%d", 
	     (int)producer_cpf,(int)breg);
  xsb_dbgmsg("first active =%d, last=%d",
	     (int)first_sched_cons,(int)last_sched_cons);
#endif
  return first_sched_cons;
}

/*-------------------------------------------------------------------------*/

/* returns 0 if reached fixpoint, otherwise, returns the next breg 
 * for batched - should extend to local (but first decide if can
 * do local without sched_ans in subg_frame)
 * also try to do for batched what I did for local: at fixpoint
 * create a sched_chain of all active nodes with unresolved answers
 * I recall that kostis mentioned cps should not be relinked
 * I cannot remember why...
 */

static CPtr find_fixpoint(VariantSF subg, CPtr producer_cpf) {

  VariantSF currSubg;
  CPtr complFrame; /* completion frame for currSubg */
  CPtr tcp; /* choice point for currSubg */
  CPtr sched_chain = 0, prev_sched = 0, tmp_sched = 0; /* build sched chain */

#ifdef PROFILE
  subinst_table[OUTER_FIXPOINT][1]++;
#endif
  complFrame = openreg;
  /* for each subgoal in the ASCC, from youngest to leader there is no
   * need to include the leader 
   */
  while(complFrame < subg_compl_stack_ptr(subg)) {
#ifdef PROFILE
    subinst_table[ITER_FIXPOINT][1]++;
#endif
    currSubg = compl_subgoal_ptr(complFrame);
    /* check if all answers have been resolved for this subgoal */
    tcp = subg_cp_ptr(currSubg);

    /* if there are unresolved answers for currSubg */
    if ((tmp_sched = sched_answers(currSubg, tcp, FALSE))) {
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
