/*  -*-c-*-  Make sure this file comes up in the C mode of emacs */ 
/* File:      schedrev.i
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


/**********************************************************************/
/* schedule answers that haven't been returned yet 
 * if returns 1, answers have been scheduled and engine will have
 * to fail over them, else continue forward execution
 * orig_breg is the address of the TCP
 */
static CPtr sched_answers(SGFrame subg_struct, CPtr orig_breg, int leader) {
  ALNptr tmp_ret_ptr;
  CPtr first_sched_node, prev_node, active_node;

#ifdef PROFILE
  subinst_table[SCHED_ANSWERS][1]++;
#endif	
  first_sched_node = prev_node = (CPtr)0;

  /* there answers and consuming nodes */  
  if(subg_answers(subg_struct) && 
     (active_node = subg_asf_list_ptr(subg_struct))) {
#ifdef DEBUG_REV
    xsb_dbgmsg("SchedAnswers: active_node=%d,subg_struct=%d",
	       (int)active_node,(int)subg_struct);
#endif
    /* check if any suspended node still has unresolved answers */
    while(active_node) {
      tmp_ret_ptr = nlcp_trie_return(active_node);
      /* if there is a new answer  - schedule */
      if(tmp_ret_ptr && aln_next_aln(tmp_ret_ptr)) { 
	if (prev_node){
	  nlcp_prevbreg(prev_node) = active_node;
	}
	else {	 /* record first active node to backtrack to */
	  first_sched_node = active_node;
	}
	prev_node = active_node;
      } /* if (tmp_ret...) */
      active_node = nlcp_prevlookup(active_node);
    } /* while active node */
    
    /* last active node always has to backtrack to TCP */
    if (prev_node) nlcp_prevbreg(prev_node) = orig_breg;
  } /* if any answers and active nodes */


#ifdef LOCAL_EVAL
  /* if leader, nothing needs to be done -- its answers will only be 
   * returned after the SCC is completed 
   * ow, schedules answers to the generator node, changing 
   * the flag of the TCP
   */
  if (!leader) {
    /* if not leader and gen has not been scheduled before */
    if(tcp_trie_return(orig_breg)==NULL) {
#ifdef DEBUG_REV
      xsb_dbgmsg("-----> LEADER became NON_LEADER, sched gen %d",
		 (int)orig_breg);
#endif
      tcp_trie_return(orig_breg) = subg_ans_list_ptr(subg_struct);
    }
    if(aln_next_aln(tcp_trie_return(orig_breg))) {
      /*so that checkcompl will ret ans*/
      tcp_tag(orig_breg) = (int)RETRY_GEN_ACTIVE_TAG;
      if (!prev_node)
	prev_node = first_sched_node = orig_breg;
    }
  } /* if not leader */
#endif /* LOCAL_EVAL */
  
#ifdef DEBUG_REV
  xsb_dbgmsg("schedule active nodes: ccbreg=%d, breg=%d", 
	     (int)orig_breg,(int)breg);
  xsb_dbgmsg("first active =%d, last=%d",
	     (int)first_sched_node,(int)prev_node);
#endif
  return first_sched_node;
}

/* returns 0 if reached fixpoint, otherwise, returns the next breg 
 * for batched - should extend to local (but first decide if can
 * do local without sched_ans in subg_frame)
 * also try to do for batched what I did for local: at fixpoint
 * create a sched_chain of all active nodes with unresolved answers
 * I recall that kostis mentioned cps should not be relinked
 * I cannot remember why...
 */
static CPtr find_fixpoint(SGFrame subg, CPtr orig_breg) {
  SGFrame currSubg;
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
    if ((tmp_sched = sched_answers(currSubg, tcp, 0))) {
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
      tcp_prevbreg(tcp) = orig_breg;  /* backtrack to leader */
    }
    complFrame = prev_compl_frame(complFrame);	
  }  /* while */

  if (prev_sched)  /* if anything has been scheduled */
    /* the first generator should backtrack to leader */
    tcp_prevbreg(prev_sched) = orig_breg;  

  return sched_chain;
}
