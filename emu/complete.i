/* File:      complete.i
** Author(s): Juliana Freire, Kostis Sagonas, Terry Swift
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
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


/* special debug includes */
#include "debugs/debug_delay.h"


#define FIXPOINT

#define ComplStkFrame	xtemp2
#define CC_CSPTR	xtemp5

#define CC_TBREG	xtemp6

#ifdef LOCAL_EVAL
#define COMPL_SUBG	xtemp15	/* subgoal to be completed */
#endif

/*----------------------------------------------------------------------*/

case check_complete: {
  CPtr orig_breg = breg, tmp_breg;
  bool leader = FALSE;

  ptcpreg = tcp_ptcp(breg);	/* this CP has exhausted program resolution */

  SUBGOAL = tcp_subgoal_ptr(breg);	/* get the subgoal that is checked */

#ifdef DEBUG_DELAY
  xcurcall = SUBGOAL;
  fprintf(stderr, ">>>> check_complete is called.  The checked subgoal is: ");
  print_subgoal(stderr, (SGFrame) xcurcall); fprintf(stderr, "\n");
#endif

  CC_CSPTR = subg_compl_stack_ptr(SUBGOAL);

  if ((prev_compl_frame(CC_CSPTR) >= COMPLSTACKBOTTOM || is_leader(CC_CSPTR))) {
    leader = 1;
  }
  
#if (defined(LOCAL_EVAL) && !defined(CHAT))
  /* return answers to caller of generator subgoals that are not leaders */
  if (tcp_tag(breg)==(Cell)RETRY_GEN_ACTIVE_TAG) {
    if (tcp_trie_return(breg) == NULL) { 
      /* this can only happen if answers are deleted */ 
      tcp_tag(breg) = (int)CHECK_COMPLETE_TAG; 
    } else { /* This code mimics the answer_return code */
      switch_envs(breg);  
      ptcpreg = tcp_ptcp(breg); 
      delayreg = tcp_pdreg(breg); 
      restore_some_wamregs(breg,ereg); 
      ARITY = tcp_arity(breg); 
      CallNumVar = *(breg + TCP_SIZE + (Cell)ARITY); 
      op3 = breg + TCP_SIZE + (Cell) ARITY + CallNumVar; 
      OldRetPtr = aln_next_aln(tcp_trie_return(breg)); /* get next answer */ 
      if (OldRetPtr){
	tcp_trie_return(breg) = OldRetPtr; 
	/* last answer consumed */ 
	TrieRetPtr = get_next_trie_solution(&OldRetPtr); 
	load_solution_trie(CallNumVar,op3,TrieRetPtr); 
	/*
	 * This piece of code is new (different from version 1.8.1) for
	 * delay variable stuff.  In LOCAL_EVAL, in order to save the
	 * substitution factor of the answer into the delay list for the
	 * parent subgoal, we have to get it from var_addr[] (the result
	 * of load_solution_trie()) instead of from the pointer
	 * `ans_var_pos_reg'.  `ans_var_pos_reg' points to the
	 * substitution factor of the answer that we saved on the heap in
	 * variant_trie_search().  Because the space of heap may be
	 * reclaimed (see restore_some_wamregs() in check_complete), the
	 * substitution factor information we saved in heap may be
	 * overwritten.
	 *
	 * This is similar to the situation in answer_return and
	 * lay_down_consumer.
	 */
	if (is_conditional_answer(aln_answer_ptr(tcp_trie_return(breg)))) { 
#ifdef DEBUG_DELAYVAR
	  fprintf(stderr, ">>>> delay_positively in check_complete\n");
#endif
	  {
	    int i;
	    CPtr temp_hreg;
	    
	    temp_hreg = hreg;
	    new_heap_functor(hreg, get_ret_psc(num_heap_term_vars));
	    for (i = 0; i < num_heap_term_vars; i++)
	      cell(hreg++) = (Cell) var_addr[i];
	    delay_positively(SUBGOAL, aln_answer_ptr(tcp_trie_return(breg)),
			     temp_hreg);
	  }
	}
	lpcreg = cpreg;
	goto contcase;
      } 
      else { 
	tcp_tag(breg) = (int)CHECK_COMPLETE_TAG;      
      }   
    } 
  } /* retry_gen_active */
#endif /* LOCAL_EVAL */

#ifdef CHAT
  undo_bindings(breg);
#else
  /* schedule answers */
  if ((tmp_breg = sched_answers(SUBGOAL, breg, leader))){
    breg = tmp_breg;
    Fail1;  
    goto contcase;
  }
#endif

  if(leader) {
    /* check if fixpoint has been reached, otherwise schedule any
     * unresolved answers */
#ifdef CHAT
    if ((tmp_breg = chat_fixpoint((SGFrame)SUBGOAL, (TChoice)breg))) {
#else
    if ((tmp_breg = find_fixpoint(SUBGOAL, breg))) {
#endif
      breg = tmp_breg;
      Fail1;
      goto contcase;
    }

#ifdef LOCAL_EVAL
    CC_TBREG=orig_breg;
    /* mark topmost SCC as completed */ 
    breg = orig_breg;
    ComplStkFrame = CC_CSPTR;

    /* check from leader up to the youngest subgoal */
    while (ComplStkFrame >= openreg) {
      COMPL_SUBG = compl_subgoal_ptr(ComplStkFrame);
      if (is_completed(COMPL_SUBG)) { /* this was early completed */
#ifdef CHAT
	chat_free_compl_susp_chat_areas((SGFrame)COMPL_SUBG);
#else
	subg_compl_susp_ptr(COMPL_SUBG) = NULL;
#endif
     }
      else { /* if not early completed */
	CPtr p, nsf;
	if ((nsf = subg_compl_susp_ptr(COMPL_SUBG)) != NULL) { 
	  /* remove completion suspensions whose parents have been completed */
	  CPtr prev_nsf = NULL;
	  while (nsf) { /* check each suspension frame */
	    if ((p = csf_ptcp(nsf)) != NULL) {
#ifdef PTCP_IN_CP
	      p = tcp_subgoal_ptr(p);
#endif
	      if (is_completed(p)) {
		/* remove nsf from list */
		if (!prev_nsf) { /* if first susp is to be deleted */
		  nsf = subg_compl_susp_ptr(COMPL_SUBG) = csf_prevcsf(nsf);
		}
		else {
		  csf_prevcsf(prev_nsf) = csf_prevcsf(nsf);
		  nsf = csf_prevcsf(prev_nsf);
		}
	      } /* if is_completed parent  */
	      else {
		mark_delayed(ComplStkFrame, subg_compl_stack_ptr(p), nsf);
		prev_nsf = nsf;
		nsf = csf_prevcsf(nsf);
	      } /* if(is_c ... parent is completed */
	    } /* if (csf_ ... if parent is tabled */
	  } /* while (nsf) */
	  if ((nsf = subg_compl_susp_ptr(COMPL_SUBG))) {
#ifdef CHAT
	    CPtr H, EB;

	    H = cp_hreg(breg);
	    EB = cp_ebreg(breg);

	    breg = CC_TBREG = chat_restore_compl_susp((chat_init_pheader)nsf, H, EB);
	    subg_compl_susp_ptr(COMPL_SUBG) = NULL;
#else
	    set_min(xtemp1, breg, bfreg);
	    save_compl_susp_cp(xtemp1,CC_TBREG,nsf);
	    subg_compl_susp_ptr(COMPL_SUBG) = NULL;
	    breg = CC_TBREG = xtemp1;		  
#endif
	  }
	} /* if there are neg. suspensions */
      } /* else if not early completed */
      ComplStkFrame = next_compl_frame(ComplStkFrame);
    } /* while - for each subg in compl stack */

    /* find continuation and reclaim space - if there are no negation
     * susps, sched answers if any */
    if (CC_TBREG==orig_breg) { 
      /* no delays, mark all SCC as completed and do simplification */
      ComplStkFrame = CC_CSPTR; 
      while (ComplStkFrame >= openreg) {
	COMPL_SUBG = compl_subgoal_ptr(ComplStkFrame);
	mark_as_completed(COMPL_SUBG); 
	if (neg_simplif_possible(COMPL_SUBG)) {
	  simplify_neg_fails(COMPL_SUBG);
	}
	ComplStkFrame = next_compl_frame(ComplStkFrame);
      } /* while */

      /* reclaim all answer lists, but the one for the leader */
      ComplStkFrame = next_compl_frame(CC_CSPTR); 
      while (ComplStkFrame >= openreg) {
	COMPL_SUBG = compl_subgoal_ptr(ComplStkFrame);
	reclaim_subg_space(COMPL_SUBG);
	ComplStkFrame = next_compl_frame(ComplStkFrame);
      } /* while */

      /* if subgoal has answers and these haven't been returned yet
       * (when answers are returned, the answer list is reclaimed
       * and set to 0, 1 or 2) */
      if (has_answer_code(SUBGOAL) && (subg_answers(SUBGOAL)>COND_ANSWERS)) {
	reclaim_subg_space((SGFrame)SUBGOAL);
	/* so that answers will be returned right after the neg susps
	 * are returned and engine fails over TCP
	 */

	/* schedule return of answers */
	if (breg!=orig_breg) {
	  fprintf(stderr,"breg!=origbreg(%d,%d)\n",(int)breg,(int)orig_breg);
	  orig_breg = breg;
	  fprintf(stderr,"Making orig_breg=%d\n",(int)orig_breg);
	}
#if (!defined(CHAT))
	tcp_tag(orig_breg)=(Cell)CHECK_COMPLETE_TAG; 
#endif
#ifdef DEBUG_REV
	fprintf(stderr,"===>TRIE Returning answers for breg=%d\n",(int)breg);
#endif
	switch_envs(orig_breg); 
	/* check where this brings the stacks, that will
	 * determine how much can be reclaimed if there
	 * are answers to be returned 
	 */
	/* JF: change breg to orig_breg */
#ifdef CHAT
	ptcpreg = compl_ptcp(CC_CSPTR);
	delayreg = compl_pdreg(CC_CSPTR);
#else
	ptcpreg = tcp_ptcp(orig_breg);
	delayreg = tcp_pdreg(orig_breg);
#endif
	restore_some_wamregs(orig_breg, ereg);
	/* restore_trail_condition_registers - because success path
	 * will be followed
	 */
	ebreg = cp_ebreg(tcp_prevbreg(orig_breg));
	hbreg = cp_hreg(tcp_prevbreg(orig_breg));
#ifdef CHAT
	compl_cons_copy_list(CC_CSPTR) = 0;
#else
	subg_asf_list_ptr(SUBGOAL) = 0;
#endif
	
	/* reclaim stacks, including leader */
	openreg = prev_compl_frame(CC_CSPTR);
	reclaim_stacks(orig_breg);
#ifdef CHAT
	CallNumVar = int_val(cell(compl_hreg(CC_CSPTR)));
	VarsInCall = compl_hreg(CC_CSPTR)-CallNumVar;	/* first variable */
#else
	ARITY = tcp_arity(orig_breg);
	CallNumVar = *(orig_breg + TCP_SIZE + (Cell)ARITY);
	VarsInCall = orig_breg+TCP_SIZE+(Cell)ARITY + 1; /* first variable */
#endif
	for (i=0; i<CallNumVar; i++) {
	  cptr = VarsInCall;
	  pushreg(*cptr);
	  VarsInCall++;
	} /* for (i=0... */
	num_vars_in_var_regs = -1;
	lpcreg = (byte *)subg_ans_root_ptr(SUBGOAL);
	/* backtrack to prev tabled subgoal after returning answers */
	breg =  tcp_prevbreg(orig_breg); /* orig_???*/ 
	delay_it = 1;
	goto contcase;
      } /* if there are answers */   
      else { /* no answers to return */
	reclaim_subg_space((SGFrame) SUBGOAL);

	/* there is nothing else to be done for this SCC */
	CC_TBREG = tcp_prevbreg(orig_breg); /* go to prev SCC */
		
	openreg = prev_compl_frame(CC_CSPTR); 
	reclaim_stacks(orig_breg);
      } /* no answers to return */
    } /* if(CC_TBREG==orig_breg) */
    breg = CC_TBREG; /* either orig, prev_cp or compl_susp */
 
#else /* LOCAL_EVAL */
    batched_compute_wfs(CC_CSPTR, orig_breg, SUBGOAL);

    /* do all possible stack reclamation */
    if (openreg == prev_compl_frame(CC_CSPTR)) {
      reclaim_stacks(orig_breg);
      if (breg == orig_breg) {
	breg = tcp_prevbreg(breg);
#ifdef CHAT
	restore_trail_condition_registers(breg);
#endif
      }
    }
#endif /* LOCAL_EVAL */
  }
  /* if not leader */
  else {
#ifdef CHAT
    chat_incremental_copy((SGFrame)SUBGOAL);
    /* let's cause some segmentation faults and fix exact completion in CHAT */
    subg_cp_ptr(SUBGOAL) = NULL;	/* preserve invariants */
#endif
    breg = tcp_prevbreg(breg); 
    /* since the trail condition registers are not reset in
     * tabletrust for CHAT or for local, they should be restored here
     */
#ifdef CHAT
    restore_trail_condition_registers(breg);
#else
#ifdef LOCAL_EVAL
    hbreg = tcp_hreg(breg);
    ebreg = tcp_ebreg(breg);
#endif
#endif
  }
  Fail1;
  goto contcase;
} /* end of check_complete */


