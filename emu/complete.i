/*  -*-c-*-  Make sure this file comes up in the C mode of emacs */ 
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

/*----------------------------------------------------------------------*/

#define ComplStkFrame	xtemp2
#define CC_CSPTR	xtemp5

/*----------------------------------------------------------------------*/

case check_complete: {
  CPtr orig_breg = breg, tmp_breg;
  bool leader = FALSE;

  /* this CP has exhausted program resolution -- backtracking occurs */
  switch_envs(breg);    /* in CHAT: undo_bindings() */
  ptcpreg = tcp_ptcp(breg);
  delayreg = tcp_pdreg(breg);

  SUBGOAL = tcp_subgoal_ptr(breg);	/* get the subgoal that is checked */

#ifdef DEBUG_DELAY
  fprintf(stderr, ">>>> check_complete is called.  The checked subgoal is: ");
  print_subgoal(stderr, (SGFrame)SUBGOAL); fprintf(stderr, "\n");
#endif

  CC_CSPTR = subg_compl_stack_ptr(SUBGOAL);

  if ((prev_compl_frame(CC_CSPTR) >= COMPLSTACKBOTTOM || is_leader(CC_CSPTR))) {
    leader = 1;
  }
  
#if (!defined(CHAT))
#ifdef LOCAL_EVAL
  /* return answers to caller of generator subgoals that are not leaders */
  if (tcp_tag(breg) == RETRY_GEN_ACTIVE_TAG) {
    if (tcp_trie_return(breg) == NULL) { 
      /* this can only happen if answers are deleted */ 
      tcp_tag(breg) = CHECK_COMPLETE_TAG; 
    } else { /* This code mimics the answer_return code */
      restore_some_wamregs(breg,ereg); 
      ARITY = tcp_arity(breg); 
      CallNumVar = *(breg + TCP_SIZE + (Cell)ARITY);
      CallNumVar = int_val(CallNumVar); /* # of SF vars is stored tagged */
      op3 = breg + TCP_SIZE + (Cell) ARITY + CallNumVar; 
      OldRetPtr = aln_next_aln(tcp_trie_return(breg)); /* get next answer */ 
      if (OldRetPtr) {
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
	    
	    if (num_heap_term_vars == 0) {
	      delay_positively(SUBGOAL, aln_answer_ptr(tcp_trie_return(breg)),
			       makestring((char *) ret_psc[0]));
	    }
	    else {
	      temp_hreg = hreg;
	      new_heap_functor(hreg, get_ret_psc(num_heap_term_vars));
	      for (i = 0; i < num_heap_term_vars; i++)
		cell(hreg++) = (Cell) var_addr[i];
	      delay_positively(SUBGOAL, aln_answer_ptr(tcp_trie_return(breg)),
			       makecs(temp_hreg));
	    }
	  }
	}
	lpcreg = cpreg;
	goto contcase;
      } 
      else { 
	tcp_tag(breg) = CHECK_COMPLETE_TAG;      
      }   
    } 
  } /* retry_gen_active */
#endif /* LOCAL_EVAL */

  /* schedule answers */
  if ((tmp_breg = sched_answers(SUBGOAL, breg, leader))){
    breg = tmp_breg;
    Fail1;  
    goto contcase;
  }
#endif /* not CHAT */


  if (leader) {
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
  {
    SGFrame compl_subg; /* subgoal to be completed */
    CPtr cc_tbreg = orig_breg;

    /* mark topmost SCC as completed */ 
    breg = orig_breg;
    ComplStkFrame = CC_CSPTR;

    /* check from leader up to the youngest subgoal */
    while (ComplStkFrame >= openreg) {
      compl_subg = compl_subgoal_ptr(ComplStkFrame);
      if (is_completed(compl_subg)) { /* this was early completed */
#ifdef CHAT
	chat_free_compl_susp_chat_areas((SGFrame)compl_subg);
#else
	subg_compl_susp_ptr(compl_subg) = NULL;
#endif
      }
      else { /* if not early completed */
	CPtr nsf;
	if ((nsf = subg_compl_susp_ptr(compl_subg)) != NULL) { 
	  CPtr p, prev_nsf = NULL;
	  /* check each suspension frame for appropriate action: if
	     their parents are completed these completion suspensions
	     fail, so forget about them; o/w delay them and let
	     simplification take care of the rest */
	  while (nsf) {
	    if ((p = csf_ptcp(nsf)) != NULL) {
	      if (is_completed(p)) {
		if (!prev_nsf) { /* deleting the first susp is special */
#ifdef CHAT
		  nsf = subg_compl_susp_ptr(compl_subg) =
		    chat_free_compl_susp_chat_area((chat_init_pheader)nsf);
#else
		  nsf = subg_compl_susp_ptr(compl_subg) = csf_prevcsf(nsf);
#endif
		}
		else {
#ifdef CHAT
		  nsf = csf_prevcsf(prev_nsf) =
		    chat_free_compl_susp_chat_area((chat_init_pheader)nsf);
#else
		  nsf = csf_prevcsf(prev_nsf) = csf_prevcsf(nsf);
#endif
		}
	      }
	      else { /* this completion suspension will be delayed */
		mark_delayed(ComplStkFrame, subg_compl_stack_ptr(p), nsf);
		prev_nsf = nsf;
		nsf = csf_prevcsf(nsf);
	      }
	    }
	  } /* while */
	  if ((nsf = subg_compl_susp_ptr(compl_subg))) {
#ifdef CHAT
	    CPtr H, EB;

	    H = cp_hreg(breg);
	    EB = cp_ebreg(breg);

	    breg = cc_tbreg = chat_restore_compl_susp((chat_init_pheader)nsf, H, EB);
	    subg_compl_susp_ptr(compl_subg) = NULL;
#else
	    set_min(xtemp1, breg, bfreg);
	    save_compl_susp_cp(xtemp1,cc_tbreg,nsf);
	    subg_compl_susp_ptr(compl_subg) = NULL;
	    breg = cc_tbreg = xtemp1;		  
#endif
	  }
	} /* if there are completion suspensions */
      } /* else if not early completed */
      ComplStkFrame = next_compl_frame(ComplStkFrame);
    } /* while - for each subg in compl stack */

    /* find continuation and reclaim space - if there are no completion
     * susps, sched answers if any */
    if (cc_tbreg==orig_breg) { 
      /* no delays, mark all SCC as completed and do simplification */
      ComplStkFrame = CC_CSPTR; 
      while (ComplStkFrame >= openreg) {
	compl_subg = compl_subgoal_ptr(ComplStkFrame);
	mark_as_completed(compl_subg); 
	if (neg_simplif_possible(compl_subg)) {
	  simplify_neg_fails(compl_subg);
	}
	ComplStkFrame = next_compl_frame(ComplStkFrame);
      } /* while */

      /* reclaim all answer lists, but the one for the leader */
      ComplStkFrame = next_compl_frame(CC_CSPTR); 
      while (ComplStkFrame >= openreg) {
	compl_subg = compl_subgoal_ptr(ComplStkFrame);
	reclaim_subg_space(compl_subg);
	ComplStkFrame = next_compl_frame(ComplStkFrame);
      } /* while */

      /* if subgoal has answers and these haven't been returned yet
       * (when answers are returned, the answer list is reclaimed
       * and set to 0, 1 or 2) */
      if (has_answer_code(SUBGOAL) && (subg_answers(SUBGOAL) > COND_ANSWERS)) {
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
	tcp_tag(orig_breg) = CHECK_COMPLETE_TAG; 
#endif
	switch_envs(orig_breg); 
	/* check where this brings the stacks, that will determine how
	   much can be reclaimed if there are answers to be returned */
	ptcpreg = tcp_ptcp(orig_breg);
	delayreg = tcp_pdreg(orig_breg);
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
	CallNumVar = int_val(CallNumVar); /* # of SF vars is stored tagged */
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
	breg = tcp_prevbreg(orig_breg); /* orig_???*/ 
	delay_it = 1;
	goto contcase;
      } /* if there are answers */   
      else { /* no answers to return */
	reclaim_subg_space((SGFrame) SUBGOAL);

	/* there is nothing else to be done for this SCC */
	cc_tbreg = tcp_prevbreg(orig_breg); /* go to prev SCC */
		
	openreg = prev_compl_frame(CC_CSPTR); 
	reclaim_stacks(orig_breg);
      } /* no answers to return */
    } /* if(cc_tbreg==orig_breg) */
    breg = cc_tbreg; /* either orig, prev_cp or compl_susp */
  }
#else /* not LOCAL_EVAL */
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


