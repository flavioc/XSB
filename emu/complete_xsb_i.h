/* File:      complete_xsb_i.h
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

XSB_Start_Instr(check_complete,_check_complete)
  CPtr    cs_ptr;
  CPtr    orig_breg = breg;
  xsbBool leader = FALSE;
  VariantSF subgoal;
#ifdef LOCAL_EVAL
  int     i;
#ifndef CHAT
  Def1op
#endif
#endif

  /* this CP has exhausted program resolution -- backtracking occurs */
  switch_envs(breg);    /* in CHAT: undo_bindings() */
  ptcpreg = tcp_ptcp(breg);
  delayreg = tcp_pdreg(breg);

  subgoal = (VariantSF) tcp_subgoal_ptr(breg);	/* subgoal that is checked */

#ifdef DEBUG_DELAY
  fprintf(stderr, ">>>> check_complete is called.  The checked subgoal is: ");
  print_subgoal(stderr, subgoal); fprintf(stderr, "\n");
#endif

  cs_ptr = subg_compl_stack_ptr(subgoal);

  if ((prev_compl_frame(cs_ptr) >= COMPLSTACKBOTTOM || is_leader(cs_ptr))) {
    leader = 1;
  }
  
/*==========================================================================*/
#if (!defined(CHAT))
/*--------------------------------------------------------------------------*/
#ifdef LOCAL_EVAL
/*--------------------------------------------------------------------------*/
  /* return answers to caller of generator subgoals that are not leaders */
  if (tcp_tag(breg) == RETRY_GEN_ACTIVE_TAG) {
    if (tcp_trie_return(breg) == NULL) { 
      /* this can only happen if answers are deleted */ 
      tcp_tag(breg) = CHECK_COMPLETE_TAG; 
    }
    else { /* This code mimics the answer_return code */
      CPtr answer_template;
      int template_size, attv_num, tmp;
      ALNptr answer_set;
      BTNptr answer_leaf;

      restore_some_wamregs(breg,ereg); 
      answer_set = ALN_Next(tcp_trie_return(breg));   /* get next answer */ 
      if ( IsNonNULL(answer_set) ) {
	tcp_trie_return(breg) = answer_set;   /* update answer continuation */
	ARITY = tcp_arity(breg); 
	answer_template = breg + TCP_SIZE + (Cell)ARITY;

	tmp = int_val(cell(answer_template));
	get_var_and_attv_nums(template_size, attv_num, tmp);
	answer_template += template_size;
	answer_leaf = ALN_Answer(answer_set);
	table_consume_answer(answer_leaf,template_size,attv_num,answer_template,
			     subg_tif_ptr(subgoal));

	/*
	 * This piece of code was added in (version 1.8.1) to handle
	 * properly variables in delay lists.  Itworks in a manner
	 * similar to answer_return (and lay down consumer, for that
	 * matter).  In order to save the
	 * substitution factor of a conditional answer into the delay list 
	 * for the root subgoal, we have to get it from var_addr[] and
	 * num_heap_term_vars (both set by table_consume_answer()).
	 */
	if (is_conditional_answer(ALN_Answer(tcp_trie_return(breg)))) { 
#ifdef DEBUG_DELAYVAR
	  fprintf(stderr, ">>>> delay_positively in check_complete\n");
#endif
	  {
	    if (num_heap_term_vars == 0) {
	      delay_positively(subgoal, ALN_Answer(tcp_trie_return(breg)),
			       makestring(get_ret_string()));
	    }
	    else {
#ifndef IGNORE_DELAYVAR
	      CPtr temp_hreg = hreg;
	      new_heap_functor(hreg, get_ret_psc(num_heap_term_vars));
	      for (i = 0; i < num_heap_term_vars; i++)
		cell(hreg++) = (Cell) var_addr[i];
	      delay_positively(subgoal, ALN_Answer(tcp_trie_return(breg)),
			       makecs(temp_hreg));
#else
	      delay_positively(subgoal, ALN_Answer(tcp_trie_return(breg)),
			       makestring(get_ret_string()));
#endif /* IGNORE_DELAYVAR */
	    }
	  }
	}
	lpcreg = cpreg;
	XSB_Next_Instr();
      } 
      else { 
	tcp_tag(breg) = CHECK_COMPLETE_TAG;      
      }
    } 
  } /* retry_gen_active */
/*--------------------------------------------------------------------------*/
#endif /* LOCAL_EVAL, still !CHAT */
/*--------------------------------------------------------------------------*/

/* 
 * This code is done regardless of whether breg is a leader.  The
 * thought was that as long as we're here, why not schedule answers
 * for this subgoal.  Its purely a heuristic -- perhaps we should test
 * to see whether its inclusion makes any difference 
 */

  { /* schedule answers */
    CPtr tmp_breg;
    if ((tmp_breg = sched_answers(subgoal, breg, leader))){
      breg = tmp_breg;
      Fail1;  
      XSB_Next_Instr();
    }
  }
/*--------------------------------------------------------------------------*/
#endif /* not CHAT */
/*==========================================================================*/

  if (leader) {
    CPtr tmp_breg;
    /* check if fixpoint has been reached, otherwise schedule any
     * unresolved answers */
#ifdef CHAT
    if ((tmp_breg = chat_fixpoint(subgoal, (TChoice)breg)))
#else
    if ((tmp_breg = find_fixpoint(subgoal, breg)))
#endif
    {
      breg = tmp_breg;
      Fail1;
      XSB_Next_Instr();
    }

/*==========================================================================*/
#ifdef LOCAL_EVAL
/*--------------------------------------------------------------------------*/
  {
    VariantSF compl_subg; /* subgoal to be completed */
    CPtr cc_tbreg = orig_breg;
    CPtr ComplStkFrame = cs_ptr;

    /* mark topmost SCC as completed */ 
    breg = orig_breg;

    /* check from leader up to the youngest subgoal */
    while (ComplStkFrame >= openreg) {
      compl_subg = compl_subgoal_ptr(ComplStkFrame);
      /* TLS: I think the following could also be done at early completion
       * (ans-return), though I'm not sure there's an advantage either
       * way
       */
      if (is_completed(compl_subg)) { /* this was early completed */
#ifdef CHAT
	chat_free_compl_susp_chat_areas(compl_subg);
#else
	subg_compl_susp_ptr(compl_subg) = NULL;
#endif
      }
      else { /* if not early completed */
	CPtr nsf;
	if ((nsf = subg_compl_susp_ptr(compl_subg)) != NULL) { 
	  CPtr p, prev_nsf = NULL;
	  /* 
	     check each suspension frame for appropriate action: if
	     their root subgoals are completed these completion
	     suspensions fail, so forget about them; o/w delay them
	     and let simplification take care of the rest
	  
	     TLS: for a local evaluation, I don't see how the root
	     subgoal of a neg-susp can be early completed... is this an
	     artifiact of CHAT non-locality

	   */
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

	    breg = cc_tbreg =
	      chat_restore_compl_susp((chat_init_pheader)nsf, H, EB);
	    subg_compl_susp_ptr(compl_subg) = NULL;
#else
	    CPtr Bmin;

	    set_min(Bmin, breg, bfreg);
	    save_compl_susp_cp(Bmin,cc_tbreg,nsf);
	    subg_compl_susp_ptr(compl_subg) = NULL;
	    breg = cc_tbreg = Bmin;		  
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
      CPtr ComplStkFrame = cs_ptr; 
      while (ComplStkFrame >= openreg) {
	compl_subg = compl_subgoal_ptr(ComplStkFrame);
	mark_as_completed(compl_subg); 
	if (neg_simplif_possible(compl_subg)) {
	  simplify_neg_fails(compl_subg);
	}
	ComplStkFrame = next_compl_frame(ComplStkFrame);
      } /* while */

      /* reclaim all answer lists, but the one for the leader */
      ComplStkFrame = next_compl_frame(cs_ptr); 
      while (ComplStkFrame >= openreg) {
	compl_subg = compl_subgoal_ptr(ComplStkFrame);
	reclaim_incomplete_table_structs(compl_subg);
	ComplStkFrame = next_compl_frame(ComplStkFrame);
      } /* while */

      /* if (leader) subgoal has answers and these haven't been returned yet
       * (when answers are returned, the answer list is reclaimed
       * and set to 0, 1 or 2) */
      if (has_answer_code(subgoal) && (subg_answers(subgoal) > COND_ANSWERS)) {

	CPtr answer_template;
	int template_size, attv_num, tmp;

	reclaim_incomplete_table_structs(subgoal);
	/* so that answers will be returned right after the neg susps
	 * are returned and engine fails over TCP
	 */

	/* schedule return of answers */
	/* TLS: I dont see how the next condition can happen, 
	   breg shd only be set
	   by negation_suspension creation and we are in an if
	   conditional that precludes that */
	   
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
	compl_cons_copy_list(cs_ptr) = 0;
#else
	subg_asf_list_ptr(subgoal) = 0;
#endif
	
	/* reclaim stacks, including leader */
	openreg = prev_compl_frame(cs_ptr);
	reclaim_stacks(orig_breg);
#ifdef CHAT
	tmp = int_val(cell(compl_hreg(cs_ptr)));
	get_var_and_attv_nums(template_size, attv_num, tmp);
	answer_template = compl_hreg(cs_ptr) - template_size;
#else
	ARITY = tcp_arity(orig_breg);
	answer_template = orig_breg + TCP_SIZE + (Cell)ARITY;
	tmp = int_val(cell(answer_template));
	get_var_and_attv_nums(template_size, attv_num, tmp);
	answer_template++;
#endif
	/* Now `answer_template' points to the mth term */
	/* Initialize var_regs[] as the attvs in the call. */
	num_vars_in_var_regs = -1;
	if (attv_num > 0) {
	  CPtr cptr;
	  for (cptr = answer_template + template_size - 1;
	       cptr >= answer_template; cptr--) {
	    if (isattv(cell(cptr)))
	      var_regs[++num_vars_in_var_regs] = (CPtr) cell(cptr);
	  }
	  /* now num_vars_in_var_regs should be attv_num - 1 */
	}

	reg_arrayptr = reg_array - 1;
	for (i = 0; i < template_size; i++) {
	  CPtr cptr = answer_template;
	  pushreg(*cptr);
	  answer_template++;
	}

	lpcreg = (byte *)subg_ans_root_ptr(subgoal);
	/* backtrack to prev tabled subgoal after returning answers */
	breg = tcp_prevbreg(orig_breg); /* orig_???*/ 
	delay_it = 1;
	XSB_Next_Instr();
      } /* if there are answers */   
      else {  /* There are no answers to return
		 ------------------------------ */
	reclaim_incomplete_table_structs(subgoal);

	/* there is nothing else to be done for this SCC */
	cc_tbreg = tcp_prevbreg(orig_breg); /* go to prev SCC */
		
	openreg = prev_compl_frame(cs_ptr); 
	reclaim_stacks(orig_breg);
      } /* no answers to return */
    } /* if(cc_tbreg==orig_breg) */
    breg = cc_tbreg; /* either orig, prev_cp or compl_susp */
  }
/*==========================================================================*/
#else /* NOT LOCAL:  FOR BATCHED SCHEDULING */
/*==========================================================================*/

    batched_compute_wfs(cs_ptr, orig_breg, subgoal);

    /* do all possible stack reclamation */
    if (openreg == prev_compl_frame(cs_ptr)) {
      reclaim_stacks(orig_breg);
      if (breg == orig_breg) {
	breg = tcp_prevbreg(breg);
#ifdef CHAT
	restore_trail_condition_registers(breg);
#endif
      }
    }
/*--------------------------------------------------------------------------*/
#endif /* ifdef LOCAL_EVAL */
/*==========================================================================*/
  }
  else {    /* if not leader */
#ifdef CHAT
    chat_incremental_copy(subgoal);
    /* let's cause some segmentation faults and fix exact completion in CHAT */
    subg_cp_ptr(subgoal) = NULL;	/* preserve invariants */
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
XSB_End_Instr()
/* end of check_complete */
