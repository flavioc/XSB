/* File:      slginsts.i
** Author(s): Swift, Rao, Sagonas, Juliana Freire, Baoqiu Cui
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
#include "debugs/debug_kostis.h"


#define ARITY	op1
#define Yn	op2
#define LABEL	op3
#ifdef CHAT
#define COMPL_STK_FRAME	xtemp3
#else
#define PREV_CONSUMER	xtemp3
#define GENERATOR_CP	xtemp3
#endif
#define COMPL_SUSP_ENV	xtemp3
#define VarsInCall	xtemp5
#define SUBGOAL		xcurcall

/*----------------------------------------------------------------------*/

case tabletry:		/* cur_label arity label xcurcall	*/
    xwammode = 1; ppad;
    ARITY = (Cell)(*lpcreg++);	/* op1 */
    pad64;
    LABEL = (CPtr)(*(byte **) lpcreg);	/* op3 */
    ADVANCE_PC;
    xcurcall = (* (CPtr *) lpcreg);  /* pointer to cur call-tabinfoptr */
    ADVANCE_PC;  /* lpcreg points to next instruction, e.g. tabretry */
    /*
     * contents of xcurcall point to the subgoal structure
     * this will create the variables on top of stack (on VarPosReg)
     * and initialize the # of vars in the subgoal (also on the stack)
     * Hence, stack reallocation, which means CP Stack relocation,
     * must occur first, or these variables will be left behind.
     */
    VarPosReg = top_of_cpstack;
    check_tcpstack_overflow(VarPosReg);
	check_glstack_overflow(MAX_ARITY,lpcreg,OVERFLOW_MARGIN) ;
    if (variant_call_search(ARITY, reg, &xcurcall)) { /* variant call exists */
    /*
     * After variant_call_search, VarPosReg sits on top of the
     * substitution factor of the call and points to a cell which
     * containins the number of variables in the call.  Under this cell
     * are all the variables (still free) --- the SUBSF.
     */
      xcurcall = (CPtr) *xcurcall;	/* point xcurcall to the call_struct */
      if (is_completed(xcurcall)) {
	if (has_answer_code(xcurcall)) goto return_table_code;
	else { Fail1; goto contcase; }
      }
      else goto lay_down_consumer;
    }
    else {  /* there is no variant-subgoal */
	    /* put subgoal in stack and init structure components */
	    /* makes call structure point back to CallTableInfo-original call */
	    /* compl_stack_ptr = openreg-COMPLFRAMESIZE+1 (all others=0) */
      /*
       * Here Paren is the leaf node in the subgoal trie for this call
       * (result from variant_call_search()).
       */
      create_subgoal_frame(*xcurcall, Paren);
      xcurcall = (CPtr) *xcurcall;
      save_find_locx(ereg);

      save_registers(VarPosReg, (int)ARITY, i, rreg);
      /*
       * Set a choice point generating node-the solution node is responsible
       * for going through the different choices and for starting the check
       * complete when it runs out of alternatives
       * the variables in the call are stored right before this CP
       * uses: bfreg,efreg,trfreg,hfreg,ereg,cpreg,trreg,hreg,ebreg,tbreg,
       * prev=breg,lpcreg
       */
#ifdef CHAT
      save_generator_choicepoint(VarPosReg, ereg, xcurcall, breg);
#else
      save_generator_choicepoint(VarPosReg, ereg, xcurcall, breg, ARITY);
#endif
      push_completion_frame((SGFrame)xcurcall);
      ptcpreg = xcurcall;
      subg_cp_ptr(xcurcall) = breg = VarPosReg;
      delayreg = NULL;
      if (root_address == 0) root_address = breg;
      hbreg = hreg;
      lpcreg = (byte *) LABEL;	/* branch to program clause */
      goto contcase;
    } 

/*----------------------------------------------------------------------*/
/*  Returns answers to consumers.  This can happen either when the	*/
/*  consumer is created and the generator has some answers, or when	*/
/*  the consumer is scheduled through a check-complete instruction.	*/
/*  Note that nlcp-trie-return points to the last answer which has	*/
/*  been consumed.							*/
/*----------------------------------------------------------------------*/

case answer_return:
    OldRetPtr = aln_next_aln(nlcp_trie_return(breg)); /* get next answer */
    if (OldRetPtr){

      switch_envs(breg);
      ptcpreg = nlcp_ptcp(breg);
      delayreg = nlcp_pdreg(breg);
      restore_some_wamregs(breg, ereg);
      /* An extra computation in the interest of clarity */
      CallNumVar = *(breg + NLCPSIZE);
      op3 = breg + NLCPSIZE + CallNumVar;
      nlcp_trie_return(breg) = OldRetPtr; /* last answer consumed */
      TrieRetPtr = get_next_trie_solution(&OldRetPtr);
      load_solution_trie(CallNumVar,op3,TrieRetPtr);
      if (is_conditional_answer(aln_answer_ptr(nlcp_trie_return(breg)))) {
	int i;
	CPtr temp_hreg;
	/*
	 * After load_solution_trie(), the substitution factor of the
	 * answer is left in array var_addr[], and its arity is in
	 * num_heap_term_vars.  We have to put it into a term ret/n (on 
	 * the heap) and pass it to delay_positively().
	 */
	if (num_heap_term_vars == 0) {
	  delay_positively(SUBGOAL, aln_answer_ptr(nlcp_trie_return(breg)),
			   makestring((char *) ret_psc[0]));
	}
	else {
	  temp_hreg = hreg;
	  new_heap_functor(hreg, get_ret_psc(num_heap_term_vars));
	  for (i = 0; i < num_heap_term_vars; i++) {
	    cell(hreg++) = (Cell) var_addr[i];
	  }
	  SUBGOAL = nlcp_subgoal_ptr(breg);
	  delay_positively(SUBGOAL, aln_answer_ptr(nlcp_trie_return(breg)),
			   makecs(temp_hreg));
	}
      }
      lpcreg = cpreg;
    } else {
#ifdef CHAT	/* update consumed answers in CHAT-area before failing out */
      nlcp_trie_return((CPtr)(&chat_get_cons_start((chat_init_pheader)nlcp_chat_area(breg)))) =
	nlcp_trie_return(breg);	/* last answer consumed */
#endif
      breg = nlcp_prevbreg(breg); /* in semi-naive this execs next active */
#ifdef CHAT
      hreg = cp_hreg(breg);
      restore_trail_condition_registers(breg);
#else
      restore_trail_condition_registers(breg);
      if (hbreg >= hfreg) hreg = hbreg; else hreg = hfreg;
#endif
      Fail1;
    }
    goto contcase;

/*----------------------------------------------------------------------*/
/*  New answers are added to the tail of the answer list of a subgoal	*/
/*  structure.  Upon the derivation of the first answer for a subgoal,	*/
/*  all negation suspensions of the subgoal are abolished.		*/
/*----------------------------------------------------------------------*/

case new_answer_dealloc:
#ifdef DEBUG_DELAYVAR
    fprintf(stderr, ">>>> (starting new_answer_dealloc) delayreg =%p\n",
	    delayreg);
#endif
    if (delayreg != NULL && answer_is_junk(delayreg)) {
      Fail1; goto contcase;
    }

    pad;
    ARITY = (Cell) (*lpcreg++);
    Yn = (Cell) (*lpcreg++);
    pad64;
#ifdef CHAT
    SUBGOAL = (CPtr)cell(ereg-Yn);
    COMPL_STK_FRAME = subg_compl_stack_ptr(SUBGOAL);
    /* substitution factor is now in the heap for generators */
    CallNumVar = int_val(cell(compl_hreg(COMPL_STK_FRAME)));
    VarsInCall = compl_hreg(COMPL_STK_FRAME)-1;
#else
    op3 = (CPtr) *(ereg-Yn);
    /*
     * All the information from the choice point stack, including
     * CallNumVar, VarsInCall, and ARITY's registers, was set in
     * tabletry.
     *
     * ARITY      : arity of the call predicate
     * CallNumVar : number of variables in the call
     * VarsInCall : answer substitution (binding results of the variables
     *              in the call)
     * breg       : first permanent var (saved in local stack as an offset) 
     */
    GENERATOR_CP = (CPtr)(tcpstack.high - int_val(op3));
    SUBGOAL = tcp_subgoal_ptr(GENERATOR_CP);
    CallNumVar = *(GENERATOR_CP + TCP_SIZE + (Cell) ARITY);
    VarsInCall = GENERATOR_CP + TCP_SIZE + (Cell) ARITY + CallNumVar;
#endif

#ifdef DEBUG_DELAYVAR
    fprintf(stderr, ">>>> ARITY = %d; Yn = %d\n", (int)ARITY, (int)Yn);
#endif

#ifdef DEBUG_DELAY
    fprintf(stderr, "\t--> This answer for ");
    print_subgoal(stderr, (SGFrame)SUBGOAL);
    if (delayreg != NULL) {
      fprintf(stderr, " has delay list = ");
      print_delay_list(stderr, delayreg);
    } else {
      fprintf(stderr, " has no delay list");
    }
#endif

    xflag = 0;

#ifdef DEBUG_DELAYVAR
    fprintf(stderr, "\n>>>> (before variant_trie_search) CallNumVar = %d\n",
	    (int)CallNumVar);
    {
      int i;
      for (i = 0; i < CallNumVar; i++) {
	fprintf(stderr, ">>>> VarsInCall[%d] = ", i);
	printterm((Cell)(VarsInCall - i), 1, 25);
	fprintf(stderr, "\n");
      }
    }
#endif

    /*
     * We want to save the substitution factor of the answer in the
     * heap, so we have to change variant_trie_search().
     */
    ans_var_pos_reg = hreg++;	/* Leave a cell for functor ret/n */
    TrieRetPtr = variant_trie_search(CallNumVar,VarsInCall,SUBGOAL,&xflag);

#ifdef DEBUG_DELAYVAR
    fprintf(stderr, ">>>> ans_var_pos_reg = ");
    if (isinteger(cell(ans_var_pos_reg)))
      fprintf(stderr, "\"ret\"\n");
    else 
      fprintf(stderr, "%s/%d\n", get_name((Psc)(cell(ans_var_pos_reg))),
	      get_arity((Psc)(cell(ans_var_pos_reg))));
#endif

    do_delay_stuff(TrieRetPtr, (SGFrame)SUBGOAL, xflag);
    undo_answer_bindings();

    if (xflag) {
      Fail1;  /* do not return repeated answer to generator */
    }
    else { /* go ahead -- look for more answers */
/*----------------------------------------------------------------------*/
#ifdef CHAT
      delayreg = compl_pdreg(COMPL_STK_FRAME); /* restore delayreg of parent */
#else
      delayreg = tcp_pdreg(GENERATOR_CP);      /* restore delayreg of parent */
#endif
      if (is_conditional_answer(TrieRetPtr)) {	/* positive delay */
#ifndef LOCAL_EVAL
#ifdef DEBUG_DELAYVAR
	fprintf(stderr, "\n>>>> delay_positively in new_answer_dealloc\n");
#endif
	/*
	 * The new answer for this call is a conditional one, so add it
	 * into the delay list for the parent predicate.  Notice that
	 * delayreg has already been restored to the delayreg of parent.
	 *
	 * This is the new version of delay_positively().  Here,
	 * ans_var_pos_reg is passed from variant_trie_search().  It is a
	 * pointer to the heap where the substitution factor of the
	 * answer was saved as a term ret/n (in variant_trie_search()).
	 */
	if (isinteger(cell(ans_var_pos_reg))) {
	  delay_positively(SUBGOAL, TrieRetPtr,
			   makestring((char *) ret_psc[0]));
	}
	else 
	  delay_positively(SUBGOAL, TrieRetPtr, makecs(ans_var_pos_reg));
#endif
      } else {
	if (CallNumVar == 0) {	/* perform early completion */
	  perform_early_completion(SUBGOAL, GENERATOR_CP);
#if (defined(LOCAL_EVAL) && !defined(CHAT))
	  breg = GENERATOR_CP;
#endif
	}
      }
/*----------------------------------------------------------------------*/
#ifdef LOCAL_EVAL
      Fail1;	/* and do not return answer to the generator */
#else
#ifdef CHAT
      ptcpreg = compl_ptcp(COMPL_STK_FRAME);
#else
      ptcpreg = tcp_ptcp(GENERATOR_CP);
#endif
      cpreg = *((byte **)ereg-1);
      ereg = *(CPtr *)ereg;
      lpcreg = cpreg; 
#endif
    }
    goto contcase;

/*----------------------------------------------------------------------*/

 case tableretry: /* PPA-L */
    ppad; op1byte;
    pad64;
    tcp_pcreg(breg) = lpcreg+sizeof(Cell);
    lpcreg = *(pb *)lpcreg;
    restore_type = 0;
    goto table_restore_sub;

/*----------------------------------------------------------------------*/
/* resets breg, sets up a completion instruction in the generator choice point
 * through the next clause cell (whose contents are executed upon failure)
 */
/*----------------------------------------------------------------------*/

case tabletrust:
    ppad; op1byte;
    pad64;
    tcp_pcreg(breg) = (byte *) & check_complete_inst;
    lpcreg = *(pb *)lpcreg;
#if (defined(LOCAL_EVAL) || defined(CHAT))
    /* trail cond. registers should not be restored here for Local */
    restore_type = 0;
#else
    restore_type = 1;
#endif
    goto table_restore_sub;

/*----------------------------------------------------------------------*/

#include "complete.i"

/*----------------------------------------------------------------------*/

case tabletrysingle:
    xwammode = 1;
    ppad;
    ARITY = (Cell)(*lpcreg++);
    pad64;
    LABEL = (CPtr)(*(byte **) lpcreg);  ADVANCE_PC;
    xcurcall = * (CPtr *) lpcreg; ADVANCE_PC;
    VarPosReg = top_of_cpstack;
    check_tcpstack_overflow(VarPosReg);
    if (variant_call_search(ARITY, reg, &xcurcall)) { /* variant call exists */
      xcurcall = (CPtr) *xcurcall;	/* point xcurcall to the call_struct */
      if (is_completed(xcurcall)) {
	if (has_answer_code(xcurcall)) goto return_table_code;
	else { Fail1; goto contcase; }
      }
      else goto lay_down_consumer;
    }
    else {  /* create generator choice point */
      create_subgoal_frame(*xcurcall, Paren);
      xcurcall = (CPtr) *xcurcall;
      save_find_locx(ereg);

      /* now lay down single clause choicepoint */
      save_registers(VarPosReg, (int)ARITY, i, rreg);
#ifdef CHAT
      save_singleclause_choicepoint(VarPosReg, ereg, xcurcall, breg);
#else
      save_singleclause_choicepoint(VarPosReg, ereg, xcurcall, breg, ARITY);
#endif
      push_completion_frame((SGFrame) xcurcall);
      ptcpreg = xcurcall;
      subg_cp_ptr(xcurcall) = breg = VarPosReg;
      delayreg = NULL;
      if (root_address == 0) root_address = breg;
      hbreg = hreg;
      lpcreg = (byte *) LABEL; /* go to clause ep */
      goto contcase;
    }

/*----------------------------------------------------------------------*/
/* resume_compl_suspension                                		*/
/*----------------------------------------------------------------------*/

case resume_compl_suspension:
#ifdef DEBUG_DELAYVAR
      fprintf(stderr, ">>>> resume_compl_suspension is called\n");
#endif
#ifdef CHAT
{
    chat_init_pheader chat_area;

    switch_envs(breg);
    ptcpreg = csf_ptcp(breg);
    delayreg = csf_pdreg(breg);
    neg_delay = csf_neg_loop(breg);
    /* not really sure whether the following two lines are needed */
 /*   ebreg = csf_ebreg(breg);
    hbreg = csf_hreg(breg); */
    restore_some_wamregs(breg, ereg); /* this also restores cpreg */
    chat_area = (chat_init_pheader)csf_chat_area(breg);
    chat_restore_compl_susp_trail(chat_area); /* the chat area is freed here */
    if ((chat_area = (chat_init_pheader)csf_prevcsf(breg)) != NULL) {
      chat_update_compl_susp(chat_area);
#ifdef CHAT_DEBUG
      fprintf(stderr, "INSTALLED THE NEXT COMPL_SUSP here...\n");
#endif
    } else {
      breg = csf_prev(breg);  /* forget this CP; simulates Fail1 */
    }
    lpcreg = cpreg;
    goto contcase;
  }
#else
/*	Switches the environments to a frame of a subgoal that was	*/
/*	suspended on completion, and sets the continuation pointer.	*/
    check_glstack_overflow(MAX_ARITY,lpcreg,OVERFLOW_MARGIN) ;
    COMPL_SUSP_ENV = cs_compsuspptr(breg);
    freeze_and_switch_envs(COMPL_SUSP_ENV, COMPL_SUSP_CP_SIZE);
    ptcpreg = csf_ptcp(COMPL_SUSP_ENV);
    neg_delay = csf_neg_loop(COMPL_SUSP_ENV);
    delayreg = csf_pdreg(COMPL_SUSP_ENV);
    cpreg = csf_cpreg(COMPL_SUSP_ENV); 
    ereg = csf_ereg(COMPL_SUSP_ENV);
    ebreg = csf_ebreg(COMPL_SUSP_ENV);
    hbreg = csf_hreg(COMPL_SUSP_ENV);
    save_find_locx(ereg);
    hbreg = hreg;
    if (csf_prevcsf(COMPL_SUSP_ENV) != NULL) {
      cs_compsuspptr(breg) = csf_prevcsf(COMPL_SUSP_ENV);
    } else {
      breg = cs_prevbreg(breg);
    }
    lpcreg = cpreg;
    goto contcase;
#endif

/*----------------------------------------------------------------------*/

return_table_code:
#ifdef DEBUG_DELAY
    xsb_warn("Returning answers from a COMPLETED table...");
#endif
    CallNumVar = *(VarPosReg);
    num_vars_in_var_regs = -1;
    reg_arrayptr = reg_array-1;
    for (i = 1; i <= CallNumVar; i++) {
       pushreg(cell(VarPosReg+i));
    }
    delay_it = 1;
    lpcreg = (byte *) subg_ans_root_ptr(xcurcall);
    goto contcase;

/*----------------------------------------------------------------------*/

lay_down_consumer:
    adjust_level(subg_compl_stack_ptr(xcurcall));
#if (!defined(CHAT))
    PREV_CONSUMER = subg_asf_list_ptr(xcurcall);
#endif
    save_find_locx(ereg);
#if (!defined(CHAT))
    efreg = ebreg;
    if (trreg > trfreg) trfreg = trreg;
    if (hfreg < hreg) hfreg = hreg;
    save_consumer_choicepoint(VarPosReg,ereg,xcurcall,PREV_CONSUMER,breg);
#else
    save_consumer_choicepoint(VarPosReg,ereg,xcurcall,breg);
#endif
#if (!defined(CHAT))
    subg_asf_list_ptr(xcurcall) = /* new consumer into front */
    bfreg =
#endif
    breg = VarPosReg;
#ifdef CHAT
    compl_cons_copy_list(subg_compl_stack_ptr(xcurcall)) =
	nlcp_chat_area(breg) = (CPtr) save_a_consumer_copy((SGFrame)xcurcall,
							   CHAT_CONS_AREA);
#endif
    OldRetPtr = subg_answers(xcurcall);
    if (OldRetPtr) {
/* CHAT had this which is correct -- but does not have variables in delay lists
      if (is_conditional_answer(aln_answer_ptr(OldRetPtr))) {
	delay_positively(xcurcall, aln_answer_ptr(OldRetPtr))
      }
 */
#ifdef CHAT      /* for the time being let's update consumed answers eagerly */
      nlcp_trie_return((CPtr)(&chat_get_cons_start((chat_init_pheader)nlcp_chat_area(breg)))) =
#endif
	nlcp_trie_return(breg) = OldRetPtr; 
      TrieRetPtr = get_next_trie_solution(&OldRetPtr);
      CallNumVar = *(breg+NLCPSIZE);
      op3 = breg + NLCPSIZE + CallNumVar;
      hbreg = hreg;
      load_solution_trie(CallNumVar,op3,TrieRetPtr);
      if (is_conditional_answer(aln_answer_ptr(nlcp_trie_return(breg)))) {
#ifdef DEBUG_DELAY
	fprintf(stderr,
		"! POSITIVELY DELAYING in lay active (delayreg = %p)\n",
		delayreg);
	fprintf(stderr, "\n>>>> delay_positively in lay_down_active\n");
	fprintf(stderr, ">>>> subgoal = ");
	print_subgoal(stderr, (SGFrame) xcurcall); fprintf(stderr, "\n");
#endif
	{
	  int i;
	  CPtr temp_hreg;
	  /*
	   * Similar to delay_positively() in retry_active, we also
	   * need to put the substitution factor of the answer,
	   * var_addr[], into a term ret/n and pass it to
	   * delay_positively().
	   */
	  if (num_heap_term_vars == 0) {
	    delay_positively(xcurcall, aln_answer_ptr(nlcp_trie_return(breg)),
			     makestring((char *) ret_psc[0]));
	  }
	  else {
	    temp_hreg = hreg;
	    new_heap_functor(hreg, get_ret_psc(num_heap_term_vars));
	    for (i = 0; i < num_heap_term_vars; i++)
	      cell(hreg++) = (Cell) var_addr[i];
	    delay_positively(xcurcall, aln_answer_ptr(nlcp_trie_return(breg)),
			     makecs(temp_hreg));
	  }
	}
      }
      lpcreg = cpreg;
    } else {
      breg = nlcp_prevbreg(breg);
#ifdef CHAT
      hreg = cp_hreg(breg);
      ereg = cp_ereg(breg);
#endif
      Fail1;
    }
    goto contcase;

/*----------------------------------------------------------------------*/

