/*  -*-c-*-  Make sure this file comes up in the C mode of emacs */ 
/* File:      slginsts.i
** Author(s): Swift, Rao, Sagonas, Freire, Cui
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


#define ARITY	op1	/* register Cell */
#define Yn	op2	/* register Cell */
#define LABEL	op3	/* CPtr */

/*----------------------------------------------------------------------*/

/*
 *  Instruction format:
 *    1st word: opcode X X pred_arity
 *    2nd word: program_clause_label
 *    3rd word: pointer to pred's TableInfo record
 */
case tabletry:
case tabletrysingle: {
  /*
   *  Retrieve instruction arguments and test the CP stack for overflow.
   *  (This latter operation should be performed first since the call
   *  lookup operation will be pushing info there WITHOUT adjusting the
   *  top of the CP stack.)  The local PCreg, "lpcreg", is incremented
   *  to point to the instruction to be executed should this one fail.
   */
  byte this_instr = *(lpcreg - 1);
  byte *continuation;
  CallInfoRecord callInfo;
  CPtr producerCPF;
  CallLookupResults lookupResults;

  xwammode = 1;
  CallInfo_Arguments(callInfo) = reg;
  ppad;   CallInfo_CallArity(callInfo) = (Cell)(*lpcreg++);  pad64;
  LABEL = (CPtr)(*(byte **) lpcreg);  ADVANCE_PC;
  CallInfo_TableInfo(callInfo) = (* (TIFptr *) lpcreg);  ADVANCE_PC;

  check_tcpstack_overflow;
  CallInfo_VarVectorLoc(callInfo) = top_of_cpstack;

  if ( this_instr == tabletry ) {
    /* lpcreg was left pointing to the next clause, e.g. tableretry */
    continuation = lpcreg;
    check_glstack_overflow(MAX_ARITY,lpcreg,OVERFLOW_MARGIN) ;
  }
  else
    continuation = (pb) &check_complete_inst;
  /*
   *  Perform a variant call-check/insert operation on the current call.
   *  The variables of this call are pushed on top of the CP stack, along
   *  with the number found (encoded as a Prolog INT) .  This forms the
   *  call's answer template (AT).  A pointer to this size, followed by
   *  the reverse template vector (as described above), is returned in
   *  CallLUR_VarVector(lookupResults).
   */
  if ( IsVariantPredicate(CallInfo_TableInfo(callInfo)) )
    variant_call_search(&callInfo,&lookupResults);
  else
    xsb_exit("Subsumptive predicates not supported yet!");

  if ( IsNULL(CallLUR_Subsumer(lookupResults)) ) {
    /* New Producer
       ------------ */
    create_subgoal_frame( CallLUR_Subsumer(lookupResults),
			  CallLUR_Leaf(lookupResults),
			  CallInfo_TableInfo(callInfo) );
    producerCPF = CallLUR_VarVector(lookupResults);
    save_find_locx(ereg);
    save_registers(producerCPF, CallInfo_CallArity(callInfo), rreg);
    SaveProducerCPF(producerCPF, continuation, CallLUR_Subsumer(lookupResults),
		    CallInfo_CallArity(callInfo));

    push_completion_frame(CallLUR_Subsumer(lookupResults));
    ptcpreg = (CPtr)CallLUR_Subsumer(lookupResults);
    subg_cp_ptr(CallLUR_Subsumer(lookupResults)) = breg = producerCPF;
    delayreg = NULL;
    if (root_address == 0) root_address = breg;
    hbreg = hreg;
    lpcreg = (byte *) LABEL;	/* branch to program clause */
    goto contcase;
  }
  else if ( is_completed(CallLUR_Subsumer(lookupResults)) ) {
    /* Unify Call with Answer Trie
       --------------------------- */
    if (has_answer_code(CallLUR_Subsumer(lookupResults))) {
      int i;
      Cell  CallNumVar;
#ifdef DEBUG_DELAY
      fprintf(stddbg, "++Returning answers from COMPLETED table: ");
      print_subgoal(stddbg, (SGFrame)CallLUR_Subsumer(lookupResults));
      fprintf(stddbg, "\n");
#endif
      CallNumVar = int_val(cell(CallLUR_VarVector(lookupResults)));
      num_vars_in_var_regs = -1;
      reg_arrayptr = reg_array-1;
      for (i = 1; i <= CallNumVar; i++) {
	pushreg(cell(CallLUR_VarVector(lookupResults)+i));
      }
      delay_it = 1;
      lpcreg = (byte *)subg_ans_root_ptr(CallLUR_Subsumer(lookupResults));
      goto contcase;
    }
    else {
      Fail1;
      goto contcase;
    }
  }
  else if ( CallLUR_VariantFound(lookupResults) ) {
    /* Previously Seen Call: was lay_down_consumer label
       ------------------------------------------------- */
    ALNptr  OldRetPtr;
    NODEptr TrieRetPtr;
    CPtr    prev_consumer_cpf;

    adjust_level(subg_compl_stack_ptr(CallLUR_Subsumer(lookupResults)));
    save_find_locx(ereg);
#if (!defined(CHAT))
    efreg = ebreg;
    if (trreg > trfreg) trfreg = trreg;
    if (hfreg < hreg) hfreg = hreg;
    prev_consumer_cpf = subg_asf_list_ptr(CallLUR_Subsumer(lookupResults));
#else
    prev_consumer_cpf = NULL;
#endif
    SaveConsumerCPF( CallLUR_VarVector(lookupResults),
		     CallLUR_Subsumer(lookupResults),
		     prev_consumer_cpf );
#if (!defined(CHAT))
    /* new consumer into front */
    subg_asf_list_ptr(CallLUR_Subsumer(lookupResults)) = bfreg =
#endif
    breg = CallLUR_VarVector(lookupResults);

#ifdef CHAT
    compl_cons_copy_list(subg_compl_stack_ptr(CallLUR_Subsumer(lookupResults))) =
	nlcp_chat_area(breg) = (CPtr) save_a_consumer_copy((SGFrame)CallLUR_Subsumer(lookupResults),
							   CHAT_CONS_AREA);
#endif
    OldRetPtr = subg_answers(CallLUR_Subsumer(lookupResults));
    if (OldRetPtr) {
      Cell CallNumVar;
#ifdef CHAT      /* for the time being let's update consumed answers eagerly */
      nlcp_trie_return((CPtr)(&chat_get_cons_start((chat_init_pheader)nlcp_chat_area(breg)))) =
#endif
	nlcp_trie_return(breg) = OldRetPtr; 
      TrieRetPtr = get_next_trie_solution(&OldRetPtr);
      CallNumVar = *(breg+NLCPSIZE);
      CallNumVar = int_val(CallNumVar); /* # of SF vars is stored tagged */
      op3 = breg + NLCPSIZE + CallNumVar;
      hbreg = hreg;
      load_solution_trie(CallNumVar,op3,TrieRetPtr);
      if (is_conditional_answer(aln_answer_ptr(nlcp_trie_return(breg)))) {
#ifdef DEBUG_DELAY
	fprintf(stddbg,
		"! POSITIVELY DELAYING in lay active (delayreg = %p)\n",
		delayreg);
	fprintf(stddbg, "\n>>>> delay_positively in lay_down_active\n");
	fprintf(stddbg, ">>>> subgoal = ");
	print_subgoal(stddbg, (SGFrame) CallLUR_Subsumer(lookupResults));
	fprintf(stddbg, "\n");
#endif
	{
	  /*
	   * Similar to delay_positively() in retry_active, we also
	   * need to put the substitution factor of the answer,
	   * var_addr[], into a term ret/n and pass it to
	   * delay_positively().
	   */
	  if (num_heap_term_vars == 0) {
	    delay_positively(CallLUR_Subsumer(lookupResults),
			     aln_answer_ptr(nlcp_trie_return(breg)),
			     makestring((char *) ret_psc[0]));
	  }
	  else {
#ifndef IGNORE_DELAYVAR
	    int i;
	    CPtr temp_hreg = hreg;
	    new_heap_functor(hreg, get_ret_psc(num_heap_term_vars));
	    for (i = 0; i < num_heap_term_vars; i++)
	      cell(hreg++) = (Cell) var_addr[i];
	    delay_positively(CallLUR_Subsumer(lookupResults),
			     aln_answer_ptr(nlcp_trie_return(breg)),
			     makecs(temp_hreg));
#else
	    delay_positively(CallLUR_Subsumer(lookupResults),
			     aln_answer_ptr(nlcp_trie_return(breg)),
			     makestring((char *) ret_psc[0]));
#endif /* IGNORE_DELAYVAR */
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
  }
  else {
    /* New Subsumed Call
       ----------------- */
    xsb_exit("New Subsumed Call: Should not yet reach this block");
  }
}

/*----------------------------------------------------------------------*/
/*  Returns answers to consumers.  This can happen either when the	*/
/*  consumer is created and the generator has some answers, or when	*/
/*  the consumer is scheduled through a check-complete instruction.	*/
/*  Note that nlcp-trie-return points to the last answer which has	*/
/*  been consumed.							*/
/*----------------------------------------------------------------------*/

case answer_return:
{
    ALNptr  OldRetPtr;
    NODEptr TrieRetPtr;

    OldRetPtr = aln_next_aln(nlcp_trie_return(breg)); /* get next answer */
    if (OldRetPtr) {
      Cell CallNumVar;
      SGFrame subgoal;

      switch_envs(breg);
      ptcpreg = nlcp_ptcp(breg);
      delayreg = nlcp_pdreg(breg);
      restore_some_wamregs(breg, ereg);
      /* An extra computation in the interest of clarity */
      CallNumVar = *(breg + NLCPSIZE);
      CallNumVar = int_val(CallNumVar); /* # of SF vars is stored tagged */
      op3 = breg + NLCPSIZE + CallNumVar;
      subgoal = (SGFrame)nlcp_subgoal_ptr(breg);
      nlcp_trie_return(breg) = OldRetPtr; /* last answer consumed */
      TrieRetPtr = get_next_trie_solution(&OldRetPtr);
      load_solution_trie(CallNumVar,op3,TrieRetPtr);
      if (is_conditional_answer(aln_answer_ptr(nlcp_trie_return(breg)))) {
	/*
	 * After load_solution_trie(), the substitution factor of the
	 * answer is left in array var_addr[], and its arity is in
	 * num_heap_term_vars.  We have to put it into a term ret/n (on 
	 * the heap) and pass it to delay_positively().
	 */
	if (num_heap_term_vars == 0) {
	  delay_positively(subgoal, aln_answer_ptr(nlcp_trie_return(breg)),
			   makestring((char *) ret_psc[0]));
	}
	else {
#ifndef IGNORE_DELAYVAR
	  int i;
	  CPtr temp_hreg = hreg;
	  new_heap_functor(hreg, get_ret_psc(num_heap_term_vars));
	  for (i = 0; i < num_heap_term_vars; i++) {
	    cell(hreg++) = (Cell) var_addr[i];
	  }
	  delay_positively(subgoal, aln_answer_ptr(nlcp_trie_return(breg)),
			   makecs(temp_hreg));
#else
	  delay_positively(subgoal, aln_answer_ptr(nlcp_trie_return(breg)),
			   makestring((char *) ret_psc[0]));
#endif /* IGNORE_DELAYVAR */
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
}

/*----------------------------------------------------------------------*/
/*  New answers are added to the tail of the answer list of a subgoal	*/
/*  structure.  Upon the derivation of the first answer for a subgoal,	*/
/*  all negation suspensions of the subgoal are abolished.		*/
/*----------------------------------------------------------------------*/

case new_answer_dealloc:
{
    int     xflag;
    CPtr    cs_frame;
    CPtr    VarsInCall;
    Cell    CallNumVar;
    NODEptr TrieRetPtr;
    SGFrame subgoal;
#if (!defined(CHAT))
    CPtr    generator_cp;
#endif

    pad;
    ARITY = (Cell) (*lpcreg++);
    Yn = (Cell) (*lpcreg++);
    pad64;
    subgoal = (SGFrame)cell(ereg-Yn);
#ifdef DEBUG_DELAYVAR
    xsb_dbgmsg(">>>> New answer for %s subgoal: ",
	       (is_completed(subgoal) ? "completed" : "incomplete"));
    print_subgoal(stddbg, subgoal);
    xsb_dbgmsg("\n");
    xsb_dbgmsg(">>>>              has delayreg = %p", delayreg);
#endif
    cs_frame = subg_compl_stack_ptr(subgoal);
    /* if the subgoal has been early completed and its space reclaimed
     * from the stacks, access to its relevant information (e.g. to its
     * substitution factor) in the stacks is not safe, so better not
     * try to add this answer; it is a redundant one anyway...
     */
    if ((subgoal_space_has_been_reclaimed(subgoal,cs_frame)) ||
	(delayreg != NULL && answer_is_junk(delayreg))) {
      Fail1; goto contcase;
    }

#ifdef CHAT
    /* in CHAT, substitution factor is in the heap for generators */
    CallNumVar = int_val(cell(compl_hreg(cs_frame)));
    VarsInCall = compl_hreg(cs_frame)-1;
#else
    /*
     * All the information from the choice point stack, including
     * CallNumVar, VarsInCall, and ARITY's registers, was set in tabletry.
     *
     * ARITY      : arity of the call predicate
     * CallNumVar : number of variables in the call
     * VarsInCall : answer substitution (binding results of the variables
     *              in the call)
     */
    generator_cp = subg_cp_ptr(subgoal);
    CallNumVar = *(generator_cp + TCP_SIZE + (Cell) ARITY);
    CallNumVar = int_val(CallNumVar); /* # of SF vars is stored tagged */
    VarsInCall = generator_cp + TCP_SIZE + (Cell) ARITY + CallNumVar;
#endif

#ifdef DEBUG_DELAYVAR
    xsb_dbgmsg(">>>> ARITY = %d; Yn = %d", (int)ARITY, (int)Yn);
#endif

#ifdef DEBUG_DELAY
    fprintf(stddbg, "\t--> This answer for ");
    print_subgoal(stddbg, subgoal);
    if (delayreg != NULL) {
      fprintf(stddbg, " has delay list = ");
      print_delay_list(stddbg, delayreg);
    } else {
      fprintf(stddbg, " has no delay list");
    }
#endif

#ifdef DEBUG_DELAYVAR
    fprintf(stddbg, "\n>>>> (before variant_trie_search) CallNumVar = %d\n",
	    (int)CallNumVar);
    {
      int i;
      for (i = 0; i < CallNumVar; i++) {
	fprintf(stddbg, ">>>> VarsInCall[%d] = ", i);
	printterm((Cell)(VarsInCall - i), 1, 25);
	fprintf(stddbg, "\n");
      }
    }
#endif

    /*
     * We want to save the substitution factor of the answer in the
     * heap, so we have to change variant_trie_search().
     */
#ifndef IGNORE_DELAYVAR
    ans_var_pos_reg = hreg++;	/* Leave a cell for functor ret/n */
#endif /* IGNORE_DELAYVAR */
    xflag = 0;
    TrieRetPtr = variant_trie_search(CallNumVar,VarsInCall,(CPtr)subgoal,&xflag);

#ifdef DEBUG_DELAYVAR
#ifndef IGNORE_DELAYVAR
    fprintf(stddbg, ">>>> ans_var_pos_reg = ");
    if (isinteger(cell(ans_var_pos_reg)))
      fprintf(stddbg, "\"ret\"\n");
    else 
      fprintf(stddbg, "%s/%d\n", get_name((Psc)(cell(ans_var_pos_reg))),
	      get_arity((Psc)(cell(ans_var_pos_reg))));
#endif /* IGNORE_DELAYVAR */
#endif /* DEBUG_DELAYVAR */

    do_delay_stuff(TrieRetPtr, subgoal, xflag);
#ifndef IGNORE_DELAYVAR
    undo_answer_bindings();
#endif /* IGNORE_DELAYVAR */
    if (xflag) {
      Fail1;  /* do not return repeated answer to generator */
    }
    else { /* go ahead -- look for more answers */
/*----------------------------------------------------------------------*/
#ifdef CHAT
      delayreg = compl_pdreg(cs_frame); /* restore delayreg of parent */
#else
      delayreg = tcp_pdreg(generator_cp);      /* restore delayreg of parent */
#endif
      if (is_conditional_answer(TrieRetPtr)) {	/* positive delay */
#ifndef LOCAL_EVAL
#ifdef DEBUG_DELAYVAR
	fprintf(stddbg, "\n>>>> delay_positively in new_answer_dealloc\n");
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
#ifndef IGNORE_DELAYVAR
	if (isinteger(cell(ans_var_pos_reg))) {
	  delay_positively(subgoal, TrieRetPtr,
			   makestring((char *) ret_psc[0]));
	}
	else 
	  delay_positively(subgoal, TrieRetPtr, makecs(ans_var_pos_reg));
#else
	delay_positively(subgoal, TrieRetPtr, makestring((char *) ret_psc[0]));
#endif /* IGNORE_DELAYVAR */

#endif /* LOCAL_EVAL */
      } else {
	if (CallNumVar == 0) {	/* perform early completion */
	  perform_early_completion(subgoal, generator_cp);
#if (defined(LOCAL_EVAL) && !defined(CHAT))
	  breg = generator_cp;
#endif
	}
      }
/*----------------------------------------------------------------------*/
#ifdef LOCAL_EVAL
      Fail1;	/* and do not return answer to the generator */
#else
#ifdef CHAT
      ptcpreg = compl_ptcp(cs_frame);
#else
      ptcpreg = tcp_ptcp(generator_cp);
#endif
      cpreg = *((byte **)ereg-1);
      ereg = *(CPtr *)ereg;
      lpcreg = cpreg; 
#endif
    }
    goto contcase;
}

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
    tcp_pcreg(breg) = (byte *) &check_complete_inst;
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
/* resume_compl_suspension                                		*/
/*----------------------------------------------------------------------*/

case resume_compl_suspension:
#ifdef DEBUG_DELAYVAR
      fprintf(stddbg, ">>>> resume_compl_suspension is called\n");
#endif
#ifdef CHAT
  {
    chat_init_pheader chat_area;

    switch_envs(breg);
    ptcpreg = csf_ptcp(breg);
    delayreg = csf_pdreg(breg);
    neg_delay = csf_neg_loop(breg);
    restore_some_wamregs(breg, ereg); /* this also restores cpreg */
    chat_area = (chat_init_pheader)csf_chat_area(breg);
    chat_restore_compl_susp_trail(chat_area); /* the chat area is freed here */
    if ((chat_area = (chat_init_pheader)csf_prevcsf(breg)) != NULL) {
      chat_update_compl_susp(chat_area);
    } else {
      breg = csf_prev(breg);  /* forget this CP; simulates Fail1 */
    }
    lpcreg = cpreg;
    goto contcase;
  }
#else
  {
    CPtr csf = cs_compsuspptr(breg);
    /* Switches the environment to a frame of a subgoal that was	*/
    /* suspended on completion, and sets the continuation pointer.	*/
    check_glstack_overflow(MAX_ARITY,lpcreg,OVERFLOW_MARGIN);
    freeze_and_switch_envs(csf, COMPL_SUSP_CP_SIZE);
    ptcpreg = csf_ptcp(csf);
    neg_delay = csf_neg_loop(csf);
    delayreg = csf_pdreg(csf);
    cpreg = csf_cpreg(csf); 
    ereg = csf_ereg(csf);
    ebreg = csf_ebreg(csf);
    hbreg = csf_hreg(csf);
    save_find_locx(ereg);
    hbreg = hreg;
    if (csf_prevcsf(csf) != NULL) {
      cs_compsuspptr(breg) = csf_prevcsf(csf);
    } else {
      breg = cs_prevbreg(breg);
    }
    lpcreg = cpreg;
    goto contcase;
  }
#endif

/*----------------------------------------------------------------------*/
