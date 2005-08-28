/* File:      slginsts_xsb_i.h
** Author(s): Swift, Rao, Sagonas, Freire, Cui, Johnson
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

/*-------------------------------------------------------------------------*/

/*
 *  Organization of Tabling Choice Points:
 *
 *             +-------------+
 *             |             |   LOW MEMORY
 *             |    Trail    |
 *             |             |
 *             |      |      |
 *             |      V      |
 *             |             |
 *             |             |
 *             |      ^      |
 *             |      |      |
 *             |             |
 *             |  CP Stack   |
 *             |             |
 *             |             |
 *             |=============|
 *             | Rest of CPF |--- Different for Generator and Consumer
 *             |-------------|_
 *             |   INT: m    | \
 *             |   Term-m    |  |
 *             |      .      |  |- Answer Template
 *             |      .      |  |
 *             |      .      |  |
 *             |   Term-1    |_/
 *             |=============|
 *             |      .      |
 *             |      .      |
 *             |      .      |    HIGH MEMORY
 *             +-------------+
 *
 *
 *  Answer Templates are stored in the Heap:
 *
 *             +-------------+
 *             |      .      |   LOW MEMORY
 *             |      .      |
 *             |      .      |
 *             |-------------|_
 *             |   Term-m    | \
 *             |      .      |  |
 *             |      .      |  |- Answer Template
 *             |      .      |  |
 *             |   Term-1    |  |
 *             |   INT: m    |_/
 *             |-------------|
 *             |             |
 *             |    Heap     |
 *             |             |
 *             |      |      |
 *             |      V      |
 *             |             |
 *             |             |
 *             |      ^      |
 *             |      |      |
 *             |             |
 *             |    Local    |
 *             |             |    HIGH MEMORY
 *             +-------------+
 */

/*-------------------------------------------------------------------------*/

/*
 *  Instruction format:
 *    1st word: opcode X X pred_arity
 *    2nd word: pred_first_clause_label
 *    3rd word: preds_TableInfo_record
 */

XSB_Start_Instr_Chained(tabletry,_tabletry)
XSB_Start_Instr(tabletrysingle,_tabletrysingle) 
  DefOps13
  /*
   *  Retrieve instruction arguments and test the system stacks for
   *  overflow.  The local PCreg, "lpcreg", is incremented to point to
   *  the instruction to be executed should this one fail.
   */
  byte this_instr = *lpcreg;
  byte *continuation;
  TabledCallInfo callInfo;
  CallLookupResults lookupResults;
  VariantSF producer_sf, consumer_sf;
  CPtr answer_template;
  int template_size, attv_num, tmp;
  TIFptr tip;
#ifdef SHARED_COMPL_TABLES
  byte * inst_addr = lpcreg;
  int table_tid ;
  int grabbed = FALSE;
  th_context * waiting_for_thread;
#endif
#ifdef MULTI_THREAD
  CPtr tbreg;
#ifdef SLG_GC
  CPtr old_cptop;
#endif
#endif

  xwammode = 1;
  CallInfo_Arguments(callInfo) = reg + 1;
  CallInfo_CallArity(callInfo) = get_xxa; 
  LABEL = (CPtr)((byte *) get_xxxl);  
  Op1(get_xxxxl);
  tip =  (TIFptr) get_xxxxl;
#ifdef MULTI_THREAD
  /* get right TIF, if thread_private */
  if (TIF_EvalMethod(tip) == 3 /*DISPATCH_BLOCK*/) {
    struct TDispBlk_t *tdispblk;
    tdispblk = (struct TDispBlk_t *)(((CPtr)tip)-2);
    if (th->tid > tdispblk->MaxThread) xsb_abort("Table Dispatch block too small");
    tip = (TIFptr)((&(tdispblk->Thread0))[th->tid]);
    if (!tip) {
      New_TIF(tip,tdispblk->psc_ptr);
      (&(tdispblk->Thread0))[th->tid] = tip;
    }
  }
#endif
  CallInfo_TableInfo(callInfo) = tip;
  ADVANCE_PC(size_xxxXX);

  check_tcpstack_overflow;
  CallInfo_VarVectorLoc(callInfo) = top_of_cpstack;

  if ( this_instr == tabletry ) {
    /* lpcreg was left pointing to the next clause, e.g. tableretry */
    continuation = lpcreg;
  }
  else 
    continuation = (pb) &check_complete_inst;

  check_glstack_overflow(CallInfo_CallArity(callInfo),lpcreg,OVERFLOW_MARGIN);

#ifdef SHARED_COMPL_TABLES
  pthread_mutex_lock( &completing_mut );
#endif
  /*
   *  Perform a call-check/insert operation on the current call.  The
   *  subterms of this call which form the answer template are
   *  computed and pushed on top of the CP stack, along with its size
   *  (encoded as a Prolog INT) .  A pointer to this size, followed by
   *  the reverse template vector (as depicted above), is returned in
   *  CallLUR_VarVector(lookupResults).  Always (now) the answer
   *  template is pushed on the Heap rather than the CPS.  In that
   *  case, (heap - 1) points to the A.T. and
   *  CallLUR_VarVector(lookupResults) has the same value as
   *  CallInfo_VarVectorLoc(callInfo).
   */
  table_call_search(CTXTc &callInfo,&lookupResults);

  producer_sf = CallLUR_Subsumer(lookupResults);
  answer_template = CallLUR_VarVector(lookupResults);

#ifdef SHARED_COMPL_TABLES
/* This allows sharing of completed tables.
   If the subgoal frame is not new, and the table is being generated by
   a different thread, wait for it to complete.
 */
  if ( !IsNULL(producer_sf) ) {
     while( !is_completed(producer_sf))
     {  
        /* if is leader and subgoal is marked to be computed by leader */
        if( th->deadlock_brk_leader && subg_grabbed(producer_sf) )
        {       subg_tid(producer_sf) = th->tid ;
                subg_grabbed(producer_sf) = FALSE ;
                grabbed = TRUE ;
                break ;
        }
	table_tid = subg_tid(producer_sf) ;
        /* if the thread owns the table, proceed */
	if (table_tid == th->tid) 
		break ;
	waiting_for_thread = find_context(table_tid) ;
	if( would_deadlock( waiting_for_thread, th ) )
        {       /* code for leader */
                reset_other_threads( th, waiting_for_thread, producer_sf );
                th->deadlock_brk_leader = TRUE ;
                pthread_cond_broadcast(&completing_cond) ;
                continue ;
        }
        th->waiting_for_subgoal = producer_sf ;
        th->waiting_for_thread = waiting_for_thread ;
	pthread_mutex_unlock(&completing_mut);
	pthread_cond_wait(&completing_cond,&completing_mut) ;
        if( th->reset_thread )
        {       th->reset_thread = FALSE ;
                pthread_mutex_unlock(&completing_mut) ;
                /* restart the tabletry instruction */
                lpcreg = pcreg ;
                XSB_Next_Instr() ;
        }
     }
     th->waiting_for_thread = NULL ;
     th->waiting_for_subgoal = NULL ;
     pthread_mutex_unlock(&completing_mut);
  } 

  if ( IsNULL(producer_sf) || grabbed ) {

    /* New Producer
       ------------ */
    CPtr producer_cpf;
    if( !grabbed )
    {
    	NewProducerSF(producer_sf, CallLUR_Leaf(lookupResults),
		      CallInfo_TableInfo(callInfo));
    	subg_tid(producer_sf) = th->tid;
    	subg_grabbed(producer_sf) = 0;
    	pthread_mutex_unlock( &completing_mut );
    }
    else
    {	subg_compl_stack_ptr(producer_sf) = openreg - COMPLFRAMESIZE;
    }
#else
#ifdef CONC_COMPL
    pthread_mutex_lock( &completing_mut ) ;
#endif
  if ( IsNULL(producer_sf) ) {
#ifdef CONC_COMPL
    pthread_mutex_unlock( &completing_mut ) ;
#endif

    /* New Producer
       ------------ */
    CPtr producer_cpf;
    NewProducerSF(producer_sf, CallLUR_Leaf(lookupResults),
		  CallInfo_TableInfo(callInfo));
#endif
#ifdef CONC_COMPL
    subg_tid(producer_sf) = th->tid;
    subg_tag(producer_sf) = INCOMP_ANSWERS;
#endif
    producer_cpf = answer_template;
    save_find_locx(ereg);
    save_registers(producer_cpf, CallInfo_CallArity(callInfo), rreg);
    SaveProducerCPF(producer_cpf, continuation, producer_sf,
		    CallInfo_CallArity(callInfo), (hreg - 1));
#ifdef SHARED_COMPL_TABLES
    tcp_reset_pcreg(producer_cpf) = inst_addr ;
#endif
#ifdef SLG_GC
    tcp_prevtop(producer_cpf) = answer_template; 
    /* answer_template points to the previous top, since the A.T. proper
       is now always copied to the heap */
#endif
    push_completion_frame(producer_sf);
    ptcpreg = (CPtr)producer_sf;
    subg_cp_ptr(producer_sf) = breg = producer_cpf;
    delayreg = NULL;
    if (root_address == 0)
      root_address = breg;
    hbreg = hreg;
    lpcreg = (byte *) LABEL;	/* branch to program clause */
    XSB_Next_Instr();
  }

  else if ( is_completed(producer_sf) ) {

#ifdef CONC_COMPL
    pthread_mutex_unlock( &completing_mut ) ;
#endif
    /* Unify Call with Answer Trie
       --------------------------- */
    if (has_answer_code(producer_sf)) {
      int i;
      xsb_dbgmsg((LOG_DELAY, "++Returning answers from COMPLETED table: "));
      dbg_print_subgoal(LOG_DELAY, stddbg, producer_sf);
      xsb_dbgmsg((LOG_DELAY, "\n"));
      answer_template = hreg - 1; 

      tmp = int_val(cell(answer_template));
      get_var_and_attv_nums(template_size, attv_num, tmp);
      num_vars_in_var_regs = -1;

      /* Initialize var_regs[] as the attvs in the call. */
      if (attv_num > 0) {
	CPtr cptr;
	for (cptr = answer_template - 1;
	     cptr >= answer_template + template_size; cptr++) {
	  if (isattv(cell(cptr)))
	    var_regs[++num_vars_in_var_regs] = (CPtr) cell(cptr);
	}
	/* now num_vars_in_var_regs should be attv_num - 1 */
      }

      reg_arrayptr = reg_array-1;
      for (i = 0; i < template_size; i++) {
	pushreg(cell(answer_template-template_size+i));
      }
      delay_it = 1;
      lpcreg = (byte *)subg_ans_root_ptr(producer_sf);
#ifdef MULTI_THREAD
/* save choice point for trie_unlock instruction */
      save_find_locx(ereg);
      tbreg = top_of_cpstack;
#ifdef SLG_GC
      old_cptop = tbreg;
#endif
      save_choicepoint(tbreg,ereg,(byte *)&trie_fail_unlock_inst,breg);
#ifdef SLG_GC
      cp_prevtop(tbreg) = old_cptop;
#endif
      breg = tbreg;
      hbreg = hreg;
#endif
      XSB_Next_Instr();
    }
    else {
      Fail1;
      XSB_Next_Instr();
    }
  }

  else if ( CallLUR_VariantFound(lookupResults) )

    /* Previously Seen Subsumed Call
       ----------------------------- */
    consumer_sf = CallTrieLeaf_GetSF(CallLUR_Leaf(lookupResults));

  else

    /* New Properly Subsumed Call
       -------------------------- */
    NewSubConsSF( consumer_sf, CallLUR_Leaf(lookupResults),
		   CallInfo_TableInfo(callInfo), producer_sf );

  /*
   * The call, represented by "consumer_sf", will consume from an
   * incomplete producer, represented by "producer_sf".
   */
  {
    CPtr consumer_cpf;
#ifdef SLG_GC
    CPtr prev_cptop;
#endif
    ALNptr answer_continuation;
    BTNptr first_answer;

    /* Create Consumer Choice Point
       ---------------------------- */
#ifdef CONC_COMPL
    if( subg_tid(producer_sf) == th->tid )
#endif
    adjust_level(subg_compl_stack_ptr(producer_sf));
    save_find_locx(ereg);

    consumer_cpf = answer_template;
#ifdef SLG_GC
    prev_cptop = consumer_cpf;
#endif

    answer_template = hreg-1;

    efreg = ebreg;
    if (trreg > trfreg) trfreg = trreg;
    if (hfreg < hreg) hfreg = hreg;
    SaveConsumerCPF( consumer_cpf, consumer_sf,
		     subg_asf_list_ptr(producer_sf), 
		     answer_template);
#ifdef SLG_GC
    nlcp_prevtop(consumer_cpf) = prev_cptop;
#endif
    subg_asf_list_ptr(producer_sf) = breg = bfreg = consumer_cpf;
#ifdef CONC_COMPL
    nlcp_tid(consumer_cpf) = makeint(th->tid) ;
    for(;;)
    {
#endif

    /* Consume First Answer or Suspend
       ------------------------------- */
    table_pending_answer( subg_ans_list_ptr(consumer_sf),
			  answer_continuation,
			  first_answer,
			  (SubConsSF)consumer_sf,
			  (SubProdSF)producer_sf,
			  answer_template,
			  TPA_NoOp,
			  TPA_NoOp );

    if ( IsNonNULL(answer_continuation) ) {
      int tmp;
      nlcp_trie_return(consumer_cpf) = answer_continuation; 
      hbreg = hreg;

      tmp = int_val(cell(answer_template));
      get_var_and_attv_nums(template_size, attv_num, tmp);
      answer_template--;

      table_consume_answer(CTXTc first_answer,template_size,attv_num,answer_template,
			   CallInfo_TableInfo(callInfo));

      if (is_conditional_answer(first_answer)) {
	xsb_dbgmsg((LOG_DELAY,
		"! POSITIVELY DELAYING in lay active (delayreg = %p)\n",
		delayreg));
	xsb_dbgmsg((LOG_DELAY, "\n>>>> delay_positively in lay_down_active\n"));
	xsb_dbgmsg((LOG_DELAY, ">>>> subgoal = "));
	dbg_print_subgoal(LOG_DELAY, stddbg, producer_sf);
	xsb_dbgmsg((LOG_DELAY, "\n"));
	{
	  /*
	   * Similar to delay_positively() in retry_active, we also
	   * need to put the substitution factor of the answer,
	   * var_addr[], into a term ret/n and pass it to
	   * delay_positively().
	   */
	  if (num_heap_term_vars == 0) {
	    delay_positively(producer_sf, first_answer,
			     makestring(get_ret_string()));
	  }
	  else {
#ifndef IGNORE_DELAYVAR
	    int i;
	    CPtr temp_hreg = hreg;
	    new_heap_functor(hreg, get_ret_psc(num_heap_term_vars));
	    for (i = 0; i < num_heap_term_vars; i++)
	      cell(hreg++) = (Cell) var_addr[i];
	    delay_positively(producer_sf, first_answer, makecs(temp_hreg));
#else
	    delay_positively(producer_sf, first_answer,
			     makestring(get_ret_string()));
#endif /* IGNORE_DELAYVAR */
	  }
	}
      }
      lpcreg = cpreg;
#ifdef CONC_COMPL
      break;
#endif
    }
    else {
#ifdef CONC_COMPL
      if( is_completed(producer_sf) || subg_tid(producer_sf) == th->tid ){
#endif
      breg = nlcp_prevbreg(consumer_cpf);
      Fail1;
#ifdef CONC_COMPL
      break;
      }
#endif
    }
#ifdef CONC_COMPL
    pthread_mutex_unlock(&completing_mut);
    pthread_cond_wait(&completing_cond,&completing_mut);
    }
    pthread_mutex_unlock(&completing_mut);
#endif
  }
XSB_End_Instr()

/*-------------------------------------------------------------------------*/

/*
 *  Instruction format:
 *    1st word: opcode X X X
 *
 *  Description:
 *    Returns to a consumer an answer if one is available, otherwise it
 *    suspends.  Answer consumption is effected by unifying the consumer's
 *    answer template with an answer.  This instruction is encountered only
 *    by backtracking into a consumer choice point frame, either as a
 *    result of WAM- style backtracking or having been resumed via a
 *    check-complete instruction.  The CPF field "nlcp-trie-return" points
 *    to the last answer consumed.  If none have yet been consumed, then it
 *    points to the dummy answer.
 */

XSB_Start_Instr(answer_return,_answer_return) 
  VariantSF consumer_sf;
  ALNptr answer_continuation;
  BTNptr next_answer;
  CPtr answer_template;
  int template_size, attv_num;

  /* Locate relevant answers
     ----------------------- */
  answer_continuation = ALN_Next(nlcp_trie_return(breg)); /* step to next answer */
  consumer_sf = (VariantSF)nlcp_subgoal_ptr(breg);
  answer_template = nlcp_template(breg);
#ifdef CONC_COMPL
  pthread_mutex_lock(&completing_mut);
  for(;;)
  {
#endif
  table_pending_answer( nlcp_trie_return(breg),
			answer_continuation,
			next_answer,
			(SubConsSF)consumer_sf,
			conssf_producer(consumer_sf),
			answer_template,
			switch_envs(breg),
			TPA_NoOp );

  if ( IsNonNULL(answer_continuation)) {
    int tmp;

    /* Restore Consumer's state
       ------------------------ */
    switch_envs(breg);
    ptcpreg = nlcp_ptcp(breg);
    delayreg = nlcp_pdreg(breg);
    restore_some_wamregs(breg, ereg);

    /* Consume the next answer
       ----------------------- */
    nlcp_trie_return(breg) = answer_continuation;   /* update */
    tmp = int_val(cell(answer_template));
    get_var_and_attv_nums(template_size, attv_num, tmp);
    answer_template--;

    table_consume_answer(CTXTc next_answer,template_size,attv_num,answer_template,
			 subg_tif_ptr(consumer_sf));

    if (is_conditional_answer(next_answer)) {
      /*
       * After load_solution_trie(), the substitution factor of the
       * answer is left in array var_addr[], and its arity is in
       * num_heap_term_vars.  We have to put it into a term ret/n (on 
       * the heap) and pass it to delay_positively().
       */
      if (num_heap_term_vars == 0) {
	delay_positively(consumer_sf, next_answer,
			 makestring(get_ret_string()));
      }
      else {
#ifndef IGNORE_DELAYVAR
	int i;
	CPtr temp_hreg = hreg;
	new_heap_functor(hreg, get_ret_psc(num_heap_term_vars));
	for (i = 0; i < num_heap_term_vars; i++) {
	  cell(hreg++) = (Cell) var_addr[i];
	}
	delay_positively(consumer_sf, next_answer, makecs(temp_hreg));
#else
	delay_positively(consumer_sf, next_answer,
			 makestring(get_ret_string()));
#endif /* IGNORE_DELAYVAR */
      }
    }
    lpcreg = cpreg;
#ifdef CONC_COMPL
    break;
#endif
  }

  else {
#ifdef CONC_COMPL
    if ( is_completed(nlcp_subgoal_ptr(breg)) || 
	 subg_tid(nlcp_subgoal_ptr(breg)) == th->tid )
    {
#endif

    /* Suspend this Consumer
       --------------------- */
    breg = nlcp_prevbreg(breg); /* in semi-naive this execs next active */
    restore_trail_condition_registers(breg);
    if (hbreg >= hfreg) hreg = hbreg; else hreg = hfreg;
    Fail1;
#ifdef CONC_COMPL
    break;
    }
#endif
  }
#ifdef CONC_COMPL
  pthread_mutex_unlock(&completing_mut);
  pthread_cond_wait(&completing_cond,&completing_mut);
  }
  pthread_mutex_unlock(&completing_mut);
#endif
XSB_End_Instr()

/*-------------------------------------------------------------------------*/

/*
 *  Instruction format:
 *    1st word: opcode X pred_arity perm_var_index
 *
 *  Description:
 *    Last instruction in each clause of a tabled predicate.  The instruction
 *    (1) saves the answer substitution in the answer trie and (2)
 *    adds a cell to the end of the answer list pointing to the root
 *    of the new subtstitution. All the
 *    information necessary to perform this Answer Check/Insert operation
 *    is saved in the producer's choice point frame.  This CP is 
 *    reached through the subgoal frame, which is noted in the first
 *    environment variable of the tabled clause.
 * 
 *    In the case where we have added an unconditional ground answer
 *    we perform early completion for the subgoal.
 *
 *    Next, if we are executing Local Evaluation, we fail after adding
 *    the answer (and perhaps performing ec)  This is not always the
 *    optimal way, as we need fail only if the subgoal is potentially
 *    a leader.
 * 
 *    If we are not executing local evaluation, we take the forward
 *    condinuation (i.e. we'll proceed).  In his case a delay element
 *    must be added to the delay list or the root subgoal of the
 *    current subgoal before proceeding.
 */

XSB_Start_Instr(new_answer_dealloc,_new_answer_dealloc) 
  Def2ops
  CPtr producer_cpf, producer_csf, answer_template;
  int template_size, attv_num, tmp;
  VariantSF producer_sf;
  xsbBool isNewAnswer = FALSE;
  BTNptr answer_leaf;

  ARITY = get_xax;
  Yn = get_xxa; /* we want the # of the register, not a pointer to it */

  ADVANCE_PC(size_xxx);
  producer_sf = (VariantSF)cell(ereg-Yn);
  producer_cpf = subg_cp_ptr(producer_sf);

#ifdef DEBUG_DELAYVAR
  xsb_dbgmsg((LOG_DEBUG,">>>> New answer for %s subgoal: ",
	     (is_completed(producer_sf) ? "completed" : "incomplete")));
  fprintf(stddbg, ">>>> ");
  dbg_print_subgoal(LOG_DEBUG, stddbg, producer_sf);
  xsb_dbgmsg((LOG_DEBUG,">>>> has delayreg = %p", delayreg));
#endif

  producer_csf = subg_compl_stack_ptr(producer_sf);

  /* if the subgoal has been early completed and its space reclaimed
   * from the stacks, access to its relevant information (e.g. to its
   * substitution factor) in the stacks is not safe, so better not
   * try to add this answer; it is a redundant one anyway...
   */
  if ((subgoal_space_has_been_reclaimed(producer_sf,producer_csf)) ||
      (IsNonNULL(delayreg) && answer_is_junk(delayreg))) {
    Fail1;
    XSB_Next_Instr();
  }

  /* answer template is now in the heap for generators */
  answer_template = tcp_template(subg_cp_ptr(producer_sf));
  tmp = int_val(cell(answer_template));
  get_var_and_attv_nums(template_size, attv_num, tmp);
  answer_template--;

#ifdef DEBUG_DELAYVAR
  xsb_dbgmsg(LOG_DEBUG,">>>> ARITY = %d; Yn = %d", (int)ARITY, (int)Yn);
#endif

  xsb_dbgmsg((LOG_DELAY, "\t--> This answer for "));
  dbg_print_subgoal(LOG_DELAY, stddbg, producer_sf);
#ifdef DEBUG_VERBOSE
  if (LOG_DELAY <= cur_log_level) {
    if (delayreg != NULL) {
      fprintf(stddbg, " has delay list = ");
      print_delay_list(stddbg, delayreg);
    } else {
      fprintf(stddbg, " has no delay list");
    }
  }
#endif

#ifdef DEBUG_DELAYVAR
  fprintf(stddbg, "\n>>>> (before variant_answer_search) template_size = %d\n",
	  (int)template_size);
  {
    int i;
    for (i = 0; i < template_size; i++) {
      fprintf(stddbg, ">>>> answer_template[%d] = ", i);
      printterm(stddbg, (Cell)(answer_template - i), 25);
      fprintf(stddbg, "\n");
    }
  }
#endif

  answer_leaf = table_answer_search( CTXTc producer_sf, template_size, attv_num,
				     answer_template, &isNewAnswer );

  if ( isNewAnswer ) {   /* go ahead -- look for more answers */
    delayreg = tcp_pdreg(producer_cpf);      /* restore delayreg of parent */
    if (is_conditional_answer(answer_leaf)) {	/* positive delay */
#ifndef LOCAL_EVAL
#ifdef DEBUG_DELAYVAR
      fprintf(stddbg, ">>>> delay_positively in new_answer_dealloc\n");
#endif
      /*
       * The new answer for this call is a conditional one, so add it
       * into the delay list for its root subgoal.  Notice that
       * delayreg has already been restored to the delayreg of parent.
       *
       * This is the new version of delay_positively().  Here,
       * ans_var_pos_reg is passed from variant_answer_search().  It is a
       * pointer to the heap where the substitution factor of the
       * answer was saved as a term ret/n (in variant_answer_search()).
       */
#ifndef IGNORE_DELAYVAR
      if (isinteger(cell(ans_var_pos_reg))) {
	delay_positively(producer_sf, answer_leaf,
			 makestring(get_ret_string()));
      }
      else 
	delay_positively(producer_sf, answer_leaf, makecs(ans_var_pos_reg));
#else
	delay_positively(producer_sf, answer_leaf,
			 makestring(get_ret_string()));
#endif /* IGNORE_DELAYVAR */
#endif /* ! LOCAL_EVAL */
    }
    else {
      if (template_size == 0) {
	/*
	 *  The table is for a ground call which we just proved true.
	 *  (We entered an ESCAPE Node, above, to note this fact in the
	 *  table.)  As we only need to do this once, we perform "early
	 *  completion" by ignoring the other clauses of the predicate
	 *  and setting the failure continuation (next_clause) field of
	 *  the CPF to a check_complete instr.
	 */
	perform_early_completion(producer_sf, producer_cpf);
#if defined(LOCAL_EVAL)
	breg = producer_cpf;
#endif
      }
    }
#ifdef CONC_COMPL
    pthread_cond_broadcast( &completing_cond ); 
#endif
#ifdef LOCAL_EVAL
    Fail1;	/* and do not return answer to the generator */
#else
    ptcpreg = tcp_ptcp(producer_cpf);
    cpreg = *((byte **)ereg-1);
    ereg = *(CPtr *)ereg;
    lpcreg = cpreg; 
#endif
  }
  else     /* repeat answer -- ignore */
     Fail1;
XSB_End_Instr()

/*-------------------------------------------------------------------------*/

/*
 *  Instruction format:
 *    1st word: opcode X X pred_arity
 *    2nd word: pred_next_clause_label
 *
 *  Description:
 *    Store the predicate's arity in "op1", update the failure continuation
 *    to the instruction following this one, and set the program counter to
 *    the predicate's next code subblock to be executed, as pointed to by
 *    the second argument to this instruction.  Finally, restore the state
 *    at the point of choice and continue execution using the predicate's
 *    next code subblock.
 */

XSB_Start_Instr(tableretry,_tableretry)
  Def1op
  Op1(get_xxa);
  tcp_pcreg(breg) = lpcreg+sizeof(Cell)*2;
  lpcreg = *(pb *)(lpcreg+sizeof(Cell));
  restore_type = 0;
  TABLE_RESTORE_SUB
XSB_End_Instr()

/*-------------------------------------------------------------------------*/

/*
 *  Instruction format:
 *    1st word: opcode X X pred_arity
 *
 *  Description:
 *    Store the predicate's arity in "op1", update the failure continuation
 *    to a check_complete instruction, and set the program counter to the
 *    predicate's last code subblock to be executed, as pointed to by the
 *    second argument to this instruction.  Finally, restore the state at
 *    the point of choice and continue execution with this last code
 *    subblock.
 */

XSB_Start_Instr(tabletrust,_tabletrust)
  Def1op
  Op1(get_xxa);
  ADVANCE_PC(size_xxx);
  tcp_pcreg(breg) = (byte *) &check_complete_inst;
  lpcreg = *(pb *)lpcreg;
#if defined(LOCAL_EVAL)
  /* trail cond. registers should not be restored here for Local */
  restore_type = 0;
#else
  restore_type = 1;
#endif
  TABLE_RESTORE_SUB
XSB_End_Instr()
/*-------------------------------------------------------------------------*/

#include "complete_xsb_i.h"

/*-------------------------------------------------------------------------*/

XSB_Start_Instr(resume_compl_suspension,_resume_compl_suspension)
#ifdef DEBUG_DELAYVAR
      fprintf(stddbg, ">>>> resume_compl_suspension is called\n");
#endif
{
  if ((unsigned long) csf_pcreg(breg) == 
      (unsigned long) &resume_compl_suspension_inst) {
    CPtr csf = breg;
    
    /* Switches the environment to a frame of a subgoal that was	*/
    /* suspended on completion, and sets the continuation pointer.	*/
    check_glstack_overflow(0,lpcreg,OVERFLOW_MARGIN);
    freeze_and_switch_envs(csf, COMPL_SUSP_CP_SIZE);
    ptcpreg = csf_ptcp(csf);
    neg_delay = (csf_neg_loop(csf) != FALSE);
    delayreg = csf_pdreg(csf);
    cpreg = csf_cpreg(csf); 
    ereg = csf_ereg(csf);
    ebreg = csf_ebreg(csf);
    hbreg = csf_hreg(csf);
    save_find_locx(ereg);
    hbreg = hreg;
    breg = csf_prevcsf(csf);
    lpcreg = cpreg;
  } else {
    CPtr csf = cs_compsuspptr(breg);
    /* Switches the environment to a frame of a subgoal that was	*/
    /* suspended on completion, and sets the continuation pointer.	*/
    check_glstack_overflow(0,lpcreg,OVERFLOW_MARGIN);
    freeze_and_switch_envs(csf, COMPL_SUSP_CP_SIZE);
    ptcpreg = csf_ptcp(csf);
    neg_delay = (csf_neg_loop(csf) != FALSE);
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
  }
}
XSB_End_Instr()

/*----------------------------------------------------------------------*/

