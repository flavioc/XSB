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

#ifdef MULTI_THREAD

#define TABLE_IS_SHARED()	(threads_current_sm == SHARED_SM)

#define LOCK_CALL_TRIE()						\
{	if( TABLE_IS_SHARED() )						\
        {	pthread_mutex_lock( &TIF_CALL_TRIE_LOCK(tip) );		\
		SYS_MUTEX_INCR( MUTEX_CALL_TRIE )			\
}	}

#define UNLOCK_CALL_TRIE()						\
{	if( TABLE_IS_SHARED() )						\
		pthread_mutex_unlock( &TIF_CALL_TRIE_LOCK(tip) );	\
}

#else
#define LOCK_CALL_TRIE()
#define UNLOCK_CALL_TRIE()
#endif

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

  int incrflag = 0; /* for incremental evaluation */

#ifdef SHARED_COMPL_TABLES
  byte * inst_addr = lpcreg;
  int table_tid ;
  int grabbed = FALSE;
  th_context * waiting_for_thread;
#endif
#ifdef MULTI_THREAD_RWL
  CPtr tbreg;
#ifdef SLG_GC
  CPtr old_cptop;
#endif
#endif

  /* incremental evaluation */
  VariantSF parent_table_sf=NULL; /* used for creating call graph */
  old_call=NULL;

  xwammode = 1;
  CallInfo_Arguments(callInfo) = reg + 1;
  CallInfo_CallArity(callInfo) = get_xxa; 
  LABEL = (CPtr)((byte *) get_xxxl);  
  Op1(get_xxxxl);
  tip =  (TIFptr) get_xxxxl;
  SET_TRIE_ALLOCATION_TYPE_TIP(tip); /* No-op in seq engine */
#ifdef MULTI_THREAD
  handle_dispatch_block(tip);
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

  LOCK_CALL_TRIE();
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

#ifdef MULTI_THREAD
  if( !IsNULL(producer_sf) )
	UNLOCK_CALL_TRIE();
#endif

/* for incremental evaluation */

/* Get parent table subgoalframe from ptcpreg */
  if(IsNonNULL(ptcpreg)){
    parent_table_sf=(VariantSF)ptcpreg;
    if(IsIncrSF(parent_table_sf))
      incrflag=1;
  }
 
/* adding called-by graph edge */
  if((incrflag)&&(IsNonNULL(producer_sf))){
    if(IsIncrSF(producer_sf)){
      addcalledge(producer_sf->callnode,parent_table_sf->callnode);  
    }else{
      if(!get_opaque(TIF_PSC(CallInfo_TableInfo(callInfo))))
	xsb_abort("Predicate %s/%d not declared incr_table\n", get_name(TIF_PSC(CallInfo_TableInfo(callInfo))),get_arity(TIF_PSC(CallInfo_TableInfo(callInfo))));       
    }
  }



  xsb_dbgmsg((LOG_DEBUG,"After variant call search AT: %x\n",answer_template));

#ifdef SHARED_COMPL_TABLES
/* This allows sharing of completed tables.
   If the subgoal frame is not new, and the table is being generated by
   a different thread, wait for it to complete.
 */
  grabbed = FALSE ;
  if (TABLE_IS_SHARED() && !IsNULL(producer_sf)) 
  {
     if( !is_completed(producer_sf) && subg_grabbed(producer_sf) 
				    && th->is_deadlock_leader )
     {
	grabbed = TRUE;
        subg_grabbed(producer_sf) = FALSE ;
	goto seq_table_try;
     }
     if( !is_completed(producer_sf) && subg_tid(producer_sf) != xsb_thread_id )
     {
     	pthread_mutex_lock(&completing_mut);
     	SYS_MUTEX_INCR( MUTEX_COMPL );
     	while( !is_completed(producer_sf))
     	{  
	   table_tid = subg_tid(producer_sf) ;
	   waiting_for_thread = find_context(table_tid) ;
	   if( would_deadlock( table_tid, xsb_thread_id ) )
           {       /* code for leader */
                   reset_other_threads( th, waiting_for_thread, producer_sf );
                   /* unlocks completing_mut asap */
		   th->is_deadlock_leader = TRUE ;
		   grabbed = TRUE;
                   subg_grabbed(producer_sf) = FALSE ;
		   goto seq_table_try;
           }
           th->waiting_for_subgoal = producer_sf ;
           th->waiting_for_tid = table_tid ;
	   th->is_deadlock_leader = FALSE ;
	   pthread_cond_wait(&TIF_ComplCond(tip),&completing_mut);
           SYS_MUTEX_INCR( MUTEX_COMPL );
        }
        /* The thread has been reset and we should restart a tabletry instr */
        th->waiting_for_tid = -1 ;
        th->waiting_for_subgoal = NULL ;
        pthread_mutex_unlock(&completing_mut);
        lpcreg = pcreg ;
        XSB_Next_Instr() ;
     } 
  }

seq_table_try:
  if ( IsNULL(producer_sf) || grabbed ) {

    /* New Producer
       ------------ */
    CPtr producer_cpf;
    if( !grabbed )
    {
      producer_sf = NewProducerSF(CTXTc CallLUR_Leaf(lookupResults),
				   CallInfo_TableInfo(callInfo));
      subg_tid(producer_sf) = xsb_thread_id;
      subg_grabbed(producer_sf) = FALSE;
      UNLOCK_CALL_TRIE() ;
    }
    else
    {	subg_compl_stack_ptr(producer_sf) = openreg - COMPLFRAMESIZE;
    }
#else  /* !SHARED_COMPL_TABLES */
  if ( IsNULL(producer_sf) ) {

    /* New Producer
       ------------ */
    CPtr producer_cpf;
    producer_sf = NewProducerSF(CTXTc CallLUR_Leaf(lookupResults),
				 CallInfo_TableInfo(callInfo));
#endif /* !SHARED_COMPL_TABLES */
#ifdef CONC_COMPL
    subg_tid(producer_sf) = xsb_thread_id;
    subg_tag(producer_sf) = INCOMP_ANSWERS;
    UNLOCK_CALL_TRIE() ;
#endif

    /* for incremental evaluation - start */
    
    /* table_call_search tried to find the affected call, so if it has
       found the answer table of the new call it is made same as the
       answer table of the old call - so that we can check whether the
       answers that will be generated for the new call are same as that for
       the old call */

    if(IsNonNULL(old_call)){
      producer_sf->callnode->prev_call=old_call;

	 producer_sf->callnode->outedges=old_call->outedges;
	 producer_sf->callnode->outcount=old_call->outcount;
	 producer_sf->callnode->outedges->callnode=producer_sf->callnode;
	 producer_sf->ans_root_ptr=old_answer_table;
	 old_call->falsecount=0; /* this will stop propagation of unmarking */
	 old_call->deleted=1; 
	 old_call=NULL;
	 old_answer_table=NULL;
    }else
      if(IsIncrSF(producer_sf))
	initoutedges(producer_sf->callnode);
    
    /* adding called-by graph edge for new call */
    if(incrflag){
      if(IsIncrSF(producer_sf)){
	addcalledge(producer_sf->callnode,parent_table_sf->callnode);  
      }else{
	if(!get_opaque(TIF_PSC(CallInfo_TableInfo(callInfo))))
	  xsb_abort("Predicate %s/%d not declared incr_table\n", get_name(TIF_PSC(CallInfo_TableInfo(callInfo))),get_arity(TIF_PSC(CallInfo_TableInfo(callInfo))));       
      }
    }
    /* for incremental evaluation - end */

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
#ifdef CONC_COMPL
    compl_ext_cons(openreg) = NULL ;
    tcp_compl_stack_ptr(producer_cpf) = openreg ;
#endif
    ptcpreg = (CPtr)producer_sf;
    subg_cp_ptr(producer_sf) = breg = producer_cpf;
    xsb_dbgmsg((LOG_COMPLETION,"just created tabled cp %x\n",breg));
    delayreg = NULL;
    if (root_address == 0)
      root_address = breg;
    hbreg = hreg;
    lpcreg = (byte *) LABEL;	/* branch to program clause */
    XSB_Next_Instr();
  }

  else if ( is_completed(producer_sf) ) {

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

      /* Initialize var_regs[] as the attvs in the call.  This is
	 needed by the trie_xxx_val instructions, and the symbols of
	 the trie nodes have been set up to account for this in
	 variant_answer_search() -- see the documentation there.  */
      if (attv_num > 0) {
	CPtr cptr;
	for (cptr = answer_template - 1;
	     cptr >= answer_template - template_size; cptr--) {
	  // tls changed from 10/05 cptr >= answer_template + template_size; cptr++) 
	  if (isattv(cell(cptr))) {
	    var_regs[++num_vars_in_var_regs] = (CPtr) cell(cptr);
	    xsb_dbgmsg((LOG_TRIE_INSTR, "setting var_regs for attv %d \n",
			num_vars_in_var_regs));
	  }
	}
	/* now num_vars_in_var_regs should be attv_num - 1 */
      }

      reg_arrayptr = reg_array-1;
      for (i = 0; i < template_size; i++) {
	pushreg(cell(answer_template-template_size+i));
      }
      delay_it = 1;
      lpcreg = (byte *)subg_ans_root_ptr(producer_sf);
#ifdef MULTI_THREAD_RWL
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
    CPtr producer_cpf = NULL;

    /* with variant tabling and thus in CONC_COMPL shared tables, 
       producer_sf == consumer_sf */
    if( subg_tid(producer_sf) == xsb_thread_id )
    {
#endif
    adjust_level(subg_compl_stack_ptr(producer_sf));
#ifdef CONC_COMPL
        consumer_cpf = answer_template;
    }
    else if( openreg < COMPLSTACKBOTTOM )
        consumer_cpf = answer_template;
    else
    {
	producer_cpf = answer_template;
    	SaveProducerCPF(producer_cpf, (pb)&check_complete_inst, producer_sf,
                    	CallInfo_CallArity(callInfo), (hreg - 1));
	consumer_cpf = breg = producer_cpf;
    }
#else
    consumer_cpf = answer_template;
#endif
    save_find_locx(ereg);

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

#ifdef CONC_COMPL
    nlcp_tid(consumer_cpf) = makeint(xsb_thread_id);

    if( IsSharedSF(producer_sf) )
	SYS_MUTEX_LOCK( MUTEX_CONS_LIST ) ;
    /* this is done again to allow to hold the lock for a shorter period */
    nlcp_prevlookup(consumer_cpf) = subg_asf_list_ptr(producer_sf) ;
#endif
    subg_asf_list_ptr(producer_sf) = consumer_cpf;
#ifdef CONC_COMPL
    if( IsSharedSF(producer_sf) )
	SYS_MUTEX_UNLOCK( MUTEX_CONS_LIST ) ;
#endif
    breg = bfreg = consumer_cpf;

    xsb_dbgmsg((LOG_COMPLETION,"created ccp at %x with prevbreg as %x\n",
		breg,nlcp_prevbreg(breg)));

#ifdef CONC_COMPL
    if( subg_tid(producer_sf) != xsb_thread_id )
    {
	push_completion_frame(producer_sf);
	compl_ext_cons(openreg) = consumer_cpf;
	if( openreg == (COMPLSTACKBOTTOM - COMPLFRAMESIZE) )
    		tcp_compl_stack_ptr(producer_cpf) = openreg ;
    }
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

      /* TLS 060913: need to initialize here, as it doesnt get
	 initialized in all paths of table_consume_answer */
      num_heap_term_vars = 0;

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
	    if (var_addr == NULL) printf("var_addr NULL 3\n");
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
    }
    else {
      breg = nlcp_prevbreg(consumer_cpf);
      Fail1;
    }
  }
XSB_End_Instr()


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

/* incremental evaluation */

XSB_Start_Instr(tabletrysinglenoanswers,_tabletrysinglenoanswers) 
    DefOps13
  /*
   *  Retrieve instruction arguments and test the system stacks for
   *  overflow.  The local PCreg, "lpcreg", is incremented to point to
   *  the instruction to be executed should this one fail.
   */
  
  
  TabledCallInfo callInfo;
  CallLookupResults lookupResults;
  VariantSF  sf;
  TIFptr tip;
  callnodeptr c;

#ifdef MULTI_THREAD
  xsb_abort("Incremental Maintenance of tables is not available for multithreaded engine.\n");
#endif  
  
  xwammode = 1;
  CallInfo_Arguments(callInfo) = reg + 1;
  CallInfo_CallArity(callInfo) = get_xxa; 
  LABEL = (CPtr)((byte *) get_xxxl);  
  Op1(get_xxxxl);
  tip =  (TIFptr) get_xxxxl;
  
  SET_TRIE_ALLOCATION_TYPE_TIP(tip); 

  
  CallInfo_TableInfo(callInfo) = tip;
    
  check_tcpstack_overflow;
  CallInfo_VarVectorLoc(callInfo) = top_of_cpstack;

  /*
  if ( this_instr == tabletry ) {
    continuation = lpcreg;
  }
  else 
    continuation = (pb) &check_complete_inst;
    
  check_glstack_overflow(CallInfo_CallArity(callInfo),lpcreg,OVERFLOW_MARGIN);
  */


  table_call_search_incr(CTXTc &callInfo,&lookupResults);
  
  if(IsNonNULL(ptcpreg)){
   sf=(VariantSF)ptcpreg;
   if(IsIncrSF(sf)){
     c=(callnodeptr)BTN_Child(CallLUR_Leaf(lookupResults));
     if(IsNonNULL(c)){
       addcalledge(c,sf->callnode);  
     }
   }else
     xsb_abort("Predicate %s/%d not declared incr_table\n", get_name(TIF_PSC(subg_tif_ptr(sf))),get_arity(TIF_PSC(subg_tif_ptr(sf))));      
  }


  ADVANCE_PC(size_xxx);
  lpcreg = *(pb *)lpcreg;

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
  
  //  fprintf(stddbg,"Starting answer return %x (%x) (prev %x) aln %x\n",
  //	  breg,*lpcreg,nlcp_prevbreg(breg),answer_continuation); 

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

    //    printf("answer_template %x size %d\n",answer_template,template_size);
    //    sfPrintGoal(CTXTdeclc stddbg, consumer_sf, FALSE);


    /* TLS 060927: need to initialize here, as it doesnt get
       initialized in all paths of table_consume_answer */
      num_heap_term_vars = 0;

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
	if (var_addr == NULL) printf("var_addr NULL 4\n");
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
  }

  else {

    /* Suspend this Consumer
       --------------------- */
    xsb_dbgmsg((LOG_DEBUG,"Failing from answer return %x to %x (inst %x)\n",
		breg,nlcp_prevbreg(breg),*tcp_pcreg(nlcp_prevbreg(breg))));
    breg = nlcp_prevbreg(breg); /* in semi-naive this execs next active */
    restore_trail_condition_registers(breg);
    if (hbreg >= hfreg) hreg = hbreg; else hreg = hfreg;
    Fail1;
  }
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

  xsb_dbgmsg((LOG_COMPLETION,"starting new_answer breg %x\n",breg));
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
      fprintf(stddbg, " has no delay list\n");
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

  SET_TRIE_ALLOCATION_TYPE_SF(producer_sf); /* No-op in seq engine */
  answer_leaf = table_answer_search( CTXTc producer_sf, template_size, attv_num,
				     answer_template, &isNewAnswer );

  if ( isNewAnswer ) {   /* go ahead -- look for more answers */
    /* incremental evaluation */
    if(IsIncrSF(producer_sf))
      subg_callnode_ptr(producer_sf)->no_of_answers++;
    
    
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
	 *
	 */
	perform_early_completion(producer_sf, producer_cpf);
#if defined(LOCAL_EVAL)
	  breg = producer_cpf;
#endif
      }
    }
#ifdef LOCAL_EVAL
    Fail1;	/* and do not return answer to the generator */
    xsb_dbgmsg((LOG_DEBUG,"Failing from new answer %x to %x (inst %x)\n",
		breg,tcp_pcreg(breg),*tcp_pcreg(breg)));

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

