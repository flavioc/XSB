/* File:      slginsts.i
** Author(s): Swift, Rao, Sagonas, Juliana Freire
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
#define PREVSUSREC	xtemp3
#define SOLUTIONCP	xtemp3
#define COMPL_SUSP_ENV	xtemp3
#define VarsInCall	xtemp5
#define SUBGOAL		xtemp12

/* #define set_min(a,b,c)	if (b < c) a = b; else a = c */ /* JF */

/*----------------------------------------------------------------------*/

case tabletry:		/* cur_label arity label xcurcall	*/
    xwammode = 1; ppad;
    ARITY = (Cell)(*lpcreg++);	/* op1 */
    pad64;
    LABEL = (CPtr)(*(byte **) lpcreg);	/* op3 */
#ifdef CP_PVR_DEBUG
    fprintf(stderr,"Table Try: arity = %d breg=%d\n", (int)ARITY,(int)breg);
#endif
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
    variant_call_search(ARITY, reg, &xcurcall, &xflag);
    if (xflag) {		    /* xflag = 1 if a variant-subgoal exists */
      xcurcall = (CPtr) *xcurcall;	/* point xcurcall to the call_struct */
      if (is_completed(xcurcall)) {
	if (has_answer_code(xcurcall)) goto return_table_code;
	else { Fail1; goto contcase; }
      }
      else goto lay_down_active;
    }
    else {  /* there is no variant-subgoal */
	    /* put subgoal in stack and init structure components */
	    /* makes call structure point back to CallTableInfo-original call */
	    /* compl_stack_ptr = openreg-COMPLFRAMESIZE+1 (all others=0) */
      create_subgoal_frame(*xcurcall, Paren);

      save_find_locx(ereg);

      save_registers(VarPosReg, (int)ARITY, i, rreg);
      /*
       * set a choice point generating node-the solution node is responsible
       * for going through the different choices and for starting the check
       * complete when it runs out of alternatives
       * the variables in the call are stored right before this CP
       * uses: bfreg,efreg,trfreg,hfreg,ereg,cpreg,trreg,hreg,ebreg,tbreg,
       * prev=breg,lpcreg
       */
      save_solution_choicepoint(VarPosReg, ereg, (CPtr) *xcurcall, breg);
      breg = VarPosReg;

#ifdef CP_PVR_DEBUG
    fprintf(stderr,"Table Try: arity = %d TCP=%d subg=%d\n",(int)ARITY,(int)breg,(int)*xcurcall);
    fprintf(stderr,"Table Try: saved delayreg==%d\n",(int)delayreg);
#endif

#ifdef LOCAL_EVAL
      tcp_arity(breg) = (int) ARITY;
#endif  /* LOCAL_EVAL */


      delayreg = NULL;
      subg_cp_ptr(*xcurcall) = ptcpreg = breg;
      if (root_address == 0) root_address = breg;

      push_completion_frame((SGFrame)*xcurcall); /* init susp_ptr of cur subgoal */
      hbreg = hreg;
      lpcreg = (byte *) LABEL;	/* branch to program clause */
    } 
    goto contcase;

/*----------------------------------------------------------------------*/
/* get all current answers - copy them out from the table, if any.	*/
/*----------------------------------------------------------------------*/

case retry_active:
#ifdef CP_PVR_DEBUG
    fprintf(stderr, "Retry Active\n");
#endif

    switch_envs(breg);
    ptcpreg = nlcp_ptcp(breg);
    delayreg = nlcp_pdreg(breg);
    restore_some_wamregs(breg, ereg);
    /* An extra computation in the interest of clarity */
    CallNumVar = *(breg + NLCPSIZE);
    op3 = breg + NLCPSIZE + CallNumVar;
    /* if an answer has already been consumed, get next otherwise get all
     * answers in the table -- note that nlcp_trie_return points to the
     * last answer which has been consumed
     */
    SUBGOAL = nlcp_subgoal_ptr(breg);
    /* to the dummy answer -- that will never be NULL */
    OldRetPtr = aln_next_aln(nlcp_trie_return(breg)); /* get next answer */
    /* jf: skip the deleted answers */
    if (OldRetPtr){
/*----------------------------------------------------------------------*/
      if (is_conditional_answer(aln_answer_ptr(OldRetPtr))) {
#ifdef DEBUG_DELAY
	fprintf(stderr,
		"! POSITIVELY DELAYING in retry active (delayreg = %p)\n",
		delayreg);
#endif
	delay_positively(SUBGOAL, aln_answer_ptr(OldRetPtr));
#ifdef DEBUG_DELAY
	fprintf(stderr, "Returning a conditional answer...\n");
#endif
      }
#ifdef DEBUG_DELAY
      else {
	fprintf(stderr, "Returning an unconditional answer...\n");
      }
#endif
/*----------------------------------------------------------------------*/
      nlcp_trie_return(breg) = OldRetPtr; /* last answer consumed */
      TrieRetPtr = get_next_trie_solution(&OldRetPtr);
#ifdef DEBUG_REV
      fprintf(stderr,"new_lookup-breg=%d: getting next trie solution %d. \n",
	      (int)breg,(int)TrieRetPtr);
#endif      
      load_solution_trie(CallNumVar,op3,TrieRetPtr);
      lpcreg = cpreg;
    }
    else {
#ifdef DEBUG_REV
	fprintf(stderr,"new_lookup %d: no trie solution to get. \n",(int) breg);
#endif
	breg = nlcp_prevbreg(breg); /* in semi-naive this execs next active */
	Fail1;
    }
    goto contcase;

/*----------------------------------------------------------------------*/
/*  New answers are added to the tail of the answer list of a subgoal	*/
/*  structure.  Upon the derivation of the first answer for a subgoal,	*/
/*  all negation suspensions of the subgoal are abolished.		*/
/*----------------------------------------------------------------------*/

case new_answer_dealloc: {
    if (delayreg != NULL && answer_is_junk(delayreg)) {
      Fail1; goto contcase;
    }

    pad;
    ARITY = (Cell) (*lpcreg++);
    Yn = (Cell) (*lpcreg++);
    pad64;
    xtemp4 = (CPtr) *(ereg-Yn);
    /*
     * breg (saved in local stack as an offset) -- first permanent var
     */
    SOLUTIONCP = (CPtr)(tcpstack.high - int_val(xtemp4));
    CallNumVar = *(SOLUTIONCP + TCP_SIZE + (Cell) ARITY);
    VarsInCall = SOLUTIONCP + TCP_SIZE + (Cell) ARITY + CallNumVar;
    SUBGOAL = tcp_subgoal_ptr(SOLUTIONCP);
/*----------------------------------------------------------------------*/
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
/*----------------------------------------------------------------------*/
    xflag = 0;
    TrieRetPtr = variant_trie_search(CallNumVar,VarsInCall,SUBGOAL,&xflag);
    do_delay_stuff(TrieRetPtr, (SGFrame)SUBGOAL, xflag);

#ifdef LOCAL_EVAL
    if(xflag) {
#ifdef DEBUG_REV
      fprintf(stderr,"Repeated answer: %d (subg=%d)\n",(int)TrieRetPtr,(int)SUBGOAL);
#endif
    } /* if (xflag) - repeated answer */
    else { /* new answer */
#ifdef DEBUG_REV
      fprintf(stderr,"New answer: %d (subg=%d)\n",(int)TrieRetPtr,(int)SUBGOAL);
#endif
      /* restore delayreg of parent */ /* shouldn't - it will fail!! */
      delayreg = tcp_pdreg(SOLUTIONCP);
/*----------------------------------------------------------------------*/
#ifdef DEBUG_DELAY
      fprintf(stderr, " (adding it ");
#endif
/*----------------------------------------------------------------------*/
      if (is_conditional_answer(TrieRetPtr)) {	/* positive delay */
	/* check: I think this is being done twice unnecesarily... */
/* THIS IS DONE IN THE COMMON DIRECTORY */
/*	delay_positively(SUBGOAL, TrieRetPtr);  */
#ifdef DEBUG_DELAY
	fprintf(stderr, "conditionally)\n");
#endif
      }
      else {
#ifdef DEBUG_DELAY
	fprintf(stderr, "unconditionally)\n");
#endif
	if (CallNumVar == 0) {	/* perform early completion */
	  tcp_pcreg(SOLUTIONCP) = (byte *) &check_complete_inst;
	  mark_as_completed(SUBGOAL);
	  breg = SOLUTIONCP; /* this is done for LOCAL_EVAL */
	} /* if (Call... */
      } /* else */
    } /* else new answer */
    
    Fail1; /* and do not return answer */

#else /* LOCAL_EVAL */
    if (xflag) {
/*----------------------------------------------------------------------*/
#ifdef DEBUG_DELAY
      fprintf(stderr, " (variant exists, failing)\n");
#endif
/*----------------------------------------------------------------------*/
      Fail1;  /* do not return repeated answer to sol_cp */
    }
    else { /* go ahead -- look for more answers */
/*----------------------------------------------------------------------*/
      delayreg = tcp_pdreg(SOLUTIONCP);	/* restore delayreg of parent */
/*----------------------------------------------------------------------*/
#ifdef DEBUG_DELAY
      fprintf(stderr, " (adding it ");
#endif
/*----------------------------------------------------------------------*/
      if (is_conditional_answer(TrieRetPtr)) {	/* positive delay */
	delay_positively(SUBGOAL, TrieRetPtr);
#ifdef DEBUG_DELAY
	fprintf(stderr, "conditionally)\n");
#endif
      } else {
#ifdef DEBUG_DELAY
	fprintf(stderr, "unconditionally)\n");
#endif
	if (CallNumVar == 0) {	/* perform early completion */
	  tcp_pcreg(SOLUTIONCP) = (byte *) &check_complete_inst;
	  mark_as_completed(SUBGOAL);
	}
      }
/*----------------------------------------------------------------------*/
      ptcpreg = tcp_ptcp(SOLUTIONCP);
      cpreg = *((byte **)ereg-1);
      ereg = *(CPtr *)ereg;
      lpcreg = cpreg; 
    }
#endif /* LOCAL_EVAL */
    goto contcase;
} /* new_answer_dealloc */

/*----------------------------------------------------------------------*/

 case tableretry: /* PPA-L */
#ifdef CP_PVR_DEBUG
    printf("Table Retry\n");
#endif
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
#ifdef CP_PVR_DEBUG
    printf("Table Trust\n");
#endif
    ppad; op1byte;
    pad64;
    tcp_pcreg(breg) = (byte *) & check_complete_inst;
    lpcreg = *(pb *)lpcreg;
#ifdef LOCAL_EVAL
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
#ifdef CP_PVR_DEBUG
    printf("Table Try Single: breg=%d\n", (int) breg);
#endif
    xwammode = 1;    ppad;
    ARITY = (Cell)(*lpcreg++);
    pad64;
    LABEL = (CPtr)(*(byte **) lpcreg);  ADVANCE_PC;
    xcurcall = * (CPtr *) lpcreg; ADVANCE_PC;
    xflag = 1;
    VarPosReg = top_of_cpstack;
    check_tcpstack_overflow(VarPosReg);
    variant_call_search(ARITY, reg, &xcurcall, &xflag);
    if (xflag) {                        /* xflag = 1 if variant */ 
      xcurcall = (CPtr) *xcurcall;	/* point xcurcall to the call_struct */
      if (is_completed(xcurcall)) {
	if (has_answer_code(xcurcall)) goto return_table_code;
	else { Fail1; goto contcase; }
      }
      else goto lay_down_active;
    }
    else {  /* create solution choice point  */
      create_subgoal_frame(*xcurcall, Paren);

      /* now lay down single clause choicepoint */
      save_find_locx(ereg);

      save_registers(VarPosReg, (int)ARITY, i, rreg);
      save_singleclause_choicepoint(VarPosReg, ereg, (CPtr) *xcurcall, breg);
      breg = VarPosReg;
#ifdef CP_PVR_DEBUG
    fprintf(stderr,"Table Try: arity = %d TCP=%d subg=%d\n",(int)ARITY,(int)breg,(int)*xcurcall);
    fprintf(stderr,"Table Try: saved delayreg==%d\n",(int)delayreg);
#endif

#ifdef LOCAL_EVAL
      tcp_arity(breg) = (int) ARITY;
#endif

      delayreg = NULL;
      subg_cp_ptr(*xcurcall) = ptcpreg = breg;
      if (root_address == 0) root_address = breg;
      push_completion_frame((SGFrame) *xcurcall);
    }
    hbreg = hreg;
    lpcreg = (byte *) LABEL; /* go to clause ep */
    goto contcase;

/*----------------------------------------------------------------------*/
/* completion_suspension (formely known as return_completion)		*/
/*	Switches the environments to a frame of a subgoal that was	*/
/*	suspended on completion, and sets the continuation pointer.	*/
/*----------------------------------------------------------------------*/

case completion_suspension:
      check_glstack_overflow(MAX_ARITY,lpcreg,OVERFLOW_MARGIN) ;
      COMPL_SUSP_ENV = cs_compsuspptr(breg);
      freeze_and_switch_envs(COMPL_SUSP_ENV, COMPL_SUSP_CP_SIZE);
      ptcpreg = csf_ptcp(COMPL_SUSP_ENV);
      neg_delay = csf_neg_loop(COMPL_SUSP_ENV);
      delayreg = csf_pdreg(COMPL_SUSP_ENV);
#ifdef DEBUG_DELAY
      fprintf(stderr, "... Executing a Completion Suspension for subgoal ");
      print_subgoal(stderr, (SGFrame)csf_subgoal_ptr(COMPL_SUSP_ENV));
      fprintf(stderr, " in the body of ");
      if (csf_ptcp(COMPL_SUSP_ENV) != NULL) {
	print_subgoal(stderr, (SGFrame)tcp_subgoal_ptr(ptcpreg));
      } else fprintf(stderr, "an UNTABLED predicate");
      if (neg_delay != FALSE) fprintf(stderr, "\t(Delayed)");
      fprintf(stderr, "\n");
#endif
      cpreg = csf_cpreg(COMPL_SUSP_ENV); 
      ereg = csf_ereg(COMPL_SUSP_ENV);
      ebreg = csf_ebreg(COMPL_SUSP_ENV);
      hbreg = csf_hreg(COMPL_SUSP_ENV);
      save_find_locx(ereg);
      hbreg = hreg;
      if (csf_prevcsf(COMPL_SUSP_ENV) != NULL) {
#ifdef KOSTIS_DEBUG
        fprintf(stderr, "Completion Suspension in THEN part...\n");
#endif
	cs_compsuspptr(breg) = csf_prevcsf(COMPL_SUSP_ENV);
      }
      else {
#ifdef KOSTIS_DEBUG
        fprintf(stderr, "Completion Suspension in ELSE part...\n");
#endif
	breg = cs_prevbreg(breg);
      }
      lpcreg = cpreg;
      goto contcase;

/*----------------------------------------------------------------------*/

return_table_code:
#ifdef DEBUG_DELAY
	xsb_warn("Returning answers from a COMPLETED table...");
#endif
	CallNumVar = *(VarPosReg);
        num_vars_in_var_regs = -1;
        reg_arrayptr = reg_array -1;
	for (cptr = VarPosReg+1; cptr <= VarPosReg+CallNumVar; cptr++) {
	  pushreg(*cptr);
	}
	lpcreg = (byte *) subg_ans_root_ptr(xcurcall);
	goto contcase;

/*----------------------------------------------------------------------*/

lay_down_active:
    check_glstack_overflow(MAX_ARITY,lpcreg,OVERFLOW_MARGIN) ;
	adjust_level(subg_compl_stack_ptr(xcurcall));
	PREVSUSREC = subg_asf_list_ptr(xcurcall);
	xtemp9 = ebreg;
	save_find_locx(ereg);
	efreg = ebreg;
	if (trreg > trfreg) trfreg = trreg;
	if (hfreg < hreg) hfreg = hreg;
	save_nl_choicepoint(VarPosReg,ereg,xcurcall,PREVSUSREC,breg);
	breg = bfreg = VarPosReg;
#ifdef CP_PVR_DEBUG
	fprintf(stderr, "---> lay down active breg=%d\n",(int)breg);
#endif
	subg_asf_list_ptr(xcurcall) = (CPtr) bfreg; /* new susp into front */
	/* jf: 010695 init nlcp_trie_return anyway, so that it points
	 * to the dummy answer */
        nlcp_trie_return(breg) = subg_ans_list_ptr(xcurcall);
	OldRetPtr = subg_answers(xcurcall);
	/* jf: skip the deleted answers */
        if (OldRetPtr)	{
/*----------------------------------------------------------------------*/
	  if (is_conditional_answer(aln_answer_ptr(OldRetPtr))) {
#ifdef DEBUG_DELAY
	    fprintf(stderr,
		    "! POSITIVELY DELAYING in lay active (delayreg = %p)\n",
		    delayreg);
#endif
	    delay_positively(xcurcall, aln_answer_ptr(OldRetPtr));
#ifdef DEBUG_DELAY
	    fprintf(stderr, "! Returning a conditional answer...\n");
#endif
	  }
#ifdef DEBUG_DELAY
	    else {
	      fprintf(stderr, "! Returning an unconditional answer...\n");
	  }
#endif
/*----------------------------------------------------------------------*/
	  nlcp_trie_return(breg) = OldRetPtr; 
	  TrieRetPtr = get_next_trie_solution(&OldRetPtr);
          CallNumVar = *(bfreg+NLCPSIZE);
          op3 = bfreg + NLCPSIZE + CallNumVar;
          xtemp14 = hbreg;
	  hbreg = hreg;
	  load_solution_trie(CallNumVar,op3,TrieRetPtr);
	  lpcreg = cpreg;
	}
	else {
	  ebreg =  xtemp9; /* jf: I'm not sure if this is needed ... */
	  breg = nlcp_prevbreg(breg);
	  Fail1;
	}
	goto contcase;

/*----------------------------------------------------------------------*/

