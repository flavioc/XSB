/* File:      tables.c
** Author(s): Ernie Johnson
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


#include "configs/xsb_config.h"
#include "debugs/xsb_debug.h"

#include "debugs/debug_tables.h"
#include "debugs/debug_delay.h"

#include <stdio.h>
#include <stdlib.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "heap_xsb.h"
#include "memory_xsb.h"
#include "register.h"
#include "binding.h"
#include "psc_xsb.h"
#include "table_stats.h"
#include "trie_internals.h"
#include "macro_xsb.h"
#include "error_xsb.h"
#include "tables.h"
#include "flags_xsb.h"



/* Manager Structures
   ------------------ */
Structure_Manager smSF     = SM_InitDecl(struct subgoal_frame,
					 SUBGOAL_FRAMES_PER_BLOCK,
					 "Subgoal Frame");
Structure_Manager smALN = SM_InitDecl(AnsListNode, ALNs_PER_BLOCK,
				      "Answer List Node");

/*-------------------------------------------------------------------------*/

/*
 * Performs the Call Check/Insert operation.
 * Upon exit, CallLUR_VarVector(*results) points to the size of the
 * answer template on the CPS, unless a producer was found and we're
 * running in CHAT node.  In that case, this value is the same as
 * CallInfo_VarVector(*call_info), and the AT is sitting on the heap,   
 * its size pointed to by (hreg - 1).  See slginsts_xsb_i.h for answer
 * template layout.
 */

void table_call_search(TabledCallInfo *call_info, CallLookupResults *results) {

  if ( IsVariantPredicate(CallInfo_TableInfo(*call_info)) )
    variant_call_search(call_info,results);
  else
    subsumptive_call_search(call_info,results);
#ifdef CHAT
  if ( IsNULL(CallLUR_Subsumer(*results)) ) {
    /*
     * A New Producer: Move answer template from CPS to Heap.  The
     * arrangement of this vector is similar to that in the CPS: the
     * size of the vector is now at the high end, but the components
     * are still arranged from high mem (first) to low (last).
     */
    CPtr tmplt_component, tmplt_var_addr, h_addr;
    int size, j, attv_num, tmp;

    tmplt_component = CallLUR_VarVector(*results);
    tmp = int_val(*tmplt_component);
    get_var_and_attv_nums(size, attv_num, tmp);

    for ( j = size - 1, tmplt_component = tmplt_component + size;
	  j >= 0;
	  j--, tmplt_component-- ) {
      tmplt_var_addr = (CPtr)*tmplt_component;
      /* Now we are sure that tmplt_var_addr is a var on the heap */
      h_addr = hreg+j;
      bld_copy(h_addr, (Cell)(tmplt_var_addr));
    }
    hreg += size;
    bld_copy(hreg, cell(CallLUR_VarVector(*results)));
    hreg++;
    /* orig version in tries.c had VarPosReg pointing at Var_{m} */
    CallLUR_VarVector(*results) = CallLUR_VarVector(*results) + size + 1;
  }
#endif
}

/*-------------------------------------------------------------------------*/

/*
 * Template is a pointer to the first term in the vector, with the
 * elements arranged from high to low memory.
 */
BTNptr table_answer_search(SGFrame producer, int size, int attv_num,
			   CPtr template, bool *is_new) {

  BTNptr answer;

  if ( IsSubsumptivePredicate(subg_tif_ptr(producer)) ) {
    answer =
      (BTNptr)subsumptive_answer_search(size,template,producer,is_new);
    if ( *is_new ) {
      ALNptr newALN;

      subg_timestamp(producer)++;

      /* Put New Answer at End of Answer Chain
	 ------------------------------------- */
      New_ALN(newALN, answer, NULL);
      SF_AppendNewAnswer(producer,newALN);
    }
  }
  else {
    /*
     * We want to save the substitution factor of the answer in the
     * heap, so we have to change variant_answer_search().
     */
    bool wasFound = TRUE;

#ifndef IGNORE_DELAYVAR
    ans_var_pos_reg = hreg++;	/* Leave a cell for functor ret/n */
#endif /* IGNORE_DELAYVAR */

    answer = variant_answer_search(size,attv_num,template,producer,&wasFound);

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

    do_delay_stuff(answer, producer, wasFound);

#ifndef IGNORE_DELAYVAR
    undo_answer_bindings();
#endif /* IGNORE_DELAYVAR */

    *is_new = ! wasFound;
  }
  return ( answer );
}

/*-------------------------------------------------------------------------*/

void table_consume_answer(BTNptr answer, int size, int attv_num, CPtr template,
			  TIFptr pred) {

  if (size == 0) {
    if ( ! IsEscapeNode(answer) )
      xsb_abort("Size of answer template is 0 but producer contains an "
		"answer\nwith a non-empty substitution!\n");
  }
  else {
    if ( IsSubsumptivePredicate(pred) )
      consume_subsumptive_answer(answer,size,template);
    else
      /* this also tracks variables created during unification */
      load_solution_trie(size,attv_num,template,answer);
  }
}

/*-------------------------------------------------------------------------*/

/*
 *  Collects answers from a producer's answer table based on an answer
 *  template and a time stamp value.  Collection is performed to
 *  replenish the answer cache of a properly subsumed subgoal.
 *  Collected answers are placed into the consumer's cache and the time
 *  of this collection is noted.  Returns a pointer to the chain of new
 *  answers found, or NULL if no answers were found.
 */

ALNptr table_retrieve_answers(SGFrame prodSF, SGFrame consSF, CPtr template) {

  int size;
  TimeStamp ts;         /* for selecting answers from subsumer's AnsTbl */
  TSTNptr tstRoot;      /* TS answer trie root of subsumer */
  ALNptr answers;


#ifdef DEBUG
  if (consSF == prodSF)
    xsb_abort("Answer Retrieval triggered for a variant!");
#endif
  size = int_val(*template);
  template = template + size;
  ts = subg_timestamp(consSF);
  tstRoot = (TSTNptr)subg_ans_root_ptr(prodSF);
  answers = retrieve_unifying_answers(tstRoot,ts,size,template);
  subg_timestamp(consSF) = TSTN_TimeStamp(tstRoot);
  if ( IsNonNULL(answers) )
    SF_AppendNewAnswerList(consSF,answers);
  return answers;
}

/*-------------------------------------------------------------------------*/

/*
 *                     Table Structure Reclamation
 *                     ===========================
 */

/*
 *  Deallocate all the data structures which become superfluous once the
 *  table has completed.  Currently, this includes the answer list nodes
 *  and the TSIs from the producer, and the answer list nodes from the
 *  subsumed subgoals.  For the producers, the engine requires that the
 *  dummy answer-list node remain, and that its 'next' field be set to
 *  either the constant CON_ANSWERS or UNCOND_ANSWERS depending on whether
 *  there were any conditional answers in the answer list.  For the
 *  subsumed (consumer) subgoals, the entire answer list, including the
 *  dummy, is reclaimed.
 *
 *  For statistical purposes, we check whether the current usage of
 *  these incomplete-table structures are new maximums.
 *
 *  Currently, timestamps from the TSIs are copied back to the TSTNs.
 *  Although not necessary, this method has some advantages.  Foremost,
 *  this field will never contain garbage values, and so we avoid dangling
 *  pointer problems.  Also, maintaining the time stamp values is
 *  beneficial for post-completion analysis of the TSTs and
 *  characteristics of the query evaluation.
 *
 *  Multiple calls to this function is avoided by checking a flag in the
 *  subgoal frame.  See macro reclaim_incomplete_table_structs() which
 *  contains the only reference to this function.
 */

void table_complete_entry(SGFrame producerSF) {

  SGFrame pSF;
  ALNptr pRealAnsList, pALN, tag;
  TSTHTptr ht;
  EntryPtr tsi_entry;


#ifdef DEBUG_STRUCT_ALLOC
  extern void print_subgoal(FILE *, SGFrame);

  print_subgoal(stderr,producerSF);
  fprintf(stderr, " complete... reclaiming structures.\n");
#endif

  if (flags[TRACE_STA])
    compute_maximum_tablespace_stats();

  /* Reclaim Auxiliary Structures from the TST
     ----------------------------------------- */
  if ( ProducerHasConsumers(producerSF) &&
       IsNonNULL(subg_ans_root_ptr(producerSF)) )

    for ( ht = TSTRoot_GetHTList(subg_ans_root_ptr(producerSF));
	  IsNonNULL(ht);  ht = TSTHT_InternalLink(ht) ) {

      /*** Put timestamps back into TSTNs ***/
      for ( tsi_entry = TSTHT_HeadEntry(ht);  IsNonNULL(tsi_entry);
	    tsi_entry = Entry_Next(tsi_entry) )
	TSTN_TimeStamp(Entry_TSTNode(tsi_entry)) = Entry_TimeStamp(tsi_entry);

      /*** Return Entry chain to Structure Manager ***/
      if ( IsNULL(TSTHT_TailEntry(ht)) ||
	   IsNonNULL(Entry_Next(TSTHT_TailEntry(ht))) ||
	   IsNULL(TSTHT_HeadEntry(ht)) ||
	   IsNonNULL(Entry_Prev(TSTHT_HeadEntry(ht))) )
	xsb_warn("Malconstructed TSI");

#ifdef DEBUG_STRUCT_ALLOC
      fprintf(stderr, "  Reclaiming Entry chain\n");
      smPrint(smEntry, "  before chain reclamation");
#endif

      /*** Because 'prev' field is first, the tail becomes the list head ***/
      SM_DeallocateStructList(smEntry,TSTHT_TailEntry(ht),
			      TSTHT_HeadEntry(ht));
      TSTHT_HeadEntry(ht) = TSTHT_TailEntry(ht) = NULL;

#ifdef DEBUG_STRUCT_ALLOC
      smPrint(smEntry, "  after chain reclamation");
#endif
    }

  /* Reclaim Producer's Answer List
     ------------------------------ */
  if ( has_answers(producerSF) ) {
    pALN = pRealAnsList = subg_answers(producerSF);
    tag = UNCOND_ANSWERS;
    do {
      if ( is_conditional_answer(ALN_Answer(pALN)) ) {
	tag = COND_ANSWERS;
	break;
      }
      pALN = ALN_Next(pALN);
    } while ( IsNonNULL(pALN) );
    subg_answers(producerSF) = tag;

#ifdef DEBUG_STRUCT_ALLOC
      fprintf(stderr, "  Reclaiming ALN chain for subgoal\n");
      smPrint(smALN, "  before chain reclamation");
#endif

    if ( IsNULL(subg_ans_list_tail(producerSF)) ||
	 IsNonNULL(ALN_Next(subg_ans_list_tail(producerSF))) )
      xsb_abort("Answer-List exception: Tail pointer incorrectly maintained");
    SM_DeallocateStructList(smALN,pRealAnsList,subg_ans_list_tail(producerSF));
    subg_ans_list_tail(producerSF) = NULL;

#ifdef DEBUG_STRUCT_ALLOC
    smPrint(smALN, "  after chain reclamation");
#endif
  }

  /* Process its Consumers
     --------------------- */
  pSF = subg_consumers(producerSF);

#ifdef DEBUG_STRUCT_ALLOC
  if ( IsNonNULL(pSF) ) {
    fprintf(stderr, "Reclaiming structures from consumers of ");
    print_subgoal(stderr, producerSF);
    fprintf(stderr, "\n");
  }
#endif

  while ( IsNonNULL(pSF) ) {

#ifdef DEBUG_STRUCT_ALLOC
    fprintf(stderr, "  Reclaiming ALN chain for consumer\n");
    smPrint(smALN, "  before chain reclamation");
#endif

    if ( has_answers(pSF) )    /* real answers exist */
      SM_DeallocateStructList(smALN,subg_ans_list_ptr(pSF),
			      subg_ans_list_tail(pSF))
    else
      SM_DeallocateStruct(smALN,subg_ans_list_ptr(pSF))

#ifdef DEBUG_STRUCT_ALLOC
    smPrint(smALN, "  after chain reclamation");
#endif

    subg_ans_list_ptr(pSF) = subg_ans_list_tail(pSF) = NULL;
    pSF = subg_consumers(pSF);
  }

#ifdef DEBUG_STRUCT_ALLOC
  fprintf(stderr, "Subgoal structure-reclamation complete!\n");
#endif
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 * Frees all the tabling space resources.
 */

void release_all_tabling_resources() {

  SM_ReleaseResources(smTableBTN);
  TrieHT_FreeAllocatedBuckets(smTableBTHT);
  SM_ReleaseResources(smTableBTHT);
  SM_ReleaseResources(smTSTN);
  TrieHT_FreeAllocatedBuckets(smTSTHT);
  SM_ReleaseResources(smTSTHT);
  SM_ReleaseResources(smEntry);
  SM_ReleaseResources(smALN);
  SM_ReleaseResources(smSF);
}

/*-------------------------------------------------------------------------*/
