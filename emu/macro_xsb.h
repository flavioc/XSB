/* File:      macro_xsb.h
** Author(s): Swift, Sagonas, Rao, Freire, Johnson
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

#ifndef __MACRO_XSB_H__
#define __MACRO_XSB_H__

#include "debug_xsb.h"

/*----------------------------------------------------------------------*/

/* typedef struct subgoal_frame *VariantSF; */

/*===========================================================================*/

/*
 *                         Table Information Frame
 *                         =======================
 *
 *  Table Information Frames are created for each tabled predicate,
 *  allowing access to its calls and their associated answers.
 */

#include "table_status_defs.h"

typedef enum Tabled_Evaluation_Method {
  VARIANT_TEM      = VARIANT_EVAL_METHOD,
  SUBSUMPTIVE_TEM  = SUBSUMPTIVE_EVAL_METHOD
} TabledEvalMethod;

typedef struct Table_Info_Frame *TIFptr;
typedef struct Table_Info_Frame {
  Psc  psc_ptr;			/* pointer to the PSC record of the subgoal */
  TabledEvalMethod method;	/* eval pred using variant or subsumption? */
  BTNptr call_trie;		/* pointer to the root of the call trie */
  VariantSF subgoals;		/* chain of predicate's subgoals */
  TIFptr next_tif;		/* pointer to next table info frame */
} TableInfoFrame;

#define TIF_PSC(pTIF)		   ( (pTIF)->psc_ptr )
#define TIF_EvalMethod(pTIF)	   ( (pTIF)->method )
#define TIF_CallTrie(pTIF)	   ( (pTIF)->call_trie )
#define TIF_Subgoals(pTIF)	   ( (pTIF)->subgoals )
#define TIF_NextTIF(pTIF)	   ( (pTIF)->next_tif )


#define IsVariantPredicate(pTIF)		\
   ( TIF_EvalMethod(pTIF) == VARIANT_TEM )

#define IsSubsumptivePredicate(pTIF)		\
   ( TIF_EvalMethod(pTIF) == SUBSUMPTIVE_TEM )


struct tif_list {
  TIFptr first;
  TIFptr last;
};
extern struct tif_list  tif_list;

#define New_TIF(pTIF,pPSC) {						\
   pTIF = (TIFptr)mem_alloc(sizeof(TableInfoFrame));			\
   if ( IsNULL(pTIF) )							\
     xsb_abort("Ran out of memory in allocation of TableInfoFrame");	\
   TIF_PSC(pTIF) = pPSC;						\
   TIF_EvalMethod(pTIF) = (TabledEvalMethod)flags[TABLING_METHOD];	\
   TIF_CallTrie(pTIF) = NULL;						\
   TIF_Subgoals(pTIF) = NULL;						\
   TIF_NextTIF(pTIF) = NULL;						\
   if ( IsNonNULL(tif_list.last) )					\
     TIF_NextTIF(tif_list.last) = pTIF;					\
   else									\
     tif_list.first = pTIF;						\
   tif_list.last = pTIF;						\
 }

#define Free_TIF(pTIF) {						\
  TIFptr tTIF = tif_list.first;						\
  if (tTIF == (pTIF)) {							\
    tif_list.first = TIF_NextTIF((pTIF)); 				\
    if (tif_list.last == (pTIF)) tif_list.last = NULL;			\
  }									\
  else {								\
    while (tTIF != NULL && TIF_NextTIF(tTIF) != (pTIF))		     	\
      tTIF = TIF_NextTIF(tTIF);						\
    if (!tTIF) xsb_exit("Trying to free nonexistent TIF");		\
    if ((pTIF) == tif_list.last) tif_list.last = tTIF;			\
    TIF_NextTIF(tTIF) = TIF_NextTIF((pTIF));				\
  }									\
  delete_predicate_table(CTXTc pTIF);					\
  mem_dealloc((pTIF),sizeof(TableInfoFrame));				\
}

/*===========================================================================*/

typedef struct ascc_edge *EPtr;
typedef struct completion_stack_frame *ComplStackFrame;

/*----------------------------------------------------------------------*/
/*  Approximate Strongly Connected Component Edge Structure.		*/
/*----------------------------------------------------------------------*/

struct ascc_edge {
  ComplStackFrame ascc_node_ptr;
  EPtr next;
};

#define ASCC_EDGE_SIZE		(sizeof(struct ascc_edge)/sizeof(CPtr))

#define edge_to_node(e)		((EPtr)(e))->ascc_node_ptr
#define next_edge(e)		((EPtr)(e))->next

/*----------------------------------------------------------------------*/
/*  Completion Stack Structure (ASCC node structure).			*/
/*									*/
/*  NOTE: Please make sure that fields "DG_edges" and "DGT_edges" are	*/
/*	  the last fields of the structure, and each time you modify	*/
/*	  this structure you also update the definition of the		*/
/*	  "compl_stk_frame_field" array defined in file debug.c		*/
/*----------------------------------------------------------------------*/

#define DELAYED		-1

/*----------------------------------------------------------------------*/

struct completion_stack_frame {
  VariantSF subgoal_ptr;
  int     _level_num;
  ALNptr  del_ret_list;   /* to reclaim deleted returns */
  int     visited;
#ifndef LOCAL_EVAL
  EPtr    DG_edges;
  EPtr    DGT_edges;
#endif
} ;

#define COMPLFRAMESIZE	(sizeof(struct completion_stack_frame)/sizeof(CPtr))

#define compl_subgoal_ptr(b)	((ComplStackFrame)(b))->subgoal_ptr
#define compl_level(b)		((ComplStackFrame)(b))->_level_num
#define compl_del_ret_list(b)	((ComplStackFrame)(b))->del_ret_list
#define compl_visited(b)	((ComplStackFrame)(b))->visited
#ifndef LOCAL_EVAL
#define compl_DG_edges(b)	((ComplStackFrame)(b))->DG_edges
#define compl_DGT_edges(b)	((ComplStackFrame)(b))->DGT_edges
#endif

#define prev_compl_frame(b)	(((CPtr)(b))+COMPLFRAMESIZE)
#define next_compl_frame(b)	(((CPtr)(b))-COMPLFRAMESIZE)


#define adjust_level(CS_FRAME) {					\
  int new_level = compl_level(CS_FRAME);				\
  if ( new_level < compl_level(openreg) ) {				\
    CPtr csf = CS_FRAME;						\
    while ( (compl_level(csf) >= new_level) && (csf >= openreg) ) {	\
      compl_level(csf) = new_level;					\
      csf = next_compl_frame(csf);					\
    }									\
  }									\
}


/*
 *  The overflow test MUST be placed after the initialization of the
 *  ComplStackFrame in the current implementation.  This is so that the
 *  corresponding subgoal which points to this frame can be found and its
 *  link can be updated if an expansion is required.  This was the simplest
 *  solution to not leaving any dangling pointers to the old area.
 */

#define push_completion_frame_common(subgoal) \
  level_num++; \
  openreg -= COMPLFRAMESIZE; \
  compl_subgoal_ptr(openreg) = subgoal; \
  compl_level(openreg) = level_num; \
  compl_del_ret_list(openreg) = NULL; \
  compl_visited(openreg) = FALSE

#define push_completion_frame_batched(subgoal) \
  compl_DG_edges(openreg) = compl_DGT_edges(openreg) = NULL

#ifdef LOCAL_EVAL
#define	push_completion_frame(subgoal)	\
  push_completion_frame_common(subgoal); \
  check_completion_stack_overflow
#else
#define	push_completion_frame(subgoal)	\
  push_completion_frame_common(subgoal); \
  push_completion_frame_batched(subgoal); \
  check_completion_stack_overflow
#endif

#if (!defined(LOCAL_EVAL))
#define compact_completion_frame(cp_frame,cs_frame,subgoal)	\
  compl_subgoal_ptr(cp_frame) = subgoal;			\
  compl_level(cp_frame) = compl_level(cs_frame);		\
  compl_visited(cp_frame) = FALSE;				\
  compl_DG_edges(cp_frame) = compl_DGT_edges(cp_frame) = NULL;  \
  cp_frame = next_compl_frame(cp_frame)
#endif

/*----------------------------------------------------------------------*/
/*  Subgoal (Call) Structure.						*/
/*----------------------------------------------------------------------*/

#include "slgdelay.h"

/*----------------------------------------------------------------------*/

enum SubgoalFrameType {
  VARIANT_PRODUCER_SFT, SUBSUMPTIVE_PRODUCER_SFT, SUBSUMED_CONSUMER_SFT
};

/* Variant (Producer) Subgoal Frame
   -------------------------------- */
typedef struct subgoal_frame {
  byte sf_type;		  /* The type of subgoal frame */
  byte is_complete;	  /* If producer, whether its answer set is complete */
  byte is_reclaimed;	  /* Whether structs for supporting answer res from an
			     incomplete table have been reclaimed */
  TIFptr tif_ptr;	  /* Table of which this call is a part */
  BTNptr leaf_ptr;	  /* Handle for call in the CallTrie */
  BTNptr ans_root_ptr;	  /* Root of the return trie */
  ALNptr ans_list_ptr;	  /* Pointer to the list of returns in the ret trie */
  ALNptr ans_list_tail;	  /* pointer to the tail of the answer list */
  void *next_subgoal;	  
  void *prev_subgoal;
  CPtr  cp_ptr;		  /* Pointer to the Generator CP */
  CPtr asf_list_ptr;	  /* Pointer to list of (CP) active subgoal frames */
  CPtr compl_stack_ptr;	  /* Pointer to subgoal's completion stack frame */
  CPtr compl_suspens_ptr; /* SLGWAM: CP stack ptr */
  PNDE nde_list;	  /* pointer to a list of negative DEs */
#ifdef MULTI_THREAD
  int tid;		  /* Thread id of the generator thread for this sg */
  byte grabbed; 	  /* Subgoal is marked to be computed for leader in
			     deadlock detection */
#endif
} variant_subgoal_frame;

#define subg_sf_type(b)		((VariantSF)(b))->sf_type
#define subg_is_complete(b)	((VariantSF)(b))->is_complete
#define subg_is_reclaimed(b)	((VariantSF)(b))->is_reclaimed
#define subg_prev_subgoal(b)	((VariantSF)(b))->prev_subgoal
#define subg_next_subgoal(b)	((VariantSF)(b))->next_subgoal
#define subg_tif_ptr(b)		((VariantSF)(b))->tif_ptr
#define subg_leaf_ptr(b)	((VariantSF)(b))->leaf_ptr
#define subg_ans_root_ptr(b)	((VariantSF)(b))->ans_root_ptr
#define subg_ans_list_ptr(b)	((VariantSF)(b))->ans_list_ptr
#define subg_ans_list_tail(b)	((VariantSF)(b))->ans_list_tail
#define subg_cp_ptr(b)		((VariantSF)(b))->cp_ptr
#define subg_asf_list_ptr(b)	((VariantSF)(b))->asf_list_ptr

/* use this for mark as completed == 0 */
#define subg_compl_stack_ptr(b)	((VariantSF)(b))->compl_stack_ptr
#define subg_compl_susp_ptr(b)	((VariantSF)(b))->compl_suspens_ptr
#define subg_nde_list(b)	((VariantSF)(b))->nde_list

#define subg_tid(b)		((VariantSF)(b))->tid
#define subg_grabbed(b)		((VariantSF)(b))->grabbed

/* Subsumptive Producer Subgoal Frame
   ---------------------------------- */
typedef struct SubsumedConsumerSubgoalFrame *SubConsSF;
typedef struct SubsumptiveProducerSubgoalFrame *SubProdSF;
typedef struct SubsumptiveProducerSubgoalFrame {
  variant_subgoal_frame  var_sf;
  SubConsSF  consumers;		/* List of properly subsumed subgoals which
				   consume from a producer's answer set */
} subsumptive_producer_sf;

#define subg_consumers(SF)	((SubProdSF)(SF))->consumers


/* Subsumed Consumer Subgoal Frame
 * -------------------------------
 *  Position of shared fields MUST correspond to that of variant_subgoal_frame.
 */
typedef struct SubsumedConsumerSubgoalFrame {
  byte sf_type;		   /* The type of subgoal frame */
  byte junk[2];
  TIFptr tif_ptr;	   /* Table of which this call is a part */
  BTNptr leaf_ptr;	   /* Handle for call in the CallTrie */
  SubProdSF producer;	   /* The subgoal frame from whose answer set answers
			      are collected into the answer list */
  ALNptr ans_list_ptr;	   /* Pointer to the list of returns in the ret trie */
  ALNptr ans_list_tail;	   /* Pointer to the tail of the answer list */
  TimeStamp ts;		   /* Time stamp to use during next answer ident */
  SubConsSF consumers;	   /* Chain link for properly subsumed subgoals */
} subsumptive_consumer_sf;

#define conssf_producer(SF)	((SubConsSF)(SF))->producer
#define conssf_timestamp(SF)	((SubConsSF)(SF))->ts
#define conssf_consumers(SF)	((SubConsSF)(SF))->consumers


/* beginning of REAL answers in the answer list */
#define subg_answers(subg)	ALN_Next(subg_ans_list_ptr(subg))


#define IsVariantSF(pSF)      (subg_sf_type(pSF) == VARIANT_PRODUCER_SFT)
#define IsSubProdSF(pSF)      (subg_sf_type(pSF) == SUBSUMPTIVE_PRODUCER_SFT)
#define IsSubConsSF(pSF)      (subg_sf_type(pSF) == SUBSUMED_CONSUMER_SFT)

#define IsVariantProducer(pSF)		IsVariantSF(pSF)
#define IsSubsumptiveProducer(pSF)	IsSubProdSF(pSF)
#define IsProperlySubsumed(pSF)		IsSubConsSF(pSF)

#define IsProducingSubgoal(pSF)		\
   ( IsVariantProducer(pSF) || IsSubsumptiveProducer(pSF) )

#define ProducerSubsumesSubgoals(pSF)	\
   ( IsSubsumptiveProducer(pSF) && IsNonNULL(subg_consumers(pSF)) )


/* Doubly-linked lists of Producer Subgoal Frames
 * ----------------------------------------------
 * Manipulating a doubly-linked list maintained through fields
 * `prev' and `next'.
 */

#define subg_dll_add_sf(pSF,Chain,NewChain) {	\
   subg_prev_subgoal(pSF) = NULL;		\
   subg_next_subgoal(pSF) = Chain;		\
   if ( IsNonNULL(Chain) )			\
     subg_prev_subgoal(Chain) = pSF;		\
   NewChain = (VariantSF)pSF;				\
 }

#define subg_dll_remove_sf(pSF,Chain,NewChain) {			 \
   if ( IsNonNULL(subg_prev_subgoal(pSF)) ) {				 \
     subg_next_subgoal(subg_prev_subgoal(pSF)) = subg_next_subgoal(pSF); \
     NewChain = Chain;							 \
   }									 \
   else									 \
     NewChain = (VariantSF)subg_next_subgoal(pSF);			 \
   if ( IsNonNULL(subg_next_subgoal(pSF)) )				 \
     subg_prev_subgoal(subg_next_subgoal(pSF)) = subg_prev_subgoal(pSF); \
   subg_prev_subgoal(pSF) = subg_next_subgoal(pSF) = NULL;		 \
 }


extern ALNptr empty_return(void);


/* Appending to the Answer List of a SF
   ------------------------------------ */
#define SF_AppendNewAnswerList(pSF,pAnsList) {	\
						\
   ALNptr pLast;				\
						\
   pLast = pAnsList;				\
   while ( IsNonNULL(ALN_Next(pLast)) )		\
     pLast = ALN_Next(pLast);			\
   SF_AppendToAnswerList(pSF,pAnsList,pLast);	\
 }

#define SF_AppendNewAnswer(pSF,pAns)	SF_AppendToAnswerList(pSF,pAns,pAns)

#define SF_AppendToAnswerList(pSF,pHead,pTail) {			\
   if ( has_answers(pSF) )						\
     /*
      *  Insert new answer at the end of the answer list.
      */								\
     ALN_Next(subg_ans_list_tail(pSF)) = pHead; 			\
   else									\
     /*
      * The dummy answer list node is the only node currently in the list.
      * It's pointed to by the head ptr, but the tail ptr is NULL.
      */								\
     ALN_Next(subg_ans_list_ptr(pSF)) = pHead;				\
   subg_ans_list_tail(pSF) = pTail;					\
 }


/* Global Structure Management
   --------------------------- */
#define SUBGOAL_FRAMES_PER_BLOCK    16

extern struct Structure_Manager smVarSF;
extern struct Structure_Manager smProdSF;
extern struct Structure_Manager smConsSF;


/* Subgoal Frame (De)Allocation
   ---------------------------- */
/*
 * Allocates and initializes a subgoal frame for a producing subgoal.
 * The TIP field is initialized, fields of the call trie leaf and this
 * subgoal are set to point to one another, while the completion stack
 * frame pointer is set to the next available location (frame) on the
 * stack (but the space is not yet allocated from the stack).  Also, an
 * answer-list node is allocated for pointing to a dummy answer node and
 * inserted into the answer list.  Note that answer sets (answer tries,
 * roots) are lazily created -- not until an answer is generated.
 * Therefore this field may potentially be NULL, as it is initialized
 * here.  memset() is used so that the remaining fields are initialized
 * to 0/NULL so, in some sense making this macro independent of the
 * number of fields.
 */

#define NewProducerSF(SF,Leaf,TableInfo) {				    \
									    \
   void *pNewSF;							    \
									    \
   if ( IsVariantPredicate(TableInfo) ) {				    \
     SM_AllocateStruct(smVarSF,pNewSF);					    \
     pNewSF = memset(pNewSF,0,sizeof(variant_subgoal_frame));		    \
     subg_sf_type(pNewSF) = VARIANT_PRODUCER_SFT;			    \
   }									    \
   else {								    \
     SM_AllocateStruct(smProdSF,pNewSF);				    \
     pNewSF = memset(pNewSF,0,sizeof(subsumptive_producer_sf));		    \
     subg_sf_type(pNewSF) = SUBSUMPTIVE_PRODUCER_SFT;			    \
   }									    \
   subg_tif_ptr(pNewSF) = TableInfo;					    \
   subg_dll_add_sf(pNewSF,TIF_Subgoals(TableInfo),TIF_Subgoals(TableInfo)); \
   subg_leaf_ptr(pNewSF) = Leaf;					    \
   CallTrieLeaf_SetSF(Leaf,pNewSF);					    \
   subg_ans_list_ptr(pNewSF) = empty_return();				    \
   subg_compl_stack_ptr(pNewSF) = openreg - COMPLFRAMESIZE;		    \
   SF = (VariantSF)pNewSF;						    \
}

#define FreeProducerSF(SF) {					\
   subg_dll_remove_sf(SF,TIF_Subgoals(subg_tif_ptr(SF)),	\
		      TIF_Subgoals(subg_tif_ptr(SF)));		\
   if ( IsVariantSF(SF) )					\
     SM_DeallocateStruct(smVarSF,SF)				\
   else								\
     SM_DeallocateStruct(smProdSF,SF)				\
 }


/*
 *  Allocates and initializes a subgoal frame for a consuming subgoal: a
 *  properly subsumed call consuming from an incomplete producer.
 *  Consuming subgoals are NOT inserted into the subgoal chain
 *  maintained by the TIF, but instead are maintained by the producer in
 *  a private linked list.  Many fields of a consumer SF are left blank
 *  since it won't be used in the same way as those for producers.  Its
 *  main purpose is to maintain the answer list and the call form.  Just
 *  as for the producer, an answer-list node is allocated for pointing
 *  to a dummy answer node and inserted into the answer list.
 *
 *  Finally, some housekeeping is needed to support lazy creation of the
 *  auxiliary structures in the producer's answer TST.  If this is the
 *  first consumer for this producer, then create these auxiliary
 *  structures.
 */

void tstCreateTSIs(TSTNptr);

#define NewSubConsSF(SF,Leaf,TableInfo,Producer) {		\
								\
   void *pNewSF;						\
								\
   SM_AllocateStruct(smConsSF,pNewSF);				\
   pNewSF = memset(pNewSF,0,sizeof(subsumptive_consumer_sf));	\
   subg_sf_type(pNewSF) = SUBSUMED_CONSUMER_SFT;		\
   subg_tif_ptr(pNewSF) = TableInfo;				\
   subg_leaf_ptr(pNewSF) = Leaf;				\
   CallTrieLeaf_SetSF(Leaf,pNewSF);				\
   conssf_producer(pNewSF) = (SubProdSF)Producer;		\
   if ( ! ProducerSubsumesSubgoals(Producer) )			\
     tstCreateTSIs((TSTNptr)subg_ans_root_ptr(Producer));	\
   subg_ans_list_ptr(pNewSF) = empty_return();			\
   conssf_timestamp(pNewSF) = CONSUMER_SF_INITIAL_TS;		\
   conssf_consumers(pNewSF) = subg_consumers(Producer);		\
   subg_consumers(Producer) = (SubConsSF)pNewSF;		\
   SF = (VariantSF)pNewSF;					\
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#define set_min(a,b,c)	if (b < c) a = b; else a = c

#define tab_level(SUBG_PTR)     \
        compl_level((subg_compl_stack_ptr(SUBG_PTR)))
#define next_tab_level(CSF_PTR) \
        compl_level(prev_compl_frame(CSF_PTR))

#define is_leader(CSF_PTR)	\
	(next_tab_level(CSF_PTR) < compl_level(CSF_PTR))

/*----------------------------------------------------------------------*/
/* Codes for completed subgoals (assigned to subg_answers)              */
/*----------------------------------------------------------------------*/

#define NO_ANSWERS	(ALNptr)0
#define UNCOND_ANSWERS	(ALNptr)1
#define COND_ANSWERS	(ALNptr)2

/*----------------------------------------------------------------------*/
/* The following 2 macros are to be used for incomplete subgoals.	*/
/*----------------------------------------------------------------------*/

#define has_answers(SUBG_PTR)	    IsNonNULL(subg_answers(SUBG_PTR))
#define has_no_answers(SUBG_PTR)    IsNULL(subg_answers(SUBG_PTR))

/*----------------------------------------------------------------------*/
/* The following 5 macros should be used only for completed subgoals.	*/
/*----------------------------------------------------------------------*/

/*
 * These defs depend on when the root of an answer trie is created.
 * Currently, this is when the first answer is added to the set.  They
 * are also dependent upon representation of the truth of ground goals.
 * Currently, true subgoals have an ESCAPE node placed below the root,
 * while false goals have no root nor leaves (since no answer was ever
 * inserted, there was no opportunity to create a root).
 */
#define has_answer_code(SUBG_PTR)				\
	( IsNonNULL(subg_ans_root_ptr(SUBG_PTR)) &&		\
	  IsNonNULL(BTN_Child(subg_ans_root_ptr(SUBG_PTR))) )

#define subgoal_fails(SUBG_PTR)			\
	( ! has_answer_code(SUBG_PTR) )

/* should only be used on ground subgoals (is for escape node inspection) */
#define subgoal_unconditionally_succeeds(SUBG_PTR)			    \
        ( has_answer_code(SUBG_PTR) &&					    \
	  is_unconditional_answer(BTN_Child(subg_ans_root_ptr(SUBG_PTR))) )

#define mark_subgoal_failed(SUBG_PTR)	\
	(subg_ans_root_ptr(SUBG_PTR) = NULL)

#define neg_simplif_possible(SUBG_PTR)	\
	((subgoal_fails(SUBG_PTR)) && (subg_nde_list(SUBG_PTR) != NULL))

/*----------------------------------------------------------------------*/

#define is_completed(SUBG_PTR)		subg_is_complete(SUBG_PTR)

#define structs_are_reclaimed(SUBG_PTR) subg_is_reclaimed(SUBG_PTR)
   
#define mark_as_completed(SUBG_PTR) {		\
          subg_is_complete(SUBG_PTR) = TRUE;	\
          reclaim_del_ret_list(SUBG_PTR);	\
        }

#define subgoal_space_has_been_reclaimed(SUBG_PTR,CS_FRAME) \
        (SUBG_PTR != compl_subgoal_ptr(CS_FRAME))

#define mark_delayed(csf1, csf2, susp) { \
	  compl_visited(csf1) = DELAYED; \
	  compl_visited(csf2) = DELAYED; \
  /* do not put TRUE but some !0 value that GC can recognize as int tagged */\
	  csf_neg_loop(susp) = XSB_INT; \
        }

/*----------------------------------------------------------------------*/
/* The following macro might be called more than once for some subgoal. */
/* So, please make sure that functions/macros that it calls are robust  */
/* under repeated uses.                                 - Kostis.       */
/* A new Subgoal Frame flag prevents multiple calls.	- Ernie		*/
/*----------------------------------------------------------------------*/

#define reclaim_incomplete_table_structs(SUBG_PTR) {	\
   if ( ! structs_are_reclaimed(SUBG_PTR) ) {		\
     table_complete_entry(SUBG_PTR);			\
     structs_are_reclaimed(SUBG_PTR) = TRUE;		\
   }							\
 }

/*----------------------------------------------------------------------*/
#ifdef DEMAND
#define Reset_Demand_Freeze_Registers \
    bdfreg = bfreg; \
    trdfreg = trfreg; \
    hdfreg = hfreg; \
    edfreg = efreg
#else
#define Reset_Demand_Freeze_Registers 
#endif

#define reset_freeze_registers \
    bfreg = (CPtr)(tcpstack.high) - CP_SIZE; \
    trfreg = (CPtr *)(tcpstack.low); \
    hfreg = (CPtr)(glstack.low); \
    efreg = (CPtr)(glstack.high) - 1; \
    level_num = xwammode = 0; \
    root_address = ptcpreg = NULL; \
    Reset_Demand_Freeze_Registers

#define adjust_freeze_registers(tcp) \
    if (bfreg < tcp_bfreg(tcp)) { bfreg = tcp_bfreg(tcp); }	 \
    if (trfreg > tcp_trfreg(tcp)) { trfreg = tcp_trfreg(tcp); }\
    if (hfreg > tcp_hfreg(tcp)) { hfreg = tcp_hfreg(tcp); }	 \
    if (efreg < tcp_efreg(tcp)) { efreg = tcp_efreg(tcp); }

#define reclaim_stacks(tcp) \
  if (tcp == root_address) { \
    reset_freeze_registers; \
    /* xsb_dbgmsg("reset registers...."); */ \
  } \
  else { \
    adjust_freeze_registers(tcp); \
    /* xsb_dbgmsg(adjust registers...."); */ \
  }

/*----------------------------------------------------------------------*/

#define pdlpush(cell)	*(pdlreg) = cell;  pdlreg--

#define pdlpop		*(++pdlreg)

#define pdlempty	(pdlreg == (CPtr)(pdl.high) - 1)

#define resetpdl \
   if (pdlreg < (CPtr) pdl.low) \
     xsb_exit("pdlreg grew too much"); \
   else (pdlreg = (CPtr)(pdl.high) - 1)

#define remove_incomplete_tables_loop(Endpoint) remove_incomplete_tries(Endpoint)

#define remove_incomplete_tables() remove_incomplete_tries(CTXTc COMPLSTACKBOTTOM)

/*----------------------------------------------------------------------*/

#define get_var_and_attv_nums(var_num, attv_num, tmp_int)	\
  var_num = tmp_int & 0xffff;					\
  attv_num = tmp_int >> 16

/*----------------------------------------------------------------------*/

#endif /* __MACRO_XSB_H__ */
