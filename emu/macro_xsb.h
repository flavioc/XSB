/* File:      macro_xsb.h
** Author(s): Swift, Sagonas, Rao, Freire
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


/*===========================================================================*/

/*
 *                         Table Information Frame
 *                         =======================
 *
 *  Table Information Frames are created for each tabled predicate,
 *  allowing access to its calls and their associated answers.
 */

#include "flags_xsb.h"

typedef enum Tabled_Evaluation_Method {
  VARIANT_TEM      = VARIANT_EVAL_METHOD,
  SUBSUMPTIVE_TEM  = SUBSUMPTIVE_EVAL_METHOD
} TabledEvalMethod;

typedef struct Table_Info_Frame *TIFptr;
typedef struct Table_Info_Frame {
  TIFptr next_tif;	/* pointer to next table info frame */
  BTNptr call_trie;	/* pointer to the root of the call trie */
  Psc  psc_ptr;		/* pointer to the PSC record of the subgoal */
  TabledEvalMethod method;
} TableInfoFrame;

#define TIF_NextTIF(pTIF)	   ( (pTIF)->next_tif )
#define TIF_CallTrie(pTIF)	   ( (pTIF)->call_trie )
#define TIF_PSC(pTIF)		   ( (pTIF)->psc_ptr )
#define TIF_EvalMethod(pTIF)	   ( (pTIF)->method )

#define IsVariantPredicate(pTIF)		\
   ( TIF_EvalMethod(pTIF) == VARIANT_TEM )

#define IsSubsumptivePredicate(pTIF)		\
   ( TIF_EvalMethod(pTIF) == SUBSUMPTIVE_TEM )


#define New_TIF(pTIF,pPSC) {					     \
   pTIF = malloc(sizeof(TableInfoFrame));			     \
   if ( IsNULL(pTIF) )						     \
     xsb_abort("Ran out of memory in allocation of TableInfoFrame"); \
   TIF_NextTIF(pTIF) = NULL;					     \
   TIF_CallTrie(pTIF) = NULL;					     \
   TIF_PSC(pTIF) = pPSC;					     \
   TIF_EvalMethod(pTIF) = flags[TABLING_METHOD];		     \
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

typedef struct subgoal_frame *SGFrame;

/*----------------------------------------------------------------------*/

struct completion_stack_frame {
  SGFrame subgoal_ptr;
  int     level_num;
#ifdef CHAT
  CPtr    hreg;	          /* for accessing the substitution factor */
  CPtr    pdreg;
  CPtr    ptcp;
  CPtr    cons_copy_list; /* Pointer to a list of consumer copy frames */
#endif
  ALNptr  del_ret_list;   /* to reclaim deleted returns */
  int     visited;
#ifndef LOCAL_EVAL
  EPtr    DG_edges;
  EPtr    DGT_edges;
#endif
} ;

#define COMPLFRAMESIZE	(sizeof(struct completion_stack_frame)/sizeof(CPtr))

#define compl_subgoal_ptr(b)	((ComplStackFrame)(b))->subgoal_ptr
#define compl_level(b)		((ComplStackFrame)(b))->level_num
#ifdef CHAT
#define compl_hreg(b)		((ComplStackFrame)(b))->hreg
#define compl_pdreg(b)		((ComplStackFrame)(b))->pdreg
#define compl_ptcp(b)		((ComplStackFrame)(b))->ptcp
#define compl_cons_copy_list(b)	((ComplStackFrame)(b))->cons_copy_list
#endif
#define compl_del_ret_list(b)	((ComplStackFrame)(b))->del_ret_list
#define compl_visited(b)	((ComplStackFrame)(b))->visited
#ifndef LOCAL_EVAL
#define compl_DG_edges(b)	((ComplStackFrame)(b))->DG_edges
#define compl_DGT_edges(b)	((ComplStackFrame)(b))->DGT_edges
#endif

#define prev_compl_frame(b)	(((CPtr)(b))+COMPLFRAMESIZE)
#define next_compl_frame(b)	(((CPtr)(b))-COMPLFRAMESIZE)

/*
 *  The overflow test MUST be placed after the initialization of the
 *  ComplStackFrame in the current implementation.  This is so that the
 *  corresponding subgoal which points to this frame can be found and its
 *  link can be updated if an expansion is required.  This was the simplest
 *  solution to not leaving any dangling pointers to the old area.
 */
#ifdef CHAT
#ifdef LOCAL_EVAL
#define	push_completion_frame(subgoal)	\
  level_num++; \
  openreg -= COMPLFRAMESIZE; \
  compl_subgoal_ptr(openreg) = subgoal; \
  compl_level(openreg) = level_num; \
  compl_hreg(openreg) = hreg - 1; /* so that it points to something useful */ \
  compl_pdreg(openreg) = delayreg; \
  compl_ptcp(openreg) = ptcpreg; \
  compl_cons_copy_list(openreg) = NULL; \
  compl_del_ret_list(openreg) = NULL; \
  compl_visited(openreg) = FALSE; \
  check_completion_stack_overflow
#else
#define	push_completion_frame(subgoal)	\
  level_num++; \
  openreg -= COMPLFRAMESIZE; \
  compl_subgoal_ptr(openreg) = subgoal; \
  compl_level(openreg) = level_num; \
  compl_hreg(openreg) = hreg - 1; /* so that it points to something useful */ \
  compl_pdreg(openreg) = delayreg; \
  compl_ptcp(openreg) = ptcpreg; \
  compl_cons_copy_list(openreg) = NULL; \
  compl_del_ret_list(openreg) = NULL; \
  compl_visited(openreg) = FALSE; \
  compl_DG_edges(openreg) = compl_DGT_edges(openreg) = NULL; \
  check_completion_stack_overflow
#endif
#else  /* Regular SLG-WAM */
#ifdef LOCAL_EVAL
#define	push_completion_frame(subgoal)	\
  level_num++; \
  openreg -= COMPLFRAMESIZE; \
  compl_subgoal_ptr(openreg) = subgoal; \
  compl_level(openreg) = level_num; \
  compl_del_ret_list(openreg) = NULL; \
  compl_visited(openreg) = FALSE; \
  check_completion_stack_overflow
#else
#define	push_completion_frame(subgoal)	\
  level_num++; \
  openreg -= COMPLFRAMESIZE; \
  compl_subgoal_ptr(openreg) = subgoal; \
  compl_level(openreg) = level_num; \
  compl_del_ret_list(openreg) = NULL; \
  compl_visited(openreg) = FALSE; \
  compl_DG_edges(openreg) = compl_DGT_edges(openreg) = NULL; \
  check_completion_stack_overflow
#endif
#endif

#if (!defined(LOCAL_EVAL))
#if (defined(CHAT))
#define compact_completion_frame(cp_frame,cs_frame,subgoal)	\
  compl_subgoal_ptr(cp_frame) = subgoal;			\
  compl_level(cp_frame) = compl_level(cs_frame);		\
  compl_hreg(cp_frame) = compl_hreg(cs_frame);			\
  compl_pdreg(cp_frame) = compl_pdreg(cs_frame);	       	\
  compl_ptcp(cp_frame) = compl_ptcp(cs_frame);			\
  compl_cons_copy_list(cp_frame) = compl_cons_copy_list(cs_frame); \
  compl_del_ret_list(cp_frame) = compl_del_ret_list(cs_frame);	\
  compl_visited(cp_frame) = FALSE;				\
  compl_DG_edges(cp_frame) = compl_DGT_edges(cp_frame) = NULL;  \
  cp_frame = next_compl_frame(cp_frame)
#else
#define compact_completion_frame(cp_frame,cs_frame,subgoal)	\
  compl_subgoal_ptr(cp_frame) = subgoal;			\
  compl_level(cp_frame) = compl_level(cs_frame);		\
  compl_visited(cp_frame) = FALSE;				\
  compl_DG_edges(cp_frame) = compl_DGT_edges(cp_frame) = NULL;  \
  cp_frame = next_compl_frame(cp_frame)
#endif
#endif

/*----------------------------------------------------------------------*/
/*  Subgoal (Call) Structure.						*/
/*----------------------------------------------------------------------*/

#include "slgdelay.h"

/*----------------------------------------------------------------------*/

     /* should not change the order - unless tables.P is updated accordingly
      * also, if adding fields, add to the end! 
      */
struct subgoal_frame {
  SGFrame next_subgoal;
  BTNptr ans_root_ptr;	/* Root of the return trie */
#if (!defined(CHAT))
  CPtr asf_list_ptr;	/* Pointer to list of (CP) active subgoal frames */
#endif
  TIFptr tif_ptr;	/* Used only in remove_open_tries */
  CPtr compl_stack_ptr;	/* Pointer to subgoal's completion stack frame */
#ifdef CHAT
  CPtr compl_suspens_ptr; /* pointer to CHAT area; type is chat_init_pheader */
#else
  CPtr compl_suspens_ptr; /* CP Stack ptr */
#endif
  ALNptr ans_list_ptr;	/* Pointer to the list of returns in the ret trie */
  SGFrame prev_subgoal;
  BTNptr leaf_ptr;	/* Used only in remove_open_tries */
  CPtr  cp_ptr;         /* Pointer to the Generator CP */
  ALNptr ans_list_tail; /* pointer to the tail of the answer list */
  CPtr compl_flag;      /* indicates whether subgoal is completed */
  PNDE nde_list;	/* pointer to a list of negative DEs */
  TimeStamp ts;		/* Producer: timestamp to be given to next new answer
			   Consumer: TS to use in next answer retrieval */
  SGFrame producer;     /* the subgoal frame from whose answer table answers
			   are collected into the answer list */
  SGFrame consumers;    /* Producer: list of subgoal frames which consume from
			     its answer table
			   Consumer: link in this chain */
  bool reclaimed_structs;   /* whether tabling structures associated with an
			       incomplete table have already been reclaimed;
			       could be folded into a single word with
			       compl_flag */
};

#define CALLSTRUCTSIZE	(sizeof(struct subgoal_frame)/sizeof(CPtr))

#define subg_next_subgoal(b)	((SGFrame)(b))->next_subgoal
#define subg_prev_subgoal(b)	((SGFrame)(b))->prev_subgoal
#define subg_ans_root_ptr(b)	((SGFrame)(b))->ans_root_ptr
#if (!defined(CHAT))
#define subg_asf_list_ptr(b)	((SGFrame)(b))->asf_list_ptr
#endif
#define subg_tif_ptr(b)		((SGFrame)(b))->tif_ptr
#define subg_leaf_ptr(b)	((SGFrame)(b))->leaf_ptr
/* use this for mark as completed == 0 */
#define subg_compl_stack_ptr(b)	((SGFrame)(b))->compl_stack_ptr
#define subg_compl_susp_ptr(b)	((SGFrame)(b))->compl_suspens_ptr
#define subg_ans_list_ptr(b)	((SGFrame)(b))->ans_list_ptr
#define subg_cp_ptr(b)		((SGFrame)(b))->cp_ptr
#define subg_ans_list_tail(b)	((SGFrame)(b))->ans_list_tail
#define subg_compl_flag(b)	((SGFrame)(b))->compl_flag
#define subg_nde_list(b)	((SGFrame)(b))->nde_list
#define subg_producer(b)	((SGFrame)(b))->producer
#define subg_consumers(b)	((SGFrame)(b))->consumers
#define subg_timestamp(b)	((SGFrame)(b))->ts
#define subg_structs_are_reclaimed(b)	((SGFrame)(b))->reclaimed_structs

/* beginning of REAL answers in the answer list */
#define subg_answers(subg) aln_next_aln(subg_ans_list_ptr(subg))

#define ConsumerIsProducerVariant(pSF)	( subg_producer(pSF) == pSF )
#define ConsumerIsProperlySubsumed(pSF)	( subg_producer(pSF) != pSF )

#define ProducerHasConsumers(pSF)	IsNonNULL(subg_consumers(pSF))

/*
 * Determines whether a producer subgoal has added answers to its set
 * since the given consumer last collected relavant answers from this set.
 */
#define ConsumerCacheNeedsUpdating(ConsSF,ProdSF)		\
   ( ( ConsSF != ProdSF ) &&					\
     IsNonNULL(subg_ans_root_ptr(ProdSF)) &&			\
     ( TSTN_TimeStamp((TSTNptr)subg_ans_root_ptr(ProdSF)) >	\
       subg_timestamp(ConsSF) ) )

extern ALNptr empty_return();


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

extern struct Structure_Manager smSF;


/* Subgoal Frames (De)Allocation
   ----------------------------- */
/*
 * Allocated Subgoal Frames are maintained on a list to facilitate
 * deallocation of specific calls and answer sets.
 */
#define SF_AddNewToAllocList(SF)	\
   SM_AddToAllocList_DL(smSF,SF,subg_prev_subgoal,subg_next_subgoal)

#define SF_RemoveFromAllocList(SF)	\
   SM_RemoveFromAllocList_DL(smSF,SF,subg_prev_subgoal,subg_next_subgoal)

/*
 * Allocates and initializes a subgoal frame for a producer subgoal.  It
 * is inserted at the head of the global subgoal list, located in the
 * structure manager, smSF.  The TIP field is initialized, fields of the
 * call trie leaf and this subgoal are set to point to one another, while
 * the completion stack frame pointer is set to the next available
 * location (frame) on the stack (but the space is not yet allocated from
 * the stack).  Also, an answer-list node is allocated for pointing to a
 * dummy answer node and inserted into the answer list.  Note that answer
 * sets (answer tries, roots) are lazily created -- not until an answer
 * is generated.  Therefore this field may potentially be NULL, as it is
 * initialized here.  memset() is used so that the remaining fields are
 * initialized to 0/NULL so, in some sense making this macro independent
 * of the number of fields.
 */

#define NewProducerSF(SF,Leaf,TableInfo) {			\
								\
   SGFrame pNewSF;						\
								\
   SM_AllocateStruct(smSF,pNewSF);				\
   pNewSF = memset(pNewSF,0,sizeof(struct subgoal_frame));	\
   subg_tif_ptr(pNewSF) = TableInfo;				\
   subg_leaf_ptr(pNewSF) = Leaf;				\
   CallTrieLeaf_SetSF(Leaf,pNewSF);				\
   subg_ans_list_ptr(pNewSF) = empty_return();			\
   subg_compl_stack_ptr(pNewSF) = openreg - COMPLFRAMESIZE;	\
   subg_producer(pNewSF) = pNewSF;				\
   subg_timestamp(pNewSF) = PRODUCER_SF_INITIAL_TS;		\
   SF_AddNewToAllocList(pNewSF);				\
   SF = pNewSF;							\
}


#define FreeProducerSF(SF)			\
   SF_RemoveFromAllocList(SF);			\
   SM_DeallocateStruct(smSF,SF)


/*
 *  Allocates and initializes a subgoal frame for a consuming subgoal: a
 *  properly subsumed call consuming from an incomplete producer.
 *  Consuming subgoals are NOT inserted into the global subgoal list but
 *  instead are maintained by the producer in a private linked list.
 *  Many fields of a consumer SF are left blank since it won't be used in
 *  the same way as those for producers.  Its main purpose is to maintain
 *  the answer list and the call form.  Just as for the producer, an
 *  answer-list node is allocated for pointing to a dummy answer node and
 *  inserted into the answer list.
 *
 *  Finally, some housekeeping is needed to support lazy creation of the
 *  auxiliary structures in the producer's answer TST.  If this is the
 *  first consumer for this producer, then create these auxiliary
 *  structures.
 */

void tstCreateStructures(TSTNptr);

#define NewConsumerSF(SF,Leaf,TableInfo,Producer) {		\
					      			\
   SGFrame pNewSF;				    		\
						    		\
   SM_AllocateStruct(smSF,pNewSF);		       		\
   pNewSF = memset(pNewSF,0,sizeof(struct subgoal_frame));	\
   subg_tif_ptr(pNewSF) = TableInfo;				\
   subg_leaf_ptr(pNewSF) = Leaf;				\
   CallTrieLeaf_SetSF(Leaf,pNewSF);		  		\
   subg_ans_list_ptr(pNewSF) = empty_return();			\
   subg_producer(pNewSF) = Producer;				\
   if ( ! ProducerHasConsumers(Producer) )	 		\
     tstCreateStructures((TSTNptr)subg_ans_root_ptr(Producer));	\
   subg_consumers(pNewSF) = subg_consumers(Producer);		\
   subg_consumers(Producer) = pNewSF;				\
   subg_timestamp(pNewSF) = CONSUMER_SF_INITIAL_TS;		\
   SF = pNewSF;							\
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#ifdef CHAT
#define set_min(a,b,c)	a = b
#else
#define set_min(a,b,c)	if (b < c) a = b; else a = c
#endif

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

#define is_completed(SUBG_PTR)	\
        ((Integer) subg_compl_flag(SUBG_PTR) < 0)

#ifdef CHAT
#define mark_as_completed(SUBG_PTR) {					\
          subg_compl_flag(SUBG_PTR) = (CPtr) -1;			\
          /* the following is used to maintain invariants; */		\
          /* ideally the completion stack should be compacted */	\
          /* and completed subgoals should be removed instead */	\
          compl_pdreg(subg_compl_stack_ptr(SUBG_PTR)) = NULL;		\
          reclaim_del_ret_list(SUBG_PTR);				\
        } 
#else
#define mark_as_completed(SUBG_PTR) {			\
          subg_compl_flag(SUBG_PTR) = (CPtr) -1;	\
          reclaim_del_ret_list(SUBG_PTR);		\
        } 
#endif

#define subgoal_space_has_been_reclaimed(SUBG_PTR,CS_FRAME) \
        (SUBG_PTR != compl_subgoal_ptr(CS_FRAME))

#define mark_delayed(csf1, csf2, susp) { \
	  compl_visited(csf1) = DELAYED; \
	  compl_visited(csf2) = DELAYED; \
  /* do not put TRUE but some !0 value that GC can recognize as int tagged */\
	  csf_neg_loop(susp) = INT; \
        }

/*----------------------------------------------------------------------*/
/* The following macro might be called more than once for some subgoal. */
/* So, please make sure that functions/macros that it calls are robust  */
/* under repeated uses.                                 - Kostis.       */
/* A new Subgoal Frame flag prevents multiple calls.	- Ernie		*/
/*----------------------------------------------------------------------*/

#ifdef CHAT
#define reclaim_incomplete_table_structs(SUBG_PTR) {	\
   if ( ! subg_structs_are_reclaimed(SUBG_PTR) ) {	\
     chat_free_cons_chat_areas(SUBG_PTR);		\
     table_complete_entry(SUBG_PTR);			\
     subg_structs_are_reclaimed(SUBG_PTR) = TRUE;	\
   }							\
 }
#else
#define reclaim_incomplete_table_structs(SUBG_PTR) {	\
   if ( ! subg_structs_are_reclaimed(SUBG_PTR) ) {	\
     table_complete_entry(SUBG_PTR);			\
     subg_structs_are_reclaimed(SUBG_PTR) = TRUE;	\
   }							\
 }
#endif

/*----------------------------------------------------------------------*/

#define adjust_level(CS_FRAME) \
    xtemp2 = (CPtr) compl_level(CS_FRAME);	\
    if ((Integer) xtemp2 < compl_level(openreg)) {  \
      for (xtemp1 = CS_FRAME;			\
	   compl_level(xtemp1) >= (Integer) xtemp2 && xtemp1 >= openreg; \
	   xtemp1 = next_compl_frame(xtemp1)) {	\
	     compl_level(xtemp1) = (Integer) xtemp2;\
      }						\
    }

/*----------------------------------------------------------------------*/

#ifdef CHAT
#define reset_freeze_registers \
    level_num = 0; \
    root_address = ptcpreg = NULL
#else
#define reset_freeze_registers \
    bfreg = (CPtr)(tcpstack.high) - CP_SIZE; \
    trfreg = (CPtr *)(tcpstack.low); \
    hfreg = (CPtr)(glstack.low); \
    efreg = (CPtr)(glstack.high) - 1; \
    level_num = xwammode = 0; \
    root_address = ptcpreg = NULL

#define adjust_freeze_registers(tcp) \
    if (bfreg < tcp_bfreg(tcp)) { bfreg = tcp_bfreg(tcp); }	 \
    if (trfreg > tcp_trfreg(tcp)) { trfreg = tcp_trfreg(tcp); }\
    if (hfreg > tcp_hfreg(tcp)) { hfreg = tcp_hfreg(tcp); }	 \
    if (efreg < tcp_efreg(tcp)) { efreg = tcp_efreg(tcp); }
#endif


#ifdef CHAT
#define reclaim_stacks(tcp) \
  if (tcp == root_address) { \
    reset_freeze_registers; \
  }
#else
#define reclaim_stacks(tcp) \
  if (tcp == root_address) { \
    reset_freeze_registers; \
    /* xsb_dbgmsg("reset registers...."); */ \
  } \
  else { \
    adjust_freeze_registers(tcp); \
    /* xsb_dbgmsg(adjust registers...."); */ \
  }
#endif

/*----------------------------------------------------------------------*/

#define pdlpush(cell)	*(pdlreg) = cell;  pdlreg--

#define pdlpop		*(++pdlreg)

#define pdlempty	(pdlreg == (CPtr)(pdl.high) - 1)

#define resetpdl \
   if (pdlreg < (CPtr) pdl.low) \
     xsb_exit("pdlreg grew too much"); \
   else (pdlreg = (CPtr)(pdl.high) - 1)

#define remove_open_tables_loop(Endpoint) remove_open_tries(Endpoint)

#define remove_open_tables() remove_open_tries(COMPLSTACKBOTTOM)

/*----------------------------------------------------------------------*/
