/* File:      choice.h
** Author(s): Xu, Swift, Sagonas, Freire, Johnson
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
** Copyright (C) ECRC, Germany, 1990
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


/* --- Types of Choice Points ----------------------------------------- */

#define STANDARD_CP_FRAME       0
#define GENERATOR_CP_FRAME	1
#define CONSUMER_CP_FRAME	2
#if (!defined(CHAT))
#define COMPL_SUSP_CP_FRAME	3
#endif

/* --- type definitions ----------------------------------------------- */

typedef struct choice_point {
    byte *next_clause;	/* the entry of next choice */
    CPtr ebreg;		/* environment backtrack -- top of env stack */
    CPtr hreg;		/* current top of heap */
    CPtr *trreg;	/* current top of trail stack */
    byte *cpreg;	/* return point of the call to the procedure	*/
    CPtr ereg;		/* current top of stack */
    CPtr prev;		/* dynamic link */
    CPtr pdreg;		/* value of delay register for the parent subgoal */
    CPtr ptcp;          /* pointer to parent tabled CP (subgoal) */
} *Choice;

#define CP_SIZE	(sizeof(struct choice_point)/sizeof(CPtr))

#define cp_pcreg(b)		((Choice)(b))->next_clause
#define cp_ebreg(b)		((Choice)(b))->ebreg
#define cp_hreg(b)		((Choice)(b))->hreg
#define cp_trreg(b)		((Choice)(b))->trreg
#define cp_cpreg(b)		((Choice)(b))->cpreg
#define cp_ereg(b)		((Choice)(b))->ereg
#define cp_prevbreg(b)		((Choice)(b))->prev
#define cp_pdreg(b)		((Choice)(b))->pdreg
#define cp_ptcp(b)              ((Choice)(b))->ptcp

#define save_choicepoint(t_breg, t_ereg, next_clause, prev) \
    t_breg -= CP_SIZE; \
    cp_ptcp(t_breg) = ptcpreg; \
    cp_pdreg(t_breg) = delayreg; \
    cp_prevbreg(t_breg) = prev; \
    cp_ereg(t_breg) = t_ereg; \
    cp_cpreg(t_breg) = cpreg; \
    cp_trreg(t_breg) = trreg; \
    cp_hreg(t_breg) = hreg; \
    cp_ebreg(t_breg) = ebreg; \
    cp_pcreg(t_breg) = next_clause

/*----------------------------------------------------------------------*/

#if (defined(LOCAL_EVAL) && !defined(CHAT))
#define CHECK_COMPLETE_TAG   0
#define RETRY_GEN_ACTIVE_TAG 1
#define RETURN_ANSWERS_TAG   2
#endif

/*----------------------------------------------------------------------*/
/* Table-Producer Choice Point						*/
/*----------------------------------------------------------------------*/

typedef struct tabled_choice_point {
    byte *next_clause;	/* the entry of next choice */
    CPtr ebreg;		/* environment backtrack -- top of env stack */
    CPtr hreg;		/* current top of heap */
    CPtr *trreg;	/* current top of trail stack */
    byte *cpreg;	/* return point of the call to the procedure */
    CPtr ereg;		/* current top of stack */
    CPtr prev;		/* previous choicepoint */
    CPtr pdreg;		/* value of delay register for the parent subgoal */
    CPtr ptcp;		/* pointer to parent tabled CP (subgoal) */
    CPtr subgoal_ptr;	/* pointer to the subgoal frame */
#if (!defined(CHAT))
/* The following are needed to reclaim frozen space at SCC completion time */
    CPtr bfreg;
    CPtr hfreg;
    CPtr *trfreg;
    CPtr efreg;
#ifdef LOCAL_EVAL
    int tag;
    ALNptr trie_return;
    int arity;
#endif
#else
    CPtr chat_roots;
#endif
} *TChoice;

#define TCP_SIZE	(sizeof(struct tabled_choice_point)/sizeof(CPtr))

#define tcp_pcreg(b)		((TChoice)(b))->next_clause
#define tcp_ebreg(b)		((TChoice)(b))->ebreg
#define tcp_hreg(b)		((TChoice)(b))->hreg
#define tcp_trreg(b)		((TChoice)(b))->trreg
#define tcp_cpreg(b)		((TChoice)(b))->cpreg
#define tcp_ereg(b)		((TChoice)(b))->ereg
#define tcp_prevbreg(b)		((TChoice)(b))->prev
#define tcp_pdreg(b)		((TChoice)(b))->pdreg
#define tcp_ptcp(b)		((TChoice)(b))->ptcp
#define tcp_subgoal_ptr(b)	((TChoice)(b))->subgoal_ptr

#if (!defined(CHAT))
#define tcp_bfreg(b)		((TChoice)(b))->bfreg
#define tcp_hfreg(b)		((TChoice)(b))->hfreg
#define tcp_trfreg(b)		((TChoice)(b))->trfreg
#define tcp_efreg(b)		((TChoice)(b))->efreg

#ifdef LOCAL_EVAL
#define tcp_tag(b)		((TChoice)(b))->tag
#define tcp_trie_return(b)	((TChoice)(b))->trie_return
#define tcp_arity(b)		((TChoice)(b))->arity
#endif
#else
#define tcp_chat_roots(b)       ((TChoice)(b))->chat_roots
#endif

#define is_generator_choicepoint(b) \
    ((cp_pcreg(b) == (byte *) &check_complete_inst) || \
     (cell_opcode(cp_pcreg(b)) == tabletrust) || \
     (cell_opcode(cp_pcreg(b)) == tableretry))

/* The following macro is used to perform early completion */
#ifdef CHAT
#define perform_early_completion(SUBGOAL,GENERATOR_CP) \
    if (subg_cp_ptr(SUBGOAL) != NULL) \
      tcp_pcreg(subg_cp_ptr(SUBGOAL)) = (byte *) &check_complete_inst; \
    mark_as_completed(SUBGOAL);
#else
#define perform_early_completion(SUBGOAL,GENERATOR_CP) \
    tcp_pcreg(GENERATOR_CP) = (byte *) &check_complete_inst; \
    mark_as_completed(SUBGOAL);
#endif


#define _SaveProducerCPF_common(TopCPS, Cont, pSF) {	\
   TopCPS -= TCP_SIZE;					\
   tcp_ptcp(TopCPS) = ptcpreg;				\
   tcp_pdreg(TopCPS) = delayreg;			\
   tcp_ereg(TopCPS) = ereg;				\
   tcp_cpreg(TopCPS) = cpreg;				\
   tcp_trreg(TopCPS) = trreg;				\
   tcp_hreg(TopCPS) = hreg;				\
   tcp_ebreg(TopCPS) = ebreg;				\
   tcp_subgoal_ptr(TopCPS) = (CPtr)pSF;			\
   tcp_prevbreg(TopCPS) = breg;				\
   tcp_pcreg(TopCPS) = Cont;				\
 }

#ifdef CHAT

#define SaveProducerCPF(TopCPS, Cont, pSF, Arity) {	\
   _SaveProducerCPF_common(TopCPS, Cont, pSF);		\
   tcp_chat_roots(TopCPS) = NULL;			\
 }

#else	/* !defined(CHAT) */

#define _SaveProducerCPF_slg(TopCPS, Cont, pSF) {	\
   _SaveProducerCPF_common(TopCPS, Cont, pSF);		\
   tcp_bfreg(TopCPS) = bfreg;				\
   tcp_efreg(TopCPS) = efreg;				\
   tcp_trfreg(TopCPS) = trfreg;				\
   tcp_hfreg(TopCPS) = hfreg;				\
 }

#ifdef LOCAL_EVAL
#define SaveProducerCPF(TopCPS, Cont, pSF, Arity) {	\
   _SaveProducerCPF_slg(TopCPS, Cont, pSF);		\
   tcp_tag(TopCPS) = CHECK_COMPLETE_TAG;		\
   tcp_arity(TopCPS) = (int) Arity;			\
   tcp_trie_return(TopCPS) = NULL;			\
 }
#else
#define SaveProducerCPF(TopCPS, Cont, pSF, Arity)	\
   _SaveProducerCPF_slg(TopCPS, Cont, pSF)
#endif

#endif	/* of CHAT */


/*----------------------------------------------------------------------*/
/* Consumer Choice Point						*/
/*----------------------------------------------------------------------*/

typedef struct consumer_choice_point {
    byte *next_clause;	/* the entry of next choice */
    CPtr ebreg;		/* environment backtrack -- top of env stack */
    CPtr hreg;		/* current top of heap */
    CPtr *trreg;	/* current top of trail stack */
    byte *cpreg;	/* return point of the call to the procedure	*/
    CPtr ereg;		/* current top of stack */
    CPtr prev;      
    CPtr pdreg;		/* value of delay register for the parent subgoal */
    CPtr ptcp;		/* pointer to parent tabled CP (subgoal) */
    CPtr subgoal_ptr;
    CPtr prevlookup;      
    ALNptr trie_return;
#ifdef CHAT
    CPtr chat_area;	/* temporarily */
#endif
} *NLChoice;

#define NLCPSIZE	(sizeof(struct consumer_choice_point)/sizeof(CPtr))

#define nlcp_pcreg(b)		((NLChoice)(b))->next_clause
#define nlcp_ebreg(b)		((NLChoice)(b))->ebreg
#define nlcp_hreg(b)		((NLChoice)(b))->hreg
#define nlcp_trreg(b)		((NLChoice)(b))->trreg
#define nlcp_cpreg(b)		((NLChoice)(b))->cpreg
#define nlcp_ereg(b)		((NLChoice)(b))->ereg
#define nlcp_subgoal_ptr(b)	((NLChoice)(b))->subgoal_ptr
#define nlcp_pdreg(b)		((NLChoice)(b))->pdreg
#define nlcp_ptcp(b)		((NLChoice)(b))->ptcp
#define nlcp_prevbreg(b)	((NLChoice)(b))->prev
#define nlcp_prevlookup(b)	((NLChoice)(b))->prevlookup
#define nlcp_trie_return(b)	((NLChoice)(b))->trie_return
#ifdef CHAT
#define nlcp_chat_area(b)	((NLChoice)(b))->chat_area
#endif

#define is_consumer_choicepoint(b) \
    (cp_pcreg(b) == (byte *) &answer_return_inst)


#define _SaveConsumerCPF_common(TopCPS,SF,PrevConsumer) {	\
   TopCPS -= NLCPSIZE; 						\
   nlcp_trie_return(TopCPS) = subg_ans_list_ptr(SF); 		\
   nlcp_subgoal_ptr(TopCPS) = (CPtr)SF;				\
   nlcp_prevlookup(TopCPS) = PrevConsumer;			\
   nlcp_ptcp(TopCPS) = ptcpreg; 				\
   nlcp_pdreg(TopCPS) = delayreg; 				\
   nlcp_prevbreg(TopCPS) = breg; 				\
   nlcp_ereg(TopCPS) = ereg; 					\
   nlcp_cpreg(TopCPS) = cpreg; 					\
   nlcp_trreg(TopCPS) = trreg; 					\
   nlcp_hreg(TopCPS) = hreg; 					\
   nlcp_ebreg(TopCPS) = ebreg; 					\
   nlcp_pcreg(TopCPS) = (pb) &answer_return_inst; 		\
 }

#ifdef CHAT
#define SaveConsumerCPF(TopCPS,SF,PrevConsumer) {	\
   _SaveConsumerCPF_common(TopCPS,SF,PrevConsumer);	\
    nlcp_chat_area(TopCPS) = NULL;			\
 }
#else
#define SaveConsumerCPF(TopCPS,SF,PrevConsumer)		\
   _SaveConsumerCPF_common(TopCPS,SF,PrevConsumer)
#endif

/*----------------------------------------------------------------------*/
/* The following CP is used for a computation that has been suspended	*/
/* on completion of a subgoal.	For CHAT it is not put on the CP stack 	*/
/* upon suspension; instead a chat-area is created for it.  This chat   */
/* area is reinstalled upon resumption of the suspended computation.    */
/* NOTE: CHAT relies on the fact that this CP and the choice points of  */
/*       consumers (NLChoice) have similar structure; so the minor      */
/*       change below indicated by the if-defs !!                       */
/*----------------------------------------------------------------------*/

typedef struct compl_susp_frame {
    byte *next_clause;	/* the completion suspension instruction */
    CPtr ebreg;		/* environment backtrack -- top of env stack */
    CPtr hreg;		/* current top of heap */
    CPtr *trreg;	/* current top of trail stack */
    byte *cpreg;	/* return point of the call to the procedure */
    CPtr ereg;		/* current top of stack */
#ifdef CHAT
    CPtr prev;		/* previous CP -- not used in the SLG-WAM */
		    /* although for garbage collection it probably should */
#endif
    CPtr pdreg;		/* value of delay register for the parent subgoal */
    CPtr ptcp;		/* pointer to parent tabled CP (subgoal) */
    CPtr subgoal_ptr;	/* pointer to the call structure */
    CPtr prevcsf;	/* previous completion suspension frame */
    Cell neg_loop;	/* !0 if the suspension is not LRD stratified */
	     /* for CHAT this field appears in the place of nlcp_trie_return */
             /* so please make sure that it has the same size as ALNptr */
#ifdef CHAT
    CPtr chat_area;	/* this field is needed for compl susp frames */
#endif
} *ComplSuspFrame;

#define CSF_SIZE	(sizeof(struct compl_susp_frame)/sizeof(CPtr))

#define csf_pcreg(b)		((ComplSuspFrame)(b))->next_clause
#define csf_ebreg(b)		((ComplSuspFrame)(b))->ebreg
#define csf_hreg(b)		((ComplSuspFrame)(b))->hreg
#define csf_trreg(b)		((ComplSuspFrame)(b))->trreg
#define csf_cpreg(b)		((ComplSuspFrame)(b))->cpreg
#define csf_ereg(b)		((ComplSuspFrame)(b))->ereg
#ifdef CHAT
#define csf_prev(b)		((ComplSuspFrame)(b))->prev
#endif
#define csf_pdreg(b)		((ComplSuspFrame)(b))->pdreg
#define csf_ptcp(b)		((ComplSuspFrame)(b))->ptcp
#define csf_subgoal_ptr(b)	((ComplSuspFrame)(b))->subgoal_ptr
#define csf_prevcsf(b)		((ComplSuspFrame)(b))->prevcsf
#define csf_neg_loop(b)		((ComplSuspFrame)(b))->neg_loop
#ifdef CHAT
#define csf_chat_area(b)	((ComplSuspFrame)(b))->chat_area
#endif

#ifdef CHAT
#define is_compl_susp_frame(b) \
    (cp_pcreg(b) == (byte *) &resume_compl_suspension_inst)
#endif

#ifdef CHAT
#define save_compl_susp_frame(WHERE,SUBG,T_PTCP,CPREG) \
    csf_pcreg(WHERE) = (pb) &resume_compl_suspension_inst; \
    csf_ebreg(WHERE) = ebreg; \
    csf_hreg(WHERE) = hreg; \
    csf_trreg(WHERE) = trreg; \
    csf_cpreg(WHERE) = CPREG; \
    csf_ereg(WHERE) = ereg; \
    csf_prev(WHERE) = NULL; /* will be filled in re-installation */\
    csf_pdreg(WHERE) = delayreg; \
    csf_ptcp(WHERE) = T_PTCP; \
    csf_subgoal_ptr(WHERE) = (CPtr)SUBG; \
    csf_prevcsf(WHERE) = subg_compl_susp_ptr(SUBG); \
    csf_neg_loop(WHERE) = FALSE; \
    csf_chat_area(WHERE) = NULL
#else
#define save_compl_susp_frame(t_breg,t_ereg,subg,t_ptcp,CPREG) \
    t_breg -= CSF_SIZE; \
    csf_neg_loop(t_breg) = FALSE; \
    csf_prevcsf(t_breg) = subg_compl_susp_ptr(subg); \
    csf_ptcp(t_breg) = t_ptcp; \
    csf_pdreg(t_breg) = delayreg; \
    csf_subgoal_ptr(t_breg) = subg; \
    csf_ereg(t_breg) = t_ereg; \
    csf_cpreg(t_breg) = CPREG; \
    csf_trreg(t_breg) = trreg; \
    csf_hreg(t_breg) = hreg; \
    csf_ebreg(t_breg) = ebreg; \
    csf_pcreg(t_breg) = (pb) NULL
#endif

/*----------------------------------------------------------------------*/
/* The following CP is used to resume a set of computations that have	*/
/* been suspended on completion of a subgoal.  Only SLG-WAM uses this. 	*/
/*----------------------------------------------------------------------*/

#if (!defined(CHAT))    /* CHAT does not need this */

typedef struct compl_susp_choice_point {
    byte *next_clause;	/* the completion suspension instruction */
    CPtr ebreg;		/* environment backtrack -- top of env stack */
    CPtr hreg;		/* current top of heap */
    CPtr compsuspptr;	/* pointer to the completion_suspension_frame */
    CPtr prev;		/* lookup: previous choicepoint */
} *ComplSuspChoice;

#define COMPL_SUSP_CP_SIZE	\
	(sizeof(struct compl_susp_choice_point)/sizeof(CPtr))

#define cs_pcreg(b)		((ComplSuspChoice)(b))->next_clause
#define cs_ebreg(b)		((ComplSuspChoice)(b))->ebreg
#define cs_hreg(b)		((ComplSuspChoice)(b))->hreg
#define cs_compsuspptr(b)	((ComplSuspChoice)(b))->compsuspptr
#define cs_prevbreg(b)		((ComplSuspChoice)(b))->prev

#define save_compl_susp_cp(t_breg,prev,compsuspptr) \
    t_breg -= COMPL_SUSP_CP_SIZE; \
    cs_prevbreg(t_breg) = prev; \
    cs_compsuspptr(t_breg) = compsuspptr;\
    cs_hreg(t_breg) = hreg; \
    cs_ebreg(t_breg) = ebreg; \
    cs_pcreg(t_breg) = (pb) &resume_compl_suspension_inst
#endif

/* --------------------------------------------------------------------	*/

/*
 *  Push on the CP Stack the arguments in the X reg's.  't_breg' gets the
 *  topmost Cell on the cpstack, 'arity' is the number of args to be pushed,
 *  'ii' is a variable supplied by the caller for macro use, and 'regbase'
 *  is a pointer to the "lowest" reg holding an argument.
 *
 *  On "exit", 't_breg' points to the topmost arg on the cpstack.
 */
#define save_registers(t_breg, arity, ii, regbase) \
    for (ii = 1; ii <= arity; ii++) bld_copy(--t_breg, cell(regbase+ii))


#define restore_registers(t_breg, arity, ii, regbase) \
    t_breg += CP_SIZE; \
    for (ii = arity; ii >= 1; ii--) bld_copy(regbase+ii, cell(t_breg++))


#define table_restore_registers(t_breg, arity, ii, regbase) \
    t_breg += TCP_SIZE; \
    for (ii = arity; ii >= 1; ii--) bld_copy(regbase+ii, cell(t_breg++))


/* Local (Environment) Stack
   ------------------------- */
/*
 *  Structure of an Activation Record (Environment Frame) is
 *
 * low mem
 *   |   Block of Permanent Variables
 *   |   Continuation Pointer  (CPreg value at procedure invocation)
 *   V   Dynamic Link  (ptr to link field of prev frame)
 * high mem
 *
 *  Ereg always points to the Dynamic Link field of an AR, while EBreg and
 *  EFreg always point to the topmost Permanent Variable in the frame.
 */

#ifdef CHAT
#define save_find_locx(t_ereg) \
    if (ereg_on_top(t_ereg)) ebreg = t_ereg - *(cpreg-2*sizeof(Cell)+3)+1;

#define restore_some_wamregs(t_breg, t_ereg) \
    hreg = hbreg; \
    cpreg = cp_cpreg(t_breg); \
    t_ereg = cp_ereg(t_breg)
#else
/*
 *  Set ebreg to the topmost env frame of those pointed to by ereg or efreg.
 */
#define save_find_locx(t_ereg) \
    if (efreg_on_top(t_ereg)) ebreg = efreg;\
    else if (ereg_on_top(t_ereg)) ebreg = t_ereg - *(cpreg-2*sizeof(Cell)+3)+1;

#define restore_some_wamregs(t_breg, t_ereg) \
    if (hbreg >= hfreg) hreg = hbreg; else hreg = hfreg; \
    cpreg = cp_cpreg(t_breg); \
    t_ereg = cp_ereg(t_breg)
#endif
