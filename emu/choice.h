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


#include "debugs/debug_kostis.h"


/* --- Types of Choice Points ----------------------------------------- */

#define STANDARD_CP_FRAME              0
#define TABLED_SINGLE_CLAUSE_CP_FRAME  1
#define TABLED_SOLUTION_CP_FRAME       2
#define TABLED_NEW_LOOKUP_CP_FRAME     3
#define TABLED_COMPL_SUSP_CP_FRAME     4

/* --- type definitions ----------------------------------------------- */

typedef struct choice_point {
    byte *next_clause;	/* the entry of next choice */
    CPtr ebreg;		/* environment backtrack -- top of env stack */
    CPtr hreg;		/* current top of heap */
    CPtr *trreg;	/* current top of trail stack */
    byte *cpreg;	/* return point of the call to the procedure	*/
    CPtr ereg;		/* current top of stack */
    CPtr prev;		/* dynamic link */
    CPtr ptcp;		/* pointer to parent tabled CP (subgoal) */
    CPtr pdreg;		/* value of delay register for the parent subgoal */
#ifdef KOSTIS_DEBUG
    Cell end;
#endif
} *Choice;

#define CP_SIZE	(sizeof(struct choice_point)/sizeof(CPtr))

#define cp_pcreg(b)		((Choice)(b))->next_clause
#define cp_ebreg(b)		((Choice)(b))->ebreg
#define cp_hreg(b)		((Choice)(b))->hreg
#define cp_trreg(b)		((Choice)(b))->trreg
#define cp_cpreg(b)		((Choice)(b))->cpreg
#define cp_ereg(b)		((Choice)(b))->ereg
#define cp_prevbreg(b)		((Choice)(b))->prev
#define cp_ptcp(b)		((Choice)(b))->ptcp
#define cp_pdreg(b)		((Choice)(b))->pdreg
#ifdef KOSTIS_DEBUG
#define cp_end(b)		((Choice)(b))->end
#endif

#ifdef KOSTIS_DEBUG
#define save_choicepoint(t_breg, t_ereg, next_clause, prev) \
    t_breg -= CP_SIZE; \
    cp_end(t_breg) = makestring("end_of_choice_point"); \
    cp_pdreg(t_breg) = delayreg; \
    cp_ptcp(t_breg) = ptcpreg; \
    cp_prevbreg(t_breg) = prev; \
    cp_ereg(t_breg) = t_ereg; \
    cp_cpreg(t_breg) = cpreg; \
    cp_trreg(t_breg) = trreg; \
    cp_hreg(t_breg) = hreg; \
    cp_ebreg(t_breg) = ebreg; \
    cp_pcreg(t_breg) = next_clause
#else
#define save_choicepoint(t_breg, t_ereg, next_clause, prev) \
    t_breg -= CP_SIZE; \
    cp_pdreg(t_breg) = delayreg; \
    cp_ptcp(t_breg) = ptcpreg; \
    cp_prevbreg(t_breg) = prev; \
    cp_ereg(t_breg) = t_ereg; \
    cp_cpreg(t_breg) = cpreg; \
    cp_trreg(t_breg) = trreg; \
    cp_hreg(t_breg) = hreg; \
    cp_ebreg(t_breg) = ebreg; \
    cp_pcreg(t_breg) = next_clause
#endif

#ifdef LOCAL_EVAL
#define CHECK_COMPLETE_TAG 0
#define RETRY_GEN_ACTIVE_TAG 1
#define RETURN_ANSWERS_TAG 2
#endif

/*----------------------------------------------------------------------*/

typedef struct tabled_choice_point {
    byte *next_clause;	/* the entry of next choice */
    CPtr ebreg;		/* environment backtrack -- top of env stack */
    CPtr hreg;		/* current top of heap */
    CPtr *trreg;	/* current top of trail stack */
    byte *cpreg;	/* return point of the call to the procedure */
    CPtr ereg;		/* current top of stack */
    CPtr prev;		/* lookup: previous susp 
			   solution: previous choicepoint */
    CPtr pdreg;		/* value of delay register for the parent subgoal */
    CPtr subgoal_ptr;	/* pointer to the call structure */
    CPtr ptcp;		/* pointer to parent tabled CP (subgoal) */
/* The following are needed to reclaim frozen space at SCC completion time */
    CPtr bfreg;
    CPtr hfreg;
    CPtr *trfreg;
    CPtr efreg;
#ifdef LOCAL_EVAL
    int tag;
    ALPtr trie_return;
    int arity;
#endif
#ifdef KOSTIS_DEBUG
    Cell end;
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
#define tcp_prevlookup(b)	((TChoice)(b))->prev  /* used for lookup CPs */
#define tcp_subgoal_ptr(b)	((TChoice)(b))->subgoal_ptr
#define tcp_ptcp(b)		((TChoice)(b))->ptcp
#define tcp_pdreg(b)		((TChoice)(b))->pdreg
#define tcp_bfreg(b)		((TChoice)(b))->bfreg
#define tcp_hfreg(b)		((TChoice)(b))->hfreg
#define tcp_trfreg(b)		((TChoice)(b))->trfreg
#define tcp_efreg(b)		((TChoice)(b))->efreg
#ifdef LOCAL_EVAL
#define tcp_tag(b)		((TChoice)(b))->tag
#define tcp_arity(b)		((TChoice)(b))->arity
#define tcp_trie_return(b)	((TChoice)(b))->trie_return
#endif
#ifdef KOSTIS_DEBUG
#define tcp_end(b)		((TChoice)(b))->end
#endif

/*----------------------------------------------------------------------*/
/* The following CP is used in the tabletrysingle to check completion.	*/
/*----------------------------------------------------------------------*/

#ifdef KOSTIS_DEBUG
#define save_singleclause_choicepoint(t_breg,t_ereg,tbreg,prev) \
    t_breg -= TCP_SIZE; \
    tcp_end(t_breg) = makestring("single_clause_tabled_CP"); \
    tcp_bfreg(t_breg) = bfreg; \
    tcp_efreg(t_breg) = efreg; \
    tcp_trfreg(t_breg) = trfreg; \
    tcp_hfreg(t_breg) = hfreg; \
    tcp_pdreg(t_breg) = delayreg; \
    tcp_ptcp(t_breg) = ptcpreg; \
    tcp_ereg(t_breg) = t_ereg; \
    tcp_cpreg(t_breg) = cpreg; \
    tcp_trreg(t_breg) = trreg; \
    tcp_hreg(t_breg) = hreg; \
    tcp_ebreg(t_breg) = ebreg; \
    tcp_subgoal_ptr(t_breg) = tbreg; \
    tcp_prevlookup(t_breg) = prev; \
    tcp_pcreg(t_breg) = (pb) &check_complete_inst;
#else
#ifdef LOCAL_EVAL
#define save_singleclause_choicepoint(t_breg,t_ereg,tbreg,prev) \
    t_breg -= TCP_SIZE; \
    tcp_bfreg(t_breg) = bfreg; \
    tcp_efreg(t_breg) = efreg; \
    tcp_trfreg(t_breg) = trfreg; \
    tcp_hfreg(t_breg) = hfreg; \
    tcp_pdreg(t_breg) = delayreg; \
    tcp_ptcp(t_breg) = ptcpreg; \
    tcp_ereg(t_breg) = t_ereg; \
    tcp_cpreg(t_breg) = cpreg; \
    tcp_trreg(t_breg) = trreg; \
    tcp_hreg(t_breg) = hreg; \
    tcp_ebreg(t_breg) = ebreg; \
    tcp_subgoal_ptr(t_breg) = tbreg; \
    tcp_prevlookup(t_breg) = prev; \
    tcp_tag(t_breg) = CHECK_COMPLETE_TAG; \
    tcp_trie_return(t_breg) = NULL; \
    tcp_pcreg(t_breg) = (pb) &check_complete_inst;
#else
#define save_singleclause_choicepoint(t_breg,t_ereg,tbreg,prev) \
    t_breg -= TCP_SIZE; \
    tcp_bfreg(t_breg) = bfreg; \
    tcp_efreg(t_breg) = efreg; \
    tcp_trfreg(t_breg) = trfreg; \
    tcp_hfreg(t_breg) = hfreg; \
    tcp_pdreg(t_breg) = delayreg; \
    tcp_ptcp(t_breg) = ptcpreg; \
    tcp_ereg(t_breg) = t_ereg; \
    tcp_cpreg(t_breg) = cpreg; \
    tcp_trreg(t_breg) = trreg; \
    tcp_hreg(t_breg) = hreg; \
    tcp_ebreg(t_breg) = ebreg; \
    tcp_subgoal_ptr(t_breg) = tbreg; \
    tcp_prevlookup(t_breg) = prev; \
    tcp_pcreg(t_breg) = (pb) &check_complete_inst;
#endif /* LOCAL_EVAL */
#endif

/* save solution & save lookup are the same (but for their tcp_pcreg())	*/

#ifdef KOSTIS_DEBUG
#define save_solution_choicepoint(t_breg,t_ereg,subg,prev) \
    t_breg -= TCP_SIZE; \
    tcp_end(t_breg) = makestring("SOLUTION_tabled_CP"); \
    tcp_bfreg(t_breg) = bfreg; \
    tcp_efreg(t_breg) = efreg; \
    tcp_trfreg(t_breg) = trfreg; \
    tcp_hfreg(t_breg) = hfreg; \
    tcp_pdreg(t_breg) = delayreg; \
    tcp_ptcp(t_breg) = ptcpreg; \
    tcp_ereg(t_breg) = t_ereg; \
    tcp_cpreg(t_breg) = cpreg; \
    tcp_trreg(t_breg) = trreg; \
    tcp_hreg(t_breg) = hreg; \
    tcp_ebreg(t_breg) = ebreg; \
    tcp_subgoal_ptr(t_breg) = subg; \
    tcp_prevlookup(t_breg) = prev; \
    tcp_pcreg(t_breg) = lpcreg
#else
#ifdef LOCAL_EVAL
#define save_solution_choicepoint(t_breg,t_ereg,subg,prev) \
    t_breg -= TCP_SIZE; \
    tcp_bfreg(t_breg) = bfreg; \
    tcp_efreg(t_breg) = efreg; \
    tcp_trfreg(t_breg) = trfreg; \
    tcp_hfreg(t_breg) = hfreg; \
    tcp_pdreg(t_breg) = delayreg; \
    tcp_ptcp(t_breg) = ptcpreg; \
    tcp_ereg(t_breg) = t_ereg; \
    tcp_cpreg(t_breg) = cpreg; \
    tcp_trreg(t_breg) = trreg; \
    tcp_hreg(t_breg) = hreg; \
    tcp_ebreg(t_breg) = ebreg; \
    tcp_subgoal_ptr(t_breg) = subg; \
    tcp_prevlookup(t_breg) = prev; \
    tcp_tag(t_breg) = CHECK_COMPLETE_TAG; \
    tcp_trie_return(t_breg) = (ALPtr) NULL; \
    tcp_pcreg(t_breg) = lpcreg
#else
#define save_solution_choicepoint(t_breg,t_ereg,subg,prev) \
    t_breg -= TCP_SIZE; \
    tcp_bfreg(t_breg) = bfreg; \
    tcp_efreg(t_breg) = efreg; \
    tcp_trfreg(t_breg) = trfreg; \
    tcp_hfreg(t_breg) = hfreg; \
    tcp_pdreg(t_breg) = delayreg; \
    tcp_ptcp(t_breg) = ptcpreg; \
    tcp_ereg(t_breg) = t_ereg; \
    tcp_cpreg(t_breg) = cpreg; \
    tcp_trreg(t_breg) = trreg; \
    tcp_hreg(t_breg) = hreg; \
    tcp_ebreg(t_breg) = ebreg; \
    tcp_subgoal_ptr(t_breg) = subg; \
    tcp_prevlookup(t_breg) = prev; \
    tcp_pcreg(t_breg) = lpcreg
#endif /* LOCAL_EVAL */
#endif

/*----------------------------------------------------------------------*/

typedef struct compl_susp_frame {
    int  type;		/* the type of completion suspension */
    CPtr ebreg;		/* environment backtrack -- top of env stack */
    CPtr hreg;		/* current top of heap */
    CPtr *trreg;	/* current top of trail stack */
    byte *cpreg;	/* return point of the call to the procedure */
    CPtr ereg;		/* current top of stack */
    CPtr prevcsf;	/* previous completion suspension frame */
    CPtr ptcp;		/* pointer to parent tabled CP (subgoal) */
    CPtr pdreg;		/* value of delay register for the parent subgoal */
    CPtr subgoal_ptr;	/* pointer to the call structure */
    bool neg_loop;	/* true if the suspension is not LRD stratified */
#ifdef KOSTIS_DEBUG
    Cell end;
#endif
} *ComplSuspFrame;

#define CSF_SIZE	(sizeof(struct compl_susp_frame)/sizeof(CPtr))

#define csf_type(b)		((ComplSuspFrame)(b))->type
#define csf_ebreg(b)		((ComplSuspFrame)(b))->ebreg
#define csf_hreg(b)		((ComplSuspFrame)(b))->hreg
#define csf_trreg(b)		((ComplSuspFrame)(b))->trreg
#define csf_cpreg(b)		((ComplSuspFrame)(b))->cpreg
#define csf_ereg(b)		((ComplSuspFrame)(b))->ereg
#define csf_prevcsf(b)		((ComplSuspFrame)(b))->prevcsf
#define csf_ptcp(b)		((ComplSuspFrame)(b))->ptcp
#define csf_pdreg(b)		((ComplSuspFrame)(b))->pdreg
#define csf_subgoal_ptr(b)	((ComplSuspFrame)(b))->subgoal_ptr
#define csf_neg_loop(b)		((ComplSuspFrame)(b))->neg_loop
#ifdef KOSTIS_DEBUG
#define csf_end(b)		((ComplSuspFrame)(b))->end
#endif

#ifdef KOSTIS_DEBUG
#define save_compl_susp_frame(t_breg,t_ereg,subg,t_ptcp,type,cpr) \
    t_breg -= CSF_SIZE; \
    csf_end(t_breg) = makestring("Completion Suspension Frame"); \
    csf_neg_loop(t_breg) = FALSE; \
    csf_pdreg(t_breg) = delayreg; \
    csf_ptcp(t_breg) = t_ptcp; \
    csf_prevcsf(t_breg) = subg_compl_susp_ptr(subg); \
    csf_subgoal_ptr(t_breg) = subg; \
    csf_ereg(t_breg) = t_ereg; \
    csf_cpreg(t_breg) = cpr; \
    csf_trreg(t_breg) = trreg; \
    csf_hreg(t_breg) = hreg; \
    csf_ebreg(t_breg) = ebreg; \
    csf_type(t_breg) = type
#else
#define save_compl_susp_frame(t_breg,t_ereg,subg,t_ptcp,type,cpr) \
    t_breg -= CSF_SIZE; \
    csf_neg_loop(t_breg) = FALSE; \
    csf_pdreg(t_breg) = delayreg; \
    csf_ptcp(t_breg) = t_ptcp; \
    csf_prevcsf(t_breg) = subg_compl_susp_ptr(subg); \
    csf_subgoal_ptr(t_breg) = subg; \
    csf_ereg(t_breg) = t_ereg; \
    csf_cpreg(t_breg) = cpr; \
    csf_trreg(t_breg) = trreg; \
    csf_hreg(t_breg) = hreg; \
    csf_ebreg(t_breg) = ebreg; \
    csf_type(t_breg) = type
#endif

/*----------------------------------------------------------------------*/

#define TFINDALL_CSF	0
#define NEGATION_CSF	1

/*----------------------------------------------------------------------*/
/* New look-up Choice Point (active subgoal frame)			*/
/*----------------------------------------------------------------------*/

typedef struct nl_choice_point {
    byte *next_clause;	/* the entry of next choice */
    CPtr ebreg;		/* environment backtrack -- top of env stack */
    CPtr hreg;		/* current top of heap */
    CPtr *trreg;	/* current top of trail stack */
    byte *cpreg;	/* return point of the call to the procedure	*/
    CPtr ereg;		/* current top of stack */
    CPtr subgoal_ptr;
    CPtr ptcp;		/* pointer to parent tabled CP (subgoal) */
    CPtr pdreg;		/* value of delay register for the parent subgoal */
    CPtr prevlookup;      
    CPtr prev;      
    ALPtr trie_return;
#ifdef KOSTIS_DEBUG
    Cell end;
#endif
} *NLChoice;

#define NLCPSIZE	(sizeof(struct nl_choice_point)/sizeof(CPtr))

#define nlcp_pcreg(b)		((NLChoice)(b))->next_clause
#define nlcp_ebreg(b)		((NLChoice)(b))->ebreg
#define nlcp_hreg(b)		((NLChoice)(b))->hreg
#define nlcp_trreg(b)		((NLChoice)(b))->trreg
#define nlcp_cpreg(b)		((NLChoice)(b))->cpreg
#define nlcp_ereg(b)		((NLChoice)(b))->ereg
#define nlcp_subgoal_ptr(b)	((NLChoice)(b))->subgoal_ptr
#define nlcp_ptcp(b)		((NLChoice)(b))->ptcp
#define nlcp_pdreg(b)		((NLChoice)(b))->pdreg
#define nlcp_prevbreg(b)	((NLChoice)(b))->prev
#define nlcp_prevlookup(b)	((NLChoice)(b))->prevlookup
#define nlcp_trie_return(b)	((NLChoice)(b))->trie_return
#ifdef KOSTIS_DEBUG
#define nlcp_end(b)		((NLChoice)(b))->end
#endif

#ifdef KOSTIS_DEBUG
#define save_nl_choicepoint(t_breg,t_ereg,subg,prevlookup,prevbreg) \
    t_breg -= NLCPSIZE; \
    nlcp_end(t_breg) = makestring("Active Subgoal Frame"); \
    nlcp_prevlookup(t_breg) = prevlookup; \
    nlcp_prevbreg(t_breg) = prevbreg; \
    nlcp_pdreg(t_breg) = delayreg; \
    nlcp_ptcp(t_breg) = ptcpreg; \
    nlcp_subgoal_ptr(t_breg) = subg; \
    nlcp_ereg(t_breg) = t_ereg; \
    nlcp_cpreg(t_breg) = cpreg; \
    nlcp_trreg(t_breg) = trreg; \
    nlcp_hreg(t_breg) = hreg; \
    nlcp_ebreg(t_breg) = ebreg; \
    nlcp_pcreg(t_breg) = (pb) &retry_active_inst; \
    nlcp_trie_return(t_breg) = NULL
#else
#define save_nl_choicepoint(t_breg,t_ereg,subg,prevlookup,prevbreg) \
    t_breg -= NLCPSIZE; \
    nlcp_prevlookup(t_breg) = prevlookup; \
    nlcp_prevbreg(t_breg) = prevbreg; \
    nlcp_pdreg(t_breg) = delayreg; \
    nlcp_ptcp(t_breg) = ptcpreg; \
    nlcp_subgoal_ptr(t_breg) = subg; \
    nlcp_ereg(t_breg) = t_ereg; \
    nlcp_cpreg(t_breg) = cpreg; \
    nlcp_trreg(t_breg) = trreg; \
    nlcp_hreg(t_breg) = hreg; \
    nlcp_ebreg(t_breg) = ebreg; \
    nlcp_pcreg(t_breg) = (pb) &retry_active_inst; \
    nlcp_trie_return(t_breg) = NULL
#endif

/*----------------------------------------------------------------------*/
/* The following CP is used for a computation that has been suspended	*/
/* on completion of another subgoal.					*/
/*----------------------------------------------------------------------*/

typedef struct compl_susp_choice_point {
    byte *next_clause;	/* the completion suspension instruction */
    CPtr ebreg;		/* environment backtrack -- top of env stack */
    CPtr hreg;		/* current top of heap */
    CPtr compsuspptr;	/* pointer to the completion_suspension_frame */
    CPtr prev;		/* lookup: previous choicepoint */
#ifdef KOSTIS_DEBUG
    Cell end;
#endif
} *ComplSuspChoice;

#define COMPL_SUSP_CP_SIZE	\
	(sizeof(struct compl_susp_choice_point)/sizeof(CPtr))

#define cs_pcreg(b)		((ComplSuspChoice)(b))->next_clause
#define cs_ebreg(b)		((ComplSuspChoice)(b))->ebreg
#define cs_hreg(b)		((ComplSuspChoice)(b))->hreg
#define cs_compsuspptr(b)	((ComplSuspChoice)(b))->compsuspptr
#define cs_prevbreg(b)		((ComplSuspChoice)(b))->prev
#ifdef KOSTIS_DEBUG
#define	cs_end(b)		((ComplSuspChoice)(b))->end
#endif

#ifdef KOSTIS_DEBUG
#define save_compl_susp_cp(t_breg,prev,compsuspptr) \
    t_breg -= COMPL_SUSP_CP_SIZE; \
    cs_end(t_breg) = makestring("Completion Suspension Choice Point"); \
    cs_prevbreg(t_breg) = prev; \
    cs_compsuspptr(t_breg) = compsuspptr;\
    cs_hreg(t_breg) = hreg; \
    cs_ebreg(t_breg) = ebreg; \
    cs_pcreg(t_breg) = (pb) &completion_suspension_inst
#else
#define save_compl_susp_cp(t_breg,prev,compsuspptr) \
    t_breg -= COMPL_SUSP_CP_SIZE; \
    cs_prevbreg(t_breg) = prev; \
    cs_compsuspptr(t_breg) = compsuspptr;\
    cs_hreg(t_breg) = hreg; \
    cs_ebreg(t_breg) = ebreg; \
    cs_pcreg(t_breg) = (pb) &completion_suspension_inst
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
    for (ii = 1; ii <= arity; ii++) bld_copy0(--t_breg, cell(regbase+ii))


#define restore_registers(t_breg, arity, ii, regbase) \
    t_breg += CP_SIZE; \
    for (ii = arity; ii >= 1; ii--) bld_copy0(regbase+ii, cell(t_breg++))


#define table_restore_registers(t_breg, arity, ii, regbase) \
    t_breg += TCP_SIZE; \
    for (ii = arity; ii >= 1; ii--) bld_copy0(regbase+ii, cell(t_breg++))


#define STARTTRREG xtemp1
#define ENDTRREG xtemp2



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

#define freeze_and_switch_envs(tbreg, CPsize)\
    if (bfreg > breg) {\
      bfreg = breg + CPsize;\
      if (trfreg < trreg)  trfreg = trreg;  \
      if (hfreg < hreg)  hfreg = hreg; \
      xtemp1 = top_of_localstk; \
      if (efreg > xtemp1) efreg = xtemp1;\
    }\
    switch_envs(tbreg)

#define switch_envs(tbreg)	{\
    STARTTRREG = (CPtr) trreg;\
    ENDTRREG = (CPtr) cp_trreg(tbreg);\
    trreg = cp_trreg(tbreg);\
    while (STARTTRREG != ENDTRREG) {\
      while (STARTTRREG > ENDTRREG) {\
	untrail((CPtr) cell(STARTTRREG-2));\
	STARTTRREG = (CPtr) cell(STARTTRREG);\
      }\
      while (ENDTRREG > STARTTRREG) {\
	ENDTRREG = (CPtr) cell(ENDTRREG);\
      }\
    }\
    ENDTRREG = (CPtr) trreg;\
    while (STARTTRREG < ENDTRREG) {\
      cell((CPtr) cell(ENDTRREG-2)) = (Cell) cell(ENDTRREG-1);\
      ENDTRREG = (CPtr) cell(ENDTRREG);\
    }\
  }

/*
 *  Set ebreg to the topmost env frame of those pointed to by ereg or efreg.
 */
#define save_find_locx(t_ereg) \
    if (efreg_on_top(t_ereg)) ebreg = efreg;\
    else if (ereg_on_top(t_ereg)) ebreg = t_ereg - *(cpreg-2*sizeof(Cell)+3) + 1;


#define restore_some_wamregs(t_breg, t_ereg) \
    if (hbreg >= hfreg) hreg = hbreg; \
	else hreg = hfreg; \
    cpreg = cp_cpreg(t_breg); \
    t_ereg = cp_ereg(t_breg)
