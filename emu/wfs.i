/* File:      wfs.i
** Author(s): Kostis Sagonas
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

/*----------------------------------------------------------------------*/

#define unvisit(leader) \
    for (csf = leader; csf >= (ComplStackFrame)openreg; csf--) \
      compl_visited(csf) = FALSE

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#define edge_alloc_chunk_size	(1024 * sizeof(struct ascc_edge))

static char *edge_space_chunk_ptr = NULL;

static EPtr free_edges = NULL, free_edge_space = NULL, top_edge_space = NULL;

void abolish_edge_space(void)
{
    char *t;

    while (edge_space_chunk_ptr) {
      t = *(char **)edge_space_chunk_ptr;
      free(edge_space_chunk_ptr);
      edge_space_chunk_ptr = t;
    }
    free_edges = free_edge_space = top_edge_space = NULL;
}

static EPtr alloc_more_edge_space(void)
{
    char *t;

    if ((t = (char *)malloc(edge_alloc_chunk_size+sizeof(Cell))) == NULL)
      xsb_abort("No space to allocate more edges for SCC detection");
    *(char **)t = edge_space_chunk_ptr;
    edge_space_chunk_ptr = t;
    free_edge_space = (EPtr)(t+sizeof(Cell));
    top_edge_space = (EPtr)(t+edge_alloc_chunk_size+sizeof(Cell));
    return free_edge_space++;
}

#define Free_Edge(theEdge) \
    next_edge(theEdge) = free_edges; \
    free_edges = theEdge

#define New_Edge(theEdge,FromEptr,ToNode) \
    if (free_edges) {\
      theEdge = free_edges; \
      free_edges = next_edge(free_edges); \
    } else { \
      if (free_edge_space < top_edge_space) { \
        theEdge = free_edge_space++; \
      } else { \
        theEdge = alloc_more_edge_space(); \
      } \
    } \
    edge_to_node(theEdge) = (ComplStackFrame) ToNode; \
    next_edge(theEdge) = FromEptr; \
    FromEptr = theEdge

#define add_ascc_edges(subgoal,cs_frame,leader_cs_frame)	\
    csf2 = (ComplStackFrame)subg_compl_stack_ptr(subgoal);	\
    if (leader_cs_frame >= csf2  && !is_completed(subgoal)) {	\
      New_Edge(eptr, compl_DGT_edges(cs_frame), csf2);		\
      New_Edge(eptr, compl_DG_edges(csf2), cs_frame);		\
    }

static void reclaim_edge_space(ComplStackFrame csf_ptr)
{
    EPtr e, eptr;

    e = compl_DG_edges(csf_ptr);
    while (e != NULL) {
      eptr = e;
      e = next_edge(e);
      Free_Edge(eptr);
    }
    compl_DG_edges(csf_ptr) = NULL;
    e = compl_DGT_edges(csf_ptr);
    while (e != NULL) {
      eptr = e;
      e = next_edge(e);
      Free_Edge(eptr);
    }
    compl_DGT_edges(csf_ptr) = NULL;
}

/*----------------------------------------------------------------------*/
/* These macros abstract the fact that SLG-WAM and CHAT construct the	*/
/* subgoal dependency graph by looking at different data structures.	*/
/*----------------------------------------------------------------------*/

#ifdef CHAT
#define first_consumer(SUBG,CSF) (compl_cons_copy_list(CSF))
#define ptcp_of_gen(SUBG,CSF)	 ((SGFrame)(compl_ptcp(CSF)))
#define ptcp_of_cons(CONS_CP) \
	((SGFrame)(nlcp_ptcp((&chat_get_cons_start((chat_init_pheader)CONS_CP)))))
#define ptcp_of_csusp(CSUSP_CP) \
	((SGFrame)(csf_ptcp((&chat_get_cons_start((chat_init_pheader)CSUSP_CP)))))
#else
#define first_consumer(SUBG,CSF) (subg_asf_list_ptr(SUBG))
#define ptcp_of_gen(SUBG,CSF)	 ((SGFrame)(tcp_ptcp(subg_cp_ptr(SUBG))))
#define ptcp_of_cons(CONS_CP)	 ((SGFrame)(nlcp_ptcp(CONS_CP)))
#define ptcp_of_csusp(CSUSP_CP)	 ((SGFrame)(csf_ptcp(CSUSP_CP)))
#endif

/*----------------------------------------------------------------------*/

static void construct_dep_graph(ComplStackFrame leader_compl_frame)
{
    EPtr eptr;
    CPtr asf, nsf;
    SGFrame p, source_subg;
    ComplStackFrame csf1, csf2;

    csf1 = leader_compl_frame;
    while (csf1 >= (ComplStackFrame)openreg) {
      source_subg = compl_subgoal_ptr(csf1);
      if (!is_completed(source_subg)) {
	/*--- find the edge of the DepGraph due to the generator node ---*/
	if ((p = ptcp_of_gen(source_subg,csf1)) != NULL) {
	  add_ascc_edges(p, csf1, leader_compl_frame);
	}
	/*--- find the edges of the DepGraph due to the consumers ---*/
	for (asf = first_consumer(source_subg,csf1);
	     asf != NULL; asf = nlcp_prevlookup(asf)) {
	  if ((p = ptcp_of_cons(asf)) != NULL) {
	    add_ascc_edges(p, csf1, leader_compl_frame);
	  }
	}
	/*--- find the edges of the DepGraph due to the suspended nodes ---*/
	for (nsf = subg_compl_susp_ptr(source_subg);
	     nsf != NULL; nsf = csf_prevcsf(nsf)) {
	  if ((p = ptcp_of_csusp(nsf)) != NULL) {
	    add_ascc_edges(p, csf1, leader_compl_frame);
	  }
	}
      }
      csf1 = (ComplStackFrame)next_compl_frame(csf1);
    }
#ifdef VERBOSE_COMPLETION
    fprintf(stderr, "! Constructed the edges of the DepGraph\n");
#endif
}

/*----------------------------------------------------------------------*/
/* The following function handles negation for Batched Scheduling.	*/
/*----------------------------------------------------------------------*/

static void batched_compute_wfs(CPtr leader_compl_frame, 
				CPtr leader_breg, 
				CPtr leader_subg)
{  
  CPtr ComplStkFrame; /* CopyFrame */
  bool sccs_needed;
  SGFrame curr_subg;
#if (!defined(CHAT))
  CPtr cont_breg = leader_breg;
#endif

  /* Perform a check whether exact completion is needed.  For subgoals
     that are already marked as (early) completed, make sure their
     completion suspension frames are not taken into account and the
     memory occupied by their chat areas is properly reclaimed. */
  sccs_needed = FALSE;
  ComplStkFrame = leader_compl_frame;
  while (ComplStkFrame >= openreg) {
    curr_subg = compl_subgoal_ptr(ComplStkFrame);
    if (is_completed(curr_subg)) {
#ifdef CHAT
      chat_free_compl_susp_chat_areas(curr_subg);
#else
      subg_compl_susp_ptr(curr_subg) = NULL;
#endif
    } else {
      if (subg_compl_susp_ptr(curr_subg) != NULL) sccs_needed = TRUE;
    }
    ComplStkFrame = next_compl_frame(ComplStkFrame);
  }

  if (sccs_needed) {
#if (!defined(CHAT))
    bool found;
#endif
    CPtr nsf;
    CPtr CopyFrame;
    ComplStackFrame csf, max_finish_csf;
    bool non_lrd_stratified;

    /* NOTE: many of the following assume that leader_compl_frame
     * remains unchanged */
    /**********************************************************************/

#ifdef VERBOSE_COMPLETION
    fprintf(stderr,
	    "\t===> SCC detection is needed...(%d subgoals in ASCC)...\n",
	    (int)((leader_compl_frame-openreg)/COMPLFRAMESIZE+1));
    print_completion_stack();
#endif

    construct_dep_graph((ComplStackFrame)leader_compl_frame);
#ifdef COMPLETION_DEBUG
    print_completion_stack();
#endif

    max_finish_csf = DFS_DGT((ComplStackFrame)leader_compl_frame);
#ifdef COMPLETION_DEBUG
    fprintf(stderr,"! MAX FINISH_SUBGOAL AT COMPL STACK: %p\n",max_finish_csf);
#endif
    /* mark as not visited all subgoals in the completion stack
     * below leader_compl_frame */
    unvisit((ComplStackFrame)leader_compl_frame);

    /* mark as visited all subgoals in the same SCC as max_finish_csf 
     * by traversing the SDG */
    find_independent_scc(max_finish_csf);
    
#ifdef COMPLETION_DEBUG
    print_completion_stack();
#endif

    /* Perform a LRD stratification check of the program-query pair	*/
    /* and classify the completion suspensions as stratified or not.	*/
    ComplStkFrame = leader_compl_frame;
    non_lrd_stratified = FALSE;
    
    while (ComplStkFrame >= openreg) {
      CPtr    susp_csf;
      SGFrame susp_subgoal;

      curr_subg = compl_subgoal_ptr(ComplStkFrame);
      if (!is_completed(curr_subg)) {
	if (compl_visited(ComplStkFrame) != FALSE) {
	  curr_subg = compl_subgoal_ptr(ComplStkFrame);  
	  for (nsf = subg_compl_susp_ptr(curr_subg);
	       nsf != NULL; nsf = csf_prevcsf(nsf)) {
	    if ((susp_subgoal = ptcp_of_csusp(nsf)) != NULL) {
	      susp_csf = subg_compl_stack_ptr(susp_subgoal);      
	      if (!is_completed(susp_subgoal) && 
		  susp_csf <= leader_compl_frame &&
		  compl_visited(susp_csf) != FALSE) {
		/*--- The suspended subgoal is in the completable SCC ---*/
		mark_delayed(ComplStkFrame, susp_csf, nsf);
		non_lrd_stratified = TRUE;
#ifdef DELAY_DEBUG
		fprintf(stderr, "\t   Subgoal ");
		print_subgoal(stderr, (SGFrame)susp_subgoal);
		fprintf(stderr, " depends negatively on subgoal ");
		print_subgoal(stderr, curr_subg);
		fprintf(stderr, "\n");
#endif
	      } /*  no completed susp_subg */
	    }
	  } /* for each nsf */
	} /* not visited */
      } /* skip completed subgoals in the completion stack */
      ComplStkFrame = next_compl_frame(ComplStkFrame);
    } /* while */
  
/*----------------------------------------------------------------------*/
/* #define SERIOUS_DEBUGGING_NEEDED	*/
#ifdef SERIOUS_DEBUGGING_NEEDED
    print_completion_stack();
#endif
/*----------------------------------------------------------------------*/
    /*--- We have to find the continuation.  It is the topmost ---*/
    /*--- subgoal of the ASCC that will not be completed. ---*/
    if (non_lrd_stratified == FALSE) {
#if (!defined(CHAT))
      found = FALSE;
      for (ComplStkFrame = openreg;
	   !found && ComplStkFrame <= leader_compl_frame;
	   ComplStkFrame = prev_compl_frame(ComplStkFrame)) {
	if (compl_visited(ComplStkFrame) <= FALSE) {
	  cont_breg = subg_cp_ptr(compl_subgoal_ptr(ComplStkFrame));
	  breg = cont_breg;
#ifdef VERBOSE_COMPLETION /* was COMPLETION_DEBUG */
	  fprintf(stderr, "------ Setting TBreg to %p...\n", cont_breg);
#endif
	  found = TRUE;
	}
      }
      if (!found) {	/* case in which the ASCC contains only one SCC */
			/* and all subgoals will be completed (no delay) */
	cont_breg = tcp_prevbreg(leader_breg);
      }
#endif
    } else {	/* the chosen SCC has a loop through negation */
      for (ComplStkFrame = openreg; ComplStkFrame <= leader_compl_frame;
	   ComplStkFrame = prev_compl_frame(ComplStkFrame)) {
	if (compl_visited(ComplStkFrame) != FALSE)
	  compl_visited(ComplStkFrame) = DELAYED;
      }
#if (!defined(CHAT))
      cont_breg = subg_cp_ptr(compl_subgoal_ptr(leader_compl_frame));
      breg = cont_breg;
#ifdef VERBOSE_COMPLETION /* was COMPLETION_DEBUG */
      fprintf(stderr, "------ Setting TBreg to %p...\n", cont_breg);
#endif
#endif
    }
    
    /*--- Now complete the subgoals of the chosen SCC ---*/
    for (ComplStkFrame = leader_compl_frame; ComplStkFrame >= openreg;
	 ComplStkFrame = next_compl_frame(ComplStkFrame)) {
      if (compl_visited(ComplStkFrame) != FALSE) {
	curr_subg = compl_subgoal_ptr(ComplStkFrame);
	if (compl_visited(ComplStkFrame) != DELAYED) {
	  mark_as_completed(curr_subg);
	  reclaim_subg_space(curr_subg);
	  if (neg_simplif_possible(curr_subg)) {
	    simplify_neg_fails(curr_subg);
	  }
	}
	
	/* If there are any completion suspensions for this subgoal. */
	if ((nsf = subg_compl_susp_ptr(curr_subg)) != NULL) {
#ifdef CHAT
	  CPtr H, EB;

	  H = cp_hreg(breg);
	  EB = cp_ebreg(breg);
#ifdef Chat_DEBUG
fprintf(stderr, "leader_cp = %p, subgoal = %p, eb = %d\n",
	breg, tcp_subgoal_ptr(breg), ((CPtr)glstack.high - 1) - EB);
#endif
#else
	  CPtr min_breg;
	  
	  set_min(min_breg, breg, bfreg);
#endif
	  if (compl_visited(ComplStkFrame) != DELAYED) {
#ifdef CHAT
	    breg = chat_restore_compl_susp((chat_init_pheader)nsf, H, EB);
#else
	    save_compl_susp_cp(min_breg, cont_breg, nsf);
	    breg = min_breg;
#endif
	    /*-- forget these completion suspensions --*/
	    subg_compl_susp_ptr(curr_subg) = NULL;
#ifdef VERBOSE_COMPLETION
	    fprintf(stderr, "------ Setting Breg to %p...\n", breg);
#endif
	  } else {	/* unsuspend only those suspensions that are delayed */
	    CPtr dnsf = NULL, ndnsf = NULL;
	    CPtr head_dnsf = NULL, head_ndnsf = NULL;
	    while (nsf != NULL) {	/* partition into two lists */
	      if (csf_neg_loop(nsf) == FALSE) {
		if (ndnsf == NULL) head_ndnsf = nsf; 
		else csf_prevcsf(ndnsf) = nsf;
		ndnsf = nsf;
		nsf = csf_prevcsf(nsf);
		csf_prevcsf(ndnsf) = NULL;
	      } else {
		if (dnsf == NULL) head_dnsf = nsf; 
		else csf_prevcsf(dnsf) = nsf;
		dnsf = nsf;
		nsf = csf_prevcsf(nsf);
		csf_prevcsf(dnsf) = NULL;
	      }
	    }
	    if (head_dnsf != NULL) {
#ifdef CHAT
	      breg = chat_restore_compl_susp((chat_init_pheader)head_dnsf, H, EB);
#else
	      save_compl_susp_cp(min_breg, cont_breg, head_dnsf);
	      breg = min_breg;
#endif
	    }
	    subg_compl_susp_ptr(curr_subg) = head_ndnsf;
	  }
#if (!defined(CHAT))
	  cont_breg = breg; /* So that other Compl_Susp_CPs can be saved. */
#endif
	}
      }
    }
#ifdef COMPLETION_DEBUG
    fprintf(stderr, "------ Completed the chosen SCC...\n");
#endif
/*----------------------------------------------------------------------*/
    
    /*--- Finally, compact the Completion Stack ---*/
    ComplStkFrame = CopyFrame = leader_compl_frame; 
    while (ComplStkFrame >= openreg) {
      curr_subg = compl_subgoal_ptr(ComplStkFrame);
      reclaim_edge_space((ComplStackFrame)ComplStkFrame);
      if (!is_completed(curr_subg)) {
	subg_compl_stack_ptr(curr_subg) = CopyFrame;
	compact_completion_frame(CopyFrame, ComplStkFrame, curr_subg);
      } else { /* this may be done 2x! */
	reclaim_subg_space(curr_subg);
      }
      ComplStkFrame = next_compl_frame(ComplStkFrame);
    }
    openreg = prev_compl_frame(CopyFrame);

#if (!defined(CHAT))
    /* chain remaining TCPs in the choice point stack */
    /* for CHAT this does not make sense as these TCPs are reclaimed */
    tcp_prevbreg(subg_cp_ptr(compl_subgoal_ptr(leader_compl_frame))) = 
      tcp_prevbreg(subg_cp_ptr(leader_subg));
    for (ComplStkFrame = next_compl_frame(leader_compl_frame);
	 ComplStkFrame >= openreg;
	 ComplStkFrame = next_compl_frame(ComplStkFrame)){
      tcp_prevbreg(subg_cp_ptr(compl_subgoal_ptr(ComplStkFrame))) = 
	subg_cp_ptr(compl_subgoal_ptr(prev_compl_frame(ComplStkFrame)));
    }
#endif
  } /* if sccs_needed */
  else { /* sccs not needed */
    ComplStkFrame = leader_compl_frame;
    while (ComplStkFrame >= openreg) {
      curr_subg = compl_subgoal_ptr(ComplStkFrame);
      mark_as_completed(curr_subg);
      if (neg_simplif_possible(curr_subg)) {
	simplify_neg_fails(curr_subg);
      }
      ComplStkFrame = next_compl_frame(ComplStkFrame);
    }
    
    ComplStkFrame = leader_compl_frame;
    while (ComplStkFrame >= openreg) {
      curr_subg = compl_subgoal_ptr(ComplStkFrame);
      reclaim_subg_space(curr_subg);
      ComplStkFrame = next_compl_frame(ComplStkFrame);
    }
    /* point openreg to first empty space */
    openreg = prev_compl_frame(leader_compl_frame);	
  }
  
#ifdef COMPLETION_DEBUG
  fprintf(stderr, "------ Completed an ASCC...\n");
#endif
} /* compute_wfs() */

/*----------------------------------------------------------------------*/
