/* File:      bineg.i
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
#include "debugs/debug_kostis.h"



/*----------------------------------------------------------------------*/
/* Contains builtin predicates for SLG negation (and tfindall/3).	*/
/*----------------------------------------------------------------------*/

    case SLG_NOT:	/* reg1: +SubgoalPtr */
	subgoal_ptr = (CPtr) ptoc_int(1);
	if (has_no_answers(subgoal_ptr) &&
	    (is_completed(subgoal_ptr) || neg_delay == FALSE)) {
	  return TRUE;
	}

        if (has_unconditional_answers((SGFrame)subgoal_ptr)) {
	  return FALSE;
	} else {
	  delay_negatively(subgoal_ptr);
	  return TRUE;
	}

/*----------------------------------------------------------------------*/

    case IS_INCOMPLETE:  /* reg1: +term; reg2: +SubgoalPtr;	*/
			 /* reg3: +PTCP; reg4: +Usage; reg5: -SubgPtr */
	check_glstack_overflow(5,pcreg,OVERFLOW_MARGIN);
	term = ptoc_tag(1);
	subgoal_ptr = (CPtr) ptoc_int(2);
	t_ptcp = (CPtr)((pb)tcpstack.high - ptoc_int(3)) ;
	if( t_ptcp == (CPtr)tcpstack.high )
		t_ptcp = NULL ;
	usage = ptoc_int(4);	/* usage =:= 0 -> tfindall ; negation */
	psc = term_psc(term);
	arity = get_arity(psc);
	if (subgoal_ptr == NULL) {
	  if ((tip = get_tip(psc)) == NULL) {
	    sprintf(message, "Predicate %s/%d is not tabled",
			     get_name(psc), arity);
	    xsb_abort(message);
	    return FALSE;
	  }
	  subgoal_ptr = ti_call_trie_root(tip);
	  get_subgoal_ptr(term, arity, (CPtr)&subgoal_ptr);
	}
	ctop_int(5, (Integer)subgoal_ptr);
#ifdef KOSTIS_DEBUG
	fprintf(stderr, "Is incomplete for ");
	print_subgoal(stderr, (SGFrame)subgoal_ptr);
	fprintf(stderr, ", (%x)\n", (int)&subg_ans_root_ptr(subgoal_ptr));
#endif
	if (is_completed(subgoal_ptr)) {
	  neg_delay = FALSE;
	  ptcpreg = t_ptcp;  /* restore ptcpreg as the compl. suspens. would */
	  return TRUE;	/* succeed */
	}
	else {	/* subgoal is not completed; save a completion suspension */
	  adjust_level(subg_compl_stack_ptr(subgoal_ptr));
	  save_find_locx(ereg);
	  efreg = ebreg;
	  if (trreg > trfreg) trfreg = trreg;
	  if (hfreg < hreg) hfreg = hreg;
	  if (bfreg > breg) bfreg = breg;
	/*  check_stack_overflow(bfreg, pcreg, (byte *)pcreg);	*/
	  save_registers(bfreg, arity, i, (CPtr) cs_val(term));
#ifdef DEBUG_DELAY
	  fprintf(stderr, "... Saving a completion suspension (~");
	  print_subgoal(stderr, (SGFrame)subgoal_ptr);
	  fprintf(stderr, " in the body of ");
	  if (t_ptcp != NULL) {
	    print_subgoal(stderr, (SGFrame)tcp_subgoal_ptr(t_ptcp));
	  } else fprintf(stderr, "an UNTABLED predicate");
	  fprintf(stderr, ")\n");
#endif
	/*--- Changed the last argument from cpreg to pcreg on 7 March 95 ---*/
	  save_compl_susp_frame(bfreg,ereg,subgoal_ptr,t_ptcp,usage,pcreg);
	  subg_compl_susp_ptr(subgoal_ptr) = bfreg;
	  return FALSE;
	}

/*----------------------------------------------------------------------*/

    case GET_PTCP:
	if( ptcpreg != NULL )
		ctop_int(1, (pb)tcpstack.high-(pb)ptcpreg);
	else    ctop_int(1, (Integer)0);
	break;

/*----------------------------------------------------------------------*/

    case GET_DELAY_LISTS:
	as_leaf = (NODEptr) ptoc_int(1);
	delay_lists = ptoc_tag(2);

	if (is_conditional_answer(as_leaf)) {
/*	  fprintf(stderr, " Conditional answer with DL = ");	*/
	  bind_list((CPtr)delay_lists, hreg);
	  for (dls = asi_idl_list((ASI) Delay(as_leaf)); dls != NULL; ) {
	    dls_head = hreg;
	    dls_tail = hreg+1;
	    new_heap_free(hreg);
	    new_heap_free(hreg);
	    idl = idlt_idl(idl_list_idl(dls));
/*	    fprint_delay_list(stderr, idl); fprintf(stderr, "\n"); */
	    build_delay_list(dls_head, idl);
	    if ((dls = idl_list_next(dls)) != NULL) {
	      bind_list(dls_tail, hreg);
	    }
	  }
	  bind_nil(dls_tail);
	} else {
	  bind_nil((CPtr)delay_lists);
	}   
	break;

/*----------------------------------------------------------------------*/

