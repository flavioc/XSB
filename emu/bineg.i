/* File:      bineg.i
** Author(s): Kostis Sagonas, Baoqiu Cui
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
			 /* reg3: +PTCP; reg4: -SubgPtr         */
	term = ptoc_tag(1);
	subgoal_ptr = (CPtr) ptoc_int(2);
#ifdef PTCP_IN_CP
	t_ptcp = (CPtr)((pb)tcpstack.high - ptoc_int(3));
	if (t_ptcp == (CPtr)tcpstack.high) t_ptcp = NULL;
#else
	t_ptcp = (CPtr) ptoc_int(3);
#endif
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
	ctop_int(4, (Integer)subgoal_ptr);
#ifdef DEBUG_DELAY
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
#ifdef CHAT_DEBUG
	  fprintf(stderr, "! Predicate is_incomplete is needed\n");
#endif
#ifdef DEBUG_DELAY
	  fprintf(stderr, "... Saving a completion suspension (~");
	  print_subgoal(stderr, (SGFrame)subgoal_ptr);
	  fprintf(stderr, " in the body of ");
	  if (t_ptcp != NULL) {
#ifdef PTCP_IN_CP
	    print_subgoal(stderr, (SGFrame)tcp_subgoal_ptr(t_ptcp));
#else
	    print_subgoal(stderr, (SGFrame)t_ptcp);
#endif
	  } else fprintf(stderr, "an UNTABLED predicate");
	  fprintf(stderr, ")\n");
#endif
	  adjust_level(subg_compl_stack_ptr(subgoal_ptr));
	  save_find_locx(ereg);
	  reg_base = (CPtr)cs_val(term);
#ifdef CHAT
	  subg_compl_susp_ptr(subgoal_ptr) = (CPtr)
		save_a_chat_compl_susp(arity, reg_base,
				       (SGFrame)subgoal_ptr, t_ptcp, cpreg);
#else
	  efreg = ebreg;
	  if (trreg > trfreg) trfreg = trreg;
	  if (hfreg < hreg) hfreg = hreg;
	  if (bfreg > breg) bfreg = breg;
	/*  check_stack_overflow(bfreg, pcreg, (byte *)pcreg);	*/
	  save_registers(bfreg, arity, i, reg_base);
	  save_compl_susp_frame(bfreg, ereg, subgoal_ptr, t_ptcp, cpreg);
	  subg_compl_susp_ptr(subgoal_ptr) = bfreg;
#endif
	  return FALSE;
	}

/*----------------------------------------------------------------------*/

    case GET_PTCP:
#ifdef PTCP_IN_CP
	if (ptcpreg != NULL)
	  ctop_int(1, (pb)tcpstack.high-(pb)ptcpreg);
	else ctop_int(1, (Integer)0);
#else
	ctop_int(1, (Integer)ptcpreg);
#endif
	break;

/*----------------------------------------------------------------------*/

    case GET_DELAY_LISTS:

      /*
       * When GET_DELAY_LISTS is called, we can assume that the
       * corresponding tabled subgoal call has been completed and so trie
       * code will be used to return the answer (see
       * trie_get_returns_for_call()).  After the execution of trie code,
       * var_regs[] contains the substitution factor of the _answer_ to
       * the call.
       *
       * This builtin has been modified since XSB 1.8.1 to handle
       * variables in delay list.
       */

#ifdef DEBUG_DELAYVAR
      fprintf(stderr, ">>>> (at the beginning of GET_DELAY_LISTS\n");
      fprintf(stderr, ">>>> global_num_vars = %d)\n",global_num_vars);
	
      {
	int i;
	for(i = 0; i <= global_num_vars; i++){
	  Cell x;
	  fprintf(stderr, ">>>> var_regs[%d] =",i);
	  x = (Cell)var_regs[i];
	  deref(x);
	  printterm(x,1,25);
	  fprintf(stderr, "\n");
	}
      }
#endif /* DEBUG_DELAYVAR */

      as_leaf = (NODEptr) ptoc_int(1);
      delay_lists = ptoc_tag(2);
      if (is_conditional_answer(as_leaf)) {
	bind_list((CPtr)delay_lists, hreg);
	{ /*
	   * Make copy of var_regs & global_num_vars (after get_returns,
	   * which calls trie_get_returns_for_call).  (global_num_vars +
	   * 1) is the number of variables left in the answer
	   * (substitution factor of the answer)
	   *
	   * So, copy_of_var_addr[] is the substitution factor of the
	   * answer for the head predicate.
	   */
	  int i;
	  copy_of_var_addr = calloc(var_addr_arraysz, sizeof(CPtr));
	  if(copy_of_var_addr == NULL){
	    fprintf(stderr, "No enough memory to calloc copy_of_var_addr!\n");
	    xsb_exit("Bye");
	  }
	  for( i = 0; i <= global_num_vars; i++)
	    copy_of_var_addr[i] = var_regs[i];
	  
	  copy_of_num_heap_term_vars = global_num_vars + 1;
	}

	for (dl = asi_dl_list((ASI) Delay(as_leaf)); dl != NULL; ) {
	  dls_head = hreg;
	  dls_tail = hreg+1;
	  new_heap_free(hreg);
	  new_heap_free(hreg);
	  de = dl_de_list(dl);
	  /*
	   * This answer may have more than one delay list.  We have to
	   * restore copy_of_num_heap_term_vars for each of them.  But,
	   * among delay elements of each delay list, it is not necessary
	   * to restore this value.
	   *
	   * Note that global_num_vars is always set back to
	   * copy_of_num_heap_term_vars at the end of build_delay_list().
	   */
	  copy_of_num_heap_term_vars = global_num_vars + 1;
	  build_delay_list(dls_head, de);
	  if ((dl = dl_next(dl)) != NULL) {
	    bind_list(dls_tail, hreg);
	  }
	}
	bind_nil(dls_tail);
      } else {
	bind_nil((CPtr)delay_lists);
      }
      free(copy_of_var_addr);
      break;

/*----------------------------------------------------------------------*/

