/* File:      complete_local.h
** Author(s): Juliana Freire, Kostis Sagonas, Terry Swift, Luis Castro
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-2001
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
#ifndef __COMPLETE_LOCAL_H__
#define __COMPLETE_LOCAL_H__

#ifdef LOCAL_EVAL
#ifndef CHAT
static inline int  ScheduleNonLeaderGenerator(VariantSF subgoal) 
{
  int i;
  Cell ARITY;

  if (tcp_trie_return(breg) == NULL) { 
    /* this can only happen if answers are deleted */ 
    tcp_tag(breg) = CHECK_COMPLETE_TAG; 
  }
  else { /* This code mimics the answer_return code */
    CPtr answer_template;
    int template_size, attv_num, tmp;
    ALNptr answer_set;
    BTNptr answer_leaf;
    
    restore_some_wamregs(breg,ereg); 
    answer_set = ALN_Next(tcp_trie_return(breg));   /* get next answer */ 
    if ( IsNonNULL(answer_set) ) {
      tcp_trie_return(breg) = answer_set;   /* update answer continuation */
      ARITY = tcp_arity(breg); 
      answer_template = breg + TCP_SIZE + ARITY;
      
      tmp = int_val(cell(answer_template));
      get_var_and_attv_nums(template_size, attv_num, tmp);
      answer_template += template_size;
      answer_leaf = ALN_Answer(answer_set);
      table_consume_answer(answer_leaf,template_size,attv_num,answer_template,
			   subg_tif_ptr(subgoal));
      
      /*
       * This piece of code was added in (version 1.8.1) to handle
       * properly variables in delay lists.  Itworks in a manner
       * similar to answer_return (and lay down consumer, for that
       * matter).  In order to save the
       * substitution factor of a conditional answer into the delay list 
       * for the root subgoal, we have to get it from var_addr[] and
       * num_heap_term_vars (both set by table_consume_answer()).
       */
      if (is_conditional_answer(ALN_Answer(tcp_trie_return(breg)))) { 
#ifdef DEBUG_DELAYVAR
	fprintf(stderr, ">>>> delay_positively in check_complete\n");
#endif
	{
	  if (num_heap_term_vars == 0) {
	    delay_positively(subgoal, ALN_Answer(tcp_trie_return(breg)),
			     makestring(get_ret_string()));
	  }
	  else {
#ifndef IGNORE_DELAYVAR
	    CPtr temp_hreg = hreg;
	    new_heap_functor(hreg, get_ret_psc(num_heap_term_vars));
	    for (i = 0; i < num_heap_term_vars; i++)
	      cell(hreg++) = (Cell) var_addr[i];
	    delay_positively(subgoal, ALN_Answer(tcp_trie_return(breg)),
			     makecs(temp_hreg));
#else
	    delay_positively(subgoal, ALN_Answer(tcp_trie_return(breg)),
			     makestring(get_ret_string()));
#endif /* IGNORE_DELAYVAR */
	  }
	}
      }
      return 1;
    } 
    else { 
      tcp_tag(breg) = CHECK_COMPLETE_TAG;      
    }
  } 
  return 0;
}
#endif /* CHAT */
#endif /* LOCAL */

#ifdef PROFILE
#define ProfileLeader \
  { \
    int num_subgoals = 0; \
    int num_completed = 0; \
    int num_consumers_in_ascc = 0; \
    int num_compl_susps_in_ascc = 0; \
    int leader_level; \
    CPtr profile_CSF = cs_ptr; \
    VariantSF prof_compl_subg; \
\
    leader_level = compl_level(profile_CSF); \
\
    /* check from leader up to the youngest subgoal */ \
    while (profile_CSF >= openreg) { \
      num_subgoals++; \
      prof_compl_subg = compl_subgoal_ptr(profile_CSF); \
      if (is_completed(prof_compl_subg)) { /* this was early completed */ \
	num_completed++; \
      } \
      else { \
	CPtr nsf; \
	nsf = subg_asf_list_ptr(prof_compl_subg); \
	while (nsf != NULL) { \
	  num_consumers_in_ascc++; \
	  nsf = nlcp_prevlookup(nsf); \
	  } \
	nsf = subg_compl_susp_ptr(prof_compl_subg); \
	while (nsf != NULL) { \
	  num_compl_susps_in_ascc++; \
	  nsf = csf_prevcsf(nsf); \
	  } \
      } \
      profile_CSF = next_compl_frame(profile_CSF); \
    } \
    if (num_subgoals > max_subgoals) { max_subgoals = num_subgoals; } \
    if (num_completed > max_completed) { max_completed = num_completed; } \
    if (num_consumers_in_ascc > max_consumers_in_ascc) { \
      max_consumers_in_ascc = num_consumers_in_ascc; \
    } \
    if (num_compl_susps_in_ascc > max_compl_susps_in_ascc) { \
      max_compl_susps_in_ascc = num_compl_susps_in_ascc; \
    } \
    if (flags[PROFFLAG] > 2) { \
      fprintf(stdmsg, \
              "p(lev(%d),lead(%d),subg(%d),ec(%d),cons(%d),cs(%d)).\n", \
	      level_num,leader_level,num_subgoals,num_completed, \
	     num_consumers_in_ascc,num_compl_susps_in_ascc); \
    } \
  }
#else
#define ProfileLeader
#endif

#ifdef LOCAL_EVAL
#ifdef CHAT
#define DisposeOfComplSusp(subgoal) \
        chat_free_compl_susp_chat_areas(subgoal)
#define DeleteCSF(nsf) \
        chat_free_compl_susp_chat_area((chat_init_pheader) nsf)
#define ResumeCSFs() \
{ \
  CPtr H, EB; \
  H = cp_hreg(breg); \
  EB = cp_ebreg(breg); \
  breg = cc_tbreg = \
    chat_restore_compl_susp((chat_init_pheader)nsf, H, EB); \
  subg_compl_susp_ptr(compl_subg) = NULL; \
}
#else
#define DisposeOfComplSusp(subgoal) \
        subg_compl_susp_ptr(subgoal) = NULL
#define DeleteCSF(nsf) \
        csf_prevcsf(nsf)
#define ResumeCSFs() \
{ \
  CPtr nsftmp; \
  if (!cur_breg) { \
    cur_breg = cc_tbreg = nsf; \
  } else { \
    csf_prevcsf(cur_breg) = nsf; \
    cur_breg = nsf; \
  } \
  for (nsftmp = cur_breg; csf_prevcsf(nsftmp); nsftmp = csf_prevcsf(nsftmp)) {\
    cs_pcreg(nsftmp) = (pb) &resume_compl_suspension_inst; \
    cs_hreg(nsftmp) = hreg; \
    cs_ebreg(nsftmp) = ebreg; \
  } \
  cs_pcreg(nsftmp) = (pb) &resume_compl_suspension_inst; \
  cs_hreg(nsftmp) = hreg; \
  cs_ebreg(nsftmp) = ebreg; \
  csf_prevcsf(nsftmp) = breg; \
  cur_breg = nsftmp; \
  subg_compl_susp_ptr(compl_subg) = NULL; \
}
  
#endif

static inline CPtr ProcessSuspensionFrames(CPtr cc_tbreg_in, CPtr cs_ptr)
{
  CPtr ComplStkFrame = cs_ptr;
  VariantSF compl_subg;
  CPtr cc_tbreg = cc_tbreg_in;
#ifndef CHAT
  CPtr cur_breg = NULL; /* tail of chain of nsf's; used in ResumeCSFs */
#endif

  /* check from leader up to the youngest subgoal */
  while (ComplStkFrame >= openreg) {
    compl_subg = compl_subgoal_ptr(ComplStkFrame);
    /* TLS: I think the following could also be done at early completion
     * (ans-return), though I'm not sure there's an advantage either
     * way
     */
    if (is_completed(compl_subg)) { /* this was early completed */
      DisposeOfComplSusp(compl_subg);
    }
    else { /* if not early completed */
      CPtr nsf;
      if ((nsf = subg_compl_susp_ptr(compl_subg)) != NULL) { 
	CPtr p, prev_nsf = NULL;
	/* 
	   check each suspension frame for appropriate action: if
	   their root subgoals are completed these completion
	   suspensions fail, so forget about them; o/w delay them
	   and let simplification take care of the rest
	*/
	while (nsf) {
	  if ((p = csf_ptcp(nsf)) != NULL) {
	    if (is_completed(p)) {
	      if (!prev_nsf) { /* deleting the first susp is special */
		nsf = subg_compl_susp_ptr(compl_subg) = DeleteCSF(nsf);
	      }
	      else {
		nsf = csf_prevcsf(prev_nsf) = DeleteCSF(nsf);
	      }
	    }
	    else { /* this completion suspension will be delayed */
	      mark_delayed(ComplStkFrame, subg_compl_stack_ptr(p), nsf);
	      prev_nsf = nsf;
	      nsf = csf_prevcsf(nsf);
	    }
	  }
	} /* while */

	if ((nsf = subg_compl_susp_ptr(compl_subg))) {
	  ResumeCSFs();
	}
      } /* if there are completion suspensions */
    } /* else if not early completed */
    ComplStkFrame = next_compl_frame(ComplStkFrame);
  } /* while - for each subg in compl stack */
  return cc_tbreg;
}

static inline void CompleteSimplifyAndReclaim(CPtr cs_ptr)
{
  VariantSF compl_subg;
  CPtr ComplStkFrame = cs_ptr; 

  /* mark all SCC as completed and do simplification also, reclaim
     space for all but the leader */

  while (ComplStkFrame >= openreg) {
    compl_subg = compl_subgoal_ptr(ComplStkFrame);
    mark_as_completed(compl_subg); 
    if (neg_simplif_possible(compl_subg)) {
      simplify_neg_fails(compl_subg);
    }

    ComplStkFrame = next_compl_frame(ComplStkFrame);
  } /* while */
  
  /* reclaim all answer lists, but the one for the leader */
  ComplStkFrame = next_compl_frame(cs_ptr);
  while (ComplStkFrame >= openreg) {
    compl_subg = compl_subgoal_ptr(ComplStkFrame);
    reclaim_incomplete_table_structs(compl_subg);
    ComplStkFrame = next_compl_frame(ComplStkFrame);
  } /* while */
}

static inline void SetupReturnFromLeader(CPtr orig_breg, CPtr cs_ptr, VariantSF subgoal)
{
  CPtr answer_template;
#ifndef CHAT
  Cell ARITY;
#endif
  int template_size, attv_num, tmp;

#if (!defined(CHAT))
  tcp_tag(orig_breg) = CHECK_COMPLETE_TAG; 
#endif
  switch_envs(orig_breg); 
  /* check where this brings the stacks, that will determine how
     much can be reclaimed if there are answers to be returned */
  ptcpreg = tcp_ptcp(orig_breg);
  delayreg = tcp_pdreg(orig_breg);
  restore_some_wamregs(orig_breg, ereg);
  /* restore_trail_condition_registers - because success path
   * will be followed
   */
  ebreg = cp_ebreg(tcp_prevbreg(orig_breg));
  hbreg = cp_hreg(tcp_prevbreg(orig_breg));
#ifdef CHAT
  compl_cons_copy_list(cs_ptr) = 0;
#else
  subg_asf_list_ptr(subgoal) = 0;
#endif
  
  /* reclaim stacks, including leader */
  openreg = prev_compl_frame(cs_ptr);
  reclaim_stacks(orig_breg);
#ifdef CHAT
  tmp = int_val(cell(compl_hreg(cs_ptr)));
  get_var_and_attv_nums(template_size, attv_num, tmp);
  answer_template = compl_hreg(cs_ptr) - template_size;
#else
  ARITY = tcp_arity(orig_breg);
  answer_template = orig_breg + TCP_SIZE + ARITY;
  tmp = int_val(cell(answer_template));
  get_var_and_attv_nums(template_size, attv_num, tmp);
  answer_template++;
#endif
  /* Now `answer_template' points to the mth term */
  /* Initialize var_regs[] as the attvs in the call. */
  num_vars_in_var_regs = -1;
  if (attv_num > 0) {
    CPtr cptr;
    for (cptr = answer_template + template_size - 1;
	 cptr >= answer_template; cptr--) {
      if (isattv(cell(cptr)))
	var_regs[++num_vars_in_var_regs] = (CPtr) cell(cptr);
    }
    /* now num_vars_in_var_regs should be attv_num - 1 */
  }
  
  reg_arrayptr = reg_array - 1;
  for (tmp = 0; tmp < template_size; tmp++) {
    CPtr cptr = answer_template;
    pushreg(*cptr);
    answer_template++;
  }
  /* backtrack to prev tabled subgoal after returning answers */
  breg = tcp_prevbreg(orig_breg);
  delay_it = 1; /* run delay lists, don't construct them */
}

#endif /* LOCAL_EVAL */
#endif /* __COMPLETE_LOCAL_H__ */
