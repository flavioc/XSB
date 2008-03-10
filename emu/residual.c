/* File:      residual.c
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


#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>


/* special debug includes */
#include "debugs/debug_residual.h"

#include "auxlry.h"
#include "context.h"
#include "cell_xsb.h"
#include "memory_xsb.h"
#include "psc_xsb.h"
#include "register.h"
#include "heap_xsb.h"
#include "binding.h"
#include "tries.h"
#include "macro_xsb.h"
#include "error_xsb.h"
#include "io_builtins_xsb.h"
#include "debug_xsb.h"
#include "flags_xsb.h"

/*----------------------------------------------------------------------*/

#ifdef DEBUG_DELAYVAR
#define print_trie_atom(X) {\
 if (isstring(X)) \
   printf("atom(%s)",string_val(X));\
 else if (isconstr(X)) \
   printf("atom(%s/%d)",get_name((Psc)dec_addr(X)),get_arity((Psc)dec_addr(X)));\
 else if (isinteger(X)) \
   printf("atom(%d)",int_val(X));\
 else if (isboxedinteger(X)) \
   printf("atom(%d)",boxedint_val(X));\
 else if (islist(X))\
   printf("./2");\
 else\
  printf("Unk(%x)",(int)X);\
 }
#endif

/*----------------------------------------------------------------------*/

#ifndef MULTI_THREAD
static Cell cell_array[500];
CPtr *copy_of_var_addr;
int copy_of_num_heap_term_vars;
#endif

/*----------------------------------------------------------------------*/

#define build_subgoal_args(SUBG)	\
	load_solution_trie(CTXTc arity, 0, &cell_array[arity-1], subg_leaf_ptr(SUBG))


/*
 * Function build_delay_list() is called by builtin #143 GET_DELAY_LISTS
 * to construct on the heap the delay list pointed by `de'.  Since XSB
 * 1.8.1, this function is changed to handle variables in delay list.
 * Basically, to construct a delayed subgoal, we have to go through three 
 * tries one by one: the call trie, answer trie, and delay trie of the
 * delayed subgoal.  Delay trie contains the substitution factor of the
 * answer for the delayed subgoal.
 */

void build_delay_list(CTXTdeclc CPtr delay_list, DE de)
{
  Psc  psc;
  int  i, j, arity;
  CPtr head, tail;
  VariantSF subg;
  BTNptr ans_subst;
#ifdef DEBUG_DELAYVAR
  BTNptr subs_factp;
#endif
  CPtr *tmp_var_addr;
  CPtr oldhreg = hreg;
 
  i = 0;
  if (de != NULL && !isnil(de)) {
    
    tail = hreg+1;
    bind_list(delay_list, hreg);
    hreg = hreg + 3; 
    build_delay_list(CTXTc tail, de_next(de)); /* recursive call, BUG, gc may move heap,destroying oldhreg!! */
    head = hreg;
    subg = de_subgoal(de);
    psc = TIF_PSC(subg_tif_ptr(subg));
    arity = get_arity(psc);
    if ((ans_subst = de_ans_subst(de)) == NULL) { /* Negative DE */
      follow(oldhreg) = makecs(hreg);
      new_heap_functor(head, tnot_psc);
      if (arity == 0) {
	bind_string(head, get_name(psc));
	hreg += 3;
      } else {
	sreg = head+1;
	follow(head++) = makecs(sreg);
	hreg += arity+4; /* need arity(tnot)+2+arity(psc)+1 new cells */
	new_heap_functor(sreg, psc);
	for (j = 1; j <= arity; j++) {
	  new_heap_free(sreg);
	  cell_array[arity-j] = cell(sreg-1);
	}
	build_subgoal_args(subg); 
      }
    } else {					/* Positive DE */
      if (arity == 0) {
	new_heap_string(oldhreg, get_name(psc));
      } else {
#ifdef DEBUG_DELAYVAR
	/*
	 * de_subs_fact(de) is the root of de's delay trie -- the saved
	 * substitution factor of the answer to the subgoal call of
	 * this delayed element.
	 */
	subs_factp = de_subs_fact(de);
#endif
	sreg = head;
	follow(oldhreg) = makecs(head);
	hreg += arity+1;
	new_heap_functor(sreg, psc);
	for (j = 1; j <= arity; j++) {
	  new_heap_free(sreg);
	  cell_array[arity-j] = cell(sreg-1);
	}
	
#ifdef DEBUG_DELAYVAR
	xsb_dbgmsg((LOG_DEBUG,">>>> (before build_subgoal_args) num_heap_term_vars = %d",
		   num_heap_term_vars));
#endif

	/*
	 * Function build_subgoal_args() goes through the subgoal trie
	 * and binds all the arguments of the subgoal skeleton, which
	 * were built in the heap (like q(_,_)), to the actual call
	 * arguments (like X and f(Y,g(Z)) in q(X, f(Y,g(Z)))).
	 *
	 * After build_subgoal_args, var_addr[] contains all the
	 * variables in the subgoal call.
	 */
	build_subgoal_args(subg);
	
#ifdef DEBUG_DELAYVAR
	xsb_dbgmsg((LOG_DEBUG,">>>> (after build_subgoal_args) num_heap_term_vars = %d",
		   num_heap_term_vars));
#endif
	
	for (i = 0, j = num_heap_term_vars-1; j >= 0; j--) {
	  cell_array[i++] = (Cell)var_addr[j];
#ifdef DEBUG_DELAYVAR
	  xsb_dbgmsg((LOG_DEBUG,">>>> var_addr[%x] = %x", j, (int)var_addr[j]));
#endif
	}
	
	/*
	 * Function load_solution_trie() goes through the answer trie
	 * and binds the call substitution factor.  The substitution
	 * factor of the answer is left in var_addr[].
	 */
	load_solution_trie(CTXTc i, 0, &cell_array[i-1], ans_subst);
	
#ifdef DEBUG_DELAYVAR
	xsb_dbgmsg((LOG_DEBUG,">>>> (after load_solution_trie) num_heap_term_vars = %d",
		   num_heap_term_vars));
#endif
	
	for (i = 0, j = num_heap_term_vars-1; j >= 0; j--) {
	  cell_array[i++] = (Cell)var_addr[j];
#ifdef DEBUG_DELAYVAR
	  xsb_dbgmsg((LOG_DEBUG,">>>> var_addr[%x] = %x", j, (int)var_addr[j]));
#endif
	}
	
	tmp_var_addr = var_addr;
	
	/*
	 * Restore var_addr[] to copy_of_var_addr[], which contains all
	 * the variables in the _head_ predicate after get_returns is
	 * called (see bineg_xsb_i.h).
	 *
	 * The content of copy_of_var_addr[] is used in
	 * load_delay_trie(), and might be changed by each delay
	 * element.  But during the whole process for one paticular
	 * delay list, copy_of_var_addr[] will be shared by all the
	 * delay elements.
	 *
	 * It is not necessary to restore copy_of_num_heap_term_vars
	 * each time before load_delay_trie() is called for a delay
	 * element.  Each variable in the trie can find its binding in
	 * copy_of_var_addr[] without num_heap_term_vars.
	 */
	var_addr = copy_of_var_addr; /* variables left in the head */
	num_heap_term_vars = copy_of_num_heap_term_vars;
	
	
#ifdef DEBUG_DELAYVAR
	xsb_dbgmsg((LOG_DEBUG,">>>> NOW copy_of_num_heap_term_vars = %d",
		   copy_of_num_heap_term_vars));
	{
	  int i;
	  for(i = 0; i < num_heap_term_vars; i++)
	    xsb_dbgmsg((LOG_DEBUG,">>>> var_addr[%d] = %x",i, (int)var_addr[i]));
	  
	  fprintf(stddbg, "Stored Subs Fact: <");
	  {
	    BTNptr x = subs_factp;
	    if (x == NULL)
	      xsb_dbgmsg((LOG_DEBUG,">>>> subs_factp is NULL"));
	    while(x != NULL){
	      print_trie_atom(Atom(x));
	      if(Sibl(x) != NULL) 
		fprintf(stddbg, "!");
	      x = Child(x);
	      
	    }
	  }
	  fprintf(stddbg, ">\n");
	}
	xsb_dbgmsg((LOG_DEBUG,">>>> num_heap_term_vars is %d before calling load_delay_trie",
		   num_heap_term_vars));
#endif /* DEBUG_DELAYVAR */
	
#ifndef IGNORE_DELAYVAR
	load_delay_trie(CTXTc i, &cell_array[i-1], de_subs_fact_leaf(de));
#endif

#ifdef DEBUG_DELAYVAR
	xsb_dbgmsg((LOG_DEBUG,">>>> num_heap_term_vars becomes %d",
		   num_heap_term_vars));
	for (i = 0; i < num_heap_term_vars; i++)
	  xsb_dbgmsg((LOG_DEBUG,">>>> var_addr[%d] = %x",i, (int)var_addr[i]));
#endif
	var_addr = tmp_var_addr;
      }
    }
    hreg++;
  } else {
    bind_nil(delay_list);
  }
}

/*---------------------- end of file residual.c ------------------------*/
