/* File:      tr_delay.h
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

/*----- Stuff for trie instructions ------------------------------------*/

/*
 * In the execution of trie code, handle_conditional_answers is called
 * (in proceed_lpcreg) when NodePtr is an answer leaf.  If the answer is
 * a conditional one, then call delay_positively() to put it into the
 * delay list of the parent predicate.
 *
 * After the execution of trie code, the substitution factor of the
 * _answer_ is stored in array var_regs[], and the number of variables is
 * saved in num_vars_in_var_regs (-1 means there is no variable, 0 means
 * there is one variable, ...)
 *
 * Instead of saving the substitution factor of the call, we can save
 * the substitution factor of the answer in the delay element.
 */

#ifndef IGNORE_DELAYVAR
#ifdef DEBUG_DELAY
#define handle_conditional_answers {					\
    CPtr temp_hreg;							\
    VariantSF subgoal;                                                    \
                                                                        \
    if (is_conditional_answer(NodePtr)) {				\
      fprintf(stddbg, "Trie-Code returning a conditional answer for ");	\
      subgoal = asi_subgoal(Delay(NodePtr));	        		\
      print_subgoal(stddbg, subgoal);			        	\
      fprintf(stddbg, " (positively delaying)\n");			\
      fprintf(stddbg, ">>>> In handle_conditional_answers macro: \n");  \
      fprintf(stddbg, ">>>>     num_vars_in_var_regs = %d\n",           \
                      num_vars_in_var_regs);			        \
      if (num_vars_in_var_regs == -1) {					\
	delay_positively(subgoal, NodePtr,				\
			 makestring(get_ret_string()));			\
      }									\
      else {								\
        /* create the answer subsf ret/n */				\
	temp_hreg = hreg;						\
	new_heap_functor(hreg, get_ret_psc(num_vars_in_var_regs + 1));	\
	{								\
	  int i;							\
	  for (i = 0; i < num_vars_in_var_regs + 1; i++) {		\
	    cell(hreg++) = (Cell) var_regs[i]; /* new */		\
	    fprintf(stddbg, ">>>>     var_regs[%d] = ", i);		\
	    printterm(cell(var_regs[i]), 1, 25);			\
	    fprintf(stddbg, "\n");					\
	  }								\
	}								\
	delay_positively(subgoal, NodePtr, makecs(temp_hreg));		\
      }									\
    }									\
  }
#else
#define handle_conditional_answers {					\
    CPtr temp_hreg;							\
    VariantSF subgoal;                                                  \
                                                                        \
    if (is_conditional_answer(NodePtr)) {				\
      subgoal = asi_subgoal(Delay(NodePtr));			        \
      if (num_vars_in_var_regs == -1) {					\
	delay_positively(subgoal, NodePtr,				\
			 makestring(get_ret_string()));			\
      }									\
      else {								\
	temp_hreg = hreg;						\
	new_heap_functor(hreg, get_ret_psc(num_vars_in_var_regs + 1));	\
	{								\
	  int i;							\
	  for (i = 0; i < num_vars_in_var_regs + 1; i++) {		\
	    cell(hreg++) = (Cell) var_regs[i]; /* new */		\
	  }								\
	}								\
	delay_positively(subgoal, NodePtr, makecs(temp_hreg));		\
      }									\
    }									\
  }
#endif /* DEBUG_DELAY */
#else  /* IGNORE_DELAYVAR */
#define handle_conditional_answers {			\
  if (is_conditional_answer(NodePtr)) {			\
    VariantSF subgoal = asi_subgoal(Delay(NodePtr));	\
    delay_positively(subgoal, NodePtr,			\
		     makestring(get_ret_string()));	\
  }							\
}
#endif /* IGNORE_DELAYVAR */

/*---------------------- end of file tr_delay.h ------------------------*/
