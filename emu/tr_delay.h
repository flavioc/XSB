/* File:      tr_delay.h
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




/*----- Stuff for trie instructions ------------------------------------*/

#ifdef DEBUG_DELAY
#define handle_conditional_answers	\
    if (is_conditional_answer(NodePtr)) { \
      fprintf(stderr, "Trie-Code returning a conditional answer for ");	\
      SUBGOAL = (CPtr) asi_subgoal((ASI) Delay(NodePtr));	\
      print_subgoal(stderr, (SGFrame) SUBGOAL);			\
      fprintf(stderr, " (positively delaying)\n");		\
      delay_positively(SUBGOAL, NodePtr);			\
    }
#else
#define handle_conditional_answers	\
    if (is_conditional_answer(NodePtr)) {			\
      SUBGOAL = (CPtr) asi_subgoal((ASI) Delay(NodePtr));	\
      delay_positively(SUBGOAL, NodePtr);			\
    }
#endif

/*---------------------- end of file tr_delay.h ------------------------*/
