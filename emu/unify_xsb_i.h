/* File:      unify_xsb_i.h
** Author(s): Bart Demoen (maintained & checked by Kostis Sagonas)
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) K.U. Leuven 1999-2000
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

#define IFTHEN_SUCCEED  XSB_Next_Instr()
#define IFTHEN_FAILED	{Fail1 ; XSB_Next_Instr() ;}


#ifdef CHAT
#define COND1      (CPtr)(op1) < hreg 
#else
#define COND1      (CPtr)(op1) < hreg ||  (CPtr)(op1) < hfreg 
#endif

#ifdef CHAT
#define COND2      (CPtr)(op2) < hreg 
#else
#define COND2      (CPtr)(op2) < hreg ||  (CPtr)(op2) < hfreg 
#endif

#define unify_xsb(loc) \
 loc##_tail_recursion: \
  XSB_Deref2(op1, goto loc##_label_op1_free);\
  XSB_Deref2(op2, goto loc##_label_op2_free);\
\
  if (isattv(op1)) goto loc##_label_op1_attv;\
  if (isattv(op2)) goto loc##_label_op2_attv;\
\
  if (cell_tag(op1) != cell_tag(op2))\
    IFTHEN_FAILED;\
\
  if (isconstr(op1)) goto loc##_label_both_struct;\
  if (islist(op1)) goto loc##_label_both_list;\
  /* now they are both atomic */\
  if (op1 == op2) IFTHEN_SUCCEED;\
  IFTHEN_FAILED;\
\
\
 loc##_label_op1_free:\
  XSB_Deref2(op2, goto loc##_label_both_free);\
  bind_copy((CPtr)(op1), op2);\
  IFTHEN_SUCCEED;\
\
\
 loc##_label_op2_free:\
  bind_copy((CPtr)(op2), op1);\
  IFTHEN_SUCCEED;\
\
\
 loc##_label_both_free:\
  if ( (CPtr)(op1) == (CPtr)(op2) ) IFTHEN_SUCCEED;\
  if ( (CPtr)(op1) < (CPtr)(op2) )\
    {\
      if (COND1)\
	/* op1 not in local stack */\
	{ bind_ref((CPtr)(op2), (CPtr)(op1)); }\
      else  /* op1 points to op2 */\
	{ bind_ref((CPtr)(op1), (CPtr)(op2)); }\
      }\
  else\
    { /* op1 > op2 */\
      if (COND2)\
	{ bind_ref((CPtr)(op1), (CPtr)(op2)); }\
      else\
	{ bind_ref((CPtr)(op2), (CPtr)(op1)); }\
    }\
  IFTHEN_SUCCEED;\
\
\
 loc##_label_both_list:\
  if (op1 == op2) IFTHEN_SUCCEED;\
\
  op1 = (Cell)(clref_val(op1));\
  op2 = (Cell)(clref_val(op2));\
  if ( !unify(cell((CPtr)op1), cell((CPtr)op2)))\
    { IFTHEN_FAILED; }\
  op1 = (Cell)((CPtr)op1+1);\
  op2 = (Cell)((CPtr)op2+1);\
  goto loc##_tail_recursion;\
\
\
 loc##_label_both_struct:\
  if (op1 == op2) IFTHEN_SUCCEED;\
\
  /* a != b */\
  op1 = (Cell)(clref_val(op1));\
  op2 = (Cell)(clref_val(op2));\
  if (((Pair)(CPtr)op1)->psc_ptr!=((Pair)(CPtr)op2)->psc_ptr)\
    {\
      /* 0(a) != 0(b) */\
      IFTHEN_FAILED;\
    }\
  {\
    int arity = get_arity(((Pair)(CPtr)op1)->psc_ptr);\
    while (--arity)\
      {\
	op1 = (Cell)((CPtr)op1+1); op2 = (Cell)((CPtr)op2+1);\
	if (!unify(cell((CPtr)op1), cell((CPtr)op2)))\
	  {\
	    IFTHEN_FAILED;\
	  }\
      }\
    op1 = (Cell)((CPtr)op1+1); op2 = (Cell)((CPtr)op2+1);\
    goto loc##_tail_recursion;\
  }\
\
\
  /* if the order of the arguments in add_interrupt is not important,*/\
  /* the following three can actually be collapsed into one; loosing*/\
  /* some meaningful attv_dbgmsg - they have been lost partially */\
  /* already */\
\
 loc##_label_op1_attv:\
  if (isattv(op2)) goto loc##_label_both_attv;\
  attv_dbgmsg(">>>> ATTV = something, interrupt needed\n");\
  add_interrupt(op1, op2);\
  IFTHEN_SUCCEED;\
\
 loc##_label_op2_attv:\
  attv_dbgmsg(">>>> something = ATTV, interrupt needed\n");\
  add_interrupt(op2, op1);\
  IFTHEN_SUCCEED;\
\
 loc##_label_both_attv:\
  if (op1 != op2)\
    {\
      attv_dbgmsg(">>>> ATTV = ???, interrupt needed\n");\
      add_interrupt(op1, op2);\
    }\
  IFTHEN_SUCCEED
