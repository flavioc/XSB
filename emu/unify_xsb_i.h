/* File:      unify_xsb_i.h
** Author(s): David S. Warren, Terrance Swift, Jiyang Xu
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

tail_recursion:
     deref2(op1, goto label_op1_free);
     deref2(op2, goto label_op2_free);

     switch (cell_tag(op1)) {
     case FREE:
     case REF1: 
     label_op2_free: bind_copy0((CPtr)(op2), op1);
                     IFTHEN_SUCCEED;
		     break;
     label_op1_free:
        deref(op2);
	if (isref(op2)) {
	  /* op2 is FREE 			free  ... free */
	  if ( (CPtr)(op1) != (CPtr)(op2) ) {
	    if ( (CPtr)(op1) < (CPtr)(op2) ) {
#ifdef CHAT
	      if ( (CPtr)(op1) < hreg )  
#else
		if ( (CPtr)(op1) < hreg ||  (CPtr)(op1) < hfreg )  
#endif
		  /* op1 not in local stack */
		  { bind_ref((CPtr)(op2), (CPtr)(op1)); }
		else  /* op1 points to op2 */
		  { bind_ref((CPtr)(op1), (CPtr)(op2)); }
	      /* doc tls -- extra garbage because stacks point in diff dirs. */
	    }
	    else { /* op1 > op2 */
#ifdef CHAT
	      if  ((CPtr)(op2) < hreg )
#else
		if  ((CPtr)(op2) < hreg || (CPtr)(op2) < hfreg )
#endif
		  { bind_ref((CPtr)(op1), (CPtr)(op2)); }
		else
		  { bind_ref((CPtr)(op2), (CPtr)(op1)); }
	    }
	  }
	  IFTHEN_SUCCEED;
	}
	else { bind_copy0((CPtr)(op1), op2);
	       IFTHEN_SUCCEED;
	}
	break; /* for op1=free */

     case CS: /* op1=c/s */
       if (isconstr(op2)) {
	 if (op1 != op2) {  /* a != b */
	   op1 = (Cell)(clref_val(op1));
	   op2 = (Cell)(clref_val(op2));
	   if (((Pair)(CPtr)op1)->psc_ptr!=((Pair)(CPtr)op2)->psc_ptr){
						/* 0(a) != 0(b) */
	     IFTHEN_FAILED;
	   } else {
	     int arity = get_arity(((Pair)(CPtr)op1)->psc_ptr);
	     while (--arity) {
	       op1 = (Cell)((CPtr)op1+1); op2 = (Cell)((CPtr)op2+1);
	       if (!unify(cell((CPtr)op1), cell((CPtr)op2))) {
		 IFTHEN_FAILED;
	       }
	     }
	     op1 = (Cell)((CPtr)op1+1); op2 = (Cell)((CPtr)op2+1);
	     goto tail_recursion;
	   }
	 }
	 IFTHEN_SUCCEED;
       }
       else if (isattv(op2)) {
	 /* fprintf(stderr, ".... CS = ATTV, interrupt needed\n"); */
	 add_interrupt(op2, op1);
	 IFTHEN_SUCCEED;
       }
       else { /* op2 is STRING, FLOAT, LIST, or INT.	*/
	 IFTHEN_FAILED;
       }
       break;	/* for op1=c/s */

     case LIST:	/* op1=list */
       if (islist(op2)) {			/* list ... list */
	 if (op1 != op2) {
	   op1 = (Cell)(clref_val(op1));
	   op2 = (Cell)(clref_val(op2));
	   if ( !unify(cell((CPtr)op1), cell((CPtr)op2)))
	     { IFTHEN_FAILED; }
	   else
	     { op1 = (Cell)((CPtr)op1+1);
	       op2 = (Cell)((CPtr)op2+1);
	       goto tail_recursion;
	     }
	 }
	 IFTHEN_SUCCEED;
       }
       else if (isattv(op2)) {
	 /* fprintf(stderr, ".... LIST = ATTV, interrupt needed\n"); */
	 add_interrupt(op2, op1);
	 IFTHEN_SUCCEED;
       }
       else { IFTHEN_FAILED; }
       break; /* op1=list */

     case INT:    /* op1=num */
     case STRING: /* op1=string */
     case FLOAT:
       if (op1 == op2)
	 { IFTHEN_SUCCEED; }
       else if (isattv(op2)) {
	 /* fprintf(stderr, ".... INT = ATTV, interrupt needed\n"); */
	 add_interrupt(op2, op1);
	 IFTHEN_SUCCEED;
       }
       else { IFTHEN_FAILED; }
       break;     /* op1=atomic */

     case ATTV:
       deref(op2);
       if (isref(op2)) {
	 /* op2 is FREE				attv ... free */
	 bind_copy0((CPtr)op2, op1);
	 IFTHEN_SUCCEED;
       }
       else if (!isattv(op2) || (isattv(op2) && op1 != op2)) {
	 /* fprintf(stderr, ".... ATTV = ???, interrupt needed\n"); */
	 add_interrupt(op1, op2);
	 IFTHEN_SUCCEED;
       }
       else	/* isattv(op2) && op1==op2: no need to interrupt */
	 IFTHEN_SUCCEED;
       break;
       
       /* default:
	  xsb_abort("Unknown term type in unify()");
	  { IFTHEN_FAILED; }
	  break; */
    }

