/* File:      unify.i
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



     free_deref(op1);
     switch (cell_tag(op1)) {
     case FREE:
     case REF1: 
	deref(op2);
	if (isref(op2)) {
	    /* op2 is FREE 			free  ... free */
		if ( (CPtr)(op1) != (CPtr)(op2) ) {
		    if ( (CPtr)(op1) < (CPtr)(op2) ) {
			if ( (CPtr)(op1) < hreg ||  (CPtr)(op1) < hfreg )  
						/* op1 not in loc stack */
			    {bind_ref((CPtr)(op2), (CPtr)(op1));}
			else  /* op1 points to op2 */
			    {bind_ref((CPtr)(op1), (CPtr)(op2));}
             /* doc tls -- extra garbage because stacks point in diff dirs. */
		    }
		    else { /* op1 > op2 */
			if  ((CPtr)(op2) < hreg || (CPtr)(op2) < hfreg )
			    {bind_ref((CPtr)(op1), (CPtr)(op2));}
			else
			    {bind_ref((CPtr)(op2), (CPtr)(op1));}
		    }
		}
		IFTHEN_SUCCEED;
	}
	else {	bind_copy0((CPtr)(op1), op2);
		IFTHEN_SUCCEED;
	}
	break; /* for op1=free */

    case CS: /* op1=c/s */
	deref(op2);
	if (isref(op2)) {
	    /* op2 is FREE		c/s ... free */
		bind_copy0((CPtr)(op2), op1);
	        IFTHEN_SUCCEED;
	}
	else if (isconstr(op2)) {
        	if (op1 != op2) {  /* a != b */
		    op1 = (Cell)(clref_val(op1));
		    op2 = (Cell)(clref_val(op2));
		    if (((Pair)(CPtr)op1)->psc_ptr!=((Pair)(CPtr)op2)->psc_ptr){
						/* 0(a) != 0(b) */
			IFTHEN_FAILED;
		    } else {
	               arity = get_arity(((Pair)(CPtr)op1)->psc_ptr);
        	       for ( i=1; i <= arity;  i++ ) {
			 if(!unify(*((CPtr)op1+i), *((CPtr)op2+i))) {
			     IFTHEN_FAILED; 
			 }
		       }
		    }
		}
		IFTHEN_SUCCEED;
	      }
	      else { /* op2 is STRING, FLOAT, LIST, or INT.	*/
		IFTHEN_FAILED;
	      }
	break;	/* for op1=c/s */

    case LIST:	/* op1=list */
        deref(op2);
        if (isref(op2)) {
	    /* op2 is FREE			   list ... free */
		bind_copy0((CPtr)(op2), op1);
		IFTHEN_SUCCEED;
	}
	else if (islist(op2)) {			/* list ... list */
		if (op1 != op2) {
		    op1 = (Cell)(clref_val(op1));
		    op2 = (Cell)(clref_val(op2));
		    if ( !unify(*((CPtr)op1), *((CPtr)op2))
		             || !unify(*((CPtr)op1+1), *((CPtr)op2+1)) ) {
			  IFTHEN_FAILED;
		    }
		}
		IFTHEN_SUCCEED;
	     }
	     else { IFTHEN_FAILED; }
	break; /* op1=list */

    case INT:	/* op1=num */
        deref(op2);
	if (isref(op2)) {
	    /* op2 is FREE:			   num ... free */
		bind_copy0((CPtr)(op2), op1);
		IFTHEN_SUCCEED;
	}
	else if (isinteger(op2)) {
	    	/* num ... num */
		if (numequal(op2, op1)) {IFTHEN_SUCCEED;} else {IFTHEN_FAILED;}
	     }
	     else	/* op2 is FLOAT, STRING, CS, or	LIST.	*/
		  { IFTHEN_FAILED; }
	break; /* op1=int */

    case STRING:	/* op1=string */
        deref(op2);
        if (isref(op2)) {
	    /* op2 is FREE			   string ... free */
		bind_copy0((CPtr)(op2), op1);
		IFTHEN_SUCCEED;
	}
	else if (isstring(op2)) {
		if (string_val(op2)==string_val(op1)) {IFTHEN_SUCCEED;}
		else {IFTHEN_FAILED;}
	     }
	     else	/* op2 is INT, FLOAT, CS, or LIST.	*/
		  { IFTHEN_FAILED; }
	break; /* op1=string*/

    case FLOAT:
        deref(op2);
        if (isref(op2)) {
	    /* op2 is FREE			   float ... free */
		bind_copy0((CPtr)(op2), op1);
		IFTHEN_SUCCEED;
	}
	else if (isfloat(op2)) {
#ifdef FLOAT_UNIFICATION_OUT
		unify_float_unification_exception;
		IFTHEN_FAILED;
#else
		if (float_val(op1)==float_val(op2)) {IFTHEN_SUCCEED;}
		else {IFTHEN_FAILED;}	
#endif
	     }
	     else	/* op2 is STRING, INT, CS, or LIST.	*/
		  { IFTHEN_FAILED; }
	break; /* op1=float */

    default:
	xsb_abort("Unknown term type in unify()");
	{ IFTHEN_FAILED; }
	break;

    }
