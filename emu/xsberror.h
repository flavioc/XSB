/* File:      xsberror.h
** Author(s): Kostis F. Sagonas
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

#include <setjmp.h>

/*----------------------------------------------------------------------*/
/* The following is a list of errors as defined by the Prolog ISO	*/
/* standard.  It is clear that today (October 1993), XSB does NOT	*/
/* catch all of them.  In fact it does not even report half of them.	*/
/* However, one fine day it might!  The following list tries to become	*/
/* a first step towards the uniform treatment of errors by XSB.		*/
/*----------------------------------------------------------------------*/

#define CALCULATION	 0
#define DATABASE	 1
#define EVALUATION	 2
#define IMPLEMENTATION	 3
#define INSTANTIATION	 4
#define IO_CONTROL	 5
#define IO_END_OF_FILE	 6
#define IO_FORMATTING	 7
#define OPERATOR	 8
#define OVERFLOW	 9
#define RANGE		10
#define SYNTAX		11
#define TYPE		12
#define UNDEFINED_PRED	13
#define UNDEFINED_VAL	14
#define UNDERFLOW	15 
#define ZERO_DIVIDE	16 

extern void xsb_exit(char *);
extern void xsb_abort(char *, ...);
extern void xsb_warn(char *, ...);
extern void xsb_mesg(char *, ...);
extern void err_handle(int, int, char *, int, char *, Cell);

#define err(d, a, f, ar)	err_handle(d, a, f, ar, NULL, (Cell)NULL)

extern char *xsb_default_segfault_msg;
extern char *xsb_segfault_message; /* put your segfault message here prior to
				      executing the command that might segfault
				   */ 

extern void (*xsb_default_segfault_handler)(int); /* where the previous value
						     of the SIGSEGV handler is
						     saved */ 
extern jmp_buf xsb_segfault_fallback_environment; /* Environment for segfault
						     longjump saved here */
extern jmp_buf xsb_abort_fallback_environment; /* Environment for abort
						  longjump saved here */

/* SIGSEGV handler that catches segfaults; used unless configured with DEBUG */
extern void xsb_segfault_catcher (int);


