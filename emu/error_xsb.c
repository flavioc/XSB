/* File:      error_xsb.c
** Author(s): Sagonas, Demoen
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
#include <stdarg.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "psc_xsb.h"
#include "subp.h"
#include "register.h"
#include "error_xsb.h"
#include "io_builtins_xsb.h"
#include "cinterf.h"
#include "tries.h"
#include "choice.h"
#include "inst_xsb.h"
#include "macro_xsb.h"
#include "tr_utils.h"
#include "cut_xsb.h"
#ifdef CHAT
#include "chat.h"
#endif

extern void exit(int status);

FILE *stdmsg;	     	     	  /* stream for XSB benign messages */
FILE *stddbg;	     	     	  /* stream for XSB debug msgs */
FILE *stdwarn;	     	     	  /* stream for XSB warnings */
FILE *stdfdbk;	     	     	  /* stream for XSB feedback messages */

/*----------------------------------------------------------------------*/

static char *err_msg[] = {
	"Calculation", "Database", "Evaluation", "Implementation",
	"Instantiation", "I/O Control", "I/O End-of-file", "I/O Formatting",
	"Operator", "Overflow", "Range", "Syntax", "Type",
	"Undefined predicate/function", "Undefined value",
	"Underflow", "Zero division" };

/*----------------------------------------------------------------------*/

/* you can pass either 1 argument---a full description (a string),
   or a variable number of arguments -- a format followed by arguments.
*/
#if defined(DEBUG) && defined(CP_DEBUG)
extern void print_cp_backtrace();
#endif

DllExport void call_conv xsb_abort(char *description, ...)
{
  char message[MAXBUFSIZE];
  va_list args;

  va_start(args, description);

  strcpy(message, "++Error[XSB]: [Runtime/C] ");
  vsprintf(message+strlen(message), description, args);
  if (message[strlen(message)-1] != '\n')
    strcat(message, "\n");

  va_end(args);
  pcreg = exception_handler(message);

#if defined(DEBUG) && defined(CP_DEBUG)
  print_cp_backtrace();
#endif
  /* this allows xsb_abort to jump out even from nested loops */
  longjmp(xsb_abort_fallback_environment, (Integer) pcreg);
}


DllExport void call_conv xsb_bug(char *description, ...)
{
  char message[MAXBUFSIZE];
  va_list args;

  va_start(args, description);

  strcpy(message, "++XSB bug: ");
  vsprintf(message+strlen(message), description, args);
  if (message[strlen(message)-1] != '\n')
    strcat(message, "\n");

  va_end(args);
  pcreg = exception_handler(message);

  /* this allows xsb_abort to jump out even from nested loops */
  longjmp(xsb_abort_fallback_environment, (Integer) pcreg);
}

/*----------------------------------------------------------------------*/

void arithmetic_abort(Cell op1, char *OP, Cell op2)
{
  static XSB_StrDefine(str_op1);
  static XSB_StrDefine(str_op2);

  print_pterm(op1, TRUE, &str_op1);
  print_pterm(op2, TRUE, &str_op2);
  if (isref(op1) || isref(op2)) {
    xsb_abort("Uninstantiated argument of evaluable function %s/2\n%s %s %s %s%s",
	      OP, "   Goal:",
	      (isref(op1)? "_Var": str_op1.string),
	      OP,
	      (isref(op2)? "_Var": str_op2.string),
	      ", probably as 2nd arg of is/2");
  }
  else {
    xsb_abort("Wrong domain in evaluable function %s/2\n%s %s %s %s found",
	      OP, "         Arithmetic expression expected, but",
	      str_op1.string, OP, str_op2.string);
  }
}

void arithmetic_abort1(char *OP, Cell op)
{
  static XSB_StrDefine(str_op);
  
  XSB_StrSet(&str_op,"_Var");
  if (! isref(op)) print_pterm(op, TRUE, &str_op);
  xsb_abort("%s evaluable function %s/2\n%s %s(%s) %s",
	    (isref(op) ? "Uninstantiated argument of" : "Wrong domain in"),
	    OP, "   Goal:", OP, str_op.string,
	    ", probably as 2nd arg of is/2");  
}

void arithmetic_comp_abort(Cell op1, char *OP, int op2)
{
  static XSB_StrDefine(str_op1);

  XSB_StrSet(&str_op1,"_Var");
  if (! isref(op1)) print_pterm(op1, TRUE, &str_op1);
  xsb_abort("%s arithmetic comparison %s/2\n%s %s %s %d",
	    (isref(op1) ? "Uninstantiated argument of" : "Wrong type in"),
	    OP, "   Goal:", str_op1.string, OP, op2);
}

/*----------------------------------------------------------------------*/

/* this is a soft type of error msg compared to xsb_abort. It doesn't abort the
   computation, but sends stuff to stderr */
DllExport void call_conv xsb_error (char *description, ...)
{
  va_list args;

  va_start(args, description);
  fprintf(stderr, "\n++Error[XSB]: [Runtime/C] ");
  vfprintf(stderr, description, args);
  va_end(args);
  fprintf(stderr, "\n");
#if defined(DEBUG) && defined(CP_DEBUG)
  print_cp_backtrace();
#endif
}

DllExport void call_conv xsb_warn(char *description, ...)
{
  va_list args;

  va_start(args, description);
  fprintf(stdwarn, "\n++Warning[XSB]: [Runtime/C] ");
  vfprintf(stdwarn, description, args);
  va_end(args);
  fprintf(stdwarn, "\n");
#if defined(DEBUG) && defined(CP_DEBUG)
  print_cp_backtrace();
#endif
}

DllExport void call_conv xsb_mesg(char *description, ...)
{
  va_list args;

  va_start(args, description);
  vfprintf(stdmsg, description, args);
  va_end(args);
  fprintf(stdmsg, "\n");
}

DllExport void call_conv xsb_dbgmsg(char *description, ...)
{
  va_list args;

  va_start(args, description);
  vfprintf(stddbg, description, args);
  va_end(args);
  fprintf(stddbg, "\n");
}

/*----------------------------------------------------------------------*/

DllExport void call_conv xsb_exit(char *description, ...)
{
  va_list args;

  va_start(args, description);
  vfprintf(stderr, description, args);
  va_end(args);

  fprintf(stdfdbk, "\nExiting XSB abnormally...\n");
  exit(1);
}

/*----------------------------------------------------------------------*/

void err_handle(int description, int arg, char *f,
		int ar, char *expected, Cell found)
{
  char message[240];	/* Allow 3 lines of error reporting.	*/
  
  switch (description) {
  case INSTANTIATION:
    sprintf(message, 
	    "! %s error in argument %d of %s/%d\n",
	    err_msg[description], arg, f, ar);
    break;
  case RANGE:	/* I assume expected != NULL */
    sprintf
      (message,
       "! %s error: in argument %d of %s/%d\n! %s expected, but %d found\n",
       err_msg[description], arg, f, 
       ar, expected, (int) int_val(found));
    break;
  case TYPE:
    if (expected == NULL) {
      sprintf(message, 
	      "! %s error in argument %d of %s/%d\n",
	      err_msg[description], arg, f, ar);
    } else  
      sprintf
	(message,
	 "! %s error: in argument %d of %s/%d\n! %s expected, but %s found\n",
	 err_msg[description], arg, f, ar, expected,
	 "something else");
    break;
  case ZERO_DIVIDE:
    sprintf(message,
	    "! %s error in %s\n! %s expected, but %lx found\n",
	    err_msg[description], f, expected, found);
    break;
  default:
    sprintf(message, 
	    "! %s error (not completely handled yet): %s\n",
	    err_msg[description], expected);
    break;
  }
  pcreg = exception_handler(message);
#if defined(DEBUG) && defined(CP_DEBUG)
  print_cp_backtrace();
#endif
}

/*************************************************************************/
/*
   Builtins for exception handling using a Prolog-based catch-throw

              $$set_scope_marker/0
              $$unwind_stack/0
              $$clean_up_block/0

   Written by Bart Demoen, after the CW report 98:
              A 20' implementation of catch and throw

   7 Febr 1999

*/

static byte *scope_marker;

int set_scope_marker()
{
  /*     printf("%x %x\n",cp_ereg(breg),ereg);*/
   scope_marker = pcreg;
   /* skipping a putpval and a call instruction */
   /* is there a portable way to do this ?      */
   /* instruction builtin has already made pcreg point to the putpval */
   scope_marker += THROWPAD;
   return(TRUE);
} /* set_scope_marker */

/* TLS: slightly modified to abort if throwing over an open table.
   Im not positive if this is right */
int unwind_stack()
{
   byte *cp, *cpmark;
   CPtr e,b;
   byte inst_cut_over;

   cpmark = scope_marker;
   /*   printf("sm 2 %d  x%x\n",scope_marker,scope_marker);*/
   /* first find the right environment */
   e = ereg;
   cp = cpreg; /* apparently not pcreg ... maybe not good in general */
   while ( (cp != cpmark) && e )
     {
       /*       printf("cp %d x%x\n",cp,cp);*/
       cp = (byte *)e[-1];
       e = (CPtr)e[0];
     }

   if ( ! e )
     xsb_exit("Throw failed because no catcher for throw");

   /* now find the corresponding breg */
   b = breg;
   while (cp_ereg(b) <= e) {
     inst_cut_over = *cp_pcreg(b); 
     CHECK_TABLE_CUT(inst_cut_over) ;      
     b = cp_prevbreg(b);
   }
   inst_cut_over = *cp_pcreg(b); 
   CHECK_TABLE_CUT(inst_cut_over) ;      
   breg = b;
   return(FALSE);

} /* unwind_stack */


int clean_up_block()
{
   byte inst_cut_over;
   if (cp_ereg(breg) > ereg) {
     /*     printf("%x %x\n",cp_ereg(breg),ereg); */
     inst_cut_over = *cp_pcreg(breg); 
     CHECK_TABLE_CUT(inst_cut_over) ;      
     breg = (CPtr)cp_prevbreg(breg);
   }
   return(TRUE);

} /* clean_up_block */

/*---------------------------- end of error_xsb.c --------------------------*/

