/* File:      xsberror.c
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


#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <signal.h>

#include "configs/config.h"

#include "auxlry.h"
#include "cell.h"
#include "psc.h"
#include "subp.h"
#include "register.h"
#include "xsberror.h"

extern void print_pterm(Cell, int, char *, int *);

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
void xsb_abort(char *description, ...)
{
  char message[MAXBUFSIZE];
  va_list args;

  va_start(args, description);

  strcpy(message, "++Error: ");
  vsprintf(message+strlen(message), description, args);
  if (message[strlen(message)-1] != '\n')
    strcat(message, "\n");

  va_end(args);
  pcreg = exception_handler(message);

  /* this allows xsb_abort to jump out even from nested loops */
  longjmp(xsb_abort_fallback_environment, (int) pcreg);
}

/*----------------------------------------------------------------------*/

void arithmetic_abort(Cell op1, char *OP, Cell op2)
{
  int  index;
  char str_op1[30], str_op2[30];

  index = 0; print_pterm(op1, 1, str_op1, &index);
  index = 0; print_pterm(op2, 1, str_op2, &index);
  if (isref(op1) || isref(op2)) {
    xsb_abort("Uninstantiated argument of evaluable function %s/2\n%s %s %s %s%s",
	      OP,
	      "   Goal:", str_op1, OP, str_op2,
	      ", probably as 2nd arg of is/2");
  }
  else {
    xsb_abort("Wrong domain in evaluable function %s/2\n%s %s %s %s found",
	      OP, "         Arithmetic expression expected, but",
	      str_op1, OP, str_op2);
  }
}

void arithmetic_abort1(char *OP, Cell op)
{
  int  index = 0;
  char str_op[30];
  
  print_pterm(op, 1, str_op, &index);
  xsb_abort("%s evaluable function %s/2\n%s %s(%s) %s",
	    (isref(op) ? "Uninstantiated argument of" : "Wrong domain in"),
	    OP, "   Goal:", OP, str_op, ", probably as 2nd arg of is/2");  
}

void arithmetic_comp_abort(Cell op1, char *OP, int op2)
{
  int  index = 0;
  char str_op1[30];

  print_pterm(op1, 1, str_op1, &index);
  xsb_abort("%s arithmetic comparison %s/2\n%s %s %s %d",
	    (isref(op1) ? "Uninstantiated argument of" : "Wrong type in"),
	    OP, "   Goal:", str_op1, OP, op2);
}

/*----------------------------------------------------------------------*/

void xsb_warn(char *description, ...)
{
  va_list args;

  va_start(args, description);
  fprintf(stderr, "\n++Warning: ");
  vfprintf(stderr, description, args);
  va_end(args);
  fprintf(stderr, "\n");
}

void xsb_mesg(char *description, ...)
{
  va_list args;

  va_start(args, description);
  vfprintf(stderr, description, args);
  va_end(args);
  fprintf(stderr, "\n");
}

/*----------------------------------------------------------------------*/

void xsb_exit(char *description, ...)
{
  va_list args;

  va_start(args, description);
  vfprintf(stderr, description, args);
  va_end(args);

  fprintf(stderr, "\nExiting XSB abnormally...\n");
  exit(1);
}

/*----------------------------------------------------------------------*/

void err_handle(int description, int arg, char *f,
		int ar, char *expected, Cell found)
{
  char message[160];	/* Allow 2 lines of error reporting.	*/
  
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
	    "! %s error (not completely handled yet)\n",
	    err_msg[description]);
    break;
  }
  pcreg = exception_handler(message);
}

/*----------------------------------------------------------------------*/
