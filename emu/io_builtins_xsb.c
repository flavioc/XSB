/* File:      io_builtins_xsb.c
** Author(s): David S. Warren, kifer
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1993-1998
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

#include "configs/config.h"
#include "debugs/debug.h"

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#ifndef WIN_NT
#include <unistd.h> 
#endif
#include <sys/stat.h>

#include "setjmp_xsb.h"
#include "auxlry.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "cinterf.h"
#include "memory_xsb.h"
#include "psc_xsb.h"
#include "heap_xsb.h"
#include "register.h"
#include "flags_xsb.h"
#include "inst_xsb.h"
#include "token_xsb.h"
#include "loader_xsb.h" /* for ZOOM_FACTOR */
#include "subp.h"
#include "tries.h"
#include "choice.h"
#include "macro_xsb.h"
#include "io_builtins_xsb.h"
#include "configs/special.h"
#include "binding.h"
#include "deref.h"
#include "findall.h"

FILE *open_files[MAX_OPEN_FILES]; /* open file table */

extern void print_pterm(prolog_term term,int toplevel,char *straddr,int *ind);

static FILE *fptr;			/* working variable */
    
#define setvar(loc,op1) \
    if (vars[opstk[op1].op].varval) \
       cell(loc) = vars[opstk[op1].op].varval; \
    else { \
	     cell(loc) = (Cell) loc; \
	     vars[opstk[op1].op].varval = (Cell) loc; \
	 }

struct fmt_spec {
  char type; 	     	     	 /* i(nteger), f(loat), s(tring) */
  /* in case of a print op and the specifiers *, this number is 1, 2, or 3,
     depending the number of *'s. This tells how manu arguments to expect. In
     case of a read operation, size can be 0, since here '*' means assignment
     suppression. */
  char size;
  char *fmt;
};

struct fmt_spec *next_format_substr(char *, int, int);
char *p_charlist_to_c_string(prolog_term, char *, int, char *, char *);

/* type is a char: 's', 'i', 'f' */
#define TYPE_ERROR_CHK(ch_type, Label) \
        if (current_fmt_spec->type != ch_type) { \
	    xsb_abort("%s: Type mismatch in argument value %d", Label, i); \
        }

#define PRINT_ARG(arg) switch (current_fmt_spec->size) { \
        case 1: fprintf(fptr, current_fmt_spec->fmt, arg); \
	        break; \
	case 2: fprintf(fptr, current_fmt_spec->fmt, width, arg); \
	        break; \
	case 3: fprintf(fptr, current_fmt_spec->fmt, width, precision, arg); \
	        break; \
	}
#define CHECK_ARITY(i, Arity, Label) if (i > Arity) { \
	      xsb_abort("%s: Not enough arguments for given format", Label); \
	}

#ifdef HAVE_SNPRINTF
/* like PRINT_ARG, but uses snprintf */
#define SPRINT_ARG(arg) switch (current_fmt_spec->size) { \
        case 1: bytes_formatted=snprintf(ptr_OutString, safe_outstring_bytes, \
					 current_fmt_spec->fmt, arg); \
	        break; \
	case 2: bytes_formatted=snprintf(ptr_OutString, safe_outstring_bytes, \
					 current_fmt_spec->fmt, width, arg); \
	        break; \
	case 3: bytes_formatted=snprintf(ptr_OutString, safe_outstring_bytes, \
					 current_fmt_spec->fmt, \
					 width, precision, arg); \
	        break; \
	}

#else
/* like PRINT_ARG, but uses sprintf -- used with compilers that don't have
   snprintf. 
   In some systems sprintf returns it's first argument, so have to use
   strlen for portability.
   */
#define SPRINT_ARG(arg) switch (current_fmt_spec->size) { \
        case 1: sprintf(ptr_OutString, current_fmt_spec->fmt, arg); \
		bytes_formatted = strlen(ptr_OutString); \
	        break; \
	case 2: sprintf(ptr_OutString, current_fmt_spec->fmt, width, arg); \
		bytes_formatted = strlen(ptr_OutString); \
	        break; \
	case 3: sprintf(ptr_OutString, current_fmt_spec->fmt, \
					width, precision, arg); \
		bytes_formatted = strlen(ptr_OutString); \
	        break; \
	}
#endif

#define CHECK_OUTPUT_SIZE \
    	if ((bytes_formatted < 0) || (bytes_formatted > safe_outstring_bytes)) { \
	    xsb_abort("FMT_WRITE_STRING: Output exceeds max size; trailing bytes might be lost"); \
	} else { /* advance string pointer, adjust safe_outstring_bytes */  \
      	    safe_outstring_bytes -= bytes_formatted; \
	    ptr_OutString += bytes_formatted; \
    	    *ptr_OutString = '\0'; \
        }

bool fmt_write(void);
bool fmt_write_string(void);
bool fmt_read(void);
    	    

bool formatted_io (void)
{
  switch (ptoc_int(1)) {
  case FMT_WRITE: return fmt_write();
  case FMT_WRITE_STRING: return fmt_write_string();
  case FMT_READ: return fmt_read();
  default:
    xsb_abort("FORMATTED_IO: Invalid operation number: %d", ptoc_int(1));
  }
  return TRUE; /* just to get rid of compiler warning */
}

/*----------------------------------------------------------------------
    like fprintf
     C invocation: formatted_io(FMT_WRITE, IOport, Format, ValTerm)
     Prolog invocation: fmt_write(+IOport, +Format, +ValTerm)
       IOport: XSB I/O port
       Format: format as atom or string;
       ValTerm: term whose args are vars to receive values returned.
----------------------------------------------------------------------*/


bool fmt_write(void)
{
  char *Fmt=NULL, *str_arg;
  static char Fmt_buf[MAXBUFSIZE+1];
  static char str_arg_buf[MAXBUFSIZE+1];      /* holder for string arguments */
  static char aux_msg[50];
  prolog_term ValTerm, Arg, Fmt_term;
  int i, Arity=0;
  long int_arg;     	     	     	      /* holder for int args         */
  float float_arg;    	     	     	      /* holder for float args       */
  struct fmt_spec *current_fmt_spec;
  int offset;	       	       	       	      /* used in print_term 	     */
  int width=0, precision=0;    	     	      /* these are used in conjunction
						 with the *.* format         */
  SET_FILEPTR(fptr, ptoc_int(2));
  Fmt_term = reg_term(3);
  if (is_list(Fmt_term))
    Fmt = p_charlist_to_c_string(Fmt_term, Fmt_buf, sizeof(Fmt_buf),
				 "FMT_WRITE", "format string");
  else if (is_string(Fmt_term))
    Fmt = string_val(Fmt_term);
  else
    xsb_abort("FMT_WRITE: Format must be an atom or a character string");

  ValTerm = reg_term(4);
  if (is_functor(ValTerm))
    Arity = get_arity(get_str_psc(ValTerm));
  else if (is_var(ValTerm))
    /* Var in the argument position means, no arguments */
    Arity = 0;
  else {
    /* assume single argument; convert ValTerm into arg(val) */
    prolog_term TmpValTerm=p2p_new();

    c2p_functor("arg", 1, TmpValTerm);
    if (is_string(ValTerm))
      c2p_string(string_val(ValTerm), p2p_arg(TmpValTerm,1));
    else if (is_int(ValTerm))
      c2p_int(int_val(ValTerm), p2p_arg(TmpValTerm,1));
    else if (is_float(ValTerm))
      c2p_float(float_val(ValTerm), p2p_arg(TmpValTerm,1));
    else
      xsb_abort("Usage: fmt_write([+IOport,] +FmtStr, +args(A1,A2,...))");

    ValTerm = TmpValTerm;
    Arity = 1;
  }

  current_fmt_spec = next_format_substr(Fmt,
					1,   /* initialize    	      	     */
					0);  /* write    	      	     */
  xsb_segfault_message =
    "++FMT_WRITE: Argument type doesn't match format specifier\n";
  signal(SIGSEGV, &xsb_segfault_catcher);
  
  i=0;
  while (i <= Arity) {
    /* last format substring (and has no conversion spec) */
    if (current_fmt_spec->type == '.') {
      PRINT_ARG("");
      if (i < Arity)
	xsb_warn("FMT_WRITE: More arguments than format specifiers");
      goto EXIT_WRITE;
    }

    i++; /* increment after checking the last format segment */

    if (current_fmt_spec->size >  1) {
      Arg = p2p_arg(ValTerm,i++);
      width = (int) int_val(Arg);
    } 
    CHECK_ARITY(i, Arity, "FMT_WRITE");

    if (current_fmt_spec->size == 3) {
      Arg = p2p_arg(ValTerm,i++);
      precision = (int) int_val(Arg);
    }
    CHECK_ARITY(i, Arity, "FMT_WRITE");

    Arg = p2p_arg(ValTerm,i);

    if (current_fmt_spec->type == 'S') {
      /* Any type: print as a string */
      offset = 0;
      print_pterm(Arg, 1, str_arg_buf, &offset);
      PRINT_ARG(str_arg_buf);
    } else if (is_string(Arg)) {
      TYPE_ERROR_CHK('s', "FMT_WRITE");
      str_arg = string_val(Arg);
      PRINT_ARG(str_arg);
    } else if (is_list(Arg)) {
      TYPE_ERROR_CHK('s', "FMT_WRITE");
      sprintf(aux_msg, "argument %d", i);
      str_arg = p_charlist_to_c_string(Arg, str_arg_buf, sizeof(str_arg_buf),
				       "FMT_WRITE", aux_msg);
      PRINT_ARG(str_arg);
    } else if (is_int(Arg)) {
      TYPE_ERROR_CHK('i', "FMT_WRITE");
      int_arg = int_val(Arg);
      PRINT_ARG(int_arg);
    } else if (is_float(Arg)) {
      TYPE_ERROR_CHK('f', "FMT_WRITE")
      float_arg = float_val(Arg);
      PRINT_ARG(float_arg);
    } else {
      xsb_abort("FMT_WRITE: Argument %d has illegal type\n", i);
    }
    current_fmt_spec = next_format_substr(Fmt,
					  0 /* no initialize */,
					  0 /* write */ );
  }

  /* print the remainder of the format string, if it exists */
  if (current_fmt_spec->type == '.')
      PRINT_ARG("");

 EXIT_WRITE:
  xsb_segfault_message = xsb_default_segfault_msg;
  signal(SIGSEGV, xsb_default_segfault_handler);
  
  return TRUE;
}



/*----------------------------------------------------------------------
   like sprintf:
    C invocation: formatted_io(FMT_WRITE_STRING, String, Format, ValTerm)
    Prolog invocation: fmt_write_string(-String, +Format, +ValTerm)
      String: string buffer
      Format: format as atom or string;
      ValTerm: Term whose args are vars to receive values returned.
----------------------------------------------------------------------*/

#define MAX_SPRINTF_STRING_SIZE MAX_IO_BUFSIZE

/* If no snprintf, we fill only half of OutString, to be on the safe side */
#ifdef HAVE_SNPRINTF
#define SAFE_OUT_SIZE MAX_SPRINTF_STRING_SIZE
int sprintf(char *s, const char *format, /* args */ ...);
#else
#define SAFE_OUT_SIZE MAX_SPRINTF_STRING_SIZE/2
#endif

bool fmt_write_string(void)
{
  char *Fmt=NULL, *str_arg;
  static char Fmt_buf[MAXBUFSIZE+1];
  static char str_arg_buf[MAXBUFSIZE+1];    /* holder for string arguments  */
  static char OutString[MAX_SPRINTF_STRING_SIZE+1];
  static char aux_msg[50];
  char *ptr_OutString = OutString;
  prolog_term ValTerm, Arg, Fmt_term;
  int i, Arity;
  long int_arg;     	     	     	    /* holder for int args     	    */
  float float_arg;     	     	     	    /* holder for float args   	    */
  struct fmt_spec *current_fmt_spec;
  int offset;	       	       	       	    /* used in print_term 	    */
  int width=0, precision=0;      	    /* these are used in conjunction
					       with the *.* format     	    */
  int bytes_formatted=0;       	       	    /* the number of bytes formatted as
					       returned by sprintf/snprintf */
  int safe_outstring_bytes = SAFE_OUT_SIZE; /* safe number of bytes to write
					       to OutString 	    	    */

  if (isnonvar(reg_term(2)))
    xsb_abort("Usage: fmt_write_string(-OutStr, +FmtStr, +args(A1,A2,...))");
  
  OutString[0] = '\0'; 	       	            /* anull the output string 	    */
  Fmt_term = reg_term(3);
  if (is_list(Fmt_term))
    Fmt = p_charlist_to_c_string(Fmt_term, Fmt_buf, sizeof(Fmt_buf),
				 "FMT_WRITE_STRING", "format string");
  else if (is_string(Fmt_term))
    Fmt = string_val(Fmt_term);
  else
    xsb_abort("FMT_WRITE_STRING: Format must be an atom or a character string");

  ValTerm = reg_term(4);
  if (is_functor(ValTerm))
    Arity = get_arity(get_str_psc(ValTerm));
  else if (is_var(ValTerm))
    /* Var in the argument position means, no arguments */
    Arity = 0;
  else {
    /* assume single argument; convert ValTerm into arg(val) */
    prolog_term TmpValTerm=p2p_new();

    c2p_functor("arg", 1, TmpValTerm);
    if (is_string(ValTerm))
      c2p_string(string_val(ValTerm), p2p_arg(TmpValTerm,1));
    else if (is_int(ValTerm))
      c2p_int(int_val(ValTerm), p2p_arg(TmpValTerm,1));
    else if (is_float(ValTerm))
      c2p_float(float_val(ValTerm), p2p_arg(TmpValTerm,1));
    else
      xsb_abort("Usage: fmt_write_string(-OutStr, +FmtStr, +args(A1,A2,...))");

    ValTerm = TmpValTerm;
    Arity = 1;
  }

  current_fmt_spec = next_format_substr(Fmt,
					1,  /* initialize     	      	     */
					0); /* write     	      	     */
  xsb_segfault_message =
    "++FMT_WRITE_STRING: Argument type doesn't match format specifier\n";
  signal(SIGSEGV, &xsb_segfault_catcher);
  
  i=0;
  while (i <= Arity) {
    /* last string (and has no conversion spec) */
    if (current_fmt_spec->type == '.') {
      SPRINT_ARG("");
      if (i < Arity)
	xsb_warn("FMT_WRITE_STRING: More arguments than format specifiers");
      CHECK_OUTPUT_SIZE; /* might xsb_abort */
      goto EXIT_WRITE_STRING;
    }

    i++; /* increment after checking the last format segment */

    if (current_fmt_spec->size >  1) {
      Arg = p2p_arg(ValTerm,i++);
      width = (int) int_val(Arg);
    } 
    CHECK_ARITY(i, Arity, "FMT_WRITE_STRING");

    if (current_fmt_spec->size == 3) {
      Arg = p2p_arg(ValTerm,i++);
      precision = (int) int_val(Arg);
    }
    CHECK_ARITY(i, Arity, "FMT_WRITE_STRING");

    Arg = p2p_arg(ValTerm,i);

    if (current_fmt_spec->type == 'S') {
      /* Any type: print as a string */
      offset = 0;
      print_pterm(Arg, 1, str_arg_buf, &offset);
      SPRINT_ARG(str_arg_buf);
    } else if (is_string(Arg)) {
      TYPE_ERROR_CHK('s', "FMT_WRITE_STRING");
      str_arg = string_val(Arg);
      SPRINT_ARG(str_arg);
    } else if (is_list(Arg)) {
      TYPE_ERROR_CHK('s', "FMT_WRITE_STRING");
      sprintf(aux_msg, "argument %d", i);
      str_arg = p_charlist_to_c_string(Arg, str_arg_buf, sizeof(str_arg_buf),
				       "FMT_WRITE_STRING", aux_msg);
      SPRINT_ARG(str_arg);
    } else if (is_int(Arg)) {
      TYPE_ERROR_CHK('i', "FMT_WRITE_STRING");
      int_arg = int_val(Arg);
      SPRINT_ARG(int_arg);
    } else if (is_float(Arg)) {
      TYPE_ERROR_CHK('f', "FMT_WRITE_STRING");
      float_arg = float_val(Arg);
      SPRINT_ARG(float_arg);
    } else {
      xsb_abort("FMT_WRITE_STRING: Argument %d has illegal type\n", i);
    }
    current_fmt_spec = next_format_substr(Fmt,
					  0 /* no initialize */,
					  0 /* write */ );
    CHECK_OUTPUT_SIZE; /* might xsb_abort */
  }

  /* print the remainder of the format string, if it exists */
  if (current_fmt_spec->type == '.') {
      SPRINT_ARG("");
      CHECK_OUTPUT_SIZE; /* might xsb_abort */
  }

 EXIT_WRITE_STRING:
  xsb_segfault_message = xsb_default_segfault_msg;
  signal(SIGSEGV, xsb_default_segfault_handler);

  /* fmt_write_string is used in places where interning of the string is needed
     (such as constructing library search paths)
     Therefore, must use string_find(..., 1). */
  ctop_string(2, string_find(OutString,1));
  
  return TRUE;
}



/*----------------------------------------------------------------------
   like fscanf
     C invocation: formatted_io(FMT_READ, IOport, Format, ArgTerm, Status)
     Prolog invocation: fmt_read(+IOport, +Format, -ArgTerm, -Status)
      IOport: XSB I/O port
      Format: format as atom or string;
      ArgTerm: Term whose args are vars to receive values returned.
      Status: 0 OK, -1 eof 
----------------------------------------------------------------------*/

bool fmt_read(void)
{
  char *Fmt=NULL;
  static char Fmt_buf[MAXBUFSIZE+1];
  prolog_term AnsTerm, Arg, Fmt_term;
  Integer i ;
  static char str_arg[MAXBUFSIZE];     	      /* holder for string arguments */
  static char aux_fmt[MAXBUFSIZE];     	      /* auxiliary fmt holder 	     */
  long int_arg;     	     	     	      /* holder for int args         */
  float float_arg;    	     	     	      /* holder for float args       */
  struct fmt_spec *current_fmt_spec;
  int Arity=0;
  int number_of_successes=0, curr_assignment=0;
  int cont; /* continuation indicator */
  int chars_accumulator=0, curr_chars_consumed=0;
  
  SET_FILEPTR(fptr, ptoc_int(2));
  Fmt_term = reg_term(3);
  if (is_list(Fmt_term))
    Fmt = p_charlist_to_c_string(Fmt_term, Fmt_buf, sizeof(Fmt_buf),
				 "FMT_READ", "format string");
  else if (is_string(Fmt_term))
    Fmt = string_val(Fmt_term);
  else
    xsb_abort("FMT_READ: Format must be an atom or a character string");

  AnsTerm = reg_term(4);
  if (is_functor(AnsTerm))
    Arity = get_arity(get_str_psc(AnsTerm));
  else if (is_var(AnsTerm)) {
    /* assume that only one input val is reuired */
    prolog_term TmpAnsTerm=p2p_new(), TmpArg;

    Arity = 1;
    c2p_functor("arg", 1, TmpAnsTerm);
    /* The following is a bit tricky: Suppose AnsTerm was X.
       We unify AnsTerm (which is avriable) with
       TmpArg, the argument of the new term TmpAnsTerm.
       Then the variable AnsTerm is reset to TmpAnsTerm so that the rest of the
       code would think that AnsTerm was arg(X).
       Eventually, X will get bound to the result */
    TmpArg = p2p_arg(TmpAnsTerm,1);
    p2p_unify(TmpArg, AnsTerm);
    AnsTerm = TmpAnsTerm;
  } else
    xsb_abort("Usage: fmt_read([IOport,] FmtStr, args(A1,A2,...), RetCode");

  /* status variable */
  if (isnonvar(reg_term(5)))
    xsb_abort("Usage: fmt_read([IOport,] FmtStr, args(A1,A2,...), RetCode");

  current_fmt_spec = next_format_substr(Fmt,
					1,   /* initialize    	      	     */
					1);  /* read    	      	     */
  strncpy(aux_fmt, current_fmt_spec->fmt, MAXBUFSIZE-4);
  strcat(aux_fmt,"%n");

  for (i = 1; (i <= Arity); i++) {
    Arg = p2p_arg(AnsTerm,i);
    cont = 0;
    curr_chars_consumed=0;

    /* if there was an assignment suppression spec, '*' */
    if (current_fmt_spec->size == 0)
      current_fmt_spec->type = '-';

    switch (current_fmt_spec->type) {
    case '-':
      /* we had an assignment suppression character: just count how 
	 many chars were scanned, don't skip to the next scan variable */
      fscanf(fptr, aux_fmt, &curr_chars_consumed);
      curr_assignment = 0;
      i--; /* don't skip scan variable */
      cont = 1; /* don't leave the loop */
      break;
    case '.': /* last format substring (and has no conversion spec) */
      curr_assignment = fscanf(fptr, current_fmt_spec->fmt);
      if (is_var(Arg))
	xsb_warn("FMT_READ: More arguments than format specifiers");
      goto EXIT_READ;
    case 's':
      curr_assignment = fscanf(fptr, aux_fmt,
			       str_arg, &curr_chars_consumed);
      /* if no match, leave prolog variable uninstantiated;
	 if it is a prolog constant, then return FALSE (no unification) */
      if (curr_assignment <= 0) {
	if (is_var(Arg)) break;
	else return FALSE;
      }
      if (is_var(Arg))
	c2p_string(str_arg,Arg);
      else if (strcmp(str_arg,string_val(Arg))) return FALSE;
      break;
    case 'n':
      int_arg = -1;
      curr_assignment = fscanf(fptr, current_fmt_spec->fmt, &int_arg);
      if (int_arg < 0) break; /* scanf failed before reaching %n */
      cont = 1; /* don't leave the loop */
      curr_chars_consumed = int_arg;
      int_arg += chars_accumulator;
      if (is_var(Arg))
	c2p_int(int_arg,Arg);
      else xsb_abort("FMT_READ: Argument %i must be a variable", i);
      break;
    case 'i':
      curr_assignment = fscanf(fptr, aux_fmt,
			       &int_arg, &curr_chars_consumed);
      /* if no match, leave prolog variable uninstantiated;
	 if it is a prolog constant, then return FALSE (no unification) */
      if (curr_assignment <= 0) {
	if (is_var(Arg)) break;
	else return FALSE;
      }
      if (is_var(Arg))
	c2p_int(int_arg,Arg);
      else if (int_arg != int_val(Arg)) return FALSE;
      break;
    case 'f':
      curr_assignment = fscanf(fptr, aux_fmt,
			       &float_arg, &curr_chars_consumed);
      /* floats never unify with anything */
      if (!is_var(Arg)) return FALSE;
      /* if no match, leave prolog variable uninstantiated */
      if (curr_assignment <= 0) break;
      c2p_float(float_arg, Arg);
      break;
    default:
      xsb_abort("FMT_READ: Unsupported format specifier for argument %d\n", i);
    }

    chars_accumulator +=curr_chars_consumed;

    /* format %n shouldn't cause us to leave the loop */
    if (curr_assignment > 0 || cont)
      number_of_successes =
	(curr_assignment ? number_of_successes+1 : number_of_successes);
    else
      break;

    current_fmt_spec = next_format_substr(Fmt,
					  0 /* no initialize */,
					  1 /* read */ );
    strcpy(aux_fmt, current_fmt_spec->fmt);
    strcat(aux_fmt,"%n");
  }

  /* if there are format specifiers beyond what corresponds to the last
     variable then we make use of %* (suppression) and of non-format
     strings. The leftover format specifiers are ignored. */
  /* last format substr without conversion spec */
  if (current_fmt_spec->type == '.')
    curr_assignment = fscanf(fptr, current_fmt_spec->fmt);
  /* last format substr with assignment suppression (spec size=0) */
  if (current_fmt_spec->size == 0)
    fscanf(fptr, aux_fmt, &curr_chars_consumed);

  /* check for end of file */
  if ((number_of_successes == 0) && (curr_assignment < 0))
    number_of_successes = -1;

 EXIT_READ:
  ctop_int(5, number_of_successes);
  return TRUE;
}

/**********
In scanning a canonical term, we maintain a functor stack, and an
operand stack. The functor stack has the name of the functor and a
pointer to its first operand on the operand stack. The operand stack
is just a stack of operands. They are Prolog terms. (How to handle
variables remains to be seen.)
***/

static Psc prevpsc = 0;


/* ----- handle read_cannonical errors: print msg and scan to end -----	*/
/***
reallocate op stack.
add clear findall stack at toploop
***/
static int findall_chunk_index;

static int read_can_error(FILE *filep, STRFILE *instr, int prevchar)
{
  char *ptr;

  xsb_error("READ_CAN_ERROR: illegal format. Next tokens:");
  while ((token->type != TK_EOC) && (token->type != TK_EOF)) {
    ptr = token->value;
    switch (token->type) {
    case TK_PUNC	: fprintf(stderr,"%c ", *ptr); break;
    case TK_VARFUNC	: fprintf(stderr,"%s ", ptr); break;
    case TK_VAR		: fprintf(stderr,"%s ", ptr); break;
    case TK_FUNC	: fprintf(stderr,"%s ", ptr); break;
    case TK_INT		: fprintf(stderr,"%d ", *(int *)ptr); break;
    case TK_ATOM	: fprintf(stderr,"%s ", ptr); break;
    case TK_VVAR	: fprintf(stderr,"%s ", ptr); break;
    case TK_VVARFUNC	: fprintf(stderr,"%s ", ptr); break;
    case TK_REAL	: fprintf(stderr,"%f ", *(double *)ptr); break;
    case TK_STR		: fprintf(stderr,"%s ", ptr); break;
    case TK_LIST	: fprintf(stderr,"%s ", ptr); break;
    case TK_HPUNC	: fprintf(stderr,"%c ", *ptr); break;
    case TK_INTFUNC	: fprintf(stderr,"%d ", *(int *)ptr); break;
    case TK_REALFUNC	: fprintf(stderr,"%f ", *(double *)ptr); break;
    }
    token = GetToken(filep,NULL,prevchar);
    prevchar = token-> nextch;
  }
  if (token->type == TK_EOC)
    fprintf(stderr,".\n");
  else
    fprintf(stderr,"\n");
  findall_free(findall_chunk_index) ;
  ctop_string(2,(char *)string_find("read_canonical_error",1));
  ctop_int(3,0);
  return TRUE;
}


/* Read a canonical term from XSB I/O port in r1 and put answer in variable in
   r2; r3 set to 0 if ground fact (non zero-ary), to 1 if variable or :-.
   Fail on EOF */

int read_canonical(void)
{
  FILE *filep;
  STRFILE *instr;
  int prevchar, arity, i, size;
  /* findall_solution_list *p; */
  CPtr h, this_term, prev_tail;
  Cell op1, j, arg2;
#define OPSTK_SIZE 1000
#define FUNFUN 0
#define FUNLIST 1
#define FUNDTLIST 2
  struct funstktype {
    char *fun;		/* functor name */
    Integer funop;	/* index into opstk of first operand */
	int funtyp;		/* 0 if functor, 1 if list, 2 if dotted-tail list */
  } funstk[OPSTK_SIZE];
  Cell funtop = 0;

  struct opstktype {
    int typ;
    prolog_term op;
  } opstk[OPSTK_SIZE];
  Cell optop = 0;

#define MAXVAR 1000
  struct vartype {
    Cell varid;
    prolog_term varval;
  } vars[MAXVAR];
  int cvarbot = MAXVAR-1;

  Pair sym;
  Float float_temp;
  char *cvar;
  int postopreq = FALSE, varfound = FALSE;
  long tempfp;
  prolog_term term;
  
  tempfp = ptoc_int(1);
  if (tempfp == -1000) {
    prevpsc = 0;
    return TRUE;
  }

  if ((tempfp < 0) && (tempfp >= -MAXIOSTRS)) {
    instr = strfileptr(tempfp);
    filep = NULL;
  } else {
    instr = NULL;
    SET_FILEPTR(filep, tempfp);
  }
  /* get findall buffer to read term into */
  findall_chunk_index = findall_init_c();
  current_findall = findall_solutions + findall_chunk_index;
  if (current_findall->tail == 0)
	xsb_exit("internal error 1 in read_canonical(findall)") ;
  h = current_findall->top_of_chunk ;
  size = 0;

  prevchar = 10;
  while (1) {
	token = GetToken(filep,instr,prevchar);
/*	print_token((int)(token->type),(char *)(token->value)); */
	prevchar = token->nextch;
	if (postopreq) {  /* must be an operand follower: , or ) or | or ] */
	    if (token->type == TK_PUNC) {
		if (*token->value == ')') {
		  funtop--;
		  if (funstk[funtop].funtyp != FUNFUN)	/* ending a list, oops */
		    return read_can_error(filep,instr,prevchar);
		  arity = optop - funstk[funtop].funop;
		  if ((h+arity+1) > (current_findall->current_chunk + FINDALL_CHUNCK_SIZE -1)) {
			if (!get_more_chunk()) return(0) ;
			h = current_findall->top_of_chunk ;
		  }
		  this_term = h;
		  op1 = funstk[funtop].funop;
		  if ((arity == 2) && !(strcmp(funstk[funtop].fun,"."))) {
			if (opstk[op1].typ == TK_VAR) { setvar(h,op1) }
			else cell(h) = opstk[op1].op;
			h++;
			if (opstk[op1+1].typ == TK_VAR) { setvar(h,op1+1) }
			else cell(h) = opstk[op1+1].op;
			h++;
			opstk[op1].op = makelist(this_term);
			opstk[op1].typ = TK_FUNC;
			size += 2;
		  } else {
		        size += arity+1;
			sym = (Pair)insert(funstk[funtop].fun,(char)arity,
				       (Psc)flags[CURRENT_MODULE],&i);
			new_heap_functor(h, sym->psc_ptr);
			for (j=op1; j<optop; h++,j++) {
			  if (opstk[j].typ == TK_VAR) { setvar(h,j) }
			  else cell(h) = opstk[j].op;
			}
			opstk[op1].op = makecs(this_term);
			opstk[op1].typ = TK_FUNC;
		  }
		  optop = op1;
		  optop++;
		} else if (*token->value == ']') {	/* end of list */
		  funtop--;
		  if (funstk[funtop].funtyp == FUNFUN)
			return read_can_error(filep,instr,prevchar);
		  if ((h+2) > (current_findall->current_chunk + FINDALL_CHUNCK_SIZE -1)) {
			if (!get_more_chunk()) return(0) ;
			h = current_findall->top_of_chunk ;
		  }
		  this_term = h;
		  op1 = funstk[funtop].funop;

		  if (opstk[op1].typ == TK_VAR) { setvar(h,op1) }
		  else cell(h) = opstk[op1].op;
		  h++;
		  size += 2;
		  if ((op1+1) == optop) {
			cell(h) = makenil;
			h++;
		  } else {
			prev_tail = h;
			h++;
			for (j=op1+1; j<optop-1; j++) {
			  if ((h+2) > (current_findall->current_chunk + FINDALL_CHUNCK_SIZE -1)) {
				if (!get_more_chunk()) return(0) ;
				h = current_findall->top_of_chunk ;
			  }
			  cell(prev_tail) = makelist(h);
			  if (opstk[j].typ == TK_VAR) { setvar(h,j) }
			  else cell(h) = opstk[j].op;
			  h++;
			  prev_tail = h;
			  h++;
			  size += 2;
			}
			j = optop-1;
			if (funstk[funtop].funtyp == FUNLIST) {
			  if ((h+2) > (current_findall->current_chunk + FINDALL_CHUNCK_SIZE -1)) {
				if (!get_more_chunk()) return(0) ;
				h = current_findall->top_of_chunk ;
			  }
			  cell(prev_tail) = makelist(h);
			  if (opstk[j].typ == TK_VAR) { setvar(h,j) }
			  else cell(h) = opstk[j].op;
			  h++;
			  prev_tail = h;
			  h++;
			  cell(prev_tail) = makenil;
			  size += 2;
			} else {
			  if (opstk[j].typ == TK_VAR) { setvar(prev_tail,j) }
			  else cell(prev_tail) = opstk[j].op;
			}
		  }
		  opstk[op1].op = makelist(this_term);
		  opstk[op1].typ = TK_FUNC;
		  optop = op1;
		  optop++;
		} else if (*token->value == ',') {
		  postopreq = FALSE;
		} else if (*token->value == '|') {
		  postopreq = FALSE;
		  if (funstk[funtop-1].funtyp != FUNLIST) 
			return read_can_error(filep,instr,prevchar);
		  funstk[funtop-1].funtyp = FUNDTLIST;
		} else return read_can_error(filep,instr,prevchar);
      } else {  /* check for neg numbers and backpatch if so */
		if (opstk[optop-1].typ == TK_ATOM && 
				!strcmp("-",string_val(opstk[optop-1].op))) {
		  if (token->type == TK_INT) {
			opstk[optop-1].typ = TK_INT;
			opstk[optop-1].op = makeint(-(*(int *)token->value));
		  } else if (token->type == TK_REAL) {
			opstk[optop-1].typ = TK_REAL;
			float_temp = (Float) *(double *)(token->value);
			opstk[optop-1].op = makefloat(-float_temp);
		  } else return read_can_error(filep,instr,prevchar);
		} else return read_can_error(filep,instr,prevchar);
      }
    } else {  /* must be an operand */
      switch (token->type) {
      case TK_PUNC:
		if (*token->value == '[') {
		  if(token->nextch == ']') {
			if (optop >= OPSTK_SIZE)
			  xsb_abort("read_canonical op stack overflow");
			token = GetToken(filep,instr,prevchar);
			/* print_token(token->type,token->value); */
			prevchar = token->nextch;
			opstk[optop].typ = TK_ATOM;
			opstk[optop].op = makenil;
			optop++;
			postopreq = TRUE;
		  } else {	/* beginning of a list */
			if (funtop >= OPSTK_SIZE)
			  xsb_abort("read_canonical fun stack overflow");
			funstk[funtop].funop = optop;
			funstk[funtop].funtyp = FUNLIST;	/* assume regular list */
			funtop++;
		  }
		  break;
		}
	  /* let a punctuation mark be a functor symbol */
      case TK_FUNC:
	        if (funtop >= OPSTK_SIZE)
		  xsb_abort("read_canonical op stack overflow");
		funstk[funtop].fun = (char *)string_find(token->value,1);
		funstk[funtop].funop = optop;
		funstk[funtop].funtyp = FUNFUN;	/* functor */
		funtop++;

		if (token->nextch != '(')
			return read_can_error(filep,instr,prevchar);
		token = GetToken(filep,instr,prevchar);
		/* print_token(token->type,token->value); */
		prevchar = token->nextch;
		break;
      case TK_VVAR:
	        if ((token->value)[1] == 0) { /* anonymous var */
		  if (cvarbot < 0)
		    xsb_abort("Too many variables in read_canonical term");
		  i = cvarbot;
		  vars[cvarbot].varid = (Cell) "_";
		  vars[cvarbot].varval = 0;
		  cvarbot--;
		  if (optop >= OPSTK_SIZE)
		    xsb_abort("read_canonical op stack overflow");
		  opstk[optop].typ = TK_VAR;
		  opstk[optop].op = (prolog_term) i;
		  optop++;
		  postopreq = TRUE;
		  break;
		}  /* else fall through and treat as regular var*/
      case TK_VAR:
		varfound = TRUE;
		cvar = (char *)string_find(token->value,1);
		i = MAXVAR-1;
		while (i>cvarbot) {
		  if (cvar == (char *)vars[i].varid) break;
		  i--;
		}
		if (i == cvarbot) {
		  if (cvarbot < 0)
		    xsb_abort("Too many variables in read_canonical term");
		  vars[cvarbot].varid = (Cell) cvar;
		  vars[cvarbot].varval = 0;
		  cvarbot--;
		}
		if (optop >= OPSTK_SIZE)
		  xsb_abort("read_canonical op stack overflow");
		opstk[optop].typ = TK_VAR;
		opstk[optop].op = (prolog_term) i;
		optop++;
		postopreq = TRUE;
		break;
      case TK_REAL:
	        if (optop >= OPSTK_SIZE)
		  xsb_abort("read_canonical op stack overflow");
		opstk[optop].typ = TK_REAL;
		float_temp = (float) *(double *)(token->value);
		opstk[optop].op = makefloat(float_temp);
		optop++;
		postopreq = TRUE;
		break;
      case TK_INT:
	        if (optop >= OPSTK_SIZE)
		  xsb_abort("read_canonical op stack overflow");
		opstk[optop].typ = TK_INT;
		opstk[optop].op = makeint(*(long *)token->value);
		optop++;
		postopreq = TRUE;
		break;
      case TK_ATOM:
	        if (optop >= OPSTK_SIZE)
		  xsb_abort("read_canonical op stack overflow");
		opstk[optop].typ = TK_ATOM;
		opstk[optop].op = makestring((char *)string_find(token->value,1));
		optop++;
		postopreq = TRUE;
		break;
      case TK_EOF:
		ctop_string(2,string_find("end_of_file",1));
		ctop_int(3,0);
		return TRUE;
      default: return read_can_error(filep,instr,prevchar);
      }
    }
    if (funtop == 0) {  /* term is finished */
      token = GetToken(filep,instr,prevchar);
      /* print_token(token->type,token->value); */
      prevchar = token->nextch;
      if (token->type != TK_EOC) return read_can_error(filep,instr,prevchar);

      if (opstk[0].typ != TK_VAR) {  /* if a variable, then a noop */
	term = opstk[0].op;
	/* p = findall_solutions + findall_chunk_index;*/
	check_glstack_overflow(3, pcreg, size*sizeof(Cell)) ;
	/*printf("checked overflow: size: %d\n",size*sizeof(Cell));*/
	arg2 = (Cell)Areg(2);
	deref(arg2);
	if (isnonvar(arg2)) 
	  xsb_abort("read_canonical argument must be a variable\n");
	bind_ref((CPtr)arg2,hreg);  /* build a new var to trail binding */
	new_heap_free(hreg);
	gl_bot = (CPtr)glstack.low; gl_top = (CPtr)glstack.high; /*??*/
	findall_copy_to_heap(term,(CPtr)arg2,&hreg) ; /* this can't fail */
	findall_free(findall_chunk_index) ; 
	
	deref(arg2);
	term = (prolog_term) arg2;
	if (isinteger(term) || 
	    isfloat(term) || 
	    isstring(term) ||
	    varfound || 
	    (isconstr(term) && !strcmp(":-",get_name(get_str_psc(term))))) {
	  ctop_int(3,0);
	  prevpsc = 0;
	}
	else if (get_str_psc(term) == prevpsc) {
	  ctop_int(3, (Integer)prevpsc);
	} else {
	  prevpsc = get_str_psc(term);
	  ctop_int(3,0);
	}
      } else {
	ctop_int(3,0);
	prevpsc = 0;
      }
contcase:
      return TRUE;
    }
  }
}


/* Scan format string and return format substrings ending with a conversion
   spec. The return value is a ptr to a struct that has the type of conversion
   spec (i, f, s) and the format substring ('.' if the whole format string has
   been scanned).

   This function doesn't fully check the validity of the conversion
   specifier. In case of a mistake, the result is unpredictable.
   We insist that a single % is a beginning of a format specifier.

   FORMAT: format string, INITIALIZE: 1-process new fmt string; 0 - continue
   with old fmt string. READ: 1 if this is called for read op; 0 for write.  */
struct fmt_spec *next_format_substr(char *format, int initialize, int read_op)
{
  static int current_substr_start;   /* current substr pointer */
  static char workspace[MAXBUFSIZE]; /* copy of format used as workspace */
  static char saved_char;    	     /* place to save char that we
				        temporarily replace with '\0' */
  static int length;	    	     /* length of format string */
  int pos, keep_going;
  char *ptr;
  static struct fmt_spec result;
  char *exclude, *expect; /* characters to exclude or expect */

  if (initialize) {
    current_substr_start = 0;
    length = strlen(format);
    strncpy(workspace, format, MAXBUFSIZE);
    workspace[length] = '\0';
  } else {
    /* restore char that was replaced with \0 */
    workspace[current_substr_start] = saved_char;
  }

  pos = current_substr_start;
  result.type = '?';
  result.size = 1;

  /* done scanning format string */
  if (current_substr_start >= length) {
    result.type = '.'; /* last substring (and has no conversion spec) */
    result.fmt = "";
    return(&result);
  }

  /* find format specification: % not followed by % */
  do {
    /* last substring (and has no conversion spec) */
    if ((ptr=strchr(workspace+pos, '%')) == NULL) {
      current_substr_start = length;
      result.type = '.';  /* last substring with no type specifier */
      result.fmt = workspace+pos;
      return(&result);
    }

    pos = (ptr - workspace) + 1;
    if (workspace[pos] == '%')
      pos++;
    else break;
  } while (1);

  /* this doesn't do full parsing; it assumes anything that starts at % and
     ends at a valid conversion character is a conversion specifier. */ 
  keep_going = TRUE;
  expect = exclude = "";
  while ((pos < length) && keep_going) {
    if (strchr(exclude, workspace[pos]) != NULL) {
      xsb_abort("Illegal format specifier `%c' in: %s",
		workspace[pos], workspace+current_substr_start);
    }
    if (strlen(expect) && strchr(expect, workspace[pos]) == NULL) {
      xsb_abort("Illegal format specifier `%c' in: %s",
		workspace[pos], workspace+current_substr_start);
    }

    expect = exclude = "";

    switch (workspace[pos++]) {
    case '1': /* flags, precision, etc. */
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      exclude = "+- #[]";
      break;
    case '.':
      exclude = "+- #[]";
      expect = "0123456789*";
      break;
    case '0':
    case '+':
    case '-':
      exclude = "+-[]";
      break;
    case 'h':
    case 'l':
      exclude = "+- #[]hlL";
      expect = "diouxXn";
      break;
    case 'L':
      expect = "eEfgG";
      exclude = "+- #[]hlL";
      break;
    case ' ':
    case '#':
      exclude = "+- #[]hlL";
      break;
    case 'c':
      if (read_op)
	result.type = 's';
      else
	result.type = 'i';
      keep_going = FALSE;
      break;
    case 'd':
    case 'i':
    case 'u':
    case 'o':
    case 'x':
    case 'X':
      keep_going = FALSE;
      result.type = 'i'; /* integer or character */
      break;
    case 'e':
    case 'E':
    case 'f':
    case 'g':
    case 'G':
      keep_going = FALSE;
      result.type = 'f'; /* float */
      break;
    case 's':
      keep_going = FALSE;
      result.type = 's'; /* string */
      break;
    case 'S':
      keep_going = FALSE;
      result.type = 'S'; /* string */
      workspace[pos-1] = 's';
      break;
    case 'p':
      xsb_abort("Format specifier %%p not supported: %s",
		workspace+current_substr_start);
    case 'n':
      if (read_op) {
	result.type = 'n'; /* %n is like integer, but in fmt_read we treat it
			      specially */
	keep_going = FALSE;
	break;
      }
      xsb_abort("Format specifier %%n not supported: %s",
		workspace+current_substr_start);
    case '[':
      /* scanf feature: [...] */
      if (!read_op) {
	xsb_abort("Format specifier [ is invalid for output: %s",
		  workspace+current_substr_start);
      }
      while ((pos < length) && (workspace[pos++] != ']'));
      if (workspace[pos-1] != ']') {
	xsb_abort("Format specifier [ has no matching ] in: %s",
		  workspace+current_substr_start);
      }
      result.type = 's';
      keep_going = FALSE;
      break;

    case '*':
      if (read_op) {
	result.size = 0;
	break;
      }
      if (strncmp(workspace+pos, ".*", 2) == 0) {
	pos = pos+2;
	expect = "feEgEscdiuoxX";
	result.size = 3;
      } else if (workspace[pos] == '.') {
	pos++;
	expect = "0123456789";
	result.size = 2;
      } else {
	result.size = 2;
	expect = "feEgEscdiuoxX";
      }
      break;

    default:
      xsb_abort("Character `%c' in illegal format context: %s",
		workspace[pos-1], workspace+current_substr_start);
    }
  }

  saved_char = workspace[pos];
  workspace[pos] = '\0';
  result.fmt = workspace+current_substr_start;
  current_substr_start = pos;
  return(&result);
}


/* Take a FILE pointer and return an XSB stream, an index into the XSB table of
   open files; return -1, if too many open files. */
int xsb_intern_file(FILE *fptr, char *context)
{
  int i;
  for (i=MIN_USR_OPEN_FILE; i < MAX_OPEN_FILES && open_files[i] != NULL; i++);
  if (i == MAX_OPEN_FILES) {
    xsb_warn("%s: Too many open files", context);
    return -1;
  } else 
    open_files[i] = fptr;
  return i;
}

