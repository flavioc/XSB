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

#include "xsb_config.h"
#include "xsb_debug.h"

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
#include "loader_xsb.h" /* for ZOOM_FACTOR */
#include "subp.h"
#include "tries.h"
#include "choice.h"
#include "macro_xsb.h"
#include "io_builtins_xsb.h"
#include "wind2unix.h"
#include "binding.h"
#include "deref.h"
#include "findall.h"

FILE *open_files[MAX_OPEN_FILES]; /* open file table */


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
  /* size: in case of a write op a the * format specifier (e.g., %*.*d), size
     is 1, 2, or 3, depending the number of *'s in that particular format.
     If there are no *'s, then this format correspnds to exactly one argument,
     so size=1. If there is one "*", then this format corresponds to 2
     arguments, etc.
     This tells how manu arguments to expect.
     In case of a read operation, size can be 0, since here '*' means
     assignment suppression. */
  char size;
  char *fmt;
};

struct fmt_spec *next_format_substr(char*, int, int);
char *p_charlist_to_c_string(prolog_term, VarString*, char*, char*);

/* type is a char: 's', 'i', 'f' */
#define TYPE_ERROR_CHK(ch_type, Label) \
        if (current_fmt_spec->type != ch_type) { \
	    xsb_abort("[%s] Type mismatch in argument value %d", Label, i); \
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
	      xsb_abort("[%s] Not enough arguments for given format", Label); \
	}

#ifdef HAVE_SNPRINTF
/* like PRINT_ARG, but uses snprintf */
#define SPRINT_ARG(arg) \
        XSB_StrEnsureSize(&OutString, OutString.length+SAFE_OUT_SIZE); \
        switch (current_fmt_spec->size) { \
        case 1: bytes_formatted=snprintf(OutString.string+OutString.length, \
					 SAFE_OUT_SIZE, \
					 current_fmt_spec->fmt, arg); \
	        break; \
	case 2: bytes_formatted=snprintf(OutString.string+OutString.length, \
					 SAFE_OUT_SIZE, \
					 current_fmt_spec->fmt, width, arg); \
	        break; \
	case 3: bytes_formatted=snprintf(OutString.string+OutString.length, \
					 SAFE_OUT_SIZE, \
					 current_fmt_spec->fmt, \
					 width, precision, arg); \
	        break; \
	} \
        OutString.length += bytes_formatted; \
        XSB_StrNullTerminate(&OutString);

#else
/* like PRINT_ARG, but uses sprintf -- used with old compilers that don't have
   snprintf.  This is error-prone: don't use broken compilers!!!
   In some systems sprintf returns it's first argument, so have to use
   strlen to count bytes formatted, for portability.
   */
#define SPRINT_ARG(arg) \
    	XSB_StrEnsureSize(&OutString, OutString.length+SAFE_OUT_SIZE); \
     	switch (current_fmt_spec->size) { \
        case 1: sprintf(OutString.string+OutString.length, \
			current_fmt_spec->fmt, arg); \
		bytes_formatted = strlen(OutString.string+OutString.length); \
	        break; \
	case 2: sprintf(OutString.string+OutString.length, \
			current_fmt_spec->fmt, width, arg); \
		bytes_formatted = strlen(OutString.string+OutString.length); \
	        break; \
	case 3: sprintf(OutString.string+OutString.length, \
			current_fmt_spec->fmt, \
			width, precision, arg); \
		bytes_formatted = strlen(OutString.string+OutString.length); \
	        break; \
	} \
        OutString.length += bytes_formatted; \
        XSB_StrNullTerminate(&OutString);
#endif


/* these are static & global, to save on alloc overhead and large footprint  */
static XSB_StrDefine(FmtBuf);  	       	      /* holder of format            */
static XSB_StrDefine(StrArgBuf);      	      /* holder for string arguments */

xsbBool fmt_write(void);
xsbBool fmt_write_string(void);
xsbBool fmt_read(void);


#include "ptoc_tag_xsb_i.h"
    	    

xsbBool formatted_io (void)
{
  switch (ptoc_int(1)) {
  case FMT_WRITE: return fmt_write();
  case FMT_WRITE_STRING: return fmt_write_string();
  case FMT_READ: return fmt_read();
  default:
    xsb_abort("[FORMATTED_IO] Invalid operation number: %d", ptoc_int(1));
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


xsbBool fmt_write(void)
{
  char *Fmt=NULL, *str_arg;
  static char aux_msg[50];
  prolog_term ValTerm, Arg, Fmt_term;
  int i, Arity=0;
  long int_arg;     	     	     	      /* holder for int args         */
  float float_arg;    	     	     	      /* holder for float args       */
  struct fmt_spec *current_fmt_spec;
  int width=0, precision=0;    	     	      /* these are used in conjunction
						 with the *.* format         */
  XSB_StrSet(&StrArgBuf,"");

  SET_FILEPTR(fptr, ptoc_int(2));
  Fmt_term = reg_term(3);
  if (islist(Fmt_term))
    Fmt = p_charlist_to_c_string(Fmt_term,&FmtBuf,"FMT_WRITE","format string");
  else if (isstring(Fmt_term))
    Fmt = string_val(Fmt_term);
  else
    xsb_abort("[FMT_WRITE] Format must be an atom or a character string");

  ValTerm = reg_term(4);
  if (isconstr(ValTerm))
    Arity = get_arity(get_str_psc(ValTerm));
  else if (isref(ValTerm))
    /* Var in the argument position means, no arguments */
    Arity = 0;
  else {
    /* assume single argument; convert ValTerm into arg(val) */
    prolog_term TmpValTerm=p2p_new();

    c2p_functor("arg", 1, TmpValTerm);
    if (isstring(ValTerm))
      c2p_string(string_val(ValTerm), p2p_arg(TmpValTerm,1));
    else if (isinteger(ValTerm)|isboxedinteger(ValTerm))
      c2p_int(oint_val(ValTerm), p2p_arg(TmpValTerm,1));
    else if (isfloat(ValTerm))
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
	xsb_warn("[FMT_WRITE] More arguments than format specifiers");
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

    if (current_fmt_spec->type == '!') { /* ignore field */
    } else if (current_fmt_spec->type == 'S') {
      /* Any type: print as a string */
      XSB_StrSet(&StrArgBuf,"");
      print_pterm(Arg, TRUE, &StrArgBuf);
      PRINT_ARG(StrArgBuf.string);
    } else if (isstring(Arg) && !isnil(Arg)) {
      TYPE_ERROR_CHK('s', "FMT_WRITE");
      str_arg = string_val(Arg);
      PRINT_ARG(str_arg);
    } else if (islist(Arg) || isnil(Arg)) {
      TYPE_ERROR_CHK('s', "FMT_WRITE");
      sprintf(aux_msg, "argument %d", i);
      str_arg = p_charlist_to_c_string(Arg, &StrArgBuf, "FMT_WRITE", aux_msg);
      PRINT_ARG(str_arg);
    } else if (isinteger(Arg)|isboxedinteger(Arg)) {
      TYPE_ERROR_CHK('i', "FMT_WRITE");
      int_arg = oint_val(Arg);
      PRINT_ARG(int_arg);
    } else if (isfloat(Arg)) {
      TYPE_ERROR_CHK('f', "FMT_WRITE")
      float_arg = float_val(Arg);
      PRINT_ARG(float_arg);
    } else {
      xsb_abort("[FMT_WRITE] Argument %d has illegal type", i);
    }
    current_fmt_spec = next_format_substr(Fmt,
					  0 /* don't initialize */,
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

xsbBool fmt_write_string(void)
{
  char *Fmt=NULL, *str_arg;
  static XSB_StrDefine(OutString);
  static char aux_msg[50];
  prolog_term ValTerm, Arg, Fmt_term;
  int i, Arity;
  long int_arg;     	     	     	    /* holder for int args     	    */
  float float_arg;     	     	     	    /* holder for float args   	    */
  struct fmt_spec *current_fmt_spec;
  int width=0, precision=0;      	    /* these are used in conjunction
					       with the *.* format     	    */
  int bytes_formatted=0;       	       	    /* the number of bytes formatted as
					       returned by sprintf/snprintf */
  XSB_StrSet(&StrArgBuf,"");
  XSB_StrSet(&OutString,"");

  if (isnonvar(reg_term(2)))
    xsb_abort("[FMT_WRITE_STRING] Arg 1 must be an unbound variable");
  
  Fmt_term = reg_term(3);
  if (islist(Fmt_term))
    Fmt = p_charlist_to_c_string(Fmt_term, &FmtBuf,
				 "FMT_WRITE_STRING", "format string");
  else if (isstring(Fmt_term))
    Fmt = string_val(Fmt_term);
  else
    xsb_abort("[FMT_WRITE_STRING] Format must be an atom or a character string");

  ValTerm = reg_term(4);
  if (isconstr(ValTerm))
    Arity = get_arity(get_str_psc(ValTerm));
  else if (isref(ValTerm))
    /* Var in the argument position means, no arguments */
    Arity = 0;
  else {
    /* assume single argument; convert ValTerm into arg(val) */
    prolog_term TmpValTerm=p2p_new();

    c2p_functor("arg", 1, TmpValTerm);
    if (isstring(ValTerm))
      c2p_string(string_val(ValTerm), p2p_arg(TmpValTerm,1));
    else if (isinteger(ValTerm)|isboxedinteger(ValTerm))
      c2p_int(oint_val(ValTerm), p2p_arg(TmpValTerm,1));
    else if (isfloat(ValTerm))
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
	xsb_warn("[FMT_WRITE_STRING] More arguments than format specifiers");
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

    if (current_fmt_spec->type == '!') { /* ignore field */
    } else if (current_fmt_spec->type == 'S') {
      /* Any type: print as a string */
      XSB_StrSet(&StrArgBuf,"");
      print_pterm(Arg, TRUE, &StrArgBuf);
      SPRINT_ARG(StrArgBuf.string);
    } else if (isstring(Arg)) {
      TYPE_ERROR_CHK('s', "FMT_WRITE_STRING");
      str_arg = string_val(Arg);
      SPRINT_ARG(str_arg);
    } else if (islist(Arg)) {
      TYPE_ERROR_CHK('s', "FMT_WRITE_STRING");
      sprintf(aux_msg, "argument %d", i);
      str_arg = p_charlist_to_c_string(Arg, &StrArgBuf,
				       "FMT_WRITE_STRING", aux_msg);
      SPRINT_ARG(str_arg);
    } else if (isinteger(Arg)|isboxedinteger(Arg)) {
      TYPE_ERROR_CHK('i', "FMT_WRITE_STRING");
      int_arg = oint_val(Arg);
      SPRINT_ARG(int_arg);
    } else if (isfloat(Arg)) {
      TYPE_ERROR_CHK('f', "FMT_WRITE_STRING");
      float_arg = float_val(Arg);
      SPRINT_ARG(float_arg);
    } else {
      xsb_abort("[FMT_WRITE_STRING] Argument %d has illegal type", i);
    }
    current_fmt_spec = next_format_substr(Fmt,
					  0 /* don't initialize */,
					  0 /* write */ );
  }

  /* print the remainder of the format string, if it exists */
  if (current_fmt_spec->type == '.') {
      SPRINT_ARG("");
  }

 EXIT_WRITE_STRING:
  xsb_segfault_message = xsb_default_segfault_msg;
  signal(SIGSEGV, xsb_default_segfault_handler);

  /* fmt_write_string is used in places where interning of the string is needed
     (such as constructing library search paths)
     Therefore, must use string_find(..., 1). */
  ctop_string(2, string_find(OutString.string,1));
  
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

xsbBool fmt_read(void)
{
  char *Fmt=NULL;
  prolog_term AnsTerm, Arg, Fmt_term;
  Integer i ;
  static XSB_StrDefine(aux_fmt);     	      /* auxiliary fmt holder 	     */
  long int_arg;     	     	     	      /* holder for int args         */
  float float_arg;    	     	     	      /* holder for float args       */
  struct fmt_spec *current_fmt_spec;
  int Arity=0;
  int number_of_successes=0, curr_assignment=0;
  int cont; /* continuation indicator */
  int chars_accumulator=0, curr_chars_consumed=0;

  SET_FILEPTR(fptr, ptoc_int(2));
  Fmt_term = reg_term(3);
  if (islist(Fmt_term))
    Fmt = p_charlist_to_c_string(Fmt_term,&FmtBuf,"FMT_READ","format string");
  else if (isstring(Fmt_term))
    Fmt = string_val(Fmt_term);
  else
    xsb_abort("[FMT_READ] Format must be an atom or a character string");

  AnsTerm = reg_term(4);
  if (isconstr(AnsTerm))
    Arity = get_arity(get_str_psc(AnsTerm));
  else if (isref(AnsTerm)) {
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
    xsb_abort("Usage: fmt_read([IOport,] FmtStr, args(A1,A2,...), Feedback)");

  /* status variable */
  if (isnonvar(reg_term(5)))
    xsb_abort("[FMT_READ] Arg 4 must be an unbound variable");

  current_fmt_spec = next_format_substr(Fmt,
					1,   /* initialize    	      	     */
					1);  /* read    	      	     */
  XSB_StrSet(&aux_fmt, current_fmt_spec->fmt);
  XSB_StrAppend(&aux_fmt,"%n");

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
      fscanf(fptr, aux_fmt.string, &curr_chars_consumed);
      curr_assignment = 0;
      i--; /* don't skip scan variable */
      cont = 1; /* don't leave the loop */
      break;
    case '.': /* last format substring (and has no conversion spec) */
      curr_assignment = fscanf(fptr, current_fmt_spec->fmt);
      if (isref(Arg))
	xsb_warn("[FMT_READ] More arguments than format specifiers");
      goto EXIT_READ;
    case 's':
      XSB_StrEnsureSize(&StrArgBuf, MAX_IO_BUFSIZE);
      curr_assignment = fscanf(fptr, aux_fmt.string,
			       StrArgBuf.string,
			       &curr_chars_consumed);
      /* if no match, leave prolog variable uninstantiated;
	 if it is a prolog constant, then return FALSE (no unification) */
      if (curr_assignment <= 0) {
	if (isref(Arg)) break;
	else return FALSE;
      }
      if (isref(Arg))
	c2p_string(StrArgBuf.string,Arg);
      else if (strcmp(StrArgBuf.string, string_val(Arg)))
	return FALSE;
      break;
    case 'n':
      int_arg = -1;
      curr_assignment = fscanf(fptr, current_fmt_spec->fmt, &int_arg);
      if (int_arg < 0) break; /* scanf failed before reaching %n */
      cont = 1; /* don't leave the loop */
      curr_chars_consumed = int_arg;
      int_arg += chars_accumulator;
      if (isref(Arg))
	c2p_int(int_arg,Arg);
      else xsb_abort("[FMT_READ] Argument %i must be a variable", i);
      break;
    case 'i':
      curr_assignment = fscanf(fptr, aux_fmt.string,
			       &int_arg, &curr_chars_consumed);
      /* if no match, leave prolog variable uninstantiated;
	 if it is a prolog constant, then return FALSE (no unification) */
      if (curr_assignment <= 0) {
	if (isref(Arg)) break;
	else return FALSE;
      }
      if (isref(Arg))
	c2p_int(int_arg,Arg);
      else if (int_arg != oint_val(Arg)) return FALSE;
      break;
    case 'f':
      curr_assignment = fscanf(fptr, aux_fmt.string,
			       &float_arg, &curr_chars_consumed);
      /* floats never unify with anything */
      if (!isref(Arg)) return FALSE;
      /* if no match, leave prolog variable uninstantiated */
      if (curr_assignment <= 0) break;
      c2p_float(float_arg, Arg);
      break;
    default:
      xsb_abort("[FMT_READ] Unsupported format specifier for argument %d", i);
    }

    chars_accumulator +=curr_chars_consumed;

    /* format %n shouldn't cause us to leave the loop */
    if (curr_assignment > 0 || cont)
      number_of_successes =
	(curr_assignment ? number_of_successes+1 : number_of_successes);
    else
      break;

    current_fmt_spec = next_format_substr(Fmt,
					  0 /* don't initialize */,
					  1 /* read */ );
    XSB_StrSet(&aux_fmt, current_fmt_spec->fmt);
    XSB_StrAppend(&aux_fmt,"%n");
  }

  /* if there are format specifiers beyond what corresponds to the last
     variable then we make use of %* (suppression) and of non-format
     strings. The leftover format specifiers are ignored. */
  /* last format substr without conversion spec */
  if (current_fmt_spec->type == '.')
    curr_assignment = fscanf(fptr, current_fmt_spec->fmt);
  /* last format substr with assignment suppression (spec size=0) */
  if (current_fmt_spec->size == 0)
    fscanf(fptr, aux_fmt.string, &curr_chars_consumed);

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

CPtr init_term_buffer() {
  findall_chunk_index = findall_init_c();
  current_findall = findall_solutions + findall_chunk_index;
  return current_findall->top_of_chunk ;
}

#define ensure_term_space(ptr,size) \
  if ((ptr+size) > (current_findall->current_chunk + FINDALL_CHUNCK_SIZE -1)) {\
	if (!get_more_chunk()) xsb_abort("Cannot allocate space for term buffer") ;\
	ptr = current_findall->top_of_chunk ;\
  }

#define free_term_buffer() findall_free(findall_chunk_index)

static int read_can_error(FILE *filep, STRFILE *instr, int prevchar, Cell prologvar)
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
    token = GetToken(filep,instr,prevchar);
    prevchar = token-> nextch;
  }
  if (token->type == TK_EOC)
    fprintf(stderr,".\n");
  else
    fprintf(stderr,"\n");
  free_term_buffer();
  unify(prologvar,makestring(string_find("read_canonical_error",1)));
  return 0;
}


/* Read a canonical term from XSB I/O port in r1 and put answer in variable in
   r2; r3 set to 0 if ground fact (non zero-ary), to 1 if variable or :-.
   Fail on EOF */

#define INIT_STK_SIZE 32
#define MAX_INIT_STK_SIZE 1000
int opstk_size = 0;
int funstk_size = 0;

#define expand_opstk {\
    opstk_size = opstk_size+opstk_size;\
    opstk = (struct opstktype *)realloc(opstk,opstk_size*sizeof(struct opstktype));\
    if (!opstk) xsb_exit("Out of space for read_canonical stacks");\
    /*printf("RC opstk expanded to %d\n",opstk_size);*/ \
  }
#define expand_funstk {\
    funstk_size = funstk_size+funstk_size;\
    funstk = (struct funstktype *)realloc(funstk,funstk_size*sizeof(struct funstktype));\
    if (!funstk) xsb_exit("Out of space for read_canonical stacks");\
    /*printf("RC funstk expanded to %d\n",funstk_size);*/ \
  }

#define FUNFUN 0
#define FUNLIST 1
#define FUNDTLIST 2
  struct funstktype {
    char *fun;		/* functor name */
    int funop;	        /* index into opstk of first operand */
    int funtyp; 	/* 0 if functor, 1 if list, 2 if dotted-tail list */
  } *funstk;

  struct opstktype {
    int typ;
    prolog_term op;
  } *opstk;

#define MAXVAR 1000
  struct vartype {
    Cell varid;
    prolog_term varval;
  } vars[MAXVAR];

int read_canonical(void)
{
  FILE *filep;
  STRFILE *instr;
  long tempfp;
  Cell prologvar;
  
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
  prologvar = ptoc_tag(2);
  ctop_int(3,read_canonical_term(filep, instr, prologvar));
  return TRUE;
}

/* read canonical term, and return prev psc pointer, if valid */
int read_canonical_term(FILE *filep, STRFILE *instr, Cell prologvar)
{
  int funtop = 0;
  int optop = 0;
  int cvarbot = MAXVAR-1;
  int prevchar, arity, i, size;
  CPtr h;
  int j, op1, retpscptr;
  Pair sym;
  Float float_temp;
  char *cvar;
  int postopreq = FALSE, varfound = FALSE;
  prolog_term term;
  
  if (opstk_size == 0) {
    opstk = 
      (struct opstktype *)malloc(INIT_STK_SIZE*sizeof(struct opstktype));
    opstk_size = INIT_STK_SIZE;
    funstk = 
      (struct funstktype *)malloc(INIT_STK_SIZE*sizeof(struct funstktype));
    funstk_size = INIT_STK_SIZE;
  }

  /* get findall buffer to read term into */
  h = init_term_buffer();
  size = 0;

  prevchar = 10;
  while (1) {
	token = GetToken(filep,instr,prevchar);
/*	print_token((int)(token-f>type),(char *)(token->value)); */
	prevchar = token->nextch;
	if (postopreq) {  /* must be an operand follower: , or ) or | or ] */
	    if (token->type == TK_PUNC) {
		if (*token->value == ')') {
		  CPtr this_term;
		  funtop--;
		  if (funstk[funtop].funtyp != FUNFUN)	/* ending a list, oops */
		    return read_can_error(filep,instr,prevchar,prologvar);
		  arity = optop - funstk[funtop].funop;
		  ensure_term_space(h,arity+1);
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
		  optop = op1+1;
		} else if (*token->value == ']') {	/* end of list */
		  CPtr this_term, prev_tail;
		  funtop--;
		  if (funstk[funtop].funtyp == FUNFUN)
			return read_can_error(filep,instr,prevchar,prologvar);
		  ensure_term_space(h,2);
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
			  ensure_term_space(h,2);
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
			  ensure_term_space(h,2);
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
		  optop = op1+1;
		} else if (*token->value == ',') {
		  postopreq = FALSE;
		} else if (*token->value == '|') {
		  postopreq = FALSE;
		  if (funstk[funtop-1].funtyp != FUNLIST) 
			return read_can_error(filep,instr,prevchar,prologvar);
		  funstk[funtop-1].funtyp = FUNDTLIST;
		} else return read_can_error(filep,instr,prevchar,prologvar);
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
		  } else return read_can_error(filep,instr,prevchar,prologvar);
		} else return read_can_error(filep,instr,prevchar,prologvar);
      }
    } else {  /* must be an operand */
      switch (token->type) {
      case TK_PUNC:
		if (*token->value == '[') {
		  if(token->nextch == ']') {
		        if (optop >= opstk_size) expand_opstk;
			token = GetToken(filep,instr,prevchar);
			/* print_token(token->type,token->value); */
			prevchar = token->nextch;
			opstk[optop].typ = TK_ATOM;
			opstk[optop].op = makenil;
			optop++;
			postopreq = TRUE;
		  } else {	/* beginning of a list */
		        if (funtop >= funstk_size) expand_funstk;
			funstk[funtop].funop = optop;
			funstk[funtop].funtyp = FUNLIST; /* assume regular list */
			funtop++;
		  }
		  break;
		}
	  /* let a punctuation mark be a functor symbol */
      case TK_FUNC:
	        if (funtop >= funstk_size) expand_funstk;
		funstk[funtop].fun = (char *)string_find(token->value,1);
		funstk[funtop].funop = optop;
		funstk[funtop].funtyp = FUNFUN;	/* functor */
		funtop++;

		if (token->nextch != '(')
			return read_can_error(filep,instr,prevchar,prologvar);
		token = GetToken(filep,instr,prevchar);
		/* print_token(token->type,token->value); */
		prevchar = token->nextch;
		break;
      case TK_VVAR:
	        if ((token->value)[1] == 0) { /* anonymous var */
		  if (cvarbot < 0)
		    xsb_abort("[READ_CANONICAL] too many variables in term");
		  i = cvarbot;
		  vars[cvarbot].varid = (Cell) "_";
		  vars[cvarbot].varval = 0;
		  cvarbot--;
		  if (optop >= opstk_size) expand_opstk;
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
		    xsb_abort("[READ_CANONICAL] too many variables in term");
		  vars[cvarbot].varid = (Cell) cvar;
		  vars[cvarbot].varval = 0;
		  cvarbot--;
		}
		if (optop >= opstk_size) expand_opstk;
		opstk[optop].typ = TK_VAR;
		opstk[optop].op = (prolog_term) i;
		optop++;
		postopreq = TRUE;
		break;
      case TK_REAL:
	        if (optop >= opstk_size) expand_opstk;
		opstk[optop].typ = TK_REAL;
		float_temp = (float) *(double *)(token->value);
		opstk[optop].op = makefloat(float_temp);
		optop++;
		postopreq = TRUE;
		break;
      case TK_INT:
	        if (optop >= opstk_size) expand_opstk;
		opstk[optop].typ = TK_INT;
		opstk[optop].op = makeint(*(long *)token->value);
		optop++;
		postopreq = TRUE;
		break;
      case TK_ATOM:
	        if (optop >= opstk_size) expand_opstk;
		opstk[optop].typ = TK_ATOM;
		opstk[optop].op = makestring((char *)string_find(token->value,1));
		optop++;
		postopreq = TRUE;
		break;
      case TK_LIST:  /* "-list */
	if (optop >= opstk_size) expand_opstk;
	if ((token->value)[0] == 0) {
	  opstk[optop].typ = TK_ATOM;
	  opstk[optop].op = makenil;
	  optop++;
	  postopreq = TRUE;
	  break;
	} else {
	  CPtr this_term, prev_tail;
	  char *charptr = token->value;
	  ensure_term_space(h,2);
	  this_term = h;
	  cell(h) = makeint((int)*charptr); charptr++;
	  h++;
	  prev_tail = h;
	  h++;
	  size += 2;
	  while (*charptr != 0) {
	    ensure_term_space(h,2);
	    cell(prev_tail) = makelist(h);
	    cell(h) = makeint((int)*charptr); charptr++;
	    h++;
	    prev_tail = h;
	    h++;
	    size += 2;
	  }
	  cell(prev_tail) = makenil;
	  opstk[optop].op = makelist(this_term);
	  opstk[optop].typ = TK_FUNC;
	  optop++;
	  postopreq = TRUE;
	  break;
	}
      case TK_EOF:
	free_term_buffer();
	if (isnonvar(prologvar)) 
	  xsb_abort("[READ_CANONICAL] Argument must be a variable");
	unify(prologvar,makestring(string_find("end_of_file",1)));
	return 0;
      default: return read_can_error(filep,instr,prevchar,prologvar);
      }
    }
    if (funtop == 0) {  /* term is finished */
      token = GetToken(filep,instr,prevchar);
      /* print_token(token->type,token->value); */
      prevchar = token->nextch; /* accept EOF as end_of_clause */
      if (token->type != TK_EOF && token->type != TK_EOC) 
	return read_can_error(filep,instr,prevchar,prologvar);

      if (opstk[0].typ != TK_VAR) {  /* if a variable, then a noop */
	term = opstk[0].op;
	check_glstack_overflow(3, pcreg, (size+1)*sizeof(Cell)) ;
	if (isnonvar(prologvar)) 
	  xsb_abort("[READ_CANONICAL] Argument must be a variable");
	bind_ref((CPtr)prologvar,hreg);  /* build a new var to trail binding */
	new_heap_free(hreg);
	gl_bot = (CPtr)glstack.low; gl_top = (CPtr)glstack.high; /*??*/
	findall_copy_to_heap(term,(CPtr)prologvar,&hreg) ; /* this can't fail */
	free_term_buffer();

	XSB_Deref(prologvar);
	term = (prolog_term) prologvar;
	if ((isinteger(term)|isboxedinteger(term)) || 
	    isfloat(term) || 
	    isstring(term) ||
	    varfound || 
	    (isconstr(term) && !strcmp(":-",get_name(get_str_psc(term))))) {
	  retpscptr = 0;
	  prevpsc = 0;
	}
	else if (get_str_psc(term) == prevpsc) {
	  retpscptr = (Integer)prevpsc;
	} else {
	  prevpsc = get_str_psc(term);
	  retpscptr = 0;
	}
      } else {
	retpscptr = 0;
	prevpsc = 0;
      }

      if (opstk_size > MAX_INIT_STK_SIZE) {
	free(opstk); opstk = NULL;
	free(funstk); funstk = NULL;
	opstk_size = 0; funstk_size = 0;
      }
      return retpscptr;
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
  static XSB_StrDefine(workspace);      /* copy of format used as workspace */
  static char saved_char;    	     /* place to save char that we
				        temporarily replace with '\0' */
  int pos, keep_going;
  char *ptr;
  static struct fmt_spec result;
  char *exclude, *expect; /* characters to exclude or expect */

  if (initialize) {
    current_substr_start = 0;
    XSB_StrSet(&workspace,format);
  } else {
    /* restore char that was replaced with \0 */
    workspace.string[current_substr_start] = saved_char;
  }

  pos = current_substr_start;
  result.type = '?';
  result.size = 1;

  /* done scanning format string */
  if (current_substr_start >= workspace.length) {
    result.type = '.'; /* last substring (and has no conversion spec) */
    result.fmt  = "";
    return(&result);
  }

  /* find format specification: % not followed by % */
  do {
    /* last substring (and has no conversion spec) */
    if ((ptr=strchr(workspace.string+pos, '%')) == NULL) {
      current_substr_start = workspace.length;
      result.type = '.';  /* last substring with no type specifier */
      result.fmt  = workspace.string+pos;
      return(&result);
    }

    pos = (ptr - workspace.string) + 1;
    if (workspace.string[pos] == '%')
      pos++;
    else break;
  } while (1);

  /* this doesn't do full parsing; it assumes anything that starts at % and
     ends at a valid conversion character is a conversion specifier. */ 
  keep_going = TRUE;
  expect = exclude = "";
  while ((pos < workspace.length) && keep_going) {
    if (strchr(exclude, workspace.string[pos]) != NULL) {
      xsb_abort("[FMT_READ/WRITE] Illegal format specifier `%c' in: %s",
		workspace.string[pos],
		workspace.string+current_substr_start);
    }
    if (strlen(expect) && strchr(expect, workspace.string[pos]) == NULL) {
      xsb_abort("[FMT_READ/WRITE] Illegal format specifier `%c' in: %s",
		workspace.string[pos],
		workspace.string+current_substr_start);
    }

    expect = exclude = "";

    switch (workspace.string[pos++]) {
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
      workspace.string[pos-1] = 's';
      break;
    case 'p':
      xsb_abort("[FMT_READ/WRITE] Format specifier %%p not supported: %s",
		workspace.string+current_substr_start);
    case 'n':
      if (read_op) {
	result.type = 'n'; /* %n is like integer, but in fmt_read we treat it
			      specially */
	keep_going = FALSE;
	break;
      }
      xsb_abort("[FMT_WRITE] Format specifier %%n not supported: %s",
		workspace.string+current_substr_start);
    case '[':
      /* scanf feature: [...] */
      if (!read_op) {
	xsb_abort("[FMT_WRITE] Format specifier [ is invalid for output: %s",
		  workspace.string+current_substr_start);
      }
      while ((pos < workspace.length) && (workspace.string[pos++] != ']'));
      if (workspace.string[pos-1] != ']') {
	xsb_abort("[FMT_READ] Format specifier [ has no matching ] in: %s",
		  workspace.string+current_substr_start);
      }
      result.type = 's';
      keep_going = FALSE;
      break;

    case '*':
      if (read_op) {
	result.size = 0;
	break;
      }
      if (strncmp(workspace.string+pos, ".*", 2) == 0) {
	pos = pos+2;
	expect = "feEgEscdiuoxX";
	result.size = 3;
      } else if (workspace.string[pos] == '.') {
	pos++;
	expect = "0123456789";
	result.size = 2;
      } else {
	result.size = 2;
	expect = "feEgEscdiuoxX";
      }
      break;

    case '!':
      printf("set !\n");
      result.type = '!';
      keep_going = FALSE;
      break;

    default:
      xsb_abort("[FMT_READ/WRITE] Character `%c' in illegal format context: %s",
		workspace.string[pos-1],
		workspace.string+current_substr_start);
    }
  }

  saved_char = workspace.string[pos];
  workspace.string[pos] = '\0';
  result.fmt = workspace.string+current_substr_start;
  current_substr_start = pos;
  return(&result);
}


/* Take a FILE pointer and return an XSB stream, an index into the XSB table of
   open files; return -1, if too many open files. */
int xsb_intern_file(FILE *fptr, char *context)
{
  int i;
  if (!fptr) return -1;

  for (i=MIN_USR_OPEN_FILE; i < MAX_OPEN_FILES && open_files[i] != NULL; i++);
  if (i == MAX_OPEN_FILES) {
    xsb_warn("[%s] Too many open files", context);
    return -1;
  } else 
    open_files[i] = fptr;
  return i;
}


/*----------------------- write_quotedname/2 ---------------------------*/

xsbBool no_quotes_needed(char *string)
{
  int nextchar;
  int ctr, flag;

  if (!strcmp(string,"[]")) return FALSE;
  ctr = 0;
  nextchar = (int) string[0];
  flag = 0;
  if (nextchar >= 97 && nextchar <= 122) {    /* 0'a=97, 0'z=122  */
    while (nextchar != '\0' && !flag) {
      if (nextchar < 48 
	  || (nextchar > 57 && nextchar < 65)
	  || ((nextchar > 90 && nextchar < 97) && nextchar != 95)
	  || (nextchar > 122))
	flag = 1;
      ctr++;
      nextchar = (int) string[ctr];
    }
    if (!flag) return FALSE;
  }

  if (string[1] == '\0') {
    if ((int) string[0] == 33 /*--- || (int) string[0] == 59 ---*/)
      return FALSE;
    if ((int) string[0] == 46) return TRUE;
  }

  nextchar = (int) string[0];
  ctr = 0; 
  while (nextchar != '\0' && !flag) {
    switch(nextchar) {
    case 35: case 36: case 38: case 42: case 43: case 45: case 46:
    case 47: case 58: case 60: case 61: case 62: case 63: case 64: 
    case 92: case 94: case 96: case 126:
      nextchar++;
      break;
    default: 
      flag = 1;
    }
    ctr++;
    nextchar = (int) string[ctr];
  }
  return flag;
}


void double_quotes(char *string, char *new_string)
{
  int ctr = 0, nctr = 0;

  while (string[ctr] != '\0') {
    if (string[ctr] == 39) {
      new_string[nctr] = 39;
      nctr++;
    }
    new_string[nctr] = string[ctr];
    nctr++; ctr++;
  }
  new_string[nctr] = '\0';
}

void write_quotedname(FILE *file, char *string)
{
  char* new_string;

  if (*string == '\0') 
    fprintf(file,"''");
  else {
    if (!no_quotes_needed(string)) {
      fprintf(file,"%s",string);
    }
    else {
      new_string  = (char *)malloc(2*(strlen(string))+1);
      double_quotes(string,new_string);
      fprintf(file,"\'%s\'",new_string);
      free(new_string);
    }
  }
}


static char *wcan_string = NULL;
static int wcan_string_len = 0;
static int wcan_disp = 0;
static char wcan_buff[32];
static Psc dollar_var_psc = NULL;
static int letter_flag = 1;

void expand_wcan_string(int need)
{
  wcan_string_len = wcan_string_len + need;
  wcan_string = realloc(wcan_string,wcan_string_len);
  /*  printf("expanding buffer to %d\n",wcan_string_len); */
}

void wcan_append_string(char *string)
{
  int wcan_ndisp, string_len;

  string_len = strlen(string);
  wcan_ndisp = wcan_disp+string_len;
  if (wcan_ndisp > wcan_string_len) expand_wcan_string(wcan_ndisp);
  strncpy(wcan_string+wcan_disp,string,string_len);
  wcan_disp = wcan_ndisp;
}

void wcan_append_string_chk(char *string)
{
  if (no_quotes_needed(string)) {
    int string_len = strlen(string);
    int ctr = 0;
    int maxlen = wcan_disp+2*(string_len+1);
    if (maxlen > wcan_string_len) 
      expand_wcan_string(maxlen);
    wcan_string[wcan_disp++] = 39;
    while (string[ctr] != '\0') {
      if (string[ctr] == 39) wcan_string[wcan_disp++] = 39;
      wcan_string[wcan_disp++] = string[ctr++];
    }
    wcan_string[wcan_disp++] = 39;
  } else {
    wcan_append_string(string);
  }
}

void write_canonical_term(Cell prologterm)
{
  XSB_Deref(prologterm);
  switch (cell_tag(prologterm)) 
    {
    case XSB_INT:
      sprintf(wcan_buff,"%ld",(long)int_val(prologterm));
      wcan_append_string(wcan_buff);
      break;
    case XSB_STRING: 
      wcan_append_string_chk(string_val(prologterm));
    break;
    case XSB_FLOAT:
      sprintf(wcan_buff,"%2.4f",float_val(prologterm));
      wcan_append_string(wcan_buff);
      break;
    case XSB_REF:
    case XSB_REF1:
      {int varval;
      if (wcan_disp+2 > wcan_string_len) expand_wcan_string(wcan_disp+2);
      wcan_string[wcan_disp++] = '_';
      if (prologterm >= (Cell)glstack.low && prologterm <= (Cell)top_of_heap) {
	wcan_string[wcan_disp++] = 'h';
	varval = (long) ((prologterm-(Cell)glstack.low+1)/sizeof(CPtr));
      }
      else {
	if (prologterm >= (Cell)top_of_localstk && prologterm <= (Cell)glstack.high) {
	  wcan_string[wcan_disp++] = 'l';
	  varval = (long) (((Cell)glstack.high-prologterm+1)/sizeof(CPtr));
	}
	else varval = prologterm;   /* Should never happen */
      }
      sprintf(wcan_buff,"%d",varval);
      wcan_append_string(wcan_buff);
      }
      break;
    case XSB_STRUCT: /* lettervar: i.e., print '$VAR'(i) terms as Cap Alpha-Num */
      if (!dollar_var_psc) {
	int new_indicator;
	dollar_var_psc = pair_psc(insert("$VAR", 1, global_mod, &new_indicator));
      }
      if (letter_flag && (get_str_psc(prologterm) == dollar_var_psc)) {
	int ival, letter;
	Cell tempi = cell(clref_val(prologterm)+1);
	XSB_Deref(tempi);
	if (!isinteger(tempi)) xsb_abort("[write_canonical]: illegal $VAR argument");
	ival = int_val(tempi);
	letter = ival % 26;
	ival = ival / 26;
	if (wcan_disp >= wcan_string_len) expand_wcan_string(wcan_disp+1);
	wcan_string[wcan_disp++] = letter + 'A';
	if (ival != 0) {
	  sprintf(wcan_buff,"%d",ival);
	  wcan_append_string(wcan_buff);
	}
      } else {
	int i;
	wcan_append_string_chk(get_name(get_str_psc(prologterm)));
	if (wcan_disp >= wcan_string_len) expand_wcan_string(wcan_disp+1);
	wcan_string[wcan_disp++] = '(';
	for (i = 1; i < get_arity(get_str_psc(prologterm)); i++) {
	  write_canonical_term(cell(clref_val(prologterm)+i));
	  if (wcan_disp >= wcan_string_len) expand_wcan_string(wcan_disp+1);
	  wcan_string[wcan_disp++] = ',';
	}
	write_canonical_term(cell(clref_val(prologterm)+i));
	if (wcan_disp >= wcan_string_len) expand_wcan_string(wcan_disp+1);
	wcan_string[wcan_disp++] = ')';
      }
      break;
    case XSB_LIST:
      {Cell tail;
      if (wcan_disp >= wcan_string_len) expand_wcan_string(wcan_disp+1);
      wcan_string[wcan_disp++] = '[';
      write_canonical_term(cell(clref_val(prologterm)));
      tail = cell(clref_val(prologterm)+1);
      XSB_Deref(tail);
      while (islist(tail)) {
	if (wcan_disp >= wcan_string_len) expand_wcan_string(wcan_disp+1);
	wcan_string[wcan_disp++] = ',';
	write_canonical_term(cell(clref_val(tail)));
	tail = cell(clref_val(tail)+1);
	XSB_Deref(tail);
      } 
      if (!isnil(tail)) {
	if (wcan_disp >= wcan_string_len) expand_wcan_string(wcan_disp+1);
	wcan_string[wcan_disp++] = '|';
	write_canonical_term(tail);
      }
      if (wcan_disp >= wcan_string_len) expand_wcan_string(wcan_disp+1);
      wcan_string[wcan_disp++] = ']';
      }
      break;
    default:
      xsb_abort("Unsupported subterm tag");
      return;
    }
  return;
}

void print_term_canonical(FILE *fptr, Cell prologterm, int letterflag)
{

  letter_flag = letterflag;
  wcan_disp = 0;
  write_canonical_term(prologterm);
  if (wcan_disp >= wcan_string_len) expand_wcan_string(wcan_disp+1);
  wcan_string[wcan_disp] = '\0';

  fprintf(fptr, "%s", wcan_string);
}

