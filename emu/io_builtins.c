/* File:      io_builtins.c
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
#include <setjmp.h>
#include <stdlib.h>
#include <unistd.h> 
#include <sys/stat.h>

#include "auxlry.h"
#include "cell.h"
#include "xsberror.h"
#include "cinterf.h"
#include "memory.h"
#include "psc.h"
#include "heap.h"
#include "register.h"
#include "flags.h"
#include "inst.h"
#include "token.h"
#include "loader.h"
#include "load_seg.h"
#include "tries.h"
#include "choice.h"
#include "xmacro.h"
#include "io_builtins.h"
#include "configs/special.h"

extern int      fileno(FILE *);	        /* this is defined in POSIX */

static FILE *fptr;			/* working variable */
    
#define setvar(op1) \
    if (vars[opstk[op1].op].varval) \
       cell(sreg) = vars[opstk[op1].op].varval; \
    else { \
	     cell(sreg) = (Cell) sreg; \
	     vars[opstk[op1].op].varval = (Cell) sreg; \
	 }

struct fmt_spec {
  char type; 	     	     	 /* i(nteger), f(loat), s(tring) */
  char size;	    	    	 /* in case f the specifiers *, this number is
				    1 or 2, depending the number of *'s */
  char *fmt;
};

struct fmt_spec *next_format_substr(char *, int, int);
char *p_charlist_to_c_string(prolog_term, char *, char *);

/* type is a char: 's', 'i', 'f' */
#define TYPE_ERROR_CHK(ch_type, Label) \
        if (current_fmt_spec->type != ch_type) { \
	    xsb_abort("%s: Type mismatch in argument %d", Label, i); \
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
   snprintf. */
#define SPRINT_ARG(arg) switch (current_fmt_spec->size) { \
        case 1: bytes_formatted=sprintf(ptr_OutString, \
     	     	     	     	     	current_fmt_spec->fmt, arg); \
	        break; \
	case 2: bytes_formatted=sprintf(ptr_OutString, \
					current_fmt_spec->fmt, width, arg); \
	        break; \
	case 3: bytes_formatted=sprintf(ptr_OutString, \
					current_fmt_spec->fmt, \
					width, precision, arg); \
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
     C invocation: formatted_io(FMT_WRITE, FileDes, Format, ValTerm)
     Prolog invocation: fmt_write(+FileDes, +Format, +ValTerm)
       FileDes: file descriptor
       Format: format as atom or string;
       ValTerm: term whose args are vars to receive values returned.
----------------------------------------------------------------------*/


bool fmt_write(void)
{
  char *Fmt=NULL;
  static char aux_msg[50];
  prolog_term ValTerm, Arg, Fmt_term;
  int i, Arity;
  char *str_arg;     	     	     	      /* holder for string arguments */
  long int_arg;     	     	     	      /* holder for int args         */
  float float_arg;    	     	     	      /* holder for float args       */
  struct fmt_spec *current_fmt_spec;
  int width=0, precision=0;    	     	      /* these are used in conjunction
						 with the *.* format         */
  fptr = fileptr((int) ptoc_int(2));
  Fmt_term = reg_term(3);
  if (is_list(Fmt_term))
    Fmt = p_charlist_to_c_string(Fmt_term, "FMT_WRITE", "format string");
  else if (is_string(Fmt_term))
    Fmt = string_val(Fmt_term);
  else
    xsb_abort("FMT_WRITE: Format must be an atom or a character string");

  ValTerm = reg_term(4);
  if (!is_functor(ValTerm))
    xsb_abort("Usage: fmt_write([File,] FormatStr, args(Arg1,...,Arg_n))");
  Arity = get_arity(get_str_psc(ValTerm));

  current_fmt_spec = next_format_substr(Fmt,
					1,   /* initialize    	      	     */
					0);  /* write    	      	     */
  xsb_segfault_message =
    "FMT_WRITE: Argument type doesn't match format specifier";
  signal(SIGSEGV, &xsb_segfault_catcher);
  
  for (i = 1; (i <= Arity); i++) {
    /* last format substring (and has no conversion spec) */
    if (current_fmt_spec->type == '.') {
      PRINT_ARG("");
      xsb_warn("FMT_WRITE: More arguments than format specifiers");
      goto EXIT_WRITE;
    }

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

    if (is_string(Arg)) {
      TYPE_ERROR_CHK('s', "FMT_WRITE");
      str_arg = string_val(Arg);
      PRINT_ARG(str_arg);
    } else if (is_list(Arg)) {
      TYPE_ERROR_CHK('s', "FMT_WRITE");
      sprintf(aux_msg, "argument %d", i);
      str_arg = p_charlist_to_c_string(Arg, "FMT_WRITE", aux_msg);
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

#define MAX_SPRINTF_STRING_SIZE MAXBUFSIZE*4

/* If no snprintf, we fill only half of OutString, to be on the safe side */
#ifdef HAVE_SNPRINTF
#define SAFE_OUT_SIZE MAX_SPRINTF_STRING_SIZE
#else
#define SAFE_OUT_SIZE MAX_SPRINTF_STRING_SIZE/2
#endif

bool fmt_write_string(void)
{
  char *Fmt=NULL;
  static char OutString[MAX_SPRINTF_STRING_SIZE+1];
  static char aux_msg[50];
  char *ptr_OutString = OutString;
  prolog_term ValTerm, Arg, Fmt_term;
  int i, Arity;
  char *str_arg;     	     	     	    /* holder for string arguments  */
  long int_arg;     	     	     	    /* holder for int args     	    */
  float float_arg;     	     	     	    /* holder for float args   	    */
  struct fmt_spec *current_fmt_spec;
  int width=0, precision=0;      	    /* these are used in conjunction
					       with the *.* format     	    */
  int bytes_formatted=0;       	       	    /* the number of bytes formatted as
					       returned by sprintf/snprintf */
  int safe_outstring_bytes = SAFE_OUT_SIZE; /* safe number of bytes to write
					       to OutString 	    	     */
  OutString[0] = '\0'; 	       	            /* anull the output string 	     */
  Fmt_term = reg_term(3);
  if (is_list(Fmt_term))
    Fmt = p_charlist_to_c_string(Fmt_term,
				 "FMT_WRITE_STRING", "format string");
  else if (is_string(Fmt_term))
    Fmt = string_val(Fmt_term);
  else
    xsb_abort("FMT_WRITE_STRING: Format must be an atom or a character string");

  ValTerm = reg_term(4);
  if (!is_functor(ValTerm))
    xsb_abort("Usage: fmt_write_string(OutBuf, FormatStr, args(Arg1,...,Arg_n))");
  Arity = get_arity(get_str_psc(ValTerm));

  current_fmt_spec = next_format_substr(Fmt,
					1,  /* initialize     	      	     */
					0); /* write     	      	     */
  xsb_segfault_message =
    "FMT_WRITE_STRING: Argument type doesn't match format specifier";
  signal(SIGSEGV, &xsb_segfault_catcher);
  
  for (i = 1; (i <= Arity); i++) {
    /* last string (and has no conversion spec) */
    if (current_fmt_spec->type == '.') {
      SPRINT_ARG("");
      xsb_warn("FMT_WRITE_STRING: More arguments than format specifiers");
      CHECK_OUTPUT_SIZE; /* might xsb_abort */
      goto EXIT_WRITE_STRING;
    }


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

    if (is_string(Arg)) {
      TYPE_ERROR_CHK('s', "FMT_WRITE_STRING");
      str_arg = string_val(Arg);
      SPRINT_ARG(str_arg);
    } else if (is_list(Arg)) {
      TYPE_ERROR_CHK('s', "FMT_WRITE_STRING");
      sprintf(aux_msg, "argument %d", i);
      str_arg = p_charlist_to_c_string(Arg, "FMT_WRITE_STRING", aux_msg);
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
     C invocation: formatted_io(FMT_READ, File, Format, ArgTerm, Status)
     Prolog invocation: fmt_read(+File, +Format, -ArgTerm, -Status)
      File: file descriptor
      Format: format as atom or string;
      ArgTerm: Term whose args are vars to receive values returned.
      Status: 0 OK, -1 eof 
----------------------------------------------------------------------*/

bool fmt_read(void)
{
  char *Fmt=NULL;
  prolog_term AnsTerm, Arg, Fmt_term;
  Integer i ;
  char str_arg[MAXBUFSIZE],    	       	      /* holder for string arguments */
    aux_fmt[MAXBUFSIZE];    	    	      /* auxiliary fmt holder 	     */
  long int_arg;     	     	     	      /* holder for int args         */
  float float_arg;    	     	     	      /* holder for float args       */
  struct fmt_spec *current_fmt_spec;
  int Arity;
  int number_of_successes=0, curr_assignment=0;
  int cont; /* continuation indicator */
  int chars_accumulator=0, curr_chars_consumed=0;
  
  fptr = fileptr((int) ptoc_int(2));
  Fmt_term = reg_term(3);
  if (is_list(Fmt_term))
    Fmt = p_charlist_to_c_string(Fmt_term, "FMT_READ", "format string");
  else if (is_string(Fmt_term))
    Fmt = string_val(Fmt_term);
  else
    xsb_abort("FMT_READ: Format must be an atom or a character string");

  AnsTerm = reg_term(4);
  if (!is_functor(AnsTerm))
    xsb_abort("Usage: fmt_read([File,] FormatStr, args(Arg1,...,Arg_n), RetCode");
  Arity = get_arity(get_str_psc(AnsTerm));

  current_fmt_spec = next_format_substr(Fmt,
					1,   /* initialize    	      	     */
					1);  /* read    	      	     */
  strncpy(aux_fmt, current_fmt_spec->fmt, MAXBUFSIZE-4);
  strcat(aux_fmt,"%n");

  for (i = 1; (i <= Arity); i++) {
    Arg = p2p_arg(AnsTerm,i);
    cont = 0;
    curr_chars_consumed=0;

    switch (current_fmt_spec->type) {
    case '.': /* last format substring (and has no conversion spec) */
      curr_assignment = fscanf(fptr, current_fmt_spec->fmt);
      if (is_var(Arg))
	xsb_warn("FMT_READ: More arguments than format specifiers");
      goto EXIT_READ;
    case 's':
      curr_assignment = fscanf(fptr, aux_fmt,
			       str_arg, &curr_chars_consumed);
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
      if (is_var(Arg))
	c2p_int(int_arg,Arg);
      else if (int_arg != int_val(Arg)) return FALSE;
      break;
    case 'f':
      curr_assignment = fscanf(fptr, aux_fmt,
			       &float_arg, &curr_chars_consumed);
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

  /* last format substr without conversion spec */
  if (current_fmt_spec->type == '.')
    curr_assignment = fscanf(fptr, current_fmt_spec->fmt);

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

static int read_can_error(FILE *filep, STRFILE *instr, int prevchar)
{
  char *ptr;

  xsb_mesg("READ_CAN_ERROR: illegal format. Next tokens:");
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
  if (token->type == TK_EOC) fprintf(stderr,".\n");
  else fprintf(stderr,"\n");
  ctop_string(2,(char *)string_find("read_canonical_error",1));
  ctop_int(3,0);
  return TRUE;
}


static int getvarnum(char *varname)
{
  int i;
  int val = 0;
  char tchar;
  
  if (varname[1] == 0) return 0;
  if ((varname[1] < '1') || (varname[1] > '9')) return -1;
  i = 1;
  while (1) {
    tchar = varname[i++];
    if (tchar == 0) break;
    if ((tchar < '0') || (tchar > '9')) return -1;
    val = 10*val+(tchar-'0');
  }
  return val;
}


/* read a canonical term from file desc in r1 and put answer in variable 
in r2, r3 set to 0 if ground fact (non zero-ary), 1 if variable or :-.
Fail on EOF */

int read_canonical(void)
{
  FILE *filep;
  STRFILE *instr;
  int prevchar, arity, i;
  Cell op1;
  int tvar;
  struct funstktype {
    char *fun;
    Integer funop;
  } funstk[200];
  Cell funtop = 0;

  struct opstktype {
    int typ;
    prolog_term op;
  } opstk[200];
  Cell optop = 0;

#define MAXVAR 400
  struct vartype {
    Cell varid;
    prolog_term varval;
  } vars[MAXVAR];
  int nvartop = 0;
  int cvarbot = MAXVAR-1;

  Pair sym;
  Float float_temp;
  char *cvar;
  int postopreq = 0, varfound = 0;
  long tempfp;
  prolog_term term;
  void ctop_tag(int, Cell);
  
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
    filep = fileptr(tempfp);
  }

  prevchar = 10;

  while (1) {
    token = GetToken(filep,instr,prevchar);
    /* print_token(token->type,token->value); */
    prevchar = token->nextch;
    if (postopreq) {  /* must be an operand follower: , or ) */
      if (token->type == TK_PUNC) {
	if (*token->value == ')') {
	  funtop--;
	  arity = optop - funstk[funtop].funop;
	  sreg = hreg;
	  op1 = funstk[funtop].funop;
	  if ((arity == 2) && !(strcmp(funstk[funtop].fun,"."))) {
	    if (opstk[op1].typ == TK_VAR) { setvar(op1) }
	    else cell(sreg) = opstk[op1].op;
	    sreg++;
	    if (opstk[op1+1].typ == TK_VAR) { setvar(op1+1) }
	    else cell(sreg) = opstk[op1+1].op;
	    sreg++;
	    opstk[op1].op = makelist(hreg);
	    opstk[op1].typ = TK_FUNC;
	  } else {
	    sym = (Pair)insert(funstk[funtop].fun,arity,
			       (Psc)flags[CURRENT_MODULE],&i);
	    new_heap_functor(sreg, sym->psc_ptr);
	    for (i=op1; i<optop; sreg++,i++) {
	      if (opstk[i].typ == TK_VAR) { setvar(i) }
	      else cell(sreg) = opstk[i].op;
	    }
	    opstk[op1].op = makecs(hreg);
	    opstk[op1].typ = TK_FUNC;
	  }
	  optop = op1;
	  optop++;
	  hreg += arity + 1;
	} else if (*token->value == ',')  postopreq = 0;
	else return read_can_error(filep,instr,prevchar);
      } else {  /* check for neg numbers and backpatch if so */
	if (opstk[optop-1].typ == TK_ATOM && 
	    !strcmp("-",string_val(opstk[optop-1].op))) {
	  if (token->type == TK_INT) {
	    opstk[optop-1].typ = TK_INT;
	    opstk[optop-1].op = makeint(-(*(int *)token->value));
	  }
	  else if (token->type == TK_REAL) {
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
	  token = GetToken(filep,instr,prevchar);
	  /* print_token(token->type,token->value); */
	  prevchar = token->nextch;
	  if (*token->value == ']') {
	    opstk[optop].typ = TK_ATOM;
	    opstk[optop++].op = makenil;
	    postopreq = 1;
	    break;
	  }
	  else return read_can_error(filep,instr,prevchar);
	}
	/* let a punctuation mark be a functor symbol */
      case TK_FUNC:
	funstk[funtop].fun = (char *)string_find(token->value,1);
	funstk[funtop].funop = optop;
	funtop++;

	token = GetToken(filep,instr,prevchar);
	/* print_token(token->type,token->value); */
	prevchar = token->nextch;
	if ((token->type != TK_PUNC) || (*token->value != '(')) 
	  return read_can_error(filep,instr,prevchar);
	break;
      case TK_VVAR:
	varfound = 1;
	tvar = getvarnum(token->value);
	if (tvar >= 0) {
	  if (tvar == 0) i = nvartop;
	  else {
	    i = 0;
	    while (i<nvartop) {
	      if (tvar == vars[i].varid) break;
	      i++;
	    }
	  }
	  if (i == nvartop) {
	    vars[nvartop].varid = tvar;
	    vars[nvartop].varval = 0;
	    nvartop++;
	  }
	  opstk[optop].typ = TK_VAR;
	  opstk[optop++].op = (prolog_term) i;
	  postopreq = 1;
	  break;
	}
      case TK_VAR:
	varfound = 1;
	cvar = (char *)string_find(token->value,1);
	i = MAXVAR-1;
	while (i>cvarbot) {
	  if (cvar == (char *)vars[i].varid) break;
	  i--;
	}
	if (i == cvarbot) {
	  vars[cvarbot].varid = (Cell) cvar;
	  vars[cvarbot].varval = 0;
	  cvarbot--;
	}
	opstk[optop].typ = TK_VAR;
	opstk[optop++].op = (prolog_term) i;
	postopreq = 1;
	break;
      case TK_REAL:
	opstk[optop].typ = TK_REAL;
	float_temp = (float) *(double *)(token->value);
	opstk[optop++].op = makefloat(float_temp);
	postopreq = 1;
	break;
      case TK_INT:
	opstk[optop].typ = TK_INT;
	opstk[optop++].op = makeint(*(long *)token->value);
	postopreq = 1;
	break;
      case TK_ATOM:
	opstk[optop].typ = TK_ATOM;
	opstk[optop++].op = makestring((char *)string_find(token->value,1));
	postopreq = 1;
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
      term = opstk[0].op;
      ctop_tag(2,term);
      if (varfound || 
	  (isconstr(term) && !strcmp(":-",get_name(get_str_psc(term)))) ||
	  isstring(term)) {
	ctop_int(3,0);
	prevpsc = 0;
      }
      else if (get_str_psc(term) == prevpsc) {
	ctop_int(3, (Integer)prevpsc);
      }
      else {
	prevpsc = get_str_psc(term);
	ctop_int(3,0);
      }
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
  keep_going = 1;
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
    case 'd':
    case 'i':
    case 'u':
    case 'o':
    case 'x':
    case 'X':
      keep_going = 0;
      result.type = 'i'; /* integer or character */
      break;
    case 'e':
    case 'E':
    case 'f':
    case 'g':
    case 'G':
      keep_going = 0;
      result.type = 'f'; /* float */
      break;
    case 's':
      keep_going = 0;
      result.type = 's'; /* float */
      break;
    case 'p':
      xsb_abort("Format specifier %%p not supported: %s",
		workspace+current_substr_start);
    case 'n':
      if (read_op) {
	result.type = 'n'; /* %n is like integer, but in fmt_read we treat it
			      specially */
	keep_going = 0;
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
      keep_going = 0;
      break;

    case '*':
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


/* convert prolog list of characters (a.k.a. prolog string) into C string.
   Arg 2: which function was called from.
   Arg 3: where in the call this happened.
   Args 2 and 3 are used for error reporting.
   This function converts escape sequences in the Prolog string
   (except octal/hexadecimal) into the corresponding real characters.
*/
char *p_charlist_to_c_string (prolog_term term, char *in_func, char *where)
{
  char str[MAXBUFSIZE+1];
  int i = 0, head_val;
  int escape_mode=FALSE;
  prolog_term list = term, list_head;

  if (!is_list(list)) {
    xsb_abort("%s: %s is not a list of characters");
  }

  while (is_list(list) && i < MAXBUFSIZE) {
    if (is_nil(list)) break;
    list_head = p2p_car(list);
    if (!is_int(list_head)) {
      xsb_abort("%s: Non-ASCII character in %s", in_func, where);
    }
    head_val = int_val(list_head);
    if (head_val < 0 || head_val > 255) {
      xsb_abort("%s: Non-ASCII character in %s", in_func, where);
    }

    head_val = (char) head_val;
    /* convert ecape sequences */
    if (escape_mode)
      switch (head_val) {
      case 'a':
	str[i] = '\a';
	break;
      case 'b':
	str[i] = '\b';
	break;
      case 'f':
	str[i] = '\f';
	break;
      case 'n':
	str[i] = '\n';
	break;
      case 'r':
	str[i] = '\r';
	break;
      case 't':
	str[i] = '\t';
	break;
      case 'v':
	str[i] = '\v';
	break;
      default:
	str[i] = head_val;
      }
    else
      str[i] = head_val;

    if (str[i] == '\\' && !escape_mode)
      escape_mode = TRUE;
    else {
      i++;
      escape_mode = FALSE;
    }
    list = p2p_cdr(list);
  } /* while */

  str[i] = '\0';
  return(string_find(str,1));
}

