/* File:      biassert.c
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
#include <errno.h>
#include <string.h>
#include <setjmp.h>
#include <stdlib.h>

#ifndef NeXT
#include <malloc.h>
#endif


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
#include "tr_utils.h"
#include "switch.h"
#include "configs/special.h"

static FILE* fptr;			/* working variable */
    
#define setvar(op1) \
    if (vars[opstk[op1].op].varval) \
       cell(sreg) = vars[opstk[op1].op].varval; \
    else { \
	     cell(sreg) = (Cell) sreg; \
	     vars[opstk[op1].op].varval = (Cell) sreg; \
	 }




/*----------------------------------------------------------------------*/
/* like fprintf:
   R1: +File 
   R2: +Format, fprintf format as atom;
   R3: +Vals, term whose args are values */
/*----------------------------------------------------------------------*/


#define MAX_FMT_ARGS 30

/* Note: all these functions should be really defined with vfprintf/vsprintf
** Leave it for the next project --mk */
bool fmt_write(void)
{
  long args[MAX_FMT_ARGS*2];
  char *Fmt;
  prolog_term ValTerm, Arg;
  int i, Arity;
  
  i = ptoc_int(1);
  fptr = fileptr(i);
  Fmt = ptoc_string(2);
  ValTerm = reg_term(3);
  Arity = get_arity(get_str_psc(ValTerm));
  if (Arity >= MAX_FMT_ARGS) {
    fprintf(stderr,"Too many fields passed to FMT_WRITE\n");
    return FALSE;
  }
  
  /* ASSUMES THAT DOUBLE IS TWICE AS LONG AS INT AND CHAR* */
  for (i = 1; (i <= Arity); i++) {
    Arg = p2p_arg(ValTerm,i);
    if (is_string(Arg)) ((char **)args)[i<<1] = string_val(Arg);
    else if (is_int(Arg)) args[i<<1] = int_val(Arg);
    else if (is_float(Arg)) ((double *)args)[i] = float_val(Arg);
    else {
      fprintf(stderr,"Illegal argument to FMT_WRITE\n");
      return FALSE;
    }
  }
  fprintf(fptr,Fmt,args[2],args[4],args[6],args[8],args[10],args[12],
	  args[14],args[16],args[18],args[20],args[22],args[24],args[26],
	  args[28],args[30],args[32],args[34],args[36],args[38],
	  args[40],args[42],args[44], args[46],args[48],args[50],
	  args[52],args[54],args[56], args[58]
	  );
  
  return TRUE;
}



/*----------------------------------------------------------------------*/
/* like sprintf:
   R1: String
   R2: +Format, sprintf format as atom;
   R3: +Vals, term whose args are values */
/*----------------------------------------------------------------------*/


#define INIT_BUF_SIZE 200

/* There is a danger of a coredump, if snprintf() is not available and
   the user formats large strings. */
#ifdef HAVE_SNPRINTF
#define MAX_SPRINT_ARGS MAX_FMT_ARGS
#else
#define MAX_SPRINT_ARGS 7
#endif

bool fmt_write_string(void)
{
  long args[MAX_FMT_ARGS*2];
  prolog_term ValTerm, Arg;
  int i, Arity;
  char *Fmt, *OutString = NULL;
  int required_buf_size = INIT_BUF_SIZE+1, old_bufsize = INIT_BUF_SIZE;
  
  Fmt = ptoc_string(2);
  ValTerm = reg_term(3);
  Arity = get_arity(get_str_psc(ValTerm));
  if (Arity >= MAX_FMT_ARGS) {
    fprintf(stderr,"Too many fields passed to FMT_WRITE_STRING\n");
    return FALSE;
  }
  
  /* ASSUMES THAT DOUBLE IS TWICE AS LONG AS INT AND CHAR* */
  for (i = 1; (i <= Arity); i++) {
    Arg = p2p_arg(ValTerm,i);
    if (is_string(Arg)) ((char **)args)[i<<1] = string_val(Arg);
    else if (is_int(Arg)) args[i<<1] = int_val(Arg);
    else if (is_float(Arg)) ((double *)args)[i] = float_val(Arg);
    else {
      fprintf(stderr,"Illegal argument to FMT_WRITE_STRING\n");
      return FALSE;
    }
  }

  /* do snprintf until we get the right size of the output string (needs only
     be done twice at most */
#ifdef HAVE_SNPRINTF
  while (required_buf_size > old_bufsize) {
      if (OutString != NULL) free(OutString);
      OutString = (char *)malloc (required_buf_size);
      old_bufsize = required_buf_size;

      required_buf_size =
	snprintf(OutString, old_bufsize, Fmt,
		 args[2],args[4],args[6],args[8],args[10],args[12],
		 args[14],args[16],args[18],args[20],args[22],args[24],
		 args[26], args[28],args[30],args[32],args[34],args[36],
		 args[38], args[39],args[40],args[42], args[44], args[46],
		 args[48], args[50], args[52],args[54], args[56], args[58]
		 );
    }
#else
  /* If snprintf is not available, we must limit the number of arguments to
     reduce the danger of a coredump in sprintf */
  OutString = (char *)malloc (MAXBUFSIZE);
  sprintf(OutString, Fmt,
	  args[2],args[4],args[6],args[8],args[10], args[12], args[14]);
#endif

  /* fmt_write_string is used in places where interning of the string is needed
     (such as constructing library search paths)
     Therefore, must use string_find(..., 1). */
  ctop_string(1, string_find(OutString,1));
  free(OutString);   /* so buffer no longer needed */

  return TRUE;
}


/* 
** Works like fgets(buf, size, stdin). Fails on reaching the end of file 
** Invoke: file_read_line(+File, -Str, -IsFullLine). Returns the string read
** and an indicator (IsFullLine = 1 or 0) of whether the string read is a full
** line. Doesn't intern the string and is always using the same
** spot in the memory. So, each file_read_line overrides the previous one.
** If you want to intern, you must do so explicitly.
*/

bool file_read_line(void)
{
  static char buf[MAXBUFSIZE];
  int filedes=ptoc_int(1);
  FILE *file=fileptr(filedes);

  /* MAXBUFSIZE-1, because fgets addts '\0' at the end */
  if (fgets(buf, MAXBUFSIZE-1, file) == NULL)
    return FALSE;
  else {
    ctop_string(2, buf);
    if (buf[(strlen(buf)-1)] == '\n')
      ctop_int(3, 1); /* full line */
    else ctop_int(3, 0); /* partial line */
    return TRUE;
  }
}


/*----------------------------------------------------------------------*/
/* like fscanf,
   R1: +File
   R2: +Format, fscanf format as atom;
   R3: +Types, Term whose aregs are types to expect;
   R4: -ValsVar, Term whose args are vars to receive values returned.
   R5: -Ret: 0 OK, -1 eof */
/*----------------------------------------------------------------------*/

bool fmt_read(void)
{
  char *args[MAX_FMT_ARGS];
  char space[MAX_FMT_ARGS<<8];
  char *Fmt;
  prolog_term TypeTerm, AnsTerm, Arg;
  Integer i ;
  int nf, CArg, Arity;
  
  i = ptoc_int(1);
  fptr = fileptr(i);
  Fmt = ptoc_string(2);
  TypeTerm = reg_term(3);
  AnsTerm = reg_term(4);
  Arity = is_string(TypeTerm) ? 0 : get_arity(get_str_psc(TypeTerm));
  if (Arity >= MAX_FMT_ARGS) {
    fprintf(stderr,"Too many fields for FMT_READ\n");
    return 0;
  }
  for (i=1; i<=Arity; i++) {
    args[i] = &(space[i<<8]);
    memset(args[i],0,1<<8);  /* initialize to 0, so %5c has terminating null */
  }
  
  nf = fscanf(fptr,Fmt,args[1],args[2],args[3],args[4],args[5],
	      args[6],args[7],args[8],args[9],args[10],args[11],args[12],
	      args[13],args[14],args[15],args[16],args[17],args[18],args[19],
	      args[20],args[21],args[22],args[23],args[24],args[25],args[26],
	      args[27],args[28]
	      );
  
  for (i = 1; (i <= nf); i++) {
    Arg = p2p_arg(AnsTerm,i);
    CArg = int_val(p2p_arg(TypeTerm,i));
    if (CArg == 1) {
      if (is_var(Arg)) c2p_string(*((char **)args + i),Arg);
      else if (strcmp(*((char **)args+i),string_val(Arg))) return 0;
    }
    else if (CArg == 2) {
      if (is_var(Arg)) c2p_int(**((long **)args + i),Arg);
      else if (**((long **)args + i) != int_val(Arg)) return 0;
    }
    else if (CArg == 3) {
      c2p_float(**((float **)args + i),Arg);
    }
    else {
      fprintf(stderr,"Illegal argument to FMT_READ\n");
      return 0;
    }
  }
  ctop_int(5,nf);
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

  fprintf(stderr,"ERROR, illegal format in read_canonical. Next tokens:\n");
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


