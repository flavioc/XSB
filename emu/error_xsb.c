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
#include "context.h"
#include "cell_xsb.h"
#include "psc_xsb.h"
#include "subp.h"
#include "register.h"
#include "error_xsb.h"
#include "io_builtins_xsb.h"
#include "cinterf.h"
#include "memory_xsb.h"
#include "tries.h"
#include "choice.h"
#include "inst_xsb.h"
#include "macro_xsb.h"
#include "tr_utils.h"
#include "binding.h"
#include "cut_xsb.h"
#include "flags_xsb.h"
#include "term_psc_xsb_i.h"
#include "thread_xsb.h"
#include "emuloop.h"

extern void remove_incomplete_tries(CTXTdeclc CPtr);
extern PrRef get_prref(CTXTdeclc Psc psc);

#ifndef MULTI_THREAD
extern jmp_buf xsb_abort_fallback_environment;
#endif

FILE *stdmsg;	     	     	  /* stream for XSB benign messages */
FILE *stddbg;	     	     	  /* stream for XSB debug msgs */
FILE *stdwarn;	     	     	  /* stream for XSB warnings */
FILE *stdfdbk;	     	     	  /* stream for XSB feedback messages */

/*----------------------------------------------------------------------*/

static char *err_msg_table[] = {
	"Calculation", "Database", "Evaluation", "Implementation",
	"Instantiation", "I/O Control", "I/O End-of-file", "I/O Formatting",
	"Operator", "Overflow", "Range", "Syntax", "Type",
	"Undefined predicate/function", "Undefined value",
	"Underflow", "Zero division" };

/*----------------------------------------------------------------------*/

#ifndef HAVE_SNPRINTF
#include <stdarg.h>
int vsnprintf(char *buffer, size_t count, const char *fmt, va_list ap) {
       int ret;

       ret = _vsnprintf(buffer, count-1, fmt, ap);
       if (ret < 0) {
               buffer[count-1] = '\0';
       }

       return ret;
}
#endif

/*----------------------------------------------------------------------*/

DllExport void call_conv xsb_initialization_exit(char *description, ...)
{
  va_list args;

  if (xsb_mode != C_CALLING_XSB) {
    va_start(args, description);
    vfprintf(stderr, description, args);
    va_end(args);

    fprintf(stdfdbk, "\nExiting XSB abnormally...\n");
    exit(1);
  }
  else {
    sprintf(xsb_get_init_error_type(),"init_error");
    va_start(args, description);
    vsnprintf(xsb_get_init_error_message(), ERRTYPELEN, description, args);
    va_end(args);
    longjmp(ccall_init_env, XSB_ERROR);
  }
}

void call_conv xsb_unrecoverable_error(CTXTdeclc char *);

DllExport void call_conv xsb_exit(CTXTdeclc  char *description, ...)
{
  va_list args;
  char message[MAXBUFSIZE];

  if (xsb_mode != C_CALLING_XSB) {
    va_start(args, description);
    vfprintf(stderr, description, args);
    va_end(args);

    fprintf(stdfdbk, "\nExiting XSB abnormally...\n");
    exit(1);
  }
  else {
    va_start(args, description);
    vsnprintf(message,MAXBUFSIZE, description, args);
    va_end(args);
    xsb_unrecoverable_error(CTXTc message);
  }
}

/* This gives an absolute exit -- used only in memory errors, 
   and perhaps shouldn't even be used there.*/
DllExport void call_conv exit_xsb(char *description)
{
  fprintf(stderr, description);

  fprintf(stdfdbk, "\nExiting XSB abnormally...\n");
  exit(1);
}

#if defined(DEBUG_VERBOSE) && defined(CP_DEBUG)
extern void print_cp_backtrace();
#endif

/* TLS: now frees Ball, which was assumed to be malloced.  Use
   mem_alloc_nocheck to avoid problems when thowing a memory error. */
DllExport void call_conv xsb_throw_internal(CTXTdeclc prolog_term Ball, unsigned long Ball_len)
{
  Psc exceptballpsc;
  PrRef Prref;
  int isnew;
  ClRef clause;
  Cell *tptr;
  prolog_term term_to_assert;
  Cell *space_for_ball_assert;

  unsigned long space_for_ball_assert_len = 3*sizeof(Cell);

  space_for_ball_assert = (Cell *) mem_alloc_nocheck(space_for_ball_assert_len,
						     LEAK_SPACE);
  if (!space_for_ball_assert) 
    xsb_exit(CTXTc "[Resource] Out of memory");

  exceptballpsc = pair_psc((Pair)insert("$$exception_ball", (byte)2, 
					pair_psc(insert_module(0,"standard")), 
					&isnew));
  tptr = space_for_ball_assert;
  term_to_assert = makecs(tptr);
  bld_functor(tptr, exceptballpsc); tptr++;
  bld_int(tptr, xsb_thread_self()); tptr++;
  cell(tptr) = Ball; 

  assert_code_to_buff_p(CTXTc term_to_assert);
  /* need arity of 3, for extra cut_to arg */
  Prref = get_prref(CTXTc exceptballpsc);
  assert_buff_to_clref_p(CTXTc term_to_assert,3,Prref,0,makeint(0),0,&clause);
  mem_dealloc(cs_val(Ball),Ball_len,LEAK_SPACE);
  mem_dealloc(space_for_ball_assert,space_for_ball_assert_len,LEAK_SPACE);
  /* reset WAM emulator state to Prolog catcher */
  if (unwind_stack(CTXT)) xsb_exit(CTXTc "Unwind_stack failed in xsb_throw_internal!");
  /* Resume main emulator instruction loop */
  pcreg = (pb)&fail_inst;
  longjmp(xsb_abort_fallback_environment, XSB_ERROR);
}

DllExport void call_conv xsb_throw(CTXTdeclc prolog_term Ball)
{
  Psc exceptballpsc;
  PrRef Prref;
  int isnew;
  ClRef clause;
  Cell *tptr;
  prolog_term term_to_assert;
  Cell *space_for_ball_assert = 0;

  if (!space_for_ball_assert) {
    /* 3 cells needed for term */
    space_for_ball_assert = (Cell *) mem_alloc(3*sizeof(Cell),LEAK_SPACE);
    if (!space_for_ball_assert) xsb_exit(CTXTc "out of memory in xsb_throw!");
  }

  exceptballpsc = pair_psc((Pair)insert("$$exception_ball", (byte)2, 
					pair_psc(insert_module(0,"standard")), 
					&isnew));
  tptr = space_for_ball_assert;
  term_to_assert = makecs(tptr);
  bld_functor(tptr, exceptballpsc); tptr++;
  bld_int(tptr, xsb_thread_self()); tptr++;
  cell(tptr) = Ball; 

  assert_code_to_buff_p(CTXTc term_to_assert);
  /* need arity of 3, for extra cut_to arg */
  Prref = (PrRef)get_ep(exceptballpsc);
  assert_buff_to_clref_p(CTXTc term_to_assert,3,Prref,0,makeint(0),0,&clause);
  mem_dealloc(space_for_ball_assert,3*sizeof(Cell),LEAK_SPACE);
  /* reset WAM emulator state to Prolog catcher */
  if (unwind_stack(CTXT)) xsb_exit(CTXTc "Unwind_stack failed in xsb_throw!");

  /* Resume main emulator instruction loop */
  pcreg = (pb)&fail_inst ;
  longjmp(xsb_abort_fallback_environment, XSB_ERROR);
}


/********************************************************************/
/* Error types */
/********************************************************************/

// void calculation_error

/*****************/
void call_conv xsb_domain_error(CTXTdeclc char *valid_domain,Cell culprit, 
					const char *predicate,int arg) 
{
  prolog_term ball_to_throw;
  int isnew;
  Cell *tptr; char message[ERRMSGLEN];
  unsigned long ball_len = 10*sizeof(Cell);

  snprintf(message, ERRMSGLEN, "in arg %d of predicate %s)",arg,predicate);

  tptr =   (Cell *) mem_alloc(ball_len,LEAK_SPACE);

  ball_to_throw = makecs(tptr);
  bld_functor(tptr, pair_psc(insert("error",3,
				    (Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;
  bld_cs(tptr,(Cell) (tptr+3));
  tptr++;
  bld_string(tptr,string_find(message,1));
  tptr++;
  bld_copy(tptr,build_xsb_backtrace(CTXT));
  tptr++;
  bld_functor(tptr, pair_psc(insert("domain_error",2,
				    (Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;
  bld_string(tptr,string_find(valid_domain,1));
  tptr++;
  if (culprit == (Cell)NULL) bld_int(tptr,0); 
  else bld_ref(tptr,culprit);

  xsb_throw_internal(CTXTc ball_to_throw,ball_len);

}

/*****************/
/* Not using overflow or underflow yet */

void call_conv xsb_basic_evaluation_error(char *message,int type)
{
  prolog_term ball_to_throw;
  int isnew;
  Cell *tptr;
  unsigned long ball_len = 10*sizeof(Cell);
#ifdef MULTI_THREAD
  char mtmessage[MAXBUFSIZE];
  int tid = xsb_thread_self();
  th_context *th;
  th = find_context(tid);
#endif

  tptr =   (Cell *) mem_alloc(ball_len,LEAK_SPACE);
  ball_to_throw = makecs(tptr);
  bld_functor(tptr, pair_psc(insert("error",3,(Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;

  if (type == EVALUATION_INSTANTIATION_ERROR) {
    bld_string(tptr,string_find("instantiation_error",1));
    tptr++;
#ifdef MULTI_THREAD
    snprintf(mtmessage,MAXBUFSIZE,"[th %d] %s",tid,message);
    bld_string(tptr,string_find(mtmessage,1));
#else  
    bld_string(tptr,string_find(message,1));
#endif
    tptr++;
    bld_copy(tptr,build_xsb_backtrace(CTXT));
  }
  else if (type == EVALUATION_DOMAIN_ERROR) {
    bld_cs(tptr,(Cell) (tptr+3));
    tptr++;
#ifdef MULTI_THREAD
    snprintf(mtmessage,MAXBUFSIZE,"[th %d] %s",tid,message);
    bld_string(tptr,string_find(mtmessage,1));
#else  
    bld_string(tptr,string_find(message,1));
#endif
    tptr++;
    bld_copy(tptr,build_xsb_backtrace(CTXT));
    tptr++;
    bld_functor(tptr, pair_psc(insert("evaluation_error",1,(Psc)flags[CURRENT_MODULE],&isnew)));
    tptr++;
    bld_string(tptr,string_find("undefined",1));
  }
  xsb_throw_internal(CTXTc ball_to_throw,ball_len);
}

DllExport void call_conv xsb_evaluation_error(int type,char *description, ...)
{
  char message[MAXBUFSIZE];
  va_list args;

  va_start(args, description);
  strcpy(message, "++Error[XSB]: [Runtime/C] ");
  vsnprintf(message+strlen(message), (MAXBUFSIZE-strlen(message)), description, args);
  if (message[strlen(message)-1] == '\n') message[strlen(message)-1] = 0;
  va_end(args);
  xsb_basic_evaluation_error(message,type);
}

/*****************/

void call_conv xsb_existence_error(CTXTdeclc char *object,Cell culprit, 
					const char *predicate,int arity, int arg) 
{
  prolog_term ball_to_throw;
  int isnew;
  Cell *tptr; char message[ERRMSGLEN];
  unsigned long ball_len = 10*sizeof(Cell);

  snprintf(message,ERRMSGLEN,"in arg %d of predicate %s/%d)",arg,predicate,arity);

  tptr =   (Cell *) mem_alloc(ball_len,LEAK_SPACE);

  ball_to_throw = makecs(tptr);
  bld_functor(tptr, pair_psc(insert("error",3,
				    (Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;
  bld_cs(tptr,(Cell) (tptr+3));
  tptr++;
  bld_string(tptr,string_find(message,1));
  tptr++;
  bld_copy(tptr,build_xsb_backtrace(CTXT));
  tptr++;
  bld_functor(tptr, pair_psc(insert("existence_error",2,
				    (Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;
  bld_string(tptr,string_find(object,1));
  tptr++;
  if (culprit == (Cell)NULL) bld_int(tptr,0); 
  else bld_ref(tptr,culprit);

  xsb_throw_internal(CTXTc ball_to_throw, ball_len);

}


/*****************/

void call_conv xsb_instantiation_error(CTXTdeclc const char *predicate,int arg) {
  prolog_term ball_to_throw;
  int isnew;
  Cell *tptr; char message[ERRMSGLEN];
  unsigned long ball_len = 10*sizeof(Cell);

  snprintf(message,ERRMSGLEN," in arg %d of predicate %s",arg,predicate);
  tptr =   (Cell *) mem_alloc(ball_len,LEAK_SPACE);

  ball_to_throw = makecs(tptr);
  bld_functor(tptr, pair_psc(insert("error",3,
				    (Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;
  bld_string(tptr,string_find("instantiation_error",1));
  tptr++;
  bld_string(tptr,string_find(message,1));
  tptr++;
  bld_copy(tptr,build_xsb_backtrace(CTXT));

  xsb_throw_internal(CTXTc ball_to_throw,ball_len);

}

/*****************/
void call_conv xsb_misc_error(CTXTdeclc char *inmsg,const char *predicate,int arity)
{
  prolog_term ball_to_throw;
  int isnew;
  Cell *tptr; char message[ERRMSGLEN];
  unsigned long ball_len = 10*sizeof(Cell);

  snprintf(message,ERRMSGLEN," in predicate %s/%d: %s",predicate,arity,inmsg);

  tptr =   (Cell *) mem_alloc(ball_len,LEAK_SPACE);

  ball_to_throw = makecs(tptr);
  bld_functor(tptr, pair_psc(insert("error",3,
				    (Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;
  bld_string(tptr,string_find("misc_error",1));
  tptr++;
  bld_string(tptr,string_find(message,1));
  tptr++;
  bld_copy(tptr,build_xsb_backtrace(CTXT));

  xsb_throw_internal(CTXTc ball_to_throw,ball_len);

}

/*****************/
/* Operation/Object_type/Culprit 

   When using permission error from the loader, and perhaps elsewhere,
   there may not be a convenient cell for culprit.  In that case,
   setting culprit to 0 in the call gives a different error message
   that does not refer to culprit.  In this case, if desired the
   culprit can be put in the object string.  This isn't perfect, but
   it works.
 */
void call_conv xsb_permission_error(CTXTdeclc
				    char *operation,char *object,Cell culprit, 
				    const char *predicate,int arity) 
{
  prolog_term ball_to_throw;
  int isnew;
  Cell *tptr; char message[ERRMSGLEN];
  unsigned long ball_len = 10*sizeof(Cell);

  snprintf(message,ERRMSGLEN,"in predicate %s/%d)",predicate,arity);

  tptr =   (Cell *) mem_alloc(ball_len,LEAK_SPACE);

  ball_to_throw = makecs(tptr);
  bld_functor(tptr, pair_psc(insert("error",3,
				    (Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;
  bld_cs(tptr,(Cell) (tptr+3));
  tptr++;
  bld_string(tptr,string_find(message,1));
  tptr++;
  bld_copy(tptr,build_xsb_backtrace(CTXT));
  tptr++;
  bld_functor(tptr, pair_psc(insert("permission_error",3,
				    (Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;
  bld_string(tptr,string_find(operation,1));
  tptr++;
  bld_string(tptr,string_find(object,1));
  tptr++;
  //  if (culprit == (Cell)NULL) bld_int(tptr,0); 
  if (culprit == (Cell)NULL) bld_string(tptr,string_find("",1)); 
  else bld_ref(tptr,culprit);

  xsb_throw_internal(CTXTc ball_to_throw,ball_len);

}

/**************/

#define MsgBuf (*tsgSBuff1)
#define FlagBuf (*tsgSBuff2)

/* Memory errors are resource errors: therefore we have to be careful
   when handling the memory for throwing the error itself.
   Accordingly, varstrings are used rather than string finds to avoid
   possible overflow of string table, and there is a malloc and
   immediate free to make sure we'll have enough for messages to throw
   the error.  Similarly, mem_alloc_nocheck() is used to avoid
   problems in allocating memory for ball.*/

void call_conv xsb_resource_error(CTXTdeclc char *resource,
					const char *predicate,int arity) 
{
  prolog_term ball_to_throw;
  int isnew;
  Cell *tptr; char message[ERRMSGLEN];
  unsigned long ball_len = 10*sizeof(Cell);

  tptr = (Cell *) malloc(1000);
  if (!tptr) 
    xsb_exit(CTXTc "++Unrecoverable Error[XSB/Runtime]: [Resource] Out of memory");
  else free(tptr);

  snprintf(message,ERRMSGLEN,"in predicate %s/%d)",predicate,arity);
  XSB_StrSet(&MsgBuf,message);
  XSB_StrSet(&FlagBuf,resource);

  tptr =   (Cell *) mem_alloc_nocheck(ball_len,LEAK_SPACE);
  if (!tptr) 
    xsb_exit(CTXTc "++Unrecoverable Error[XSB/Runtime]: [Resource] Out of memory");

  ball_to_throw = makecs(tptr);
  bld_functor(tptr, pair_psc(insert("error",3,
				    (Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;
  bld_cs(tptr,(Cell) (tptr+3));
  tptr++;
  //  bld_string(tptr,string_find(message,1));
  bld_string(tptr,MsgBuf.string);
  tptr++;
  bld_copy(tptr,build_xsb_backtrace(CTXT));
  tptr++;
  bld_functor(tptr, pair_psc(insert("resource_error",1,
				    (Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;

  bld_string(tptr,FlagBuf.string);

  xsb_throw_internal(CTXTc ball_to_throw, ball_len);

}

/* TLS: exiting in MT engine because I haven't yet put contexts into
   mem_xxxocs.  And, I'm not sure whether this is a good idea. */

#ifdef MULTI_THREAD
void call_conv xsb_memory_error(char *resource,char *message)
{
  char exit_message[ERRMSGLEN];

  snprintf(exit_message,ERRMSGLEN,
	  "++Unrecoverable Error[XSB/Runtime]: [Resource] Out of memory (%s)",
	  message);
  exit_xsb(exit_message);
}
#else
void call_conv xsb_memory_error(char *resource,char *message) {
  xsb_resource_error_nopred(CTXTdeclc resource,message);
}
#endif

/* Like xsb_resource_error(), but does not include predicate and
   argument information. */
void call_conv xsb_resource_error_nopred(CTXTdeclc char *resource,char *message)
{
  prolog_term ball_to_throw;
  int isnew;
  Cell *tptr; 
  unsigned long ball_len = 10*sizeof(Cell);

  tptr = (Cell *) malloc(1000);
  if (!tptr) 
    xsb_exit(CTXTc "++Unrecoverable Error[XSB/Runtime]: [Resource] Out of memory");
  else free(tptr);

  XSB_StrSet(&MsgBuf,message);
  XSB_StrSet(&FlagBuf,resource);

  tptr =   (Cell *) mem_alloc_nocheck(ball_len,LEAK_SPACE);
  if (!tptr) 
    xsb_exit(CTXTc "++Unrecoverable Error[XSB/Runtime]: [Resource] Out of memory");

  ball_to_throw = makecs(tptr);
  bld_functor(tptr, pair_psc(insert("error",3,
				    (Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;
  bld_cs(tptr,(Cell) (tptr+3));
  tptr++;
  //  bld_string(tptr,string_find(message,1));
  bld_string(tptr,MsgBuf.string);
  tptr++;
  bld_copy(tptr,makenil);
  tptr++;
  bld_functor(tptr, pair_psc(insert("resource_error",1,
				    (Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;

  bld_string(tptr,FlagBuf.string);

  xsb_throw_internal(CTXTc ball_to_throw, ball_len);

}

#undef MsgBuf
#undef FlagBuf

/**************/

void call_conv xsb_syntax_error(CTXTdeclc char *message) 
{
  prolog_term ball_to_throw;
  int isnew;
  Cell *tptr;
  unsigned long ball_len = 10*sizeof(Cell);
#ifdef MULTI_THREAD
  char mtmessage[MAXBUFSIZE];
  int tid = xsb_thread_self();
#endif

  tptr =   (Cell *) mem_alloc(ball_len,LEAK_SPACE);
  ball_to_throw = makecs(tptr);
  bld_functor(tptr, pair_psc(insert("error",3,
				    (Psc)flags[CURRENT_MODULE],&isnew)));

  tptr++;
  bld_string(tptr,string_find("syntax_error",1));
  tptr++;
#ifdef MULTI_THREAD
  snprintf(mtmessage,MAXBUFSIZE,"[th %d] %s",tid,message);
  bld_string(tptr,string_find(mtmessage,1));
#else  
  bld_string(tptr,string_find(message,1));
#endif
  tptr++;
  bld_copy(tptr,build_xsb_backtrace(CTXT));
  xsb_throw_internal(CTXTc ball_to_throw,ball_len);
}			       

/**************/

void call_conv xsb_table_error(CTXTdeclc char *message) 
{
  prolog_term ball_to_throw;
  int isnew;
  Cell *tptr;
  unsigned long ball_len = 10*sizeof(Cell);
#ifdef MULTI_THREAD
  char mtmessage[MAXBUFSIZE];
  int tid = xsb_thread_self();
  //  th_context *th;
  //  th = find_context(xsb_thread_self());
#endif

  tptr =   (Cell *) mem_alloc(ball_len,LEAK_SPACE);
  ball_to_throw = makecs(tptr);
  bld_functor(tptr, pair_psc(insert("error",3,
				    (Psc)flags[CURRENT_MODULE],&isnew)));

  tptr++;
  bld_string(tptr,string_find("table_error",1));
  tptr++;
#ifdef MULTI_THREAD
  snprintf(mtmessage,MAXBUFSIZE,"[th %d] %s",tid,message);
  bld_string(tptr,string_find(mtmessage,1));
#else  
  bld_string(tptr,string_find(message,1));
#endif
  tptr++;
  bld_copy(tptr,build_xsb_backtrace(CTXT));
  xsb_throw_internal(CTXTc ball_to_throw,ball_len);
}			       

/**************/

void call_conv xsb_type_error(CTXTdeclc char *valid_type,Cell culprit, 
					const char *predicate,int arg) 
{
  prolog_term ball_to_throw;
  int isnew;
  Cell *tptr; char message[ERRMSGLEN];
  unsigned long ball_len = 10*sizeof(Cell);

  snprintf(message,ERRMSGLEN,"in arg %d of predicate %s)",arg,predicate);

  tptr =   (Cell *) mem_alloc(ball_len,LEAK_SPACE);

  ball_to_throw = makecs(tptr);
  bld_functor(tptr, pair_psc(insert("error",3,
				    (Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;
  bld_cs(tptr,(Cell) (tptr+3));
  tptr++;
  bld_string(tptr,string_find(message,1));
  tptr++;
  bld_copy(tptr,build_xsb_backtrace(CTXT));
  tptr++;
  bld_functor(tptr, pair_psc(insert("type_error",2,
				    (Psc)flags[CURRENT_MODULE],&isnew)));
  tptr++;
  bld_string(tptr,string_find(valid_type,1));
  tptr++;
  if (culprit == (Cell)NULL) bld_int(tptr,0); 
  else bld_ref(tptr,culprit);

  xsb_throw_internal(CTXTc ball_to_throw, ball_len);

}

/**************/

void call_conv xsb_unrecoverable_error(CTXTdeclc char *message) 
{
  prolog_term ball_to_throw;
  int isnew;
  Cell *tptr;
  unsigned long ball_len = 10*sizeof(Cell);
#ifdef MULTI_THREAD
  char mtmessage[MAXBUFSIZE];
  int tid = xsb_thread_self();
#endif

  tptr =   (Cell *) mem_alloc(ball_len,LEAK_SPACE);
  ball_to_throw = makecs(tptr);
  bld_functor(tptr, pair_psc(insert("error",3,
				    (Psc)flags[CURRENT_MODULE],&isnew)));

  tptr++;
  bld_string(tptr,string_find("unrecoverable_error",1));
  tptr++;
#ifdef MULTI_THREAD
  snprintf(mtmessage,MAXBUFSIZE,"[th %d] %s",tid,message);
  bld_string(tptr,string_find(mtmessage,1));
#else  
  bld_string(tptr,string_find(message,1));
#endif
  tptr++;
  bld_copy(tptr,build_xsb_backtrace(CTXT));
  xsb_throw_internal(CTXTc ball_to_throw,ball_len);
}			       

/*****************/

void call_conv xsb_basic_abort(char *message)
{
  prolog_term ball_to_throw;
  int isnew;
  Cell *tptr;
  unsigned long ball_len = 10*sizeof(Cell);
#ifdef MULTI_THREAD
  char mtmessage[MAXBUFSIZE];
  int tid = xsb_thread_self();
  th_context *th;
  th = find_context(tid);
#endif

  if( !wam_initialized )
  {
	xsb_initialization_exit(message) ;
  }

  tptr =   (Cell *) mem_alloc(ball_len,LEAK_SPACE);
  ball_to_throw = makecs(tptr);
  bld_functor(tptr, pair_psc(insert("error",3,(Psc)flags[CURRENT_MODULE],&isnew)));

  tptr++;
  bld_string(tptr,string_find("misc_error",1));
  tptr++;
#ifdef MULTI_THREAD
  snprintf(mtmessage,MAXBUFSIZE,"[th %d] %s",tid,message);
  bld_string(tptr,string_find(mtmessage,1));
#else  
  bld_string(tptr,string_find(message,1));
#endif
  tptr++;
  bld_copy(tptr,build_xsb_backtrace(CTXT));
  xsb_throw_internal(CTXTc ball_to_throw,ball_len);
}

DllExport void call_conv xsb_abort(char *description, ...)
{
  char message[MAXBUFSIZE];
  va_list args;

  va_start(args, description);
  //  strcpy(message, "++Error[XSB]: [Runtime/C] ");
  strcpy(message, " ");
  vsnprintf(message+strlen(message), (MAXBUFSIZE-strlen(message)), description, args);
  if (message[strlen(message)-1] == '\n') message[strlen(message)-1] = 0;
  va_end(args);
  xsb_basic_abort(message);
}

DllExport void call_conv abort_xsb(char * description)
{
  char message[MAXBUFSIZE];
  strcpy(message, "++Error[XSB]: [Runtime/C] ");
  snprintf(message+strlen(message), (MAXBUFSIZE-strlen(message)), description);
  if (message[strlen(message)-1] == '\n')
  {
    message[strlen(message)-1] = 0;
  }
  xsb_basic_abort(message);
}

/* could give these a different ball to throw */
DllExport void call_conv xsb_bug(char *description, ...)
{
  char message[MAXBUFSIZE];
  va_list args;

  va_start(args, description);

  strcpy(message, "++XSB bug: ");
  vsnprintf(message+strlen(message), (MAXBUFSIZE-strlen(message)), description, args);
  if (message[strlen(message)-1] != '\n')
    strcat(message, "\n");

  va_end(args);
  xsb_basic_abort(message);
}

DllExport void call_conv bug_xsb(char *description)
{
  char message[MAXBUFSIZE];
  strcpy(message, "++XSB bug: ");
  snprintf(message+strlen(message), (MAXBUFSIZE-strlen(message)), description);
  if (message[strlen(message)-1] != '\n')
    strcat(message, "\n");

  xsb_basic_abort(message);
}

/*----------------------------------------------------------------------*/

/* TLS: changed this to be close to the standard by reporting
   instantiation errors and evaluation errors (both reported by
   xsb_evaluation_error()).  Underflow and overflow errors are still
   not caught.
*/

#define str_op1 (*tsgSBuff1)
#define str_op2 (*tsgSBuff2)
void arithmetic_abort(CTXTdeclc Cell op1, char *OP, Cell op2)
{
  XSB_StrSet(&str_op1,"");
  XSB_StrSet(&str_op2,"");
  print_pterm(CTXTc op1, TRUE, &str_op1);
  print_pterm(CTXTc op2, TRUE, &str_op2);
  if (isref(op1) || isref(op2)) {
    xsb_evaluation_error(EVALUATION_INSTANTIATION_ERROR,
			 "Uninstantiated argument of evaluable function %s/2\n%s %s %s %s%s",
			 OP, "   Goal:",
			 (isref(op1)? "_Var": str_op1.string),
			 OP,
			 (isref(op2)? "_Var": str_op2.string),
			 ", probably as 2nd arg of is/2");
  }
  else {
    //    xsb_abort("Wrong domain in evaluable function %s/2\n%s %s %s %s found",
    //	      OP, "         Arithmetic expression expected, but",
    //	      str_op1.string, OP, str_op2.string);
    xsb_evaluation_error(EVALUATION_DOMAIN_ERROR,
			 "Wrong domain in evaluable function %s/2\n%s %s %s %s found",
			 OP, "         Arithmetic expression expected, but",
			 str_op1.string, OP, str_op2.string);
  }
}
#undef str_op1
#undef str_op2

#define str_op (*tsgSBuff1)
void arithmetic_abort1(CTXTdeclc char *OP, Cell op)
{
  XSB_StrSet(&str_op,"_Var");
  if (! isref(op)) print_pterm(CTXTc op, TRUE, &str_op);
  xsb_abort("%s evaluable function %s/2\n%s %s(%s) %s",
	    (isref(op) ? "Uninstantiated argument of" : "Wrong domain in"),
	    OP, "   Goal:", OP, str_op.string,
	    ", probably as 2nd arg of is/2");  
}
#undef str_op

#define str_op1 (*tsgSBuff1)
void arithmetic_comp_abort(CTXTdeclc Cell op1, char *OP, int op2)
{
  XSB_StrSet(&str_op1,"_Var");
  if (! isref(op1)) print_pterm(CTXTc op1, TRUE, &str_op1);
  xsb_abort("%s arithmetic comparison %s/2\n%s %s %s %d",
	    (isref(op1) ? "Uninstantiated argument of" : "Wrong type in"),
	    OP, "   Goal:", str_op1.string, OP, op2);
}
#undef str_op1

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
#if defined(DEBUG_VERBOSE) && defined(CP_DEBUG)
  print_cp_backtrace();
#endif
}

DllExport void call_conv error_xsb (char *description)
{
  fprintf(stderr, "\n++Error[XSB]: [Runtime/C] ");
  fprintf(stderr, description);
  fprintf(stderr, "\n");
#if defined(DEBUG_VERBOSE) && defined(CP_DEBUG)
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
#if defined(DEBUG_VERBOSE) && defined(CP_DEBUG)
  print_cp_backtrace();
#endif
}

DllExport void call_conv warn_xsb(char *description)
{
  fprintf(stdwarn, "\n++Warning[XSB]: [Runtime/C] ");
  fprintf(stdwarn, description);
  fprintf(stdwarn, "\n");
#if defined(DEBUG_VERBOSE) && defined(CP_DEBUG)
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

DllExport void call_conv mesg_xsb(char *description)
{
  fprintf(stdmsg, description);
  fprintf(stdmsg, "\n");
}

#ifdef DEBUG_VERBOSE
DllExport void call_conv xsb_dbgmsg1(int log_level, char *description, ...)
{
  va_list args;

  if (log_level <= cur_log_level) {
    va_start(args, description);
    vfprintf(stddbg, description, args);
    va_end(args);
    //    fprintf(stddbg, "\n");
  }
}

DllExport void call_conv dbgmsg1_xsb(int log_level, char *description)
{
  if (log_level <= cur_log_level) {
    fprintf(stddbg, description);
  }
}
#endif

/*----------------------------------------------------------------------*/

/* TLS: obsolete for most error types */
void err_handle(CTXTdeclc int description, int arg, char *f,
		int ar, char *expected, Cell found)
{
  char message[ERRMSGLEN];	/* Allow 3 lines of error reporting.	*/
  switch (description) {
  case RANGE:	/* I assume expected != NULL */
    snprintf
      (message,ERRMSGLEN,
       "! %s error: in argument %d of %s/%d\n! %s expected, but %d found",
       err_msg_table[description], arg, f, 
       ar, expected, (int) int_val(found));
    break;
  case ZERO_DIVIDE:
    snprintf(message,ERRMSGLEN,
	    "! %s error in %s\n! %s expected, but %lx found",
	    err_msg_table[description], f, expected, found);
    break;
  default:
    snprintf(message,ERRMSGLEN,
	    "! %s error (not completely handled yet): %s",
	    err_msg_table[description], expected);
    break;
  }
  xsb_basic_abort(message);
#if defined(DEBUG_VERBOSE) && defined(CP_DEBUG)
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

/* TLS: I keep forgetting what is going on here, so I'm documenting
   it.  Each time a catch is called, a scope marker is set and this
   scope marker points to the literal '$$clean_up_block' in the first
   clause of catch.  There is a little monkying to make the scope
   marker equal to this (THROWPAD).  Upon a throw, unwind_stack()
   checks cp regs of various envs to see whether a given cpreg is
   equal to this -- i.e. the env represents that of a catch.  If so,
   we fail into the second clause of the catch and try to unify with
   the exception ball.  If so, we do what the handler tells us, if
   not, we call unwind_stack again to look for the right catcher to
   unify with the ball. */

#ifndef MULTI_THREAD
byte *catch_scope_marker;
#endif

int set_scope_marker(CTXTdecl)
{
  /*   printf("%x %x\n",cp_ereg(breg),ereg);*/
   catch_scope_marker = pcreg;
   /* skipping a putpval and a call instruction */
   /* is there a portable way to do this ?      */
   /* instruction builtin has already made pcreg point to the putpval */
   catch_scope_marker += THROWPAD;
   return(TRUE);
} /* set_scope_marker */



int unwind_stack(CTXTdecl)
{
   byte *cp, *cpmark;
   CPtr e,b, xtemp1, xtemp2;
   CPtr tmp_compl_frm = NULL;

   cpmark = catch_scope_marker;
   /* first find the right environment */
   e = ereg;
   cp = cpreg; /* apparently not pcreg ... maybe not good in general */
   while ( (cp != cpmark) && e )
     {
       /*            printf("cp %d x%x\n",cp,cp);*/
       cp = (byte *)e[-1];
       e = (CPtr)e[0];
     }

   if ( ! e )
     xsb_exit(CTXTc "Throw failed because no catcher for throw");

   /* now find the corresponding breg */
   b = breg;
   while (cp_ereg(b) <= e) {
     if (IS_TABLE_INSTRUC(*cp_pcreg(b))) 
       tmp_compl_frm = subg_compl_stack_ptr(tcp_subgoal_ptr(b));
     b = cp_prevbreg(b);
   }
   if (IS_TABLE_INSTRUC(*cp_pcreg(b))) 
     tmp_compl_frm = subg_compl_stack_ptr(tcp_subgoal_ptr(b));
   breg = b;
   if (tmp_compl_frm != NULL) {
     remove_incomplete_tries(CTXTc prev_compl_frame(tmp_compl_frm));
   }
   unwind_trail(breg,xtemp1,xtemp2);

   return(FALSE);

} /* unwind_stack */

/* Bart's original  
int clean_up_block(CTXTdecl)
{
  if (cp_ereg(breg) > ereg) {
     //          printf("%x %x\n",cp_ereg(breg),ereg); 
    breg = (CPtr)cp_prevbreg(breg);
  }
   return(TRUE);
} 
*/

int clean_up_block(CTXTdeclc int bregBefore)
{
  if (bregBefore == (int) ((pb)tcpstack.high - (pb)breg)) {
    //    printf("setting breg %x to prevbreg %x\n",breg,cp_prevbreg(breg));
    breg = (CPtr)cp_prevbreg(breg);
  }
  return(TRUE);
} 

/*
 * You should probably get rid of clean_up_block and reimplement the
 * first clause of catch/3 as:
 * 
 * catch(Goal,_Catcher,_Handler) :-
 *         '$$set_scope_marker',  % should not be called in any other place
 *                                % because it remembers the pcreg
 *         '_$savecp'(Before),
 *         call(Goal),
 *         '_$savecp'(After),
 *         (Before == After ->
 *             !
 *        ;
 *           true
 *       ).
 *
 * This will do the cleanup correctly (I think :-)
 */

/*---------------------------- end of error_xsb.c --------------------------*/

