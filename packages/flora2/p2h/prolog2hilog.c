/* File:      prolog2hilog.c
** Author(s): kifer
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 2000
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

#include <stdio.h>
#include <string.h>

#ifdef WIN_NT
#define XSB_DLL
#endif

#include "auxlry.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "cinterf.h"


#if 0
#define P2HDEBUG
#define P2HDEBUG_VERBOSE
#endif

/* take hilog term and a hilog apply op and return prolog term.
   If the apply term is != the one used in the hilog term, assume it is already
   a prolog term and don't convert */
static prolog_term hilog2prolog(prolog_term hterm, char *apply);
/* take prolog term and a symbol name of the apply operator and return hilog
   term. If prolog term already has the main functor==hilog apply, then don't
   convert. */
static prolog_term prolog2hilog(prolog_term pterm, char *apply);
static char *pterm2string(prolog_term term);
inline static int is_hilog(prolog_term term, char *apply_funct);
inline static int is_commalist(prolog_term term);
static prolog_term map_commalist(prolog_term (*func)(), prolog_term term, char *apply);
static prolog_term map_list(prolog_term func(), prolog_term term, char *apply);



/*
  When called from Prolog, takes 3 args:
  - Pterm:  Prolog term
  - Hterm:  HiLog term
  - Apply:  Symbol name for the HiLog apply predicate

  If Pterm is a variable, then it is unified with Hterm.
  If Hterm is a variable, then it is unified with Pterm.
  If both Pterm and Hterm are scalar (int, float, string), then they are
  unified. 

  If Pterm or Hterm is a list or a commalist (a,b,c,), then the function is
  applied to each element and the results are returned as a list or a commalist
  (whichever applies).

  If Hterm is term (and not a list or a commalist), then Hterm is assumed to be
  a HiLog term of the kind that converts to Prolog. Hterm is then converted to
  Prolog and the result is unified with Pterm.  If the main functor is !=
  Apply, then it is assumed to be a prolog term and the term is returned
  without conversion.

  If Pterm is a term that is not a list or a commalist, then it is assumed to
  be a prolog term. It is converted to HiLog using the apply-functor name given
  in Apply (which must be an atom).  The result then unifies with Hterm.  If
  the main functor is = Apply, then we assume that the term is already a HiLog
  term and the term is simply returned without change.

  For instance,
      plg2hlg(f(a,g(b,X)),Y,abc)
      Y = abc(f,a,abc(g,b,_h123))

      plg2hlg(X, cde(f,a,cde(g,b,Y))),Z)
      X = abc(f,a,abc(g,b,_h123))
      Z = abc

  Doesn't do occur-check!!! Something like
      plg2hlg(X, cde(f,a,cde(g,b,X))),Z)
  Will loop and eventually crash because X occurs in Pterm and in Hterm.
 */
DllExport xsbBool call_conv plg2hlg () {
  prolog_term pterm = reg_term(1);
  prolog_term hterm = reg_term(2);
  prolog_term apply_t = reg_term(3);
  prolog_term temp_pterm;
  char *apply;

#ifdef P2HDEBUG_VERBOSE
  xsb_dbgmsg("plg2hlg: Arg1=%s", pterm2string(pterm));
  xsb_dbgmsg("plg2hlg: Arg2=%s", pterm2string(hterm));
  xsb_dbgmsg("plg2hlg: Arg3=%s", pterm2string(apply_t));
#endif

  if (!is_atom(apply_t))
    xsb_abort("PLG2HLG: Arg 3 = `%s' (the `apply' functor) isn't an atom.",
	      pterm2string(apply_t));

  apply = string_val(apply_t);

  if (is_var(pterm)) {
    /* prolog term is variable; convert hilog to prolog */
    if (is_var(hterm))
      return p2p_unify(pterm, hterm);

    temp_pterm = hilog2prolog(hterm, apply);
    return p2p_unify(pterm, temp_pterm);

  } else {
    temp_pterm = prolog2hilog(pterm, apply);
    /* prolog term is instantiated; convert to hilog */
    return p2p_unify(temp_pterm, hterm);
  }
  return TRUE; /* this is unreachable---just to pacify the compiler */
}

static inline xsbBool is_scalar(prolog_term pterm)
{
  if (is_atom(pterm) || is_int(pterm) || is_float(pterm))
    return TRUE;
  return FALSE;
}


static prolog_term hilog2prolog(prolog_term hterm, char *apply)
{
  prolog_term pterm = p2p_new();
  prolog_term pfunctor;
  int arity, i;

  if (is_var(hterm))
    return p2p_new();
  else if (is_list(hterm))
    return map_list(hilog2prolog,hterm,apply);
  else if (is_commalist(hterm))
    return map_commalist(hilog2prolog,hterm,apply);

  if (is_scalar(hterm) || is_var(hterm))
    return hterm;
#ifdef P2HDEBUG
  if (!is_functor(hterm))
    xsb_abort("PLG2HLG: Arg 2 =`%s' is not a HiLog term.",
	      pterm2string(hterm));
#endif

  /* Don't convert if already Prolog */
  if (!is_hilog(hterm,apply)) return hterm;

  arity=p2c_arity(hterm);

  pfunctor = p2p_arg(hterm,1);
  if (!is_atom(pfunctor))
    xsb_abort("PLG2HLG: HiLog term `%s' not convertible to Prolog.",
	      pterm2string(hterm));
  if (arity > 1)
    c2p_functor(string_val(pfunctor), arity-1, pterm);
  else
    return pfunctor;

#ifdef P2HDEBUG_VERBOSE
  xsb_dbgmsg("h2p start: Pterm=%s", pterm2string(pterm));
  xsb_dbgmsg("h2p start: Hterm=%s", pterm2string(hterm));
  xsb_dbgmsg("h2p start: Apply=%s", apply);
#endif

  for (i=2; i<=arity; i++) {
    p2p_unify(hilog2prolog(p2p_arg(hterm,i), apply),
	      p2p_arg(pterm, i-1));
#ifdef P2HDEBUG_VERBOSE
    xsb_dbgmsg("h2p loop: Pterm=%s", pterm2string(pterm));
#endif
  }
  return pterm;
}


static prolog_term prolog2hilog(prolog_term pterm, char *apply)
{
  prolog_term hterm = p2p_new();
  int arity, i;

  if (is_var(pterm))
    return p2p_new();
  else if (is_list(pterm))
    return map_list(prolog2hilog,pterm,apply);
  else if (is_commalist(pterm))
    return map_commalist(prolog2hilog,pterm,apply);

  if (is_scalar(pterm) || is_var(pterm))
    return pterm;
  if (!is_functor(pterm))
    xsb_abort("PLG2HLG: Arg 1 = `%s' (the Prolog term) must be a var, a const, or a functor.",
	      pterm2string(pterm));

  /* Don't convert if already HiLog */
  if (is_hilog(pterm,apply)) return pterm;

  arity = p2c_arity(pterm);
  c2p_functor(apply,arity+1,hterm);
  c2p_string(p2c_functor(pterm), p2p_arg(hterm,1)); /* set the functor arg */

#ifdef P2HDEBUG_VERBOSE
  xsb_dbgmsg("p2h start: Pterm=%s", pterm2string(pterm));
  xsb_dbgmsg("p2h start: Hterm=%s", pterm2string(hterm));
  xsb_dbgmsg("p2h start: Apply=%s", apply);
#endif

  /* set the rest of the args */
  for (i=1; i<=arity; i++) {
    p2p_unify(prolog2hilog(p2p_arg(pterm,i),apply), p2p_arg(hterm,i+1));
#ifdef P2HDEBUG_VERBOSE
  xsb_dbgmsg("p2h loop: Hterm=%s", pterm2string(hterm));
#endif
  }
  return hterm;
}


static prolog_term map_list(prolog_term func(), prolog_term termList, char *apply)
{
  prolog_term listHead, listTail;
  prolog_term outList=p2p_new(), outListHead, outListTail;
  prolog_term temp_term;

  c2p_list(outList);
  listTail = termList;
  outListTail = outList;

  while (!is_nil(listTail)) {
    listHead = p2p_car(listTail);
    outListHead = p2p_car(outListTail);
    temp_term = func(listHead,apply);
    p2p_unify(outListHead, temp_term);
    listTail = p2p_cdr(listTail);
    outListTail = p2p_cdr(outListTail);
    c2p_list(outListTail);
  }

  c2p_nil(outListTail); /* bind tail to nil */
  
  return outList;
}

static prolog_term map_commalist(prolog_term (*func)(), prolog_term termCList, char *apply)
{
  prolog_term clistHead, clistTail;
  prolog_term outCList=p2p_new(), outCListHead, outCListTail;
  prolog_term temp_term;
  char *clist_functor = ",";

  c2p_functor(clist_functor, 2, outCList);
  outCListTail = outCList;
  clistTail = termCList;

  while (is_commalist(clistTail)) {
    clistHead = p2p_arg(clistTail,1);
    outCListHead = p2p_arg(outCListTail,1);
    temp_term = func(clistHead,apply);
    p2p_unify(temp_term,outCListHead);
    clistTail = p2p_arg(clistTail,2);
    outCListTail = p2p_arg(outCListTail,2);
    if (is_commalist(clistTail))
      c2p_functor(clist_functor, 2, outCListTail);
  }

  p2p_unify(outCListTail,func(clistTail,apply));
  return outCList;
}


static char *pterm2string(prolog_term term)
{ 
  static VarString *StrArgBuf;
  prolog_term term2 = p2p_deref(term);

  XSB_StrCreate(StrArgBuf);
  XSB_StrSet(StrArgBuf,"");
  print_pterm(term2, 1, StrArgBuf); 
  return StrArgBuf->string;
} 


inline static int is_hilog(prolog_term term, char *apply_funct)
{
  return (strcmp(apply_funct, p2c_functor(term))==0);
}


inline static int is_commalist(prolog_term term)
{
  if (is_scalar(term) || is_list(term)) return FALSE;
  return (strcmp(",", p2c_functor(term))==0);
}

/* 
   plg2hlg(a(qq,b(c,4),b(c,5,d(X,U))),X,aaa).
   plg2hlg(aaa(qq,b(c,4)),X,aaa).
   plg2hlg(X, aaa(qq,b(c,4),aaa(kkk,Bbb,aaa(ppp,aaa(uuu,Aaa),Ooo))),aaa).
   plg2hlg(X, aaa(qq,aaa(aaa,4)),aaa).
   plg2hlg(X, [], aaa).
   plg2hlg([], X, aaa).
   plg2hlg(X, [aaa(qq,b(c,4)), f(abc), aaa(b,c(K),aaa(bbb,aaa(ccc,aaa(ddd))))],aaa).
   plg2hlg(X, [aaa(qq,b(c,4)), f(abc), aaa(b,c(K))],aaa).
   plg2hlg(X, [[aaa(qq,b(c,4)), f(abc)], aaa(b,c(K))],aaa).
   plg2hlg([aaa(qq,b(c,4)), a(qq,b(c,4)), f(q(a),b,c(p,q(Y)))], X, aaa).
   plg2hlg([aaa(qq,b(c,4)), [a(qq,b(c,4))], [f(q(a),b,c(p,q(Y))), b(_)]], X, aaa).
   plg2hlg(X, (aaa(qq,b(c,4)), f(abc), aaa(b,c(K),aaa(bbb,aaa(ccc,aaa(ddd))))),aaa).
   plg2hlg(X, (aaa(qq,b(c,4)), f(abc), aaa(b,c(K))),aaa).
   plg2hlg(X, ((aaa(qq,b(c,4)), f(abc)), aaa(b,c(K))),aaa).
   plg2hlg((aaa(qq,b(c,4)), a(qq,b(c,4)), f(q(a),b,c(p,q(Y)))), X, aaa).
   plg2hlg((aaa(qq,b(c,4)), a(qq,b(c,4)), f(q(a),b,c(p,q(Y)))), X, aaa).
   plg2hlg(((aaa(qq,b(c,4)), a(qq,b(c,4))), (f(q(a),b,c(p,q(Y))), b(_))), X, aaa).
*/
