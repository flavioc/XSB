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

#include "auxlry.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "cinterf.h"


#if 0
#define P2HDEBUG
#define P2HDEBUG_VERBOSE
#endif

/* take hilog term, return prolog term and the symbol name of the apply
   operator used in that hilog term */
static prolog_term hilog2prolog(prolog_term hterm, char **apply);
/* take orilog term and a symbol name of the apply operator and return hilog
   term */
static prolog_term prolog2hilog(prolog_term pterm, char *apply);
static char *pterm2string(prolog_term term);


/*
  When called from Prolog, takes 3 args:
  - Pterm:  Prolog term
  - Hterm:  HiLog term
  - Apply:  Symbol name for the HiLog apply predicate

  If Pterm is a variable, then it is unified with Hterm.
  If Hterm is a variable, then it is unified with Pterm.
  If both Pterm and Hterm are scalar (int, float, string), then they are
  unified. 
  If Hterm is term, then Hterm is assumed to be a HiLog term of the kind that
  converts to Prolog. Hterm is then converted to Prolog and the result is
  unified with Pterm. 
  Apply is unified with the symbol used as the "apply" functor in Hterm.

  If Pterm is a term, then it is assumed to be a functor. It is converted to
  HiLog using the apply-functor name given in Apply (which must be an atom).
  The result then unifies with Hterm.

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
xsbBool plg2hlg () {
  prolog_term pterm = reg_term(1);
  prolog_term hterm = reg_term(2);
  prolog_term apply_t = reg_term(3);

#ifdef P2HDEBUG_VERBOSE
  xsb_dbgmsg("plg2hlg: Arg1=%s", pterm2string(pterm));
  xsb_dbgmsg("plg2hlg: Arg2=%s", pterm2string(hterm));
  xsb_dbgmsg("plg2hlg: Arg3=%s", pterm2string(apply_t));
#endif

  if (is_var(pterm)) {
    /* prolog term is variable; convert hilog to prolog */
    if (is_var(hterm))
      return p2p_unify(pterm, hterm);
    else  {
      char *apply_h=NULL;
      prolog_term temp_pterm = hilog2prolog(hterm, &apply_h);

      if (is_var(apply_t)) {
#ifdef P2HDEBUG_VERBOSE
	xsb_dbgmsg("plg2hlg: Apply_h=%s", apply_h);
#endif
	if (apply_h != NULL)
	  c2p_string(apply_h, apply_t); /* arg 3 gets the value of apply_h */
	return p2p_unify(pterm, temp_pterm);
      } else if (is_string(apply_t)) {
	printf("apply_h=%s apply_t=%s\n",apply_h,string_val(apply_t));
	return (p2p_unify(pterm, temp_pterm)
		&& (apply_h==NULL
		    || strcmp(apply_h,string_val(apply_t))==0));
      } else
	xsb_abort("PLG2HLG: Arg 3 = `%s' (the `apply' functor) isn't an atom.",
		  pterm2string(apply_t));
    }
  } else {
    /* prolog term is instantiated; convert to hilog */
    char *apply;

    if (!is_string(apply_t))
      xsb_abort("PLG2HLG: Arg 3 = `%s' (the `apply' functor) isn't an atom.",
		pterm2string(apply_t));

    apply = string_val(apply_t);
    return p2p_unify(prolog2hilog(pterm,apply), hterm);
  }
  return TRUE; /* this is unreachable---just to pacify the compiler */
}

static inline xsbBool is_scalar(prolog_term pterm)
{
  if (is_string(pterm) || is_int(pterm) || is_float(pterm))
    return TRUE;
  return FALSE;
}


static prolog_term hilog2prolog(prolog_term hterm, char **apply)
{
  prolog_term pterm = p2p_new();
  prolog_term pfunctor;
  int arity, i;

  if (is_scalar(hterm) || is_var(hterm))
    return hterm;
#ifdef P2HDEBUG
  if (!is_functor(hterm))
    xsb_abort("PLG2HLG: Arg 2 =`%s' is not a HiLog term.",
	      pterm2string(hterm));
#endif
  /* this is the apply symbol */
  *apply = p2c_functor(hterm);
  arity=p2c_arity(hterm);
#ifdef P2HDEBUG_VERBOSE
  xsb_dbgmsg("h2p: Apply_h=%s", *apply);
#endif

  pfunctor = p2p_arg(hterm,1);
#ifdef P2HDEBUG
  if (!is_string(pfunctor))
    xsb_abort("PLG2HLG: HiLog term `%s' not convertable to Prolog.",
	      pterm2string(hterm));
#endif
  c2p_functor(string_val(pfunctor), arity-1, pterm);

#ifdef P2HDEBUG_VERBOSE
  xsb_dbgmsg("h2p start: Pterm=%s", pterm2string(pterm));
  xsb_dbgmsg("h2p start: Hterm=%s", pterm2string(hterm));
  xsb_dbgmsg("h2p start: Apply=%s", *apply);
#endif

  for (i=2; i<=arity; i++) {
    char *apply;
    p2p_unify(hilog2prolog(p2p_arg(hterm,i), &apply),
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

  if (is_scalar(pterm) || is_var(pterm))
    return pterm;
  if (!is_functor(pterm))
    xsb_abort("PLG2HLG: Arg 1 = `%s' (the Prolog term) must be a var, a const, or a functor.",
	      pterm2string(pterm));

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
    p2p_unify(prolog2hilog(p2p_arg(pterm,i),apply),
	      p2p_arg(hterm,i+1));
#ifdef P2HDEBUG_VERBOSE
  xsb_dbgmsg("p2h loop: Hterm=%s", pterm2string(hterm));
#endif
  }
  return hterm;
}


static char *pterm2string(prolog_term term)
{ 
  static XSB_StrDefine(StrArgBuf);
  prolog_term term2 = p2p_deref(term);
  XSB_StrSet(&StrArgBuf,"");
  print_pterm(term2, 1, &StrArgBuf); 
  return StrArgBuf.string;
} 

