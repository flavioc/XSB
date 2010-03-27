/* File:      trie_lookup.c
** Author(s): Ernie Johnson
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

#include "debugs/debug_tries.h"

#include <stdio.h>
#include <stdlib.h>

#include "auxlry.h"
#include "context.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "psc_xsb.h"
#include "deref.h"
#include "table_stats.h"
#include "trie_internals.h"
#include "tst_aux.h"
#include "subp.h"
#include "debug_xsb.h"
#include "flags_xsb.h"
#include "memory_xsb.h"
#if (defined(DEBUG_VERBOSE) || defined(DEBUG_ASSERTIONS))
#include "tst_utils.h"
#endif

/****************************************************************************

   This file defines a set of functions for searching a trie.  These
   functions can be categorized into high-level interface functions:

     void *variant_trie_lookup(void *, int, CPtr, Cell[])
     void *subsumptive_trie_lookup(void *, int, CPtr, TriePathType *, Cell[])

   and low-level internal functions:

     void *var_trie_lookup(void *, xsbBool *, Cell *)
     void *iter_sub_trie_lookup(void *, TriePathType *)
     BTNptr rec_sub_trie_lookup(BTNptr, TriePathType *)

   The following simple invariants are enforced and enable the
   interoperability between these functions, higher-level wrappers,
   and those routines which perform a check/insert operation:

   * The interface routines assume a non-NULL trie pointer and accept
     a term set as an integer count and an array of terms.
   * The low-level routines assume a non-EMPTY trie, the term set to be
     on the TermStack, and appropriate initialization of any other
     required auxiliary data structure (trie stacks and arrays).

   Each function at a particular level should assure compliance with a
   lower-level's constraint before invocation of that lower-level
   function.
    
****************************************************************************/

#include "xsb.lookup.c"

/*=========================================================================*/

/*
 *			   Variant Trie Lookup
 *			   ===================
 *
 * Searches a branch of a NON-EMPTY trie for a path containing the given
 * set of terms.  The branch is identified by a non-NULL pointer to a
 * trie node which roots the branch.  The terms, which are stored on the
 * Termstack, are compared against the symbols in the nodes lying BELOW
 * the given node.  The last node containing a symbol which matches a
 * subterm on the TermStack is returned.  If all the terms were matched,
 * then this node will be a leaf of the trie.  If no terms were mathced,
 * then this node will be the given node.  The second argument is set
 * appropriately to inform the caller whether the terms were found.  If
 * no matching path is found, then the term symbol which failed to match
 * with a trie node is set in the last argument.  If a path was found,
 * then the value of this argument is undefined.
 *
 * The TermStack and TrailStack are not cleared before returning control
 * to the caller.  Therefore, in the case of a failed lookup, these
 * stacks, together with the failed symbol, contain the remaining
 * information necessary to resume an insertion where the lookup
 * terminated.  In the case of success, the TrailStack contains the
 * variables appearing in the term set.
 *
 * This function is intended for internal use by the trie search and
 * lookup routines.
 */

void *var_trie_lookup(CTXTdeclc void *branchRoot, xsbBool *wasFound,
		      Cell *failedSymbol) {

  BTNptr parent;	/* Last node containing a matched symbol */

  Cell symbol = 0;	/* Trie representation of current heap symbol,
			   used for matching/inserting into a TSTN */

  int std_var_num;	/* Next available TrieVar index; for standardizing
			   variables when interned */
	
  int nodeType; /* as a parameter to ProcessNextSubtermFromTrieStacks */


#ifdef DEBUG_ASSERTIONS
  if ( IsNULL(BTN_Child((BTNptr)branchRoot)) )
    TrieError_InterfaceInvariant("var_trie_lookup");
#endif

  parent = branchRoot;
  std_var_num = Trail_NumBindings;
  while ( ! TermStack_IsEmpty ) {
#ifdef DEBUG_ASSERTIONS
    if ( IsLeafNode(parent) )
      TrieError_TooManyTerms("var_trie_lookup");
#endif
    ProcessNextSubtermFromTrieStacks(symbol,nodeType,std_var_num);
    {
      BTNptr chain;
      int chain_length;
      if ( IsHashHeader(BTN_Child(parent)) ) {
	BTHTptr ht = BTN_GetHashHdr(parent);
	chain = *CalculateBucketForSymbol(ht,symbol);
      }
      else
	chain = BTN_Child(parent);
      SearchChainForSymbol(chain,symbol,chain_length);
      if ( IsNonNULL(chain) )
	parent = chain;
      else {
	*wasFound = FALSE;
	*failedSymbol = symbol;
	return parent;
      }
    }
  }
#ifdef DEBUG_ASSERTIONS
  if ( ! IsLeafNode(parent) )
    TrieError_TooFewTerms("var_trie_lookup");
#endif
  *wasFound = TRUE;
  return parent;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 * Given a term in the form of an arity and vector of subterms, if the
 * term is present in the given trie, returns a pointer to the leaf
 * representing the term, otherwise returns NULL.
 *
 * If the term is found and if an array (a non-NULL pointer) is supplied
 * in the last argument, then the variables in the term are copied into
 * it, with the 0th element containing the (unencoded) count.  The ith
 * encountered variable is placed in array element i.
 * 
 * Routine used in meta-predicates such as get_call()
 */

void *variant_trie_lookup(CTXTdeclc void *trieRoot, int nTerms, CPtr termVector,
			  Cell varArray[]) {

  BTNptr trieNode;
  xsbBool wasFound;
  Cell symbol;


#ifdef DEBUG_ASSERTIONS
  if ( IsNULL(trieRoot) || (nTerms < 0) )
    TrieError_InterfaceInvariant("variant_trie_lookup()");
#endif

  if ( IsEmptyTrie((BTNptr)trieRoot) )
    return NULL;

  if ( nTerms > 0 ) {
    Trail_ResetTOS;
    TermStack_ResetTOS;
    TermStack_PushLowToHighVector(termVector,nTerms);
    trieNode = var_trie_lookup(CTXTc trieRoot,&wasFound,&symbol);
    if ( wasFound ) {
      if ( IsNonNULL(varArray) ) {
	int i;

	for ( i = 0;  i < (int) Trail_NumBindings;  i++ )
	  varArray[i+1] = (Cell)Trail_Base[i];
	varArray[0] = i;
      }
    }
    else
      trieNode = NULL;
    Trail_Unwind_All;
  }
  else {
    trieNode = trie_escape_lookup(trieRoot);
    if ( IsNonNULL(trieNode) && IsNonNULL(varArray) )
      varArray[0] = 0;
  }
  return trieNode;
}

/*=========================================================================*/

/*
 *		     Recursive Subsumptive Trie Lookup
 *		     =================================
 */


/*
 * Searches a branch of a NON-EMPTY trie for a path which subsumes a
 * given set of terms.  The branch is identified by a non-NULL pointer
 * to a trie node which roots the branch.  The terms, which are stored
 * on the Termstack, are compared against the symbols in the nodes lying
 * BELOW the given node.  If a subsuming path is discovered, then
 * returns a pointer to the leaf identifying the path, otherwise returns
 * NULL.  The last argument describes the relationship between the
 * discovered path, if any, and the given terms.
 *
 * In addition to the TermStack, the following structures should also be
 * primed for use: tstTermStackLog, tstTrail, TrieVarBindings[], and
 * VarEnumerator[].
 */

static BTNptr rec_sub_trie_lookup(CTXTdeclc BTNptr parent, TriePathType *pathType) {

  Cell subterm, symbol;
  BTNptr cur, match, var, leaf;
  int arity, trievar_index;
  CPtr args;


#ifdef DEBUG_ASSERTIONS
  if ( IsNULL(parent) )
    TrieError_InterfaceInvariant("rec_sub_trie_lookup");
#endif

  /*
   * Base Case:
   * ---------
   * We've paired a subterm with a term in the trie.  How this trie
   * term relates to the subterm will be set as we unroll the
   * recursion.  Return a handle for this trie term.
   */
  if ( TermStack_IsEmpty ) {
#ifdef DEBUG_ASSERTIONS
    if ( ! IsLeafNode(parent) )
      TrieError_TooFewTerms("rec_sub_trie_lookup");
    xsb_dbgmsg((LOG_TRIE, "Base case: empty TermStack"));
#endif
    return parent;
  }

#ifdef DEBUG_ASSERTIONS
  if ( IsLeafNode(parent) )
    TrieError_TooManyTerms("rec_sub_trie_lookup");
#endif

  /*
   * Recursive Case:
   * --------------
   * Find a pairing of the next subterm on the TermStack with a symbol
   * in the trie below 'parent.'  If one is found, then recurse.
   * Otherwise, signal the failure of the exploration of this branch
   * by returning NULL.
   */
  xsb_dbgmsg((LOG_TRIE, "Recursive case:"));
  TermStack_Pop(subterm);
  TermStackLog_PushFrame;
  XSB_Deref(subterm);
  if ( isref(subterm) ) {

    /* Handle Call Variables
       ===================== */

    if ( IsHashHeader(BTN_Child(parent)) )
      var = BTHT_BucketArray(BTN_GetHashHdr(parent))[TRIEVAR_BUCKET];
    else
      var = BTN_Child(parent);

    if ( ! IsStandardizedVariable(subterm) ) {
      xsb_dbgmsg((LOG_TRIE,"  Found new call variable: "));

      /*
       * Pair this free call variable with a new trie variable (there is at
       * most one of these among a set of child nodes).  Mark the call var to
       * indicate that it has already been seen, in case of repeated
       * occurrences in the call.  Bind the trie var to the call var and
       * trail them both.
       */
      for ( cur = var;  IsNonNULL(cur);  cur = BTN_Sibling(cur) )
	if ( IsTrieVar(BTN_Symbol(cur)) && IsNewTrieVar(BTN_Symbol(cur)) ) {
	  xsb_dbgmsg((LOG_TRIE, "  Binding it to free trievar"));
	  trievar_index = DecodeTrieVar(BTN_Symbol(cur));
	  /*** TrieVar_BindToSubterm(trievar_index,subterm); ***/
	  TrieVarBindings[trievar_index] = subterm;
	  Trail_Push(&TrieVarBindings[trievar_index]);
	  /*** CallVar_MarkIt(subterm,trievar_index); ***/
	  StandardizeVariable(subterm,trievar_index);
	  Trail_Push(subterm);
	  leaf = rec_sub_trie_lookup(CTXTc cur,pathType);
	  if ( IsNonNULL(leaf) ) {
	    if ( *pathType == NO_PATH )
	      *pathType = VARIANT_PATH;
	    return leaf;
	  }
	  else {
	  xsb_dbgmsg((LOG_TRIE, 
		     " Binding to free trievar didn't lead to valid path"));
	    Trail_PopAndReset;
	    Trail_PopAndReset;
	    break;
	  }
	}
      xsb_dbgmsg((LOG_TRIE, "No free trievar here"));
    }
    else {
      xsb_dbgmsg((LOG_TRIE, "  Found repeat call variable\n"
		 "  Option #1: Look to pair with repeat trie var"));
      /*
       * Option 1: Look for a nonlinear trie variable which has already been
       * --------  bound to this nonlinear call variable earlier in the
       *           search.  There may be more than one.
       */
      for ( cur = var;  IsNonNULL(cur);  cur = BTN_Sibling(cur) )
	if ( IsTrieVar(BTN_Symbol(cur)) &&
	     ! IsNewTrieVar(BTN_Symbol(cur)) ) {
	  trievar_index = DecodeTrieVar(BTN_Symbol(cur));
	  /***********************************************
	     could just compare
	         *(TrieVarBindings[trievar_index]) -to- subterm
		                  - OR -
		 TrieVarBindings[trievar_index]
		   -to- TrieVarBindings[IndexOfStdVar(subterm)]
	  ***********************************************/
	  if ( are_identical_terms(TrieVarBindings[trievar_index],
				   subterm) ) {
	    xsb_dbgmsg((LOG_TRIE, "  Found trivar with identical binding"));
	    leaf = rec_sub_trie_lookup(CTXTc cur,pathType);
	    if ( IsNonNULL(leaf) ) {
	      /*
	       * This may or may not be a variant path, depending on what has
	       * happened higher-up in the trie.  We therefore make a
	       * conservative "guess" and leave it to be determined at that
	       * point during the recursive unrolling.
	       */
	      if ( *pathType == NO_PATH )
		*pathType = VARIANT_PATH;
	      return leaf;
	    }
	    else
	      xsb_dbgmsg((LOG_TRIE, 
			 "  Pairing with identically bound trievar didn't lead"
			 " to valid path"));
	  }
	}
      /*
       * Option 2: Bind the nonlinear call variable with an unbound trie
       * --------  variable.  There is only one of these in a sibling set.
       */    
      xsb_dbgmsg((LOG_TRIE, 
		 "  Option #2: Bind new trievar to repeat call var"));
      for ( cur = var;  IsNonNULL(cur);  cur = BTN_Sibling(cur) )
	if ( IsTrieVar(BTN_Symbol(cur)) && IsNewTrieVar(BTN_Symbol(cur)) ) {
	  xsb_dbgmsg((LOG_TRIE, "    Found new trievar; binding it"));
	  trievar_index = DecodeTrieVar(BTN_Symbol(cur));
	  /*** TrieVar_BindToMarkedCallVar(trievar_index,subterm); ***/
	  TrieVarBindings[trievar_index] =
	    TrieVarBindings[IndexOfStdVar(subterm)];
	  Trail_Push(&TrieVarBindings[trievar_index]);
	  leaf = rec_sub_trie_lookup(CTXTc cur,pathType);
	  if ( IsNonNULL(leaf) ) {
	    *pathType = SUBSUMPTIVE_PATH;
	    return leaf;
	  }
	  else {
      xsb_dbgmsg((LOG_TRIE, 
		 "    Binding new trievar to repeat callvar didn't lead to"
		 " valid path"));
	    Trail_PopAndReset;
	    break;
	  }
	}
    }
  }
  else {

    /* Handle NonVariable Subterms
       =========================== */
    /*
     * The following should trie-encode the first symbol of subterm and
     * record any recursive components of subterm (for reconstitution later,
     * if needed).
     */
    if ( isconstant(subterm) ) {      /* XSB_INT, XSB_FLOAT, XSB_STRING */
      xsb_dbgmsg((LOG_TRIE, "  Found constant"));
      symbol = EncodeTrieConstant(subterm);
      arity = 0;
      args = NULL;
    }
    else if ( isconstr(subterm) ) {   /* XSB_STRUCT */
      xsb_dbgmsg((LOG_TRIE, "  Found structure"));
      symbol = EncodeTrieFunctor(subterm);
      arity = get_arity((Psc)*clref_val(subterm));
      args = clref_val(subterm) + 1;
    }
    else if ( islist(subterm) ) {     /* XSB_LIST */
      xsb_dbgmsg((LOG_TRIE, "  Found list"));
      symbol = EncodeTrieList(subterm);
      arity = 2;
      args = clref_val(subterm);
    }
    else {
      Trail_Unwind_All;
      xsb_abort("rec_sub_trie_lookup(): bad tag");
      *pathType = NO_PATH;
      return NULL;
    }

    /*
     * Determine the node chains below 'parent' where 'symbol' and trie
     * variables may exist.
     */
    if ( IsHashHeader(BTN_Child(parent)) ) {
      BTNptr *buckets;
      BTHTptr ht;

      ht = BTN_GetHashHdr(parent);
      buckets = BTHT_BucketArray(ht);
      match = buckets[TrieHash(symbol,BTHT_GetHashSeed(ht))];
      var = buckets[TRIEVAR_BUCKET];
    }
    else  /* the children are arranged as a simple chain of nodes */
      var = match = BTN_Child(parent);

    /*
     * Option 1: Look for an identical symbol in the trie.  There is at most
     * --------  one of these among a set of child nodes.
     */
    xsb_dbgmsg((LOG_TRIE, "  Nonvar Option #1: Find matching symbol in trie"));
    for ( cur = match;  IsNonNULL(cur);  cur = BTN_Sibling(cur) )
      if ( symbol == BTN_Symbol(cur) ) {
	int origTermStackTopIndex = TermStack_Top - TermStack_Base;
	xsb_dbgmsg((LOG_TRIE, "  Found matching trie symbol"));
	TermStack_PushLowToHighVector(args,arity);
	leaf = rec_sub_trie_lookup(CTXTc cur,pathType);
	if ( IsNonNULL(leaf) ) {
	  if ( *pathType == NO_PATH )
	    *pathType = VARIANT_PATH;
	  return leaf;
	}
	else {
	  /* Could not successfully continue from this match, so try another
	     pairing, performed below.  Throw away whatever was pushed onto
	     the TermStack above by resetting the top-of-stack pointer. */
	  xsb_dbgmsg((LOG_TRIE, 
		     "  Matching trie symbol didn't lead to valid path"));
	  TermStack_SetTOS(origTermStackTopIndex);
	  break;
	}
      }

    /*
     * Option 2: Look for a trie variable which has already been bound to
     * --------  an identical symbol during this process.
     */
    xsb_dbgmsg((LOG_TRIE, 
	       "  Nonvar Option #2: Match with previously bound trievar"));
    for ( cur = var;  IsNonNULL(cur);  cur = BTN_Sibling(cur) )
      if ( IsTrieVar(BTN_Symbol(cur)) && ! IsNewTrieVar(BTN_Symbol(cur)) ) {
	trievar_index = DecodeTrieVar(BTN_Symbol(cur));
	if ( are_identical_terms(TrieVarBindings[trievar_index],
				    subterm) ) {
	  xsb_dbgmsg((LOG_TRIE, "  Found trievar bound to matching symbol"));
	  leaf = rec_sub_trie_lookup(CTXTc cur,pathType);
	  if ( IsNonNULL(leaf) ) {
	    *pathType = SUBSUMPTIVE_PATH;
	    return leaf;
	  }
	  else
	    xsb_dbgmsg((LOG_TRIE, "  Bound trievar didn't lead to valid path"));
	}
      }

    /*
     * Option 3: Bind the symbol with an unbound trie variable.
     * --------
     */
    xsb_dbgmsg((LOG_TRIE, "  Nonvar Option #3: Bind free trievar to symbol"));
    for ( cur = var;  IsNonNULL(cur);  cur = BTN_Sibling(cur) )
      if ( IsTrieVar(BTN_Symbol(cur)) && IsNewTrieVar(BTN_Symbol(cur)) ) {
	xsb_dbgmsg((LOG_TRIE, "  Binding free trievar to symbol"));
	trievar_index = DecodeTrieVar(BTN_Symbol(cur));
	/*** TrieVar_BindToSubterm(trievar_index,subterm); ***/
	TrieVarBindings[trievar_index] = subterm;
	Trail_Push(&TrieVarBindings[trievar_index]);
	leaf = rec_sub_trie_lookup(CTXTc cur,pathType);
	if ( IsNonNULL(leaf) ) {
	  *pathType = SUBSUMPTIVE_PATH;
	  return leaf;
	}
	else {
	  /* Remove the binding from the variable created above, exit the
             loop, and drop through to fail; this was our last option.  Note
             that there is only one unbound trie variable per sibling set. */
	  xsb_dbgmsg((LOG_TRIE, 
		     "Binding free trievar to symbol didn't lead to "
		     "valid path"));
	  Trail_PopAndReset;
	  break;
	}
      }
  }

  /* Nothing worked, so fail.  Make stacks same as when this was called. */
  xsb_dbgmsg((LOG_TRIE, "All options failed!"));
  TermStackLog_PopAndReset;
  *pathType = NO_PATH;
  return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 * Given a term as an arity and array of subterms, determines whether
 * there exists a subsuming path in the given trie.  A pointer to the
 * leaf of the discovered path, if any, is returned, and a flag is set
 * to indicate how the path relates to the subterm.
 * 
 * Used in get_producer_call/3 and other Prolog routines.
 */

void *subsumptive_trie_lookup(CTXTdeclc void *trieRoot, int nTerms, CPtr termVector,
			      TriePathType *path_type, Cell subtermArray[]) {

  BTNptr leaf;


#ifdef DEBUG_ASSERTIONS
  if ( IsNULL(trieRoot) || (nTerms < 0) )
    TrieError_InterfaceInvariant("subsumptive_trie_lookup()");
#endif

  *path_type = NO_PATH;
  if ( IsEmptyTrie((BTNptr)trieRoot) )
    return NULL;

  if ( nTerms > 0) {
    Trail_ResetTOS;
    TermStackLog_ResetTOS;
    TermStack_ResetTOS;
    TermStack_PushLowToHighVector(termVector,nTerms);
    leaf = rec_sub_trie_lookup(CTXTc trieRoot, path_type);
    if ( IsNonNULL(leaf) && IsNonNULL(subtermArray) ) {
      int i;
      for ( i = 0;
	    TrieVarBindings[i] != (Cell) (& TrieVarBindings[i]);
	    i++ )
	subtermArray[i+1] = TrieVarBindings[i];
      subtermArray[0] = i;
    }
    Trail_Unwind_All;
    dbg_printTriePathType(LOG_TRIE, stddbg, *path_type, leaf);
  }
  else {
    leaf = trie_escape_lookup(trieRoot);
    if ( IsNonNULL(leaf) ) {
      *path_type = VARIANT_PATH;
      if ( IsNonNULL(subtermArray) )
	subtermArray[0] = 0;
    }
  }
  return leaf;
}

/*=========================================================================*/
