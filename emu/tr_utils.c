/* File:      tr_utils.c
** Author(s): Prasad Rao, Juliana Freire, Kostis Sagonas
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
#include <stdlib.h>

#include "xsb_config.h"
#include "xsb_debug.h"

/* Special debug includes */
#include "debugs/debug_tries.h"


#include "auxlry.h"
#include "cell_xsb.h"
#include "cinterf.h"
#include "binding.h"
#include "psc_xsb.h"
#include "heap_xsb.h"
#include "memory_xsb.h"
#include "register.h"
#include "deref.h"
#include "flags_xsb.h"
#include "trie_internals.h"
#include "tst_aux.h"
#if (!defined(WAM_TRAIL))
#include "cut_xsb.h"
#endif
#include "macro_xsb.h"
#include "sw_envs.h"
#include "choice.h"
#include "inst_xsb.h"
#include "error_xsb.h"
#include "io_builtins_xsb.h"
#include "trassert.h"
#include "tr_utils.h"
#include "tst_utils.h"
#ifdef CHAT
#include "chat.h"
#endif

/*----------------------------------------------------------------------*/

#define MAX_VAR_SIZE	200

#ifdef DEBUG
extern void printterm(Cell, byte, int);
#endif

#include "ptoc_tag_xsb_i.h"
#include "term_psc_xsb_i.h"

/*----------------------------------------------------------------------*/

xsbBool has_unconditional_answers(VariantSF subg)
{
  ALNptr node_ptr = subg_answers(subg);
 
  /* Either subgoal has no answers or it is completed */
  /* and its answer list has already been reclaimed. */
  /* In either case, the result is immediately obtained. */
 
  if (node_ptr <= COND_ANSWERS) return (node_ptr == UNCOND_ANSWERS);
 
  /* If the subgoal has not been completed, or is early completed but its */
  /* answer list has not been reclaimed yet, check each of its nodes. */
 
  while (node_ptr) {
    if (is_unconditional_answer(ALN_Answer(node_ptr))) return TRUE;
    node_ptr = ALN_Next(node_ptr);
  }
  return FALSE;
}

/*----------------------------------------------------------------------*/

/*
 * Given a term as an arity and array of subterms, determines whether
 * this term is present in the given trie.  If an array is supplied,
 * the variables in the term are copied into it, with the 0th element
 * containing the count.  The leaf representing the term is returned
 * if present, or NULL otherwise.
 */

BTNptr variant_trie_lookup(int nTerms, CPtr termVector, BTNptr trieRoot,
			   Cell varArray[]) {

  BTNptr trieNode;      /* Used for stepping down through the trie */

  Cell symbol;		/* Trie representation of current heap symbol,
			   used for matching/inserting into a TSTN */

  Cell subterm;		/* Used for stepping through the term */

  int std_var_num;	/* Next available TrieVar index; for standardizing
			   variables when interned */


  if ( IsNULL(trieRoot) || IsNULL(BTN_Child(trieRoot)) )
    return NULL;

  else if ( nTerms > 0) {
    trieNode = trieRoot;
    std_var_num = 0;

    Trail_ResetTOS;
    TermStack_ResetTOS;
    TermStack_PushLowToHighVector(termVector,nTerms);

    while ( ! TermStack_IsEmpty && IsNonNULL(trieNode) ) {
      subterm = TermStack_Pop;
      XSB_Deref(subterm);
      switch (cell_tag(subterm)) {

      case XSB_REF:
      case XSB_REF1:
	if ( ! IsStandardizedVariable(subterm) ) {
	  StandardizeVariable(subterm, std_var_num);
	  Trail_Push(subterm);
	  symbol = EncodeNewTrieVar(std_var_num);
	  std_var_num++;
	}
	else
	  symbol = EncodeTrieVar(IndexOfStdVar(subterm));
	break;

      case XSB_STRING:
      case XSB_INT:
      case XSB_FLOAT:
	symbol = EncodeTrieConstant(subterm);
	break;

      case XSB_STRUCT:
	symbol = EncodeTrieFunctor(subterm);
	TermStack_PushFunctorArgs(subterm);
	break;

      case XSB_LIST:
	symbol = EncodeTrieList(subterm);
	TermStack_PushListArgs(subterm);
	break;

      default: 
	/* Bad tag Error */
	trieNode = NULL;
	continue;
      }

      if ( IsHashHeader(BTN_Child(trieNode)) ) {
	BTHTptr ht = BTN_GetHashHdr(trieNode);
	trieNode = *CalculateBucketForSymbol(ht,symbol);
      }
      else
	trieNode = BTN_Child(trieNode);
      {
	int chain_length;
	SearchChainForSymbol(trieNode,symbol,chain_length);
      }
    }

    if ( IsNonNULL(varArray) ) {
      int i;

      varArray[0] = tstTrail.top - tstTrail.base;
      for ( i = 1; (byte)i <= varArray[0]; i++ )
	varArray[i] = (Cell)tstTrail.base[i - 1];
    }

    Trail_Unwind_All;
    return trieNode;
  }

  else if ( IsEscapeNode(BTN_Child(trieRoot)) ) {
    if ( IsNonNULL(varArray) )
      varArray[0] = 0;
    return BTN_Child(trieRoot);
  }

  else {  /* Error conditions */
    if ( nTerms == 0 )
      /* Malformed trie or nTerms is wrong */
      return NULL;
    else
      /* nTerms is less than 0 */
      return NULL;
  }
}

/*-----------------------------------------------------------------------*/

/*  #define DEBUG_TRIE_LOOKUP */

#ifdef DEBUG_TRIE_LOOKUP
static void printTriePathType(TriePathType type, BTNptr leaf) {

  switch (type) {
  case NO_PATH:
    printf("No path found :-(\n");
    break;
  case VARIANT_PATH:
    printf("Variant path found: ");
    triePrintPath(leaf,FALSE);
    break;
  case SUBSUMPTIVE_PATH:
    printf("Subsumptive path found: ");
    triePrintPath(leaf,FALSE);
    break;
  default:
    printf("What kind of path? (%d)\n", type);
    break;
  }
}
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 * Searches the trie branch below a given node for a path which subsumes
 * a given subterm.  If such a path is discovered, then returns a
 * pointer to the leaf identifying the path, otherwise returns NULL.
 * Also indicates in a flag whether a) the path is actually a variant of
 * the subterm, b) the path properly subsumes the subterm, or c) no path
 * was found at all.
 *
 * Assumes that the subterm has been pushed onto the tstTermStack, and
 * that the following data structures have been primed for use:
 * tstTermStackLog, tstTrail, TrieVarBindings[], and VarEnumerator[].
 */

static BTNptr sub_trie_lookup(BTNptr parent, TriePathType *pathType) {

  Cell subterm, symbol;
  BTNptr cur, match, var, leaf;
  int arity, trievar_index;
  CPtr args;
  extern xsbBool are_identical_subterms(Cell,Cell);

  /*
   * Base Case:
   * ---------
   * We've paired a subterm with a term in the trie.  How this trie
   * term relates to the subterm will be set as we unroll the
   * recursion.  Return a handle for this trie term.
   */
  if ( TermStack_IsEmpty ) {
#ifdef DEBUG_TRIE_LOOKUP
    printf("Base case: empty TermStack\n");
#endif
    return parent;
  }

  /*
   * Recursive Case:
   * --------------
   * Find a pairing of the next subterm on the TermStack with a symbol
   * in the trie below 'parent.'  If one is found, then recurse.
   * Otherwise, signal the failure of the exploration of this branch
   * by returning NULL.
   */
#ifdef DEBUG_TRIE_LOOKUP
  printf("Recursive case:\n");
#endif
  subterm = TermStack_Pop;
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
#ifdef DEBUG_TRIE_LOOKUP
      printf("  Found new call variable: ");
#endif

      /*
       * Pair this free call variable with a new trie variable (there is at
       * most one of these among a set of child nodes).  Mark the call var to
       * indicate that it has already been seen, in case of repeated
       * occurrences in the call.  Bind the trie var to the call var and
       * trail them both.
       */
      for ( cur = var;  IsNonNULL(cur);  cur = BTN_Sibling(cur) )
	if ( IsTrieVar(BTN_Symbol(cur)) && IsNewTrieVar(BTN_Symbol(cur)) ) {
#ifdef DEBUG_TRIE_LOOKUP
	  printf("  Binding it to free trievar\n");
#endif
	  trievar_index = DecodeTrieVar(BTN_Symbol(cur));
	  /*** TrieVar_BindToSubterm(trievar_index,subterm); ***/
	  TrieVarBindings[trievar_index] = subterm;
	  Trail_Push(&TrieVarBindings[trievar_index]);
	  /*** CallVar_MarkIt(subterm,trievar_index); ***/
	  StandardizeVariable(subterm,trievar_index);
	  Trail_Push(subterm);
	  leaf = sub_trie_lookup(cur,pathType);
	  if ( IsNonNULL(leaf) ) {
	    if ( *pathType == NO_PATH )
	      *pathType = VARIANT_PATH;
	    return leaf;
	  }
	  else {
#ifdef DEBUG_TRIE_LOOKUP
	  printf(" Binding to free trievar didn't lead to valid path\n");
#endif
	    Trail_PopAndReset;
	    Trail_PopAndReset;
	    break;
	  }
	}
#ifdef DEBUG_TRIE_LOOKUP
      printf("No free trievar here\n");
#endif
    }
    else {
#ifdef DEBUG_TRIE_LOOKUP
      printf("  Found repeat call variable\n"
	     "  Option #1: Look to pair with repeat trie var\n");
#endif
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
	  if ( are_identical_subterms(TrieVarBindings[trievar_index],
				      subterm) ) {
#ifdef DEBUG_TRIE_LOOKUP
	    printf("  Found trivar with identical binding\n");
#endif
	    leaf = sub_trie_lookup(cur,pathType);
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
#ifdef DEBUG_TRIE_LOOKUP
	    else
	      printf("  Pairing with identically bound trievar didn't lead to valid path\n");
#endif
	  }
	}
      /*
       * Option 2: Bind the nonlinear call variable with an unbound trie
       * --------  variable.  There is only one of these in a sibling set.
       */    
#ifdef DEBUG_TRIE_LOOKUP
      printf("  Option #2: Bind new trievar to repeat call var\n");
#endif
      for ( cur = var;  IsNonNULL(cur);  cur = BTN_Sibling(cur) )
	if ( IsTrieVar(BTN_Symbol(cur)) && IsNewTrieVar(BTN_Symbol(cur)) ) {
#ifdef DEBUG_TRIE_LOOKUP
      printf("    Found new trievar; binding it\n");
#endif
	  trievar_index = DecodeTrieVar(BTN_Symbol(cur));
	  /*** TrieVar_BindToMarkedCallVar(trievar_index,subterm); ***/
	  TrieVarBindings[trievar_index] =
	    TrieVarBindings[IndexOfStdVar(subterm)];
	  Trail_Push(&TrieVarBindings[trievar_index]);
	  leaf = sub_trie_lookup(cur,pathType);
	  if ( IsNonNULL(leaf) ) {
	    *pathType = SUBSUMPTIVE_PATH;
	    return leaf;
	  }
	  else {
#ifdef DEBUG_TRIE_LOOKUP
      printf("    Binding new trievar to repeat callvar didn't lead to valid path\n");
#endif
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
#ifdef DEBUG_TRIE_LOOKUP
      printf("  Found constant\n");
#endif
      symbol = EncodeTrieConstant(subterm);
      arity = 0;
      args = NULL;
    }
    else if ( isconstr(subterm) ) {   /* XSB_STRUCT */
#ifdef DEBUG_TRIE_LOOKUP
      printf("  Found structure\n");
#endif
      symbol = EncodeTrieFunctor(subterm);
      arity = get_arity((Psc)*clref_val(subterm));
      args = clref_val(subterm) + 1;
    }
    else if ( islist(subterm) ) {     /* XSB_LIST */
#ifdef DEBUG_TRIE_LOOKUP
      printf("  Found list\n");
#endif
      symbol = EncodeTrieList(subterm);
      arity = 2;
      args = clref_val(subterm);
    }
    else {
      Trail_Unwind_All;
      xsb_abort("sub_trie_lookup(): bad tag");
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
#ifdef DEBUG_TRIE_LOOKUP
    printf("  Nonvar Option #1: Find matching symbol in trie\n");
#endif
    for ( cur = match;  IsNonNULL(cur);  cur = BTN_Sibling(cur) )
      if ( symbol == BTN_Symbol(cur) ) {
	CPtr origTermStackTop = tstTermStack.top;
#ifdef DEBUG_TRIE_LOOKUP
	printf("  Found matching trie symbol\n");
#endif
	TermStack_PushLowToHighVector(args,arity);
	leaf = sub_trie_lookup(cur,pathType);
	if ( IsNonNULL(leaf) ) {
	  if ( *pathType == NO_PATH )
	    *pathType = VARIANT_PATH;
	  return leaf;
	}
	else {
#ifdef DEBUG_TRIE_LOOKUP
	  printf("  Matching trie symbol didn't lead to valid path\n");
#endif
	  tstTermStack.top = origTermStackTop;
	  break;
	}
      }

    /*
     * Option 2: Look for a trie variable which has already been bound to
     * --------  an identical symbol during this process.
     */

#ifdef DEBUG_TRIE_LOOKUP
    printf("  Nonvar Option #2: Match with previously bound trievar\n");
#endif
    for ( cur = var;  IsNonNULL(cur);  cur = BTN_Sibling(cur) )
      if ( IsTrieVar(BTN_Symbol(cur)) && ! IsNewTrieVar(BTN_Symbol(cur)) ) {
	trievar_index = DecodeTrieVar(BTN_Symbol(cur));
	if ( are_identical_subterms(TrieVarBindings[trievar_index],
				    subterm) ) {
#ifdef DEBUG_TRIE_LOOKUP
	  printf("  Found trievar bound to matching symbol\n");
#endif
	  leaf = sub_trie_lookup(cur,pathType);
	  if ( IsNonNULL(leaf) ) {
	    *pathType = SUBSUMPTIVE_PATH;
	    return leaf;
	  }
#ifdef DEBUG_TRIE_LOOKUP
	  else
	    printf("  Bound trievar didn't lead to valid path\n");
#endif
	}
      }

    /*
     * Option 3: Bind the symbol with an unbound trie variable.
     * --------
     */
#ifdef DEBUG_TRIE_LOOKUP
    printf("  Nonvar Option #3: Bind free trievar to symbol\n");
#endif
    for ( cur = var;  IsNonNULL(cur);  cur = BTN_Sibling(cur) )
      if ( IsTrieVar(BTN_Symbol(cur)) && IsNewTrieVar(BTN_Symbol(cur)) ) {
#ifdef DEBUG_TRIE_LOOKUP
	printf("  Binding free trievar to symbol\n");
#endif
	trievar_index = DecodeTrieVar(BTN_Symbol(cur));
	/*** TrieVar_BindToSubterm(trievar_index,subterm); ***/
	TrieVarBindings[trievar_index] = subterm;
	Trail_Push(&TrieVarBindings[trievar_index]);
	leaf = sub_trie_lookup(cur,pathType);
	if ( IsNonNULL(leaf) ) {
	  *pathType = SUBSUMPTIVE_PATH;
	  return leaf;
	}
	else {
#ifdef DEBUG_TRIE_LOOKUP
	  printf("Binding free trievar to symbol didn't lead to valid path\n");
#endif
	  Trail_PopAndReset;
	  break;  /* only one unbound trie variable per sibling set */
	}
      }
  }

  /* Nothing worked, so fail.  Make stacks same as when this was called. */
#ifdef DEBUG_TRIE_LOOKUP
    printf("All options failed!\n");
#endif
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
 */

BTNptr subsumptive_trie_lookup(int nTerms, CPtr termVector, BTNptr trieRoot,
			       TriePathType *path_type) {

  BTNptr leaf;

  *path_type = NO_PATH;
  if ( IsNULL(trieRoot) || IsNULL(BTN_Child(trieRoot)) )
    return NULL;

  else if ( nTerms > 0) {
    Trail_ResetTOS;
    TermStackLog_ResetTOS;
    TermStack_ResetTOS;
    TermStack_PushLowToHighVector(termVector,nTerms);
    leaf = sub_trie_lookup(trieRoot, path_type);
    Trail_Unwind_All;
#ifdef DEBUG_TRIE_LOOKUP
    printTriePathType(*path_type, leaf);
#endif
    return leaf;
  }
  else {
    /* Error in function call */
    return NULL;
  }
}

/*----------------------------------------------------------------------*/

VariantSF get_subgoal_ptr(Cell callTerm, TIFptr pTIF) {

  int arity;
  BTNptr call_trie_leaf;

  arity = get_arity(term_psc(callTerm));
  call_trie_leaf = variant_trie_lookup(arity, clref_val(callTerm) + 1,
				       TIF_CallTrie(pTIF), NULL);
  if ( IsNonNULL(call_trie_leaf) )
    return CallTrieLeaf_GetSF(call_trie_leaf);
  else
    return NULL;
}

/*----------------------------------------------------------------------*/

/*
 * Given a vector of terms and their number, N, builds a ret/N structure
 * on the heap containing those terms.  Returns this constructed term.
 */

Cell build_ret_term(int arity, Cell termVector[]) {

  Pair sym;
  CPtr ret_term;
  int  i, new;

  if ( arity == 0 )
    return makestring(ret_psc[0]);  /* return as a term */
  else {
    ret_term = hreg;  /* pointer to where ret(..) will be built */
    sym = insert("ret", (byte)arity, (Psc)flags[CURRENT_MODULE], &new);
    new_heap_functor(hreg, pair_psc(sym));
    for ( i = 0; i < arity; i++ )
      nbldval(termVector[i]);
    return makecs(ret_term);  /* return as a term */
  }
}

/*----------------------------------------------------------------------*/

/*
 * Create the answer template for a subsumed call with the given producer.
 * The template is stored in an array supplied by the caller.
 */

void construct_answer_template(Cell callTerm, SubProdSF producer,
			       Cell template[]) {

  Cell subterm, symbol;
  int  i;

  /*
   * Store the symbols along the path of the more general call.
   */
  SymbolStack_ResetTOS;
  SymbolStack_PushPath(subg_leaf_ptr(producer));

  /*
   * Push the arguments of the subsumed call.
   */
  TermStack_ResetTOS;
  TermStack_PushFunctorArgs(callTerm);

  /*
   * Create the answer template while we process.  Since we know we have a
   * more general subsuming call, we can greatly simplify the "matching"
   * process: we know we either have exact matches of non-variable symbols
   * or a variable paired with some subterm of the current call.
   */
  i = 1;
  while ( ! TermStack_IsEmpty ) {
    subterm = TermStack_Pop;
    XSB_Deref(subterm);
    symbol = SymbolStack_Pop;
    if ( IsTrieVar(symbol) && IsNewTrieVar(symbol) ) {
      template[i] = subterm;
      i++;
    }
    else if ( IsTrieFunctor(symbol) )
      TermStack_PushFunctorArgs(subterm)
    else if ( IsTrieList(symbol) )
      TermStack_PushListArgs(subterm)
  }
  template[0] = i - 1;
}

/*----------------------------------------------------------------------*/

/*
 * Given a term representing a tabled call, determine whether it is
 * recorded in the Call Table.  If it is, then return a pointer to its
 * subgoal frame and construct on the heap the answer template required
 * to retrieve answers for this call.  Place a reference to this term in
 * the location pointed to by the second argument.
 */

VariantSF get_call(Cell callTerm, Cell *retTerm) {

  Psc  psc;
  TIFptr tif;
  int arity;
  BTNptr call_trie_leaf;
  VariantSF sf;
  Cell callVars[MAX_VAR_SIZE + 1];


  psc = term_psc(callTerm);
  if ( IsNULL(psc) ) {
    err_handle(TYPE, 1, "get_call", 3, "callable term", callTerm);
    return NULL;
  }
  tif = get_tip(psc);
  if ( IsNULL(tif) )
    xsb_abort("Predicate %s/%d is not tabled", get_name(psc), get_arity(psc));

  arity = get_arity(term_psc(callTerm));
  call_trie_leaf = variant_trie_lookup(arity, clref_val(callTerm) + 1,
				       TIF_CallTrie(tif), callVars);
  if ( IsNULL(call_trie_leaf) )
    return NULL;
  else {
    sf = CallTrieLeaf_GetSF(call_trie_leaf);
    if ( IsProperlySubsumed(sf) )
      construct_answer_template(callTerm, conssf_producer(sf), callVars);
    *retTerm = build_ret_term(callVars[0],&callVars[1]);
    return sf;
  }
}

/*======================================================================*/

/*
 *                     D E L E T I N G   T R I E S
 *                     ===========================
 */


/* Stack for top-down traversing and freeing components of a trie
   -------------------------------------------------------------- */
struct freeing_stack_node{
  BTNptr item;
  struct freeing_stack_node *next;
};

#define push_node(node){\
     struct freeing_stack_node *temp;\
     temp = (struct freeing_stack_node *)malloc(sizeof(struct freeing_stack_node));\
     if (temp == NULL){\
       xsb_exit("Out of Memory");\
     } else {\
       temp->next   = node_stk_top;\
       temp->item   = node;\
       node_stk_top = temp;\
     }\
}

#define pop_node(node){\
    struct freeing_stack_node *temp;\
    if (node_stk_top == NULL) {\
       xsb_dbgmsg("DELETE_PREDICATE_TABLE: pop attempted from NULL");\
       return;\
    }\
    node         = node_stk_top->item;\
    temp         = node_stk_top;\
    node_stk_top = node_stk_top->next;\
    free(temp);\
}


static void free_trie_ht(BTHTptr ht) {

  TrieHT_RemoveFromAllocList(*smBTHT,ht);
  free(BTHT_BucketArray(ht));
  SM_DeallocateStruct(*smBTHT,ht);
}


static void delete_variant_table(BTNptr x) {

  struct freeing_stack_node *node_stk_top = 0, *call_nodes_top = 0;
  BTNptr node, rnod, *Bkp; 
  BTHTptr ht;


  if ( IsNULL(x) )
    return;

  push_node(x);
  while (node_stk_top != 0) {
    pop_node(node);
    if ( IsHashHeader(node) ) {
      ht = (BTHTptr) node;
      for (Bkp = BTHT_BucketArray(ht);
	   Bkp < BTHT_BucketArray(ht) + BTHT_NumBuckets(ht);
	   Bkp++) {
	if ( IsNonNULL(*Bkp) )
	  push_node(*Bkp);
      }
      free_trie_ht(ht);
    }
    else {
      if ( IsNonNULL(Sibl(node)) )
	push_node(Sibl(node));
      if ( IsNonNULL(BTN_Child(node)) ) {
	if ( IsLeafNode(node) ) {
	  /*
	   * Remove the subgoal frame and its dependent structures
	   */
	  VariantSF pSF = CallTrieLeaf_GetSF(node);
	  if ( IsNonNULL(subg_ans_root_ptr(pSF)) ) {
	    call_nodes_top = node_stk_top;
	    push_node((BTNptr)subg_ans_root_ptr(pSF));
	    while (node_stk_top != call_nodes_top) {
	      pop_node(rnod);
	      if ( IsHashHeader(rnod) ) {
		ht = (BTHTptr) rnod;
		for (Bkp = BTHT_BucketArray(ht);
		     Bkp < BTHT_BucketArray(ht) + BTHT_NumBuckets(ht);
		     Bkp++) {
		  if ( IsNonNULL(*Bkp) )
		    push_node(*Bkp);
		}
		free_trie_ht(ht);
	      }
	      else {
		if (Sibl(rnod)) 
		  push_node(Sibl(rnod));
		if ( ! IsLeafNode(rnod) )
		  push_node(Child(rnod));
		SM_DeallocateStruct(*smBTN,rnod);
	      }
	    }
	  } /* free answer trie */
	  free_answer_list(pSF);
	  FreeProducerSF(pSF);
	} /* is leaf */
	else 
	  push_node(Child(node));
      } /* there is a child of "node" */
      SM_DeallocateStruct(*smBTN,node);
    }
  }
}

/*----------------------------------------------------------------------*/
/* Delete the table for a given tabled predicate, specified as a TIF    */
/*----------------------------------------------------------------------*/

void delete_predicate_table(TIFptr tif) {

  if ( IsVariantPredicate(tif) )
    delete_variant_table(TIF_CallTrie(tif));
  else
    delete_subsumptive_table(tif);
  TIF_CallTrie(tif) = NULL;
  TIF_Subgoals(tif) = NULL;
}

/*----------------------------------------------------------------------*/

static int is_hash(BTNptr x) 
{
  if( x == NULL)
    return(0);
  else
    return( IsHashHeader(x) );
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 * Set values for "parent" -- the parent node of "current" -- and
 * "cur_hook" -- an address containing a pointer into "current"'s level
 * in the trie.  If there is no parent node, use the value of
 * "root_hook" to find the level.  If the hook is actually contained in
 * the parent of current (as its child field), then we've ascended as
 * far as we need to go.  Set parent to NULL to indicate this.
 */

static void set_parent_and_node_hook(BTNptr current, BTNptr *root_hook,
				     BTNptr *parent, BTNptr **cur_hook) {

  BTNptr par;

  if ( IsTrieRoot(current) )  /* defend against root having a set parent field */
    par = NULL;
  else {
    par = BTN_Parent(current);
    if ( IsNonNULL(par) && (root_hook == &BTN_Child(par)) )
      par = NULL;    /* stop ascent when hooking node is reached */
  }
  if ( IsNULL(par) )
    *cur_hook = root_hook;
  else
    *cur_hook = &BTN_Child(par);
  *parent = par;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 * Given some non-root node which is *not the first* (or only) sibling,
 * find the node which precedes it in the chain.  Should ONLY be used
 * when deleting trie components.  If a hash table is encountered, then
 * its number of contents is decremented.
 */
static BTNptr get_prev_sibl(BTNptr node)
{
  BTNptr sibling_chain;

  sibling_chain = BTN_Child(BTN_Parent(node));
  if ( IsHashHeader(sibling_chain) ) {
    BTHTptr ht = (BTHTptr)sibling_chain;
    BTHT_NumContents(ht)--;
    sibling_chain = *CalculateBucketForSymbol(ht,BTN_Symbol(node));
  }
  while(sibling_chain != NULL){
    if (BTN_Sibling(sibling_chain) == node)
      return(sibling_chain);
    sibling_chain = BTN_Sibling(sibling_chain);
  }  
  xsb_abort("Error in get_previous_sibling");
  return(NULL);
}

/*----------------------------------------------------------------------*/
/* deletes and reclaims a whole branch in the return trie               */
/*----------------------------------------------------------------------*/

/*
 * Delete a branch in the trie down from node `lowest_node_in_branch'
 * up to the level pointed to by the hook location, as pointed to by
 * `hook'.  Under normal use, the "hook" is either for the root of the
 * trie, or for the first level of the trie (is a pointer to the child
 * field of the root).
 */

void delete_branch(BTNptr lowest_node_in_branch, BTNptr *hook) {

  int num_left_in_hash;
  BTNptr prev, parent_ptr, *y1, *z;


  while ( IsNonNULL(lowest_node_in_branch) && 
	  ( Contains_NOCP_Instr(lowest_node_in_branch) ||
	    IsTrieRoot(lowest_node_in_branch) ) ) {
    /*
     *  Walk up a path with no branches, i.e., the nodes along this path
     *  have no siblings.  We know this because the instruction in the
     *  node is of the no_cp variety.
     */
    set_parent_and_node_hook(lowest_node_in_branch,hook,&parent_ptr,&y1);
    if (is_hash(*y1)) {
      z = CalculateBucketForSymbol((BTHTptr)(*y1),
				   BTN_Symbol(lowest_node_in_branch));
      if ( *z != lowest_node_in_branch )
	xsb_dbgmsg("DELETE_BRANCH: trie node not found in hash table");
      *z = NULL;
      num_left_in_hash = --BTHT_NumContents((BTHTptr)*y1);
      if (num_left_in_hash  > 0) {
	/*
	 * `lowest_node_in_branch' has siblings, even though they are not in
	 * the same chain.  Therefore we cannot delete the parent, and so
	 * we're done.
	 */
	SM_DeallocateStruct(*smBTN,lowest_node_in_branch);
	return;
      }
      else
	free_trie_ht((BTHTptr)(*y1));
    }
    /*
     *  Remove this node and continue.
     */
    SM_DeallocateStruct(*smBTN,lowest_node_in_branch);
    lowest_node_in_branch = parent_ptr;
  }
  if (lowest_node_in_branch == NULL)
    *hook = 0;
  else {
    if (Contains_TRY_Instr(lowest_node_in_branch)) {
      /* Alter sibling's instruction:  trust -> no_cp  retry -> try */
      Instr(Sibl(lowest_node_in_branch)) =
	Instr(Sibl(lowest_node_in_branch)) -1;
      y1 = &BTN_Child(BTN_Parent(lowest_node_in_branch));
      if (is_hash(*y1)) {
	z = CalculateBucketForSymbol((BTHTptr)(*y1),
				     BTN_Symbol(lowest_node_in_branch));
	num_left_in_hash = --BTHT_NumContents((BTHTptr)*y1);
      }
      else
	z = y1;
      *z = Sibl(lowest_node_in_branch);      
    }
    else { /* not the first in the sibling chain */
      prev = get_prev_sibl(lowest_node_in_branch);      
      Sibl(prev) = Sibl(lowest_node_in_branch);
      if (Contains_TRUST_Instr(lowest_node_in_branch))
	Instr(prev) -= 2; /* retry -> trust ; try -> nocp */
    }
    SM_DeallocateStruct(*smBTN,lowest_node_in_branch);
  }
}

/*----------------------------------------------------------------------*/

void safe_delete_branch(BTNptr lowest_node_in_branch) {

  byte choicepttype;

  MakeStatusDeleted(lowest_node_in_branch);
  choicepttype = 0x3 & Instr(lowest_node_in_branch);
  Instr(lowest_node_in_branch) = choicepttype | trie_no_cp_fail;
}

void undelete_branch(BTNptr lowest_node_in_branch) {

   byte choicepttype; 
   byte typeofinstr;

   if( IsDeletedNode(lowest_node_in_branch) ){
     choicepttype = 0x3 &  Instr(lowest_node_in_branch);
     typeofinstr = (~0x3) & BTN_Status(lowest_node_in_branch);

     Instr(lowest_node_in_branch) = choicepttype | typeofinstr;
     MakeStatusValid(lowest_node_in_branch);
   }
   else
     xsb_dbgmsg("Attempt to undelete a node that is not deleted");
 }

/*----------------------------------------------------------------------*/

#define DELETE_TRIE_STACK_INIT 100
#define MAX_DELETE_TRIE_STACK_SIZE 1000
#define DT_NODE 0
#define DT_DS 1
#define DT_HT 2

char *delete_trie_op = NULL;
BTNptr *delete_trie_node = NULL;
BTHTptr *delete_trie_hh = NULL;

int trie_op_size, trie_node_size, trie_hh_size;

#define push_delete_trie_node(node,op) {\
  trie_op_top++;\
  if (trie_op_top >= trie_op_size) {\
    trie_op_size = 2*trie_op_size;\
    delete_trie_op = (char *)realloc(delete_trie_op,trie_op_size*sizeof(char));\
    if (!delete_trie_op) xsb_exit("out of space for deleting trie");\
    /*printf("realloc delete_trie_op to %d\n",trie_op_size);*/\
  }\
  delete_trie_op[trie_op_top] = op;\
  trie_node_top++;\
  if (trie_node_top >= trie_node_size) {\
    trie_node_size = 2*trie_node_size;\
    delete_trie_node = (BTNptr *)realloc(delete_trie_node,trie_node_size*sizeof(BTNptr));\
    if (!delete_trie_node) xsb_exit("out of space for deleting trie");\
    /*printf("realloc delete_trie_node to %d\n",trie_node_size);*/\
  }\
  delete_trie_node[trie_node_top] = node;\
}  
#define push_delete_trie_hh(hh) {\
  trie_op_top++;\
  if (trie_op_top >= trie_op_size) {\
    trie_op_size = 2*trie_op_size;\
    delete_trie_op = (char *)realloc(delete_trie_op,trie_op_size*sizeof(char));\
    if (!delete_trie_op) xsb_exit("out of space for deleting trie");\
    /*printf("realloc delete_trie_op to %d\n",trie_op_size);*/\
  }\
  delete_trie_op[trie_op_top] = DT_HT;\
  trie_hh_top++;\
  if (trie_hh_top >= trie_hh_size) {\
    trie_hh_size = 2*trie_hh_size;\
    delete_trie_hh = (BTHTptr *)realloc(delete_trie_hh,trie_hh_size*sizeof(BTHTptr));\
    if (!delete_trie_hh) xsb_exit("out of space for deleting trie");\
    /*printf("realloc delete_trie_hh to %d\n",trie_hh_size);*/\
  }\
  delete_trie_hh[trie_hh_top] = hh;\
}  


void delete_trie(BTNptr iroot) {

  BTNptr root, sib, chil;  
  int trie_op_top = 0;
  int trie_node_top = 0;
  int trie_hh_top = -1;

  if (!delete_trie_op) {
    delete_trie_op = (char *)malloc(DELETE_TRIE_STACK_INIT*sizeof(char));
    delete_trie_node = (BTNptr *)malloc(DELETE_TRIE_STACK_INIT*sizeof(BTNptr));
    delete_trie_hh = (BTHTptr *)malloc(DELETE_TRIE_STACK_INIT*sizeof(BTHTptr));
    trie_op_size = trie_node_size = trie_hh_size = DELETE_TRIE_STACK_INIT;
  }

  delete_trie_op[0] = 0;
  delete_trie_node[0] = iroot;
  while (trie_op_top >= 0) {
    /*    printf("top %d %d %d %p\n",trie_op_top,trie_hh_top,
	  delete_trie_op[trie_op_top],delete_trie_node[trie_node_top]); */
    switch (delete_trie_op[trie_op_top--]) {
    case DT_DS:
      root = delete_trie_node[trie_node_top--];
      SM_DeallocateStruct(*smBTN,root);
      break;
    case DT_HT:
      free_trie_ht(delete_trie_hh[trie_hh_top--]);
      break;
    case DT_NODE:
      root = delete_trie_node[trie_node_top--];
      if ( IsNonNULL(root) ) {
	if ( IsHashHeader(root) ) {
	  BTHTptr hhdr;
	  BTNptr *base, *cur;
	  hhdr = (BTHTptr)root;
	  base = BTHT_BucketArray(hhdr);
	  push_delete_trie_hh(hhdr);
	  for ( cur = base; cur < base + BTHT_NumBuckets(hhdr); cur++ ) {
	    if (IsNonNULL(*cur)) {
	      push_delete_trie_node(*cur,DT_NODE);
	    }
	  }
	}
	else {
	  sib  = Sibl(root);
	  chil = Child(root);      
	  /* Child nodes == NULL is not the correct test*/
	  if (IsLeafNode(root)) {
	    if (IsNonNULL(chil))
	      xsb_exit("Anomaly in delete_trie !");
	    push_delete_trie_node(root,DT_DS);
	    if (IsNonNULL(sib)) {
	      push_delete_trie_node(sib,DT_NODE);
	    }
	  }
	  else {
	    push_delete_trie_node(root,DT_DS);
	    if (IsNonNULL(sib)) {
	      push_delete_trie_node(sib,DT_NODE);
	    }
	    if (IsNonNULL(chil)) {
	      push_delete_trie_node(chil,DT_NODE);
	    }
	  }
	}
      } else printf("null node");
      break;
    }
  }
  if (trie_op_size > MAX_DELETE_TRIE_STACK_SIZE) {
    free(delete_trie_op); delete_trie_op = NULL;
    free(delete_trie_node); delete_trie_node = NULL;
    free(delete_trie_hh); delete_trie_hh = NULL;
    trie_op_size = 0; 
  }
}


/*======================================================================*/

/*
 *                  A N S W E R   O P E R A T I O N S
 *                  =================================
 */

/*----------------------------------------------------------------------*/

/* This does not reclaim space for deleted nodes, only marks
 * the node as deleted and changes the try instruction to fail.
 * The deleted node is then linked into the del_nodes_list
 * in the completion stack.
 */
void delete_return(BTNptr l, VariantSF sg_frame) 
{
  ALNptr a, n, next;
  NLChoice c;
#ifdef CHAT
  chat_init_pheader chat_ptr;
#else
#ifdef LOCAL_EVAL
  TChoice  tc;
#endif
#endif

#ifdef DEBUG_RECLAIM_DEL_RET
    xsb_dbgmsg("DELETE_NODE: %d - Par: %d", l, Parent(l));
#endif
  safe_delete_branch(l);
  if (!is_completed(sg_frame)) {
    n = subg_ans_list_ptr(sg_frame);
    /* Find previous sibling -pvr */
    while (ALN_Answer(ALN_Next(n)) != l) {
      n = ALN_Next(n);/* if a is not in that list a core dump will result */
    }
    if (n == NULL) {
      xsb_exit("Error in delete_return()");
    }
    a               = ALN_Next(n);
    next            = ALN_Next(a);
    ALN_Next(a) = compl_del_ret_list(subg_compl_stack_ptr(sg_frame));
    compl_del_ret_list(subg_compl_stack_ptr(sg_frame)) = a;    

    ALN_Next(n) = next;
    
    /* Make consumed answer field of consumers point to
       previous sibling if they point to a deleted answer */
#ifdef CHAT
    chat_ptr = (chat_init_pheader)compl_cons_copy_list(subg_compl_stack_ptr(sg_frame));
    while (chat_ptr != NULL) {
      c = (NLChoice)(&chat_get_cons_start(chat_ptr));
      if (nlcp_trie_return(c) == a) {
	nlcp_trie_return(c) = n;
      }
      chat_ptr = (chat_init_pheader)nlcp_prevlookup(c);
    }
#else
    c = (NLChoice) subg_asf_list_ptr(sg_frame);
    while(c != NULL){
      if(nlcp_trie_return(c) == a){
	nlcp_trie_return(c) = n;
      }
      c = (NLChoice)nlcp_prevlookup(c);
    }
#endif

#if (defined(LOCAL_EVAL) && !defined(CHAT))
      /* if gen-cons points to deleted answer, make it
       * point to previous sibling */
      tc = (TChoice)subg_cp_ptr(sg_frame);
      if (tcp_trie_return(tc) == a) {
	tcp_trie_return(tc) = n;
      }
#endif
   
    ALN_Next(n) = next;
    if(next == NULL){ /* last answer */
      subg_ans_list_tail(sg_frame) = n;
    }      
  }
}

/*----------------------------------------------------------------------*/
/* Given a tabled subgoal, go through its list of deleted nodes (in the
 * completion stack), and reclaim the leaves and corresponding branches
 *----------------------------------------------------------------------*/

void  reclaim_del_ret_list(VariantSF sg_frame) {
  ALNptr x,y;
  
  x = compl_del_ret_list(subg_compl_stack_ptr(sg_frame));
  
  while (x != NULL) {
    y = x;
    x = ALN_Next(x);
    delete_branch(ALN_Answer(y), &subg_ans_root_ptr(sg_frame));
    SM_DeallocateStruct(smALN,y);
  }
}
 
/*----------------------------------------------------------------------*/

/*
**   Used in aggregs.P to implement aggregates.
**   Takes:   breg (the place where choice point is saved) and arity.  
**   Returns: subgoal skeleton (i.e., ret(X,Y,Z), where X,Y,Z are all the 
**    	      	                distinct variables in the subgoal);
**   	      Pointer to the subgoal.
*/

void breg_retskel(void)
{
    Pair    sym;
    Cell    term;
    VariantSF sg_frame;
    CPtr    tcp, cptr, where;
    int     new, i;
#ifndef CHAT
    int     arity;
#endif
    Integer breg_offset, Nvars;

    breg_offset = ptoc_int(1);
    tcp = (CPtr)((Integer)(tcpstack.high) - breg_offset);
    sg_frame = (VariantSF)(tcp_subgoal_ptr(tcp));
#ifdef CHAT
    where = compl_hreg(subg_compl_stack_ptr(sg_frame));
    Nvars = int_val(cell(where)) & 0xffff; /* See get_var_and_attv_nums() */
    cptr = where - Nvars - 1;
#else
    arity = ptoc_int(2);
    where = tcp + TCP_SIZE + (Cell)arity;
    Nvars = int_val(cell(where)) & 0xffff; /* See get_var_and_attv_nums() */
    cptr = where + Nvars;
#endif
    if (Nvars == 0) {
      ctop_string(3, (char *) ret_psc[0]);
    } else {
/*
      sreg = hreg;
      bind_cs((CPtr)term, sreg);
      sym = insert("ret", Nvars, (Psc)flags[CURRENT_MODULE], &new);
      new_heap_functor(sreg, sym->psc_ptr);
#ifdef CHAT
      for (i = Nvars; i > 0; i--) {
	bind_copy(sreg, (Cell)(*(CPtr)(cptr+i)));
#else
      for (i = 0; i < Nvars; i++) {
	bind_copy(sreg, (Cell)(*(CPtr)(cptr-i)));
#endif
	sreg++;
      }
      hreg = sreg;
*/
      bind_cs((CPtr)ptoc_tag(3), hreg);
      sym = insert("ret", (byte)Nvars, (Psc)flags[CURRENT_MODULE], &new);
      new_heap_functor(hreg, sym->psc_ptr);
#ifdef CHAT
      for (i = Nvars; i > 0; i--) {
	term = (Cell)(*(CPtr)(cptr+i));
#else
      for (i = 0; i < Nvars; i++) {
	term = (Cell)(*(CPtr)(cptr-i));
#endif
        nbldval(term);
      }
    }
    ctop_int(4, (Integer)sg_frame);
}


/*======================================================================*/

/*
 *                    I N T E R N E D   T R I E S
 *                    ===========================
 */

#define ADJUST_SIZE 100

BTNptr *Set_ArrayPtr = NULL;
/*
 * first_free_set is the index of the first deleted set.  The deleted
 * tries are deleted in builtin DELETE_TRIE, and the corresponding
 * elements in Set_ArrayPtr are linked to form a list.  So
 * Set_ArrayPtr[first_free_set] contains the index of the next deleted
 * set, ..., the last one contains 0.  If first_free_set == 0, that
 * means no free set available.
 */
static int first_free_set = 0;
static int Set_ArraySz = 100;
/*
 * num_sets is the number of sets have been used (including the fixed
 * trie, Set_ArrayPtr[0] (see trie_intern/3)).  It is also the index for
 * the next element to use when no free element is available.
 */
static int num_sets = 1;

/*----------------------------------------------------------------------*/

/* Allocate an array of handles to interned tries. */

void init_newtrie(void)
{
  Set_ArrayPtr = (BTNptr *) calloc(Set_ArraySz,sizeof(BTNptr));
}

/*----------------------------------------------------------------------*/

/* Returns a handle to an unused interned trie. */

void newtrie(void)
{
  int i;
  
  if (first_free_set != 0) {	/* a free set is available */
    i = first_free_set;		/* save it in i */
    ctop_int(1, first_free_set);
    first_free_set = (long) Set_ArrayPtr[first_free_set] >> 2;
    Set_ArrayPtr[i] = NULL;	/* must be reset to NULL */
  }
  else {
    if (num_sets == Set_ArraySz) { /* run out of elements */
      BTNptr *temp_arrayptr;

      temp_arrayptr = Set_ArrayPtr;
      Set_ArraySz += ADJUST_SIZE;  /* adjust the array size */
      Set_ArrayPtr = (BTNptr *) calloc(Set_ArraySz ,sizeof(BTNptr));
      if (Set_ArrayPtr == NULL)
	xsb_exit("Out of memory in new_trie/1");
      for (i = 0; i < num_sets; i++)
	Set_ArrayPtr[i] = temp_arrayptr[i];
      free(temp_arrayptr);
    }
    ctop_int(1, num_sets);
    num_sets++;
  }
}

/*----------------------------------------------------------------------*/

void trie_intern(void)
{
  prolog_term term;
  int RootIndex;
  int flag;
  BTNptr Leaf;

  term = ptoc_tag(1);
  RootIndex = ptoc_int(2);

#ifdef DEBUG_INTERN
  printf("Interning ");
  printterm(term,1,25);
  printf("In position %d\n", RootIndex);
#endif
  switch_to_trie_assert;
  Leaf = whole_term_chk_ins(term,&(Set_ArrayPtr[RootIndex]),&flag);
  switch_from_trie_assert;
  
  ctop_int(3,(Integer)Leaf);
  ctop_int(4,flag);
#ifdef DEBUG_INTERN
  printf("Exit flag %d\n",flag);
#endif
}

/*----------------------------------------------------------------------*/

int trie_interned(void)
{
  int RootIndex;
  int ret_val = FALSE;
  Cell Leafterm, trie_term;

  trie_term =  ptoc_tag(1);
  RootIndex = ptoc_int(2);
  Leafterm = ptoc_tag(3);
  
  /*
   * Only if Set_ArrayPtr[RootIndex] is a valid BTNptr can we run this
   * builtin.  That means Set_ArrayPtr[RootIndex] can neither be NULL,
   * nor a deleted set (deleted by builtin delete_trie/1).
   */
  if ((Set_ArrayPtr[RootIndex] != NULL) &&
      (!((long) Set_ArrayPtr[RootIndex] & 0x3))) {
    XSB_Deref(trie_term);
    XSB_Deref(Leafterm);
    if (isref(Leafterm)) {  
      reg_arrayptr = reg_array -1;
      num_vars_in_var_regs = -1;
      pushreg(trie_term);
      pcreg = (byte *)Set_ArrayPtr[RootIndex];
      ret_val =  TRUE;
    }
    else{
      xsb_exit("Not yet grd Leafterm!");
    }
  }
  return(ret_val);
}

/*----------------------------------------------------------------------*/

/*
 * This is builtin #162: TRIE_DISPOSE(+ROOT, +LEAF), to dispose a branch
 * of the trie rooted at Set_ArrayPtr[ROOT].
 */

void trie_dispose(void)
{
  BTNptr Leaf;
  long Rootidx;

  Rootidx = ptoc_int(1);
  Leaf = (BTNptr)ptoc_int(2);
  switch_to_trie_assert;
  delete_branch(Leaf, &(Set_ArrayPtr[Rootidx]));
  switch_from_trie_assert;
}

/*----------------------------------------------------------------------*/

#define DELETED_SET 1

void delete_interned_trie(int tmpval) {
  /*
   * We can only delete a valid BTNptr, so that only those sets
   * that were used before can be put into the free set list.
   */
  if ((Set_ArrayPtr[tmpval] != NULL) &&
      (!((long) Set_ArrayPtr[tmpval] & 0x3))) {
    switch_to_trie_assert;
    delete_trie(Set_ArrayPtr[tmpval]);
    switch_from_trie_assert;
    /*
     * Save the value of first_free_set into Set_ArrayPtr[tmpval].
     * Some simple encoding is needed, because in trie_interned/4 we
     * have to know this set is already deleted.
     */
    Set_ArrayPtr[tmpval] = (BTNptr) (first_free_set << 2 | DELETED_SET);
    first_free_set = tmpval;
  }
}
