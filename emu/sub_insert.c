/* File:      sub_insert.c
** Author(s): Ernie Johnson
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
** Copyright (C) ECRC, Germany, 1990
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
#include <stdlib.h>

#include "auxlry.h"
#include "cell.h"
#include "register.h"
#include "xsberror.h"
#include "psc.h"
#include "deref.h"
#include "table_stats.h"
#include "trie_internals.h"
#include "tst_aux.h"
#include "xmacro.h"
#include "binding.h"
#include "inst.h"


#define _DEBUG_CALL_CHK_INS


/*  Data Structures and Related Macros
    ==================================  */

/*
 *  callCPStack
 *  -----------
 *  for saving state information so the search may resume from that
 *  point down an alternate path in the call trie.
 */

typedef struct {
  BTNptr alt_node;	/* BTN at which to continue the search */
  BTNptr var_chain;	/* beginning of variable chain */
  CPtr termstk_top;	/* current top-of-tstTermStack at CP creation */
  pLogFrame log_top;	/* current top-of-tstTermStackLog at CP creation */
  CPtr *trail_top;	/* current top-of-tstTrail at CP creation */
} callChoicePointFrame;
typedef callChoicePointFrame *pCPFrame;

#define CALL_CPSTACK_SIZE   K

static struct {
  pCPFrame top;          /* next available location to place an entry */
  pCPFrame ceiling;      /* overflow pointer: ptr to CPF off array end */
  callChoicePointFrame base[CALL_CPSTACK_SIZE];
} callCPStack;

/* Use these to access the frame to which `top' points */
#define CPF_AlternateNode     ((callCPStack.top)->alt_node)
#define CPF_VariableChain     ((callCPStack.top)->var_chain)
#define CPF_TermStackTop      ((callCPStack.top)->termstk_top)
#define CPF_TermStackLogTop   ((callCPStack.top)->log_top)
#define CPF_TrailTop          ((callCPStack.top)->trail_top)

#define CPStack_ResetTOS     callCPStack.top = callCPStack.base
#define CPStack_IsEmpty      (callCPStack.top == callCPStack.base)
#define CPStack_IsFull       (callCPStack.top == callCPStack.ceiling)

#ifdef CHECK_TST_STACKS
#define CPStack_OverflowCheck     \
   if (CPStack_IsFull)            \
     Sub_CallChkIns_Error("callCPStack overflow!")
#else
#define CPStack_OverflowCheck
#endif

/*
 *  All that is assumed on CP creation is that the term stack and log have
 *  been altered for the subterm under consideration.  Any additional
 *  changes necessary to continue the search, e.g., pushing functor args
 *  or trailing a variable, must be made AFTER a CPF is pushed.
 */
#define CPStack_PushFrame(AltNode, VarChain) {		\
   CPStack_OverflowCheck;				\
   CPF_AlternateNode = AltNode;				\
   CPF_VariableChain = VarChain;			\
   CPF_TermStackTop = tstTermStack.top + 1;		\
   CPF_TermStackLogTop = tstTermStackLog.top - 1;	\
   CPF_TrailTop = tstTrail.top;				\
   callCPStack.top++;					\
 }

/*
 *  Perform a backtracking operation using the above data structures.
 */
#define CPStack_PopFrame {                \
   callCPStack.top--;                     \
   pCurrentBTN = CPF_AlternateNode;       \
   variableChain = CPF_VariableChain;     \
   search_mode = MATCH_WITH_TRIEVAR;      \
   RestoreTermStack;                      \
   Trail_Unwind(CPF_TrailTop)             \
 }

/*
 *  Reduce the number of memory moves by not blindly restoring all changes,
 *  but only those that will effect the soon-to-be active portion of the
 *  term stack.  Use the following to replace the body of the while loop:
 *      tstTermStackLog.top--;
 *      if (LogFrame_Addr < CPF_TermStackTop)
 *        *LogFrame_Addr = LogFrame_Value;
 */
#define RestoreTermStack				\
   while (tstTermStackLog.top > CPF_TermStackLogTop)	\
     TermStackLog_PopAndReset				\
   tstTermStack.top = CPF_TermStackTop


/* ========================================================================= */


/* ERROR HANDLING
   ============== */

/* Make sure the tst-aux trail is initialized before using these... */

#define Sub_CallChkIns_Error(String)     sub_call_error(String)

static void sub_call_error(char *string) {
  Trail_Unwind_All;
  xsb_abort("Error in Subsumptive Call Check/Insert Operation:\n\t%s\n",
	    string);
}


/* ========================================================================= */


/* TRIE- AND CALL-VARIABLE HANDLING
   ================================ */

/*
 * The subsumptive call check/insert algorithm requires that we track both
 * variables appearing in the call (for handling nonlinearity in case we
 * have to insert the call) and bindings made to the trie variables during
 * processing (to find the answer template in case of subsumption).
 * Therefore we take the following approach.  Call variables are bound to
 * cells of the VarEnumerator array, just as is done during term
 * interning.  For noting the bindings made to variables appearing in the
 * trie, we employ another array, called TrieVarBindings[], which will
 * maintain pointers to dereferenced subterms of the call.  The Ith
 * TrieVar along a particular path is associated with TrieVarBindings[I].
 *
 * Notice that as a result of using subsumption, (1) variables in the call
 * can only be "matched" with variables in the trie, and (2) there will be
 * at least as many variables in a subsuming path of the trie as in the
 * call itself.  Therefore, given that I is the ID of the next unique
 * trievar, when a never-before seen variable is encountered in the call,
 * we bind it to the Ith cell of VarEnumerator, thusly standardizing it.
 * Likewise, TrieVarBindings[I] gets the address of the call variable.  By
 * using the same array indexer, I, we can determine from a standardized
 * call variable the first trievar that has been bound to it, and this
 * enables us to find the actual address of the call variable so that
 * additional trie variables may bind themselves to it, if necessary.
 */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Given a trievar number and a prolog subterm (not a VarEnumerator addr),
 *  bind the trievar to that subterm and trail the trievar.
 */
#define TrieVar_BindToSubterm(TrieVarNum,Subterm)	\
   TrieVarBindings[TrieVarNum] = Subterm;		\
   Trail_Push(&TrieVarBindings[TrieVarNum])

/*
 *  Given a trievar number and a marked callvar (bound to a VarEnumerator
 *  cell), bind the trievar to the variable subterm represented by the
 *  marked callvar, and trail the trievar.
 */
#define TrieVar_BindToMarkedCallVar(TrieVarNum,CallVarMarker)	\
   TrieVarBindings[TrieVarNum] =				\
     TrieVarBindings[CallVar_Index(CallVarMarker)];		\
   Trail_Push(&TrieVarBindings[TrieVarNum])

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Given an unmarked, dereferenced call variable and a trievar number,
 *  mark the variable with this number by setting it to point to the
 *  Index-th cell of the VarEnumerator array, and trail the call variable.
 */
#define CallVar_MarkIt(DerefedVar,Index)	\
   StandardizeVariable(DerefedVar,Index);	\
   Trail_Push((CPtr)DerefedVar)

/*
 *  Given a dereferenced call variable, determine whether it has already
 *  been marked, i.e. seen during prior processing and hence bound to a
 *  VarEnumerator cell.
 */
#define CallVar_IsMarked(pDerefedCallVar)	\
   IsStandardizedVariable(pDerefedCallVar)

/*
 *  Given an address into VarEnumerator, determine its index in this array.
 *  (This index will also correspond to the first trie variable that bound
 *   itself to it.)
 */
#define CallVar_Index(VarEnumAddr)  IndexOfStdVar(VarEnumAddr)


/* ========================================================================= */


/* CONSTRUCTING A VARIANT
   ====================== */

/*
 *  The Subsumptive Call Check/Insert Operation calls for variant entries
 *  to be made in case no subsuming call is present in the trie, or if a
 *  subsuming (not variant) call is present but has an incomplete answer
 *  table.  Since this operation favors variant selection from the trie,
 *  we can utilize the work performed during the initial insurgence, up to
 *  the point where the trie deviated from a variant form of the call.  We
 *  note the last node which was successfully matched, the contents of the
 *  tstTermStack, and the bindings made to the variables of the call.
 *  This information is sufficient to proceed, from the last successful
 *  match, with the insertion of new nodes into the trie to create a
 *  variant entry for the call, if need be.
 */

static struct VariantContinuation {
  BTNptr last_node_matched;
  int num_subterms;
  Cell term_stack[TST_TERMSTACK_SIZE];
  int num_bindings;
  struct {
    CPtr pCallVar;
    Cell value;
  } trail[TST_TRAIL_SIZE];
} variant;


#define SetNoVariant(LastNodeMatched)		\
   if (variant_path == YES) {			\
     save_state_for_insertion(LastNodeMatched);	\
     variant_path = NO;				\
   }


/*
 * This should be called immediately after determining that no variant
 * exists, in particular, BEFORE any additional trailing is performed.
 */

static void save_state_for_insertion(BTNptr last_node_match) {

  int counter;
  CPtr termptr, *binding;

  variant.last_node_matched = last_node_match;

  counter = 0;
  termptr = tstTermStack.base;
  /*
   * Include the subterm that couldn't be matched.
   */
  while (termptr <= tstTermStack.top) {
    variant.term_stack[counter] = *termptr;
    termptr++;
    counter++;
  }
  variant.num_subterms = counter;

  counter = 0;
  binding = tstTrail.base;
  while (binding < tstTrail.top) {
    termptr = *binding;
    /*
     * Take only those bindings made to the call variables.
     * (Finding the value through a single deref is only possible if the
     *  original subterm was deref'ed before trailing.  Currently, this is
     *  the case, with trailing of unmarked callvars occuring in only one
     *  place in the code.)
     */
    if ( ! IsUnboundTrieVar(termptr) ) {
      variant.trail[counter].pCallVar = termptr;
      variant.trail[counter].value = *termptr;
      counter++;
    }
    binding++;
  }
  variant.num_bindings = counter;
}


/*
 *  Firstly, the state is restored to the point from which insertion is to
 *  be continued.  This includes readying the term stack with the remaining
 *  portion of the call, rebinding any call variables already encountered,
 *  and noting these bindings on the trail.  (Bindings of trievars have
 *  been filtered out, so only bindings of callvars appear in the trail.)
 *  Then the remaining portion of the call is stepped through and inserted
 *  below the node of the last match.
 */

static BTNptr construct_variant_call(bool wantAnsTemp, CPtr *answer_temp) {

  Cell subterm, symbol;
  BTNptr pCurrentBTN, pParentBTN;
  int tag, counter;

  /* Restore State
     ------------- */
  TermStack_ResetTOS;
  for (counter = 0; counter < variant.num_subterms; counter++)
    TermStack_Push(variant.term_stack[counter]);

  Trail_ResetTOS;
  for (counter = 0; counter < variant.num_bindings; counter++) {
    Trail_Push(variant.trail[counter].pCallVar);
    bld_ref(variant.trail[counter].pCallVar, variant.trail[counter].value);
  }

  pParentBTN = variant.last_node_matched;
  pCurrentBTN = BTN_Child(pParentBTN);
  counter = variant.num_bindings;
  symbol = tag = 0;      /* suppress compiler warning */

  /* Insert Remaining Portion of Call
     -------------------------------- */
  while ( ! TermStack_IsEmpty ) {
    subterm = TermStack_Pop;
    deref(subterm);
    switch (tag = cell_tag(subterm)) {
    case REF:
    case REF1:
      if ( ! CallVar_IsMarked(subterm) ) {
	symbol = EncodeNewTrieVar(counter);
	CallVar_MarkIt(subterm,counter);
	counter++;
      }
      else
	symbol = EncodeTrieVar(CallVar_Index(subterm));
      break;
    case STRING:
    case INT:
    case FLOAT:
      symbol = EncodeTrieConstant(subterm);
      break;
    case CS:
      symbol = EncodeTrieFunctor(subterm);
      TermStack_PushFunctorArgs(subterm)
      break;
    case LIST:
      symbol = EncodeTrieList(subterm);
      TermStack_PushListArgs(subterm);
      break;
    default:
      fprintf(stderr, "subterm: unknown (%ld),  symbol: ? (%ld)\n",
	      cell_tag(subterm), TrieSymbolType(symbol));
      Sub_CallChkIns_Error("Inserting variant: Trie symbol with bogus tag!");
      break;
    }
    /*
     * Insert 'symbol' under 'pParentBTN'.
     */
    if ( IsNULL(pCurrentBTN) ) {
      New_BTN(pCurrentBTN,CALL_TRIE_TT,INTERRIOR_NT,symbol,pParentBTN,NULL);
      BTN_Child(pParentBTN) = pCurrentBTN;
    }
    else if ( IsHashHeader(pCurrentBTN) ) {
      BTNptr *pBucket;
      BTHTptr pBTHT;

      pBTHT = (BTHTptr)pCurrentBTN;
      pBucket = CalculateBucketForSymbol(pBTHT,symbol);
      New_BTN(pCurrentBTN,CALL_TRIE_TT,HASHED_INTERRIOR_NT,
	      symbol,pParentBTN,*pBucket);
      *pBucket = pCurrentBTN;
      BTHT_NumContents(pBTHT)++;
      if ( BTHT_NumContents(pBTHT) > BTHT_NumBuckets(pBTHT) )
	expand_trie_ht(pBTHT);
    }
    else {
      int count = 0;
      BTNptr pBTN;

      New_BTN(pBTN,CALL_TRIE_TT,INTERRIOR_NT,symbol,pParentBTN,pCurrentBTN);
      BTN_Child(pParentBTN) = pBTN;
      pCurrentBTN = pBTN;

      /* Change to hash structure if there are "too many" children now */
      while ( IsNonNULL(pBTN) ) {
	count++;
	pBTN = BTN_Sibling(pBTN);
      }
      if (count > MAX_SIBLING_LEN)
	hashify_children(pParentBTN, CALL_TRIE_TT);
    }
    pParentBTN = pCurrentBTN;
    pCurrentBTN = BTN_Child(pCurrentBTN);
  }
  /*
   * Post-process new trie-term, set answer template (if wanted),
   * cleanup the trail, and return.
   */
  MakeLeafNode(pParentBTN);
  TN_UpgradeInstrTypeToSUCCESS(pParentBTN,tag);

  if (wantAnsTemp) {
    CPtr location, *pCallVar;

    location = *answer_temp;
    pCallVar = tstTrail.base;
    while (pCallVar < tstTrail.top)
      *location-- = (Cell)*pCallVar++;
    *location = makeint(counter);
    *answer_temp = location;
  }

  Trail_Unwind_All
  return(pParentBTN);
}


/* ========================================================================= */


/* CREATING THE ANSWER TEMPLATE
   ============================ */

/*
 *  In the event that a subsuming path is found which maintains its own
 *  answer table, the answer template constructed while tracing out this
 *  path in the trie is the one that is needed.  Extract the template from
 *  the TrieVarBindings array and place it on the SLG-WAM Choice Point Stack.
 */

#define SetAnswerTemplate(Location) {					\
									\
  int index;								\
									\
  index = 0;								\
  while (TrieVarBindings[index] != (Cell)(TrieVarBindings + index)) {	\
    *Location = TrieVarBindings[index];					\
    Location--;								\
    index++;								\
  }									\
  *Location = makeint(index);						\
}


/*
 *  In the event that the found subsumer does not create its own answer
 *  table, but instead selects answers from another, more general
 *  subgoal's answer table, we must construct an answer template based
 *  on this more general subsumer's call.
 *
 *  Since we have direct access to this more general call, and know that
 *  it subsumes the current call, we can quickly construct the required
 *  template.
 */

inline static CPtr compute_answer_template(TabledCallInfo *call_info,
					   SGFrame subsumer,
					   CPtr answer_tmplt) {

  int sizeAnsTmplt, symbol_tag;
  BTNptr pBTN;
  Cell subterm, symbol;

  /*
   * Store the symbols along the path of the more general call.
   */
  SymbolStack_ResetTOS;
  pBTN = subg_leaf_ptr(subsumer);
  while ( ! IsTrieRoot(pBTN) ) {
    SymbolStack_Push(BTN_Symbol(pBTN));
    pBTN = BTN_Parent(pBTN);
  }
  SymbolStack_OverflowCheck;

  TermStack_ResetTOS;    /* should already be empty, but just in case... */
  TermStack_PushLowToHighVector(CallInfo_Arguments(*call_info),
			        CallInfo_CallArity(*call_info))

  /*
   * Create the answer template while we process.  Since we know we have a
   * more general subsuming call, we can greatly simplify the "matching"
   * process: we know we either have exact matches of non-variable symbols
   * or a variable paired with some subterm of the current call.
   */
  sizeAnsTmplt = 0;
  while ( ! TermStack_IsEmpty ) {
    subterm = TermStack_Pop;
    deref(subterm);
    symbol = SymbolStack_Pop;
    symbol_tag = TrieSymbolType(symbol);
    if ( symbol_tag == TrieVar ) {
      if ( IsNewTrieVar(symbol) ) {
	*answer_tmplt-- = subterm;
	sizeAnsTmplt++;
      }
    }
    else if ( symbol_tag == CS )
      TermStack_PushFunctorArgs(subterm)
    else if ( symbol_tag == LIST )
      TermStack_PushListArgs(subterm)
  }
  *answer_tmplt = makeint(sizeAnsTmplt);
  return answer_tmplt;
}

/* ========================================================================= */


/* ALGORITHMIC SHORTHANDS
   ====================== */

/*
 *  When first stepping onto a particular trie level, we may find
 *  ourselves either looking at a hash table header or a trie node in a
 *  simple chain.  Given the object first encountered at this level
 *  (pointed to by 'pCurrentBTN') and a trie-encoded symbol, determine
 *  the node chains on this level which would contain that symbol or
 *  contain any trie variables, should either exist.
 */
#define Set_Matching_and_TrieVar_Chains(Symbol,MatchChain,VarChain)	\
   if ( IsNonNULL(pCurrentBTN) && IsHashHeader(pCurrentBTN) ) {		\
     BTNptr *buckets;							\
     BTHTptr pBTHT;							\
									\
     pBTHT = (BTHTptr)pCurrentBTN;					\
     buckets = BTHT_BucketArray(pBTHT);					\
     MatchChain = buckets[TrieHash(Symbol,BTHT_GetHashSeed(pBTHT))];	\
     VarChain = buckets[TRIEVAR_BUCKET];				\
   }									\
   else	   /* simple chain of nodes */					\
     VarChain = MatchChain = pCurrentBTN

/* ------------------------------------------------------------------------- */

/* 
 *  Exact Matches for Non-Variable Subterms
 *  ---------------------------------------
 *  Exact matches for non-variable subterms of the call are always looked
 *  for first.  Once an exact match has been found, there is no need to
 *  search further, since there is at most one occurrence of a symbol at
 *  a level below a particular node.  Further exploration will
 *  concentrate on pairing the subterm with trie variables.
 *
 *  After a successful match, a choice point frame is laid, with
 *  VariableChain providing the trie-path continuation.  We step down onto
 *  the next level of the trie, below the matched node, and then branch
 *  back to the major loop of the algorithm.  If no match is found, then
 *  execution exits this block with a NULL MatchChain.  MatchChain may be
 *  NULL at block entry.
 *
 *  TermStack_PushOp is provided so that this macro can be used for
 *  constants, functor symbols, and lists.  Constants pass in a
 *  TermStack_NOOP, while functors use TermStack_PushFunctorArgs(), and
 *  lists use TermStack_PushListArgs().
 */

#define NonVarSearchChain_ExactMatch(Symbol,MatchChain,VariableChain,	\
				     TermStack_PushOp)			\
									\
   while ( IsNonNULL(MatchChain) ) {					\
     if (Symbol == BTN_Symbol(MatchChain)) {				\
       Conditionally_Create_ChoicePointFrame(VariableChain)		\
       TermStack_PushOp							\
       Descend_In_Trie_and_Continue(MatchChain);			\
     }									\
     MatchChain = BTN_Sibling(MatchChain);				\
   }

/* ------------------------------------------------------------------------- */

/* 
 *  Matching Non-Variable Subterms with Bound Trievars
 *  --------------------------------------------------
 *  After having either (1) failed to find an exact match for the
 *  call's non-variable subterm, or (2) backtracked to explore other
 *  unifications, we proceed to check for equality of the subterm and
 *  the bindings made to previously seen trievars.  (A trie variable
 *  whose first occurrence (along the path from the root to here) is
 *  at this level would, of course, not have a binding.)  There may be
 *  several trievars at a level with such a binding.
 *
 *  We check for true equality, meaning that variables appearing in both
 *  must be identical.  (This is because no part of the call may become
 *  futher instantiated as a result of the check/insert operation.)  If
 *  they are the same, we succeed, else we continue to look in the chain
 *  for a suitable trievar.  Processing a success entails (1) laying a
 *  choice point since there may be other pairings in this node chain, (2)
 *  moving to the child of the matching node, (3) resetting our search mode
 *  to exact matches, and (4) branching back to the major loop of the
 *  algorithm.
 *
 *  If no match is found, then execution exits this block with a NULL
 *  'CurNODE'.  'CurNODE' may be NULL at block entry.
 */

#define NonVarSearchChain_BoundTrievar(Subterm,CurNODE,VarChain) {	  \
									  \
   int trievar_index;							  \
									  \
   while ( IsNonNULL(CurNODE) ) {					  \
     if ( IsTrieVar(BTN_Symbol(CurNODE)) &&				  \
          ! IsNewTrieVar(BTN_Symbol(CurNODE)) ) {			  \
       trievar_index = DecodeTrieVar(BTN_Symbol(CurNODE));		  \
       if (identical_subterms(TrieVarBindings[trievar_index],Subterm) ) { \
	 Create_ChoicePointFrame(BTN_Sibling(CurNODE),VarChain)		  \
	 Descend_In_Trie_and_Continue(CurNODE);				  \
       }								  \
     }									  \
     CurNODE = BTN_Sibling(CurNODE);					  \
   }									  \
 }

/* ------------------------------------------------------------------------- */

/* 
 *  Matching Non-Variable Subterms with Unbound Trievars
 *  ----------------------------------------------------
 *  Unifying a call's non-variable subterm with an unbound trievar is
 *  the last pairing operation explored.  Only one unbound trievar may
 *  occur below a particular node; it is a new (first occurrence of this
 *  particular) variable lying along the subpath from the root to the
 *  current position in the trie.  Such trievars are tagged with a
 *  "first occurrence" marker.  If we don't find one, we exit with a
 *  NULL 'VarChain'.  (Node: Because of the hashing scheme, VarChain may
 *  contain symbols other than variables; furthermore, it may be empty
 *  altogether.)
 */

#define NonVarSearchChain_UnboundTrievar(Subterm,VarChain) {	\
								\
   int trievar_index;						\
								\
   while ( IsNonNULL(VarChain) ) {				\
     if ( IsTrieVar(BTN_Symbol(VarChain)) &&			\
          IsNewTrieVar(BTN_Symbol(VarChain)) ) {		\
       trievar_index = DecodeTrieVar(BTN_Symbol(VarChain));	\
       TrieVar_BindToSubterm(trievar_index,Subterm);		\
       Descend_In_Trie_and_Continue(VarChain);			\
     }								\
     VarChain = BTN_Sibling(VarChain);				\
   }								\
 }

/* ------------------------------------------------------------------------- */

/* On a Successful Unification
 * ---------------------------
 * For continuing with forward execution.  When we find a successful
 * pairing, we continue the search with the next subterm on the
 * tstTermStack and the children of the trie node that was paired.
 */
#define Descend_In_Trie_and_Continue(PairedBTN)      \
   pParentBTN = PairedBTN;                           \
   pCurrentBTN = BTN_Child(PairedBTN);               \
   search_mode = MATCH_SYMBOL_EXACTLY;               \
   goto While_TermStack_NotEmpty

/* ------------------------------------------------------------------------- */

/* Choice Point Frame Creation
   =========================== */

/* Conditional Choice Points
 * -------------------------
 * Used after successfully finding an exact match between a non-variable
 * subterm of the call and a symbol in the trie.  The check for the
 * existence of a variable in the variable chain can be done quickly and
 * may result in the avoidance of choice point creation.  (Whether the
 * discovered variable is relevant is another matter...)
 */

#define Conditionally_Create_ChoicePointFrame(VariableChain) {	\
   BTNptr alternateBTN = VariableChain;				\
								\
   while ( IsNonNULL(alternateBTN) ) {				\
     if ( IsTrieVar(BTN_Symbol(alternateBTN)) ) {		\
       CPStack_PushFrame(alternateBTN, VariableChain)		\
       break;							\
     }								\
     alternateBTN = BTN_Sibling(alternateBTN);			\
   }								\
 }


/* Choice Point Creation
 * ---------------------
 * We lay a CPF whenever we've found a trievar binding identical to the
 * call's subterm.  A computation which may result in avoiding CPF creation
 * would be expensive (and useless if we never return to this CPF), so it
 * might be best to just lay one and take your chances that we never use
 * it.  If we do use it, however, the additional overhead of having the
 * main algorithm discover that it is (nearly) useless isn't too high
 * compared to the computation we could have done.  The benefit is that we
 * don't do it unless we have to.
 */

#define Create_ChoicePointFrame(AlternateBTN,VariableChain) \
   CPStack_PushFrame(AlternateBTN,VariableChain)

/* ------------------------------------------------------------------------- */

/* Checking for Identical Terms
   ---------------------------- */

static int identical_subterms(Cell term1, Cell term2) {

  deref(term1);
  deref(term2);
  
  if (term1 == term2)
    return TRUE;

  if(cell_tag(term1) != cell_tag(term2))
    return FALSE;

  if (cell_tag(term1) == CS) {
    CPtr cptr1 = clref_val(term1);
    CPtr cptr2 = clref_val(term2);
    Psc psc1 = (Psc)*cptr1;
    int i;

    if ( psc1 != (Psc)*cptr2 )
      return FALSE;

    for (cptr1++, cptr2++, i = 0; i < get_arity(psc1); i++)
      if (identical_subterms(*cptr1,*cptr2) == FALSE) 
	return FALSE;

    return TRUE;
  }
  else if (cell_tag(term1) == LIST) {
    CPtr cptr1 = clref_val(term1);
    CPtr cptr2 = clref_val(term2);

    if ( (identical_subterms(*cptr1, *cptr2) == FALSE) ||
	 (identical_subterms(*(cptr1 + 1), *(cptr2 + 1)) == FALSE) )
      return FALSE;
    else
      return TRUE;
  }
  else
    return FALSE;
}


/* ========================================================================= */


/* INTERFACE ROUTINES
   ================== */

void initSubCallChkIns() {

  int i;

  callCPStack.ceiling = callCPStack.base + CALL_CPSTACK_SIZE;

  /* set entries to unbound */
  for (i = 0; i < NUM_TRIEVARS; i++)
    TrieVarBindings[i] = (Cell)(TrieVarBindings + i);
}


/*
 * Subsumptive Call Check/Insert Operation
 * ---------------------------------------
 *
 * Look for a call that subsumes the incoming call, thus allowing only
 * one-way bindings: from trie variable to call subterm.  This algorithm
 * also attempts to minimize the number of bindings needed to match a call
 * existing in the trie with the incoming call.  However, it applies this
 * minimization in a greedy way, so that a global minimum could be missed.
 * The general strategy in pairing the call's subterms to trie symbols
 * favors, in order, 1) exact matches, 2) matches to bindings of
 * previously bound trie variables, 3) pairings with unbound trie
 * variables (causing new bindings).
 *
 * An entry for the call is created when either 1) no subsuming call
 * exists in the table, or 2) a more general (not variant) call exists,
 * but its table is incomplete.  No entry is created when either 1) a
 * variant call already exists in the table, or 2) a more general (not
 * variant) call is found and its table is complete.
 */

typedef enum Search_Strategy_Mode {
  MATCH_SYMBOL_EXACTLY, MATCH_WITH_TRIEVAR
} SearchMode;


void subsumptive_call_search(TabledCallInfo *callStruct,
			     CallLookupResults *results) {

  BTNptr pCurrentBTN;        /* Used for stepping through siblings while
				looking for a match with subterm */
  BTNptr pParentBTN;         /* The parent of pCurrentBTN.  Needed to identify
				the leaf node when pCurrentBTN becomes NULL */

  BTNptr variableChain;      /* Chain of nodes in which to search for
				variables in case we cannot find an exact
				match in the chain headed by pCurrentBTN */

  Cell subterm;              /* The part of the call we are inspecting */

  Cell symbol;               /* subterm, as it would appear in a trie node */

  int trievar_index;         /* temp for converting trievar num encoding */

  bool variant_path;         /* Denotes whether the call is a variant of that
				represented by the current path.  Search state
				info is saved when its value changes from YES
				to NO.  This may only occur once as this
				procedure begins by looking for a variant
				representation of the call in the trie. */

  SearchMode search_mode;    /* Depending on the subterm, indicates which type
			        of unifications we are interested in */

  CPtr answer_template;      /* Initially, the location to create the answer
				template, then a pointer to the template
				itself: INT-encoded size followed by a vector
				of subterms. */

  SGFrame sf_with_table;     /* Pointer to the Subgoal Frame from which the
			        call will consume. */

  TIFptr pTIF;               /* TableInfoFrame of called predicate */



#ifdef DEBUG_CALL_CHK_INS
  char *targetGN = "";     /* allows you to spy on a particular predicate */
  char *goal_name;

  goal_name = get_name(TIF_PSC(CallInfo_TableInfo(*callStruct)));
  if ( strcmp(targetGN,goal_name) == 0 ) {
    printf("\nCall Check/Insert (#%d overall) on:\n  ",
	   numSubOps.callci.total + 1);
    printCall(*callStruct);
    printf("\n");
  }
#endif

  NumSubOps_CallCheckInsert++;
  pTIF = CallInfo_TableInfo(*callStruct);
  if ( IsNULL(TIF_CallTrie(pTIF)) )
    TIF_CallTrie(pTIF) = newBasicTrie(TIF_PSC(pTIF),CALL_TRIE_TT);
  pParentBTN = TIF_CallTrie(pTIF);
  pCurrentBTN = BTN_Child(pParentBTN);
  variableChain = NULL;   /* suppress compiler warning */
  answer_template = CallInfo_VarVectorLoc(*callStruct) - 1;

  /* Check the term passed in
     ------------------------ */
  if (CallInfo_CallArity(*callStruct) == 0) {
    if ( IsNonNULL(pCurrentBTN) ) {
      if ( ! IsEscapeNode(pCurrentBTN) )
	xsb_abort("Non-ESCAPE node found in trie of 0-arity pred");
      CallLUR_VariantFound(*results) = TRUE;
      if ( is_completed(CallTrieLeaf_GetSF(pCurrentBTN)) )
	NumSubOps_CallToCompletedTable++;
      else
	NumSubOps_VariantCall++;
    }
    else {
      CreateEscapeBTN(pCurrentBTN,CALL_TRIE_TT,pParentBTN);
      BTN_Child(pParentBTN) = pCurrentBTN;
      CallLUR_VariantFound(*results) = FALSE;
      NumSubOps_ProducerCall++;
    }
    CallLUR_Leaf(*results) = pCurrentBTN;
    CallLUR_Subsumer(*results) = CallTrieLeaf_GetSF(pCurrentBTN);
    cell(answer_template) = makeint(0);
    CallLUR_VarVector(*results) = answer_template;
    return;
  }


  /* Initialize data structures
     -------------------------- */
  TermStack_ResetTOS;
  TermStackLog_ResetTOS;
  CPStack_ResetTOS;
  Trail_ResetTOS;

  TermStack_PushLowToHighVector(CallInfo_Arguments(*callStruct),
				CallInfo_CallArity(*callStruct));

  variant_path = YES;
  search_mode = MATCH_SYMBOL_EXACTLY;

  /* Major loop of the algorithm
     --------------------------- */

 While_TermStack_NotEmpty:

  while ( ! TermStack_IsEmpty ) {
    subterm = TermStack_Pop;
    TermStackLog_PushFrame;
    deref(subterm);
    switch(cell_tag(subterm)) {
      
    /* SUBTERM IS A CONSTANT
       --------------------- */
    case STRING:
    case INT:
    case FLOAT:
      /*
       *  NOTE:  A Trie constant looks like a Heap constant.
       */
      if (search_mode == MATCH_SYMBOL_EXACTLY) {
	symbol = EncodeTrieConstant(subterm);
	Set_Matching_and_TrieVar_Chains(symbol,pCurrentBTN,variableChain);
	NonVarSearchChain_ExactMatch(symbol, pCurrentBTN, variableChain,
				     TermStack_NOOP)
	/*
	 *  We've failed to find an exact match of the constant in a node
	 *  of the trie, so now we consider bound trievars whose bindings
	 *  exactly match the constant.
	 */
	pCurrentBTN = variableChain;
	SetNoVariant(pParentBTN);
      }
      NonVarSearchChain_BoundTrievar(subterm,pCurrentBTN,variableChain);
	/*
	 *  We've failed to find an exact match of the constant with a
	 *  binding of a trievar.  Our last alternative is to bind an
	 *  unbound trievar to this constant.
	 */
      NonVarSearchChain_UnboundTrievar(subterm,variableChain);
      break;


    /* SUBTERM IS A STRUCTURE
       ---------------------- */
    case CS:
      /*
       *  NOTE:  A trie CS is a CS-tagged PSC ptr, while a heap CS
       *         is a CS-tagged ptr to a PSC ptr.
       */
      if (search_mode == MATCH_SYMBOL_EXACTLY) {
	symbol = EncodeTrieFunctor(subterm);
	Set_Matching_and_TrieVar_Chains(symbol,pCurrentBTN,variableChain);
	NonVarSearchChain_ExactMatch(symbol, pCurrentBTN, variableChain,
				     TermStack_PushFunctorArgs(subterm))
	/*
	 *  We've failed to find an exact match of the functor's name in
	 *  a node of the trie, so now we consider bound trievars whose
	 *  bindings exactly match the subterm.
	 */
	pCurrentBTN = variableChain;
	SetNoVariant(pParentBTN);
      }
      NonVarSearchChain_BoundTrievar(subterm,pCurrentBTN,variableChain);
	/*
	 *  We've failed to find an exact match of the function expression
	 *  with a binding of a trievar.  Our last alternative is to bind
	 *  an unbound trievar to this subterm.
	 */
      NonVarSearchChain_UnboundTrievar(subterm,variableChain);
      break;

      
    /* SUBTERM IS A LIST
       ----------------- */
    case LIST:
      /*
       *  NOTE:  A trie LIST uses a plain LIST tag wherever a recursive
       *         substructure begins, while a heap LIST uses a LIST-
       *         tagged ptr to a pair of Cells, the first being the head
       *         and the second being the recursive tail, possibly another
       *         LIST-tagged ptr.
       */
      if (search_mode == MATCH_SYMBOL_EXACTLY) {
	symbol = EncodeTrieList(subterm);
	Set_Matching_and_TrieVar_Chains(symbol,pCurrentBTN,variableChain);
	NonVarSearchChain_ExactMatch(symbol, pCurrentBTN, variableChain,
				     TermStack_PushListArgs(subterm))
	/*
	 *  We've failed to find a node in the trie with a LIST symbol, so
	 *  now we consider bound trievars whose bindings exactly match the
	 *  actual list subterm.
	 */
	pCurrentBTN = variableChain;
	SetNoVariant(pParentBTN);
      }
      NonVarSearchChain_BoundTrievar(subterm,pCurrentBTN,variableChain);
	/*
	 *  We've failed to find an exact match of the list with a binding
	 *  of a trievar.  Our last alternative is to bind an unbound
	 *  trievar to this subterm.
	 */
      NonVarSearchChain_UnboundTrievar(subterm,variableChain);
      break;
      

    /* SUBTERM IS AN UNBOUND VARIABLE
       ------------------------------ */
    case REF:
    case REF1:
      /*
       *  A never-before-seen variable in the call must always match a
       *  free variable in the trie.  We can determine this by checking
       *  for a "first occurrence" tag in the trievar encoding.  Let Num
       *  be the index of this trievar variable.  Then we bind
       *  TrieVarBindings[Num] to 'subterm', the address of the deref'ed
       *  unbound call variable.  We also bind the call variable to
       *  VarEnumerator[Num] so that we can recognize that the call variable
       *  has already been seen.
       *
       *  When such a call variable is re-encountered, we know which
       *  trievar was the first to bind itself to this call variable: we
       *  used its index in marking the call variable when we bound it to
       *  VarEnumerator[Num].  This tagging scheme allows us to match
       *  additional unbound trie variables to it.  Recall that the
       *  TrieVarBindings array should contain *real* subterms, and not the
       *  callvar tags that we've constructed (the pointers into the
       *  VarEnumerator array).  So we must reconstruct a previously-seen
       *  variable's *real* address in order to bind a new trievar to it.
       *  We can do this by computing the index of the trievar that first
       *  bound itself to it, and look in that cell of the TrieVarBindings
       *  array to get the call variable's *real* address.
       *
       *  Notice that this allows us to match variants.  For if we have a
       *  variant up to the point where we encounter a marked callvar,
       *  there can be at most one trievar which exactly matches it.  An
       *  unbound callvar, then, matches exactly only with an unbound
       *  trievar.  Therefore, only when a previously seen callvar must be
       *  paired with an unbound trievar to continue the check/insert
       *  operation do we say that no variant exists.  (Just as is the case
       *  for other call subterm types, the lack of an exact match and its
       *  subsequent pairing with an unbound trievar destroys the
       *  possibility of a variant.)  */

      if (search_mode == MATCH_SYMBOL_EXACTLY) {
	if ( IsNonNULL(pCurrentBTN) && IsHashHeader(pCurrentBTN) )
	  pCurrentBTN = variableChain =
	    BTHT_BucketArray((BTHTptr)pCurrentBTN)[TRIEVAR_BUCKET];
	else
	  variableChain = pCurrentBTN;

	if ( ! CallVar_IsMarked(subterm) ) {
	  /*
	   *  The subterm is a call variable that has not yet been seen
	   *  (and hence is not tagged).  Therefore, it can only be paired
	   *  with an unbound trievar, and there can only be one of these
	   *  in a chain.  If we find it, apply the unification, mark the
	   *  callvar, trail them both, and continue.  Otherwise, fail.
	   *  Note we don't need to lay a CPF since this is the only
	   *  possible pairing that could result.
	   */
	  while( IsNonNULL(pCurrentBTN) ) {
	    if ( IsTrieVar(BTN_Symbol(pCurrentBTN)) &&
		 IsNewTrieVar(BTN_Symbol(pCurrentBTN)) ) {
	      trievar_index = DecodeTrieVar(BTN_Symbol(pCurrentBTN));
	      TrieVar_BindToSubterm(trievar_index,subterm);
	      CallVar_MarkIt(subterm,trievar_index);
	      Descend_In_Trie_and_Continue(pCurrentBTN);
	    }
	    pCurrentBTN = BTN_Sibling(pCurrentBTN);
	  }
	  SetNoVariant(pParentBTN);
	  break;     /* no pairing, so backtrack */
	}
      }
      /*
       *  We could be in a forward or backward execution mode.  In either
       *  case, the call variable has been seen before, and we first look
       *  to pair this occurrence of the callvar with a trievar that was
       *  previously bound to this particular callvar.  Note that there
       *  could be several such trievars.  Once we have exhausted this
       *  possibility, either immediately or through backtracking, we then
       *  allow the binding of an unbound trievar to this callvar.
       */
      while ( IsNonNULL(pCurrentBTN) ) {
	if ( IsTrieVar(BTN_Symbol(pCurrentBTN)) &&
	     ! IsNewTrieVar(BTN_Symbol(pCurrentBTN)) ) {
	  trievar_index = DecodeTrieVar(BTN_Symbol(pCurrentBTN));
	  if (identical_subterms(TrieVarBindings[trievar_index],subterm)) {
	    Create_ChoicePointFrame(BTN_Sibling(pCurrentBTN),variableChain);
	    Descend_In_Trie_and_Continue(pCurrentBTN);
	  }
	}
	pCurrentBTN = BTN_Sibling(pCurrentBTN);
      }
      /*
       *  We may have arrived here under several circumstances, but notice
       *  that the path we are on cannot be a variant one.  In case the
       *  possibility of a variant entry being present was still viable up
       *  to now, we save state info in case we need to create a variant
       *  entry later.  We now go to our last alternative, that of
       *  checking for an unbound trievar to pair with the marked callvar.
       *  If one is found, we trail the trievar, create the binding, and
       *  continue.  No CPF need be created since there can be at most one
       *  new trievar below any given node.
       */
      SetNoVariant(pParentBTN);

      while( IsNonNULL(variableChain) ) {
	if ( IsTrieVar(BTN_Symbol(variableChain)) &&
	     IsNewTrieVar(BTN_Symbol(variableChain)) ) {
	  trievar_index = DecodeTrieVar(BTN_Symbol(variableChain));
	  TrieVar_BindToMarkedCallVar(trievar_index,subterm);
	  Descend_In_Trie_and_Continue(variableChain);
	}
	variableChain = BTN_Sibling(variableChain);
      }
      break;


    /* SUBTERM HAS UNKNOWN CELL TAG
       ---------------------------- */
    default:
      Sub_CallChkIns_Error("Subterm with bogus tag!");
      break;
    } /* END switch(subterm_tag) */

    /*
     *  We've reached a dead-end since we were unable to match the current
     *  subterm to a trie node.  Therefore, we backtrack to continue the
     *  search, or, if there are no more choice point frames--in which
     *  case the trie has been completely searched--a variant entry is
     *  inserted into the call trie.
     */

    if (! CPStack_IsEmpty)
      CPStack_PopFrame
    else {
      NumSubOps_ProducerCall++;
      Trail_Unwind_All;
      CallLUR_Subsumer(*results) = NULL;  /* no subsumer, so no subg_frame */
      CallLUR_Leaf(*results) = construct_variant_call(YES,&answer_template);
      CallLUR_VariantFound(*results) = NO;
      CallLUR_VarVector(*results) = answer_template;
#ifdef DEBUG_CALL_CHK_INS
      if ( strcmp(targetGN,goal_name) == 0 ) {
	printf("New Producer Goal: ");
	triePrintPath(CallLUR_Leaf(*results),NO);
	printf("\n");
      }
#endif
      return;
    }

  } /* END while( ! TermStack_IsEmpty ) */

  /*
   *  The TermStack is empty, so we've reached a leaf node whose
   *  corresponding term subsumes the current call.  If this subsuming
   *  call maintains its own answer set, then this call can consume from
   *  it.  Otherwise, this subsuming call is itself subsumed and is
   *  consuming from some producer.  The new call will then consume from
   *  this producer, too.  However, the computed answer template was for
   *  the found subsuming call, not the one from which consumption will
   *  occur.  Therefore, the template must be recomputed.  In either case,
   *  if no variant was found AND the subsuming call is incomplete, an
   *  entry is created in the Call Trie.
   */

  /* Set Correct Answer Template
     --------------------------- */
  sf_with_table = CallTrieLeaf_GetSF(pParentBTN);
  if ( subg_producer(sf_with_table) == sf_with_table ) {
#ifdef DEBUG_CALL_CHK_INS
    if ( strcmp(targetGN,goal_name) == 0 ) {
      printf("Found producer:\n  ");
      sfPrintGoal(sf_with_table,YES);
      printf("\nWith  ");   /* continue after this 'else' */
    }
#endif
    if ( is_completed(sf_with_table) )
      NumSubOps_CallToCompletedTable++;
    else {
      if ( variant_path == YES )
	NumSubOps_VariantCall++;
      else
	NumSubOps_SubsumedCall++;
    }
    SetAnswerTemplate(answer_template);
    Trail_Unwind_All;
  }
  else {
#ifdef DEBUG_CALL_CHK_INS
    if ( strcmp(targetGN,goal_name) == 0 ) {
      printf("Found entry without own answer table:\n  ");
      sfPrintGoal(sf_with_table,YES);
      printf("\nRecomputing template for:\n  ");
      sfPrintGoal(subg_AnsTblSGF(sf_with_table),YES);
      printf("\n");   /* continue with A.T. print, below */
    }
#endif
    sf_with_table = subg_producer(sf_with_table);
    if ( is_completed(sf_with_table) )
      NumSubOps_CallToCompletedTable++;
    else
      NumSubOps_SubsumedCall++;
    Trail_Unwind_All;
    answer_template =
      compute_answer_template(callStruct, sf_with_table, answer_template);
  }

#ifdef DEBUG_CALL_CHK_INS
  if ( strcmp(targetGN,goal_name) == 0 )
    printAnswerTemplate(answer_template + *answer_template, *answer_template);
#endif
  CallLUR_Subsumer(*results) = sf_with_table;
  CallLUR_Leaf(*results) = pParentBTN;
  CallLUR_VariantFound(*results) = variant_path;
  CallLUR_VarVector(*results) = answer_template;

  /* Conditionally Create Call Entry
     ------------------------------- */
  if ( (variant_path == NO) && (! is_completed(sf_with_table)) ) {
    NumSubOps_SubsumedCallEntry++;
    CallLUR_Leaf(*results) = construct_variant_call(NO,&answer_template);
#ifdef DEBUG_CALL_CHK_INS
    if ( strcmp(targetGN,goal_name) == 0 ) {
      printf("Constructed new Call entry:\n  ");
      triePrintPath(CallLUR_Leaf(*results),NO);
      printf("\n");
    }
#endif
  }
}
