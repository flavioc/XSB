/* File:      tst_insert.c
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

#include <stdio.h>
#include <stdlib.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "inst_xsb.h"
#include "register.h"
#include "error_xsb.h"
#include "psc_xsb.h"
#include "deref.h"
#include "table_stats.h"
#include "trie_internals.h"
#include "macro_xsb.h"
#include "tst_aux.h"


/*=========================================================================*/

/*
 *                  TSI Creation and Access Methods
 *                  ===============================
 */


/*
 *  Allocate a TS_IndexNode, associate it with a TSTN 'tstn', and place
 *  it at the head of the TS Index managed by the hash table 'ht'.  This
 *  operation is used for symbols newly inserted into an established
 *  hash table.  The timestamp of this new entry will be set at the end
 *  of the subsumptive_answer_search operation when we walk back up the
 *  trie adjusting all timestamps to a new max as we go.  Hence, the
 *  head of the entry list is where this new entry belongs.
 */

inline static TSINptr tsiHeadInsert(TSTHTptr ht, TSTNptr tstn) {

  TSINptr pTSIN;

  New_TSIN(pTSIN, tstn);
  TSIN_Prev(pTSIN) = NULL;
  TSIN_Next(pTSIN) = TSTHT_IndexHead(ht);
  TSIN_Prev(TSTHT_IndexHead(ht)) = pTSIN;
  TSTHT_IndexHead(ht) = pTSIN;
  return pTSIN;
}
    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Used during the creation of a tsi.  Allocate a TS_IndexNode for a
 *  TS_TrieNode and insert it into the TSI in (decreasing) timestamp order.
 *
 *  NOTE: We cannot assume that the time stamp of the incoming node is  
 *  greater than that of all of the nodes already present in the TSI.
 *  Although this is the norm once the TSI is established, when a
 *  sibling list is moved to a hashing format, Entries are created for
 *  the nodes one at a time, but this processing order is not
 *  guaranteed to coincide with time stamp order.
 */

inline static TSINptr tsiOrderedInsert(TSTHTptr ht, TSTNptr tstn) {

  TSINptr newTSIN;      /* To be inserted after pTSIN */
  TSINptr pTSIN;        /* Steps thru each TSIN inspecting time stamp */


  New_TSIN(newTSIN, tstn);

  /* Determine proper position for insertion
     --------------------------------------- */
  pTSIN = TSTHT_IndexHead(ht);
  while ( IsNonNULL(pTSIN) &&
	 (TSIN_TimeStamp(newTSIN) < TSIN_TimeStamp(pTSIN)) )
    pTSIN = TSIN_Next(pTSIN);


  /* Splice newTSIN between pTSIN and its predecessor
     -------------------------------------------------- */
  if ( IsNonNULL(pTSIN) ) {
    TSIN_Prev(newTSIN) = TSIN_Prev(pTSIN);
    TSIN_Next(newTSIN) = pTSIN;
    if ( IsTSindexHead(pTSIN) )
      TSTHT_IndexHead(ht) = newTSIN;
    else
      TSIN_Next(TSIN_Prev(pTSIN)) = newTSIN;
    TSIN_Prev(pTSIN) = newTSIN;
  }
  else {   /* Insertion is at the end of the TSIN list */
    TSIN_Prev(newTSIN) = TSTHT_IndexTail(ht);
    TSIN_Next(newTSIN) = NULL;
    if ( IsNULL(TSTHT_IndexHead(ht)) )  /* First insertion into TSI */
      TSTHT_IndexHead(ht) = newTSIN;
    else
      TSIN_Next(TSTHT_IndexTail(ht)) = newTSIN;
    TSTHT_IndexTail(ht) = newTSIN;
  }

  return(newTSIN);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef COMMENT	/* tsiRemoveEntry is not used anywhere now */
/*
 *  Remove a TS_IndexNode from a tsi and place it on the global TSI free
 *  list for later reuse.
 */

static void tsiRemoveEntry(TSTHTptr ht, TSINptr tsin) {

  /* Splice out the TSIN from the Index
     ---------------------------------- */
  if ( IsTSindexHead(tsin) )
    TSTHT_IndexHead(ht) = TSIN_Next(tsin);
  else
    TSIN_Next(TSIN_Prev(tsin)) = TSIN_Next(tsin);
  if ( IsTSindexTail(tsin) )
    TSTHT_IndexTail(ht) = TSIN_Prev(tsin);
  else
    TSIN_Prev(TSIN_Next(tsin)) = TSIN_Prev(tsin);

  /* Place the TSIN on the FreeList
     ------------------------------- */
  SM_DeallocateStruct(smTSIN,tsin);
}
#endif /* COMMENT */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Increase the time stamp of a hashed TSTN to that which is greater
 *  than any other.  Hence, its TSIN must be moved to the head of
 *  the list to maintain our ordering property.
 */

inline static void tsiPromoteEntry(TSTNptr tstn, TimeStamp ts) {

  TSINptr tsin;
  TSTHTptr ht;

  tsin = TSTN_GetTSIN(tstn);
  TSIN_TimeStamp(tsin) = ts;
  if ( IsTSindexHead(tsin) )
    return;

  /* Splice out the TSIN from the Index
     ---------------------------------- */
  ht = TSTN_GetHashHdr(TSTN_Parent(tstn));
  TSIN_Next(TSIN_Prev(tsin)) = TSIN_Next(tsin);
  if ( IsTSindexTail(tsin) )
    TSTHT_IndexTail(ht) = TSIN_Prev(tsin);
  else
    TSIN_Prev(TSIN_Next(tsin)) = TSIN_Prev(tsin);

  /* Place the TSIN at the head of the Index
     --------------------------------------- */ 
  TSIN_Prev(tsin) = NULL;
  TSIN_Next(tsin) = TSTHT_IndexHead(ht);
  TSIN_Prev(TSTHT_IndexHead(ht)) = tsin;
  TSTHT_IndexHead(ht) = tsin;
}

/* ---------------------------------------------------------------------- */

/*
 *              TST Hash Table Creation and Access Methods
 *              ==========================================
 */

/*
 *  The number of children of 'parent' has increased beyond the threshold
 *  and requires a hashing structure.  This function creates a hash table
 *  and inserts the children into it.  If the subgoal to which this
 *  answer set belongs properly subsumes other calls, then a TSI is also
 *  created for the children.  The header of the hash table is then
 *  referenced through the `Child' filed of the parent.  Hash tables in a
 *  TST are also linked to one another through the TST's root.
 */

inline static void tstnHashifyChildren(TSTNptr parent, TSTNptr root,
				       xsbBool needTSI) {

  TSTNptr children;           /* child list of the parent */
  TSTNptr tstn;               /* current child for processing */
  TSTHTptr ht;                /* HT header struct */
  TSTNptr *tablebase;         /* first bucket of allocated HT */
  unsigned long  hashseed;    /* for inserting TSTNs into the HT */


  New_TSTHT(ht,TS_ANSWER_TRIE_TT,root);
  children = TSTN_Child(parent);
  TSTN_SetHashHdr(parent,ht);
  tablebase = TSTHT_BucketArray(ht);
  hashseed = TSTHT_GetHashSeed(ht);
  for (tstn = children;  IsNonNULL(tstn);  tstn = children) {
    children = TSTN_Sibling(tstn);
    TrieHT_InsertNode(tablebase, hashseed, tstn);
    MakeHashedNode(tstn);
    if ( needTSI )
      TSTN_SetTSIN(tstn, tsiOrderedInsert(ht, tstn));
  }
}

/*-------------------------------------------------------------------------*/

/*
 *                     Inserting a Single Symbol
 *                     =========================
 */

/*
 *  Search among the children of `parent', which are maintained in a
 *  hash table, for a node containing `symbol'.  If one does not exist,
 *  then create a TSTN containing this symbol.
 *
 *  Return a pointer to the node containing `symbol'.  Set the flag
 *  appropriately, stating whether the node returned was just created.
 */

inline static TSTNptr tsthtInsertSymbol(TSTNptr parent, Cell symbol,
					xsbBool needTSI, xsbBool *is_new) {

  TSTHTptr ht;
  TSTNptr tstn, chain, *bucket;
  int chain_length;


  ht = TSTN_GetHashHdr(parent);
  bucket = CalculateBucketForSymbol(ht,symbol);
  tstn = chain = *bucket;
  SearchChainForSymbol(tstn,symbol,chain_length);
  if ( IsNULL(tstn) ) {
    *is_new = TRUE;
    TSTHT_NumContents(ht)++;
#ifdef SHOW_HASHTABLE_ADDITIONS
    printf("Hash Table size is %lu and now contains %lu elements.\n",
	   TSTHT_NumBuckets(ht), TSTHT_NumContents(ht));
    printf("Addition being made to bucket %lu; now has length %d.\n",
	   TrieHash(symbol, TSTHT_NumBuckets(ht) - 1), chain_length+1);
#endif
    New_TSTN(tstn,TS_ANSWER_TRIE_TT,HASHED_INTERRIOR_NT,symbol,parent,chain);
    if ( needTSI )
      TSTN_SetTSIN(tstn, tsiHeadInsert(ht,tstn));
    *bucket = tstn;
    chain_length++;  /* total number of nodes now inhabiting this bucket */
    TrieHT_ExpansionCheck(ht,chain_length);
  }
  else
    *is_new = FALSE;

  return tstn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Search the children (there is at least one) of TSTN `parent' for a
 *  node containing `symbol'.  If one does not exist, then create a
 *  TSTN containing this symbol.  If the sibling chain is not already
 *  "too long", then insert the new node as the first child in the
 *  chain.  Otherwise, create a hashing environment for the parent's
 *  children.  In either case, return a pointer to the node containing
 *  `symbol'.  Set the flag appropriately, stating whether the node
 *  returned was just created.
 */

inline static TSTNptr tstnInsertSymbol(TSTNptr parent, Cell symbol,
				       TSTNptr root, xsbBool needTSI,
				       xsbBool *is_new) {

  TSTNptr tstn, chain;
  int chain_length;

  tstn = chain = TSTN_Child(parent);
  SearchChainForSymbol(tstn,symbol,chain_length);
  if ( IsNULL(tstn) ) {
    *is_new = TRUE;
    New_TSTN(tstn,TS_ANSWER_TRIE_TT,INTERRIOR_NT,symbol,parent,chain);
    TSTN_Child(parent) = tstn;
    chain_length++;
    if ( IsLongSiblingChain(chain_length) )
      tstnHashifyChildren(parent,root,needTSI);
  }
  else
    *is_new = FALSE;
  return tstn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Create a new child for TSTN 'parent', which currently has no children,
 *  containing symbol 'symbol'.  Optimizes away some unnecessary checks of
 *  tstnInsertSymbol.
 */

inline static TSTNptr tstnAddSymbol(TSTNptr parent, Cell symbol) {

  TSTNptr newTSTN;

  New_TSTN(newTSTN,TS_ANSWER_TRIE_TT,INTERRIOR_NT,symbol,parent,NULL);
  TSTN_Child(parent) = newTSTN;
  return (newTSTN);
}

/*-------------------------------------------------------------------------*/

/*
 *                           Helper Utilities
 *                           ================
 */

/* Create an Empty Answer Set
   -------------------------- */

inline static void *newAnswerTST(int arity) {

  TSTNptr root;

  New_TSTN( root, TS_ANSWER_TRIE_TT, TRIE_ROOT_NT,
	    EncodeTriePSC(get_ret_psc(arity)), NULL, NULL );
  TSTN_TimeStamp(root) = EMPTY_TST_TIMESTAMP;
  return root;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Given a pointer to a leaf TSTN for a newly inserted set of terms,
 *  update the timestamps of all nodes lying along the path from this
 *  leaf to the root.  Updates effect the TSIs, if they exist.
 */

inline static void update_timestamps(TSTNptr leaf, TSTNptr root,
				     TimeStamp ts, xsbBool containsTSIs) {

  if ( containsTSIs )
    do {
      if ( IsHashedNode(leaf) )
	tsiPromoteEntry(leaf, ts);
      else
	TSTN_TimeStamp(leaf) = ts;
      leaf = TSTN_Parent(leaf);
    } while ( leaf != root );
  else
    do {
      TSTN_TimeStamp(leaf) = ts;
      leaf = TSTN_Parent(leaf);
    } while ( leaf != root );
  TSTN_TimeStamp(root) = ts;
}

/*-------------------------------------------------------------------------*/

/*
 * Locate/insert the answer substitution, given as a size and a vector
 * terms stored in low-to-high memory fashion, into the answer set,
 * represented as a Time Stamp Trie, of a producing call.  If the
 * producer properly subsumes some issued call, then the time stamp
 * indices are updated in cases of insertion.  The time stamp to
 * associate with a new answer is stored in the subgopal frame.  Returns
 * a pointer to the TST leaf representing the substitution, and sets a
 * flag to indicate whether this substitution was new.
 */

TSTNptr subsumptive_answer_search(int nTerms, CPtr termVector,
				  SGFrame sfProducer, xsbBool *isNew) {

  TSTNptr tstRoot;          /* The root node of the TST answer set */

  TimeStamp tsNewAnswer;    /* Time stamp to assign to a new answer */

  xsbBool maintainTSI;      /* Whether indices have been created and need
			       to be maintained during insertion */

  TSTNptr pParentTSTN;      /* Used for stepping down through the trie */

  Cell symbol;		    /* Trie representation of current heap symbol,
			       used for matching/inserting into a TSTN */

  Cell subterm;		    /* Used for stepping through the term */

  int symbol_type,	    /* Type of current subterm */
      std_var_num;	    /* Next available TrieVar index; for standardizing
			       call variables when interned */



  NumSubOps_AnswerCheckInsert++;

#ifdef INTERN_DEBUG
  {
    int i;
    printf("Entered subsumptive_answer_search() with the following terms:\n");
    for (i = 0; i < nTerms; i++) {
      printf("\t");
      printterm((Cell)(termVector - i),1,25);
      printf("\n");
    }
  }
#endif

  if ( IsNULL(subg_ans_root_ptr(sfProducer)) )
    subg_ans_root_ptr(sfProducer) = newAnswerTST(nTerms);
  tstRoot = (TSTNptr)subg_ans_root_ptr(sfProducer);
  tsNewAnswer = subg_timestamp(sfProducer);

  if (nTerms == 0) {
    /* Create/Find an Escape Node
       -------------------------- */
    if ( IsNULL(TSTN_Child(tstRoot)) ) {
      NumSubOps_AnswerInsert++;
      CreateEscapeTSTN(pParentTSTN,TS_ANSWER_TRIE_TT,tstRoot);
      TSTN_TimeStamp(pParentTSTN) = TSTN_TimeStamp(tstRoot) = tsNewAnswer;
      TSTN_Child(tstRoot) = pParentTSTN;
      *isNew = TRUE;
      return pParentTSTN;
    }
    else if ( IsEscapeNode(TSTN_Child(tstRoot)) ) {
      *isNew = FALSE;
      return ( TSTN_Child(tstRoot) );
    }
    else
      xsb_abort("TST Answer Insertion\n"
		"Incorrect substitution size (%d): 0\n", nTerms);
  }

  /* Search the TST
     -------------- */
  pParentTSTN = tstRoot;
  maintainTSI = ProducerHasConsumers(sfProducer);
  *isNew = FALSE;
  std_var_num = 0;
  symbol = symbol_type = 0;     /* suppress compiler warning */
  Trail_ResetTOS;
  TermStack_ResetTOS;
  TermStack_PushHighToLowVector(termVector,nTerms);

  while ( ! TermStack_IsEmpty ) {
  #ifdef INTERN_DEBUG
    printf("TermStack contains %d terms\n",tstTermStack.top-tstTermStack.base);
  #endif
    subterm = TermStack_Pop;
    XSB_Deref(subterm);
    symbol_type = cell_tag(subterm);
    switch (symbol_type) {

    case XSB_REF:
    case XSB_REF1:
    #ifdef INTERN_DEBUG
      printf("Found variable: ");
      printterm(subterm, 1, 8);
      printf("\n");
    #endif
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
    #ifdef INTERN_DEBUG
      printf("Found literal (str, int, flt): ");
      printterm((Cell)subterm, 1, 8);
      printf(" (raw val: 0x%lx)\n", (Cell)subterm);
    #endif
      symbol = EncodeTrieConstant(subterm);
      break;

    case XSB_STRUCT:
    #ifdef INTERN_DEBUG
      printf("Found function symbol: ");
      printterm(subterm, 1, 8);
      printf("\n");
    #endif
      symbol = EncodeTrieFunctor(subterm);
      TermStack_PushFunctorArgs(subterm);
      break;

    case XSB_LIST:
    #ifdef INTERN_DEBUG
      printf("Found list: ");
      printterm((Cell)subterm, 1, 8);
      printf("\n");
    #endif
      symbol = EncodeTrieList(subterm);
      TermStack_PushListArgs(subterm);
      break;

    default: 
      Trail_Unwind_All;
      xsb_abort("TST Answer Insertion\n"
		"Bad tag in heap term.  Tag: %x   Value: %lx\n"
		"Time-stamped trie left in dubious state\n",
		symbol_type, subterm);
    }

    /* Determine the search/insert function to call */

    if ( IsNULL(TSTN_Child(pParentTSTN)) ) {
      *isNew = TRUE;
      pParentTSTN = tstnAddSymbol(pParentTSTN, symbol);
    }
    else if ( IsHashHeader(TSTN_Child(pParentTSTN)) )
      pParentTSTN = tsthtInsertSymbol(pParentTSTN, symbol, maintainTSI,
				      isNew);
    else
      pParentTSTN = tstnInsertSymbol(pParentTSTN, symbol, tstRoot,
				     maintainTSI, isNew);
  }

  Trail_Unwind_All;

  /* 'pParentTSTN' points to the leaf representing the set of given terms */

  if (*isNew) {
    NumSubOps_AnswerInsert++;
    update_timestamps(pParentTSTN,tstRoot,tsNewAnswer,maintainTSI);
    MakeLeafNode(pParentTSTN);
    TN_UpgradeInstrTypeToSUCCESS(pParentTSTN,symbol_type);
  }
  return(pParentTSTN);
}

/*--------------------------------------------------------------------------*/

/*
 *  To support lazy creation of TSIs for incomplete TST Answer Sets.
 *  TSIs are created once a properly subsumed subgoal is identified.
 */

void tstCreateStructures(TSTNptr pTST) {

  TSTNptr *pBucket, tstn;
  TSTHTptr ht;
  int bucketNum;

  if ( IsNULL(pTST) )
    return;

  /*** For each hash table ... ***/
  for ( ht = TSTRoot_GetHTList(pTST);  IsNonNULL(ht);
        ht = TSTHT_InternalLink(ht) ) {

    /*** For each bucket in this hash table ... ***/
    for ( pBucket = TSTHT_BucketArray(ht), bucketNum = 0;
	  (unsigned int)bucketNum < TSTHT_NumBuckets(ht);
	  pBucket++, bucketNum++ )

      /*** For each TSTN in a bucket... ***/
      for ( tstn = *pBucket;  IsNonNULL(tstn);  tstn = TSTN_Sibling(tstn) )

	/*** Create a TSIN for each symbol (TSTN) ***/
	TSTN_SetTSIN(tstn,tsiOrderedInsert(ht,tstn));
  }
}
