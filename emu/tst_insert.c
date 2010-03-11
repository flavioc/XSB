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

#include "debugs/debug_tries.h"

#include "auxlry.h"
#include "context.h"
#include "cell_xsb.h"
#include "inst_xsb.h"
#include "register.h"
#include "error_xsb.h"
#include "psc_xsb.h"
#include "deref.h"
#include "table_stats.h"
#include "trie_internals.h"
#include "tst_aux.h"
#include "thread_xsb.h"
#include "memory_xsb.h"



/*===========================================================================

    This file defines a set of functions for inserting a set of terms
    into different types of tries.  Note that these routines STRICTLY
    insert the terms -- NO searching is performed.

      BTNptr bt_insert(BTNptr,BTNptr,Cell)
      TSTNptr tst_insert(TSTNptr,TSTNptr,Cell,xsbBool)

    They are intended for internal use only by the trie search routines,
    which use them for term insert only after ensuring that some related
    term set doesn't already exist in the trie.

    These insertion functions assume a non-NULL node pointer, a nonempty
    set of terms, and appropriate initialization of the trie Trail.

===========================================================================*/


/*=========================================================================*/

/*
 *             Time-Stamp Index Creation and Access Methods
 *             ============================================
 *
 * Recall that Time-Stamp Indices are central to identifying unifying
 * term sets with time stamps greater than a given value.
 */

/*-------------------------------------------------------------------------*/

/*
 * Allocate a TSI node, associate it with a TST node 'tstn', and place
 * it at the head of the Time-Stamp Index managed by the hash table 'ht'.
 *
 * This operation is used for symbols inserted into an established hash
 * table.  The timestamp of this new entry will be set at the end of the
 * TST Insert operation when we walk back up the trie adjusting all
 * timestamps to a new max as we go.  Hence, the head of the entry list
 * is where this new entry belongs.
 */

inline static  TSINptr tsiHeadInsert(CTXTdeclc TSTHTptr ht, TSTNptr tstn) {

  TSINptr pTSIN;

  New_TSIN(pTSIN, tstn);
  TSIN_Prev(pTSIN) = NULL;
  TSIN_Next(pTSIN) = TSTHT_IndexHead(ht);
  TSIN_Prev(TSTHT_IndexHead(ht)) = pTSIN;
  TSTHT_IndexHead(ht) = pTSIN;
  return pTSIN;
}
    
/*-------------------------------------------------------------------------*/

/*
 *  Used during the creation of a Time-Stamp Index, allocates a TSI node
 *  for a given TST node and inserts it into the TSI in (decreasing)
 *  timestamp order.
 *
 *  NOTE: We cannot assume that the time stamp of the incoming node is  
 *  greater than that of all of the nodes already present in the TSI.
 *  Although this is the norm once the TSI is established, when a
 *  sibling list is moved to a hashing format, Entries are created for
 *  the nodes one at a time, but this node-processing order is not
 *  guaranteed to coincide with time stamp order.
 */

inline static  TSINptr tsiOrderedInsert(CTXTdeclc TSTHTptr ht, TSTNptr tstn) {

  TSINptr nextTSIN;     /* Steps thru each TSIN inspecting time stamp */
  TSINptr newTSIN;      /* To be inserted after nextTSIN */


  New_TSIN(newTSIN, tstn);

  /* Determine proper position for insertion
     --------------------------------------- */
  nextTSIN = TSTHT_IndexHead(ht);
  while ( IsNonNULL(nextTSIN) &&
	 (TSIN_TimeStamp(newTSIN) < TSIN_TimeStamp(nextTSIN)) )
    nextTSIN = TSIN_Next(nextTSIN);


  /* Splice newTSIN between nextTSIN and its predecessor
     --------------------------------------------------- */
  if ( IsNonNULL(nextTSIN) ) {
    TSIN_Prev(newTSIN) = TSIN_Prev(nextTSIN);
    TSIN_Next(newTSIN) = nextTSIN;
    if ( IsTSindexHead(nextTSIN) )
      TSTHT_IndexHead(ht) = newTSIN;
    else
      TSIN_Next(TSIN_Prev(nextTSIN)) = newTSIN;
    TSIN_Prev(nextTSIN) = newTSIN;
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

  return newTSIN;
}

/*-------------------------------------------------------------------------*/

/*
 * Remove a TSI entry node from a TSI and place it on the global TSI
 * free list for later reuse.
 */

void tsiRemoveEntry(CTXTdeclc TSTHTptr ht, TSINptr tsin) {

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

/*-------------------------------------------------------------------------*/

/*
 * Increase the time stamp of a hashed TSTN to that which is greater
 * than any other.  Hence, its TSI entry must be moved to the head of
 * the list to maintain our ordering property.
 */

inline static  void tsiPromoteEntry(TSTNptr tstn, TimeStamp ts) {

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

/*--------------------------------------------------------------------------*/

/*
 * This function may be called externally, and is made available to
 * support lazy creation of Time-Stamp Indices.
 *
 * An example of this use is for incomplete subsumptive Answer Sets.
 * TSIs are created only once a properly subsumed subgoal is issued.
 */

void tstCreateTSIs(CTXTdeclc TSTNptr pTST) {

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
	TSTN_SetTSIN(tstn,tsiOrderedInsert(CTXTc ht,tstn));
  }
}

/*=========================================================================*/

/*
 *			TST Hash Table Creation
 *			=======================
 */


/*
 * The number of children of 'parent' has increased beyond the threshold
 * and requires a hashing structure.  This function creates a hash table
 * and inserts the children into it.  The value of the third argument
 * determines whether a TSI is also created for the children.
 *
 * After its creation, the hash table is referenced through the `Child'
 * field of the parent.  Hash tables in a TST are also linked to one
 * another through the TST's root.
 */

inline static
void tstnHashifyChildren(CTXTdeclc TSTNptr parent, TSTNptr root, xsbBool createTSI) {

  TSTNptr children;           /* child list of the parent */
  TSTNptr tstn;               /* current child for processing */
  TSTHTptr ht;                /* HT header struct */
  TSTNptr *tablebase;         /* first bucket of allocated HT */
  unsigned long  hashseed;    /* for hashing symbols of the TSTNs */


  New_TSTHT(ht,TSTN_TrieType(root),root);
  children = TSTN_Child(parent);
  TSTN_SetHashHdr(parent,ht);
  tablebase = TSTHT_BucketArray(ht);
  hashseed = TSTHT_GetHashSeed(ht);
  for (tstn = children;  IsNonNULL(tstn);  tstn = children) {
    children = TSTN_Sibling(tstn);
    TrieHT_InsertNode(tablebase, hashseed, tstn);
    MakeHashedNode(tstn);
    if ( createTSI )
      TSTN_SetTSIN(tstn, tsiOrderedInsert(CTXTc ht, tstn));
  }
}

/*=========================================================================*/

/*
 *                    Inserting a Symbol into a Trie
 *                    ==============================
 */


/*-------------------------------------------------------------------------*/

/*
 * Adds a node containing 'symbol' below 'parent', which currently has
 * no children.  Optimizes away certain checks performed by the
 * xxxInsertSymbol() routines.
 */

inline static
TSTNptr tstnAddSymbol(CTXTdeclc TSTNptr parent, Cell *symbol, int trieType, int nodeType) {

  TSTNptr newTSTN;

  New_TSTN(newTSTN,trieType,nodeType,symbol,parent,NULL);
  TSTN_Child(parent) = newTSTN;
  return newTSTN;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

inline static
BTNptr btnAddSymbol(CTXTdeclc BTNptr parent, Cell symbol, int trieType) {

  BTNptr newBTN;

  New_BTN(newBTN,trieType,INTERIOR_NT,symbol,parent,NULL);
  BTN_Child(parent) = newBTN;
  return newBTN;
}

/*-------------------------------------------------------------------------*/

/*
 * Inserts a node containing 'symbol' at the head of the sibling chain
 * below 'parent' and returns a pointer to this node.  If this addition
 * causes the chain to become "too long", then creates a hashing
 * environment for the children.
 */

inline static
TSTNptr tstnInsertSymbol(CTXTdeclc TSTNptr parent, Cell *symbol, int trieType,
			 int nodeType, TSTNptr root, xsbBool createTSI) {

  TSTNptr tstn, chain;
  int chain_length;


  chain = TSTN_Child(parent);
  New_TSTN(tstn,trieType,nodeType,symbol,parent,chain);
  TSTN_Child(parent) = tstn;
  chain_length = 1;
  while ( IsNonNULL(chain) ) {
    chain_length++;
    chain = TSTN_Sibling(chain);
  }
  if ( IsLongSiblingChain(chain_length) )
    tstnHashifyChildren(CTXTc parent,root,createTSI);
  return tstn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

inline static
BTNptr btnInsertSymbol(CTXTdeclc BTNptr parent, Cell symbol, int trieType) {

  BTNptr btn, chain;
  int chain_length;


  chain = BTN_Child(parent);
  New_BTN(btn,trieType,INTERIOR_NT,symbol,parent,chain);
  BTN_Child(parent) = btn;
  chain_length = 1;
  while ( IsNonNULL(chain) ) {
    chain_length++;
    chain = BTN_Sibling(chain);
  }
  if ( IsLongSiblingChain(chain_length) )
    hashify_children(CTXTc parent,trieType);
  return btn;
}

/*-------------------------------------------------------------------------*/

/*
 * Inserts a node containing 'symbol' in the appropriate bucket of the
 * hash table maintained by 'parent' and returns a pointer to this node.
 * If this addition causes the chain to become "too long", then expand
 * the hash table.
 */

inline static
TSTNptr tsthtInsertSymbol(CTXTdeclc TSTNptr parent, Cell *symbol, int trieType,
			  int nodeType, xsbBool maintainsTSI) {

  TSTHTptr ht;
  TSTNptr tstn, chain, *bucket;
  int chain_length;

  ht = TSTN_GetHashHdr(parent);
#ifdef SUBSUMPTION_YAP
  bucket = CalculateBucketForSymbol(ht, GET_HASH_SYMBOL(symbol[0], nodeType));
#else
  bucket = CalculateBucketForSymbol(ht,symbol[0]);
#endif
  chain = *bucket;
  New_TSTN(tstn,trieType,HASHED_INTERIOR_NT | nodeType,symbol,parent,chain);
  *bucket = tstn;
  TSTHT_NumContents(ht)++;
  if ( maintainsTSI )
    TSTN_SetTSIN(tstn, tsiHeadInsert(CTXTc ht,tstn));
  chain_length = 1;
  while ( IsNonNULL(chain) ) {
    chain_length++;
    chain = TSTN_Sibling(chain);
  }

#ifdef SHOW_HASHTABLE_ADDITIONS
  xsb_dbgmsg((LOG_DEBUG,"Hash Table size is %lu and now contains %lu elements.",
	     TSTHT_NumBuckets(ht), TSTHT_NumContents(ht)));
  xsb_dbgmsg((LOG_DEBUG,"Addition being made to bucket %lu; now has length %d.",
	     TrieHash(symbol, TrieHT_GetHashSeed(ht)), chain_length));
#endif

  TrieHT_ExpansionCheck(ht,chain_length);
  return tstn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

inline static  BTNptr bthtInsertSymbol(CTXTdeclc BTNptr parent, Cell symbol,
				       int trieType) {

  BTHTptr ht;
  BTNptr btn, chain, *bucket;
  int chain_length;


  ht = BTN_GetHashHdr(parent);
  bucket = CalculateBucketForSymbol(ht,symbol);
  chain = *bucket;
  New_BTN(btn,trieType,HASHED_INTERIOR_NT,symbol,parent,chain);
  *bucket = btn;
  BTHT_NumContents(ht)++;
  chain_length = 1;
  while ( IsNonNULL(chain) ) {
    chain_length++;
    chain = BTN_Sibling(chain);
  }

#ifdef SHOW_HASHTABLE_ADDITIONS
  xsb_dbgmsg((LOG_DEBUG,"Hash Table size is %lu and now contains %lu elements.",
	     BTHT_NumBuckets(ht), BTHT_NumContents(ht)));
  xsb_dbgmsg((LOG_DEBUG,"Addition being made to bucket %lu; now has length %d.",
	     TrieHash(symbol, TrieHT_GetHashSeed(ht)), chain_length));
#endif

  TrieHT_ExpansionCheck(ht,chain_length);
  return btn;
}

/*=========================================================================*/

/*
 *                 Updating Time Stamps Along a New Path
 *                 =====================================
 *
 *  Given a pointer to a leaf TSTN for a newly inserted set of terms,
 *  update the timestamps of all nodes lying along the path from this
 *  leaf to the root.  Updates effect the TSIs, if they exist.
 */

inline static  void update_timestamps(TSTNptr tstLeaf, TSTNptr tstRoot,
				      xsbBool containsTSIs) {

  TimeStamp tsNewAnswer;


  tsNewAnswer = TSTN_TimeStamp(tstRoot) + 1;
  if ( containsTSIs )
    do {
      if ( IsHashedNode(tstLeaf) )
	tsiPromoteEntry(tstLeaf, tsNewAnswer);
      else
	TSTN_TimeStamp(tstLeaf) = tsNewAnswer;
      tstLeaf = TSTN_Parent(tstLeaf);
    } while ( tstLeaf != tstRoot );
  else
    do {
      TSTN_TimeStamp(tstLeaf) = tsNewAnswer;
      tstLeaf = TSTN_Parent(tstLeaf);
    } while ( tstLeaf != tstRoot );
  TSTN_TimeStamp(tstRoot) = tsNewAnswer;
}

/*=========================================================================*/

/*
 *			Term Insertion Into a Trie
 *			==========================
 *
 * Inserts a nonempty set of terms into a trie (a non-NULL root pointer)
 * below the node 'lastMatch'.  Note that if the trie is empty or if no
 * symbols were previously matched, then 'lastMatch' will reference the
 * root.  The terms to insert may exist in one of two forms:
 *
 *  1) The first symbol to be inserted is passed in as an argument, and
 *     the remaining terms are present on the TermStack.  (This supports
 *     the variant-lookup operation which processes a subterm BEFORE
 *     searching for its occurrence in the trie; by then, the stacks
 *     have been altered.)  In this case, the TermStack may be empty.
 *
 *  2) All terms to be inserted are present on the TermStack, in which
 *     case the symbol argument 'firstSymbol' has the value
 *     NO_INSERT_SYMBOL.  (This supports the subsumptive-lookup operation
 *     which processes a subterm only AFTER determining that it can be
 *     subsumed by a symbol in the trie.)  In this case, the TermStack is
 *     expected to contain at least one term.
 *
 * In either case, any trie variables already encountered along the path
 * from the root of the trie to 'lastMatch' have been standardized and
 * their bindings noted on the Trail.  After insertion is complete, the
 * leaf node representing the term set is returned.
 */

/*-------------------------------------------------------------------------*/

/*
 * If the TST contains Time-Stamp Indices -- as noted in the argument
 * 'maintainTSI' -- these need to be maintained during insertion.
 */

TSTNptr tst_insert(CTXTdeclc TSTNptr tstRoot, TSTNptr lastMatch, Cell firstSymbol,
		   xsbBool maintainTSI) {
#ifdef SUBSUMPTION_YAP
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT
  Cell symbol[2]; /* for floats */
#else
  Cell symbol[1];
#endif
#else
  Cell symbol[1];
#endif
  int std_var_num,
      trieType,
      nodeType = INTERIOR_NT;

  symbol[0] = firstSymbol;
  std_var_num = Trail_NumBindings;
  trieType = TSTN_TrieType(tstRoot);

  /* Insert initial symbol
     --------------------- */
  if ( firstSymbol == NO_INSERT_SYMBOL )
    ProcessNextSubtermFromTrieStacks(symbol[0],nodeType,std_var_num);

  if ( IsNULL(TSTN_Child(lastMatch)) )
    lastMatch = tstnAddSymbol(CTXTc lastMatch,symbol,trieType,nodeType);
  else if ( IsHashHeader(TSTN_Child(lastMatch)) )
    lastMatch = tsthtInsertSymbol(CTXTc lastMatch,symbol,trieType,nodeType,maintainTSI);
  else
    lastMatch = tstnInsertSymbol(CTXTc lastMatch,symbol,trieType,nodeType,
        tstRoot,maintainTSI);

  /* Insert remaining symbols
     ------------------------ */
  while ( ! TermStack_IsEmpty ) {
    ProcessNextSubtermFromTrieStacks(symbol[0],nodeType,std_var_num);
    lastMatch = tstnAddSymbol(CTXTc lastMatch,symbol,trieType,nodeType);
  }
  update_timestamps(lastMatch,tstRoot,maintainTSI);
  MakeLeafNode(lastMatch);
#ifdef SUBSUMPTION_XSB
  TN_UpgradeInstrTypeToSUCCESS(lastMatch,TrieSymbolType(symbol[0]));
#endif
  AnsVarCtr = AnsVarCtr + std_var_num;
  return lastMatch;
}

/*-------------------------------------------------------------------------*/


BTNptr bt_insert(CTXTdeclc BTNptr btRoot, BTNptr lastMatch, Cell firstSymbol) {

  Cell symbol;
  int std_var_num;
  int trieType;
  int nodeType;


  symbol = firstSymbol;
  std_var_num = Trail_NumBindings;
  trieType = BTN_TrieType(btRoot);

  /* Insert initial symbol
     --------------------- */
  if ( symbol == NO_INSERT_SYMBOL )
    ProcessNextSubtermFromTrieStacks(symbol,nodeType,std_var_num);

  if ( IsNULL(BTN_Child(lastMatch)) )
    lastMatch = btnAddSymbol(CTXTc lastMatch,symbol,trieType);
  else if ( IsHashHeader(BTN_Child(lastMatch)) )
    lastMatch = bthtInsertSymbol(CTXTc lastMatch,symbol,trieType);
  else
    lastMatch = btnInsertSymbol(CTXTc lastMatch,symbol,trieType);

  /* Insert remaining symbols
     ------------------------ */
  while ( ! TermStack_IsEmpty ) {
    ProcessNextSubtermFromTrieStacks(symbol,nodeType,std_var_num);
    lastMatch = btnAddSymbol(CTXTc lastMatch,symbol,trieType);
  }
  MakeLeafNode(lastMatch);
  TN_UpgradeInstrTypeToSUCCESS(lastMatch,TrieSymbolType(symbol));
  return lastMatch;
}

/*=========================================================================*/
