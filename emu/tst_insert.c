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
 *                    Inserting a Symbol into a Trie
 *                    ==============================
 */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

inline static
BTNptr btnAddSymbol(CTXTdeclc BTNptr parent, Cell symbol, int trieType) {

  BTNptr newBTN;

  New_BTN(newBTN,trieType,INTERIOR_NT,symbol,parent,NULL);
  BTN_Child(parent) = newBTN;
  return newBTN;
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

#include "xsb.tst.c"

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
