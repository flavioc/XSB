/* File:      sub_delete.c
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


#include "configs/xsb_config.h"
#include "debugs/xsb_debug.h"

#include <stdio.h>
#include <stdlib.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "psc_xsb.h"
#include "trie_internals.h"
#include "macro_xsb.h"
#include "error_xsb.h"



/* Freeing Individual Structures
   ----------------------------- */

static void free_btn(BTNptr btn) {
  SM_DeallocateStruct(smTableBTN,btn);
}

static void free_btht(BTHTptr btht) {
  TrieHT_RemoveFromAllocList(smTableBTHT,btht);
  SM_DeallocateStruct(smTableBTHT,btht);
}

static void free_tstn(TSTNptr tstn) {
  SM_DeallocateStruct(smTSTN,tstn);
}

static void free_tstht(TSTHTptr tstht) {
  TrieHT_RemoveFromAllocList(smTSTHT,tstht);
  SM_DeallocateStruct(smTSTHT,tstht);
}

static void free_tsi(TSTHTptr tstht) {
  if ( IsNonNULL(TSTHT_HeadEntry(tstht)) )
    SM_DeallocateStructList(smEntry,TSTHT_TailEntry(tstht),
			    TSTHT_HeadEntry(tstht));
}

static void free_producer_sf(SGFrame sf) {
  FreeProducerSF(sf);
}

static void free_consumer_sf(SGFrame sf) {
  SM_DeallocateStruct(smSF,sf);
}

/*
 * Answer List of a Consumer may already be completely deallocated, even
 * the dummy node.
 */
static void free_al(SGFrame sf) {
  if ( IsNonNULL(subg_ans_list_ptr(sf)) )
    free_answer_list(sf);
}


/* Deleting Structures with Substructures
   -------------------------------------- */

static void delete_btht(BTHTptr btht) {
  free(BTHT_BucketArray(btht));
  free_btht(btht);
}

static void delete_tstht(TSTHTptr tstht) {
  free(BTHT_BucketArray(tstht));
  free_tsi(tstht);
  free_tstht(tstht);
}

static void delete_sf(SGFrame sf) {
  free_al(sf);
  if ( IsProducerSF(sf) )
    free_producer_sf(sf);
  else
    free_consumer_sf(sf);
}

/*-----------------------------------------------------------------------*/

/* Deleting Branches of Subsumptive Tables
   --------------------------------------- */
/*
 *  Delete the given TST Answer Table node and recursively all of
 *  its children.
 */

static void delete_tst_answer_set(TSTNptr root) {

  TSTNptr current, sibling;
  TSTHTptr hash_hdr;
  int i;


  if ( IsNULL(root) )
    return;

  if ( ! IsLeafNode(root) ) {
    if ( IsHashHeader(TSTN_Child(root)) ) {
    hash_hdr = TSTN_GetHashHdr(root);
    for ( i = 0;  i < TSTHT_NumBuckets(hash_hdr);  i++ )
      for ( current = TSTHT_BucketArray(hash_hdr)[i];
	    IsNonNULL(current);  current = sibling ) {
	sibling = TSTN_Sibling(current);
	delete_tst_answer_set(current);
      }
    delete_tstht(hash_hdr);
    }
    else
      for ( current = TSTN_Child(root);  IsNonNULL(current);
	    current = sibling ) {
	sibling = TSTN_Sibling(current);
	delete_tst_answer_set(current);
      }
  }
  free_tstn(root);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Delete the given subsumptive Call Table node and recursively all
 *  of its children.
 */

void delete_subsumptive_table(BTNptr root) {

  BTNptr current, sibling;
  BTHTptr hash_hdr;
  int i;
  SGFrame sf;


  if ( IsNULL(root) )
    return;

  if ( IsLeafNode(root) ) {
    sf = CallTrieLeaf_GetSF(root);
    delete_tst_answer_set((TSTNptr)subg_ans_root_ptr(sf));
    delete_sf(sf);
  }
  else {
    if ( IsHashHeader(BTN_Child(root)) ) {
      hash_hdr = BTN_GetHashHdr(root);
      for ( i = 0;  i < BTHT_NumBuckets(hash_hdr);  i++ )
	for ( current = BTHT_BucketArray(hash_hdr)[i];
	      IsNonNULL(current);  current = sibling ) {
	  sibling = BTN_Sibling(current);
	  delete_subsumptive_table(current);
	}
      delete_btht(hash_hdr);
    }
    else
      for ( current = BTN_Child(root);  IsNonNULL(current);
	    current = sibling ) {
	sibling = BTN_Sibling(current);
	delete_subsumptive_table(current);
      }
  }
  free_btn(root);
}
