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


#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>
#include <stdlib.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "psc_xsb.h"
#include "trie_internals.h"
#include "macro_xsb.h"
#include "error_xsb.h"
#include "thread_xsb.h"
#include "memory_xsb.h"

extern BTHTptr hhadded;


/* Freeing Individual Structures
   ----------------------------- */

static void free_btn(CTXTdeclc BTNptr btn) {
  SM_DeallocateStruct(smTableBTN,btn);
}

static void free_btht(CTXTdeclc BTHTptr btht) {
  TrieHT_RemoveFromAllocList(smTableBTHT,btht);
  SM_DeallocateStruct(smTableBTHT,btht);
}

static void free_tstn(CTXTdeclc TSTNptr tstn) {
  SM_DeallocateStruct(smTSTN,tstn);
}

static void free_tstht(CTXTdeclc TSTHTptr tstht) {
  TrieHT_RemoveFromAllocList(smTSTHT,tstht);
  SM_DeallocateStruct(smTSTHT,tstht);
}

static void free_tsi(CTXTdeclc TSTHTptr tstht) {
  if ( IsNonNULL(TSTHT_IndexHead(tstht)) )
    SM_DeallocateStructList(smTSIN,TSTHT_IndexTail(tstht),
			    TSTHT_IndexHead(tstht));
}

static void free_producer_sf(CTXTdeclc VariantSF sf) {
  FreeProducerSF(sf);
}

static void free_consumer_sf(CTXTdeclc VariantSF sf) {
  SM_DeallocateStruct(smConsSF,sf);
}

/*
 * Answer List of a Consumer may already be completely deallocated, even
 * the dummy node.
 */
static void free_al(CTXTdeclc VariantSF sf) {
  if ( IsNonNULL(subg_ans_list_ptr(sf)) )
    free_answer_list(sf);
}


/* Deleting Structures with Substructures
   -------------------------------------- */

static void delete_btht(CTXTdeclc BTHTptr btht) {
  mem_dealloc(BTHT_BucketArray(btht),BTHT_NumBuckets(btht)*sizeof(void *),TABLE_SPACE);
  free_btht(CTXTc btht);
}

static void delete_tstht(CTXTdeclc TSTHTptr tstht) {
  mem_dealloc(BTHT_BucketArray(tstht),BTHT_NumBuckets(tstht)*sizeof(void *),TABLE_SPACE);
  free_tsi(CTXTc tstht);
  free_tstht(CTXTc tstht);
}

static void delete_sf(CTXTdeclc VariantSF sf) {
  free_al(CTXTc sf);
  if ( IsProducingSubgoal(sf) )
    free_producer_sf(CTXTc sf);
  else
    free_consumer_sf(CTXTc sf);
}

/*-----------------------------------------------------------------------*/

/*
 *  Delete the given TST Answer Set node and recursively all of
 *  its children.
 */

static void delete_tst_answer_set(CTXTdeclc TSTNptr root) {

  TSTNptr current, sibling;
  TSTHTptr hash_hdr;
  unsigned int i;


  if ( IsNULL(root) )
    return;

  /* I inserted the check for TSTN_Child(root) below to avoid
     a segmentation fault. It seems to be working fine with it, 
     and there doesn't seem to be any memory leak (wrt abolishing
     subsumptive tables), but as I don't know the code, I'd feel
     better if somebody who did looked at it.       -- lfcastro */

  if ( TSTN_Child(root) && IsHashHeader(TSTN_Child(root)) ) {
    hash_hdr = TSTN_GetHashHdr(root);
    for ( i = 0;  i < TSTHT_NumBuckets(hash_hdr);  i++ )
      for ( current = TSTHT_BucketArray(hash_hdr)[i];
	    IsNonNULL(current);  current = sibling ) {
	sibling = TSTN_Sibling(current);
	delete_tst_answer_set(CTXTc current);
      }
    delete_tstht(CTXTc hash_hdr);
  }
  else if ( ! IsLeafNode(root) )
    for ( current = TSTN_Child(root);  IsNonNULL(current);
	  current = sibling ) {
      sibling = TSTN_Sibling(current);
      delete_tst_answer_set(CTXTc current);
    }
  free_tstn(CTXTc root);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Delete the given Call Table node and recursively all of its children.
 */

void delete_call_index(CTXTdeclc BTNptr root) {

  BTNptr current, sibling;
  BTHTptr hash_hdr;
  unsigned int i;

  if ( IsNULL(root) )
    return;

  if ( ! IsLeafNode(root) ) {
    if ( IsHashHeader(BTN_Child(root)) ) {

      hash_hdr = BTN_GetHashHdr(root);
      for ( i = 0;  i < BTHT_NumBuckets(hash_hdr);  i++ )
	for ( current = BTHT_BucketArray(hash_hdr)[i];
	      IsNonNULL(current);  current = sibling ) {
	  sibling = BTN_Sibling(current);
	  delete_call_index(CTXTc current);
	}
      delete_btht(CTXTc hash_hdr);
    }
    else 
      for ( current = BTN_Child(root);  IsNonNULL(current);
	    current = sibling ) {
	sibling = BTN_Sibling(current);
	delete_call_index(CTXTc current);
      }
  }
  free_btn(CTXTc root);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void delete_subsumptive_table(CTXTdeclc TIFptr tif) {

  SubProdSF cur_prod, next_prod;
  SubConsSF cur_cons, next_cons;

  for ( cur_prod = (SubProdSF)TIF_Subgoals(tif);
	IsNonNULL(cur_prod);  cur_prod = next_prod ) {
    for ( cur_cons = subg_consumers(cur_prod);
	  IsNonNULL(cur_cons);  cur_cons = next_cons ) {
      next_cons = conssf_consumers(cur_cons);
      delete_sf(CTXTc (VariantSF)cur_cons);
    }
    next_prod = (SubProdSF)subg_next_subgoal(cur_prod);
    delete_tst_answer_set(CTXTc (TSTNptr)subg_ans_root_ptr(cur_prod));
    delete_sf(CTXTc (VariantSF)cur_prod);
  }
  delete_call_index(CTXTc TIF_CallTrie(tif));
}
