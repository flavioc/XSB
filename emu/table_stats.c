/* File:      table_stats.c
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


#include "configs/config.h"
#include "debugs/debug.h"

#include <stdio.h>

#include "auxlry.h"
#include "cell.h"
#include "psc.h"
#include "table_stats.h"
#include "trie_internals.h"
#include "xmacro.h"
#include "xsberror.h"



/*==========================================================================*/

/*
 *		  T A B L I N G   S T A T I S T I C S
 *		  ===================================
 */


/*
 *                      Recording Current Usage
 *                      -----------------------
 */


NodeStats btn_statistics() {

  NodeStats btn_stats;


  SM_CurrentCapacity(smTableBTN, NodeStats_NumBlocks(btn_stats),
		     NodeStats_NumAllocNodes(btn_stats));
  SM_CountFreeStructs(smTableBTN, NodeStats_NumFreeNodes(btn_stats));
  NodeStats_NodeSize(btn_stats) = sizeof(BasicTrieNode);

  return btn_stats;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Currently calculates for tabling resources.  Could parameterize for
   asserted/user resources */

HashStats btht_statistics() {

  HashStats ht_stats;
  counter num_used_hdrs;
  BTHTptr pBTHT;
  BTNptr *ppBTN;


  SM_CurrentCapacity(smTableBTHT, HashStats_NumBlocks(ht_stats),
		     HashStats_NumAllocHeaders(ht_stats));
  SM_CountFreeStructs(smTableBTHT, HashStats_NumFreeHeaders(ht_stats));
  HashStats_HeaderSize(ht_stats) = sizeof(BasicTrieHT);

  num_used_hdrs = 0;
  HashStats_NumBuckets(ht_stats) = 0;
  HashStats_TotalOccupancy(ht_stats) = 0;
  HashStats_NonEmptyBuckets(ht_stats) = 0;
  HashStats_BucketSize(ht_stats) = sizeof(BTNptr);
  pBTHT = SM_AllocList(smTableBTHT);
  while ( IsNonNULL(pBTHT) ) {
#ifdef DEBUG
    /* Counter for contents of current hash table
       ------------------------------------------ */
    counter num_contents = 0;
#endif
    num_used_hdrs++;
    HashStats_NumBuckets(ht_stats) += BTHT_NumBuckets(pBTHT);
    HashStats_TotalOccupancy(ht_stats) += BTHT_NumContents(pBTHT);
    for ( ppBTN = BTHT_BucketArray(pBTHT);
	  ppBTN < BTHT_BucketArray(pBTHT) + BTHT_NumBuckets(pBTHT);
	  ppBTN++ )
      if ( IsNonNULL(*ppBTN) ) {
#ifdef DEBUG
	/* Count the objects in each bucket
	   -------------------------------- */
	BTNptr pBTN = *ppBTN;
	do {
	  num_contents++;
	  pBTN = BTN_Sibling(pBTN);
	} while ( IsNonNULL(pBTN) );
#endif
	HashStats_NonEmptyBuckets(ht_stats)++;
      }
#ifdef DEBUG
    /* Compare counter and header values
       --------------------------------- */
    if ( num_contents != BTHT_NumContents(pBTHT) )
      xsb_warn("Inconsistent Basic Trie Hash Table Usage Calculations:\n"
	       "\tHash table occupancy mismatch.");
#endif
    pBTHT = BTHT_NextBTHT(pBTHT);
  }
  if ( HashStats_NumAllocHeaders(ht_stats) !=
       (num_used_hdrs + HashStats_NumFreeHeaders(ht_stats)) )
    xsb_warn("Inconsistent Basic Trie Hash Table Usage Calculations:\n"
	     "\tHeader count mismatch:  Alloc: %d  Used: %d  Free: %d",
	     HashStats_NumAllocHeaders(ht_stats),
	     num_used_hdrs, HashStats_NumFreeHeaders(ht_stats));

  return ht_stats;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

SubgStats subgoal_statistics() {

  SubgStats sg_stats;
  SGFrame pProdSF, pSubSF;


  SM_CurrentCapacity(smSF, SubgStats_NumBlocks(sg_stats),
		     SubgStats_NumAllocFrames(sg_stats));
  SM_CountFreeStructs(smSF, SubgStats_NumFreeFrames(sg_stats));
  SubgStats_FrameSize(sg_stats) = sizeof(struct subgoal_frame);

  SubgStats_NumProducers(sg_stats) = SubgStats_NumConsumers(sg_stats) = 0;
  for ( pProdSF = SM_AllocList(smSF);  IsNonNULL(pProdSF);
        pProdSF = subg_next_subgoal(pProdSF) ) {
    SubgStats_NumProducers(sg_stats)++;
    for ( pSubSF = subg_consumers(pProdSF);  IsNonNULL(pSubSF); 
	  pSubSF = subg_consumers(pSubSF) )
      SubgStats_NumConsumers(sg_stats)++;
  }
  if ( SubgStats_NumAllocFrames(sg_stats) !=
       (SubgStats_NumProducers(sg_stats) + SubgStats_NumConsumers(sg_stats)
	+ SubgStats_NumFreeFrames(sg_stats)) )
    xsb_warn("Inconsistent Subgoal Frame Usage Calculations:\n"
	     "\tSubgoal Frame count mismatch");

  return sg_stats;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

NodeStats tstn_statistics() {

  NodeStats tstn_stats;


  SM_CurrentCapacity(smTSTN, NodeStats_NumBlocks(tstn_stats),
		     NodeStats_NumAllocNodes(tstn_stats));
  SM_CountFreeStructs(smTSTN,NodeStats_NumFreeNodes(tstn_stats));
  NodeStats_NodeSize(tstn_stats) = sizeof(TS_TrieNode);

  return tstn_stats;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

NodeStats aln_statistics() {

  NodeStats aln_stats;


  SM_CurrentCapacity(smALN, NodeStats_NumBlocks(aln_stats),
		     NodeStats_NumAllocNodes(aln_stats));
  SM_CountFreeStructs(smALN,NodeStats_NumFreeNodes(aln_stats));
  NodeStats_NodeSize(aln_stats) = sizeof(AnsListNode);

  return aln_stats;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

HashStats tstht_statistics() {

  HashStats ht_stats;
  counter num_used_hdrs;
  TSTHTptr pTSTHT;
  TSTNptr *ppTSTN;


  SM_CurrentCapacity(smTSTHT, HashStats_NumBlocks(ht_stats),
		     HashStats_NumAllocHeaders(ht_stats));
  SM_CountFreeStructs(smTSTHT, HashStats_NumFreeHeaders(ht_stats));
  HashStats_HeaderSize(ht_stats) = sizeof(TST_HashTable);

  num_used_hdrs = 0;
  HashStats_NumBuckets(ht_stats) = 0;
  HashStats_TotalOccupancy(ht_stats) = 0;
  HashStats_NonEmptyBuckets(ht_stats) = 0;
  HashStats_BucketSize(ht_stats) = sizeof(TSTNptr);
  pTSTHT = SM_AllocList(smTSTHT);
  while ( IsNonNULL(pTSTHT) ) {
#ifdef DEBUG
    /* Counter for contents of current hash table
       ------------------------------------------ */
    counter num_contents = 0;
#endif
    num_used_hdrs++;
    HashStats_NumBuckets(ht_stats) += TSTHT_NumBuckets(pTSTHT);
    HashStats_TotalOccupancy(ht_stats) += TSTHT_NumContents(pTSTHT);
    for ( ppTSTN = TSTHT_BucketArray(pTSTHT);
	  ppTSTN < TSTHT_BucketArray(pTSTHT) + TSTHT_NumBuckets(pTSTHT);
	  ppTSTN++ )
      if ( IsNonNULL(*ppTSTN) ) {
#ifdef DEBUG
	/* Count the objects in each bucket
	   -------------------------------- */
	TSTNptr pTSTN = *ppTSTN;
	do {
	  num_contents++;
	  pTSTN = TSTN_Sibling(pTSTN);
	} while ( IsNonNULL(pTSTN) );
#endif
	HashStats_NonEmptyBuckets(ht_stats)++;
      }
#ifdef DEBUG
    /* Compare counter and header values
       --------------------------------- */
    if ( num_contents != TSTHT_NumContents(pTSTHT) )
      xsb_warn("Inconsistent Time Stamp Trie Hash Table Usage Calculations:\n"
	       "\tHash table occupancy mismatch.");
#endif
    pTSTHT = TSTHT_NextTSTHT(pTSTHT);
  }
  if ( HashStats_NumAllocHeaders(ht_stats) !=
       (num_used_hdrs + HashStats_NumFreeHeaders(ht_stats)) )
    xsb_warn("Inconsistent Time Stamp Trie Hash Table Usage Calculations:\n"
	     "\tHeader count mismatch.");

  return ht_stats;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

NodeStats tsi_statistics() {

  NodeStats tsi_stats;


  SM_CurrentCapacity(smEntry, NodeStats_NumBlocks(tsi_stats),
		     NodeStats_NumAllocNodes(tsi_stats));
  SM_CountFreeStructs(smEntry,NodeStats_NumFreeNodes(tsi_stats));
  NodeStats_NodeSize(tsi_stats) = sizeof(TSI_Entry);

  return tsi_stats;
}

/*-------------------------------------------------------------------------*/

/*
 *                       Recording Maximum Usage
 *                       -----------------------
 */

/*
 * Most of the data structures used in tabling are persistent: table
 * info frames, subgoal frames, and the trie nodes and hash tables
 * (basic and time-stamped).  Therefore, the maximum usage of each of
 * these structures at any particular time during an evaluation is the
 * same as their current usage.  However, there are some structures
 * whose usage fluctuates during an evaluation.  In particular,
 * TimeStamp Indices and answer list nodes are reclaimed upon
 * completion.  Therefore, we explicitly track their maximum usage at
 * those times just prior to reclamation.  The following data
 * structure maintains the maximum usage for each of these structures.
 * And as the (ultimate) maximum usage of a particular structure is
 * independent of that of other structures, we also explicitly track
 * the maximum utilization of table space.
 */

struct {
  counter tsi;                   /* TS Index Nodes */
  counter alns;                  /* Answer List Nodes */
  unsigned long  total_bytes;    /* total tablespace in bytes */
} maxTableSpaceUsage = {0,0,0};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void reset_maximum_tablespace_stats() {

  maxTableSpaceUsage.tsi = maxTableSpaceUsage.alns = 0;
  maxTableSpaceUsage.total_bytes = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 * To be used to facilitate statistical recordings, but in situations
 * where one is not currently inspecting results, e.g., when reclaiming
 * table space at completion.  This procedure queries the current state
 * of usage of all table components, and tests whether this is a new
 * maximum.
 */

void compute_maximum_tablespace_stats() {

  NodeStats tstn, btn, aln, tsi;
  SubgStats sf;
  HashStats tstht, btht;

  btn = btn_statistics();
  btht = btht_statistics();
  sf = subgoal_statistics();
  tstn = tstn_statistics();
  tstht = tstht_statistics();
  tsi = tsi_statistics();
  aln = aln_statistics();

  update_maximum_tablespace_stats(&btn,&btht,&sf,&aln,&tstn,&tstht,&tsi);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 * To be used to facilitate statistical reporting at the moment -- when
 * one is currently inspecting results.  Presumably one would want to
 * independently query the current state of usage of all table
 * components for such reporting, so we use these results rather than
 * recompute them.
 */

void update_maximum_tablespace_stats(NodeStats *btn, HashStats *btht,
				      SubgStats *sf, NodeStats *aln,
				      NodeStats *tstn, HashStats *tstht,
				      NodeStats *tsi) {
   unsigned long  byte_size;

   byte_size =
     CurrentTotalTableSpaceUsed(*btn,*btht,*sf,*aln,*tstn,*tstht,*tsi);
   if ( byte_size > maxTableSpaceUsage.total_bytes )
     maxTableSpaceUsage.total_bytes = byte_size;
   if ( NodeStats_NumUsedNodes(*aln) > maxTableSpaceUsage.alns )
     maxTableSpaceUsage.alns = NodeStats_NumUsedNodes(*aln);
   if ( NodeStats_NumUsedNodes(*tsi) > maxTableSpaceUsage.tsi )
     maxTableSpaceUsage.tsi = NodeStats_NumUsedNodes(*tsi);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

counter maximum_timestamp_index_nodes() {

  return (maxTableSpaceUsage.tsi);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

counter maximum_answer_list_nodes() {

  return (maxTableSpaceUsage.alns);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

unsigned long  maximum_total_tablespace_usage() {

  return (maxTableSpaceUsage.total_bytes);
}

/*-------------------------------------------------------------------------*/

/*
 *               Recording and Displaying Operational Counts
 *               -------------------------------------------
 */

NumSubOps numSubOps = INIT_NUMSUBOPS;


void reset_subsumption_stats() {

  NumSubOps initRecord = INIT_NUMSUBOPS;

  numSubOps = initRecord;
}


void print_subsumption_stats() {

  printf("Subsumptive Operations:\n"
	 "  Subsumptive call check/insert ops: %8d\n"
	 "  * Calls to nonexistent or incomplete tables\n"
	 "    - Producers:                   %6d\n"
	 "    - Variants of producers:       %6d\n"
	 "    - Properly subsumed:           %6d\n"
	 "        Resulted in call table entry: %d\n"
	 "  * Calls to completed tables:     %6d\n",
	 NumSubOps_CallCheckInsert, NumSubOps_ProducerCall,
	 NumSubOps_VariantCall, NumSubOps_SubsumedCall,
	 NumSubOps_SubsumedCallEntry, NumSubOps_CallToCompletedTable);
  printf("  Answer check/insert operations:    %8d\n"
	 "    Actual answer inserts:         %6d\n"
	 "    Derivation ratio (New/Total):    %4.2f\n",
	 NumSubOps_AnswerCheckInsert, NumSubOps_AnswerInsert,
	 ( (NumSubOps_AnswerCheckInsert != 0)
	   ? (float)NumSubOps_AnswerInsert / (float)NumSubOps_AnswerCheckInsert
	   : 0 ));
  printf("  Answer retrieval operations:       %8d\n"
	 "  Answer-list consumption ops:       %8d\n",
	 NumSubOps_AnswerRetrieval, NumSubOps_AnswerConsumption);
}

/*==========================================================================*/
