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


#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "psc_xsb.h"
#include "table_stats.h"
#include "trie_internals.h"
#include "macro_xsb.h"
#include "error_xsb.h"
#include "flags_xsb.h"
#include "debug_xsb.h"


/*==========================================================================*/

/*
 *		  T A B L I N G   S T A T I S T I C S
 *		  ===================================
 */


/*
 *                      Recording Current Usage
 *                      -----------------------
 */


NodeStats node_statistics(Structure_Manager *sm) {

  NodeStats stats;


  SM_CurrentCapacity(*sm, NodeStats_NumBlocks(stats),
		     NodeStats_NumAllocNodes(stats));
  SM_CountFreeStructs(*sm, NodeStats_NumFreeNodes(stats));
  NodeStats_NodeSize(stats) = SM_StructSize(*sm);

  return stats;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

HashStats hash_statistics(Structure_Manager *sm) {

  HashStats ht_stats;
  counter num_used_hdrs;
  BTHTptr pBTHT;
  BTNptr *ppBTN;


  ht_stats.hdr = node_statistics(sm);

  num_used_hdrs = 0;
  HashStats_NumBuckets(ht_stats) = 0;
  HashStats_TotalOccupancy(ht_stats) = 0;
  HashStats_NonEmptyBuckets(ht_stats) = 0;
  HashStats_BucketSize(ht_stats) = sizeof(void *);
  pBTHT = (BTHTptr)SM_AllocList(*sm);
  while ( IsNonNULL(pBTHT) ) {
#ifdef DEBUG_ASSERTIONS
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
#ifdef DEBUG_ASSERTIONS
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
#ifdef DEBUG_ASSERTIONS
    /* Compare counter and header values
       --------------------------------- */
    if ( num_contents != BTHT_NumContents(pBTHT) )
      xsb_warn("Inconsistent %s Usage Calculations:\n"
	       "\tHash table occupancy mismatch.", SM_StructName(*sm));
#endif
    pBTHT = BTHT_NextBTHT(pBTHT);
  }
  if ( HashStats_NumAllocHeaders(ht_stats) !=
       (num_used_hdrs + HashStats_NumFreeHeaders(ht_stats)) )
    xsb_warn("Inconsistent %s Usage Calculations:\n"
	     "\tHeader count mismatch:  Alloc: %d  Used: %d  Free: %d",
	     SM_StructName(*sm), HashStats_NumAllocHeaders(ht_stats),
	     num_used_hdrs,  HashStats_NumFreeHeaders(ht_stats));

  return ht_stats;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

NodeStats subgoal_statistics(Structure_Manager *sm) {

  NodeStats sg_stats;
  TIFptr tif;
  VariantSF pProdSF;
  SubConsSF pConsSF;
  int nSubgoals;


  sg_stats = node_statistics(sm);
  nSubgoals = 0;
  if ( sm == &smVarSF ) {
    for ( tif = tif_list.first;  IsNonNULL(tif);  tif = TIF_NextTIF(tif) )
      if ( IsVariantPredicate(tif) )
	for ( pProdSF = TIF_Subgoals(tif);  IsNonNULL(pProdSF);
	      pProdSF = (VariantSF)subg_next_subgoal(pProdSF) )
	  nSubgoals++;
  }
  else if ( sm == &smProdSF ) {
    for ( tif = tif_list.first;  IsNonNULL(tif);  tif = TIF_NextTIF(tif) )
      if ( IsSubsumptivePredicate(tif) )
	for ( pProdSF = TIF_Subgoals(tif);  IsNonNULL(pProdSF);
	      pProdSF = (VariantSF)subg_next_subgoal(pProdSF) )
	  nSubgoals++;
  }
  else if ( sm == &smConsSF ) {
    for ( tif = tif_list.first;  IsNonNULL(tif);  tif = TIF_NextTIF(tif) )
      if ( IsSubsumptivePredicate(tif) )
	for ( pProdSF = TIF_Subgoals(tif);  IsNonNULL(pProdSF);
	      pProdSF = (VariantSF)subg_next_subgoal(pProdSF) )
	  for ( pConsSF = subg_consumers(pProdSF);  IsNonNULL(pConsSF); 
		pConsSF = conssf_consumers(pConsSF) )
	    nSubgoals++;
  }
  else {
    xsb_dbgmsg((LOG_DEBUG, "Incorrect use of subgoal_statistics()\n"
	       "SM does not contain subgoal frames"));
    return sg_stats;
  }

  if ( NodeStats_NumUsedNodes(sg_stats) != (counter) nSubgoals )
    xsb_warn("Inconsistent Subgoal Frame Usage Calculations:\n"
	     "\tSubgoal Frame count mismatch");

  return sg_stats;
}

/*-------------------------------------------------------------------------*/

/*
 *                      Displaying Current Usage
 *                      ------------------------
 */

void print_detailed_tablespace_stats() {

  NodeStats
    btn,		/* Basic Trie Nodes */
    tstn,		/* Time Stamp Trie Nodes */
    aln,		/* Answer List Nodes */
    tsi,		/* Time Stamp Indices (Index Entries/Nodes) */
    varsf,		/* Variant Subgoal Frames */
    prodsf,		/* Subsumptive Producer Subgoal Frames */
    conssf;		/* Subsumptive Consumer Subgoal Frames */

  HashStats
    btht,		/* Basic Trie Hash Tables */
    tstht;		/* Time Stamp Trie Hash Tables */
  

  btn = node_statistics(&smTableBTN);
  btht = hash_statistics(&smTableBTHT);
  varsf = subgoal_statistics(&smVarSF);
  prodsf = subgoal_statistics(&smProdSF);
  conssf = subgoal_statistics(&smConsSF);
  aln = node_statistics(&smALN);
  tstn = node_statistics(&smTSTN);
  tstht = hash_statistics(&smTSTHT);
  tsi = node_statistics(&smTSIN);

  printf("\n"
	 "Table Space Usage\n");
  printf("  Current Total Allocation:   %12u bytes\n"
	 "  Current Total Usage:        %12u bytes\n",
	 CurrentTotalTableSpaceAlloc(btn,btht,varsf,prodsf,conssf,aln,
				     tstn,tstht,tsi),
	 CurrentTotalTableSpaceUsed(btn,btht,varsf,prodsf,conssf,aln,
				    tstn,tstht,tsi));
  printf("\n"
	 "  Basic Tries\n");
  printf("    Basic Trie Nodes (%u blocks)\n"
	 "      Allocated:   %10u  (%8u bytes)\n"
	 "      Used:        %10u  (%8u bytes)\n"
	 "      Free:        %10u  (%8u bytes)\n",
	 NodeStats_NumBlocks(btn),
	 NodeStats_NumAllocNodes(btn),  NodeStats_SizeAllocNodes(btn),
	 NodeStats_NumUsedNodes(btn),  NodeStats_SizeUsedNodes(btn),
	 NodeStats_NumFreeNodes(btn),  NodeStats_SizeFreeNodes(btn));
  printf("    Basic Trie Hash Tables (%u blocks)\n"
	 "      Headers:     %10u  (%8u bytes)\n"
	 "        Used:      %10u  (%8u bytes)\n"
	 "        Free:      %10u  (%8u bytes)\n"
	 "      Buckets:     %10u  (%8u bytes)\n"
	 "        Used:      %10u\n"
	 "        Empty:     %10u\n"
	 "      Occupancy:   %10u BTNs\n",
	 HashStats_NumBlocks(btht),
	 HashStats_NumAllocHeaders(btht),  HashStats_SizeAllocHeaders(btht),
	 HashStats_NumUsedHeaders(btht),  HashStats_SizeUsedHeaders(btht),
	 HashStats_NumFreeHeaders(btht),  HashStats_SizeFreeHeaders(btht),
	 HashStats_NumBuckets(btht),  HashStats_SizeAllocBuckets(btht),
	 HashStats_NonEmptyBuckets(btht),  HashStats_EmptyBuckets(btht),
	 HashStats_TotalOccupancy(btht));
  printf("\n"
	 "  Subgoal Frames\n"
	 "    Variant Subgoal Frames (%u blocks)\n"
	 "      Allocated:     %10u  (%8u bytes)\n"
	 "      Used:          %10u  (%8u bytes)\n"
	 "      Free:          %10u  (%8u bytes)\n"
	 "    Subsumptive Producer Subgoal Frames (%u blocks)\n"
	 "      Allocated:     %10u  (%8u bytes)\n"
	 "      Used:          %10u  (%8u bytes)\n"
	 "      Free:          %10u  (%8u bytes)\n"
	 "    Subsumptive Consumer Subgoal Frames (%u blocks)\n"
	 "      Allocated:     %10u  (%8u bytes)\n"
	 "      Used:          %10u  (%8u bytes)\n"
	 "      Free:          %10u  (%8u bytes)\n",
	 NodeStats_NumBlocks(varsf),
	 NodeStats_NumAllocNodes(varsf),  NodeStats_SizeAllocNodes(varsf),
	 NodeStats_NumUsedNodes(varsf),  NodeStats_SizeUsedNodes(varsf),
	 NodeStats_NumFreeNodes(varsf),  NodeStats_SizeFreeNodes(varsf),
	 NodeStats_NumBlocks(prodsf),
	 NodeStats_NumAllocNodes(prodsf),  NodeStats_SizeAllocNodes(prodsf),
	 NodeStats_NumUsedNodes(prodsf),  NodeStats_SizeUsedNodes(prodsf),
	 NodeStats_NumFreeNodes(prodsf),  NodeStats_SizeFreeNodes(prodsf),
	 NodeStats_NumBlocks(conssf),
	 NodeStats_NumAllocNodes(conssf),  NodeStats_SizeAllocNodes(conssf),
	 NodeStats_NumUsedNodes(conssf),  NodeStats_SizeUsedNodes(conssf),
	 NodeStats_NumFreeNodes(conssf),  NodeStats_SizeFreeNodes(conssf));
  printf("\n"
	 "  Answer List Nodes (%u blocks)\n"
	 "    Allocated:     %10u  (%8u bytes)\n"
	 "    Used:          %10u  (%8u bytes)\n"
	 "    Free:          %10u  (%8u bytes)\n",
	 NodeStats_NumBlocks(aln),
	 NodeStats_NumAllocNodes(aln),  NodeStats_SizeAllocNodes(aln),
	 NodeStats_NumUsedNodes(aln),  NodeStats_SizeUsedNodes(aln),
	 NodeStats_NumFreeNodes(aln),  NodeStats_SizeFreeNodes(aln));
  printf("\n"
	 "  Time Stamp Tries\n"
	 "    Time Stamp Trie Nodes (%u blocks)\n"
	 "      Allocated:   %10u  (%8u bytes)\n"
	 "      Used:        %10u  (%8u bytes)\n"
	 "      Free:        %10u  (%8u bytes)\n",
	 NodeStats_NumBlocks(tstn),
	 NodeStats_NumAllocNodes(tstn),  NodeStats_SizeAllocNodes(tstn),
	 NodeStats_NumUsedNodes(tstn),  NodeStats_SizeUsedNodes(tstn),
	 NodeStats_NumFreeNodes(tstn) ,  NodeStats_SizeFreeNodes(tstn));
  printf("    Time Stamp Trie Hash Tables (%u blocks)\n"
	 "      Headers:     %10u  (%8u bytes)\n"
	 "        Used:      %10u  (%8u bytes)\n"
	 "        Free:      %10u  (%8u bytes)\n"
	 "      Buckets:     %10u  (%8u bytes)\n"
	 "        Used:      %10u\n"
	 "        Empty:     %10u\n"
	 "      Occupancy:   %10u TSTNs\n",
	 HashStats_NumBlocks(tstht),
	 HashStats_NumAllocHeaders(tstht),  HashStats_SizeAllocHeaders(tstht),
	 HashStats_NumUsedHeaders(tstht),  HashStats_SizeUsedHeaders(tstht),
	 HashStats_NumFreeHeaders(tstht),  HashStats_SizeFreeHeaders(tstht),
	 HashStats_NumBuckets(tstht),  HashStats_SizeAllocBuckets(tstht),
	 HashStats_NonEmptyBuckets(tstht),  HashStats_EmptyBuckets(tstht),
	 HashStats_TotalOccupancy(tstht));
  printf("    Time Stamp Trie Index Nodes (%u blocks)\n"
	 "      Allocated:   %10u  (%8u bytes)\n"
	 "      Used:        %10u  (%8u bytes)\n"
	 "      Free:        %10u  (%8u bytes)\n",
	 NodeStats_NumBlocks(tsi),
	 NodeStats_NumAllocNodes(tsi),  NodeStats_SizeAllocNodes(tsi),
	 NodeStats_NumUsedNodes(tsi),  NodeStats_SizeUsedNodes(tsi),
	 NodeStats_NumFreeNodes(tsi),  NodeStats_SizeFreeNodes(tsi));

  if (flags[TRACE_STA]) {
    /* Report Maximum Usages
       --------------------- */
    update_maximum_tablespace_stats(&btn,&btht,&varsf,&prodsf,&conssf,
				    &aln,&tstn,&tstht,&tsi);
    printf("\n"
	   "Maximum Total Usage:        %12ld bytes\n",
	   maximum_total_tablespace_usage());
    printf("Maximum Structure Usage:\n"
	   "  ALNs:            %10u  (%8u bytes)\n"
	   "  TSINs:           %10u  (%8u bytes)\n",
	   maximum_answer_list_nodes(),
	   maximum_answer_list_nodes() * NodeStats_NodeSize(aln),
	   maximum_timestamp_index_nodes(),
	   maximum_timestamp_index_nodes() * NodeStats_NodeSize(tsi));
  }
  printf("\n");
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
  NodeStats varsf, prodsf, conssf;
  HashStats tstht, btht;

  btn = node_statistics(&smTableBTN);
  btht = hash_statistics(&smTableBTHT);
  varsf = subgoal_statistics(&smVarSF);
  prodsf = subgoal_statistics(&smProdSF);
  conssf = subgoal_statistics(&smConsSF);
  tstn = node_statistics(&smTSTN);
  tstht = hash_statistics(&smTSTHT);
  tsi = node_statistics(&smTSIN);
  aln = node_statistics(&smALN);

  update_maximum_tablespace_stats(&btn,&btht,&varsf,&prodsf,&conssf,
				  &aln,&tstn,&tstht,&tsi);
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
				     NodeStats *varsf, NodeStats *prodsf,
				     NodeStats *conssf, NodeStats *aln,
				     NodeStats *tstn, HashStats *tstht,
				     NodeStats *tsi) {
   unsigned long  byte_size;

   byte_size = CurrentTotalTableSpaceUsed(*btn,*btht,*varsf,*prodsf,*conssf,
					  *aln,*tstn,*tstht,*tsi);
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


void print_detailed_subsumption_stats() {

  printf("Subsumptive Operations\n"
	 "  Subsumptive call check/insert ops: %8u\n"
	 "  * Calls to nonexistent or incomplete tables\n"
	 "    - Producers:                   %6u\n"
	 "    - Variants of producers:       %6u\n"
	 "    - Properly subsumed:           %6u\n"
	 "        Resulted in call table entry: %u\n"
	 "  * Calls to completed tables:     %6u\n",
	 NumSubOps_CallCheckInsert, NumSubOps_ProducerCall,
	 NumSubOps_VariantCall, NumSubOps_SubsumedCall,
	 NumSubOps_SubsumedCallEntry, NumSubOps_CallToCompletedTable);
  printf("  Answer check/insert operations:    %8u\n"
	 "  * Actual answer inserts:         %6u\n"
	 "  * Derivation ratio (New/Total):    %4.2f\n",
	 NumSubOps_AnswerCheckInsert, NumSubOps_AnswerInsert,
	 ( (NumSubOps_AnswerCheckInsert != 0)
	   ? (float)NumSubOps_AnswerInsert / (float)NumSubOps_AnswerCheckInsert
	   : 0 ));
  printf("  Relevant-answer identify ops:      %8u\n"
	 "  Answer-list consumption ops:       %8u\n",
	 NumSubOps_IdentifyRelevantAnswers, NumSubOps_AnswerConsumption);
}

/*==========================================================================*/
