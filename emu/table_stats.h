/* File:      table_stats.h
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


#ifndef TABLE_STATISTICS

#define TABLE_STATISTICS


/*=========================================================================*/

/*
 *       Interface for Reporting Tabling-Component Usage Statistics
 *       ==========================================================
 */


/*
 *                       Recording Current Usage
 *                       -----------------------
 */


/* For Generic Nodes
   ----------------- */
typedef struct {
  counter nBlocks;      /* num blocks of nodes allocated (if applicable) */
  counter nAlloced;     /* total number of nodes allocated */
  counter nFree;        /* number of allocated nodes not currently in use */
  counter size;         /* size of your particular node */
} NodeStats;

#define NodeStats_NumBlocks(NS)		( (NS).nBlocks )

#define NodeStats_NumAllocNodes(NS)	( (NS).nAlloced )
#define NodeStats_NumFreeNodes(NS)	( (NS).nFree )
#define NodeStats_NumUsedNodes(NS)	( (NS).nAlloced - (NS).nFree )

#define NodeStats_NodeSize(NS)		( (NS).size )

#define NodeStats_SizeAllocNodes(NS)	( NodeStats_NumAllocNodes(NS)	\
					  * NodeStats_NodeSize(NS) )
#define NodeStats_SizeFreeNodes(NS)	( NodeStats_NumFreeNodes(NS)	\
					  * NodeStats_NodeSize(NS) )
#define NodeStats_SizeUsedNodes(NS)	( NodeStats_NumUsedNodes(NS)	\
					  * NodeStats_NodeSize(NS) )

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* For Subgoal Frames
   ------------------ */
typedef struct {
  NodeStats sf;          /* general stats about Subgoal Frames */
  counter nProducers;    /* number of producer subgoal frames */
  counter nConsumers;    /* subgoal frames for consuming (subsumed) subgoals */
} SubgStats;

#define SubgStats_NumBlocks(SS)		NodeStats_NumBlocks((SS).sf)

#define SubgStats_NumAllocFrames(SS)	NodeStats_NumAllocNodes((SS).sf)
#define SubgStats_NumFreeFrames(SS)	NodeStats_NumFreeNodes((SS).sf)
#define SubgStats_NumUsedFrames(SS)	NodeStats_NumUsedNodes((SS).sf)
#define SubgStats_NumProducers(SS)	( (SS).nProducers )
#define SubgStats_NumConsumers(SS)	( (SS).nConsumers )

#define SubgStats_FrameSize(SS)		NodeStats_NodeSize((SS).sf)
#define SubgStats_SizeAllocFrames(SS)	NodeStats_SizeAllocNodes((SS).sf)
#define SubgStats_SizeFreeFrames(SS)	NodeStats_SizeFreeNodes((SS).sf)
#define SubgStats_SizeUsedFrames(SS)	NodeStats_SizeUsedNodes((SS).sf)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* For Hash Tables
   --------------- */
typedef struct {
  NodeStats hdr;        /* contains stats about the headers */
  counter ttlBkts;      /* total number of buckets allocated */
  counter ttlUsedBkts;  /* number of nonempty buckets */
  counter occupancy;    /* number of objects maintained by the hash table(s) */
  counter bktSize;      /* size of each bucket (usually just a pointer) */
} HashStats;

#define HashStats_NumBlocks(HS)		NodeStats_NumBlocks((HS).hdr)

#define HashStats_NumAllocHeaders(HS)	NodeStats_NumAllocNodes((HS).hdr)
#define HashStats_NumFreeHeaders(HS)	NodeStats_NumFreeNodes((HS).hdr)
#define HashStats_NumUsedHeaders(HS)	NodeStats_NumUsedNodes((HS).hdr)

#define HashStats_HeaderSize(HS)	NodeStats_NodeSize((HS).hdr)

#define HashStats_SizeAllocHeaders(HS)	NodeStats_SizeAllocNodes((HS).hdr)
#define HashStats_SizeUsedHeaders(HS)	NodeStats_SizeUsedNodes((HS).hdr)
#define HashStats_SizeFreeHeaders(HS)	NodeStats_SizeFreeNodes((HS).hdr)

#define HashStats_NumBuckets(HS)	( (HS).ttlBkts )
#define HashStats_NonEmptyBuckets(HS)	( (HS).ttlUsedBkts )
#define HashStats_EmptyBuckets(HS)	( (HS).ttlBkts - (HS).ttlUsedBkts )
#define HashStats_TotalOccupancy(HS)	( (HS).occupancy )

#define HashStats_BucketSize(HS)	( (HS).bktSize )

#define HashStats_SizeAllocBuckets(HS)	( HashStats_NumBuckets(HS) *	\
					  HashStats_BucketSize(HS) )

#define HashStats_SizeAllocTotal(HS)	( HashStats_SizeAllocHeaders(HS) + \
					  HashStats_SizeAllocBuckets(HS) )
#define HashStats_SizeUsedTotal(HS)	( HashStats_SizeUsedHeaders(HS) +  \
					  HashStats_SizeAllocBuckets(HS) )
#define HashStats_SizeFreeTotal(HS)	HashStats_SizeFreeHeaders(HS)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Collection Routines
   ------------------- */
SubgStats subgoal_statistics();
NodeStats aln_statistics();
NodeStats btn_statistics();
HashStats btht_statistics();
NodeStats tstn_statistics();
HashStats tstht_statistics();
NodeStats tsi_statistics();

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Helpful Totaling Routines
   ------------------------- */
#define CurrentTotalTableSpaceAlloc(BTN,BTHT,SF,ALN,TSTN,TSTHT,TSI)	   \
   ( NodeStats_SizeAllocNodes(BTN)  +  HashStats_SizeAllocTotal(BTHT)  +   \
     SubgStats_SizeAllocFrames(SF)  +					   \
     NodeStats_SizeAllocNodes(TSTN)  +  HashStats_SizeAllocTotal(TSTHT)  + \
     NodeStats_SizeAllocNodes(TSI)  +  NodeStats_SizeAllocNodes(ALN) )

#define CurrentTotalTableSpaceUsed(BTN,BTHT,SF,ALN,TSTN,TSTHT,TSI)	 \
   ( NodeStats_SizeUsedNodes(BTN)  +  HashStats_SizeUsedTotal(BTHT)  +	 \
     SubgStats_SizeUsedFrames(SF)  +					 \
     NodeStats_SizeUsedNodes(TSTN)  +  HashStats_SizeUsedTotal(TSTHT)  + \
     NodeStats_SizeUsedNodes(TSI)  +  NodeStats_SizeUsedNodes(ALN) )

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Printing Routines
   ----------------- */
void print_detailed_tablespace_stats();

/*-------------------------------------------------------------------------*/

/*
 *                       Recording Maximum Usage
 *                       -----------------------
 */

/* Reset Usage
   ----------- */
void reset_maximum_tablespace_stats();

/* Poll Current Usage for New Maximum
   ---------------------------------- */
void compute_maximum_tablespace_stats();
void update_maximum_tablespace_stats(NodeStats *btn, HashStats *btht,
				     SubgStats *sf, NodeStats *aln,
				     NodeStats *tstn, HashStats *tstht,
				     NodeStats *tsi);

/* Read Currently Recorded Maximum Values
   -------------------------------------- */
/* One should first force a check of the maximum usage before querying
   for these values. */
counter maximum_answer_list_nodes();
counter maximum_timestamp_index_nodes();
unsigned long  maximum_total_tablespace_usage();

/*=========================================================================*/

/*
 *   Interface for Recording and Reporting Tabling-Operation Statistics
 *   ==================================================================
 */


/*
 *                      For Subsumptive Evaluations
 *                      ---------------------------
 */

typedef struct {
  struct {               /* Call Check/Insert Operation */
    counter total;       /* - total number of call-check/insert ops */
    counter complete;    /* - calls which were satisfied by completed table */
    struct {
      counter new;       /* - number of created producers */
      counter vrnt;      /* - number of calls which are variants of an
			   established producer */
    } producer;
    struct {
      counter total;     /* - total number of properly subsumed calls */
      counter entry;     /* - number of properly subsumed calls that were given
		           an entry in the call table */
    } subsumed;
  } CallCI;
  struct {               /* Answer Check/Insert Operation */
    counter total;       /* - total number of answer-check/insert ops */
    counter inserts;     /* - number of inserted substitutions */
  } AnsCI;
  counter consumption;   /* Answer Consumptions for all consumer subgoals */
  counter retrieval;     /* Answer Collections for all subsumed subgoals */
} NumSubOps;

#define INIT_NUMSUBOPS   { {0,0,{0,0},{0,0}}, {0,0}, 0, 0 }
extern NumSubOps numSubOps;

#define NumSubOps_CallCheckInsert	   numSubOps.CallCI.total
#define NumSubOps_CallToCompletedTable	   numSubOps.CallCI.complete
#define NumSubOps_ProducerCall		   numSubOps.CallCI.producer.new
#define NumSubOps_VariantCall		   numSubOps.CallCI.producer.vrnt
#define NumSubOps_SubsumedCall		   numSubOps.CallCI.subsumed.total
#define NumSubOps_SubsumedCallEntry	   numSubOps.CallCI.subsumed.entry
#define NumSubOps_AnswerCheckInsert	   numSubOps.AnsCI.total
#define NumSubOps_AnswerInsert		   numSubOps.AnsCI.inserts
#define NumSubOps_AnswerConsumption	   numSubOps.consumption
#define NumSubOps_AnswerRetrieval	   numSubOps.retrieval


void reset_subsumption_stats();
void print_detailed_subsumption_stats();

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *                        For Variant Evaluations
 *                        -----------------------
 */

/* Move code to here from trie* files */

/*=========================================================================*/


#endif
