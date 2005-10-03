/* File:      trace_xsb.c
** Author(s): Jiyang Xu, Terrance Swift, Kostis Sagonas
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


#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "inst_xsb.h"
#include "memory_xsb.h"
#include "register.h"
#include "psc_xsb.h"
#include "table_stats.h"
#include "trie_internals.h"
#include "tries.h"
#include "macro_xsb.h"
#include "choice.h"
#include "flags_xsb.h"
#include "heap_xsb.h"
#include "thread_xsb.h"
#include "trace_xsb.h"

/*======================================================================*/

double time_start;      /* time from which stats started being collected */

struct trace_str tds;			/* trace datastructure */
struct trace_str ttt;			/* trace total */
struct trace_str trace_init = {		/* initial value for a trace str */
    0, 0, 0, 0, 0, 0, 0.0
   };

/*======================================================================*/
/* perproc_stat()							*/
/*======================================================================*/

/*
 * Moves values from 'tds' into 'ttt' for reporting in total_stat().
 * (Since 'ttt' is always reset when the builtin statistics/1
 *  (statistics/0 calls statistics(1)) is called, 'ttt' always gets *
 *  what's in 'tds'.  The *real* check for max usage occurs in *
 *  subp.c::intercept(), which compares current usage to previous max *
 *  values held in 'tds'.)
 */

void perproc_stat(void)
{
  tds.time_count = cpu_time() - time_start;
  if (ttt.maxgstack_count < tds.maxgstack_count) 
     ttt.maxgstack_count = tds.maxgstack_count;
  if (ttt.maxlstack_count < tds.maxlstack_count) 
     ttt.maxlstack_count = tds.maxlstack_count;
  if (ttt.maxtrail_count < tds.maxtrail_count)
     ttt.maxtrail_count = tds.maxtrail_count;
  if (ttt.maxcpstack_count < tds.maxcpstack_count)
     ttt.maxcpstack_count = tds.maxcpstack_count;
  if (ttt.maxopenstack_count < tds.maxopenstack_count)
     ttt.maxopenstack_count = tds.maxopenstack_count;
  if (ttt.maxlevel_num < tds.maxlevel_num)
     ttt.maxlevel_num = tds.maxlevel_num;
  ttt.time_count += tds.time_count;
}

/*======================================================================*/
/* total_stat()								*/
/*======================================================================*/

/*
 * Prints current memory usage info, operational counts, and, if the
 * "-s" option was given to xsb at invocation, maximum usage from the
 * time of 'time_start'.
 */

void total_stat(CTXTdeclc double elapstime) {

  NodeStats
    tbtn,		/* Table Basic Trie Nodes */
    abtn,		/* Asserted Basic Trie Nodes */
    tstn,		/* Time Stamp Trie Nodes */
    aln,		/* Answer List Nodes */
    tsi,		/* Time Stamp Indices (Index Entries/Nodes) */
    varsf,		/* Variant Subgoal Frames */
    prodsf,		/* Subsumptive Producer Subgoal Frames */
    conssf;		/* Subsumptive Consumer Subgoal Frames */

  HashStats
    tbtht,		/* Table Basic Trie Hash Tables */
    abtht,		/* Asserted Basic Trie Hash Tables */
    tstht;		/* Time Stamp Trie Hash Tables */
  
  unsigned long
    total_alloc, total_used,
    tablespace_alloc, tablespace_used,
    trieassert_alloc, trieassert_used,
    gl_avail, tc_avail,
    de_space_alloc, de_space_used,
    dl_space_alloc, dl_space_used;

  int
    num_de_blocks, num_dl_blocks,
    de_count, dl_count;


  tbtn = node_statistics(&smTableBTN);
  tbtht = hash_statistics(&smTableBTHT);
  varsf = subgoal_statistics(&smVarSF);
  prodsf = subgoal_statistics(&smProdSF);
  conssf = subgoal_statistics(&smConsSF);
  aln = node_statistics(&smALN);
  tstn = node_statistics(&smTSTN);
  tstht = hash_statistics(&smTSTHT);
  tsi = node_statistics(&smTSIN);

  tablespace_alloc = CurrentTotalTableSpaceAlloc(tbtn,tbtht,varsf,prodsf,
						 conssf,aln,tstn,tstht,tsi);
  tablespace_used = CurrentTotalTableSpaceUsed(tbtn,tbtht,varsf,prodsf,
					       conssf,aln,tstn,tstht,tsi);

  abtn = node_statistics(&smAssertBTN);
  abtht = hash_statistics(&smAssertBTHT);
  trieassert_alloc =
    NodeStats_SizeAllocNodes(abtn) + HashStats_SizeAllocTotal(abtht);
  trieassert_used =
    NodeStats_SizeUsedNodes(abtn) + HashStats_SizeUsedTotal(abtht);

  gl_avail = (top_of_localstk - top_of_heap - 1) * sizeof(Cell);
  tc_avail = (top_of_cpstack - (CPtr)top_of_trail - 1) * sizeof(Cell);
  
  de_space_alloc = allocated_de_space(& num_de_blocks);
  de_space_used = de_space_alloc - unused_de_space();
  de_count = (de_space_used - num_de_blocks * sizeof(Cell)) /
	     sizeof(struct delay_element);

  dl_space_alloc = allocated_dl_space(& num_dl_blocks);
  dl_space_used = dl_space_alloc - unused_dl_space();
  dl_count = (dl_space_used - num_dl_blocks * sizeof(Cell)) /
	     sizeof(struct delay_list);

  total_alloc =
    pspacesize  +  trieassert_alloc  +  tablespace_alloc  +
    (pdl.size + glstack.size + tcpstack.size + complstack.size) * K +
    de_space_alloc + dl_space_alloc;

  total_used  =
    pspacesize  +  trieassert_used  +  tablespace_used  +
    (glstack.size * K - gl_avail) + (tcpstack.size * K - tc_avail) +
    de_space_used + dl_space_used;


  printf("\n");
  printf("Memory (total)    %12ld bytes: %12ld in use, %12ld free\n",
	 total_alloc, total_used, total_alloc - total_used);
  printf("  permanent space %12ld bytes: %12ld in use, %12ld free\n",
	 pspacesize + trieassert_alloc, pspacesize + trieassert_used,
	 trieassert_alloc - trieassert_used);
  printf("  glob/loc space  %12ld bytes: %12ld in use, %12ld free\n",
	 glstack.size * K, glstack.size * K - gl_avail, gl_avail);
  printf("    global                            %12ld bytes\n",
	 (long)((top_of_heap - (CPtr)glstack.low + 1) * sizeof(Cell)));
  printf("    local                             %12ld bytes\n",
	 (long)(((CPtr)glstack.high - top_of_localstk) * sizeof(Cell)));
  printf("  trail/cp space  %12ld bytes: %12ld in use, %12ld free\n",
	 tcpstack.size * K, tcpstack.size * K - tc_avail, tc_avail);
  printf("    trail                             %12ld bytes\n",
	 (long)((top_of_trail - (CPtr *)tcpstack.low + 1) * sizeof(CPtr)));
  printf("    choice point                      %12ld bytes\n",
	 (long)(((CPtr)tcpstack.high - top_of_cpstack) * sizeof(Cell)));
  printf("  SLG unific. space %10ld bytes: %12ld in use, %12ld free\n",
	 pdl.size * K, (unsigned long)(pdlreg+1) - (unsigned long)pdl.high,
	 pdl.size * K - ((unsigned long)(pdlreg+1)-(unsigned long)pdl.high)); 
  printf("  SLG completion  %12ld bytes: %12ld in use, %12ld free\n",
	 (unsigned long)complstack.size * K,
	 (unsigned long)COMPLSTACKBOTTOM - (unsigned long)top_of_complstk,
	 (unsigned long)complstack.size * K -
	 ((unsigned long)COMPLSTACKBOTTOM - (unsigned long)top_of_complstk));
  printf("  SLG table space %12ld bytes: %12ld in use, %12ld free\n",
	 tablespace_alloc,  tablespace_used,
	 tablespace_alloc - tablespace_used);
  printf("\n");

/* TLS: Max stack stuff is probably not real useful with multiple
   threads -- to even get it to work correcly you'd have to use locks.
*/
#ifndef MT_ENGINE
  if (flags[TRACE_STA]) {
    /* Report Maximum Usages
       --------------------- */
    printf("  Maximum stack used: global %ld, local %ld, trail %ld, cp %ld,\n",
	   ttt.maxgstack_count, ttt.maxlstack_count, 
	   ttt.maxtrail_count, ttt.maxcpstack_count);
    printf("                      SLG completion %ld (%ld subgoals)\n",
	   ttt.maxopenstack_count,
	   (ttt.maxopenstack_count/sizeof(struct completion_stack_frame)));

    update_maximum_tablespace_stats(&tbtn,&tbtht,&varsf,&prodsf,&conssf,
				    &aln,&tstn,&tstht,&tsi);
    printf("  Maximum table space used:  %ld bytes\n",
	   maximum_total_tablespace_usage());
    printf("\n");
  }
#endif

  printf("Tabling Operations\n");
  printf("  %u subsumptive call check/insert ops: %u producers, %u variants,\n"
	 "  %u properly subsumed (%u table entries), %u used completed table.\n"
	 "  %u relevant answer ident ops.  %u consumptions via answer list.\n",
	 NumSubOps_CallCheckInsert,		NumSubOps_ProducerCall,
	 NumSubOps_VariantCall,			NumSubOps_SubsumedCall,
	 NumSubOps_SubsumedCallEntry,		NumSubOps_CallToCompletedTable,
	 NumSubOps_IdentifyRelevantAnswers,	NumSubOps_AnswerConsumption);
  {
    long ttl_ops = ans_chk_ins + NumSubOps_AnswerCheckInsert,
	 ttl_ins = ans_inserts + NumSubOps_AnswerInsert;

    printf("  %ld variant call check/insert ops: %ld producers, %ld variants.\n"
	   "  %ld answer check/insert ops: %ld unique inserts, %ld redundant.\n",
	   subg_chk_ins, subg_inserts, subg_chk_ins - subg_inserts,
	   ttl_ops, ttl_ins, ttl_ops - ttl_ins);
  }
  printf("\n");

  if (de_count > 0) {
    printf(" %6d DEs in the tables (space: %5ld bytes allocated, %5ld in use)\n",
	   de_count, de_space_alloc, de_space_used);
    printf(" %6d DLs in the tables (space: %5ld bytes allocated, %5ld in use)\n",
	   dl_count, dl_space_alloc, dl_space_used);
    printf("\n");
  }

#ifdef GC
  printf("\n");
  print_gc_statistics();
#endif

#ifdef MULTI_THREAD

  /*  print_mutex_use();*/

#endif

  printf("Time: %.3f sec. cputime,  %.3f sec. elapsetime\n",
	 ttt.time_count, elapstime);
}

/*======================================================================*/

/*
 * Called when builtin statistics(0) is invoked.  Resets all operational
 * counts and max memory usage info.
 */

void perproc_reset_stat(void)
{
   tds = trace_init;
   reset_subsumption_stats();
   reset_maximum_tablespace_stats();
   ans_chk_ins = ans_inserts = 0;
   subg_chk_ins = subg_inserts = 0;
   time_start = cpu_time();
}

/*======================================================================*/

void reset_stat_total(void)
{
   ttt = trace_init;
}

/*======================================================================*/
