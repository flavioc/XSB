/* File:      trace.c
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


#include "configs/config.h"
#include "debugs/debug.h"

#include <stdio.h>

#include "auxlry.h"
#include "cell.h"
#include "inst.h"
#include "xsb_memory.h"
#include "register.h"
#include "psc.h"
#include "table_stats.h"
#include "tries.h"
#include "choice.h"
#include "flags.h"
#include "heap.h"
#include "xmacro.h"

#ifdef CHAT
#include "chat.h"
#endif

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

void total_stat(double elapstime) {

  NodeStats
    btn,		/* Basic Trie Nodes */
    tstn,		/* Time Stamp Trie Nodes */
    aln,		/* Answer List Nodes */
    tsi;		/* Time Stamp Indices (Index Entries/Nodes) */

  SubgStats  sf;	/* Subgoal Frames */

  HashStats
    btht,		/* Basic Trie Hash Tables */
    tstht;		/* Time Stamp Trie Hash Tables */
  
  unsigned long
    total_alloc, total_used,
    tablespace_alloc, tablespace_used,
    gl_avail, tc_avail,
    chat_alloc, chat_used,
    de_space_alloc, de_space_used,
    dl_space_alloc, dl_space_used;

  int
    num_de_blocks, num_dl_blocks,
    de_count, dl_count;


  btn = btn_statistics();
  btht = btht_statistics();
  sf = subgoal_statistics();
  aln = aln_statistics();
  tstn = tstn_statistics();
  tstht = tstht_statistics();
  tsi = tsi_statistics();

  tablespace_alloc =
    CurrentTotalTableSpaceAlloc(btn,btht,sf,aln,tstn,tstht,tsi);
  tablespace_used =
    CurrentTotalTableSpaceUsed(btn,btht,sf,aln,tstn,tstht,tsi);

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

#ifdef CHAT
  chat_alloc = chat_max_alloc();
  chat_used = chat_now_used();
#else
  chat_alloc = chat_used = 0;
#endif

  total_alloc =
    pspacesize  +  tablespace_alloc  +
    (pdl.size + glstack.size + tcpstack.size + complstack.size) * K +
    chat_alloc + de_space_alloc + dl_space_alloc;

  total_used  =
    pspacesize  +  tablespace_used  +
    (glstack.size * K - gl_avail) + (tcpstack.size * K - tc_avail) +
    chat_used + de_space_used + dl_space_used;


  printf("\n");
  printf("memory (total)    %12ld bytes: %12ld in use, %12ld free\n",
	 total_alloc, total_used, total_alloc - total_used);
  printf("  permanent space %12ld bytes\n", pspacesize);
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
  printf("\n");

  printf("  SLG table space:\n");  
  printf("    Current Total Allocation: %12ld bytes\n"
	 "    Current Total Usage:      %12ld bytes\n\n",
	 tablespace_alloc, tablespace_used);
  printf("    Basic Tries:\n");
  printf("      Basic Trie Nodes (%d blocks):\n"
	 "        Allocated:   %10d  (%8d bytes)\n"
	 "        Used:        %10d  (%8d bytes)\n"
	 "        Free:        %10d  (%8d bytes)\n",
	 NodeStats_NumBlocks(btn),
	 NodeStats_NumAllocNodes(btn),  NodeStats_SizeAllocNodes(btn),
	 NodeStats_NumUsedNodes(btn),  NodeStats_SizeUsedNodes(btn),
	 NodeStats_NumFreeNodes(btn),  NodeStats_SizeFreeNodes(btn));
  printf("      Basic Trie Hash Tables (%d blocks):\n"
	 "        Headers:     %10d  (%8d bytes)\n"
	 "          Used:      %10d  (%8d bytes)\n"
	 "          Free:      %10d  (%8d bytes)\n"
	 "        Buckets:     %10d  (%8d bytes)\n"
	 "          Used:      %10d\n"
	 "          Empty:     %10d\n"
	 "        Occupancy:   %10d BTNs\n",
	 HashStats_NumBlocks(btht),
	 HashStats_NumAllocHeaders(btht),  HashStats_SizeAllocHeaders(btht),
	 HashStats_NumUsedHeaders(btht),  HashStats_SizeUsedHeaders(btht),
	 HashStats_NumFreeHeaders(btht),  HashStats_SizeFreeHeaders(btht),
	 HashStats_NumBuckets(btht),  HashStats_SizeAllocBuckets(btht),
	 HashStats_NonEmptyBuckets(btht),  HashStats_EmptyBuckets(btht),
	 HashStats_TotalOccupancy(btht));
  printf("\n");
  printf("    Subgoal Frames (%d blocks):\n"
	 "      Allocated:     %10d  (%8d bytes)\n"
	 "      Used:          %10d  (%8d bytes)\n"
	 "        Generators:  %8d  (%8d bytes)\n"
	 "        Consumers:   %8d  (%8d bytes)\n"
	 "      Free:          %10d  (%8d bytes)\n",
	 SubgStats_NumBlocks(sf),
	 SubgStats_NumAllocFrames(sf),  SubgStats_SizeAllocFrames(sf),
	 SubgStats_NumUsedFrames(sf),  SubgStats_SizeUsedFrames(sf),
	 SubgStats_NumProducers(sf),
	 SubgStats_NumProducers(sf) * SubgStats_FrameSize(sf),
	 SubgStats_NumConsumers(sf),
	 SubgStats_NumConsumers(sf) * SubgStats_FrameSize(sf),
	 SubgStats_NumFreeFrames(sf),  SubgStats_SizeFreeFrames(sf));
  printf("\n");
  printf("    Answer List Nodes (%d blocks):\n"
	 "      Allocated:     %10d  (%8d bytes)\n"
	 "      Used:          %10d  (%8d bytes)\n"
	 "      Free:          %10d  (%8d bytes)\n",
	 NodeStats_NumBlocks(aln),
	 NodeStats_NumAllocNodes(aln),  NodeStats_SizeAllocNodes(aln),
	 NodeStats_NumUsedNodes(aln),  NodeStats_SizeUsedNodes(aln),
	 NodeStats_NumFreeNodes(aln),  NodeStats_SizeFreeNodes(aln));
  printf("\n");
  printf("    Time Stamp Tries:\n"
	 "      Time Stamp Trie Nodes (%d blocks):\n"
	 "        Allocated:   %10d  (%8d bytes)\n"
	 "        Used:        %10d  (%8d bytes)\n"
	 "        Free:        %10d  (%8d bytes)\n",
	 NodeStats_NumBlocks(tstn),
	 NodeStats_NumAllocNodes(tstn),  NodeStats_SizeAllocNodes(tstn),
	 NodeStats_NumUsedNodes(tstn),  NodeStats_SizeUsedNodes(tstn),
	 NodeStats_NumFreeNodes(tstn) ,  NodeStats_SizeFreeNodes(tstn));
  printf("      Time Stamp Trie Hash Tables (%d blocks):\n"
	 "        Headers:     %10d  (%8d bytes)\n"
	 "          Used:      %10d  (%8d bytes)\n"
	 "          Free:      %10d  (%8d bytes)\n"
	 "        Buckets:     %10d  (%8d bytes)\n"
	 "          Used:      %10d\n"
	 "          Empty:     %10d\n"
	 "        Occupancy:   %10d TSTNs\n",
	 HashStats_NumBlocks(tstht),
	 HashStats_NumAllocHeaders(tstht),  HashStats_SizeAllocHeaders(tstht),
	 HashStats_NumUsedHeaders(tstht),  HashStats_SizeUsedHeaders(tstht),
	 HashStats_NumFreeHeaders(tstht),  HashStats_SizeFreeHeaders(tstht),
	 HashStats_NumBuckets(tstht),  HashStats_SizeAllocBuckets(tstht),
	 HashStats_NonEmptyBuckets(tstht),  HashStats_EmptyBuckets(tstht),
	 HashStats_TotalOccupancy(tstht));
  printf("      Time Stamp Trie Index Nodes (%d blocks):\n"
	 "        Allocated:   %10d  (%8d bytes)\n"
	 "        Used:        %10d  (%8d bytes)\n"
	 "        Free:        %10d  (%8d bytes)\n",
	 NodeStats_NumBlocks(tsi),
	 NodeStats_NumAllocNodes(tsi),  NodeStats_SizeAllocNodes(tsi),
	 NodeStats_NumUsedNodes(tsi),  NodeStats_SizeUsedNodes(tsi),
	 NodeStats_NumFreeNodes(tsi),  NodeStats_SizeFreeNodes(tsi));
  printf("\n");

  if (flags[TRACE_STA]) {
    /* Report Maximum Usages
       --------------------- */
    printf("   Maximum stack use: global %ld, local %ld, trail %ld, cp %ld\n",
	   ttt.maxgstack_count, ttt.maxlstack_count, 
	   ttt.maxtrail_count, ttt.maxcpstack_count);
    printf("   Maximum stack use: SLG completion %ld (%ld subgoals)\n",
	   ttt.maxopenstack_count,
	   (ttt.maxopenstack_count/sizeof(struct completion_stack_frame)));
    update_maximum_tablespace_stats(&btn,&btht,&sf,&aln,&tstn,&tstht,&tsi);
    printf("   Maximum used ALNs  : %d  (%ld bytes)\n"
	   "   Maximum used TSINs: %d  (%ld bytes)\n",
	   maximum_answer_list_nodes(),
	   (long)(maximum_answer_list_nodes() * NodeStats_NodeSize(aln)),
	   maximum_timestamp_index_nodes(),
	   (long)(maximum_timestamp_index_nodes() * NodeStats_NodeSize(tsi)));
    printf("   Maximum total tablespace utilization:    %10ld bytes\n"
	   "     (BTNs, BTHTs, SFs, ALNs, TSTNs, TSTHTs, TSINs)\n",
	   maximum_total_tablespace_usage());
    printf("\n");
  }

#ifndef CHAT
  print_subsumption_stats();
#endif

  printf("\nVariant Operations:\n");
  printf("  %ld subgoal check/insert attempts", subg_chk_ins);
  printf(" inserted %5ld subgoals in the tables\n", subg_inserts);
  printf("  %ld answer check/insert attempts", ans_chk_ins);
  printf(" inserted %5ld answers in the tables\n", ans_inserts);

  if (de_count > 0) {
    printf("\n");
    printf(" %6d DEs in the tables (space: %5ld bytes allocated, %5ld in use)\n",
	   de_count, de_space_alloc, de_space_used);
    printf(" %6d DLs in the tables (space: %5ld bytes allocated, %5ld in use)\n",
	   dl_count, dl_space_alloc, dl_space_used);
  }

  printf("\n");

#ifdef CHAT
  print_chat_statistics();
#endif
#ifdef GC
  printf("\n");
  print_gc_statistics();
#endif

  printf("  Time: %.3f sec. cputime,  %.3f sec. elapsetime\n",
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
#ifdef CHAT
   reset_chat_statistics();
#endif
   time_start = cpu_time();
}

/*======================================================================*/

void reset_stat_total(void)
{
   ttt = trace_init;
}

/*======================================================================*/
