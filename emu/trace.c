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


#include <stdio.h>

#include "configs/config.h"
#include "debugs/debug.h"

#include "auxlry.h"
#include "cell.h"
#include "inst.h"
#include "memory.h"
#include "register.h"
#include "tries.h"
#include "choice.h"
#include "flags.h"
#include "psc.h"
#include "xmacro.h"

#ifdef CHAT
#include "chat.h"
#endif

extern int count_subgoals(void);

double time_start;

struct trace_str tds;			/* trace datastructure */
struct trace_str ttt;			/* trace total */
struct trace_str trace_init = {		/* initial value for a trace str */
    0, 0, 0, 0, 0, 0, 0.0
   };

/*======================================================================*/
/* perproc_stat()							*/
/*======================================================================*/

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

void total_stat(double elapstime)
{
  unsigned long lstktop, trie_alloc, trie_used, chat_alloc, chat_used,
                total_alloc, total_used, gl_avail, tc_avail;
  unsigned long subg_count, subg_space, trie_hash_alloc;

  lstktop = (unsigned long)ereg;

  subg_count = count_subgoals();
  trie_hash_alloc = allocated_trie_hash_size();
  trie_alloc = allocated_trie_size() + trie_hash_alloc;
  trie_used = trie_alloc - free_trie_size();
  subg_space = sizeof(Cell)*CALLSTRUCTSIZE*subg_count;

#ifdef CHAT
  chat_alloc = chat_max_alloc();
  chat_used = chat_now_used();
#else
  chat_alloc = chat_used = 0;
#endif

  total_alloc = pspacesize + trie_alloc + subg_space + chat_alloc +
		(pdl.size + glstack.size + tcpstack.size + complstack.size) * K;

  gl_avail    = lstktop-(unsigned long)hreg;
  tc_avail    = (unsigned long)breg-(unsigned long)trreg;

  total_used  = pspacesize + trie_used + subg_space + chat_used +
                (glstack.size * K - gl_avail) + (tcpstack.size * K - tc_avail);

  printf("\n");
  printf("memory (total)    %12ld bytes: %12ld in use, %12ld free\n",
	 total_alloc, total_used, total_alloc-total_used);
  printf("  permanent space %12ld bytes\n", pspacesize);
  printf("  glob/loc space  %12ld bytes: %12ld in use, %12ld free\n",
	 glstack.size * K, glstack.size * K - gl_avail, gl_avail);
  printf("    global                            %12ld bytes\n",
	 (unsigned long)hreg - (unsigned long)glstack.low);
  printf("    local                             %12ld bytes\n",
	 (unsigned long)glstack.high - lstktop);
  printf("  trail/cp space  %12ld bytes: %12ld in use, %12ld free\n",
	 tcpstack.size * K, tcpstack.size * K - tc_avail, tc_avail);
  printf("    trail                             %12ld bytes\n",
	 (unsigned long)trreg - (unsigned long)(tcpstack.low));
  printf("    choice point                      %12ld bytes\n",
	 (unsigned long)tcpstack.high - (unsigned long)breg);
  printf("  SLG subgoal space %10ld bytes: %12ld in use, %12ld free\n",
	 sizeof(Cell)*subg_count*(unsigned long)CALLSTRUCTSIZE,
	 sizeof(Cell)*subg_count*(unsigned long)CALLSTRUCTSIZE,0L);
  printf("  SLG unific. space %10ld bytes: %12ld in use, %12ld free\n",
	 pdl.size * K, (unsigned long)(pdlreg+1) - (unsigned long)pdl.high,
	 pdl.size * K - ((unsigned long)(pdlreg+1)-(unsigned long)pdl.high)); 
  printf("  SLG completion  %12ld bytes: %12ld in use, %12ld free\n",
	 (unsigned long)complstack.size * K,
	 (unsigned long)COMPLSTACKBOTTOM - (unsigned long)openreg,
	 (unsigned long)complstack.size * K -
	 ((unsigned long)COMPLSTACKBOTTOM - (unsigned long)openreg));
  printf("  SLG trie space    %10ld bytes: %12ld in use, %12ld free\n",
	 trie_alloc, trie_used, trie_alloc - trie_used);
  printf("   (call+ret. trie%12ld bytes,     trie hash tables %12ld bytes)\n",
	 (long)allocated_trie_size(), trie_hash_alloc);
  printf("\n");

  if (flags[TRACE_STA]) {
    printf("   Maximum stack use: global %ld, local %ld, trail %ld, cp %ld\n",
	   ttt.maxgstack_count, ttt.maxlstack_count, 
	   ttt.maxtrail_count, ttt.maxcpstack_count);
    total_used = ttt.maxgstack_count + ttt.maxlstack_count +
                 ttt.maxtrail_count + ttt.maxcpstack_count +
		 ttt.maxopenstack_count;
    printf("   Maximum stack use: SLG completion %ld, Total %ld.  Max level: %ld\n",
	   ttt.maxopenstack_count, total_used, ttt.maxlevel_num);
    printf("\n");
  }

  printf(" %6ld subgoal check/insert attempts", subg_chk_ins);
  printf(" inserted %5ld subgoals in the tables\n", subg_inserts);
  printf(" %6ld answer  check/insert attempts", ans_chk_ins);
  printf(" inserted %5ld answers  in the tables\n", ans_inserts);

/*     if (ide_count > 0) { */
/* 	printf(" %5d ide tab check/insert attempts", ide_chk_ins); */
/* 	printf(" interned %5d dl elems in the tables\n", ide_count); */
/* 	printf(" %5d idl tab check/insert attempts", idl_chk_ins); */
/* 	printf(" interned %5d dl lists in the tables\n\n", idl_count); */
/*     } else printf("\n"); */

#ifdef CHAT
  printf("\n");
  print_chat_statistics();
#endif
  printf("\n  currently active:   %6ld subgoals, %5d levels\n\n",
	 subg_count, level_num);
#ifdef GC
  print_gc_statistics();
#endif

  printf(" %.3f sec. cputime,  %.3f sec. elapsetime\n",
	 ttt.time_count, elapstime);
}

void perproc_reset_stat(void)
{
   tds = trace_init;
/*    ide_chk_ins = idl_chk_ins = 0; */
   ans_chk_ins = ans_inserts = 0;
   subg_chk_ins = subg_inserts = 0;
#ifdef CHAT
   reset_chat_statistics();
#endif
   time_start = cpu_time();
}

void reset_stat_total(void)
{
   ttt = trace_init;
}
