/* File:      xsb_memory.c
** Author(s): Ernie Johnson, Swift, Xu, Sagonas
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


/*======================================================================*/
/* This module provides abstractions of memory management functions	*/
/* for the abstract machine. The following interface routines are	*/
/* exported:								*/
/*	Program area handling:						*/
/*	    mem_alloc(size):  alloc size bytes (on 8 byte boundary)	*/
/*	    mem_dealloc(addr, size):  dealloc space			*/
/*      Stack area:                                                     */
/*          tcpstack_realloc(size)                                      */
/*          complstack_realloc(size)                                    */
/*======================================================================*/

/* config.h must be the first #include.  Please don't move it. */
#include "configs/config.h"    /* needed by "cell.h" */
#include "debugs/debug.h"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "auxlry.h"
#include "cell.h"      /* CPtr */
#include "xsb_memory.h"
#include "register.h"  /* breg, trreg */
#include "psc.h"       /* needed by "tries.h" and "xmacro.h" */
#include "tries.h"     /* needed by "choice.h" */
#include "choice.h"    /* choice point structures and macros */
#include "xsberror.h"  /* xsb_exit() */
#include "xmacro.h"    /* Completion Stack and Subgoal Frame def's */

#include "flags.h"
#include "subp.h"

/* === alloc permanent memory ============================================== */

byte *mem_alloc(unsigned long size)
{
    byte * ptr;

    size = (size+7) & ~0x7 ;	      /* round to 8 */
    pspacesize += size;
    ptr = (byte *) malloc(size);
    return ptr;
}


/* === dealloc permanent memory ============================================ */

void mem_dealloc(byte *addr, unsigned long size)
{
    size = (size+7) & ~0x7 ;	      /* round to 8 */
    pspacesize -= size;
    free(addr);
}


/* === reallocate stack memory ============================================= */

/*
 * Re-allocate the space for the trail and choice point stack data area
 *   to "new_size" K-byte blocks.
 */

void tcpstack_realloc(long new_size) {

/* Variables for Trail and Choice Point Stack Updating
   --------------------------------------------------- */
  byte *new_trail,        /* bottom of new trail area */
       *new_cps;          /* bottom of new choice point stack area */

  long trail_offset,      /* byte offsets between the old and new */
       cps_offset;        /*    stack bottoms */

#ifndef CHAT
  CPtr *trail_link;    /* for stepping thru Trail and altering dyn links */
#endif
  CPtr *cell_ptr;      /* for stepping thru CPStack and altering cell values */
  byte *cell_val;      /* consider each cell to contain a ptr value */

  /*
   * These two are just place holders, pointing into the OLD stack regions.
   * Primarily needed for determining into what region, trail or cp, cells
   * were pointing.
   */
  byte *cps_top,     /* ptr to topmost byte on the CP Stack */
       *trail_top;   /* ptr into topmost frame on the Trail (link field) */


  /* Variables for Updating Incomplete Subgoal's SGFs
     ------------------------------------------------- */
  ComplStackFrame csf_ptr;    /* for stepping through the ComplStack */
  SGFrame subg_ptr;           /* and altering the CP ptrs in the SGFs */


  if (new_size == tcpstack.size)
    return;

  /* Compute stack tops
     ------------------ */
  cps_top = (byte *)top_of_cpstack;
  trail_top = (byte *)top_of_trail;

  if (new_size > tcpstack.size) {

    /* Reallocate the data area
       ------------------------ */
    new_trail = (byte *)realloc(tcpstack.low, new_size * K);
    if (new_trail == NULL)
      xsb_exit("Not enough core to resize the Trail and Choice Point Stack!");
    new_cps = new_trail + new_size * K;

    /* Push the CP Stack to the high end
       --------------------------------- */
    /*
     * 'realloc' copies the data in the reallocated region to the new region.
     * 'memmove' can perform an overlap copy: we take the choice point data
     *   from where it was moved and push it to the high end of the newly
     *   allocated region.
     */
    trail_offset = (long)(new_trail - tcpstack.low);
    cps_offset = (long)(new_cps - tcpstack.high);
    memmove(cps_top + cps_offset,              /* move to */
	    cps_top + trail_offset,            /* move from */
	    (long)(tcpstack.high - cps_top) ); /* number of bytes */
  }
  else {

    /* Float the CP Stack data up and reallocate
       ----------------------------------------- */
    memmove(cps_top - (tcpstack.size - new_size) * K,  /* move to */
	    cps_top,                                   /* move from */
	    (long)(tcpstack.high - cps_top) );         /* number of bytes */
    new_trail = (byte *)realloc(tcpstack.low, new_size * K);
    trail_offset = (long)(new_trail - tcpstack.low);
    new_cps = new_trail + new_size * K;
    cps_offset = (long)(new_cps - tcpstack.high);
  }

  /* Update the Trail links
     ---------------------- */
  /*
   *  Start at the base of the Trail and work up.  The base has a self-
   *  pointer to mark the bottom of the Trail.  The TRx reg's point to the
   *  previous-link field of the Trail frames (i.e., the last field).
   */
#ifndef CHAT
  if (trail_offset != 0) {
    for (trail_link = (CPtr *)new_trail;
	 trail_link <= (CPtr *)(trail_top + trail_offset);
	 trail_link = trail_link + 3)
      *trail_link = (CPtr)((byte *)*trail_link + trail_offset);
  }
#endif

  /* Update the CP Stack pointers
     ---------------------------- */
  /*
   *  Start at the top of the CP Stack and work down, stepping through
   *  each field of every frame on the stack.  Any field containing a
   *  pointer either into the Trail or CP Stack must be updated.
   *  Bx reg's point to the first field of any CP frame.
   */
  for (cell_ptr = (CPtr *)(cps_top + cps_offset);
       cell_ptr < (CPtr *)new_cps;
       cell_ptr++) {
    
    cell_val = (byte *)*cell_ptr;
    if(isref(cell_val)) {
      /*
       *  If the cell points into the CP Stack, adjust with offset.
       */
      if ( (cell_val >= cps_top) && (cell_val < tcpstack.high) )
	*cell_ptr = (CPtr)(cell_val + cps_offset);
      /*
       *  If the cell points into the Trail, adjust with offset.
       */
      else if ( (cell_val >= tcpstack.low) && (cell_val <= trail_top) )
	*cell_ptr = (CPtr)(cell_val + trail_offset);
      else if ( (cell_val < cps_top) && (cell_val > trail_top) ) {
	printf("Trail/Choice Point Stack Reallocation Error!\n");
	printf("Erroneous pointer:  points between Trail and CP stack tops\n");
	printf("\taddr:%p, value:%p\n", cell_ptr, cell_val);
      }
    }
  }

  /* Update the Subgoal Frames' pointers into the CP Stack 
     ----------------------------------------------------- */
  for (csf_ptr = (ComplStackFrame)openreg;
       csf_ptr < (ComplStackFrame)complstack.high;
	 csf_ptr++) {
    subg_ptr = (SGFrame)compl_subgoal_ptr(csf_ptr);
    /* Alter specific fields
       --------------------- */
#if (!defined(CHAT))
    if (subg_asf_list_ptr(subg_ptr) != NULL)
      subg_asf_list_ptr(subg_ptr) =
	(CPtr)( (byte *)subg_asf_list_ptr(subg_ptr) + cps_offset );
#endif
    if (subg_compl_susp_ptr(subg_ptr) != NULL)
      subg_compl_susp_ptr(subg_ptr) =
	(CPtr)( (byte *)subg_compl_susp_ptr(subg_ptr) + cps_offset );
    if (subg_cp_ptr(subg_ptr) != NULL)
      subg_cp_ptr(subg_ptr) =
	(CPtr)((byte *)subg_cp_ptr(subg_ptr) + cps_offset);
  }
						     
  /* Update the system variables
     --------------------------- */
  tcpstack.low = new_trail;
  tcpstack.high = new_cps;
  tcpstack.size = new_size;
  
  trreg = (CPtr *)((byte *)trreg + trail_offset);
  breg = (CPtr)((byte *)breg + cps_offset);
#if (!defined(CHAT))
  trfreg = (CPtr *)((byte *)trfreg + trail_offset);
  bfreg = (CPtr)((byte *)bfreg + cps_offset);
#endif
  if (root_address != NULL)
    root_address = (CPtr)((byte *)root_address + cps_offset);
}

/* ------------------------------------------------------------------------- */

void handle_tcpstack_overflow(void)
{
  if (flags[STACK_REALLOC]) {
#ifdef DEBUG
    fprintf(stderr, "Expanding trail and choice point stack...\n");
#endif
    tcpstack_realloc(resize_stack(tcpstack.size,0));
  }
  else {
    xsb_exit("Trail/ChoicePoint stack overflow detected but expansion is off");
  }
}

/* ------------------------------------------------------------------------- */

/*
 * Re-allocate the space for the completion stack data area to "new_size"
 *   K-byte blocks.
 */

void complstack_realloc (long new_size) {

  byte *new_top,    /* bottom of new trail area */
  *new_bottom;      /* bottom of new choice point stack area */

  long top_offset,   /* byte offsets between the old and new */
  bottom_offset;    /*    stack bottoms */

  byte *cs_top;     /* ptr to topmost byte on the complstack */

  ComplStackFrame csf_ptr;
  SGFrame subg_ptr;

  
  if (new_size == complstack.size)
    return;
  
  cs_top = (byte *)top_of_complstk;
  
  if (new_size > complstack.size) {

    /* Reallocate the data area
       ------------------------ */
    new_top = (byte *)realloc(complstack.low, new_size * K);
    if (new_top == NULL)
      xsb_exit("Not enough core to resize the Completion Stack!");
    new_bottom = new_top + new_size * K;
    
    /* Push the completion stack to the high end
       ----------------------------------------- */
    top_offset = (long)(new_top - complstack.low);
    bottom_offset = (long)(new_bottom - complstack.high);
    memmove(cs_top + bottom_offset,     /* move to */
	    cs_top + top_offset,        /* move from */
	    (long)(complstack.high - cs_top) );
  }
  else {

    /* Float the Completion Stack data up and reallocate
       ------------------------------------------------- */
    memmove(cs_top - (complstack.size - new_size) * K,  /* move to */
	    cs_top,                                     /* move from */
	    (long)(complstack.high - cs_top) );         /* number of bytes */
    new_top = (byte *)realloc(complstack.low, new_size * K);
    top_offset = (long)(new_top - complstack.low);
    new_bottom = new_top + new_size * K;
    bottom_offset = (long)(new_bottom - complstack.high);
  }

  /* Update subgoals' pointers into the completion stack
     --------------------------------------------------- */
  for (csf_ptr = (ComplStackFrame)(cs_top + bottom_offset);
       csf_ptr < (ComplStackFrame)new_bottom;
       csf_ptr++) {
    subg_ptr = compl_subgoal_ptr(csf_ptr);
    subg_compl_stack_ptr(subg_ptr) =
      (CPtr)((byte *)subg_compl_stack_ptr(subg_ptr) + bottom_offset);
  }


  /* Update the system variables
     --------------------------- */
  complstack.low = new_top;
  complstack.high = new_bottom;
  complstack.size = new_size;
  
  openreg = (CPtr)((byte *)openreg + bottom_offset);
}
