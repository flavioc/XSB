/* File:      memory_xsb.c
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
/* for the abstract machine.  The following interface routines are	*/
/* exported:								*/
/*	Program area handling:						*/
/*	    mem_alloc(size,cat):  alloc size bytes (on 8 byte boundary)	*/
/*	    mem_dealloc(addr,size,cat):  dealloc space			*/
/*      Stack area:                                                     */
/*          tcpstack_realloc(size)                                      */
/*          complstack_realloc(size)                                    */
/*======================================================================*/

/* xsb_config.h must be the first #include.  Please don't move it. */
#include "xsb_config.h"    /* needed by "cell_xsb.h" */
#include "xsb_debug.h"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "auxlry.h"
#include "binding.h"
#include "context.h"
#include "cell_xsb.h"
#include "memory_xsb.h"
#include "register.h"
#include "psc_xsb.h"
#include "tries.h"     /* needed by "choice.h" */
#include "choice.h"
#include "error_xsb.h"
#include "macro_xsb.h"
#include "thread_xsb.h"

#include "flags_xsb.h"
#include "subp.h"
#include "debug_xsb.h"

#if defined(GENERAL_TAGGING)

#if BITS64
#define _SHIFT_VALUE 60
#else
#define _SHIFT_VALUE 28
#endif

extern long int next_free_code;
extern unsigned long enc[], dec[];

void inline extend_enc_dec_as_nec(void *lptr, void *hptr) {
    unsigned long nibble;
    unsigned long lnibble = (unsigned long)lptr >> _SHIFT_VALUE;
    unsigned long hnibble = (unsigned long)hptr >> _SHIFT_VALUE;
    for (nibble = lnibble; nibble <= hnibble; nibble++) {
      if (enc[nibble] == -1) {
	SYS_MUTEX_LOCK_NOERROR(MUTEX_GENTAG);
	if (enc[nibble] == -1) { /* be sure not changed since test */
	  enc[nibble] = next_free_code << _SHIFT_VALUE;
	  dec[next_free_code] = nibble << _SHIFT_VALUE;
	  // printf("recoding %lx to %lx\n",nibble,next_free_code);
	  next_free_code++;
	}
	SYS_MUTEX_UNLOCK_NOERROR(MUTEX_GENTAG);
      }
    }
}
#endif

/* Statistics on number of mem_allocs (maintained for NON_OPT_COMPILE)
   -------------------------------------------------------------------- */

struct memcount_t {
  int num_mem_allocs;    // includes callocs
  int num_mem_reallocs;
  int num_mem_deallocs;
};

extern char *pspace_cat[];

#ifdef NON_OPT_COMPILE
struct memcount_t memcount_gl = {0,0,0};

void print_mem_allocs() {

  printf("\nSystem memory interactions since last use:\n");
  printf("Memory Allocations: %d\n",memcount_gl.num_mem_allocs);
  printf("Memory Reallocations: %d\n",memcount_gl.num_mem_reallocs);
  printf("Memory Deallocations: %d\n",memcount_gl.num_mem_deallocs);

  memcount_gl.num_mem_allocs = memcount_gl.num_mem_reallocs = memcount_gl.num_mem_deallocs = 0;
}
#else
void print_mem_allocs() {
}
#endif

//extern FILE *logfile;
//static long alloc_cnt = 0;  /* DSW for analyzing memory usage */

/* === alloc permanent memory ============================================== */

/* TLS: NOERROR locking done in mem_xxxoc() will not cause concurrency
   problems -- we just need to check for null ptr values and possibly
   abort after unlocking (when we check -- we're sometimes a little
   sloppy). */

void *mem_alloc(unsigned long size, int category)
{
    byte * ptr;

    size = (size+7) & ~0x7 ;	      /* round to 8 */

#ifdef NON_OPT_COMPILE
    memcount_gl.num_mem_allocs++;
    SYS_MUTEX_LOCK_NOERROR(MUTEX_MEM);
    pspacesize[category] += size;
#else
#ifndef MULTI_THREAD
    pspacesize[category] += size;
#endif
#endif

    ptr = (byte *) malloc(size);

#if defined(GENERAL_TAGGING)
    extend_enc_dec_as_nec(ptr,ptr+size);
#endif

#ifdef NON_OPT_COMPILE
    SYS_MUTEX_UNLOCK_NOERROR(MUTEX_MEM);
#endif

    if (ptr == NULL && size > 0) {
      char msgstring[40];
      sprintf(msgstring,"memory [%s]",pspace_cat[category]);
      xsb_memory_error(msgstring,"mem_alloc()");
    }
    //    fprintf(logfile,"alloc(mem_alloc,%ld,'%p',%ld,%d).\n",alloc_cnt++,ptr,size,category);
    return ptr;
}


/* TLS: does not check returns -- for use in error messages and
   throw */
void *mem_alloc_nocheck(unsigned long size, int category)
{
    byte * ptr;

    size = (size+7) & ~0x7 ;	      /* round to 8 */
#ifdef NON_OPT_COMPILE
    memcount_gl.num_mem_allocs++;
    SYS_MUTEX_LOCK_NOERROR(MUTEX_MEM);
    pspacesize[category] += size;
#else
#ifndef MULTI_THREAD
    pspacesize[category] += size;
#endif
#endif


    ptr = (byte *) malloc(size);
#if defined(GENERAL_TAGGING)
    //    printf("mem_alloc %x %x\n",ptr,ptr+size);
    extend_enc_dec_as_nec(ptr,ptr+size);
#endif
#ifdef NON_OPT_COMPILE
    SYS_MUTEX_UNLOCK_NOERROR(MUTEX_MEM);
#endif
    //    fprintf(logfile,"alloc(mem_alloc_nocheck,%ld,'%p',%ld,%d).\n",alloc_cnt++,ptr,size,category);
    return ptr;
}


/* === calloc permanent memory ============================================= */
void *mem_calloc(unsigned long size, unsigned long occs, int category)
{
    byte * ptr;
    unsigned long length = (size*occs+7) & ~0x7;

#ifdef NON_OPT_COMPILE
    //    printf("Callocing size %d occs %d category %d\n",size,occs,category);
    memcount_gl.num_mem_allocs++;
    SYS_MUTEX_LOCK_NOERROR(MUTEX_MEM);
    pspacesize[category] += length;
#else
#ifndef MULTI_THREAD
    pspacesize[category] += length;
#endif
#endif

    ptr = (byte *) calloc(size,occs);
#if defined(GENERAL_TAGGING)
    //    printf("mem_calloc %x %x\n",ptr,ptr+length);
    extend_enc_dec_as_nec(ptr,ptr+length);
#endif
#ifdef NON_OPT_COMPILE
    SYS_MUTEX_UNLOCK_NOERROR(MUTEX_MEM);
#endif
    if (ptr == NULL && size > 0 && occs > 0) {
      char msgstring[40];
      sprintf(msgstring,"memory [%s]",pspace_cat[category]);
      xsb_memory_error(msgstring,"mem_calloc()");
    }
    //    fprintf(logfile,"alloc(mem_calloc,%ld,'%p',%ld,%d).\n",alloc_cnt++,ptr,length,category);
    return ptr;
}

/* same as mem_calloc except return NULL on failure instead of throwing error */
void *mem_calloc_nocheck(unsigned long size, unsigned long occs, int category)
{
    byte * ptr;
    unsigned long length = (size*occs+7) & ~0x7;

#ifdef NON_OPT_COMPILE
    //    printf("Callocing size %d occs %d category %d\n",size,occs,category);
    memcount_gl.num_mem_allocs++;
    SYS_MUTEX_LOCK_NOERROR(MUTEX_MEM);
    pspacesize[category] += length;
#else
#ifndef MULTI_THREAD
    pspacesize[category] += length;
#endif
#endif

    ptr = (byte *) calloc(size,occs);
#if defined(GENERAL_TAGGING)
    //    printf("mem_calloc %x %x\n",ptr,ptr+length);
    extend_enc_dec_as_nec(ptr,ptr+length);
#endif
#ifdef NON_OPT_COMPILE
    SYS_MUTEX_UNLOCK_NOERROR(MUTEX_MEM);
#endif
    //    fprintf(logfile,"alloc(mem_calloc_nocheck,%ld,'%p',%ld,%d).\n",alloc_cnt++,ptr,length,category);
    return ptr;
}


/* === realloc permanent memory ============================================ */

void *mem_realloc(void *addr, unsigned long oldsize, unsigned long newsize, int category)
{
  byte *new_addr;
    newsize = (newsize+7) & ~0x7 ;	      /* round to 8 */
    oldsize = (oldsize+7) & ~0x7 ;	      /* round to 8 */
#ifdef NON_OPT_COMPILE
    memcount_gl.num_mem_reallocs++;
    SYS_MUTEX_LOCK_NOERROR(MUTEX_MEM);
    pspacesize[category] = pspacesize[category] - oldsize + newsize;
#else
#ifndef MULTI_THREAD
    pspacesize[category] = pspacesize[category] - oldsize + newsize;
#endif
#endif

    new_addr = (byte *) realloc(addr,newsize);
#if defined(GENERAL_TAGGING)
    extend_enc_dec_as_nec(new_addr,new_addr+newsize);
#endif
#ifdef NON_OPT_COMPILE
    SYS_MUTEX_UNLOCK_NOERROR(MUTEX_MEM);
#endif
    if (new_addr == NULL && newsize > 0) {
      char msgstring[40];
      sprintf(msgstring,"memory [%s]",pspace_cat[category]);
      xsb_memory_error(msgstring,"mem_realloc()");
    }
    //    fprintf(logfile,"alloc(mem_realloc,%ld,'%p','%p',%ld,%ld,%d).\n",alloc_cnt++,addr,new_addr,oldsize,newsize,category);
    return new_addr;
}

void *mem_realloc_nocheck(void *addr, unsigned long oldsize, unsigned long newsize, int category)
{
  byte *new_addr;
    newsize = (newsize+7) & ~0x7 ;	      /* round to 8 */
    oldsize = (oldsize+7) & ~0x7 ;	      /* round to 8 */
#ifdef NON_OPT_COMPILE
    memcount_gl.num_mem_reallocs++;
    SYS_MUTEX_LOCK_NOERROR(MUTEX_MEM);
    pspacesize[category] = pspacesize[category] - oldsize + newsize;
#else
#ifndef MULTI_THREAD
    pspacesize[category] = pspacesize[category] - oldsize + newsize;
#endif
#endif

    new_addr = (byte *) realloc(addr,newsize);
#if defined(GENERAL_TAGGING)
    extend_enc_dec_as_nec(new_addr,new_addr+newsize);
#endif
#ifdef NON_OPT_COMPILE
    SYS_MUTEX_UNLOCK_NOERROR(MUTEX_MEM);
#endif
    //    fprintf(logfile,"alloc(mem_realloc_nocheck,%ld,'%p','%p',%ld,%ld,%d).\n",alloc_cnt++,addr,new_addr,oldsize,newsize,category);
    return new_addr;
}


/* === dealloc permanent memory ============================================ */

void mem_dealloc(void *addr, unsigned long size, int category)
{
  //  int i;
    size = (size+7) & ~0x7 ;	      /* round to 8 */
#ifdef NON_OPT_COMPILE
    memcount_gl.num_mem_deallocs++;
    SYS_MUTEX_LOCK_NOERROR(MUTEX_MEM);
    //    if (size > 0) for (i=0; i<size/4-1; i++) *((CPtr *)addr + i) = (CPtr)0xefefefef;
    pspacesize[category] -= size;
#else
#ifndef MULTI_THREAD
    pspacesize[category] -= size;
#endif
#endif
    //    fprintf(logfile,"alloc(mem_dealloc,%ld,'%p',%ld,%d).\n",alloc_cnt++,addr,size,category);

    free(addr);
#ifdef NON_OPT_COMPILE
    SYS_MUTEX_UNLOCK_NOERROR(MUTEX_MEM);
#endif
}


/* === reallocate stack memory ============================================= */

/*
 * Re-allocate the space for the trail and choice point stack data area
 * to "new_size" K-byte blocks.
 */

void tcpstack_realloc(CTXTdeclc long new_size) {

  byte *cps_top,         /* addr of topmost byte in old CP Stack */
       *trail_top;       /* addr of topmost frame (link field) in old Trail */

  byte *new_trail,        /* bottom of new trail area */
       *new_cps;          /* bottom of new choice point stack area */

  long trail_offset,      /* byte offsets between the old and new */
       cps_offset;        /*   stack bottoms */

  CPtr *trail_link;    /* for stepping thru Trail and altering dyn links */
  CPtr *cell_ptr;      /* for stepping thru CPStack and altering cell values */
  byte *cell_val;      /* consider each cell to contain a ptr value */

  ComplStackFrame csf_ptr;    /* for stepping through the ComplStack */
  VariantSF subg_ptr;         /* and altering the CP ptrs in the SGFs */

#ifdef CONC_COMPL
  if( flags[NUM_THREADS] > 1 && openreg < COMPLSTACKBOTTOM )
	xsb_exit(CTXTc "Concurrent Completion doesn't yet support choice point stack expansion\n\
Please use -c N or cpsize(N) to start with a larger choice point stack"
	);
#endif

  if (new_size == tcpstack.size)
    return;

  cps_top = (byte *)top_of_cpstack;
  trail_top = (byte *)top_of_trail;

  //  fprintf(stddbg,"Reallocating the Trail and Choice Point Stack data area\n");

  /* Expand the Trail / Choice Point Stack Area
     ------------------------------------------ */
  if (new_size > tcpstack.size) {
    if (tcpstack.size == tcpstack.init_size) {
      xsb_dbgmsg((LOG_DEBUG, "\tBottom:\t\t%p\t\tInitial Size: %ldK",
		 tcpstack.low, tcpstack.size));
      xsb_dbgmsg((LOG_DEBUG, "\tTop:\t\t%p", tcpstack.high));
    }
    /*
     * Increase the size of the data area and push the Choice Point Stack
     * to its high-memory end.
     *
     * 'realloc' copies the data in the reallocated region to the new region.
     * 'memmove' can perform an overlap copy: we take the choice point data
     *   from where it was moved and push it to the high end of the newly
     *   allocated region.
     */
    new_trail = (byte *)realloc(tcpstack.low, new_size * K);
    if ( IsNULL(new_trail) )
      xsb_exit(CTXTc "Not enough core to resize the Trail and Choice Point Stack!");
    new_cps = new_trail + new_size * K;

#if defined(GENERAL_TAGGING)
    // seems not nec (any tagged pointers into here?)
    //       printf("tcpstack_realloc %p %p\n",new_trail,new_cps);
    //       extend_enc_dec_as_nec(new_trail,new_cps);
#endif

    trail_offset = (long)(new_trail - tcpstack.low);
    cps_offset = (long)(new_cps - tcpstack.high);
    memmove(cps_top + cps_offset,              /* move to */
	    cps_top + trail_offset,            /* move from */
	    (long)(tcpstack.high - cps_top) ); /* number of bytes */
  }
  /* Compact the Trail / Choice Point Stack Area
     ------------------------------------------- */
  else {
    /*
     * Float the Choice Point Stack data up to lower-memory and reduce the
     * size of the data area.
     */
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
  if (trail_offset != 0) {
    for (trail_link = (CPtr *)(trail_top + trail_offset);
	 trail_link > (CPtr *)new_trail;
	 trail_link = trail_link - 3) {
      *trail_link = (CPtr)((byte *)*trail_link + trail_offset);
      /* Check if this is a 4 word trail frame */
      if (((Cell)*(trail_link-2) & PRE_IMAGE_MARK)) trail_link--;
    }
    /* Also have to fix the bottommost trail frame - not done in the above
       cycle because of the trail_link-2 check */
    *trail_link = (CPtr)((byte *)*trail_link + trail_offset);
  }

  /* Update the pointers in the CP Stack
     ----------------------------------- */
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
      /*
       *  If the cell points into the region between the two stacks, then
       *  this may signal a bug in the engine.
       */
      else if ( (cell_val < cps_top) && (cell_val > trail_top) )
	xsb_warn("During Trail / Choice Point Stack Reallocation\n\t   "
		 "Erroneous pointer:  Points between Trail and CP Stack tops"
		 "\n\t   Addr:%p, Value:%p", cell_ptr, cell_val);
    }
  }

  /* Update the Subgoal Frames' pointers into the CP Stack 
     ----------------------------------------------------- */
  for (csf_ptr = (ComplStackFrame)openreg;
       csf_ptr < (ComplStackFrame)complstack.high;
       csf_ptr++) {

    subg_ptr = compl_subgoal_ptr(csf_ptr);
    if ( IsNonNULL(subg_asf_list_ptr(subg_ptr)) )
      subg_asf_list_ptr(subg_ptr) =
	(CPtr)( (byte *)subg_asf_list_ptr(subg_ptr) + cps_offset );

    if ( IsNonNULL(subg_compl_susp_ptr(subg_ptr)) )
      subg_compl_susp_ptr(subg_ptr) =
	(CPtr)( (byte *)subg_compl_susp_ptr(subg_ptr) + cps_offset );
    if ( IsNonNULL(subg_cp_ptr(subg_ptr)) )
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
  trfreg = (CPtr *)((byte *)trfreg + trail_offset);
  bfreg = (CPtr)((byte *)bfreg + cps_offset);
  if ( IsNonNULL(root_address) )
    root_address = (CPtr)((byte *)root_address + cps_offset);

  xsb_dbgmsg((LOG_DEBUG, "\tNew Bottom:\t%p\t\tNew Size: %ldK",
	     tcpstack.low, tcpstack.size));
  xsb_dbgmsg((LOG_DEBUG, "\tNew Top:\t%p\n", tcpstack.high));
}

/* ------------------------------------------------------------------------- */

void handle_tcpstack_overflow(CTXTdecl)
{
  if (pflags[STACK_REALLOC]) {
    xsb_warn("Expanding the Trail and Choice Point Stack...");
    tcpstack_realloc(CTXTc resize_stack(tcpstack.size,0));
  }
  else {
    xsb_exit(CTXTc "Trail/ChoicePoint stack overflow detected but expansion is off");
  }
}

/* ------------------------------------------------------------------------- */

/*
 * Re-allocate the space for the completion stack data area to "new_size"
 * K-byte blocks.
 */

void complstack_realloc (CTXTdeclc long new_size) {

  byte *new_top,    /* bottom of new trail area */
  *new_bottom;      /* bottom of new choice point stack area */

  long top_offset,   /* byte offsets between the old and new */
  bottom_offset;    /*    stack bottoms */

  byte *cs_top;     /* ptr to topmost byte on the complstack */

  ComplStackFrame csf_ptr;
  VariantSF subg_ptr;

#ifdef CONC_COMPL
  byte **cp_ptr ;
#endif

  
  if (new_size == complstack.size)
    return;
  
  cs_top = (byte *)top_of_complstk;
  
  xsb_dbgmsg((LOG_DEBUG, "Reallocating the Completion Stack"));

  /* Expand the Completion Stack
     --------------------------- */
  if (new_size > complstack.size) {
    if (complstack.size == complstack.init_size) { 
      xsb_dbgmsg((LOG_DEBUG, "\tBottom:\t\t%p\t\tInitial Size: %ldK",
		 complstack.low, complstack.size));
      xsb_dbgmsg((LOG_DEBUG, "\tTop:\t\t%p", complstack.high));
    }

    /*
     * Increase the size of the data area and push the Completion Stack
     * to its high-memory end.
     */
    new_top = (byte *)realloc(complstack.low, new_size * K);
    if ( IsNULL(new_top) )
      xsb_exit(CTXTc "Not enough core to resize the Completion Stack!");
    new_bottom = new_top + new_size * K;
    
    top_offset = (long)(new_top - complstack.low);
    bottom_offset = (long)(new_bottom - complstack.high);
    /* the following floats the completion stack from the middle
     * of the re-allocated block of memory to its upper end
     */
    memmove(cs_top + bottom_offset,     /* move to */
	    cs_top + top_offset,        /* move from */
	    (long)(complstack.high - cs_top) );
  }
  /* Compact the Completion Stack
     ---------------------------- */
  else {
    /*
     * Float the Completion Stack data up to lower-memory and reduce the
     * size of the data area.
     */
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
#ifdef CONC_COMPL
    /* In CONC_COMPL there might be completion stack frames pointing to
       subgoal frames owned by other threads which in turn point
       to the other thread's completion stack */
    if( subg_tid(subg_ptr) == xsb_thread_id )
#endif
    subg_compl_stack_ptr(subg_ptr) =
      (CPtr)((byte *)subg_compl_stack_ptr(subg_ptr) + bottom_offset);
  } 

#ifdef CONC_COMPL
  /* In CONC_COMPL there are pointers from the choice points into
     the completion stack */

  for ( cp_ptr = (byte **)(top_of_cpstack);
	cp_ptr < (byte **)tcpstack.high;
	cp_ptr++ )
	if( *cp_ptr >= cs_top && *cp_ptr < complstack.high )
		*cp_ptr += bottom_offset ;
#endif

  /* Update the system variables
     --------------------------- */
  complstack.low = new_top;
  complstack.high = new_bottom;
  complstack.size = new_size;
  
  openreg = (CPtr)((byte *)openreg + bottom_offset);

  xsb_dbgmsg((LOG_DEBUG, "\tNew Bottom:\t%p\t\tNew Size: %ldK",
	     complstack.low, complstack.size));
  xsb_dbgmsg((LOG_DEBUG, "\tNew Top:\t%p\n", complstack.high));
}
