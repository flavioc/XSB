/* File:      memory_xsb.h
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


/*===========================================================================*/

/*
 *			SLG-WAM Stack Management
 *			========================
 *
 * Information on each independent data area needed by the slg-wam is kept
 * in one of these structures.  "low" and "high" point into memory to
 * delineate the bounds of the data area: "low" gets the address returned
 * by an allocation routine, while "high" gets "low" + "size" * K.  Note:
 * this means that (1) "size" represents the number of K-byte blocks
 * allocated for this stack, and (2) "high" points off the end of the
 * allocated area.
 *
 * The logical, data structure specific details of how a region is used,
 * e.g., which pointer represents the bottom of a stack, is represented in
 * the code and the documentation.  "init_size" is used for storing the size
 * the stack was initialized to, for purposes of restoring it to this size
 * after each query via the trimcore/0 predicate.
 */


/* Info Structure for the Data Regions
   ----------------------------------- */
typedef struct stack_info {
   byte *low;
   byte *high;
   long size;
   long init_size;
} System_Stack;


/* The SLG-WAM System Data Regions
   ------------------------------- */
extern System_Stack pdl,            /* PDL                        */
                    glstack,        /* Global + Local Stacks      */
                    tcpstack,       /* Trail + Choice Point Stack */
                    complstack;     /* Completion Stack           */


/*
 *  Finding the tops of stacks.
 *  ---------------------------
 *    In this form, the result can be used immediately for deref, recast,
 *    etc., as well as for assignment.  ALL macros return a pointer to the
 *    topmost USED cell on their respective stack.
 */
#define top_of_heap      (hreg - 1)
#ifdef CHAT
#define top_of_localstk	 ( (ereg < ebreg) \
			      ? ereg - *(cpreg - 2*sizeof(Cell)+3) + 1 \
			      : ebreg )
#define top_of_trail	 trreg
#define top_of_cpstack	 breg
#else
#define top_of_localstk  ( ((efreg < ebreg) && (efreg < ereg)) \
                           ? efreg  \
			   : ( (ereg < ebreg) \
			       ? ereg - *(cpreg - 2*sizeof(Cell)+3) + 1 \
			       : ebreg ) )
#define top_of_trail     ((trreg > trfreg) ? trreg : trfreg)
#define top_of_cpstack   ((breg < bfreg) ? breg : bfreg)
#endif

#define top_of_complstk  openreg

/* Testing pointer addresses
   ------------------------- */
#define IsInHeap(Ptr)	( ( (CPtr)(Ptr) <= top_of_heap ) &&	\
    			  ( (CPtr)(Ptr) >= (CPtr)glstack.low ) )

#define IsInEnv(Ptr)	( ( (CPtr)(Ptr) < (CPtr)glstack.high ) &&	\
			  ( (CPtr)(Ptr) >= top_of_localstk) )

#define IsInTrail(Ptr)	( ( (CPtr)(Ptr) <= (CPtr)top_of_trail ) &&	\
    			  ( (CPtr)(Ptr) >= (CPtr)cpstack.low ) )

#define IsInCPS(Ptr)	( ( (CPtr)(Ptr) < (CPtr)cpstack.high ) &&	\
			  ( (CPtr)(Ptr) >= top_of_cpstack) )


#define COMPLSTACKBOTTOM ((CPtr) complstack.high)


/*
 *  Size of margin between facing stacks before reallocating a larger area.
 */
#define OVERFLOW_MARGIN	(2048 * ZOOM_FACTOR)


/* Calculate New Stack Size
   ------------------------ */
#define resize_stack(stack_size,min_exp) /*"stack_size" is in K-byte blocks*/\
   (((unsigned long)stack_size) < (min_exp)/K ? (stack_size) + (min_exp)/K : 2 * (stack_size))


/* Program and Symbol Tables Space (in Bytes)
   ------------------------------------------ */
extern long pspacesize;


/* Memory Function Prototypes
   -------------------------- */
extern byte *mem_alloc(unsigned long);
extern void mem_dealloc(byte *, unsigned long);
extern void tcpstack_realloc(long);
extern void complstack_realloc(long);
extern void handle_tcpstack_overflow(void);


/* Instruction Externs
   ------------------- */
extern byte *inst_begin;       /* ptr to beginning of instruction array. */

extern Cell answer_return_inst, check_complete_inst, hash_handle_inst,
	    resume_compl_suspension_inst, fail_inst, halt_inst, proceed_inst,
	    reset_inst;


/* Stack Overflow Checkers
   ----------------------- */

#ifdef DEBUG

#define check_tcpstack_overflow {					    \
									    \
   CPtr cps_top = top_of_cpstack;					    \
									    \
   if ((pb)cps_top < (pb)top_of_trail + OVERFLOW_MARGIN) {		    \
     if ((pb)cps_top < (pb)top_of_trail) {				    \
       xsb_error("Trail clobbered Choice Point Stack");	       	       	    \
       print_statistics(1);						    \
       trail_cp_exception(lpcreg);					    \
       XSB_Next_Instr();       						    \
     }									    \
     else {								    \
       fprintf(stdwarn, "\nTrail / Choice Point Stack overflow:   ");	    \
       if (flags[STACK_REALLOC]) {					    \
         fprintf(stdwarn, "Expanding ...\n");				    \
         if (tcpstack.size == tcpstack.init_size) {			    \
           fprintf(stdwarn, "\tBottom:\t\t0x%p\t\tInitial Size: %ldK\n",    \
                   tcpstack.low, tcpstack.size);			    \
           fprintf(stdwarn, "\tTop:\t\t0x%p\n", tcpstack.high);		    \
         }								    \
         tcpstack_realloc(resize_stack(tcpstack.size,0));		    \
         fprintf(stdwarn, "\tNew Bottom:\t0x%p\t\tNew Size: %ldK\n",	    \
                 tcpstack.low, tcpstack.size);				    \
         fprintf(stdwarn, "\tNew Top:\t0x%p\n", tcpstack.high);		    \
       }								    \
       else {								    \
         fprintf(stdwarn, "Reallocation turned OFF!\n");		    \
         print_statistics(1);						    \
         trail_cp_exception(lpcreg);					    \
         XSB_Next_Instr();     						    \
       }								    \
     }									    \
   }									    \
 }

#define check_glstack_overflow(arity,PCREG,EXTRA,todo_on_exception)          \
    if ((pb)top_of_localstk < (pb)top_of_heap + OVERFLOW_MARGIN + EXTRA) {   \
        if ((pb)top_of_localstk < (pb)top_of_heap) {                         \
            PCREG = exception_handler("\nFatal ERROR:  -- Local Stack clobbered Heap --\n");  \
        todo_on_exception;                                                   \
      }                                                                      \
      else {                                                                 \
        fprintf(stdwarn, "\nHeap / Local Stack overflow:   ");            \
        if (flags[STACK_REALLOC]) {                                          \
          fprintf(stdwarn, "Expanding ...\n");                                \
          if (glstack.size == glstack.init_size) {                           \
            fprintf(stdwarn, "\tBottom:\t\t0x%p\t\tInitial Size: %ldK\n",     \
                    glstack.low, glstack.size);                              \
            fprintf(stdwarn, "\tTop:\t\t0x%p\n", glstack.high);               \
          }                                                                  \
          glstack_realloc(resize_stack(glstack.size,EXTRA+OVERFLOW_MARGIN),  \
                       arity);                       \
          fprintf(stdwarn, "\tNew Bottom:\t0x%p\t\tNew Size: %ldK\n",         \
                  glstack.low, glstack.size);                                \
          fprintf(stdwarn, "\tNew Top:\t0x%p\n", glstack.high);               \
        }                                                                    \
        else {                                                               \
          fprintf(stdwarn, "Reallocation turned OFF!\n");                     \
          print_statistics(1);                                               \
          local_global_exception(PCREG);                                     \
          todo_on_exception;                                                 \
       }                                                                     \
      }                                                                      \
    }

#define check_completion_stack_overflow                               	\
   if ( (pb)openreg < (pb)complstack.low + OVERFLOW_MARGIN ) {        	\
     fprintf(stdwarn, "\nCompletion Stack overflow:   ");              	\
     if (flags[STACK_REALLOC]) {                                      	\
       fprintf(stdwarn, "Expanding ...\n");                            	\
       if (complstack.size == complstack.init_size) {                 	\
         fprintf(stdwarn, "\tBottom:\t\t0x%p\t\tInitial Size: %ldK\n", 	\
                 complstack.low, complstack.size);                    	\
         fprintf(stdwarn, "\tTop:\t\t0x%p\n", complstack.high);        	\
       }                                                              	\
       complstack_realloc(resize_stack(complstack.size,0));           	\
       fprintf(stdwarn, "\tNew Bottom:\t0x%p\t\tNew Size: %ldK\n",     	\
	       complstack.low, complstack.size);                      	\
       fprintf(stdwarn, "\tNew Top:\t0x%p\n", complstack.high);        	\
     }                                                                	\
     else {                                                           	\
       fprintf(stdwarn, "Reallocation turned OFF!\n");                 	\
       print_statistics(1);                                        	\
       complstack_exception(lpcreg);                               	\
       XSB_Next_Instr();                                              	\
     }                                                      		\
     fflush(stdwarn);                                        		\
   }


#else


#define check_tcpstack_overflow {					\
									\
   CPtr cps_top = top_of_cpstack;					\
									\
   if ((pb)cps_top < (pb)top_of_trail + OVERFLOW_MARGIN) {		\
     if ((pb)cps_top < (pb)top_of_trail) {				\
       lpcreg = exception_handler("\nFatal ERROR:  --Trail "		\
				  "clobbered Choice Point Stack--\n");	\
       XSB_Next_Instr();       						\
     }									\
     else {								\
       if (flags[STACK_REALLOC])					\
         tcpstack_realloc(resize_stack(tcpstack.size,0));		\
       else {								\
         trail_cp_exception(lpcreg);					\
         XSB_Next_Instr();	       					\
       }								\
     }									\
   }									\
 }

#define check_glstack_overflow(arity,PCREG,EXTRA,todo_on_exception)  \
    if ((pb)top_of_localstk < (pb)top_of_heap + OVERFLOW_MARGIN + EXTRA) {   \
      if ((pb)top_of_localstk < (pb)top_of_heap) {                         \
        PCREG = exception_handler("\nFatal ERROR:  -- Local Stack clobbered Heap --\n");  \
        todo_on_exception;                                                   \
      }                                                                      \
      else {                                                                 \
        if ((flags[STACK_REALLOC] == FALSE) ||                               \
          (glstack_realloc(resize_stack(glstack.size,EXTRA+OVERFLOW_MARGIN), \
                          arity) != 0)) {                                    \
          local_global_exception(PCREG);                                     \
          todo_on_exception;                                                 \
        }                                                                    \
      }                                                                      \
    }

#define check_completion_stack_overflow                                      \
    if ( (pb)openreg < (pb)complstack.low + OVERFLOW_MARGIN ) {              \
      if (flags[STACK_REALLOC])                                              \
        complstack_realloc(resize_stack(complstack.size,0));                 \
      else {                                                                 \
	complstack_exception(lpcreg);                                        \
	XSB_Next_Instr();                                                    \
      }                                                                      \
    }

#endif
