/* File:      tst_aux.h
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


#ifndef PRIVATE_TST_DEFS

#define PRIVATE_TST_DEFS

#include "debugs/debug_tables.h"


/*===========================================================================*/

/*
 *                     Auxiliary Data Structures
 *                     =========================
 *
 *  This file contains typedefs of and macros for using some objects
 *  universally needed by the subsumptive components of the engine,
 *  i.e. the operations:
 *
 *  i) Subsumptive Call Check/Insert
 *  ii) Variant TST Answer Check/Insert
 *  iii) TST Answer Retrieval
 *  iv) Answer Consumption
 */


/* We could have each file declare its own version of this which would take
   appropriate recovery measures contingent upon the operation itself. */

#ifdef DEBUG_SUB_STACK_OVERFLOW
#define Print_Overflow_Warning(StackName) {	\
   xsb_warn("%s overflow!\n", StackName);	\
   *(CPtr)0 = 0;				\
 }
#else
#define Print_Overflow_Warning(StackName)	\
   xsb_abort("%s overflow!\n", StackName)
#endif

/*-------------------------------------------------------------------------*/

/*
 *  tstTermStack
 *  ------------
 *  for flattening of the heap term during processing
 */

#define TST_TERMSTACK_SIZE    K

struct tstTermStack {
  CPtr top;           /* next available location to place an entry */
  CPtr ceiling;       /* overflow pointer: points to Cell beyond array end */
  Cell base[TST_TERMSTACK_SIZE];
};

extern struct tstTermStack    tstTermStack;


#define TermStack_ResetTOS          tstTermStack.top = tstTermStack.base
#define TermStack_IsEmpty           (tstTermStack.top == tstTermStack.base)
#define TermStack_Push(Term)        *tstTermStack.top++ = Term
#define TermStack_Pop               *(--tstTermStack.top)

/* Specialty Pushing Macros
   ------------------------ */
#define TermStack_NOOP      /* Nothing to push when constants match */

#define TermStack_PushFunctorArgs(CS_Cell)                   \
   TermStack_PushLowToHighVector( clref_val(CS_Cell) + 1,    \
				  get_arity((Psc)*clref_val(CS_Cell)) )

#define TermStack_PushListArgs(LIST_Cell) {                  \
   CPtr pListHeadCell = clref_val(LIST_Cell);                \
                                                             \
   TermStack_OverflowCheck(2);                               \
   TermStack_Push( *(pListHeadCell + 1) );                   \
   TermStack_Push( *(pListHeadCell) );                       \
 }


#define TermStack_PushLowToHighVector(pVectorLow,Magnitude) {    \
   int i, numElements;                                           \
   CPtr pElement;                                                \
                                                                 \
   numElements = Magnitude;                                      \
   pElement = pVectorLow + numElements;                          \
   TermStack_OverflowCheck(numElements);                         \
   for (i = 0; i < numElements; i++)                             \
     TermStack_Push(*--pElement);                                \
 }
   
#define TermStack_PushHighToLowVector(pVectorHigh,Magnitude) {   \
   int i, numElements;                                           \
   CPtr pElement;                                                \
                                                                 \
   numElements = Magnitude;                                      \
   pElement = pVectorHigh - numElements;                         \
   TermStack_OverflowCheck(numElements);                         \
   for (i = 0; i < numElements; i++)                             \
     TermStack_Push(*++pElement);                                \
 }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Utility Macros
   -------------- */
#define TermStack_Init  \
   tstTermStack.ceiling = tstTermStack.base + TST_TERMSTACK_SIZE

/*
 *  (Since the "top" field points to an unused location, the highest
 *   used location after a push of NumSubterms subterms will be the
 *   current top + (NumSubterms - 1), which we want to be lower than
 *   the off-the-end pointer "ceiling".)
 */
#define TermStack_OverflowCheck(NumSubterms)                     \
   if (tstTermStack.top + NumSubterms > tstTermStack.ceiling)    \
     Print_Overflow_Warning("tstTermStack")

/* ------------------------------------------------------------------------- */

/*
 *  tstTermStackLog
 *  ---------------
 *  For noting the changes made to the tstTermStack during processing
 *  where backtracking is required.  Only the changes necessary to
 *  transform the tstTermStack from its current state to a prior state
 *  are logged.  Therefore, we only need record values popped from the
 *  tstTermStack.
 *
 *  Each frame of the log consists of the address of a tstTermStack
 *  location and the value that was stored there.  tstTermStack values
 *  are reinstated as a side effect of a tstTermStackLog_Pop.
 */

typedef struct {
  CPtr addr;             /* location within the tstTermStack... */
  Cell value;            /* where this value appeared. */
} tstLogFrame;
typedef tstLogFrame *pLogFrame;

#define TST_TERMSTACKLOG_SIZE    K

struct tstTermStackLog {
  pLogFrame top;       /* next available location to place an entry */
  pLogFrame ceiling;   /* overflow pointer: points to Cell beyond array end */
  tstLogFrame base[TST_TERMSTACKLOG_SIZE];
};

extern struct tstTermStackLog    tstTermStackLog;


#define TermStackLog_ResetTOS    tstTermStackLog.top = tstTermStackLog.base
#define TermStackLog_IsFull   (tstTermStackLog.top == tstTermStackLog.ceiling)
#define TermStackLog_Init  \
   tstTermStackLog.ceiling  = tstTermStackLog.base + TST_TERMSTACKLOG_SIZE

#define TermStackLog_PushFrame {                 \
   TermStackLog_OverflowCheck;                   \
   LogFrame_Addr = tstTermStack.top;             \
   LogFrame_Value = *(tstTermStack.top);         \
   tstTermStackLog.top++;                        \
 }

#define TermStackLog_PopAndReset {               \
   tstTermStackLog.top--;                        \
   *LogFrame_Addr = LogFrame_Value;              \
 }

/* Use these to access the frame to which `top' points */
#define LogFrame_Addr     ((tstTermStackLog.top)->addr)
#define LogFrame_Value    ((tstTermStackLog.top)->value)


#define TermStackLog_OverflowCheck     \
   if (TermStackLog_IsFull)            \
     Print_Overflow_Warning("tstTermStackLog")

/* ------------------------------------------------------------------------- */

/*
 *  tstSymbolStack
 *  ---------------
 *  for constructing terms from the symbols stored along a path in the trie
 */

#define TST_SYMBOLSTACK_SIZE   K

struct tstSymbolStack {
  CPtr top;           /* next available location to place an entry */
  CPtr ceiling;       /* overflow pointer: points to Cell beyond array end */
  Cell base[TST_SYMBOLSTACK_SIZE];
};

extern struct tstSymbolStack    tstSymbolStack;


#define SymbolStack_ResetTOS      tstSymbolStack.top = tstSymbolStack.base
#define SymbolStack_IsEmpty       (tstSymbolStack.top == tstSymbolStack.base)
#define SymbolStack_Push(Symbol)  *tstSymbolStack.top++ = Symbol
#define SymbolStack_Pop           *--tstSymbolStack.top
#define SymbolStack_Peek          *(tstSymbolStack.top - 1)

#define SymbolStack_PushPath(pLeaf) { 		\
   TSTNptr pTSTN = (TSTNptr)pLeaf;    		\
                                      		\
   while ( ! IsTrieRoot(pTSTN) ) {    		\
     SymbolStack_Push(TSTN_Symbol(pTSTN));	\
     pTSTN = TSTN_Parent(pTSTN);  		\
   }                              		\
   SymbolStack_OverflowCheck;     		\
 }

#define SymbolStack_Init  \
   tstSymbolStack.ceiling = tstSymbolStack.base + TST_SYMBOLSTACK_SIZE

#define SymbolStack_OverflowCheck                          \
   if (tstSymbolStack.top > tstSymbolStack.ceiling)        \
     Print_Overflow_Warning("tstSymbolStack")

/* ------------------------------------------------------------------------- */

/*
 *  tstTrail
 *  ---------
 *  For recording bindings made during processing.  This Trail
 *  performs simple WAM trailing: saves address locations only.
 */

#define TST_TRAIL_SIZE    K

struct tstTrail {
  CPtr *top;           /* next available location to place an entry */
  CPtr *ceiling;       /* overflow pointer: points to Cell beyond array end */
  CPtr base[TST_TRAIL_SIZE];
};

extern struct tstTrail    tstTrail;


#define Trail_Init         tstTrail.ceiling = tstTrail.base + TST_TRAIL_SIZE
#define Trail_ResetTOS     tstTrail.top = tstTrail.base
#define Trail_IsFull       tstTrail.top == tstTrail.ceiling

#define Trail_Push(Addr) {              \
   Trail_OverflowCheck;                 \
   *tstTrail.top++ = (CPtr)(Addr);      \
 }

#define Trail_Unwind_All   Trail_Unwind(tstTrail.base)

#define Trail_Unwind(UnwindBase)     		\
   while(tstTrail.top > UnwindBase) {		\
     tstTrail.top--;                 		\
     bld_free(*tstTrail.top);        		\
   }

#define Trail_OverflowCheck        \
   if (Trail_IsFull)               \
     Print_Overflow_Warning("tstTrail")

/*=========================================================================*/


#endif
