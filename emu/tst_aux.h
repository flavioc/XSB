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
#include "debugs/debug_tries.h"
#include "dynamic_stack.h"


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
 *  iii) Relevant Answer Identification
 *  iv) Answer Consumption
 */

/*---------------------------------------------------------------------------*/

/*
 *  tstTermStack
 *  ------------
 *  For flattening a heap term during processing.
 */

extern DynamicStack tstTermStack;
#define TST_TERMSTACK_INITSIZE    25

#define TermStack_Top		((CPtr)DynStk_Top(tstTermStack))
#define TermStack_Base		((CPtr)DynStk_Base(tstTermStack))
#define TermStack_NumTerms	DynStk_NumFrames(tstTermStack)
#define TermStack_ResetTOS	DynStk_ResetTOS(tstTermStack)
#define TermStack_IsEmpty	DynStk_IsEmpty(tstTermStack)

#define TermStack_SetTOS(Index)			\
   DynStk_Top(tstTermStack) = TermStack_Base + Index

#define TermStack_Push(Term) {			\
   CPtr newFrame;				\
						\
   DynStk_Push(tstTermStack,newFrame);		\
   *newFrame = Term;				\
 }

#define TermStack_BlindPush(Term) {		\
   CPtr newFrame;				\
						\
   DynStk_BlindPush(tstTermStack,newFrame);	\
   *newFrame = Term;				\
 }

#define TermStack_Pop(Term) {			\
   CPtr newFrame;				\
						\
   DynStk_BlindPop(tstTermStack,newFrame);	\
   Term = *newFrame;				\
 }

#define TermStack_Peek(Term) {			\
   CPtr newFrame;				\
						\
   DynStk_BlindPeek(tstTermStack,newFrame);	\
   Term = *newFrame;				\
 }

/* Specialty Pushing Macros
   ------------------------ */
#define TermStack_NOOP      /* Nothing to push when constants match */

#define TermStack_PushFunctorArgs(CS_Cell)                   \
   TermStack_PushLowToHighVector( clref_val(CS_Cell) + 1,    \
				  get_arity((Psc)*clref_val(CS_Cell)) )

#define TermStack_PushListArgs(LIST_Cell) {	\
   CPtr pListHeadCell = clref_val(LIST_Cell);	\
						\
   DynStk_ExpandIfOverflow(tstTermStack,2);	\
   TermStack_BlindPush( *(pListHeadCell + 1) );	\
   TermStack_BlindPush( *(pListHeadCell) );	\
 }

/*
 * The following macros enable the movement of an argument vector to the
 * TermStack.  Two versions are supplied depending on whether the vector
 * is arranged from high-to-low memory or from low-to-high.  The vector
 * pointer is assumed to reference the first element of the vector.
 */

#define TermStack_PushLowToHighVector(pVectorLow,Magnitude) {	\
   int i, numElements;						\
   CPtr pElement;						\
								\
   numElements = Magnitude;					\
   pElement = pVectorLow + numElements;				\
   DynStk_ExpandIfOverflow(tstTermStack,numElements);		\
   for (i = 0; i < numElements; i++)				\
     TermStack_BlindPush(*--pElement);				\
 }
   
#define TermStack_PushHighToLowVector(pVectorHigh,Magnitude) {	\
   int i, numElements;						\
   CPtr pElement;						\
								\
   numElements = Magnitude;					\
   pElement = pVectorHigh - numElements;			\
   DynStk_ExpandIfOverflow(tstTermStack,numElements);		\
   for (i = 0; i < numElements; i++)				\
     TermStack_BlindPush(*++pElement);				\
 }

/*
 * This macro copies an array of terms onto the TermStack, checking for
 * overflow only once at the beginning, rather than with each push.  The
 * elements to be pushed are assumed to exist in array elements
 * 0..(NumElements-1).
 */

#define TermStack_PushArray(Array,NumElements) {	\
   counter i;						\
							\
   DynStk_ExpandIfOverflow(tstTermStack,NumElements);	\
   for ( i = 0;  i < NumElements;  i++ )		\
     TermStack_BlindPush(Array[i]);			\
 }

/* ------------------------------------------------------------------------- */

#ifdef DEBUG_TRIE_STACK
#define Print_Overflow_Warning(StackName) {	\
   xsb_warn("%s overflow!\n", StackName);	\
   *(CPtr)0 = 0;				\
 }
#else
#define Print_Overflow_Warning(StackName)	\
   xsb_abort("%s overflow!\n", StackName)
#endif

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
  int index;             /* location within the tstTermStack... */
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

#define TermStackLog_PushFrame {			\
   TermStackLog_OverflowCheck;				\
   LogFrame_Index = TermStack_Top - TermStack_Base;	\
   LogFrame_Value = *(TermStack_Top);			\
   tstTermStackLog.top++;				\
 }

#define TermStackLog_PopAndReset {			\
   tstTermStackLog.top--;				\
   TermStack_Base[LogFrame_Index] = LogFrame_Value;	\
 }

/* Use these to access the frame to which `top' points */
#define LogFrame_Index		((tstTermStackLog.top)->index)
#define LogFrame_Value		((tstTermStackLog.top)->value)


#define TermStackLog_OverflowCheck     \
   if (TermStackLog_IsFull)            \
     Print_Overflow_Warning("tstTermStackLog")

/* ------------------------------------------------------------------------- */

/*
 *  tstSymbolStack
 *  ---------------
 *  for constructing terms from the symbols stored along a path in the trie
 */

extern DynamicStack tstSymbolStack;
#define TST_SYMBOLSTACK_INITSIZE   25

#define SymbolStack_Top		  ((CPtr)DynStk_Top(tstSymbolStack))
#define SymbolStack_Base	  ((CPtr)DynStk_Base(tstSymbolStack))
#define SymbolStack_NumSymbols	  (SymbolStack_Top - SymbolStack_Base)
#define SymbolStack_ResetTOS	  DynStk_ResetTOS(tstSymbolStack)
#define SymbolStack_IsEmpty	  DynStk_IsEmpty(tstSymbolStack)

#define SymbolStack_Push(Symbol) {		\
   CPtr newFrame;				\
						\
   DynStk_Push(tstSymbolStack,newFrame);	\
   *newFrame = Symbol;				\
 }

#define SymbolStack_Pop(Symbol) {		\
   CPtr newFrame;				\
						\
   DynStk_BlindPop(tstSymbolStack,newFrame);	\
   Symbol = *newFrame;				\
}

#define SymbolStack_Peek(Symbol) {		\
   CPtr newFrame;				\
						\
   DynStk_BlindPeek(tstSymbolStack,newFrame);	\
   Symbol = *newFrame;				\
}

#define SymbolStack_PushPathRoot(Leaf,Root) {	\
   BTNptr btn = (BTNptr)Leaf;			\
						\
   while ( ! IsTrieRoot(btn) ) {		\
     SymbolStack_Push(BTN_Symbol(btn));		\
     btn = BTN_Parent(btn);			\
   }						\
   Root = (void *)btn;				\
 }

#define SymbolStack_PushPath(Leaf) {  		\
   BTNptr root;					\
   SymbolStack_PushPathRoot(Leaf,root);		\
 }

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
#define Trail_NumBindings  ( tstTrail.top - tstTrail.base )

#define Trail_Push(Addr) {              \
   Trail_OverflowCheck;                 \
   *tstTrail.top++ = (CPtr)(Addr);      \
 }

#define Trail_PopAndReset {		\
   tstTrail.top--;			\
   bld_free(*tstTrail.top);		\
 }

#define Trail_Unwind_All   Trail_Unwind(tstTrail.base)

#define Trail_Unwind(UnwindBase)	\
   while(tstTrail.top > UnwindBase)	\
     Trail_PopAndReset

#define Trail_OverflowCheck        \
   if (Trail_IsFull)               \
     Print_Overflow_Warning("tstTrail")

/*=========================================================================*/

/*
 *			Using the Trie Stacks
 *			=====================
 */

#define ProcessNextSubtermFromTrieStacks(Symbol,StdVarNum) {	\
								\
   Cell subterm;						\
								\
   TermStack_Pop(subterm);					\
   XSB_Deref(subterm);						\
   switch ( cell_tag(subterm) ) {				\
   case XSB_REF:						\
   case XSB_REF1:						\
     if ( ! IsStandardizedVariable(subterm) ) {			\
       StandardizeVariable(subterm, StdVarNum);			\
       Trail_Push(subterm);					\
       Symbol = EncodeNewTrieVar(StdVarNum);			\
       StdVarNum++;						\
     }								\
     else							\
       Symbol = EncodeTrieVar(IndexOfStdVar(subterm));		\
     break;							\
   case XSB_STRING:						\
   case XSB_INT:						\
   case XSB_FLOAT:						\
     Symbol = EncodeTrieConstant(subterm);			\
     break;							\
   case XSB_STRUCT:						\
     Symbol = EncodeTrieFunctor(subterm);			\
     TermStack_PushFunctorArgs(subterm);			\
     break;							\
   case XSB_LIST:						\
     Symbol = EncodeTrieList(subterm);				\
     TermStack_PushListArgs(subterm);				\
     break;							\
   default:							\
     Symbol = 0;  /* avoid "uninitialized" compiler warning */	\
     TrieError_UnknownSubtermTag(subterm);			\
   }								\
 }

/*=========================================================================*/


#endif
