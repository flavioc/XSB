/* File:      tst_utils.c
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
#include <stdlib.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "binding.h"
#include "psc_xsb.h"
#include "register.h"
#include "deref.h"
#include "trie_internals.h"
#include "tst_aux.h"
#include "macro_xsb.h"
#include "choice.h"
#include "inst_xsb.h"
#include "error_xsb.h"


/* ======================================================================= */

extern void printterm(Cell, byte, int);   /* prints to stddbg */

#ifdef BITS64
#define IntegerFormatString	"%ld"
#else
#define IntegerFormatString	"%d"
#endif

/* ====================================================================== */

/*
 *            Stacks needed by TST-Subsumptive Operations
 *            ===========================================
 */


DynamicStack  tstTermStack;
struct tstTermStackLog  tstTermStackLog;
DynamicStack  tstSymbolStack;
struct tstTrail  tstTrail;


void tstInitDataStructs() {

  extern void initTSTRetrieve(void);
  extern void initSubsumptiveLookup(void);

  DynStk_Init(&tstTermStack, TST_TERMSTACK_INITSIZE, Cell, "TST Term Stack");
  TermStackLog_Init;
  DynStk_Init(&tstSymbolStack, TST_SYMBOLSTACK_INITSIZE, Cell,
	      "Trie-Symbol Stack");
  Trail_Init;
  initSubsumptiveLookup();
  initTSTRetrieve();
}


void tstShrinkDynStacks() {

  dsShrink(&tstTermStack);
  dsShrink(&tstSymbolStack);
}

/* ======================================================================= */

/*
 *		    D E B U G G I N G   R O U T I N E S
 *	            ===================================
 */


/*-------------------------------------------------------------------------*/

/*
 *			  Printing Trie Nodes
 *			  -------------------
 */

char *stringNodeType(byte fieldNodeType) {

  switch (fieldNodeType) {
  case TRIE_ROOT_NT:
    return("TRIE_ROOT_NT");
  case HASH_HEADER_NT:
    return("HASH_HEADER_NT");
  case LEAF_NT:
    return("LEAF_NT");
  case HASHED_LEAF_NT:
    return("HASHED_LEAF_NT");
  case INTERRIOR_NT:
    return("INTERRIOR_NT");
  case HASHED_INTERRIOR_NT:
    return("HASHED_INTERRIOR_NT");
  default:
    {
      char t[20], *s;
      sprintf(t, "unknown (%c)", fieldNodeType);
      if ( IsNULL(s = (char *)malloc(strlen(t)+1)) )
	return("unknown");
      else {
	strcpy(s,t);
	return(s);
      }
    }
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *stringTrieType(byte fieldTrieType) {

  switch (fieldTrieType) {
  case CALL_TRIE_TT:
    return("CALL_TRIE_TT");
  case BASIC_ANSWER_TRIE_TT:
    return("BASIC_ANSWER_TRIE_TT");
  case TS_ANSWER_TRIE_TT:
    return("TS_ANSWER_TRIE_TT");
  case DELAY_TRIE_TT:
    return("DELAY_TRIE_TT");
  case ASSERT_TRIE_TT:
    return("ASSERT_TRIE_TT");
  case INTERN_TRIE_TT:
    return("INTERN_TRIE_TT");
  default:
    {
      char t[20], *s;
      sprintf(t, "unknown (%c)", fieldTrieType);
      if ( IsNULL(s = (char *)malloc(strlen(t)+1)) )
	return("unknown");
      else {
	strcpy(s,t);
	return(s);
      }
    }
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void printTrieSymbol(FILE *fp, Cell symbol) {

  if ( symbol == ESCAPE_NODE_SYMBOL )
    fprintf(fp, "%lu [ESCAPE_NODE_SYMBOL]", ESCAPE_NODE_SYMBOL);
  else {
    switch(TrieSymbolType(symbol)) {
    case XSB_INT:
      fprintf(fp, IntegerFormatString, int_val(symbol));
      break;
    case XSB_FLOAT:
      fprintf(fp, "%f", float_val(symbol));
      break;
    case XSB_STRING:
      fprintf(fp, "%s", string_val(symbol));
      break;
    case XSB_TrieVar:
      fprintf(fp, "V" IntegerFormatString, DecodeTrieVar(symbol));
      break;
    case XSB_STRUCT:
      {
	Psc psc = DecodeTrieFunctor(symbol);
	fprintf(fp, "%s/%d", get_name(psc), get_arity(psc));
      }
      break;
    case XSB_LIST:
      fprintf(fp, "LIST");
      break;
    default:
      fprintf(fp, "Unknown symbol (tag = %ld)", cell_tag(symbol));
      break;
    }
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void printTrieNode(FILE *fp, BTNptr pTN) {

  fprintf(fp, "Trie Node: Addr(%p)", pTN);
  if ( IsDeletedNode(pTN) )
    fprintf(fp, "  (DELETED)");
  fprintf(fp, "\n\tInstr(%s), NodeType(%s)\n\tTrieType(%s), Symbol(",
	  inst_name(TN_Instr(pTN)),
	  stringNodeType(TN_NodeType(pTN)),
	  stringTrieType(TN_TrieType(pTN)));
  printTrieSymbol(fp, TN_Symbol(pTN));
  fprintf(fp, ")");
  if ( IsInTimeStampedTrie(pTN) )
    fprintf(fp, ", TimeStamp(%ld)", TSTN_TimeStamp((TSTNptr)pTN));
  fprintf(fp, "\n\tParent(%p), Child(%p), Sibling(%p)\n",
	 TN_Parent(pTN), TN_Child(pTN), TN_Sibling(pTN));
}

/*-----------------------------------------------------------------------*/

/*
 *		Trie-Path Printing Using the SymbolStack
 *		----------------------------------------
 */

static void symstkPrintNextTerm(FILE *fp) {

  Cell symbol;

  SymbolStack_Pop(symbol);
  switch ( TrieSymbolType(symbol) ) {
  case XSB_INT:
    fprintf(fp, IntegerFormatString, int_val(symbol));
    break;
  case XSB_FLOAT:
    fprintf(fp, "%f", float_val(symbol));
    break;
  case XSB_STRING:
    fprintf(fp, "%s", string_val(symbol));
    break;
  case XSB_TrieVar:
    fprintf(fp, "V" IntegerFormatString, DecodeTrieVar(symbol));
    break;
  case XSB_STRUCT:
    {
      Psc psc;
      int i;

      psc = DecodeTrieFunctor(symbol);
      fprintf(fp, "%s(", get_name(psc));
      for (i = 1; i < (int)get_arity(psc); i++) {
	symstkPrintNextTerm(fp);
	fprintf(fp, ",");
      }
      symstkPrintNextTerm(fp);
      fprintf(fp, ")");
    }
    break;
  case XSB_LIST:
    {
      fprintf(fp, "[");
      symstkPrintNextTerm(fp);
      SymbolStack_Peek(symbol);
      while (symbol == XSB_LIST) {
	/*
	 * Remove the symbol, XSB_LIST, we just peeked at from the
	 * SymbolStack, placing the next term on top of the stack.
	 * Recall that this symbol is not a term but merely indicates
	 * that another recursive component of a list follows.
	 */
	SymbolStack_Pop(symbol);
	symstkPrintNextTerm(fp);
	fprintf(fp, ",");
	SymbolStack_Peek(symbol);
      }
      /*
       *  This symbol ends the list and so should either be the nil
       *  ("[]") string or a variable
       */
      if ( symbol == makestring(nil_sym) )
	SymbolStack_Pop(symbol)
      else {
	fprintf(fp, "|");
	symstkPrintNextTerm(fp);
      }
      fprintf(fp, "]");
    }
    break;
  default:
    fprintf(fp, "Unknown symbol");
    break;
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void printTriePath(FILE *fp, BTNptr pLeaf, xsbBool printLeafAddr) {

  BTNptr pRoot;
  Psc pscPred;

  if (IsNULL(pLeaf)) {
    fprintf(fp, "NULL");
    return;
  }

  if ( printLeafAddr )
    fprintf(fp, "Leaf %p:", pLeaf);

  if ( ! IsLeafNode(pLeaf) ) {
    fprintf(fp, "printTriePath() called with non-Leaf Node!");
    return;
  }

  if (IsEscapeNode(pLeaf)) {
    fprintf(fp, "ESCAPE node");
  }
  else {
    SymbolStack_ResetTOS;
    SymbolStack_PushPathRoot(pLeaf,pRoot);
    pscPred = DecodeTrieFunctor(TN_Symbol(pRoot));
    if ( IsNonNULL(pscPred) )
      fprintf(fp, "%s", get_name(pscPred));
    fprintf(fp, "(");
    symstkPrintNextTerm(fp);
    while (! SymbolStack_IsEmpty) {
      fprintf(fp, ",");
      symstkPrintNextTerm(fp);
    }
    fprintf(fp, ")");
  }
}

/*-----------------------------------------------------------------------*/

/*
 *		       Printing the Answer Template
 *		       ----------------------------
 *
 *  The answer template is assumed to be stored in high-to-low memory
 *  vector fashion, with the incoming template pointer pointing to the
 *  first term Cell (highest memory Cell of the vector).
 */

void printAnswerTemplate(CPtr pAnsTmplt, int size) {

  int i;

  fprintf(stddbg, "Answer Template:  (");
  if (size > 0) {
    for (i = 1; i < size; i++) {
      printterm(*pAnsTmplt--,1,10);
      fprintf(stddbg, ",");
    }
    printterm(*pAnsTmplt,1,10);
  }
  fprintf(stddbg, ")\n");
}

/*-------------------------------------------------------------------------*/

/*
 *		Printing Componenets of a Subgoal Frame
 *		---------------------------------------
 *
 * Functions for subgoal frame printing can be found in debug_xsb.c.
 */


/* Printing a SF's associated Call
   ------------------------------- */
void sfPrintGoal(FILE *fp, VariantSF pSF, xsbBool printAddr) {

  Psc pPSC;

  pPSC = TIF_PSC(subg_tif_ptr(pSF));
  if ( printAddr )
    fprintf(fp, "SF %p  ", pSF);
  printTriePath(fp, subg_leaf_ptr(pSF), NO);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Printing Subsumed Calls
   ----------------------- */
void sfPrintConsGoals(FILE *fp, SubProdSF pProd) {

  SubConsSF pCons;

  fprintf(fp, "Producer:\n  ");
  sfPrintGoal(fp, (VariantSF)pProd, YES);
  fprintf(fp, "\nConsumers:\n");
  for ( pCons = subg_consumers(pProd);  IsNonNULL(pCons);
        pCons = conssf_consumers(pCons) ) {
    fprintf(fp, "  ");
    sfPrintGoal(fp, (VariantSF)pCons, YES);
    fprintf(fp, "\n");
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Printing Answers in an Answer List
   ---------------------------------- */
void printAnswerList(FILE *fp, ALNptr pALN) {

  fprintf(fp, "Answer List %p:\n", pALN);
  while ( IsNonNULL(pALN) ) {
    fprintf(fp, "  ");
    printTriePath(fp, ALN_Answer(pALN), YES);
    fprintf(fp, "\n");
    pALN = ALN_Next(pALN);
  }
}
/*-------------------------------------------------------------------------*/

/*
 *			    Miscellaneous
 *			    -------------
 */

void printTabledCall(TabledCallInfo callInfo) {

  int arity, i;
  Psc pPSC;
  
  pPSC = TIF_PSC(CallInfo_TableInfo(callInfo));
  fprintf(stddbg, "%s(", get_name(pPSC));
  arity = CallInfo_CallArity(callInfo);
  for (i = 0; i < arity; i++) {
    printterm( (Cell)(CallInfo_Arguments(callInfo)+i), 1, 8 );
    if (i+1 < arity)
      fprintf(stddbg, ",");
  }
  fprintf(stddbg, ")");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void printTriePathType(FILE *fp, TriePathType type, BTNptr leaf) {

  switch (type) {
  case NO_PATH:
    fprintf(fp,"No path found :-(\n");
    break;
  case VARIANT_PATH:
    fprintf(fp,"Variant path found: ");
    printTriePath(fp,leaf,FALSE);
    break;
  case SUBSUMPTIVE_PATH:
    fprintf(fp,"Subsumptive path found: ");
    printTriePath(fp,leaf,FALSE);
    break;
  default:
    fprintf(fp,"What kind of path? (%d)\n", type);
    break;
  }
}
