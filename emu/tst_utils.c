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

extern void printterm(Cell, byte, int);

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


struct tstTermStack      tstTermStack;
struct tstTermStackLog   tstTermStackLog;
struct tstSymbolStack    tstSymbolStack;
struct tstTrail          tstTrail;


void tstInitDataStructs() {

  extern void initSubCallChkIns();
  extern void initTSTRetrieve();

  TermStack_Init;
  TermStackLog_Init;
  SymbolStack_Init;
  Trail_Init;
  initSubCallChkIns();   /* init add'l data structs used in sub_insert.c */
  initTSTRetrieve();     /* init add'l data structs used in tst_retrv.c */
}

/* ======================================================================= */

/*
 *		    D E B U G G I N G   R O U T I N E S
 *	            ===================================
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


void printTabledCall(TabledCallInfo callInfo) {

  int arity, i;
  Psc pPSC;
  
  pPSC = TIF_PSC(CallInfo_TableInfo(callInfo));
  fprintf(stddbg, "%s(", get_name(pPSC));
  arity = CallInfo_CallArity(callInfo);
  for (i = 1; i <= arity; i++) {
    printterm( (Cell)(CallInfo_Arguments(callInfo)+i), 1, 8 );
    if (i < arity)
      fprintf(stddbg, ",");
  }
  fprintf(stddbg, ")");
}


CPtr decode_ptr(Cell cell) {
  return ( clref_val(cell) );
}


int decode_int(Cell cell) {
  return ( int_val(cell) );
}


int decode_tag(Cell cell) {
  return ( cell_tag(cell) );
}

/*-----------------------------------------------------------------------*/

/*
 *		Trie-Path Printing Using the SymbolStack
 *		----------------------------------------
 */

static void symstkPrintNextTerm() {

  Cell symbol = SymbolStack_Pop;

  switch(TrieSymbolType(symbol)) {
  case XSB_INT:
    printf(IntegerFormatString, int_val(symbol));
    break;
  case XSB_FLOAT:
    printf("%f", float_val(symbol));
    break;
  case XSB_STRING:
    printf("%s", string_val(symbol));
    break;
  case XSB_TrieVar:
    printf("V" IntegerFormatString, DecodeTrieVar(symbol));
    break;
  case XSB_STRUCT:
    {
      Psc psc;
      int i;

      psc = DecodeTrieFunctor(symbol);
      printf("%s(", get_name(psc));
      for (i = 1; i < (int)get_arity(psc); i++) {
	symstkPrintNextTerm();
	printf(",");
      }
      symstkPrintNextTerm();
      printf(")");
    }
    break;
  case XSB_LIST:
    {
      printf("[");
      symstkPrintNextTerm();
      symbol = SymbolStack_Peek;
      while (symbol == XSB_LIST) {
	/*
	 * Remove the symbol we just peeked at (using assignment eliminates
	 * a compiler warning) placing the next term on top of the stack.
	 */
	symbol = SymbolStack_Pop;
	symstkPrintNextTerm();
	printf(",");
	symbol = SymbolStack_Peek;
      }
      /*
       *  This symbol ends the list and so should either be the nil
       *  ("[]") string or a variable
       */
      if ( symbol == makestring(nil_sym) )
	symbol = SymbolStack_Pop;
      else {
	printf("|");
	symstkPrintNextTerm();
      }
      printf("]");
    }
    break;
  default:
    printf("Unknown symbol");
    break;
  }
}


void triePrintPath(BTNptr pLeaf, xsbBool printLeafAddr) {

  Psc pscPred;

  if (IsNULL(pLeaf)) {
    printf("NULL\n");
    return;
  }

  if ( printLeafAddr )
    printf("Leaf %p:", pLeaf);

  if ( ! IsLeafNode(pLeaf) ) {
    fprintf(stderr, "triePrintPath() called with non-Leaf Node!\n");
    return;
  }

  if (IsEscapeNode(pLeaf)) {
    printf("ESCAPE node\n");
  }
  else {
    SymbolStack_ResetTOS;
    while( IsNonNULL(pLeaf) && (! IsTrieRoot(pLeaf)) ) {
      SymbolStack_Push(TN_Symbol(pLeaf));
      pLeaf = TN_Parent(pLeaf);
    }
    SymbolStack_OverflowCheck;
    pscPred = DecodeTrieFunctor(TN_Symbol(pLeaf));
    if ( IsNonNULL(pscPred) )
      printf("%s",get_name(pscPred));
    printf("(");
    while (! SymbolStack_IsEmpty) {
      symstkPrintNextTerm();
      if (! SymbolStack_IsEmpty)
	printf(",");
    }
    printf(")\n");
  }
}

/*-----------------------------------------------------------------------*/

/* Printing the Answer Template
 * ----------------------------
 *  The answer template is assumed to be stored in high-to-low memory
 *  vector fashion, with the incoming template pointer pointing to the
 *  first term Cell (highest memory Cell of the vector).
 */

void printAnswerTemplate(CPtr pAnsTmplt, int size) {

  int i;

  printf("Answer Template:  (");
  if (size > 0) {
    for (i = 1; i < size; i++) {
      printterm(*pAnsTmplt--,1,10);
      printf(",");
    }
    printterm(*pAnsTmplt,1,10);
  }
  printf(")\n");
}


/* Printing a SF's associated Call
   ------------------------------- */
void sfPrintGoal(VariantSF pSF, xsbBool printAddr) {

  Psc pPSC;

  pPSC = TIF_PSC(subg_tif_ptr(pSF));
  if ( printAddr )
    printf("SF %p  ", pSF);
  printf("%s", get_name(pPSC));
  triePrintPath(subg_leaf_ptr(pSF),NO);
}


/* Printing Subsumed Calls
   ----------------------- 
void sfPrintConsGoals(VariantSF pProd) {

  VariantSF pCons;

  printf("Producer:\n  ");
  sfPrintGoal(pProd,YES);
  printf("\nConsumers:\n");
  for ( pCons = subg_Consumers(pProd);  IsNonNULL(pCons);
        pCons = subg_Consumers(pCons) ) {
    printf("  ");
    sfPrintGoal(pCons,YES);
    printf("\n");
  }
}
*/

/* Printing Answers in an Ans List
   ------------------------------- */
void printAnswerList(ALNptr pALN) {

  printf("Answer List %p:\n", pALN);
  while ( IsNonNULL(pALN) ) {
    printf("  ");
    triePrintPath(ALN_Answer(pALN),YES);
    printf("\n");
    pALN = ALN_Next(pALN);
  }
}
