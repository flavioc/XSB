/* File:      trie_internals.h
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


#ifndef INTERNAL_TRIE_DEFS


#define INTERNAL_TRIE_DEFS


/*
 * Internal specifications of tries that the hard-core trie routines
 * require.  However, these specs need not be visible to engine
 * components which simply use tries through normal channels.
 */

#include "inst.h"
#include "struct_manager.h"
#include "tries.h"


/*===========================================================================*/

/*
 *	  G E N E R I C   C O M P O N E N T - O P E R A T I O N S
 *	  =======================================================
 *
 *  See the docs at the top of tries.h.
 *
 *  We use the following notation:
 *    TN_*         Trie Node operation
 *    TrieHT_*     Trie Hash Table operation
 *    TSC_*        Trie SubComponent (TN or THT) operation
 *    pTN          pointer to a Trie Node
 *    pTHT         pointer to a Trie Hash Table
 *    pTSC         pointer to a Trie SubComponent
 */

#define TrieHT_Instr(pTHT)	   TSC_Instr(pTHT)
#define TrieHT_Status(pTHT)	   TSC_Status(pTHT)
#define TrieHT_TrieType(pTHT)	   TSC_TrieType(pTHT)
#define TrieHT_NodeType(pTHT)      TSC_NodeType(pTHT)
#define TrieHT_NumBuckets(pTHT)    ( (pTHT)->numBuckets )
#define TrieHT_NumContents(pTHT)   ( (pTHT)->numContents )
#define TrieHT_BucketArray(pTHT)   ( (pTHT)->pBucketArray )
#define TrieHT_PrevHT(pTHT)	   ( (pTHT)->prev )
#define TrieHT_NextHT(pTHT)	   ( (pTHT)->next )
#define TrieHT_GetHashSeed(pTHT)   ( TrieHT_NumBuckets(pTHT) - 1 )

/*---------------------------------------------------------------------------*/

/*
 *                   Instruction Opcode Maintenance
 *                   ==============================
 *
 *  Trie-embedded instructions are classified according to the type of
 *  choice point (CP) operation they perform and the type of symbol being
 *  inspected.  For each trie node, the choice point component of the
 *  node's instruction is directly dependent upon the node's position in a
 *  sibling chain: the first node is a "try" type instruction, the last is
 *  a "trust" type, and any ones in between get "retry" type instructions.
 *  If the node is the ONLY one in a chain, then no choice point need be
 *  established, and hence it's category is termed "no_cp".
 *
 *  The assignment of an instruction to a trie node occurs during node
 *  allocation when both the symbol it will contain and its initial
 *  position in a sibling chain are known: by default, new nodes are
 *  placed at the head of chains.  However, this means that the node which
 *  previously headed the chain must have its instruction's CP component
 *  altered.  Additionally, when moving a chain of nodes into a hash
 *  table, or expanding a hash table, requires altering the CP component.
 *  Fortunately, the representation of embedded-trie instructions allows
 *  such maintenance of their CP component.
 */


#define TN_SetInstr(pTN,Symbol)					\
   switch( TrieSymbolType(Symbol) ) {				\
   case CS:							\
     TN_Instr(pTN) = (byte)trie_try_str;			\
     break;							\
   case INT:							\
   case STRING:							\
   case FLOAT:							\
     TN_Instr(pTN) = (byte)trie_try_numcon;			\
     break;							\
   case TrieVar:						\
     if (IsNewTrieVar(Symbol))					\
       TN_Instr(pTN) = (byte)trie_try_var;			\
     else							\
       TN_Instr(pTN) = (byte)trie_try_val;			\
     break;							\
   case LIST:							\
     TN_Instr(pTN) = (byte)trie_try_list;			\
     break;							\
   default:							\
     xsb_abort("Trie Node creation: Bad tag in symbol %lx",	\
               Symbol);						\
   }


#define TN_ResetInstrCPs(pHead,pSibling) {		\
   if ( IsNonNULL(pSibling) )				\
     TN_RotateInstrCPtoRETRYorTRUST(pSibling);		\
   else							\
     TN_Instr(pHead) -= 0x2;	/* TRY -> NO_CP */	\
 }


/*
 *  Applied to the current head of a node-chain when adding a new node at
 *  the head of this chain.  A "try" type instruction becomes a "retry"
 *  and a "no_cp" type becomes a "trust".
 */

#define TN_RotateInstrCPtoRETRYorTRUST(pTN)   TN_Instr(pTN) += 0x1


/*
 *  An optimization which includes a "leafness" tag in the instruction
 *  for nodes -- containing either INT, FLOAT, or STRING data -- which
 *  appear as leaves of a trie.
 */

#define TN_UpgradeInstrTypeToSUCCESS(pTN,SymbolTag)			\
   if (SymbolTag == STRING || SymbolTag == INT || SymbolTag == FLOAT)	\
     TN_Instr(pTN) += 0x4


/*
 *  When converting a long chain of nodes to a hash structure, the current
 *  order of the nodes within the chain is not preserved.  Hence the
 *  order-dependent component of the instructions must be modified: the
 *  choice point type.  These macros coerce a node's instruction's CP
 *  component to one of two types -- the two which may appear at the head
 *  of a chain, where nodes are inserted -- while maintaining the same
 *  symbol type and success status.
 */

#define TN_ForceInstrCPtoNOCP(pTN)		\
   TN_Instr(pTN) = TN_Instr(pTN) & ~0x3

#define TN_ForceInstrCPtoTRY(pTN)		\
   TN_Instr(pTN) = (TN_Instr(pTN) & ~0x3) | 0x2


/*
 *  For testing the CP-type of an embedded trie instruction.
 */

#define Contains_NOCP_Instr(pTN)	( (TN_Instr(pTN) & 0x3) == 0 )
#define Contains_TRUST_Instr(pTN) 	( (TN_Instr(pTN) & 0x3) == 1 )
#define Contains_TRY_Instr(pTN)   	( (TN_Instr(pTN) & 0x3) == 2 )
#define Contains_RETRY_Instr(pTN) 	( (TN_Instr(pTN) & 0x3) == 3 )

#define TN_InstrCPType(pTN)		( TN_Instr(pTN) & 0x3 )

/*---------------------------------------------------------------------------*/

/*
 *			    Status Definitions
 *			    ==================
 */

#define VALID_NODE_STATUS	( (byte) 0 )

#define IsValidNode(pTSC)	( TSC_Status(pTSC) == VALID_NODE_STATUS )
#define IsDeletedNode(pTSC)	( TSC_Status(pTSC) != VALID_NODE_STATUS )

#define MakeStatusValid(pTSC)	  TSC_Status(pTSC) = VALID_NODE_STATUS

/* The following definition depends upon the instruction field having
   already been set to a valid trie instruction code. */

#define MakeStatusDeleted(pTSC)	  TSC_Status(pTSC) = TSC_Instr(pTSC)

/*---------------------------------------------------------------------------*/

/*
 *			Trie-Type Definitions
 *			=====================
 *
 *  Should denote both:
 *  1) The underlying structure of the trie: Basic or Time-Stamped
 *  2) The role the trie is playing in the system
 */

enum Types_of_Tries {
  CALL_TRIE_TT, BASIC_ANSWER_TRIE_TT, TS_ANSWER_TRIE_TT,
  DELAY_TRIE_TT, ASSERT_TRIE_TT, INTERN_TRIE_TT
};

#define IsInCallTrie(pTSC)	   ( TSC_TrieType(pTSC) == CALL_TRIE_TT )
#define IsInAnswerTrie(pTSC)				\
   ( TSC_TrieType(pTSC) == BASIC_ANSWER_TRIE_TT  ||	\
     TSC_TrieType(pTSC) == TS_ANSWER_TRIE_TT )
#define IsInDelayTrie(pTSC)	   ( TSC_TrieType(pTSC) == DELAY_TRIE_TT )
#define IsInAssertTrie(pTSC)	   ( TSC_TrieType(pTSC) == ASSERT_TRIE_TT )
#define IsInInternTrie(pTSC)	   ( TSC_TrieType(pTSC) == INTERN_TRIE_TT )

#define IsInTimeStampedTrie(pTSC)  ( TSC_TrieType(pTSC) == TS_ANSWER_TRIE_TT )
#define IsInBasicTrie(pTSC)        ( ! IsInTimeStampedTrie(pTSC) )

/*---------------------------------------------------------------------------*/

/*
 *			Node-Type Definitions
 *			=====================
 *
 *  Four bits are used to describe the following attributes of a trie
 *  subcomponent:
 *    3rd bit:  TrieRoot / Non-TrieRoot
 *    2nd bit:  Hash Header / Trie Node
 *    1st bit:  Leaf / Interrior
 *    0th bit:  Hashed / Non-Hashed
 *  There are 6 basic types of TSCs that we wish to discriminate.
 */

enum Types_of_Trie_Nodes {
  TRIE_ROOT_NT		= 0x08,   /* binary:  1000 */
  HASH_HEADER_NT	= 0x04,   /* binary:  0100 */
  LEAF_NT		= 0x02,   /* binary:  0010 */
  HASHED_LEAF_NT	= 0x03,   /* binary:  0011 */
  INTERRIOR_NT		= 0x00,   /* binary:  0000 */
  HASHED_INTERRIOR_NT	= 0x01    /* binary:  0001 */
};

#define  HASHED_NODE_MASK	0x01
#define  LEAF_NODE_MASK		0x02


#define IsTrieRoot(pTSC)	(TSC_NodeType(pTSC) == TRIE_ROOT_NT)
#define IsHashHeader(pTSC)	(TSC_NodeType(pTSC) == HASH_HEADER_NT)
#define IsHashedNode(pTSC)	(TSC_NodeType(pTSC) & HASHED_NODE_MASK)
#define IsLeafNode(pTSC)	(TSC_NodeType(pTSC) & LEAF_NODE_MASK)

#define IsEscapeNode(pTSC)	(TSC_Instr(pTSC) == trie_proceed)

/* We could also have defined these this way...
#define IsTrieRoot(pTSC)	(TSC_Instr(pTSC) == trie_root)
#define IsHashHeader(pTSC)	(TSC_Instr(pTSC) == hash_opcode)
*/

/*
 *  From an INTERRIOR-typed node, create a LEAF-typed node, keeping
 *  the hashing status in-tact.  All nodes are assigned a status of
 *  INTERRIOR at allocation time.  Leaf status isn't known until
 *  some time afterwards.
 */
#define MakeLeafNode(pTN)	\
   TN_NodeType(pTN) = TN_NodeType(pTN) | LEAF_NODE_MASK

/*
 *  From an unHASHED-typed node, create a HASHED-typed node, keeping the
 *  LEAF/INTERRIOR status in-tact.  Used when converting from a sibling
 *  chain to a hash structure.
 */
#define MakeHashedNode(pTN)	\
   TN_NodeType(pTN) = TN_NodeType(pTN) | HASHED_NODE_MASK

/*---------------------------------------------------------------------------*/

/*
 *			   Symbol Manipulations
 *			   ====================
 *
 *  The symbols stored in tries differ slightly from their representation
 *  in the heap.  INTs, FLOATs, and STRINGs have the same tagged
 *  structure.  Lists reflect their recursive nature: [Head|Tail] begins
 *  with a symbol which is just the tag LIST whose Child node contains a
 *  symbol for Head and the Child of this node starts Tail.  E.g., [a,b]
 *  = LIST-a-LIST-b-[], where [] is a STRING constant, and '-' represents
 *  a parent-to-child connection.  Structures are CS-tagged pointers to
 *  PSC-records.  Variables are standardized and represented by TrieVar-
 *  tagged integers, starting from 0.
 */

#define TrieSymbolType(Symbol)		cell_tag(Symbol)

#define IsTrieList(Symbol)		( TrieSymbolType(Symbol) == LIST )
#define IsTrieFunctor(Symbol)		( TrieSymbolType(Symbol) == CS )
#define IsTrieVar(Symbol)		( TrieSymbolType(Symbol) == TrieVar )
#define IsTrieConstant(Symbol)			\
   ( (TrieSymbolType(Symbol) == STRING) ||	\
     (TrieSymbolType(Symbol) == INT) ||		\
     (TrieSymbolType(Symbol) == FLOAT) )

#define EncodeTrieFunctor(Cell_CS)	makecs(follow(clref_val(Cell_CS)))
#define EncodeTrieList(Cell_LIST)	( (Cell)LIST )
#define EncodeTrieConstant(Cell_Const)	( (Cell)Cell_Const )

#define DecodeTrieFunctor(Symbol)	(Psc)cs_val(Symbol)
#define DecodeTrieList(Symbol)		( Symbol )
#define DecodeTrieConstant(Symbol)	( Symbol )

/* For initializing the Symbol field in root nodes with a predicate */
#define EncodeTriePSC(pPSC)		makecs(pPSC)
#define DecodeTriePSC(Symbol)		DecodeTrieFunctor(Symbol)

/*
 *  Symbols in Escape nodes are never looked at, so we arbitrarily
 *  choose some value.
 */
#define ESCAPE_NODE_SYMBOL		(Cell)0


/* TrieVar Operations
 * ------------------
 *  Trie Variables are a type of symbol stored in a trie which represent
 *  standardized variables.  In actuality, they are 'TrieVar'-tagged,
 *  non-negative integers.  Along a path in the trie, the same integered
 *  trievar represents the same variable.  Since it is sometimes useful to
 *  know whether a trievar has already been encountered higher up in the
 *  path, first occurrences of trievars contain an additional (bit) tag.
 *
 *  When interning a term into a trie, variables in the term must be
 *  marked as they are encountered (to handle nonlinearity).  Marking of
 *  (or standardizing) these variables is performed by binding them to a
 *  special array of self-referential pointers, CallVarEnum[].  After
 *  dereferencing the variable, we can check to see whether the pointer
 *  lies within the array; if so, the variable has already been
 *  encountered.  The integer assigned to a trievar is the index into this
 *  array for the cell that marks the term's variable.
 *
 *  When unifying a term with a trie path, it will be necessary to track
 *  bindings made to variables of the trie.  Another array of
 *  self-referential pointers, TrieVarBindings[], is used to maintain
 *  these bindings.  The binding for a trievar with index I is contained
 *  in TrieVarBindings[I].
 */

extern Cell CallVarEnum[];
extern Cell TrieVarBindings[];

#define NEW_TRIEVAR_TAG      0x10000

#define EncodeNewTrieVar(Index)		maketrievar(Index | NEW_TRIEVAR_TAG)
#define EncodeTrieVar(Index)		maketrievar(Index)

#define DecodeTrieVar(Symbol)	  ( trievar_val(Symbol) & ~NEW_TRIEVAR_TAG )

/* Use this test only after determining the Symbol to be a TrieVar */
#define IsNewTrieVar(Symbol)	  ( trievar_val(Symbol) & NEW_TRIEVAR_TAG )


#define StandardizeVariable(dFreeVar,Index)	\
   bld_ref((CPtr)dFreeVar,CallVarEnum[Index])

#define IsStandardizedVariable(dFreeVar)			\
   ( ((CPtr)(dFreeVar) >= CallVarEnum) &&			\
     ((CPtr)(dFreeVar) <= (CallVarEnum + NUM_TRIEVARS - 1)) )

#define ResetStandardizedVariable(VarAddr)	\
   bld_free( ((CPtr)VarAddr) )

/*
 *  Given an address that has been determined to lie within the
 *  CallVarEnum array, determine its index within this array.
 */
#define IndexOfStandardizedVariable(pVarEnumCell)	\
   ( (CPtr)(pVarEnumCell) - CallVarEnum )


/*
 *  Derefs a symbol stored in a trie node by converting a TrieVar into
 *  an address, and then performing a normal deref operation.
 */
#define TrieSymbol_Deref(Symbol)			\
   if (IsTrieVar(Symbol)) {				\
     Symbol = TrieVarBindings[DecodeTrieVar(Symbol)];	\
     deref(Symbol);					\
   }

#define IsUnboundTrieVar(dFreeVar)					\
   ( ((CPtr)(dFreeVar) >= TrieVarBindings) &&				\
     ((CPtr)(dFreeVar) <= (TrieVarBindings + NUM_TRIEVARS - 1)) )

/*---------------------------------------------------------------------------*/

/*
 *			 Subcomponent Operations
 *		         =======================
 */

/*
 *                            Trie Nodes
 *                            ----------
 */

#define TN_Init(TN,TrieType,NodeType,Symbol,Parent,Sibling) {	\
								\
   if ( NodeType != TRIE_ROOT_NT ) {				\
     TN_SetInstr(TN,Symbol);					\
     TN_ResetInstrCPs(TN,Sibling);				\
   }								\
   else								\
     TN_Instr(TN) = trie_root;					\
   TN_Status(TN) = VALID_NODE_STATUS;				\
   TN_TrieType(TN) = TrieType;					\
   TN_NodeType(TN) = NodeType;					\
   TN_Symbol(TN) = Symbol;					\
   TN_Parent(TN) = Parent;					\
   TN_Child(TN) = NULL;						\
   TN_Sibling(TN) = Sibling;					\
 }


#define SearchChainForSymbol(Chain,Symbol,ChainLength) {	\
   ChainLength = 0;						\
   while ( IsNonNULL(Chain) && (TN_Symbol(Chain) != Symbol) ) {	\
     ChainLength++;						\
     Chain = TN_Sibling(Chain);					\
   }								\
 }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *                         Trie Hash Tables
 *                         ----------------
 *
 *  A hash table is created below a trie node when the number of its
 *  children becomes greater than MAX_SIBLING_LEN.  The hash table is
 *  organized as a header structure containing the status of the hash
 *  table, as well as a few other fields.  One is a pointer which
 *  links all allocated (in use) hash table headers, allowing rapid
 *  deallocation of the bucket arrays when the tables are abolished.    
 *  The other is an InstrPlusType field needed to support compiled
 *  tries.
 *
 *  A simple and efficient hashing function is used for maintaining
 *  nodes in the hash table.  It uses a bit-mask, rather than division,
 *  to obtain a number within the range of buckets.  Because of this,
 *  the size of the hash table must ALWAYS BE a power of 2.
 */

/*
 *  Threshold to determine when to change from a chain of children to a
 *  hash table for them.
 */
#define MAX_SIBLING_LEN   8
#define IsLongSiblingChain(ChainLength)	   ( ChainLength > MAX_SIBLING_LEN )

/*
 *  Hashing function for symbols
 */
#define CELL_TAG_SIZE	4	/* should go into cell.h for each machine */
#define TRIEVAR_BUCKET	0

#define TrieHash(Symbol, HashSeed)			\
   ( IsTrieVar(Symbol)					\
      ? TRIEVAR_BUCKET					\
      : ( ((Symbol) >> CELL_TAG_SIZE) & (HashSeed) )	\
    )

#define CalculateBucketForSymbol(pHT,Symbol)		\
   ( TrieHT_BucketArray(pHT) + TrieHash(Symbol,TrieHT_GetHashSeed(pHT)) )


/*
 *  Hashtable sizes; must be power of 2 and > MAX_SIBLING_LEN.
 */
#define TrieHT_INIT_SIZE     64
#define TrieHT_NewSize(pHT)  ( TrieHT_NumBuckets(pHT) << 1 )

extern void     hashify_children(BTNptr, int);


/*
 *  Inserting into a bucket with few nodes reflects a good hash.  In
 *  this case, we do not want to waste time expanding the table.  If,
 *  however, the bucket contains more nodes than some threshhold AND
 *  the contents of the table as a whole exceeds its size, then we
 *  expand the hash table.
 *  We could also have defined this test to be solely dependent on
 *  the number of nodes contained in the hash table, or on the length
 *  of the chain in the hashed-to bucket.
 */

#define BUCKET_CONTENT_THRESHOLD	(MAX_SIBLING_LEN / 2)

#define TrieHT_ExpansionCheck(pHT,NumBucketContents) {		\
   if ( (NumBucketContents > BUCKET_CONTENT_THRESHOLD) &&	\
        (TrieHT_NumContents(pHT) > TrieHT_NumBuckets(pHT)) )	\
     expand_trie_ht((BTHTptr)pHT);				\
 }

   
/*
 *  Insert a Trie Node into a hash table whose size is HashSeed+1.
 */
#define TrieHT_InsertNode(pBucketArray,HashSeed,pTN) {			  \
									  \
   void **pBucket;							  \
									  \
   pBucket = (void **)(pBucketArray + TrieHash(TN_Symbol(pTN),HashSeed)); \
   if ( IsNonNULL(*pBucket) ) {						  \
     TN_ForceInstrCPtoTRY(pTN);						  \
     TN_RotateInstrCPtoRETRYorTRUST((BTNptr)*pBucket);			  \
   }									  \
   else									  \
     TN_ForceInstrCPtoNOCP(pTN);					  \
   TN_Sibling(pTN) = *pBucket;						  \
   *pBucket = pTN;							  \
 }

/*===========================================================================*/

/*
 *	  S P E C I F I C   C O M P O N E N T   O P E R A T I O N S
 *	  =========================================================
 */

/*-------------------------------------------------------------------------*/

/*
 *				 Basic Tries
 *				 ===========
 */

/*
 *                             Basic Trie Node
 *                             ---------------
 */

/* For BTNs which hash children
   ---------------------------- */
#define BTN_SetHashHdr(pBTN,pTHT)	TN_SetHashHdr(pBTN,pTHT)
#define BTN_GetHashHdr(pBTN)		( (BTHTptr)TN_GetHashHdr(pBTN) )

/* For leaves of Call Tries
   ------------------------ */
#define CallTrieLeaf_SetSF(pBTN,pSF)     BTN_Child(pBTN) = (BTNptr)(pSF)
#define CallTrieLeaf_GetSF(pBTN)         ((SGFrame)BTN_Child(pBTN))

/* Allocating New BTNs
   ------------------- */
#define BTNs_PER_BLOCK   2*K
extern Structure_Manager smTableBTN;
extern Structure_Manager smAssertBTN;
extern Structure_Manager *smBTN;

BTNptr new_btn(int TrieType, int NodeType, Cell Symbol,
	       BTNptr Parent, BTNptr Sibling);

#define New_BTN(BTN,TrieType,NodeType,Symbol,Parent,Sibling)	\
   BTN = new_btn(TrieType,NodeType,Symbol,Parent,Sibling)

#define CreateEscapeBTN(pBTN,TrieType,Parent) {				\
   New_BTN(pBTN,TrieType,LEAF_NT,ESCAPE_NODE_SYMBOL,Parent,NULL);	\
   BTN_Instr(pBTN) = trie_proceed;					\
 }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *                        Basic Trie Hash Tables
 *                        ----------------------
 */

typedef struct Basic_Trie_HashTable *BTHTptr;
typedef struct Basic_Trie_HashTable {
  InstrPlusType info;
  unsigned long  numContents;
  unsigned long  numBuckets;
  BTNptr *pBucketArray;
  BTHTptr prev, next;		   /* DLL needed for branch deletion */
} BasicTrieHT;

/* Field Access Macros
   ------------------- */
#define BTHT_Instr(pTHT)		TrieHT_Instr(pTHT)
#define BTHT_Status(pTHT)		TrieHT_Status(pTHT)
#define BTHT_TrieType(pTHT)		TrieHT_TrieType(pTHT)
#define BTHT_NodeType(pTHT)		TrieHT_NodeType(pTHT)
#define BTHT_NumBuckets(pTHT)		TrieHT_NumBuckets(pTHT)
#define BTHT_NumContents(pTHT)		TrieHT_NumContents(pTHT)
#define BTHT_BucketArray(pTHT)		TrieHT_BucketArray(pTHT)
#define BTHT_PrevBTHT(pTHT)		TrieHT_PrevHT(pTHT)
#define BTHT_NextBTHT(pTHT)		TrieHT_NextHT(pTHT)

#define BTHT_GetHashSeed(pTHT)		TrieHT_GetHashSeed(pTHT)

/* General Header Management
   ------------------------- */
#define _New_TrieHT(SM,THT,TrieType) {					\
									\
   BTHTptr btht;							\
									\
   SM_AllocateStruct(SM,btht);						\
   BTHT_Instr(btht) = hash_opcode;					\
   BTHT_Status(btht) = VALID_NODE_STATUS;				\
   BTHT_TrieType(btht) = TrieType;					\
   BTHT_NodeType(btht) = HASH_HEADER_NT;				\
   BTHT_NumContents(btht) = MAX_SIBLING_LEN + 1;			\
   BTHT_NumBuckets(btht) = TrieHT_INIT_SIZE;				\
   BTHT_BucketArray(btht) =						\
     (BTNptr *)calloc(TrieHT_INIT_SIZE, sizeof(void *));		\
   if ( IsNonNULL(BTHT_BucketArray(btht)) )				\
     TrieHT_AddNewToAllocList(SM,btht)					\
   else {								\
     SM_DeallocateStruct(SM,btht);					\
     xsb_abort("No room to allocate buckets for tabling hash table");	\
   }									\
   THT = (void *)btht;							\
}

extern void     expand_trie_ht(BTHTptr);

/*
 * Allocated Hash Tables are maintained on a list to facilitate
 * deallocation of the bucket arrays when the trie space is freed
 * en masse.
 */
#define TrieHT_AddNewToAllocList(SM,THT)	\
   SM_AddToAllocList_DL(SM,THT,BTHT_PrevBTHT,BTHT_NextBTHT)

#define TrieHT_RemoveFromAllocList(SM,THT)	\
   SM_RemoveFromAllocList_DL(SM,THT,BTHT_PrevBTHT,BTHT_NextBTHT)

/* Preparation for mass deallocation */
#define TrieHT_FreeAllocatedBuckets(SM) {		\
   BTHTptr pBTHT;					\
							\
   for ( pBTHT = SM_AllocList(SM);  IsNonNULL(pBTHT);	\
	 pBTHT = BTHT_NextBTHT(pBTHT) )			\
     free(BTHT_BucketArray(pBTHT));			\
 }

/* Allocating Headers
   ------------------ */
#define BTHTs_PER_BLOCK   16
extern Structure_Manager smTableBTHT;
extern Structure_Manager smAssertBTHT;
extern Structure_Manager *smBTHT;

#define New_BTHT(BTHT,TrieType)      _New_TrieHT(*smBTHT,BTHT,TrieType)

/*---------------------------------------------------------------------------*/

/*
 *			    Time-Stamped Tries
 *			    ==================
 */

/*
 *			  Time-Stamped Trie Nodes
 *			  -----------------------
 */

/* For roots of TS Answer Tries
   ---------------------------- */
#define TSTRoot_SetHTList(pTST,pTSTHT)  TSTN_Sibling(pTST) = (TSTNptr)pTSTHT
#define TSTRoot_GetHTList(pTST)         ((TSTHTptr)TSTN_Sibling(pTST))

/* For TSTNs which hash children
   ----------------------------- */
#define TSTN_SetHashHdr(pTSTN,pTSTHT)	TN_SetHashHdr(pTSTN,pTSTHT)
#define TSTN_GetHashHdr(pTSTN)		( (TSTHTptr)TN_GetHashHdr(pTSTN) )

/* For Hashed TSTNs
   ---------------- */
#define TSTN_SetEntry(pTSTN,Entry)   TSTN_TimeStamp(pTSTN) = (TimeStamp)(Entry)
#define TSTN_GetEntry(pTSTN)	      ((EntryPtr)TSTN_TimeStamp(pTSTN))
#define TSTN_GetTSfromEntry(pTSTN)    Entry_TimeStamp(TSTN_GetEntry(pTSTN))

/* For leaves of TS Answer Tries
   ----------------------------- */
#define TSTN_GetDelayList(pTSTN)      TSTN_Child(pTSTN)

/* Allocating New TSTNs
   -------------------- */
#define TSTNs_PER_BLOCK   16
extern Structure_Manager smTSTN;

TSTNptr new_tstn(int TrieType, int NodeType, Cell Symbol,
		 TSTNptr Parent, TSTNptr Sibling);

#define New_TSTN(TSTN,TrieType,NodeType,Symbol,Parent,Sibling)	\
   TSTN = new_tstn(TrieType,NodeType,Symbol,Parent,Sibling)

#define CreateEscapeTSTN(pTSTN,TrieType,Parent) {			\
   New_TSTN(pTSTN,TrieType,LEAF_NT,ESCAPE_NODE_SYMBOL,Parent,NULL);	\
   TSTN_Instr(pTSTN) = trie_proceed;					\
 }

#define EMPTY_TST_TIMESTAMP	0
#define TSTN_DEFAULT_TIMESTAMP	1
#define PRODUCER_SF_INITIAL_TS	TSTN_DEFAULT_TIMESTAMP
#define CONSUMER_SF_INITIAL_TS	EMPTY_TST_TIMESTAMP

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *                         Time Stamp Index
 *                         ----------------
 *
 *  Hash tables allow for indexing on symbols of the stored terms.  But
 *  in the absence of symbol-key info (i.e., variable processing) it is
 *  desirable to select symbols based on time stamp value.  Therefore,
 *  hashed symbols are also organized in a secondary indexing structure
 *  based on their time stamps.  This index allows for the selection of
 *  symbols with valid time stamps -- symbols having time stamp values
 *  greater than some given time stamp -- in time proportional to such
 *  symbols.  The organization of a Time Stamp Index is maintained by a
 *  TST hash table and consists of a doubly-linked list of nodes (the
 *  'prev' field of the first node and the 'next' field of the last are
 *  always NULL) which, when traversed from head to tail, visits the
 *  symbols in decreasing time-stamp order.  These nodes are assigned
 *  one to a TSTN and contain bidirectional references, allowing for
 *  reorganization in constant time (TSTNs are likely to change their
 *  time stamp value during insertions).  The nodes of this index are
 *  globally maintained by a "Structure Manager" structure, are
 *  allocated from this pool when needed, and returned when not.  To
 *  allow rapid deallocation in accordance with the constraints of the
 *  Structure_Manager, the first word in these indexing nodes contain
 *  one of the fields used to link them into a chain.
 */

typedef struct TimeStamp_Index_Node *EntryPtr;
typedef struct TimeStamp_Index_Node {
  EntryPtr prev;
  EntryPtr next;
  TimeStamp ts;
  TSTNptr tstn;
} TSI_Entry;

#define Entry_Prev(pEntry)		((pEntry)->prev)
#define Entry_Next(pEntry)		((pEntry)->next)
#define Entry_TimeStamp(pEntry)		((pEntry)->ts)
#define Entry_TSTNode(pEntry)		((pEntry)->tstn)

#define IsHeadOfEntryList(pEntry)	IsNULL(Entry_Prev(pEntry))
#define IsTailOfEntryList(pEntry)	IsNULL(Entry_Next(pEntry))

/* Memory Management
   ----------------- */
#define TSI_ENTRIES_PER_BLOCK    16

extern Structure_Manager smEntry;

/*
 *  Set `pEntry' to an unused Entry in the global TSI_Entry resource,
 *  and associate this Entry with the TSTN pointed to by pTSTN.
 * 'prev' and 'next' links are left to the caller to set.
 */
#define New_Entry(pEntry, pTSTN) {			\
   SM_AllocateStruct(smEntry,pEntry);			\
   Entry_TSTNode(pEntry) = pTSTN;			\
   Entry_TimeStamp(pEntry) = TSTN_TimeStamp(pTSTN);	\
 }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *                       Time-Stamped Trie Hash Table
 *                       ----------------------------
 *
 */

typedef struct HashTable_for_TSTNs *TSTHTptr;
typedef struct HashTable_for_TSTNs {
  InstrPlusType info;
  unsigned long  numContents;
  unsigned long  numBuckets;
  TSTNptr *pBucketArray;
  TSTHTptr prev, next;
  TSTHTptr internalLink;     /* for reclaimation of Entries w/i a TST */
  EntryPtr head_entry;       /* these fields maintain TSI */
  EntryPtr tail_entry;
} TST_HashTable;

/* Field Access Macros
   ------------------- */
#define TSTHT_Instr(pTSTHT)		TrieHT_Instr(pTSTHT)
#define TSTHT_Padding(pTSTHT)		TrieHT_Status(pTSTHT)
#define TSTHT_TrieType(pTSTHT)		TrieHT_TrieType(pTSTHT)
#define TSTHT_NodeType(pTSTHT)		TrieHT_NodeType(pTSTHT)
#define TSTHT_NumBuckets(pTSTHT)	TrieHT_NumBuckets(pTSTHT)
#define TSTHT_NumContents(pTSTHT)	TrieHT_NumContents(pTSTHT)
#define TSTHT_BucketArray(pTSTHT)	TrieHT_BucketArray(pTSTHT)
#define TSTHT_PrevTSTHT(pTSTHT)		TrieHT_PrevHT(pTSTHT)
#define TSTHT_NextTSTHT(pTSTHT)		TrieHT_NextHT(pTSTHT)
#define TSTHT_InternalLink(pTSTHT)	((pTSTHT)->internalLink)
#define TSTHT_HeadEntry(pTSTHT)		((pTSTHT)->head_entry)
#define TSTHT_TailEntry(pTSTHT)		((pTSTHT)->tail_entry)

#define TSTHT_GetHashSeed(pTSTHT)   BTHT_GetHashSeed((BTHTptr)(pTSTHT))

/* Memory Management
   ----------------- */
#define TSTHTs_PER_BLOCK    K

extern Structure_Manager smTSTHT;

#define New_TSTHT(TSTHT,TrieType,TST) {				\
   _New_TrieHT(smTSTHT,TSTHT,TrieType);				\
   TSTHT_InternalLink(TSTHT) = TSTRoot_GetHTList(TST);		\
   TSTRoot_SetHTList(TST,TSTHT);				\
   TSTHT_HeadEntry(TSTHT) = TSTHT_TailEntry(TSTHT) = NULL;	\
 }

/*===========================================================================*/

/*
 *                             Answer List Node
 *                             ================
 */

#define ALNs_PER_BLOCK     512
extern Structure_Manager smALN;

/* Allocating New ALNs
   ------------------- */
#define New_ALN(pALN, pTN, pNext) {		\
   SM_AllocateStruct(smALN,pALN);		\
   ALN_Answer(pALN) = pTN;			\
   ALN_Next(pALN) = pNext;			\
 }

#define free_answer_list(SubgoalFrame) {			\
   if ( subg_answers(SubgoalFrame) > COND_ANSWERS )		\
     SM_DeallocateStructList(smALN,				\
			     subg_ans_list_ptr(SubgoalFrame),	\
			     subg_ans_list_tail(SubgoalFrame))	\
   else								\
     SM_DeallocateStruct(smALN,subg_ans_list_ptr(SubgoalFrame))	\
 }

/*=========================================================================*/


#endif
