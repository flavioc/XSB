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
 * components which simply use these structures through normal
 * channels.
 */


#include "struct_manager.h"
#include "tries.h"



/*===========================================================================*/

/*
 *	  G E N E R A L   C O M P O N E N T   O P E R A T I O N S
 *	  =======================================================
 *
 *  The definitions in this section apply to both types of tries: Basic
 *  and Time-Stamped.  We ensure type compatibility by defining generic
 *  macros for accessing their common fields and using them exclusively
 *  in the definitions that follow.
 *
 *  We use the following notation:
 *    TN_*         Trie Node operation
 *    TrieHT_*     Trie Hash Table operation
 *    TSC_*        Trie SubComponent (TN or THT) operation
 *    pTN          pointer to a Trie Node
 *    pTHT         pointer to a Trie Hash Table
 *    pTSC         pointer to a Trie SubComponent
 */

#define TN_Instr(pTN)		TSC_Instr(pTN)
#define TN_Status(pTN)		TSC_Status(pTN)
#define TN_TrieType(pTN)	TSC_TrieType(pTN)
#define TN_NodeType(pTN)	TSC_NodeType(pTN)
#define TN_Parent(pTN)		( (void *)BTN_Parent((BTNptr)(pTN)) )
#define TN_Child(pTN)		( (void *)BTN_Child((BTNptr)(pTN)) )
#define TN_Sibling(pTN)		( (void *)BTN_Sibling((BTNptr)(pTN)) )
#define TN_Symbol(pTN)		BTN_Symbol((BTNptr)(pTN))
#define TN_GetHashHdr(pTN)	TN_Child(pTN)

#define TrieHT_Instr(pTHT)	   TSC_Instr(pTHT)
#define TrieHT_Status(pTHT)	   TSC_Status(pTHT)
#define TrieHT_TrieType(pTHT)	   TSC_TrieType(pTHT)
#define TrieHT_NodeType(pTHT)      TSC_NodeType(pTHT)
#define TrieHT_NumBuckets(pTHT)    BTHT_NumBuckets((BTHTptr)(pTHT))
#define TrieHT_NumContents(pTHT)   BTHT_NumContents((BTHTptr)(pTHT))
#define TrieHT_BucketArray(pTHT)   ( (void **)BTHT_BucketArray((BTHTptr)(pTHT)) )
#define TrieHT_AllocLink(pTHT)     ( (void *)BTHT_AllocLink((BTHTptr)(pTHT)) )
#define TrieHT_GetHashMask(pTHT)   BTHT_GetHashMask((BTHTptr)(pTHT))

#define TSC_Instr(pTSC)		IPT_Instr(*(InstrPlusType *)(pTSC))
#define TSC_Status(pTSC)	IPT_Status(*(InstrPlusType *)(pTSC))
#define TSC_TrieType(pTSC)	IPT_TrieType(*(InstrPlusType *)(pTSC))
#define TSC_NodeType(pTSC)	IPT_NodeType(*(InstrPlusType *)(pTSC))

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


#define TN_SetInstr(pTN,Symbol)				\
   switch( TrieSymbolType(Symbol) ) {			\
   case CS:						\
     TN_Instr(pTN) = (byte)trie_try_str;		\
     break;						\
   case INT:						\
   case STRING:						\
   case FLOAT:						\
     TN_Instr(pTN) = (byte)trie_try_numcon;		\
     break;						\
   case TrieVar:					\
     if (IsNewTrieVar(Symbol))				\
       TN_Instr(pTN) = (byte)trie_try_var;		\
     else						\
       TN_Instr(pTN) = (byte)trie_try_val;		\
     break;						\
   case LIST:						\
     TN_Instr(pTN) = (byte)trie_try_list;		\
     break;						\
   default:						\
     xsb_abort("Trie Node creation: Bad tag in symbol %lx", \
               Symbol);	    	    	    		\
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

#define  TRIE_ROOT_NT		0x08   /* binary:  1000 */
#define  HASH_HEADER_NT		0x04   /* binary:  0100 */
#define  LEAF_NT		0x02   /* binary:  0010 */
#define  HASHED_LEAF_NT		0x03   /* binary:  0011 */
#define  INTERRIOR_NT		0x00   /* binary:  0000 */
#define  HASHED_INTERRIOR_NT	0x01   /* binary:  0001 */

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
 *  these variables is performed by binding them to a special array of
 *  self-referential pointers, VarEnumerator[].  After dereferencing the
 *  variable, we can check to see whether the pointer lies within the
 *  array; if so, the variable has already been encountered.  The integer
 *  assigned to a trievar is the index into this array for the cell that
 *  marks the term's variable.
 */

#define NEW_TRIEVAR_TAG      0x10000

#define EncodeNewTrieVar(Index)		maketrievar(Index | NEW_TRIEVAR_TAG)
#define EncodeTrieVar(Index)		maketrievar(Index)

#define DecodeTrieVar(Symbol)	  ( trievar_val(Symbol) & ~NEW_TRIEVAR_TAG )

/* Use this test only after determining the Symbol to be a TrieVar */
#define IsNewTrieVar(Symbol)	  ( trievar_val(Symbol) & NEW_TRIEVAR_TAG )


#define StandardizeVariable(DerefedVar,Index)	\
   bld_ref(DerefedVar,VarEnumerator[Index])

#define IsStandardizedVariable(DerefedVar)			\
   ( ((CPtr)(DerefedVar) >= VarEnumerator) &&			\
     ((CPtr)(DerefedVar) <= (VarEnumerator + NUM_TRIEVARS - 1)) )

/*
 *  Given an address that has been determined to lie within the
 *  VarEnumerator array, determine its index within this array.
 */
#define IndexOfStandardizedVariable(pVarEnumCell)	\
   ( (CPtr)(pVarEnumCell) - VarEnumerator )

/*---------------------------------------------------------------------------*/

/*
 *			 Subcomponent Operations
 *		         =======================
 */

#define SearchChainForSymbol(Chain,Symbol,ChainLength) {	\
   ChainLength = 0;						\
   while ( IsNonNULL(Chain) && (TN_Symbol(Chain) != Symbol) ) {	\
     ChainLength++;						\
     Chain = TN_Sibling(Chain);					\
   }								\
 }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *                       Basic Trie Hash Table
 *                       ---------------------
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

#define TrieHash(Symbol, HashMask)			\
   ( IsTrieVar(Symbol)					\
      ? TRIEVAR_BUCKET					\
      : ( ((Symbol) >> CELL_TAG_SIZE) & (HashMask) )	\
    )

/*
 *  Hashtable sizes; must be power of 2 and > MAX_SIBLING_LEN.
 */
#define TrieHT_INIT_SIZE     64
#define TrieHT_NewSize(pHT)  ( TrieHT_NumBuckets(pHT) << 1 )

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
     expand_trie_ht(pHT);					\
 }

   
/*
 *  Insert a Trie Node into a hash table whose size is HashMask+1.
 */
#define TrieHT_InsertNode(pBucketArray,HashMask,pTrieNode) {		\
									\
   BTNptr pTN, *pBucket;						\
          								\
   pTN = (BTNptr)pTrieNode;						\
   pBucket = (BTNptr *)pBucketArray + TrieHash(TN_Symbol(pTN),HashMask);\
   if ( IsNonNULL(*pBucket) ) {						\
     TN_ForceInstrCPtoTRY(pTN);						\
     TN_RotateInstrCPtoRETRYorTRUST(*pBucket);				\
   }									\
   else									\
     TN_ForceInstrCPtoNOCP(pTN);					\
   BTN_Sibling(pTN) = *pBucket;						\
   *pBucket = pTN;							\
 }


#define Calculate_Bucket_for_Symbol(pHT,Symbol)		\
   (void *)( TrieHT_BucketArray(pHT) +			\
	     TrieHash(Symbol,TrieHT_GetHashMask(pHT)) )


/*===========================================================================*/

/*
 *	  S P E C I F I C   C O M P O N E N T   O P E R A T I O N S
 *	  =========================================================
 */

/*
 *				Basic Tries
 *				===========
 */


/*
 *                            Basic Trie Node
 *                            ---------------
 */

#define BTN_SetHashHdr(pBTN,pTHT)   BTN_Child(pBTN) = (BTNptr)(pTHT)
#define BTN_GetHashHdr(pBTN)        ((BTHTptr)BTN_Child(pBTN))

#define BTN_SetSF(pBTN,pSF)     BTN_Child(pBTN) = (BTNptr)(pSF)
#define BTN_GetSF(pBTN)         ((SGFrame)BTN_Child(pBTN))

#define BTNs_PER_BLOCK   2*K
extern Structure_Manager smTableBTN;
extern Structure_Manager smAssertBTN;
extern Structure_Manager *smBTN;

/* Allocating New BTNs
   ------------------- */
#define New_BTN(pBTN,TrieType,NodeType,Symbol,Parent,Sibling) {	\
   SM_AllocateStruct(*smBTN,pBTN);				\
   TN_SetInstr(pBTN,Symbol);					\
   TN_ResetInstrCPs(pBTN,Sibling);				\
   BTHT_Status(pBTN) = VALID_NODE_STATUS;			\
   BTN_TrieType(pBTN) = TrieType;				\
   BTN_NodeType(pBTN) = NodeType;				\
   BTN_Symbol(pBTN) = Symbol;					\
   BTN_Parent(pBTN) = Parent;					\
   BTN_Child(pBTN) = NULL;					\
   BTN_Sibling(pBTN) = Sibling;					\
 }

#define CreateEscapeBTN(pBTN,TrieType,Parent) {				\
   New_BTN(pBTN,TrieType,LEAF_NT,ESCAPE_NODE_SYMBOL,Parent,NULL);	\
   BTN_Instr(pBTN) = trie_proceed;					\
 }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *                     Basic Trie Hash Table Headers
 *                     -----------------------------
 */


#define BTHTs_PER_BLOCK   16
extern Structure_Manager smTableBTHT;
extern Structure_Manager smAssertBTHT;
extern Structure_Manager *smBTHT;

#define New_BTHT(pBTHT,TrieType) {					\
   SM_AllocateStruct(*smBTHT,pBTHT);					\
   BTHT_Instr(pBTHT) = hash_opcode;					\
   BTHT_Status(pBTHT) = VALID_NODE_STATUS;				\
   BTHT_TrieType(pBTHT) = TrieType;					\
   BTHT_NodeType(pBTHT) = HASH_HEADER_NT;				\
   BTHT_NumContents(pBTHT) = MAX_SIBLING_LEN + 1;			\
   BTHT_NumBuckets(pBTHT) = TrieHT_INIT_SIZE;				\
   BTHT_BucketArray(pBTHT) =						\
     (BTNptr *)calloc(TrieHT_INIT_SIZE, sizeof(BTNptr));		\
   if ( IsNonNULL(BTHT_BucketArray(pBTHT)) )				\
     BTHT_AddNewToAllocList(pBTHT) 					\
   else {								\
     SM_DeallocateStruct(*smBTHT,pBTHT);				\
     xsb_abort("No room to allocate buckets for tabling hash table");	\
   }									\
 }

/*
 * Allocated Hash Tables are maintained on a list to facilitate
 * deallocation of the bucket arrays when the trie space is freed
 * en masse.
 */
#define BTHT_AddNewToAllocList(pBTHT)					\
   SM_AddToAllocList_DL(*smBTHT,pBTHT,BTHT_PrevBTHT,BTHT_NextBTHT)

#define BTHT_RemoveFromAllocList(pBTHT)					 \
   SM_RemoveFromAllocList_DL(*smBTHT,pBTHT,BTHT_PrevBTHT,BTHT_NextBTHT)

/* Prepare for mass deallocation */
#define BTHT_FreeAllocatedBuckets {				\
   BTHTptr pBTHT;						\
								\
   for ( pBTHT = SM_AllocList(*smBTHT);  IsNonNULL(pBTHT);	\
	 pBTHT = BTHT_NextBTHT(pBTHT) )				\
     free(BTHT_BucketArray(pBTHT));				\
 }

/*===========================================================================*/

/*
 *			      Subgoal Frames
 *			      ==============
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



#endif
