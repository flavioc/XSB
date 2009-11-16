/* File:      tries.c
** Author(s): Prasad Rao, David S. Warren, Kostis Sagonas,
**    	      Juliana Freire, Baoqiu Cui
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


#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Special debug includes */
#include "debugs/debug_tries.h"

#include "auxlry.h"
#include "context.h"
#include "cell_xsb.h"
#include "inst_xsb.h"
#include "psc_xsb.h"
#include "heap_xsb.h"
#include "flags_xsb.h"
#include "deref.h"
#include "memory_xsb.h"
#include "register.h"
#include "binding.h"
#include "trie_internals.h"
#include "macro_xsb.h"
#include "choice.h"
#include "cinterf.h"
#include "error_xsb.h"
#include "tr_utils.h"
#include "rw_lock.h"
#include "thread_xsb.h"
#include "debug_xsb.h"
#include "subp.h"
#include "call_graph_xsb.h" /* for incremental evaluation */
#include "biassert_defs.h"
#include "loader_xsb.h" /* for XOOM_FACTOR */

/*----------------------------------------------------------------------*/
/* The following variables are used in other parts of the system        */
/*----------------------------------------------------------------------*/

counter subg_chk_ins, subg_inserts, ans_chk_ins, ans_inserts; /* statistics */

#ifndef MULTI_THREAD
int  num_heap_term_vars;
CPtr *var_addr;
int  var_addr_arraysz;
Cell VarEnumerator[NUM_TRIEVARS];
Cell TrieVarBindings[NUM_TRIEVARS];
#endif

/* xsbBool check_table_cut = TRUE;  flag for close_open_tables to turn off
				    cut-over-table check */

/*
 * global_num_vars is a new variable to save the value of variable
 * num_vars_in_var_regs temporarily.
 */
#ifndef MULTI_THREAD
int global_num_vars;
#endif

/*
 * Array VarEnumerator_trail[] is used to trail the variable bindings when we
 * copy terms into tries.  The variables trailed using VarEnumerator_trail are
 * those that are bound to elements in VarEnumerator[].
 */
#ifndef MULTI_THREAD
CPtr VarEnumerator_trail[NUM_TRIEVARS];
CPtr *VarEnumerator_trail_top;
#endif


char *trie_node_type_table[] = {"interior_nt","hashed_interior_nt","leaf_nt",
			   "hashed_leaf_nt","hash_header_nt","undefined",
			   "undefined","undefined","trie_root_nt"};

char *trie_trie_type_table[] = {"call_trie_tt","basic_answer_trie_tt",
				"ts_answer_trie_tt","delay_trie_tt",
				"assert_trie_tt","intern_trie_tt"
};

/*----------------------------------------------------------------------*/
/* Safe assignment -- can be generalized by type.
   CPtr can be abstracted out */
#define safe_assign(ArrayNam,Index,Value,ArraySz) {\
   if (Index >= ArraySz) {\
     trie_expand_array(CPtr,ArrayNam,ArraySz,Index,"var_addr");\
   }\
   ArrayNam[Index] = Value;\
}

/*----------------------------------------------------------------------*/
/*****************Addr Stack************* 

 TLS 08/05: The addr_stack and term_stack (below) are used by
 answer_return to copy information out of a trie and into a ret/n
 structure.  Its also used by table predicates to get delay lists.

 */

#ifndef MULTI_THREAD
static int addr_stack_pointer = 0;
static CPtr *Addr_Stack;
static int addr_stack_size    = DEFAULT_ARRAYSIZ;
#endif

#define pop_addr Addr_Stack[--addr_stack_pointer]
#define push_addr(X) {\
    if (addr_stack_pointer == addr_stack_size) {\
       trie_expand_array(CPtr, Addr_Stack ,addr_stack_size,0,"Addr_Stack");\
    }\
    Addr_Stack[addr_stack_pointer++] = ((CPtr) X);\
}

/*----------------------------------------------------------------------*/
/*****************Term Stack*************/
#ifndef MULTI_THREAD
static int  term_stackptr = -1;
static Cell *term_stack;
static long term_stacksize = DEFAULT_ARRAYSIZ;
#endif

#define pop_term term_stack[term_stackptr--]
#define push_term(T) {\
    if (term_stackptr+1 == term_stacksize) {\
       trie_expand_array(Cell,term_stack,term_stacksize,0,"term_stack");\
    }\
    term_stack[++term_stackptr] = ((Cell) T);\
}

/*----------------------------------------------------------------------*/
/*********Simpler trails ****************/

#define undo_answer_bindings		\
    while (VarEnumerator_trail_top >= VarEnumerator_trail) {	\
	untrail(*VarEnumerator_trail_top);		\
	VarEnumerator_trail_top--;			\
    }	

#define StandardizeAndTrailVariable(addr,n)	\
   StandardizeVariable(addr,n);			\
    *(++VarEnumerator_trail_top) = addr;
		
/*----------------------------------------------------------------------*/
/* Variables used only in this file                                     */
/*----------------------------------------------------------------------*/

/* for make_heap_term */
static BasicTrieNode dummy_ans_node = {{0,1,0,0},NULL,NULL,NULL,0};

#ifndef MULTI_THREAD
int AnsVarCtr;
#endif

/*----------------------------------------------------------------------*/

/*
 *          T R I E   S T R U C T U R E   M A N A G E M E N T
 *          =================================================
 */
char *TrieSMNameTable[] = {"Basic Trie Node (Private)",
		   "Basic Trie Hash Table (Private)"};

/* For Call and Answer Tries
   ------------------------- */

Structure_Manager smTableBTN  = SM_InitDecl(BasicTrieNode, BTNs_PER_BLOCK,
					    "Basic Trie Node");
Structure_Manager smTableBTHT = SM_InitDecl(BasicTrieHT, BTHTs_PER_BLOCK,
					    "Basic Trie Hash Table");

/*
Structure_Manager smTableBTHTArray = BuffM_InitDecl((TrieHT_INIT_SIZE*(sizeof(Cell))), BTHTs_PER_BLOCK,
					    "Basic Trie Hash Table ARRAY");
*/
/* For Assert & Intern Tries
   ------------------------- */
Structure_Manager smAssertBTN  = SM_InitDecl(BasicTrieNode, BTNs_PER_BLOCK,
					     "Basic Trie Node");
Structure_Manager smAssertBTHT = SM_InitDecl(BasicTrieHT, BTHTs_PER_BLOCK,
					     "Basic Trie Hash Table");

/* Maintains Current Structure Space
   --------------------------------- */

/* MT engine uses both shared and private structure managers,
   sequential engine doesn't.  In addition, in MT engine, all
   subsumptive tables are private, thus use subsumptive_smBTN/BTHT for
   structure managers common to both variant and private tables. */

#ifndef MULTI_THREAD
Structure_Manager smTSTN      = SM_InitDecl(TS_TrieNode, TSTNs_PER_BLOCK,
					    "Time-Stamped Trie Node");
Structure_Manager smTSTHT     = SM_InitDecl(TST_HashTable, TSTHTs_PER_BLOCK,
					    "Time-Stamped Trie Hash Table");
Structure_Manager smTSIN      = SM_InitDecl(TS_IndexNode, TSINs_PER_BLOCK,
					    "Time-Stamp Indexing Node");

Structure_Manager *smBTN = &smTableBTN;
Structure_Manager *smBTHT = &smTableBTHT;

#endif


/*----------------------------------------------------------------------*/

void init_trie_aux_areas(CTXTdecl)
{
  int i;

  /* TLS: commented these out to catch private/shared bugs more
     quickly */
#ifndef MULTI_THREAD
  smBTN = &smTableBTN;
  smBTHT = &smTableBTHT;
#endif

  addr_stack_size = 0;
  Addr_Stack = NULL;
  addr_stack_pointer = 0;

  term_stacksize = 0;
  term_stack = NULL;
  term_stackptr = -1;

  var_addr_arraysz = 0;
  var_addr = NULL;

  reg_array = NULL;
  reg_array_size = 0;
  reg_arrayptr = reg_array -1;

  for (i = 0; i < NUM_TRIEVARS; i++)
    VarEnumerator[i] = (Cell) & (VarEnumerator[i]);
}

void free_trie_aux_areas(CTXTdecl)
{
  mem_dealloc(term_stack,term_stacksize,TABLE_SPACE);
  mem_dealloc(var_addr,var_addr_arraysz,TABLE_SPACE);
  mem_dealloc(Addr_Stack,addr_stack_size,TABLE_SPACE);
  mem_dealloc(reg_array,reg_array_size,TABLE_SPACE);
}

/*-------------------------------------------------------------------------*/

BTNptr new_btn(CTXTdeclc int trie_t, int node_t, Cell symbol, BTNptr parent,
	       BTNptr sibling) {

  void *btn;

  SM_AllocatePossSharedStruct( *smBTN, btn );
  TN_Init(((BTNptr)btn),trie_t,node_t,symbol,parent,sibling);
  return (BTNptr)btn;
}

/*-------------------------------------------------------------------------*/

TSTNptr new_tstn(CTXTdeclc int trie_t, int node_t, Cell symbol, TSTNptr parent,
		TSTNptr sibling) {

  void * tstn;

  SM_AllocateStruct(smTSTN,tstn);
  TN_Init(((TSTNptr)tstn),trie_t,node_t,symbol,parent,sibling);
  TSTN_TimeStamp(((TSTNptr)tstn)) = TSTN_DEFAULT_TIMESTAMP;
  return (TSTNptr)tstn;
}

/*----------------------------------------------------------------------*/

BTHTptr  New_BTHT(CTXTdeclc Structure_Manager * SM,int TrieType) {
   void * btht;								

#ifdef MULTI_THREAD  
     /* Lock shared BTHT structure manager and malloc BTHT array */
   if (threads_current_sm == SHARED_SM) {
     /* Lock shared BTHT structure manager and malloc BTHT array */
     SM_AllocateSharedStruct(*SM,btht);
     SM_Lock(*SM);
     SM_AddToAllocList_DL(*SM,((BTHTptr)btht),BTHT_PrevBTHT,BTHT_NextBTHT);
     SM_Unlock(*SM);
     BTHT_BucketArray(((BTHTptr)btht)) =				
       (BTNptr *)mem_calloc(TrieHT_INIT_SIZE, sizeof(void *),TABLE_SPACE); 
   }
   else {
     SM_AllocateStruct(*SM,btht);
     SM_AddToAllocList_DL(*SM,((BTHTptr)btht),BTHT_PrevBTHT,BTHT_NextBTHT);
     BTHT_BucketArray(((BTHTptr)btht)) =				
       (BTNptr *)mem_calloc(TrieHT_INIT_SIZE, sizeof(void *),TABLE_SPACE); 
   }
#else
   /* Dont lock BTHT structure manager and malloc BTHT array */
   SM_AllocateStruct(*SM,btht);
   SM_AddToAllocList_DL(*SM,((BTHTptr)btht),BTHT_PrevBTHT,BTHT_NextBTHT);
   BTHT_BucketArray(((BTHTptr)btht)) =					 
     (BTNptr *)mem_calloc(TrieHT_INIT_SIZE, sizeof(void *),TABLE_SPACE); 
#endif
   
   BTHT_Instr(((BTHTptr)btht)) = hash_opcode;				
   BTHT_Status(((BTHTptr)btht)) = VALID_NODE_STATUS;			
   BTHT_TrieType(((BTHTptr)btht)) = TrieType;				
   BTHT_NodeType(((BTHTptr)btht)) = HASH_HEADER_NT;			
   BTHT_NumContents(((BTHTptr)btht)) = MAX_SIBLING_LEN + 1;		
   BTHT_NumBuckets(((BTHTptr)btht)) = TrieHT_INIT_SIZE;			
   return (BTHTptr) btht;								
}

/*-------------------------------------------------------------------------*/

/*
 * Creates a root node for a given type of trie.
 */

BTNptr newBasicTrie(CTXTdeclc Cell symbol, int trie_type) {

  BTNptr pRoot;

  New_BTN( pRoot, trie_type, TRIE_ROOT_NT, symbol, NULL, NULL );
  return pRoot;
}

/*-------------------------------------------------------------------------*/

/* TLS: took out single use, 11/09
 * Creates a root node for a given type of trie.  Differs from above in that
 * the parent is intended to be set to the subgoal frame.
 * 
 * BTNptr newBasicAnswerTrie(CTXTdeclc Cell symbol, CPtr Paren, int trie_type) {
 * 
 *   BTNptr pRoot;
 * 
 *   New_BTN( pRoot, trie_type, TRIE_ROOT_NT, symbol, Paren, NULL );
 *   return pRoot;
 * }
 */


/*----------------------------------------------------------------------*/

/* Used by one_node_chk_ins only. */
#define IsInsibling(wherefrom,count,Found,item,TrieType)		\
{									\
  LocalNodePtr = wherefrom;						\
  while (LocalNodePtr && (BTN_Symbol(LocalNodePtr) != item)) {		\
    LocalNodePtr = BTN_Sibling(LocalNodePtr);				\
    count++;								\
  }									\
  if ( IsNULL(LocalNodePtr) ) {						\
    Found = 0;								\
    New_BTN(LocalNodePtr,TrieType,INTERIOR_NT,item,Paren,wherefrom);	\
    count++;								\
    wherefrom = LocalNodePtr;  /* hook the new node into the trie */	\
  }									\
  Paren = LocalNodePtr;							\
}


/*
 *  Insert/find a single symbol in the trie structure 1-level beneath a
 *  parent NODE, pointed to by `Paren', whose child link field is
 *  pointed to by 'GNodePtrPtr'.  (If 'Paren' is NULL, then we are most
 *  likely searching beneath some other structure, like the TIP, and
 *  'GNodePtrPtr' points to its "trie root" field.)  If the symbol
 *  cannot be found, create a NODE for this symbol and make it the child
 *  of `Paren' by setting the field that 'GNodePtrPtr' points to to this
 *  new NODE.  Upon exiting this macro, 'Paren' is set to point to the
 *  node containing this symbol and 'GNodePtrPtr' gets the address of
 *  this nodes' Child field.
~ *
 *  Algorithm:
 *  ---------
 *  If the parent has no children, then create a node for the symbol
 *  and link it to the parent and vice versa.  Set the `Found' flag to
 *  indicate that a new node was necessary.
 *
 *  Otherwise, if the parent utilizes a hash structure for maintaining
 *  its children, check to see if there is enough room for one more
 *  entry.  If not, then expand the hash structure.  Search for the
 *  node containing the symbol in question, inserting it if it is not
 *  found.  Signify through `Found' the result of this action.
 *
 *  Otherwise, look for the symbol in a normal chain of children
 *  beneath the parent.  If it is not found, then insert it and check
 *  to see if the chain has now become too long; if so, then create a
 *  hash structure for the parent's children.  Signify through `Found'
 *  the result of this action.
 *
 *  Prepare for the next insertion/lookup by changing the `hook' to
 *  that of the child pointer field of the node which contains the
 *  just-processed symbol.
 */

#define one_node_chk_ins(Found,item,TrieType) {				\
									\
   int count = 0;							\
   BTNptr LocalNodePtr;							\
									\
   TRIE_W_LOCK();							\
   if ( IsNULL(*GNodePtrPtr) ) {					\
     New_BTN(LocalNodePtr,TrieType,INTERIOR_NT,item,Paren,NULL);	\
     *GNodePtrPtr = Paren = LocalNodePtr;				\
     Found = 0;								\
   }									\
   else if ( IsHashHeader(*GNodePtrPtr) ) {				\
     BTHTptr ht = (BTHTptr)*GNodePtrPtr;				\
     GNodePtrPtr = CalculateBucketForSymbol(ht,item);			\
     IsInsibling(*GNodePtrPtr,count,Found,item,TrieType);		\
     if (!Found) {							\
       MakeHashedNode(LocalNodePtr);					\
       BTHT_NumContents(ht)++;						\
       TrieHT_ExpansionCheck(ht,count);					\
     }									\
   }									\
   else {								\
     BTNptr pParent = Paren;						\
     IsInsibling(*GNodePtrPtr,count,Found,item,TrieType);		\
     if (IsLongSiblingChain(count))					\
       /* used to pass in GNodePtrPtr (ptr to hook) */			\
       hashify_children(CTXTc pParent,TrieType);			\
   }									\
   GNodePtrPtr = &(BTN_Child(LocalNodePtr));				\
   TRIE_W_UNLOCK();							\
}

/* This functional version of the above code is not used in the system
   -- but I  use it for debugging (sequential mode) so please leave it around

void fone_node_chk_ins(int Found,Cell item,int TrieType,BTNptr **GNPP,
		       BTNptr *Parent) {
									
   int count = 0;							
   BTNptr LocalNodePtr;					

   BTNptr *GNodePtrPtr = *GNPP;
   BTNptr Paren = *Parent;
									
   TRIE_W_LOCK();							
   if ( IsNULL(*GNodePtrPtr) ) {					
     New_BTN(LocalNodePtr,TrieType,INTERIOR_NT,item,Paren,NULL);	
     *GNodePtrPtr = Paren = LocalNodePtr;				
     Found = 0;								
   }									
   else if ( IsHashHeader(*GNodePtrPtr) ) {				
     BTHTptr ht = (BTHTptr)*GNodePtrPtr;				
     GNodePtrPtr = CalculateBucketForSymbol(ht,item);			
     IsInsibling(*GNodePtrPtr,count,Found,item,TrieType);		
     if (!Found) {							
     MakeHashedNode(LocalNodePtr);					
       BTHT_NumContents(ht)++;						
       TrieHT_ExpansionCheck(ht,count);					
     }									
   }									
   else {								
     BTNptr pParent = Paren;						
     IsInsibling(*GNodePtrPtr,count,Found,item,TrieType);		
     if (IsLongSiblingChain(count))					
//       used to pass in GNodePtrPtr (ptr to hook)		      
       hashify_children(CTXTc pParent,TrieType);			
   }									
   GNodePtrPtr = &(BTN_Child(LocalNodePtr));				
   *GNPP = GNodePtrPtr;
   *Parent = Paren;
}
*/

/*----------------------------------------------------------------------*/

/* Trie-HashTable maintenance routines.
   ------------------------------------
   parentHook is the address of a field in some structure (should now be
   another trie node as all tries now have roots) which points to a chain
   of trie nodes whose length has become "too long."
*/

int hashcount_gl = 0;

void hashify_children(CTXTdeclc BTNptr parent, int trieType) {

  BTNptr children;		/* child list of the parent */
  BTNptr btn;			/* current child for processing */
  BTHTptr ht;			/* HT header struct */
  BTNptr *tablebase;		/* first bucket of allocated HT */
  unsigned long  hashseed;	/* needed for hashing of BTNs */

  ht = New_BTHT(CTXTc smBTHT, trieType);
  children = BTN_Child(parent);
  BTN_SetHashHdr(parent,ht);
  tablebase = BTHT_BucketArray(ht);
  hashseed = BTHT_GetHashSeed(ht);
  for (btn = children;  IsNonNULL(btn);  btn = children) {
    children = BTN_Sibling(btn);
    TrieHT_InsertNode(tablebase, hashseed, btn);
    MakeHashedNode(btn);
  }
}

/*-------------------------------------------------------------------------*/

/*
 *  Expand the hash table pointed to by 'pHT'.  Note that we can do this
 *  in place by using realloc() and noticing that, since the hash tables
 *  and hashing function are based on powers of two, a node existing in
 *  a bucket will either remain in that bucket -- in the lower part of
 *  the new table -- or jump to a corresponding bucket in the upper half
 *  of the expanded table.  This function can serve for all types of
 *  tries since only fields contained in a Basic Trie Hash Table are
 *  manipulated.
 *
 *  As expansion is a method for reducing access time and is not a
 *  critical operation, if the table cannot be expanded at this time due
 *  to memory limitations, then simply return.  Otherwise, initialize
 *  the top half of the new area, and rehash each node in the buckets of
 *  the lower half of the table.
 */

BTNptr *resize_hash_array(CTXTdeclc BTHTptr pHT, int new_size) {
  BTNptr *bucket_array;     /* base address of resized hash table */

  //  printf("resizing hash to %d\n",new_size);
  bucket_array = (BTNptr *)mem_realloc( BTHT_BucketArray(pHT), BTHT_NumBuckets(pHT)*sizeof(void*),
					new_size * sizeof(BTNptr),TABLE_SPACE );
  return bucket_array;
}
    
void expand_trie_ht(CTXTdeclc BTHTptr pHT) {

  BTNptr *bucket_array;     /* base address of resized hash table */
  BTNptr *upper_buckets;    /* marker in the resized HT delimiting where the
			        newly allocated buckets begin */
  BTNptr *bucket;           /* for stepping through buckets of the HT */
  BTNptr curNode;           /* TSTN being processed */
  BTNptr nextNode;          /* rest of the TSTNs in a bucket */

  unsigned long  new_size;  /* double duty: new HT size, then hash mask */

  new_size = TrieHT_NewSize(pHT);

  bucket_array = resize_hash_array(CTXTc pHT, new_size);

  if ( IsNULL(bucket_array) )     return;

  upper_buckets = bucket_array + BTHT_NumBuckets(pHT);
  for (bucket = upper_buckets;  bucket < bucket_array + new_size;  bucket++)
    *bucket = NULL;
  BTHT_NumBuckets(pHT) = new_size;
  new_size--;     /* 'new_size' is now the hashing mask */
  BTHT_BucketArray(pHT) = bucket_array;
	 
  for (bucket = bucket_array;  bucket < upper_buckets;  bucket++) {
    curNode = *bucket;
    *bucket = NULL;
    while ( IsNonNULL(curNode) ) {
      nextNode = TN_Sibling(curNode);
      TrieHT_InsertNode(bucket_array, new_size, curNode);
      curNode = nextNode;
    }
  }
}

/*----------------------------------------------------------------------*/

/*
 * Push the symbols along the path from the leaf to the root in a trie
 * onto the termstack.
 */
static int follow_par_chain(CTXTdeclc BTNptr pLeaf)
{
  int heap_space = 0;
  Cell sym;

  term_stackptr = -1; /* Forcibly Empty term_stack */
  while ( IsNonNULL(pLeaf) && (! IsTrieRoot(pLeaf)) ) {
    sym = BTN_Symbol(pLeaf);
    push_term(sym);
    if (TrieSymbolType(sym) == XSB_STRUCT) heap_space+=2;
    else heap_space++;
    pLeaf = BTN_Parent(pLeaf);
  }
  return heap_space;
}

/*----------------------------------------------------------------------*/

/*
 * Given a hook to an answer-list node, returns the answer contained in
 * that node and updates the hook to the next node in the chain.
 */
BTNptr get_next_trie_solution(ALNptr *NextPtrPtr)
{
  BTNptr TempPtr;

  TempPtr = ALN_Answer(*NextPtrPtr);
  *NextPtrPtr = ALN_Next(*NextPtrPtr);
  return(TempPtr);
}

/*----------------------------------------------------------------------*/

#define rec_macro_make_heap_term(Macro_addr) {				\
  int rj,rArity;							\
  while(addr_stack_pointer) {						\
    Macro_addr = (CPtr)pop_addr;					\
    xtemp2 = pop_term;							\
    switch( TrieSymbolType(xtemp2) ) {					\
    case XSB_TrieVar: {							\
      int index = DecodeTrieVar(xtemp2);				\
      if (IsNewTrieVar(xtemp2)) {					\
	safe_assign(var_addr,index,Macro_addr,var_addr_arraysz);	\
	num_heap_term_vars++;						\
      }									\
      else if (IsNewTrieAttv(xtemp2)) {					\
        safe_assign(var_addr,index,					\
		    (CPtr) makeattv(hreg),var_addr_arraysz);		\
        num_heap_term_vars++;						\
        new_heap_free(hreg);						\
        push_addr(hreg);						\
        hreg++;								\
      }									\
      *Macro_addr = (Cell) var_addr[index];				\
    }									\
    break;								\
    case XSB_STRING:							\
    case XSB_INT:	       						\
    case XSB_FLOAT:	       						\
      *Macro_addr = xtemp2;						\
      break;								\
    case XSB_LIST:	       						\
      *Macro_addr = (Cell) makelist(hreg);				\
      hreg += 2;							\
      push_addr(hreg-1);						\
      push_addr(hreg-2);						\
      break;								\
    case XSB_STRUCT:		       					\
      *Macro_addr = (Cell) makecs(hreg);				\
      xtemp2 = (Cell) DecodeTrieFunctor(xtemp2);			\
      *hreg = xtemp2;							\
      rArity = (int) get_arity((Psc) xtemp2);				\
      for (rj= rArity; rj >= 1; rj --) {				\
	push_addr(hreg+rj);						\
      }									\
      hreg += rArity;							\
      hreg++;								\
      break;								\
    default:								\
      xsb_abort("Bad tag in macro_make_heap_term");			\
      return;								\
    }									\
  }									\
}

/*----------------------------------------------------------------------*/

#define macro_make_heap_term(ataddr,ret_val,dummy_addr) {		\
  int mArity,mj;							\
  xtemp2 = pop_term;							\
  switch( TrieSymbolType(xtemp2) ) {					\
  case XSB_TrieVar: {							\
    int index = DecodeTrieVar(xtemp2);					\
    if (IsNewTrieVar(xtemp2)) { /* diff with CHAT - Kostis */		\
      safe_assign(var_addr,index,ataddr,var_addr_arraysz);		\
      num_heap_term_vars++;						\
    }									\
    else if (IsNewTrieAttv(xtemp2)) {					\
      safe_assign(var_addr, index,					\
		  (CPtr) makeattv(hreg),var_addr_arraysz);		\
      num_heap_term_vars++;						\
      new_heap_free(hreg);						\
      push_addr(hreg);							\
      hreg++;								\
      rec_macro_make_heap_term(dummy_addr);				\
    }									\
    ret_val = (Cell) var_addr[index];					\
  }									\
  break;								\
  case XSB_STRING:     							\
  case XSB_INT:	       							\
  case XSB_FLOAT:      							\
    ret_val = xtemp2;							\
    break;								\
  case XSB_LIST:			       				\
    ret_val = (Cell) makelist(hreg) ;					\
    hreg += 2;								\
    push_addr(hreg-1);							\
    push_addr(hreg-2);							\
    rec_macro_make_heap_term(dummy_addr);				\
    break;								\
  case XSB_STRUCT:		       					\
    ret_val = (Cell) makecs(hreg);					\
    xtemp2 = (Cell) DecodeTrieFunctor(xtemp2);				\
    *hreg = xtemp2;							\
    mArity = (int) get_arity((Psc) xtemp2);				\
    for (mj= mArity; mj >= 1; mj--) {					\
      push_addr(hreg+mj);						\
    }									\
    hreg += mArity;							\
    hreg++;								\
    rec_macro_make_heap_term(dummy_addr);				\
    break;								\
  default:								\
    xsb_abort("Bad tag in macro_make_heap_term");			\
    return;								\
  }									\
}

/*----------------------------------------------------------------------*/
/* does not save substitution factor so should not be used for copying
   in answers with delay lists.  I.e. it should be used by
   delay_chk_insert, asserting tries, and interning tries.
*/

#define recvariant_trie(flag,TrieType) {				\
  int  j;								\
									\
  while (!pdlempty ) {							\
    xtemp1 = (CPtr) pdlpop;						\
    XSB_CptrDeref(xtemp1);						\
    tag = cell_tag(xtemp1);						\
    switch (tag) {							\
    case XSB_FREE:							\
    case XSB_REF1:							\
      if (! IsStandardizedVariable(xtemp1)) {				\
	StandardizeAndTrailVariable(xtemp1,ctr);			\
	item = EncodeNewTrieVar(ctr);					\
	one_node_chk_ins(flag, item, TrieType);				\
	ctr++;								\
      } else {								\
	item = IndexOfStdVar(xtemp1);					\
	item = EncodeTrieVar(item);					\
	one_node_chk_ins(flag, item, TrieType);				\
      }									\
      break;								\
    case XSB_STRING:							\
    case XSB_INT:							\
    case XSB_FLOAT:							\
      one_node_chk_ins(flag, EncodeTrieConstant(xtemp1), TrieType);	\
      break;								\
    case XSB_LIST:							\
      one_node_chk_ins(flag, EncodeTrieList(xtemp1), TrieType);		\
      pdlpush(cell(clref_val(xtemp1)+1));				\
      pdlpush(cell(clref_val(xtemp1)));					\
      break;								\
    case XSB_STRUCT:							\
      psc = (Psc) follow(cs_val(xtemp1));				\
      item = makecs(psc);						\
      one_node_chk_ins(flag, item, TrieType);				\
      for (j = get_arity(psc); j>=1 ; j--) {				\
	pdlpush(cell(clref_val(xtemp1)+j));				\
      }									\
      break;								\
    case XSB_ATTV:							\
      /* Now xtemp1 can only be the first occurrence of an attv */	\
      xtemp1 = clref_val(xtemp1); /* the VAR part of the attv */	\
      StandardizeAndTrailVariable(xtemp1, ctr);				\
      one_node_chk_ins(flag, EncodeNewTrieAttv(ctr), INTERN_TRIE_TT);	\
      attv_ctr++; ctr++;						\
      pdlpush(cell(xtemp1+1));	/* the ATTR part of the attv */		\
      break;								\
    default:								\
      xsb_abort("Bad type tag in recvariant_trie...\n");		\
    }									\
  }									\
  resetpdl;								\
}

/*----------------------------------------------------------------------*/

/*
 * This is a special version of recvariant_trie(), and it is only used by 
 * variant_answer_search().  The only difference between this and
 * recvariant_trie() is that this version will save the answer
 * substitution factor into the heap (see the following lines):
 *
 * 	bld_free(hreg);
 * 	bind_ref(xtemp1, hreg);
 * 	xtemp1 = hreg++;
 */

#define recvariant_trie_ans_subsf(flag,TrieType) {			\
  int  j;								\
									\
  while (!pdlempty ) {							\
    xtemp1 = (CPtr) pdlpop;						\
    XSB_CptrDeref(xtemp1);						\
    tag = cell_tag(xtemp1);						\
    switch (tag) {							\
    case XSB_FREE:							\
    case XSB_REF1:							\
      if (! IsStandardizedVariable(xtemp1)){				\
	bld_free(hreg);							\
	bind_ref(xtemp1, hreg);						\
	xtemp1 = hreg++;						\
	StandardizeAndTrailVariable(xtemp1,ctr);			\
	one_node_chk_ins(flag,EncodeNewTrieVar(ctr),TrieType);		\
	ctr++;								\
      } else {								\
	one_node_chk_ins(flag,						\
			 EncodeTrieVar(IndexOfStdVar(xtemp1)),		\
			 TrieType);					\
      }									\
      break;								\
    case XSB_STRING:							\
    case XSB_INT:							\
    case XSB_FLOAT:							\
      one_node_chk_ins(flag, EncodeTrieConstant(xtemp1), TrieType);	\
      break;								\
    case XSB_LIST:							\
      one_node_chk_ins(flag, EncodeTrieList(xtemp1), TrieType);		\
      pdlpush(cell(clref_val(xtemp1)+1));				\
      pdlpush(cell(clref_val(xtemp1)));					\
      break;								\
    case XSB_STRUCT:							\
      psc = (Psc) follow(cs_val(xtemp1));				\
      item = makecs(psc);						\
      one_node_chk_ins(flag, item, TrieType);				\
      for (j = get_arity(psc); j>=1 ; j--) {				\
	pdlpush(cell(clref_val(xtemp1)+j));				\
      }									\
      break;								\
    case XSB_ATTV:							\
      /* Now xtemp1 can only be the first occurrence of an attv */	\
      /* *(hreg++) = (Cell) xtemp1;	*/				\
      xtemp1 = clref_val(xtemp1); /* the VAR part of the attv */	\
      StandardizeAndTrailVariable(xtemp1, ctr);				\
      one_node_chk_ins(flag, EncodeNewTrieAttv(ctr), TrieType);		\
      attv_ctr++; ctr++;						\
      pdlpush(cell(xtemp1+1));	/* the ATTR part of the attv */		\
      break;								\
    default:								\
      xsb_abort("Bad type tag in recvariant_trie_ans_subsf...\n");	\
    }									\
  }									\
  resetpdl;								\
}


#include "term_psc_xsb_i.h"
#include "ptoc_tag_xsb_i.h"


/*
 * Called in SLG instruction `new_answer_dealloc', variant_answer_search()
 * checks if the answer has been returned before and, if not, inserts it
 * into the answer trie.  Here, `sf_size' is the number of variables in the
 * substitution factor of the called subgoal, `attv_num' is the number of
 * attributed variables in the call, `cptr' is the pointer to the
 * substitution factor, and `subgoal_ptr' is the subgoal frame of the
 * call.  At the end of this function, `flagptr' tells if the answer
 * has been returned before.
 *
 * The returned value of this function is the leaf of the answer trie.
 */

BTNptr variant_answer_search(CTXTdeclc int sf_size, int attv_num, CPtr cptr,
			     VariantSF subgoal_ptr, xsbBool *flagptr) {

  Psc   psc;
  CPtr  xtemp1;
  int   i, j, flag = 1;
  Cell  tag = XSB_FREE, item, tmp_var;
  ALNptr answer_node;
  int ctr, attv_ctr;
  BTNptr Paren, *GNodePtrPtr;

  byte choicepttype;  /* for incremental evaluation */ 
  byte typeofinstr;   /* for incremental evaluation */ 

#ifndef MULTI_THREAD
  ans_chk_ins++; /* Counter (answers checked & inserted) */
#else
#ifdef NON_OPT_COMPILE
  ans_chk_ins++; /* Counter (answers checked & inserted) */
#endif
#endif

  VarEnumerator_trail_top = (CPtr *)(& VarEnumerator_trail[0]) - 1;
  AnsVarCtr = 0;
  ctr = 0;
  if ( IsNULL(subg_ans_root_ptr(subgoal_ptr)) ) {
    Cell retSymbol;
    if ( sf_size > 0 )
      retSymbol = EncodeTriePSC(get_ret_psc(sf_size));
    else
      retSymbol = EncodeTrieConstant(makestring(get_ret_string()));
    subg_ans_root_ptr(subgoal_ptr) =
      new_btn(CTXTc BASIC_ANSWER_TRIE_TT, TRIE_ROOT_NT, retSymbol, (BTNptr)subgoal_ptr, (BTNptr)NULL);
  }
  Paren = subg_ans_root_ptr(subgoal_ptr);
  GNodePtrPtr = &BTN_Child(Paren);

  /* Documentation rewritten by TLS: 
   * To properly generate instructions for attributed variables, you
   * need to know which attributed variables are identical to those in
   * the call, and which represent new bindings to attributed or vanilla
   * variables.  The marking below binds binds the VAR part of the
   * attvs to an element of VarEnumerator[].  When the for() loop
   * dereferences these variables they can be recognized as pointing
   * into VarEnumerator, and a trie_xxx_val instruction will be
   * generated for them.  Other attvs will dereference elsewhere and
   * will generate a trie_xxx_attv instruction.  Note that in doing
   * this, attributes in the call will not need to be re-entered in
   * the table.
   * 
   * According to Bao's algorithm, in order for trie instructions for
   * completed tables to work for attvs, attvs in the call must be
   * traversed before the main loop and bound to elements of
   * varEnumerator so that the trie_xxx_val instructions can recognize
   * them and avoid interrupts.  As a result, both here and in the tabletry
   * setup for completed tables, the substitution factor is traversed
   * and the attvs set to the lower portion of varEnumerator.  To save
   * time, this is only done when there is at least one attv in 
   * the call (attv_num > 0).  ¹
   */
  if (attv_num > 0) {
    for (i = 0; i < sf_size; i++) {
      tmp_var = cell(cptr - i);
      if (isattv(tmp_var)) {
	xtemp1 = clref_val(tmp_var); /* the VAR part */
	if (xtemp1 == (CPtr) cell(xtemp1)) { /* this attv is not changed */
	  StandardizeAndTrailVariable(xtemp1, ctr);
	}
	ctr++;
      }
    }
    /* now ctr should be equal to attv_num */
  }
  attv_ctr = attv_num;

  for (i = 0; i < sf_size; i++) {
    xtemp1 = (CPtr) (cptr - i); /* One element of VarsInCall.  It might
				 * have been bound in the answer for
				 * the call.
				 */
    XSB_CptrDeref(xtemp1);
    tag = cell_tag(xtemp1);
    switch (tag) {
    case XSB_FREE: 
    case XSB_REF1:
      if (! IsStandardizedVariable(xtemp1)) {
	/*
	 * Note that unlike variant_call_search(), vas() trails
	 * variables (by using VarEnumerator_trail_top, rather than
	 * full SLG-WAM trailing.  Thus, if this is the first
	 * occurrence of this variable, then: 
	 *
	 * 	StandardizeAndTrailVariable(xtemp1, ctr)
	 * 			||
	 * 	bld_ref(xtemp1, VarEnumerator[ctr]);
	 * 	*(++VarEnumerator_trail_top) = xtemp1
	 *
	 * By doing this, all the variables appearing in the answer
	 * are bound to elements in VarEnumerator[], and each element
	 * in VarEnumerator[] is a free variable itself.  vcs() was
	 * able to avoid the trail because all variables were placed
	 * on the substitution factor; variables encountered in an
	 * answer substitution can be anywhere on the heap.  Also
	 * note that this function uses the pdl stack rather than
	 * reg_array, as does vsc().
         * The variables will be used in 
	 * delay_chk_insert() (in function do_delay_stuff()).
	 */

#ifndef IGNORE_DELAYVAR
        bld_free(hreg); // make sure there is no pointer from heap to local stack.
	bind_ref(xtemp1, hreg);
	xtemp1 = hreg++;
#endif
	StandardizeAndTrailVariable(xtemp1,ctr);
	item = EncodeNewTrieVar(ctr);
	one_node_chk_ins(flag, item, BASIC_ANSWER_TRIE_TT);
	ctr++;
      } else {
	item = IndexOfStdVar(xtemp1);
	item = EncodeTrieVar(item);
	one_node_chk_ins(flag, item, BASIC_ANSWER_TRIE_TT);
      }
      break;
    case XSB_STRING: 
    case XSB_INT:
    case XSB_FLOAT:
      one_node_chk_ins(flag, EncodeTrieConstant(xtemp1),
		       BASIC_ANSWER_TRIE_TT);
      break;
    case XSB_LIST:
      one_node_chk_ins(flag, EncodeTrieList(xtemp1), BASIC_ANSWER_TRIE_TT);
      pdlpush(cell(clref_val(xtemp1)+1));
      pdlpush(cell(clref_val(xtemp1)));
      recvariant_trie_ans_subsf(flag, BASIC_ANSWER_TRIE_TT);
      break;
    case XSB_STRUCT:
      psc = (Psc)follow(cs_val(xtemp1));
      item = makecs(psc);
      one_node_chk_ins(flag, item, BASIC_ANSWER_TRIE_TT);
      for (j = get_arity(psc); j >= 1 ; j--) {
	pdlpush(cell(clref_val(xtemp1)+j));
      }
      recvariant_trie_ans_subsf(flag, BASIC_ANSWER_TRIE_TT);
      break;
    case XSB_ATTV:
      /* Now xtemp1 can only be the first occurrence of an attv */
      //      *(hreg++) = (Cell) xtemp1;
      xtemp1 = clref_val(xtemp1); /* the VAR part of the attv */
      /*
       * Bind the VAR part of this attv to VarEnumerator[ctr], so all the
       * later occurrences of this attv will look like a regular variable
       * (after dereferencing).
       */
      StandardizeAndTrailVariable(xtemp1, ctr);	
      one_node_chk_ins(flag, EncodeNewTrieAttv(ctr), BASIC_ANSWER_TRIE_TT);
      attv_ctr++; ctr++;
      pdlpush(cell(xtemp1+1));	/* the ATTR part of the attv */
      recvariant_trie_ans_subsf(flag, BASIC_ANSWER_TRIE_TT);
      break;
    default:
      xsb_abort("Bad type tag in variant_answer_search()");
    }                                                       
  }
  resetpdl;                                                   

#ifndef IGNORE_DELAYVAR
  /*
   * Put the substitution factor of the answer into a term ret/n (if 
   * the sf_size of the substitution factor is 0, then put integer 0
   * into cell ans_var_pos_reg).
   *
   * Notice that undo_answer_bindings in pre-1.9 version of XSB
   * has been removed here, because all the variable bindings of this
   * answer will be used in do_delay_stuff() immediatly after the
   * return of vas() when we build the delay list for this answer.
   */
  if (ctr == 0)
    bld_int(ans_var_pos_reg, 0);
  else	
    bld_functor(ans_var_pos_reg, get_ret_psc(ctr));
#else /* IGNORE_DELAYVAR */
  undo_answer_bindings(CTXT);
#endif

  /*
     * Save the number of variables in the answer, i.e. the sf_size of
     * the substitution factor of the answer, into `AnsVarCtr'.
     */
  AnsVarCtr = ctr;		

  /* if there is no term to insert, an ESCAPE node has to be created/found */

  if (sf_size == 0) {
    one_node_chk_ins(flag, ESCAPE_NODE_SYMBOL, BASIC_ANSWER_TRIE_TT);
    Instr(Paren) = trie_proceed;
  }

  /* incremental evaluation: reinserting marked deleted node*/ 
  /* If a new answer is inserted, the call is changed */
  if(IsIncrSF(subgoal_ptr)){
    if((IsNonNULL(subgoal_ptr->callnode->prev_call))&&(flag==0)){
      subgoal_ptr->callnode->prev_call->changed=1;
    }
    
    /* If the answer already exists and is marked deleted, which means
       it is generating an old answer. In this case we remove the answer
       marking and reduce no_of_answers */
    
    if((flag==1)&&(IsDeletedNode(Paren))){
      
      choicepttype = 0x3 &  BTN_Instr(Paren);
      typeofinstr = (~0x3) & BTN_Status(Paren);
      BTN_Instr(Paren) = choicepttype | typeofinstr;
      MakeStatusValid(Paren);
      
      flag=0;
      
      subgoal_ptr->callnode->prev_call->no_of_answers--;
      
      New_ALN(subgoal_ptr,answer_node,Paren,NULL);
      SF_AppendNewAnswer(subgoal_ptr,answer_node);	
      
    }else
      if ( flag == 0 ) {
	MakeLeafNode(Paren);
	TN_UpgradeInstrTypeToSUCCESS(Paren,tag);
#ifndef MULTI_THREAD
	ans_inserts++;
#else
#ifdef NON_OPT_COMPILE
	ans_inserts++;
#endif
#endif
	
	New_ALN(subgoal_ptr,answer_node,Paren,NULL);
	SF_AppendNewAnswer(subgoal_ptr,answer_node);
      }
  }else
    /* incremental eval end */
  /*
   *  If an insertion was performed, do some maintenance on the new leaf,
   *  and place the answer handle onto the answer list.
   */
  if ( flag == 0 ) {
    MakeLeafNode(Paren);
    TN_UpgradeInstrTypeToSUCCESS(Paren,tag);
#ifndef MULTI_THREAD
    ans_inserts++;
#else
#ifdef NON_OPT_COMPILE
    ans_inserts++;
#endif
#endif

    New_ALN(subgoal_ptr,answer_node,Paren,NULL);
    SF_AppendNewAnswer(subgoal_ptr,answer_node);
  }

  *flagptr = flag;	
  return Paren;
}

/*
 * Function delay_chk_insert() is called only from intern_delay_element()
 * to create the delay trie for the corresponding delay element.  This
 * delay trie contains the substitution factor of the answer to the subgoal
 * call of this delay element.  Its leaf node will be saved as a field,
 * de_subs_fact_leaf, in the delay element.
 *
 * This function is closely related to variant_answer_search(), because it
 * uses the value of AnsVarCtr that is set in variant_answer_search().  The
 * body of this function is almost the same as the core part of
 * variant_answer_search(), except that `ctr', the counter of the variables
 * in the answer, starts from AnsVarCtr.  Initially, before the first delay
 * element in the delay list of a subgoal (say p/2), is interned, AnsVarCtr
 * is the number of variables in the answer for p/2 and it was set in
 * variant_answer_search() when this answer was returned.  Then, AnsVarCtr
 * will be dynamically increased as more and more delay elements for p/2
 * are interned.
 *
 * After variant_answer_search() is finished, VarEnumerator[] contains the
 * variables in the head of the corresponding clause for p/2.  When we call
 * delay_chk_insert() to intern the delay list for p/2, VarEnumerator[]
 * will be used again to bind the variables that appear in the body.
 * Because we have to check if a variable in a delay element of p/2 is
 * already in the head, the old bindings of variables to VarEnumerator[]
 * are still needed.  So undo_answer_bindings has to be delayed.
 *
 * In the arguments, `arity' is the arity of the the answer substitution
 * factor, `cptr' points to the first field of term ret/n (the answer
 * substitution factor), `hook' is a pointer to a location where the top of
 * this delay trie will become anchored.  Since these delay "tries" only
 * occur as single paths, there is currently no need for a root node.
 */
 
BTNptr delay_chk_insert(CTXTdeclc int arity, CPtr cptr, CPtr *hook)
{
    Psc  psc;
    Cell item;
    CPtr xtemp1;
    int  i, j, tag = XSB_FREE, flag = 1;
    int ctr, attv_ctr;
    BTNptr Paren, *GNodePtrPtr;
 
    VarEnumerator_trail_top = (CPtr *)(& VarEnumerator_trail[0]) - 1;
#ifdef DEBUG_DELAYVAR
    xsb_dbgmsg((LOG_DEBUG,">>>> start delay_chk_insert()"));
#endif

    Paren = NULL;

    if (hook)
      GNodePtrPtr = (BTNptr *) hook;
    else GNodePtrPtr = (BTNptr *) NULL;

    ctr = AnsVarCtr;

#ifdef DEBUG_DELAYVAR
    printf(">>>> [D1] AnsVarCtr = %d ", AnsVarCtr);printf("\n");
#endif

    for (i = 0; i<arity; i++) {
      /*
       * Notice: the direction of saving the variables in substitution
       * factors has been changed.  Because Prasad saves the substitution
       * factors in CP stack (--VarPosReg), but I save them in heap
       * (hreg++).  So (cptr - i) is changed to (cptr + i) in the
       * following line.
       */
      xtemp1 = (CPtr) (cptr + i);
      //      fprintf(stddbg, "  delay_chk_insert arg[%d] =  %x ",i, xtemp1);
      XSB_CptrDeref(xtemp1);
      //      printterm(stddbg,(unsigned int)xtemp1,25);fprintf(stddbg,"\n");
      xsb_dbgmsg((LOG_BD, "\n"));
      tag = cell_tag(xtemp1);
      switch (tag) {
      case XSB_FREE:
      case XSB_REF1:
	if (! IsStandardizedVariable(xtemp1)) {
          StandardizeAndTrailVariable(xtemp1,ctr);
          one_node_chk_ins(flag,EncodeNewTrieVar(ctr),
			   DELAY_TRIE_TT);
          ctr++;
	  //	  fprintf(stddbg,"  standardized ");printterm(stddbg,(unsigned int)xtemp1,25);fprintf(stddbg,"\n");
        }
        else {
          one_node_chk_ins(flag,
			   EncodeTrieVar(IndexOfStdVar(xtemp1)),
			   DELAY_TRIE_TT);
        }
        break;
      case XSB_STRING: 
      case XSB_INT:
      case XSB_FLOAT:
        one_node_chk_ins(flag, EncodeTrieConstant(xtemp1), DELAY_TRIE_TT);
        break;
      case XSB_LIST:
        one_node_chk_ins(flag, EncodeTrieList(xtemp1), DELAY_TRIE_TT);
        pdlpush(cell(clref_val(xtemp1)+1));
        pdlpush(cell(clref_val(xtemp1)));
        recvariant_trie(flag,DELAY_TRIE_TT);
        break;
      case XSB_STRUCT:
        one_node_chk_ins(flag, makecs(follow(cs_val(xtemp1))),DELAY_TRIE_TT);
        for (j = get_arity((Psc)follow(cs_val(xtemp1))); j >= 1 ; j--) {
          pdlpush(cell(clref_val(xtemp1)+j));
        }
        recvariant_trie(flag,DELAY_TRIE_TT);
        break;
      case XSB_ATTV:
	//	/* Now xtemp1 can only be the first occurrence of an attv */
	//	*(hreg++) = (Cell) xtemp1;
	xtemp1 = clref_val(xtemp1); /* the VAR part of the attv */
	/*
	 * Bind the VAR part of this attv to VarEnumerator[ctr], so all the
	 * later occurrences of this attv will look like a regular variable
	 * (after dereferencing).
	 */
	if (! IsStandardizedVariable(xtemp1)) {
	  StandardizeAndTrailVariable(xtemp1, ctr);	
	  one_node_chk_ins(flag, EncodeNewTrieAttv(ctr), DELAY_TRIE_TT);
          ctr++; attv_ctr++;
        }
        else {
          one_node_chk_ins(flag,
			   EncodeTrieVar(IndexOfStdVar(xtemp1)),
			   DELAY_TRIE_TT);
        }
	pdlpush(cell(xtemp1+1));	/* the ATTR part of the attv */
	recvariant_trie(flag, DELAY_TRIE_TT);
	break;
      default:
          xsb_abort("Bad type tag in delay_chk_insert()\n");
        }
    }
    resetpdl;  
    AnsVarCtr = ctr;

#ifdef DEBUG_DELAYVAR
    xsb_dbgmsg((LOG_DEBUG,">>>> [D2] AnsVarCtr = %d", AnsVarCtr));
#endif

    /*
     *  If an insertion was performed, do some maintenance on the new leaf.
     */
    if ( flag == 0 ) {
      MakeLeafNode(Paren);
      TN_UpgradeInstrTypeToSUCCESS(Paren,tag);
    }
 
    xsb_dbgmsg((LOG_BD, "----------------------------- Exit\n"));
    return Paren;
}

/*----------------------------------------------------------------------*/
/* for each variable in call, builds its binding on the heap.		*/
/*----------------------------------------------------------------------*/

/*
 * Expects that the path in the trie -- to which the variables (stored in
 * the vector `cptr') are to be unified -- has been pushed onto the
 * termstack.
 */
static void load_solution_from_trie(CTXTdeclc int arity, CPtr cptr)
{
   int i;
   CPtr xtemp1, Dummy_Addr;
   Cell returned_val, xtemp2;

   for (i=0; i<arity; i++) {
     xtemp1 = (CPtr) (cptr-i);
     XSB_CptrDeref(xtemp1);
     macro_make_heap_term(xtemp1,returned_val,Dummy_Addr);
     if (xtemp1 != (CPtr)returned_val) {
       if (isref(xtemp1)) {	/* a regular variable */
	 dbind_ref(xtemp1,returned_val);
       }
       else {			/* an XSB_ATTV */
	 /* Bind the variable part of xtemp1 to returned_val */
	 add_interrupt(CTXTc cell(((CPtr)dec_addr(xtemp1) + 1)), returned_val); 
	 dbind_ref((CPtr) dec_addr(xtemp1), returned_val);
       }
     }
   }
   resetpdl;
}

/*----------------------------------------------------------------------*/

/*
 * Unifies the path in the interned trie identified by `Leaf' with the term
 * `term'.  It appears that `term' is expected to be an unbound variable.
 * Also, `Root' does not appear to be used.
 */
static void bottomupunify(CTXTdeclc Cell term, BTNptr Root, BTNptr Leaf)
{
  CPtr Dummy_Addr;
  Cell returned_val, xtemp2;
  CPtr gen;
  int  i, heap_needed;


  num_heap_term_vars = 0;     
  heap_needed = follow_par_chain(CTXTc Leaf);
  if (glstack_overflow(heap_needed*sizeof(Cell))) {
    reg[1] = term;
    check_glstack_overflow(1,pcreg,heap_needed*sizeof(Cell));
    term = reg[1];
  }
  XSB_Deref(term);
  gen = (CPtr) term;
  macro_make_heap_term(gen,returned_val,Dummy_Addr);
  bld_ref(gen,returned_val);

  for(i = 0; i < num_heap_term_vars; i++){
    var_regs[i] = var_addr[i];
  }
  /*
   * global_num_vars is needed by get_lastnode_cs_retskel() (see
   * trie_interned/4 in intern.P).
   *
   * Last_Nod_Sav is also needed by get_lastnode_cs_retskel().  We can
   * set it to Leaf.
   */
  global_num_vars = num_vars_in_var_regs = num_heap_term_vars - 1;
  Last_Nod_Sav = Leaf;
}

/*----------------------------------------------------------------------*/

/*
 *  Used with tries created via the builtin trie_intern, to access a
 *  trie from a leaf.  Not yet extended to shared tries.
 */

xsbBool bottom_up_unify(CTXTdecl)
{
  Cell    term;
  BTNptr root;
  BTNptr leaf;
  int     rootidx;

  leaf = (BTNptr) ptoc_int(CTXTc 3);   
  if( IsDeletedNode(leaf) )
    return FALSE;

  term    = ptoc_tag(CTXTc 1);
  rootidx = ptoc_int(CTXTc 2);
  root    = itrie_array[rootidx].root;  
  bottomupunify(CTXTc term, root, leaf);
  return TRUE;
}

/*----------------------------------------------------------------------*/

void handle_heap_overflow_trie(CTXTdeclc CPtr *cptr, int arity, int heap_needed) {
  int i;

  if (*cptr > (CPtr)glstack.low && *cptr < hreg) {
    if (arity <= 254) {
      reg[1] = (Cell)*cptr;
      for (i = 1; i <= arity; i++) reg[i+1] = *(*cptr-arity+i);
      check_glstack_overflow(arity+1,pcreg,heap_needed*sizeof(Cell));
      *cptr = (CPtr)reg[1];
      for (i = 1; i <= arity; i++) *(*cptr-arity+i) = reg[i+1];
    } else xsb_abort("Heap overflow; Cannot garbage collect\n");
    /* if this happens, change to do stack_realloc only */
  } else if (arity <= 255) {
    for (i = 1; i <= arity; i++) reg[i] = *(*cptr-arity+i);
    check_glstack_overflow(arity,pcreg,heap_needed*sizeof(Cell));
    for (i = 1; i <= arity; i++) *(*cptr-arity+i) = reg[i];
  } else xsb_abort("Heap overflow; Cannot garbage collect\n");
}


/*
 * `TriePtr' is a leaf in the answer trie, and `cptr' is a vector of
 * variables for receiving the substitution.
 * 
 * TLS: 09/11/15
 * In addition to providing space to hold an answer, we need to ensure space to 
 * delay the predicate if needed.  I'm taking a guess: we need up to 255 words to hold
 * the variables of the predicate, plus a few words for the delay element.  Rounding it up, 
 * and multipling by 8 bytes per word (in case we're in 64-bit mode) moves it up to 2400
 * 
 */
#define DELAYING_FUDGE_FACTOR 2400

void load_solution_trie(CTXTdeclc int arity, int attv_num, CPtr cptr, BTNptr TriePtr)
{
  CPtr xtemp;
  int heap_needed;
  
  num_heap_term_vars = 0;
  if (arity > 0) {
    /* Initialize var_addr[] as the attvs in the call. */
    if (attv_num > 0) {
      for (xtemp = cptr; xtemp > cptr - arity; xtemp--) {
	if (isattv(cell(xtemp))) {
	  //	  var_addr[num_heap_term_vars] = (CPtr) cell(xtemp);
	  safe_assign(var_addr,num_heap_term_vars,(CPtr) cell(xtemp),var_addr_arraysz);
	  num_heap_term_vars++;
	}
      }
    }
    heap_needed = follow_par_chain(CTXTc TriePtr);
    if (glstack_overflow(heap_needed*sizeof(Cell))) {
      printf("stack overflow could cause problems for delay lists\n");
      handle_heap_overflow_trie(CTXTc &cptr,arity,heap_needed);
    }
    load_solution_from_trie(CTXTc arity,cptr);
  }
}

/*----------------------------------------------------------------------*/

void load_delay_trie(CTXTdeclc int arity, CPtr cptr, BTNptr TriePtr)
{
   if (arity) {
     int heap_needed;
     heap_needed = follow_par_chain(CTXTc TriePtr);
     if (glstack_overflow(heap_needed*sizeof(Cell))) 
       handle_heap_overflow_trie(CTXTc &cptr,arity,heap_needed);
     load_solution_from_trie(CTXTc arity,cptr);
   }
}
 
/*----------------------------------------------------------------------*/

#define recvariant_call(flag,TrieType,xtemp1) {				\
  int  j;								\
									\
  while (!pdlempty) {							\
    xtemp1 = (CPtr) pdlpop;						\
    XSB_CptrDeref(xtemp1);						\
    switch(tag = cell_tag(xtemp1)) {					\
    case XSB_FREE:							\
    case XSB_REF1:							\
      if (! IsStandardizedVariable(xtemp1)) {				\
	*(--VarPosReg) = (Cell) xtemp1;					\
	StandardizeVariable(xtemp1,ctr);				\
	one_node_chk_ins(flag,EncodeNewTrieVar(ctr),TrieType);		\
	ctr++;								\
      } else{								\
	one_node_chk_ins(flag, EncodeTrieVar(IndexOfStdVar(xtemp1)),	\
			 TrieType);					\
      }									\
      break;								\
    case XSB_STRING:							\
    case XSB_INT:							\
    case XSB_FLOAT:							\
      one_node_chk_ins(flag, EncodeTrieConstant(xtemp1), TrieType);	\
      break;								\
    case XSB_LIST:							\
      one_node_chk_ins(flag, EncodeTrieList(xtemp1), TrieType);		\
      pdlpush( cell(clref_val(xtemp1)+1) );				\
      pdlpush( cell(clref_val(xtemp1)) );				\
      break;								\
    case XSB_STRUCT:							\
      psc = (Psc) follow(cs_val(xtemp1));				\
      item = makecs(psc);						\
      one_node_chk_ins(flag, item, TrieType);				\
      for (j=get_arity(psc); j>=1; j--) {				\
	pdlpush(cell(clref_val(xtemp1)+j));				\
      }									\
      break;								\
    case XSB_ATTV:							\
      /* Now xtemp1 can only be the first occurrence of an attv */	\
      *(--VarPosReg) = (Cell) xtemp1;					\
      xtemp1 = clref_val(xtemp1); /* the VAR part of the attv */	\
      StandardizeVariable(xtemp1, ctr);					\
      one_node_chk_ins(flag, EncodeNewTrieAttv(ctr), TrieType);		\
      attv_ctr++; ctr++;						\
      pdlpush(cell(xtemp1+1));	/* the ATTR part of the attv */		\
      break;								\
    default:								\
      xsb_abort("Bad type tag in recvariant_call...\n");		\
    }									\
  }									\
  resetpdl;								\
}

/*----------------------------------------------------------------------*/

/* TLS gloss: 
 * 
 * To me it seems this function is written in an overly general way.
 * I dont see a real need to encapsulate all of its input and output
 * as it is only called once, in tabletry.  What's lost is: 
 * cptr is simply a pointer to the reg_array, cptr = reg+1 
 * VarPosReg is top_of_cpstack.  This can cause confusion since later in
 * table_call_search (which calls this function) the substitution
 * factor is copied to the heap.
 * 
 * In addition, the manner in which attributed variables are handled
 * gives rise to some special features in the code.  When adding an
 * answer, it is not straightforward to determine whether a binding
 * to a substitution factor was made in the original call or as part
 * of program clause resolution.  variant_call_search() creates a
 * substitution factor on the choice point stack.  Immediately after
 * variant_call_search() returns, table_call_search() will copy the
 * substitution factor from the choice point stack to the heap.  It
 * can then be determined whether attributed variables are old or new
 * by comparing the value of a cell in the choice point stack to the
 * corresponding value in the heap.  If they are the same, the
 * attributed variable was in the call, and a trie_xxx_val
 * instruction can be used.  If not, other actions must be taken --
 * generating either a trie_xxx_val or trie_xxx_attv.
 * 
 * While most of this happens in later functions, certain
 * features of vcs() can be accounted for by these later actions.
 * For instance, each local variable is copied to the heap in vcs().
 * This is to avoid pointers from the heap substitution factor (once
 * it is created) into the local stack.
 *
 */

/*
 * Searches/inserts a subgoal call structure into a subgoal call trie.
 * During search/insertion, the variables of the subgoal call are
 * pushed on top of the CP stack (through VarPosReg), along with the #
 * of variables that were pushed.  This forms the substitution factor.
 * Prolog variables are standardized during this process to recognize
 * multiple (nonlinear) occurences.  They must be reset to an unbound
 * state before termination.
 * 
 * Important variables: 
 * Paren - to be set to point to inserted term's leaf
 * VarPosReg - pointer to top of CPS; place to put the substitution factor
 *    in high-to-low memory format.
 * GNodePtrPtr - Points to the parent-internal-structure's
 *    "child" or "NODE_link" field.  It's a place to anchor any newly
 *    created NODEs.
 * ctr - contains the number of distinct variables found
 *    in the call.
 * Pay careful attention to the expected argument vector accepted by this
 * function.  It actually points one Cell *before* the term vector!  Notice
 * the treatment of "cptr" as these terms are inspected.
 */

void variant_call_search(CTXTdeclc TabledCallInfo *call_info,
			 CallLookupResults *results)
{
  Psc  psc;
  CPtr call_arg;
  int  arity, i, j, flag = 1;
  Cell tag = XSB_FREE, item;
  CPtr cptr, VarPosReg, tVarPosReg;
  int ctr, attv_ctr;
  BTNptr Paren, *GNodePtrPtr;

#ifndef MULTI_THREAD
  subg_chk_ins++;
#else
#ifdef NON_OPT_COMPILE
  subg_chk_ins++;
#endif
#endif
  Paren = TIF_CallTrie(CallInfo_TableInfo(*call_info));
  GNodePtrPtr = &BTN_Child(Paren);
  arity = CallInfo_CallArity(*call_info);
  /* cptr is set to point to the reg_array */
  cptr = CallInfo_Arguments(*call_info);
  tVarPosReg = VarPosReg = CallInfo_VarVectorLoc(*call_info);
  ctr = attv_ctr = 0;

  for (i = 0; i < arity; i++) {
    xsb_dbgmsg((LOG_DEBUG,">>>> (argument %d)",i+1));
    call_arg = (CPtr) (cptr + i);            /* Note! */
    XSB_CptrDeref(call_arg);
    tag = cell_tag(call_arg);
    switch (tag) {
    case XSB_FREE:
    case XSB_REF1:
      if (! IsStandardizedVariable(call_arg)) {

	/* Call_arg is now a dereferenced register value.  If it
	 * points to a local variable, make both the local variable
	 * and call_arg point to a new free variable in the heap.
	 * As noted in the documentation at the start of this function,
	 * this is to support attributed variables in tabling.   
	 */

	xsb_dbgmsg((LOG_DEBUG,"   new variable ctr = %d)",ctr));

	if (top_of_localstk <= call_arg &&
	    call_arg <= (CPtr) glstack.high - 1) {
	  bld_free(hreg);
	  bind_ref(call_arg, hreg);
	  call_arg = hreg++;
	}
	/*
	 * Make VarPosReg, which points to the top of the choice point
	 * stack, point to call_arg, which now points a free variable in the
	 * heap.  Make that heap free variable point to the
	 * VarEnumerator array, via StandardizeVariable.   The
	 * VarEnumerator array contains variables that point to
	 * themselves (init'd in init_trie_aux_areas()).  vcs() does
	 * not change bindings in the VarEnumerator array -- it just
	 * changes bindings of heap variables that point into it.
         */
	*(--VarPosReg) = (Cell) call_arg;	
	StandardizeVariable(call_arg,ctr);
	one_node_chk_ins(flag,EncodeNewTrieVar(ctr),
			 CALL_TRIE_TT);
	ctr++;
      } else {
	one_node_chk_ins(flag,EncodeTrieVar(IndexOfStdVar(call_arg)),CALL_TRIE_TT);
      }
      break;
    case XSB_STRING:
    case XSB_INT:
    case XSB_FLOAT:
      one_node_chk_ins(flag, EncodeTrieConstant(call_arg), CALL_TRIE_TT);
      break;
    case XSB_LIST:
      one_node_chk_ins(flag, EncodeTrieList(call_arg), CALL_TRIE_TT);
      pdlpush(cell(clref_val(call_arg)+1));
      pdlpush(cell(clref_val(call_arg)));
      recvariant_call(flag,CALL_TRIE_TT,call_arg);
      break;
    case XSB_STRUCT:
      psc = (Psc)follow(cs_val(call_arg));
      item = makecs(psc);
      one_node_chk_ins(flag, item, CALL_TRIE_TT);
      for (j=get_arity(psc); j>=1 ; j--) {
	pdlpush(cell(clref_val(call_arg)+j));
      }
      recvariant_call(flag,CALL_TRIE_TT,call_arg);
      break;
    case XSB_ATTV:
      /* call_arg is derefed register value pointing to heap.  Make
	 the subst factor CP-stack pointer, VarPosReg, point to it. */
      *(--VarPosReg) = (Cell) call_arg;
      xsb_dbgmsg((LOG_TRIE,"In VSC: attv deref'd reg %x; val: %x into AT: %x",
		 call_arg,clref_val(call_arg),VarPosReg));
      call_arg = clref_val(call_arg); /* the VAR part of the attv */
      /*
       * Bind the VAR part of this attv to VarEnumerator[ctr], so all the
       * later occurrences of this attv will look like a regular variable
       * (after dereferencing).
       */
      StandardizeVariable(call_arg, ctr);	
      one_node_chk_ins(flag, EncodeNewTrieAttv(ctr), CALL_TRIE_TT);
      attv_ctr++; ctr++;
      pdlpush(cell(call_arg+1));	/* the ATTR part of the attv */
      recvariant_call(flag, CALL_TRIE_TT, call_arg);
      break;
    default:
      xsb_abort("Bad type tag in variant_call_search...\n");
    }
  }
  resetpdl;
    
  if (arity == 0) {
    one_node_chk_ins(flag, ESCAPE_NODE_SYMBOL, CALL_TRIE_TT);
    Instr(Paren) = trie_proceed;
  }

  /*
   *  If an insertion was performed, do some maintenance on the new leaf.
   */
  if ( flag == 0 ) {
#ifndef MULTI_THREAD
    subg_inserts++;
#else
#ifdef NON_OPT_COMPILE
    subg_inserts++;
#endif
#endif
    MakeLeafNode(Paren);
    TN_UpgradeInstrTypeToSUCCESS(Paren,tag);
  }

  cell(--VarPosReg) = makeint(attv_ctr << 16 | ctr);
  /* 
   * "Untrail" any variable that used to point to VarEnumerator.  For
   * variables, note that *VarPosReg is the address of a cell in the
   * heap.  To reset that variable, we make that address free.
   * Similarly, *VarPosReg may contain the (encoded) address of an
   * attv on the heap.  In this case, we make the VAR part of that
   * attv point to itself.  The actual value in VarPosReg (i.e. the
   * of a substitution factor) doesn't change in either case.
   */     
  while (--tVarPosReg > VarPosReg) {
    if (isref(*tVarPosReg))	/* a regular variable */
      ResetStandardizedVariable(*tVarPosReg);
    else			/* an XSB_ATTV */
      ResetStandardizedVariable(clref_val(*tVarPosReg));
  }

  CallLUR_Leaf(*results) = Paren;
  CallLUR_Subsumer(*results) = CallTrieLeaf_GetSF(Paren);
  CallLUR_VariantFound(*results) = flag;
  CallLUR_VarVector(*results) = VarPosReg;
  return;
}

/*----------------------------------------------------------------------*/

/* TLS: this should be used only for subsumptive tables.  And even so,
   I think it could probably be replaced by some other deletion
   routine in tr_utils.c (?) */

static void remove_calls_and_returns(CTXTdeclc VariantSF CallStrPtr)
{
  ALNptr pALN;

  /* Delete the call entry
     --------------------- */
  SET_TRIE_ALLOCATION_TYPE_SF(CallStrPtr);
  delete_branch(CTXTc subg_leaf_ptr(CallStrPtr),
		&TIF_CallTrie(subg_tif_ptr(CallStrPtr)),VARIANT_EVAL_METHOD);

  /* Delete its answers
     ------------------ */
  for ( pALN = subg_answers(CallStrPtr);  IsNonNULL(pALN); pALN = ALN_Next(pALN) )
    delete_branch(CTXTc ALN_Answer(pALN), &subg_ans_root_ptr(CallStrPtr),SUBSUMPTIVE_EVAL_METHOD);

  /* Delete the table entry
     ---------------------- */
  free_answer_list(CallStrPtr);
  FreeProducerSF(CallStrPtr);
}

void remove_incomplete_tries(CTXTdeclc CPtr bottom_parameter)
{
  xsbBool warned = FALSE;
  VariantSF CallStrPtr;
  TIFptr tif;

  while (openreg < bottom_parameter) {
    CallStrPtr = (VariantSF)compl_subgoal_ptr(openreg);
    if (!is_completed(CallStrPtr)) {

      if (warned == FALSE) {
	xsb_mesg("Removing incomplete tables...");
	//	check_table_cut = FALSE;  /* permit cuts over tables */
	warned = TRUE;
      }
      if (IsVariantSF(CallStrPtr)) {
	SET_TRIE_ALLOCATION_TYPE_SF(CallStrPtr); // set smBTN to private/shared
	tif = subg_tif_ptr(CallStrPtr);
	delete_branch(CTXTc CallStrPtr->leaf_ptr, &tif->call_trie,VARIANT_EVAL_METHOD); /* delete call */
	delete_variant_sf_and_answers(CTXTc CallStrPtr,FALSE); // delete answers + subgoal
      } else remove_calls_and_returns(CTXTc CallStrPtr);
    }
    openreg += COMPLFRAMESIZE;
  }
}

/*----------------------------------------------------------------------*/
/*
 * For creating interned tries via buitin "trie_intern".
 * These differ from the trie inserts used by tables because there may
 * be failure continuations pointing into the trie -- here we need to
 * take these continuations into account before we hash.  Otherwise
 * they are identical to their non interned versions.
 */

#define one_interned_node_chk_ins(Found,item,TrieType)		{	\
									\
   int count = 0;							\
   BTNptr LocalNodePtr;							\
									\
   TRIE_W_LOCK();							\
   if ( IsNULL(*GNodePtrPtr) ) {					\
     New_BTN(LocalNodePtr,TrieType,INTERIOR_NT,item,Paren,NULL);	\
     *GNodePtrPtr = Paren = LocalNodePtr;				\
     Found = 0;								\
   }									\
   else if ( IsHashHeader(*GNodePtrPtr) ) {				\
     BTHTptr ht = (BTHTptr)*GNodePtrPtr;				\
     GNodePtrPtr = CalculateBucketForSymbol(ht,item);			\
     IsInsibling(*GNodePtrPtr,count,Found,item,TrieType);		\
     if (!Found) {							\
       MakeHashedNode(LocalNodePtr);					\
       BTHT_NumContents(ht)++;						\
       Interned_TrieHT_ExpansionCheck(ht,count);					\
     }									\
   }									\
   else {								\
     BTNptr pParent = Paren;						\
     IsInsibling(*GNodePtrPtr,count,Found,item,TrieType);		\
     if (IsLongSiblingChain(count)) {					\
       if (cps_check_flag == CPS_CHECK)					\
	 expand_flag = interned_trie_cps_check(CTXTc *hook);		\
       if (expand_flag == EXPAND_HASHES) {				\
	 /* printf("hashing intrned node \n");*/			\
	 hashify_children(CTXTc pParent,TrieType);			\
       }								\
     }									\
   }									\
       /* used to pass in GNodePtrPtr (ptr to hook) */			\
   GNodePtrPtr = &(BTN_Child(LocalNodePtr));				\
   TRIE_W_UNLOCK();							\
}

#define recvariant_trie_intern(flag,TrieType) {				\
  int  j;								\
									\
  while (!pdlempty ) {							\
    xtemp1 = (CPtr) pdlpop;						\
    XSB_CptrDeref(xtemp1);						\
    tag = cell_tag(xtemp1);						\
    switch (tag) {							\
    case XSB_FREE:							\
    case XSB_REF1:							\
      if (! IsStandardizedVariable(xtemp1)) {				\
	StandardizeAndTrailVariable(xtemp1,ctr);			\
	item = EncodeNewTrieVar(ctr);					\
	one_interned_node_chk_ins(flag, item, TrieType);				\
	ctr++;								\
      } else {								\
	item = IndexOfStdVar(xtemp1);					\
	item = EncodeTrieVar(item);					\
	one_interned_node_chk_ins(flag, item, TrieType);				\
      }									\
      break;								\
    case XSB_STRING:							\
    case XSB_INT:							\
    case XSB_FLOAT:							\
      one_interned_node_chk_ins(flag, EncodeTrieConstant(xtemp1), TrieType);	\
      break;								\
    case XSB_LIST:							\
      one_interned_node_chk_ins(flag, EncodeTrieList(xtemp1), TrieType);		\
      pdlpush(cell(clref_val(xtemp1)+1));				\
      pdlpush(cell(clref_val(xtemp1)));					\
      break;								\
    case XSB_STRUCT:							\
      psc = (Psc) follow(cs_val(xtemp1));				\
      item = makecs(psc);						\
      one_interned_node_chk_ins(flag, item, TrieType);				\
      for (j = get_arity(psc); j>=1 ; j--) {				\
	pdlpush(cell(clref_val(xtemp1)+j));				\
      }									\
      break;								\
    case XSB_ATTV:							\
      /* Now xtemp1 can only be the first occurrence of an attv */	\
      xtemp1 = clref_val(xtemp1); /* the VAR part of the attv */	\
      StandardizeAndTrailVariable(xtemp1, ctr);				\
      one_interned_node_chk_ins(flag, EncodeNewTrieAttv(ctr), INTERN_TRIE_TT);	\
      attv_ctr++; ctr++;						\
      pdlpush(cell(xtemp1+1));	/* the ATTR part of the attv */		\
      break;								\
    default:								\
      xsb_abort("Bad type tag in recvariant_trie...\n");		\
    }									\
  }									\
  resetpdl;								\
}

BTNptr whole_term_chk_ins(CTXTdeclc Cell term, BTNptr *hook, int *flagptr, int cps_check_flag, int expand_flag)
{
    Psc  psc;
    CPtr xtemp1;
    int  j, flag = 1;
    Cell tag = XSB_FREE, item;
    int ctr, attv_ctr;
    BTNptr Paren, *GNodePtrPtr;


    if ( IsNULL(*hook) )
      *hook = newBasicTrie(CTXTc EncodeTriePSC(get_intern_psc()),INTERN_TRIE_TT);
    Paren = *hook;
    GNodePtrPtr = &BTN_Child(Paren);

    xtemp1 = (CPtr) term;
    XSB_CptrDeref(xtemp1);
    tag = cell_tag(xtemp1);

    VarEnumerator_trail_top = (CPtr *)(& VarEnumerator_trail[0]) - 1;
    ctr = attv_ctr = 0;

    switch (tag) {
    case XSB_FREE: 
    case XSB_REF1:
      if (! IsStandardizedVariable(xtemp1)) {
	StandardizeAndTrailVariable(xtemp1,ctr);
	one_interned_node_chk_ins(flag,EncodeNewTrieVar(ctr),
			 INTERN_TRIE_TT);
	ctr++;
      } else {
	one_interned_node_chk_ins(flag,
			 EncodeTrieVar(IndexOfStdVar(xtemp1)),
			 INTERN_TRIE_TT);
      }
      break;
    case XSB_STRING: 
    case XSB_INT:
    case XSB_FLOAT:
      one_interned_node_chk_ins(flag, EncodeTrieConstant(xtemp1), INTERN_TRIE_TT);
      break;
    case XSB_LIST:
      one_interned_node_chk_ins(flag, EncodeTrieList(xtemp1), INTERN_TRIE_TT);
      pdlpush(cell(clref_val(xtemp1)+1));
      pdlpush(cell(clref_val(xtemp1)));
      recvariant_trie_intern(flag,INTERN_TRIE_TT);
      break;
    case XSB_STRUCT:
      one_interned_node_chk_ins(flag, makecs(follow(cs_val(xtemp1))),INTERN_TRIE_TT);
      for (j = get_arity((Psc)follow(cs_val(xtemp1))); j >= 1 ; j--) {
	pdlpush(cell(clref_val(xtemp1)+j));
      }
      recvariant_trie_intern(flag,INTERN_TRIE_TT);
      break;
    case XSB_ATTV:
      /* Now xtemp1 can only be the first occurrence of an attv */
      xtemp1 = clref_val(xtemp1); /* the VAR part of the attv */
      /*
       * Bind the VAR part of this attv to VarEnumerator[ctr], so all the
       * later occurrences of this attv will look like a regular variable
       * (after dereferencing).
       */
      StandardizeAndTrailVariable(xtemp1, ctr);	
      one_interned_node_chk_ins(flag, EncodeNewTrieAttv(ctr), INTERN_TRIE_TT);
      attv_ctr++; ctr++;
      pdlpush(cell(xtemp1+1));	/* the ATTR part of the attv */
      recvariant_trie_intern(flag, INTERN_TRIE_TT);
      break;
    default:
      xsb_abort("Bad type tag in whole_term_chk_ins()");
    }

    /*
     *  If an insertion was performed, do some maintenance on the new leaf.
     */
    if ( flag == 0 ) {
      MakeLeafNode(Paren);
      TN_UpgradeInstrTypeToSUCCESS(Paren,tag);
    }

    /*
     * var_regs[] is used to construct the last argument of trie_intern/5
     * (Skel).  This is done in construct_ret(), which is called in
     * get_lastnode_cs_retskel().
     */
    for (j = 0; j < ctr; j++) var_regs[j] = VarEnumerator_trail[j];
    /*
     * Both global_num_vars and Last_Nod_Sav are needed by
     * get_lastnode_cs_retskel() (see trie_intern/5 in intern.P).
     */
    global_num_vars = num_vars_in_var_regs = ctr - 1;
    Last_Nod_Sav = Paren;
    undo_answer_bindings;

    /* if node was deleted, then return 0 to indicate that the insertion took
       place conceptually (even if not physically */
    if (IsDeletedNode(Paren)) {
      *flagptr = 0;
      undelete_branch(Paren);
    } else
      *flagptr = flag;

    return(Paren);
}

/*----------------------------------------------------------------------*/
/* one_term_chk_ins(termptr,hook,flag)					*/
/*----------------------------------------------------------------------*/

/*
 * For creating asserted tries with builtin "trie_assert".
 */

BTNptr one_term_chk_ins(CTXTdeclc CPtr termptr, BTNptr root, int *flagptr)
{
  int  arity;
  CPtr cptr;
  CPtr xtemp1;
  int  i, j, flag = 1;
  Cell tag = XSB_FREE, item;
  Psc  psc;
  int ctr, attv_ctr;
  BTNptr Paren, *GNodePtrPtr;

  psc = term_psc((prolog_term)termptr);
  arity = get_arity(psc);
  cptr = (CPtr)cs_val(termptr);

  VarEnumerator_trail_top = (CPtr *)(& VarEnumerator_trail[0]) - 1;
  ctr = attv_ctr = 0;
  /*
   * The value of `Paren' effects the "body" of the trie: nodes which
   * are created the first level down get this value in their parent
   * field.  This could be a problem when deleting trie paths, as this
   * root needs to persist beyond the life of its body.
   */
  Paren = root;
  GNodePtrPtr = &BTN_Child(root);
  for (i = 1; i<=arity; i++) {
    xtemp1 = (CPtr) (cptr + i);
    XSB_CptrDeref(xtemp1);
    tag = cell_tag(xtemp1);
    switch (tag) {
    case XSB_FREE: 
    case XSB_REF1:
      if (! IsStandardizedVariable(xtemp1)) {
	StandardizeAndTrailVariable(xtemp1,ctr);
	one_node_chk_ins(flag, EncodeNewTrieVar(ctr),
			 ASSERT_TRIE_TT);
	ctr++;
      } else {
	one_node_chk_ins(flag,
			 EncodeTrieVar(IndexOfStdVar(xtemp1)),
			 ASSERT_TRIE_TT);
      }
      break;
    case XSB_STRING: 
    case XSB_INT:
    case XSB_FLOAT:
      one_node_chk_ins(flag, EncodeTrieConstant(xtemp1), ASSERT_TRIE_TT);
      break;
    case XSB_LIST:
      one_node_chk_ins(flag, EncodeTrieList(xtemp1), ASSERT_TRIE_TT);
      pdlpush(cell(clref_val(xtemp1)+1));
      pdlpush(cell(clref_val(xtemp1)));
      recvariant_trie(flag,ASSERT_TRIE_TT);
      break;
    case XSB_STRUCT:
      psc = (Psc) follow(cs_val(xtemp1));
      one_node_chk_ins(flag, makecs(psc),ASSERT_TRIE_TT);
      for (j = get_arity(psc); j >= 1 ; j--) {
	pdlpush(cell(clref_val(xtemp1)+j));
      }
      recvariant_trie(flag,ASSERT_TRIE_TT);
      break;
    case XSB_ATTV:
      /* Now xtemp1 can only be the first occurrence of an attv */
      xtemp1 = clref_val(xtemp1); /* the VAR part of the attv */
      /*
       * Bind the VAR part of this attv to VarEnumerator[ctr], so all the
       * later occurrences of this attv will look like a regular variable
       * (after dereferencing).
       */
      StandardizeAndTrailVariable(xtemp1, ctr);	
      one_node_chk_ins(flag, EncodeNewTrieAttv(ctr), ASSERT_TRIE_TT);
      attv_ctr++; ctr++;
      pdlpush(cell(xtemp1+1));	/* the ATTR part of the attv */
      recvariant_trie(flag, ASSERT_TRIE_TT);
      break;
    default:
      xsb_abort("Bad type tag in one_term_check_ins()");
    }
  }                
  resetpdl;                                                   

  undo_answer_bindings;

  /* if there is no term to insert, an ESCAPE node has to be created/found */

  if (arity == 0) {
    one_node_chk_ins(flag, ESCAPE_NODE_SYMBOL, ASSERT_TRIE_TT);
    Instr(Paren) = trie_proceed;
  }

  /*
   *  If an insertion was performed, do some maintenance on the new leaf.
   */
  if ( flag == 0 ) {
    MakeLeafNode(Paren);
    TN_UpgradeInstrTypeToSUCCESS(Paren,tag);
  }

  *flagptr = flag;	
  return(Paren);
}

/*----------------------------------------------------------------------*/

/*
 * This is builtin #150: TRIE_GET_RETURN
 */

byte *trie_get_returns(CTXTdeclc VariantSF sf, Cell retTerm) {

  BTNptr ans_root_ptr;
  Cell retSymbol;
#ifdef MULTI_THREAD_RWL
   CPtr tbreg;
#ifdef SLG_GC
   CPtr old_cptop;
#endif
#endif


#ifdef DEBUG_DELAYVAR
  xsb_dbgmsg((LOG_DEBUG,">>>> (at the beginning of trie_get_returns"));
  xsb_dbgmsg((LOG_DEBUG,">>>> num_vars_in_var_regs = %d)", num_vars_in_var_regs));
#endif

  if ( IsProperlySubsumed(sf) )
    ans_root_ptr = subg_ans_root_ptr(conssf_producer(sf));
  else
    ans_root_ptr = subg_ans_root_ptr(sf);
  if ( IsNULL(ans_root_ptr) )
    return (byte *)&fail_inst;

  if ( isconstr(retTerm) )
    retSymbol = EncodeTrieFunctor(retTerm);  /* ret/n rep as XSB_STRUCT */
  else
    retSymbol = retTerm;   /* ret/0 would be represented as a XSB_STRING */
  if ( retSymbol != BTN_Symbol(ans_root_ptr) )
    return (byte *)&fail_inst;

  num_vars_in_var_regs = -1;
  if ( isconstr(retTerm) ) {
    int i, arity;
    CPtr cptr;

    arity = get_arity(get_str_psc(retTerm));
    /* Initialize var_regs[] as the attvs in the call. */
    for (i = 0, cptr = clref_val(retTerm) + 1;  i < arity;  i++, cptr++) {
      if (isattv(cell(cptr)))
	var_regs[++num_vars_in_var_regs] = (CPtr) cell(cptr);
    }
    /* now num_vars_in_var_regs should be attv_num - 1 */

    reg_arrayptr = reg_array -1;
    for (i = arity, cptr = clref_val(retTerm);  i >= 1;  i--) {
      push_reg_array(cell(cptr+i));
    }
  }
#ifdef DEBUG_DELAYVAR
  xsb_dbgmsg((LOG_DEBUG,">>>> The end of trie_get_returns ==> go to answer trie"));
#endif
  delay_it = 0;  /* Don't delay the answer. */
#ifdef MULTI_THREAD_RWL
/* save choice point for trie_unlock instruction */
//       save_find_locx(ereg);
//       tbreg = top_of_cpstack;
#ifdef SLG_GC
       old_cptop = tbreg;
#endif
       save_choicepoint(tbreg,ereg,(byte *)&trie_fail_unlock_inst,breg);
#ifdef SLG_GC
       cp_prevtop(tbreg) = old_cptop;
#endif
       breg = tbreg;
       hbreg = hreg;
#endif
  return (byte *)ans_root_ptr;
}

/*----------------------------------------------------------------------*/

byte * trie_get_calls(CTXTdecl)
{
   Cell call_term;
   Psc psc_ptr;
   TIFptr tip_ptr;
   BTNptr call_trie_root;
   CPtr cptr;
   int i;
#ifdef MULTI_THREAD_RWL
   CPtr tbreg;
#ifdef SLG_GC
   CPtr old_cptop;
#endif
#endif
   call_term = ptoc_tag(CTXTc 1);
   if ((psc_ptr = term_psc(call_term)) != NULL) {
     tip_ptr = get_tip(CTXTc psc_ptr);
     if (tip_ptr == NULL) {
       /* TLS: added the !get_tabled() to handle cases where the
	  predicate has been tabled but a tip has not yet been created
	  (e.g clauses for a dynamic tabled predicate have not yet
	  been defined */
       if (!get_incr(psc_ptr) && !get_tabled(psc_ptr))
	 xsb_abort("get_calls/3 called with non-tabled predicate: %s/%d",
		   get_name(psc_ptr),get_arity(psc_ptr));
       return (byte *)&fail_inst;
     }
     call_trie_root = TIF_CallTrie(tip_ptr);
     if (call_trie_root == NULL)
       return (byte *)&fail_inst;
     else {
       cptr = (CPtr)cs_val(call_term);
       reg_arrayptr = reg_array-1;
       num_vars_in_var_regs = -1;
       for (i = get_arity(psc_ptr); i>=1; i--) {
#ifdef DEBUG_DELAYVAR
	 xsb_dbgmsg((LOG_DEBUG,">>>> push one cell"));
#endif
	 push_reg_array(cell(cptr+i));
       }
#ifdef MULTI_THREAD_RWL
/* save choice point for trie_unlock instruction */
//       save_find_locx(ereg);
//       tbreg = top_of_cpstack;
#ifdef SLG_GC
       old_cptop = tbreg;
#endif
       save_choicepoint(tbreg,ereg,(byte *)&trie_fail_unlock_inst,breg);
#ifdef SLG_GC
       cp_prevtop(tbreg) = old_cptop;
#endif
       breg = tbreg;
       hbreg = hreg;
#endif

       return (byte *)call_trie_root;
     }
   }
   else
     return (byte *)&fail_inst;
}

/*----------------------------------------------------------------------*/

/*
 * This function is changed from get_lastnode_and_retskel().  It is the
 * body of *inline* builtin GET_LASTNODE_CS_RETSKEL(LastNode, CallStr,
 * RetSkel). [1/9/1999]
 *
 * This function is called immediately after using the trie intructions
 * to traverse one branch of the call or answer trie.  A side-effect of
 * executing these instructions is that the leaf node of the branch is
 * left in a global variable "Last_Nod_Sav".  One reason for writing it
 * so is that it is important that the construction of the return
 * skeleton is an operation that cannot be interrupted by garbage
 * collection.
 *
 * In case we just traversed the Call Trie of a subsumptive predicate,
 * and the call we just unified with is subsumed, then the answer
 * template (i.e., the return) must be reconstructed based on the
 * original call, the argument "callTerm" below, and the subsuming call
 * in the table.  Otherwise, we return the variables placed in
 * "var_regs[]" during the embedded-trie-code walk.
 */
Cell get_lastnode_cs_retskel(CTXTdeclc Cell callTerm) {

  prolog_int arity;
  Cell *vectr;

  arity = global_num_vars + 1;
  vectr = (Cell *)var_regs;
  if ( IsInCallTrie(Last_Nod_Sav) ) {
    VariantSF sf = CallTrieLeaf_GetSF(Last_Nod_Sav);
    if ( IsProperlySubsumed(sf) ) {
      construct_answer_template(CTXTc callTerm, conssf_producer(sf),
				(Cell *)var_regs);
      arity = (prolog_int)var_regs[0];
      vectr = (Cell *)&var_regs[1];
    }
  }
  return ( build_ret_term(CTXTc arity, vectr) );
}

/*----------------------------------------------------------------------*/
/* creates an empty (dummy) answer.					*/
/*----------------------------------------------------------------------*/

ALNptr empty_return(CTXTdeclc VariantSF subgoal)
{
    ALNptr i;
  
    /* Used only in one context hence this abuse */
    New_ALN(subgoal,i,&dummy_ans_node,NULL);
    return i;
}

