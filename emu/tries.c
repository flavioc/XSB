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


#include <stdio.h>
#include <stdlib.h>

#include "configs/config.h"
#include "debugs/debug.h"

/* Special debug includes */
#include "debugs/debug_tries.h"

#include "auxlry.h"
#include "cell.h"
#include "inst.h"
#include "psc.h"
#include "heap.h"
#include "flags.h"
#include "deref.h"
#include "xsb_memory.h"
#include "register.h"
#include "binding.h"
#include "trie_internals.h"
#include "xmacro.h"
#include "choice.h"
#include "cinterf.h"
#include "xsberror.h"
#include "tr_utils.h"

/*----------------------------------------------------------------------*/

extern Psc term_psc(Cell);
extern Cell ptoc_tag(int);
extern void ctop_tag(int, Cell);
extern TIFptr get_tip(Psc);
#ifdef DPVR_DEBUG_BD
extern void printterm(Cell, byte, int);
#endif

/*----------------------------------------------------------------------*/
/* The following variables are used in other parts of the system        */
/*----------------------------------------------------------------------*/

BTNptr Paren;

long subg_chk_ins, subg_inserts, ans_chk_ins, ans_inserts; /* statistics */

int  num_heap_term_vars;
CPtr *var_addr;
int  var_addr_arraysz = DEFAULT_ARRAYSIZ;
Cell CallVarEnum[NUM_TRIEVARS];
Cell TrieVarBindings[NUM_TRIEVARS];

/*
 * global_num_vars is a new variable to save the value of variable
 * num_vars_in_var_regs temporarily.
 */
int global_num_vars;

/*
 * Array mini_trail[] is used to trail the variable bindings when we
 * copy terms into tries.  The variables trailed using mini_trail are
 * those that are bound to elements in CallVarEnum[].
 */
static CPtr mini_trail[NUM_TRIEVARS];
static CPtr *mini_trail_top;

/*----------------------------------------------------------------------*/
/* Safe assignment -- can be generalized by type.
   CPtr can be abstracted out */
#define safe_assign(ArrayNam,Index,Value,ArraySz) {\
   if (Index >= ArraySz) {\
     trie_expand_array(CPtr,ArrayNam,ArraySz,"var_addr");\
   }\
   ArrayNam[Index] = Value;\
}

/*----------------------------------------------------------------------*/
/*****************Addr Stack*************/
static int addr_stack_pointer = 0;
static CPtr *Addr_Stack;
static int addr_stack_size    = DEFAULT_ARRAYSIZ;

#define pop_addr Addr_Stack[--addr_stack_pointer]
#define push_addr(X) {\
    if (addr_stack_pointer == addr_stack_size) {\
       trie_expand_array(CPtr, Addr_Stack ,addr_stack_size,"Addr_Stack");\
    }\
    Addr_Stack[addr_stack_pointer++] = ((CPtr) X);\
}

/*----------------------------------------------------------------------*/
/*****************Term Stack*************/
static int  term_stackptr = -1;
static Cell *term_stack;
static long term_stacksize = DEFAULT_ARRAYSIZ;

#define pop_term term_stack[term_stackptr--]
#define push_term(T) {\
    if (term_stackptr+1 == term_stacksize) {\
       trie_expand_array(Cell,term_stack,term_stacksize,"term_stack");\
    }\
    term_stack[++term_stackptr] = ((Cell) T);\
}

/*----------------------------------------------------------------------*/
/*********Simpler trails ****************/

#define simple_table_undo_bindings		\
    while (mini_trail_top >= mini_trail) {	\
	untrail(*mini_trail_top);		\
	mini_trail_top--;			\
    }	

#define StandardizeAndTrailVariable(addr,n)	\
   StandardizeVariable(addr,n);			\
    *(++mini_trail_top) = addr;
		
/*----------------------------------------------------------------------*/
/* Variables used only in this file                                     */
/*----------------------------------------------------------------------*/

static BasicTrieNode dummy_ans_node = {{0,1,0,0},NULL,NULL,NULL,0};

static int AnsVarCtr, ctr;

static BTNptr *GNodePtrPtr;

/*----------------------------------------------------------------------*/

/*
 *          T R I E   S T R U C T U R E   M A N A G E M E N T
 *          =================================================
 */

/* For Call and Answer Tries
   ------------------------- */
Structure_Manager smTableBTN  = SM_InitDecl(BasicTrieNode, BTNs_PER_BLOCK,
					    "Basic Trie Node");
Structure_Manager smTableBTHT = SM_InitDecl(BasicTrieHT, BTHTs_PER_BLOCK,
					    "Basic Trie Hash Table");
Structure_Manager smTSTN      = SM_InitDecl(TS_TrieNode, TSTNs_PER_BLOCK,
					    "Time-Stamped Trie Node");
Structure_Manager smTSTHT     = SM_InitDecl(TST_HashTable, TSTHTs_PER_BLOCK,
					    "Time-Stamped Trie Hash Table");
Structure_Manager smEntry     = SM_InitDecl(TSI_Entry, TSI_ENTRIES_PER_BLOCK,
					    "TST Aux Node");

/* For Assert & Intern Tries
   ------------------------- */
Structure_Manager smAssertBTN  = SM_InitDecl(BasicTrieNode, BTNs_PER_BLOCK,
					     "Basic Trie Node");
Structure_Manager smAssertBTHT = SM_InitDecl(BasicTrieHT, BTHTs_PER_BLOCK,
					     "Basic Trie Hash Table");

/* Maintains Current Structure Space
   --------------------------------- */
Structure_Manager *smBTN = &smTableBTN,
                  *smBTHT = &smTableBTHT;

/*----------------------------------------------------------------------*/

void init_trie_aux_areas(void)
{
  int i;

  alloc_arr(Cell,term_stack,term_stacksize);
  alloc_arr(CPtr,var_addr,var_addr_arraysz);
  alloc_arr(CPtr,Addr_Stack,addr_stack_size);
  alloc_arr(Cell,reg_array,reg_array_size);
  reg_arrayptr = reg_array -1;

  for (i = 0; i < NUM_TRIEVARS; i++)
    CallVarEnum[i] = (Cell) & (CallVarEnum[i]);
}

/*-------------------------------------------------------------------------*/

BTNptr new_btn(int trie_t, int node_t, Cell symbol, BTNptr parent,
	       BTNptr sibling) {

  BTNptr btn;

  SM_AllocateStruct(*smBTN,btn);
  TN_Init(btn,trie_t,node_t,symbol,parent,sibling);
  return btn;
}

/*-------------------------------------------------------------------------*/

TSTNptr new_tstn(int trie_t, int node_t, Cell symbol, TSTNptr parent,
		TSTNptr sibling) {

  TSTNptr tstn;

  SM_AllocateStruct(smTSTN,tstn);
  TN_Init(tstn,trie_t,node_t,symbol,parent,sibling);
  TSTN_TimeStamp(tstn) = TSTN_DEFAULT_TIMESTAMP;
  return tstn;
}

/*-------------------------------------------------------------------------*/

/*
 * Creates a root node for a given type of trie.
 */

BTNptr newBasicTrie(Psc predicate, int trie_type) {

  BTNptr pRoot;

  New_BTN( pRoot, trie_type, TRIE_ROOT_NT, EncodeTriePSC(predicate),
	   NULL, NULL );
  return pRoot;
}

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
    New_BTN(LocalNodePtr,TrieType,INTERRIOR_NT,item,Paren,wherefrom);	\
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
 *
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
 *  just-processed symbol.  */

#define one_node_chk_ins(Found,item,TrieType) {				\
									\
   int count = 0;							\
   BTNptr LocalNodePtr;							\
									\
   if ( IsNULL(*GNodePtrPtr) ) {					\
     New_BTN(LocalNodePtr,TrieType,INTERRIOR_NT,item,Paren,NULL);	\
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
       hashify_children(pParent,TrieType);				\
   }									\
   GNodePtrPtr = &(BTN_Child(LocalNodePtr));				\
}

/*----------------------------------------------------------------------*/

/* Trie-HashTable maintenance routines.
   ------------------------------------
   parentHook is the address of a field in some structure (should now be
   another trie node as all tries now have roots) which points to a chain
   of trie nodes whose length has become "too long."
*/

void hashify_children(BTNptr parent, int trieType) {

  BTNptr children;		/* child list of the parent */
  BTNptr btn;			/* current child for processing */
  BTHTptr ht;			/* HT header struct */
  BTNptr *tablebase;		/* first bucket of allocated HT */
  unsigned long  hashseed;	/* needed for hashing of BTNs */


  New_BTHT(ht,trieType);
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


void expand_trie_ht(BTHTptr pHT) {

  BTNptr *bucket_array;     /* base address of resized hash table */
  BTNptr *upper_buckets;    /* marker in the resized HT delimiting where the
			        newly allocated buckets begin */

  BTNptr *bucket;           /* for stepping through buckets of the HT */

  BTNptr curNode;           /* TSTN being processed */
  BTNptr nextNode;          /* rest of the TSTNs in a bucket */

  unsigned long  new_size;  /* double duty: new HT size, then hash mask */


  new_size = TrieHT_NewSize(pHT);
  bucket_array = (BTNptr *)realloc( BTHT_BucketArray(pHT),
				     new_size * sizeof(BTNptr) );
  if ( IsNULL(bucket_array) )
    return;

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
static void follow_par_chain(BTNptr pLeaf)
{
  term_stackptr = -1; /* Forcibly Empty term_stack */
  while ( IsNonNULL(pLeaf) && (! IsTrieRoot(pLeaf)) ) {
    push_term((BTN_Symbol(pLeaf)));
    pLeaf = BTN_Parent(pLeaf);
  }
}

/*----------------------------------------------------------------------*/

/*
 * Given a hook to an answer-list node, returns the answer contained in
 * that node and updates the hook to the next node in the chain.
 */
BTNptr get_next_trie_solution(ALNptr *NextPtrPtr)
{
  BTNptr TempPtr;

  TempPtr = aln_answer_ptr(*NextPtrPtr);
  *NextPtrPtr = aln_next_aln(*NextPtrPtr);
  return(TempPtr);
}

/*----------------------------------------------------------------------*/

#define rec_macro_make_heap_term(Macro_addr)\
{int rj,rArity;\
   while(addr_stack_pointer)\
   {\
     Macro_addr = (CPtr)pop_addr;\
     xtemp2 = pop_term;\
     switch( TrieSymbolType(xtemp2) )\
     {\
      case TrieVar: {\
	int index = DecodeTrieVar(xtemp2);\
	if (IsNewTrieVar(xtemp2)) {\
          safe_assign(var_addr,index,Macro_addr,var_addr_arraysz);\
	  *Macro_addr = (Cell)var_addr[index];\
	  num_heap_term_vars++;\
	}\
	else\
	  *Macro_addr = (Cell) var_addr[index];\
	}\
	break;\
      case STRING:\
      case INT:\
      case FLOAT:\
	*Macro_addr = DecodeTrieConstant(xtemp2);\
	break;\
      case LIST: \
	*Macro_addr = (Cell) makelist(hreg);\
	hreg += 2;\
	push_addr(hreg-1);\
	push_addr(hreg-2);\
	break;\
      case CS:\
	*Macro_addr = (Cell) makecs(hreg);\
	xtemp2 = (Cell) DecodeTrieFunctor(xtemp2);\
	*hreg = xtemp2;\
	rArity = (int) get_arity((Psc) xtemp2);\
	for (rj= rArity; rj >= 1; rj --) {\
	  push_addr(hreg+rj);\
	}\
	hreg += rArity;\
	hreg++;\
	break;\
      default:\
        xsb_abort("Bad tag in macro_make_heap_term");\
	return;\
   }\
  }\
}

/*----------------------------------------------------------------------*/

#define macro_make_heap_term(ataddr,ret_val,dummy_addr)\
{ int mArity,mj;\
  xtemp2 = pop_term;\
  switch( TrieSymbolType(xtemp2) )\
   {\
    case TrieVar: {\
        int index = DecodeTrieVar(xtemp2);\
	if (IsNewTrieVar(xtemp2)) { /* diff with CHAT - Kostis */\
          safe_assign(var_addr,index,ataddr,var_addr_arraysz);\
	  ret_val = (Cell)var_addr[index];\
	  num_heap_term_vars++;\
	}\
	else\
	  ret_val = (Cell) var_addr[index];\
      }\
      break;\
    case STRING:\
    case INT:\
    case FLOAT:\
	ret_val = DecodeTrieConstant(xtemp2);\
	break;\
    case LIST: \
	ret_val = (Cell) makelist(hreg) ;\
	hreg += 2;\
	push_addr(hreg-1);\
	push_addr(hreg-2);\
	rec_macro_make_heap_term(dummy_addr);\
	break;\
    case CS:\
	ret_val = (Cell) makecs(hreg);\
	xtemp2 = (Cell) DecodeTrieFunctor(xtemp2);\
	*hreg = xtemp2;\
	mArity = (int) get_arity((Psc) xtemp2);\
	for (mj= mArity; mj >= 1; mj--) {\
	  push_addr(hreg+mj);\
	}\
	hreg += mArity;\
	hreg++;\
	rec_macro_make_heap_term(dummy_addr);\
	break;\
     default: \
        xsb_abort("Bad tag in macro_make_heap_term");\
	return;\
   }\
}

/*----------------------------------------------------------------------*/

#define recvariant_trie(flag,TrieType)\
{\
	int  j;\
\
	while (!pdlempty ) {\
	  xtemp1 = (CPtr) pdlpop;\
	  cptr_deref(xtemp1);\
	  tag = cell_tag(xtemp1);\
	  switch (tag) {\
	    case FREE: case REF1: \
	      if (! IsStandardizedVariable(xtemp1)) {\
	        StandardizeAndTrailVariable(xtemp1,ctr);\
	        item = EncodeNewTrieVar(ctr);\
	        one_node_chk_ins(flag, item, TrieType);\
	        ctr++;\
	      } else {\
	        item = IndexOfStandardizedVariable(xtemp1);\
	        item = EncodeTrieVar(item);\
                one_node_chk_ins(flag, item, TrieType);\
	      }\
            break;\
	    case STRING: case INT: case FLOAT:\
	      one_node_chk_ins(flag, EncodeTrieConstant(xtemp1), TrieType);\
	      break;\
	    case LIST:\
	      one_node_chk_ins(flag, EncodeTrieList(xtemp1), TrieType);\
	      pdlpush(cell(clref_val(xtemp1)+1));\
              pdlpush(cell(clref_val(xtemp1)));\
	      break;\
	    case CS:\
              psc = (Psc) follow(cs_val(xtemp1));\
              item = makecs(psc);\
	      one_node_chk_ins(flag, item, TrieType);\
	      for (j = get_arity(psc); j>=1 ; j--) {\
  	         pdlpush(cell(clref_val(xtemp1)+j));\
              }\
	      break;\
	    default: \
              xsb_abort("Bad type tag in recvariant_trie...\n");\
	  }\
	}\
        resetpdl;\
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

#define recvariant_trie_ans_subsf(flag,TrieType)\
{\
        int  j;\
\
	while (!pdlempty ) {\
	  xtemp1 = (CPtr) pdlpop;\
	  cptr_deref(xtemp1);\
	  tag = cell_tag(xtemp1);\
	  switch (tag) {\
	    case FREE: case REF1:\
	      if (! IsStandardizedVariable(xtemp1)){\
		bld_free(hreg);\
		bind_ref(xtemp1, hreg);\
		xtemp1 = hreg++;\
	        StandardizeAndTrailVariable(xtemp1,ctr);\
	        one_node_chk_ins(flag,EncodeNewTrieVar(ctr),TrieType);\
	        ctr++;\
	      } else {\
                one_node_chk_ins(flag,EncodeTrieVar(IndexOfStandardizedVariable(xtemp1)),TrieType);\
	      }\
              break;\
	    case STRING: case INT: case FLOAT:\
	      one_node_chk_ins(flag, EncodeTrieConstant(xtemp1), TrieType);\
	      break;\
	    case LIST:\
	      one_node_chk_ins(flag, EncodeTrieList(xtemp1), TrieType);\
	      pdlpush(cell(clref_val(xtemp1)+1));\
              pdlpush(cell(clref_val(xtemp1)));\
	      break;\
	    case CS:\
	      psc = (Psc) follow(cs_val(xtemp1));\
	      item = makecs(psc);\
	      one_node_chk_ins(flag, item, TrieType);\
	      for (j = get_arity(psc); j>=1 ; j--) {\
  	         pdlpush(cell(clref_val(xtemp1)+j));\
	      }\
	      break;\
	    default: \
              xsb_abort("Bad type tag in recvariant_trie_ans_subsf...\n");\
	  }\
	}\
        resetpdl;\
}

/*----------------------------------------------------------------------
 * variant_answer_search(NumVarsInCall, VarsInCall, SubgoalPtr, Ptrflag)
 *
 * Called in SLG instruction `new_answer_dealloc', variant_answer_search()
 * checks if the answer has been returned before and, if not, inserts it
 * into the answer trie.  Here, `arity' is the number of variables in the
 * call (arity of the answer substitution), `cptr' is the pointer to
 * VarsInCall (all the variables in call, saved in the CP stack and
 * already bound to some terms), and `subgoal_ptr' is the subgoal frame
 * of the call.  At the end of this function, `flagptr' tells if the
 * answer has been returned before.
 *
 * The returned value of this function is the leaf of the answer trie.  
 *----------------------------------------------------------------------*/

BTNptr variant_answer_search(int arity, CPtr cptr, SGFrame subgoal_ptr,
			     bool *flagptr) {

    Psc   psc;
    CPtr  xtemp1;
    int   i, j, flag = 1;
    Cell  tag = FREE, item;
    ALNptr answer_node;

    ans_chk_ins++; /* Counter (answers checked & inserted) */

    mini_trail_top = (CPtr *)(& mini_trail[0]) - 1;
    AnsVarCtr = 0;
    ctr = 0;
    if ( IsNULL(subg_ans_root_ptr(subgoal_ptr)) )
      subg_ans_root_ptr(subgoal_ptr) =
	newBasicTrie(get_ret_psc(arity),BASIC_ANSWER_TRIE_TT);
    Paren = subg_ans_root_ptr(subgoal_ptr);
    GNodePtrPtr = &BTN_Child(Paren);
    for (i = 0; i < arity; i++) {
      xtemp1 = (CPtr) (cptr - i); /* One element of VarsInCall.  It might
				   * have been bound in the answer for
				   * the call.
				   */
      cptr_deref(xtemp1);
      tag = cell_tag(xtemp1);
      switch (tag) {
      case FREE: case REF1:
	if (! IsStandardizedVariable(xtemp1)) {
	  /*
	   * If this is the first occurrence of this variable, then:
	   *
	   * 	StandardizeAndTrailVariable(xtemp1, ctr)
	   * 			||
	   * 	bld_ref(xtemp1, CallVarEnum[ctr]);
	   * 	*(++mini_trail_top) = xtemp1
	   *
	   * Notice that all the variables appear in the answer are bound
	   * to elements in CallVarEnum[], and each element in
	   * CallVarEnum[] is a free variable itself.  Besides, all
	   * these variables are trailed (saved in mini_trail[]) and they
	   * will be used in delay_chk_insert() (in function
	   * do_delay_stuff()).
	   */
#ifndef IGNORE_DELAYVAR
	  bld_free(hreg); /* To make sure there is no pointer from heap to 
			   * local stack.
			   */
	  bind_ref(xtemp1, hreg);
	  xtemp1 = hreg++;
#endif
	  StandardizeAndTrailVariable(xtemp1,ctr);
	  item = EncodeNewTrieVar(ctr);
	  one_node_chk_ins(flag, item, BASIC_ANSWER_TRIE_TT);
	  ctr++;
	} else {
	  item = IndexOfStandardizedVariable(xtemp1);
	  item = EncodeTrieVar(item);
	  one_node_chk_ins(flag, item, BASIC_ANSWER_TRIE_TT);
	}
	break;
      case STRING: case INT: case FLOAT:
	one_node_chk_ins(flag, EncodeTrieConstant(xtemp1),
			 BASIC_ANSWER_TRIE_TT);
	break;
      case LIST:
	one_node_chk_ins(flag, EncodeTrieList(xtemp1), BASIC_ANSWER_TRIE_TT);
	pdlpush(cell(clref_val(xtemp1)+1));
	pdlpush(cell(clref_val(xtemp1)));
#ifndef IGNORE_DELAYVAR
	recvariant_trie_ans_subsf(flag, BASIC_ANSWER_TRIE_TT);
#else
	recvariant_trie(flag, BASIC_ANSWER_TRIE_TT);
#endif 
	break;
      case CS:
	psc = (Psc)follow(cs_val(xtemp1));
	item = makecs(psc);
	one_node_chk_ins(flag, item, BASIC_ANSWER_TRIE_TT);
	for (j = get_arity(psc); j >= 1 ; j--) {
	  pdlpush(cell(clref_val(xtemp1)+j));
	}
#ifndef IGNORE_DELAYVAR
	recvariant_trie_ans_subsf(flag, BASIC_ANSWER_TRIE_TT);
#else
	recvariant_trie(flag, BASIC_ANSWER_TRIE_TT);
#endif
	break;
      default:
	xsb_abort("Bad type tag in variant_answer_search()");
      }                                                       
    }
    resetpdl;                                                   

#ifndef IGNORE_DELAYVAR
    /*
     * Put the substitution factor of the answer into a term ret/n (if 
     * the arity of the substitution factor is 0, then put integer 0
     * into cell ans_var_pos_reg).
     *
     * Notice that simple_table_undo_bindings in the old version of XSB
     * has been removed here, because all the variable bindings of this
     * answer will be used later on (in do_delay_stuff()) when we build
     * the delay list for this answer.
     */
    if (ctr == 0)
      bld_int(ans_var_pos_reg, 0);
    else	
      bld_functor(ans_var_pos_reg, get_ret_psc(ctr));
#else /* IGNORE_DELAYVAR */
    undo_answer_bindings();
#endif

    /*
     * Save the number of variables in the answer, i.e. the arity of
     * the substitution factor of the answer, into `AnsVarCtr'.
     */
    AnsVarCtr = ctr;		

#ifdef DEBUG_DELAYVAR
    xsb_dbgmsg(">>>> [V] AnsVarCtr = %d", AnsVarCtr);
#endif

    /* if there is no term to insert, an ESCAPE node has to be created/found */

    if (arity == 0) {
      one_node_chk_ins(flag, ESCAPE_NODE_SYMBOL, BASIC_ANSWER_TRIE_TT);
      Instr(Paren) = trie_proceed;
    }

    /*
     *  If an insertion was performed, do some maintenance on the new leaf,
     *  and place the answer handle onto the answer list.
     */
    if ( flag == 0 ) {
      MakeLeafNode(Paren);
      TN_UpgradeInstrTypeToSUCCESS(Paren,tag);
      ans_inserts++;

      New_ALN(answer_node,Paren,NULL);
      SF_AppendNewAnswer(subgoal_ptr,answer_node);
    }

    *flagptr = flag;	
    return Paren;
}

/*
 * undo_answer_bindings() has the same functionality of
 * simple_table_undo_bindings.  It is called just after do_delay_stuff(),
 * and do_delay_stuff() is called after variant_answer_search (in
 * new_answer_dealloc)
 *
 * In XSB 1.8.1, simple_table_undo_bindings is called in
 * variant_answer_search().  But to handle variables in delay list in
 * do_delay_stuff() , we need the variable binding information got from
 * variant_answer_search().  So we have to take simple_table_undo_bindings
 * out of variant_answer_search() and call it after do_delay_stuff() is
 * done.
 */

void undo_answer_bindings() {
  simple_table_undo_bindings;
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
 * After variant_answer_search() is finished, CallVarEnum[] contains the
 * variables in the head of the corresponding clause for p/2.  When we call
 * delay_chk_insert() to intern the delay list for p/2, CallVarEnum[]
 * will be used again to bind the variables that appear in the body.
 * Because we have to check if a variable in a delay element of p/2 is
 * already in the head, the old bindings of variables to CallVarEnum[]
 * are still needed.  So undo_answer_bindings has to be delayed.
 *
 * In the arguments, `arity' is the arity of the the answer substitution
 * factor, `cptr' points to the first field of term ret/n (the answer
 * substitution factor), `hook' is a pointer to a location where the top of
 * this delay trie will become anchored.  Since these delay "tries" only
 * occur as single paths, there is currently no need for a root node.
 */
 
BTNptr delay_chk_insert(int arity, CPtr cptr, CPtr *hook)
{
    Psc  psc;
    Cell item;
    CPtr xtemp1;
    int  i, j, tag = FREE, flag = 1;
 
#ifdef DEBUG_DELAYVAR
    xsb_dbgmsg(">>>> start delay_chk_insert()");
#endif

    Paren = NULL;
    GNodePtrPtr = (BTNptr *) hook;

    ctr = AnsVarCtr;

#ifdef DEBUG_DELAYVAR
    xsb_dbgmsg(">>>> [D1] AnsVarCtr = %d", AnsVarCtr);
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
#ifdef DPVR_DEBUG_BD
      fprintf(stddbg, "arg[%d] =  %x ",i, xtemp1);
#endif
      cptr_deref(xtemp1);
#ifdef DPVR_DEBUG_BD
      printterm(xtemp1,1,25);
      fprintf(stddbg, "\n");
#endif
      tag = cell_tag(xtemp1);
      switch (tag) {
      case FREE: case REF1:
	if (! IsStandardizedVariable(xtemp1)) {
          StandardizeAndTrailVariable(xtemp1,ctr);
          one_node_chk_ins(flag,EncodeNewTrieVar(ctr),
			   DELAY_TRIE_TT);
          ctr++;
        }
        else {
          one_node_chk_ins(flag,
			   EncodeTrieVar(IndexOfStandardizedVariable(xtemp1)),
			   DELAY_TRIE_TT);
        }
        break;
      case STRING: case INT: case FLOAT:
        one_node_chk_ins(flag, EncodeTrieConstant(xtemp1), DELAY_TRIE_TT);
        break;
      case LIST:
        one_node_chk_ins(flag, EncodeTrieList(xtemp1), DELAY_TRIE_TT);
        pdlpush(cell(clref_val(xtemp1)+1));
        pdlpush(cell(clref_val(xtemp1)));
        recvariant_trie(flag,DELAY_TRIE_TT);
        break;
      case CS:
        one_node_chk_ins(flag, makecs(follow(cs_val(xtemp1))),DELAY_TRIE_TT);
        for (j = get_arity((Psc)follow(cs_val(xtemp1))); j >= 1 ; j--) {
          pdlpush(cell(clref_val(xtemp1)+j));
        }
        recvariant_trie(flag,DELAY_TRIE_TT);
        break;
        default:
          xsb_abort("Bad type tag in delay_chk_insert()\n");
        }
    }
    resetpdl;  
    AnsVarCtr = ctr;

#ifdef DEBUG_DELAYVAR
    xsb_dbgmsg(">>>> [D2] AnsVarCtr = %d", AnsVarCtr);
#endif

    /*
     *  If an insertion was performed, do some maintenance on the new leaf.
     */
    if ( flag == 0 ) {
      MakeLeafNode(Paren);
      TN_UpgradeInstrTypeToSUCCESS(Paren,tag);
    }
 
#ifdef DPVR_DEBUG_BD
    fprintf(stddbg, "----------------------------- Exit\n");
#endif
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
static void load_solution_from_trie(int arity, CPtr cptr)
{
   int i;
   CPtr xtemp1, Dummy_Addr;
   Cell returned_val, xtemp2;

   for (i=0; i<arity; i++) {
     xtemp1 = (CPtr) (cptr-i);
     cptr_deref(xtemp1);
     macro_make_heap_term(xtemp1,returned_val,Dummy_Addr);
     if (xtemp1 != (CPtr)returned_val) {
       dbind_ref(xtemp1,returned_val);
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
static void bottomupunify(Cell term, BTNptr Root, BTNptr Leaf)
{
  CPtr Dummy_Addr;
  Cell returned_val, xtemp2;
  CPtr gen;
  int  i;


  num_heap_term_vars = 0;     
  follow_par_chain(Leaf);
  deref(term);
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
 *  Used with tries created via the builtin trie_intern.
 */
bool bottom_up_unify(void)
{
  Cell    term;
  BTNptr root;
  BTNptr leaf;
  int     rootidx;
  extern  BTNptr *Set_ArrayPtr;


  leaf = (BTNptr) ptoc_int(3);   
  if( IsDeletedNode(leaf) )
    return FALSE;

  term    = ptoc_tag(1);
  rootidx = ptoc_int(2);
  root    = Set_ArrayPtr[rootidx];  
  bottomupunify(term, root, leaf);
  return TRUE;
}

/*----------------------------------------------------------------------*/

/*
 * `TriePtr' is a leaf in the answer trie, and `cptr' is a vector of
 * variables for receiving the substitution.
 */
void load_solution_trie(int arity, CPtr cptr, BTNptr TriePtr)
{
  num_heap_term_vars = 0;
  if (arity > 0) {
    follow_par_chain(TriePtr);
    load_solution_from_trie(arity,cptr);
  }
}

/*----------------------------------------------------------------------*/

void load_delay_trie(int arity, CPtr cptr, BTNptr TriePtr)
{
   if (arity) {
     follow_par_chain(TriePtr);
     load_solution_from_trie(arity,cptr);
   }
}
 
/*----------------------------------------------------------------------*/

#define recvariant_call(flag,TrieType,xtemp1)\
{\
  int  j;\
\
  while (!pdlempty) {\
    xtemp1 = (CPtr) pdlpop;\
    cptr_deref(xtemp1);\
    switch(tag = cell_tag(xtemp1)) {\
      case FREE: case REF1: \
        if (! IsStandardizedVariable(xtemp1)) {\
	  *(--VarPosReg) = (Cell) xtemp1;\
	  StandardizeVariable(xtemp1,ctr);\
	  one_node_chk_ins(flag,EncodeNewTrieVar(ctr),TrieType);\
	  ctr++;\
        } else{\
	  one_node_chk_ins(flag,EncodeTrieVar(IndexOfStandardizedVariable(xtemp1)),TrieType);\
        }\
        break;\
      case STRING: case INT: case FLOAT:\
	one_node_chk_ins(flag, EncodeTrieConstant(xtemp1), TrieType);\
	break;\
      case LIST:\
	one_node_chk_ins(flag, EncodeTrieList(xtemp1), TrieType);\
	pdlpush( cell(clref_val(xtemp1)+1) );\
        pdlpush( cell(clref_val(xtemp1)) );\
	break;\
      case CS:\
        psc = (Psc) follow(cs_val(xtemp1));\
	item = makecs(psc);\
	one_node_chk_ins(flag, item, TrieType);\
	for (j=get_arity(psc); j>=1; j--) {\
	  pdlpush(cell(clref_val(xtemp1)+j));\
	}\
	break;\
      default: \
        xsb_abort("Bad type tag in recvariant_call...\n");\
    }\
  }\
  resetpdl;\
}

/*----------------------------------------------------------------------*/

/*
 * Searches/inserts a subgoal call structure into a subgoal call trie.
 * During search/insertion, the variables of the subgoal call are pushed
 *   on top of the CP stack (through VarPosReg), along with the # of
 *   variables that were pushed.  This forms the substitution factor.
 * Prolog variables are standardized during this process to recognize
 *   multiple (nonlinear) occurences.  They must be reset to an unbound
 *   state before termination.
 * Many global variables:
 * Paren - to be set to point to inserted term's leaf
 * VarPosReg - pointer to top of CPS; place to put the substitution factor
 *    in high-to-low memory format.
 * GNodePtrPtr - local to file?  Points to the parent-internal-structure's
 *    "child" or "NODE_link" field.  It's a place to anchor any newly
 *    created NODEs.
 * ctr - local to file; contains the number of distinct variables found
 *    in the call.
 * Pay careful attention to the expected argument vector accepted by this
 * function.  It actually points one Cell *before* the term vector!  Notice
 * the treatment of "cptr" as these terms are inspected.
 */

void variant_call_search(TabledCallInfo *call_info, CallLookupResults *results)
{
    Psc  psc;
    CPtr call_arg;
    int  arity, i, j, flag = 1;
    Cell tag = FREE, item;
    CPtr cptr, VarPosReg, tVarPosReg;
    TIFptr pTIF;


    subg_chk_ins++;
    pTIF = CallInfo_TableInfo(*call_info);
    if ( IsNULL(TIF_CallTrie(pTIF)) )
      TIF_CallTrie(pTIF) = newBasicTrie(TIF_PSC(pTIF),CALL_TRIE_TT);
    Paren = TIF_CallTrie(pTIF);
    GNodePtrPtr = &BTN_Child(Paren);
    arity = CallInfo_CallArity(*call_info);
    cptr = CallInfo_Arguments(*call_info);
    tVarPosReg = VarPosReg = CallInfo_VarVectorLoc(*call_info);
    ctr = 0;

    for (i = 0; i < arity; i++) {
      call_arg = (CPtr) (cptr + i);            /* Note! */
      cptr_deref(call_arg);
      tag = cell_tag(call_arg);
      switch (tag) {
        case FREE: case REF1:
	  if (! IsStandardizedVariable(call_arg)) {
	    /*
	     * Save pointers of the substitution factor of the call
	     * into CP stack.  Each pointer points to a variable in 
	     * the heap.  The variables may get bound in the later
	     * computation.
	     */
	    *(--VarPosReg) = (Cell) call_arg;
	    StandardizeVariable(call_arg,ctr);
	    one_node_chk_ins(flag,EncodeNewTrieVar(ctr),
			     CALL_TRIE_TT);
	    ctr++;
	  } else {
	    one_node_chk_ins(flag,EncodeTrieVar(IndexOfStandardizedVariable(call_arg)),CALL_TRIE_TT);
	  }
	  break;
	case STRING: case INT: case FLOAT:
	  one_node_chk_ins(flag, EncodeTrieConstant(call_arg), CALL_TRIE_TT);
	  break;
	case LIST:
	  one_node_chk_ins(flag, EncodeTrieList(call_arg), CALL_TRIE_TT);
	  pdlpush(cell(clref_val(call_arg)+1));
	  pdlpush(cell(clref_val(call_arg)));
	  recvariant_call(flag,CALL_TRIE_TT,call_arg);
	  break;
	case CS:
	  psc = (Psc)follow(cs_val(call_arg));
	  item = makecs(psc);
	  one_node_chk_ins(flag, item, CALL_TRIE_TT);
	  for (j=get_arity(psc); j>=1 ; j--) {
	    pdlpush(cell(clref_val(call_arg)+j));
	  }
	  recvariant_call(flag,CALL_TRIE_TT,call_arg);
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
      subg_inserts++;
      MakeLeafNode(Paren);
      TN_UpgradeInstrTypeToSUCCESS(Paren,tag);
    }

    cell(--VarPosReg) = makeint(ctr); /* tag the # of subs fact vars */
    while (--tVarPosReg > VarPosReg)
      ResetStandardizedVariable(*tVarPosReg);

    CallLUR_Leaf(*results) = Paren;
    CallLUR_Subsumer(*results) = CallTrieLeaf_GetSF(Paren);
    CallLUR_VariantFound(*results) = flag;
    CallLUR_VarVector(*results) = VarPosReg;
    return;
}

/*----------------------------------------------------------------------*/

static void remove_calls_and_returns(SGFrame CallStrPtr)
{
  ALNptr pALN;


  /* Delete the call entry
     --------------------- */
  delete_branch(subg_leaf_ptr(CallStrPtr),
		&TIF_CallTrie(subg_tif_ptr(CallStrPtr)));

  /* Delete its answers
     ------------------ */
  for ( pALN = subg_answers(CallStrPtr);  IsNonNULL(pALN);
	pALN = ALN_Next(pALN) )
    delete_branch(ALN_Answer(pALN), &subg_ans_root_ptr(CallStrPtr));

  /* Delete the table entry
     ---------------------- */
  free_answer_list(CallStrPtr);
  FreeProducerSF(CallStrPtr);
}


void remove_open_tries(CPtr bottom_parameter)
{
  bool warned = FALSE;
  SGFrame CallStrPtr;

  while (openreg < bottom_parameter) {
    CallStrPtr = (SGFrame)compl_subgoal_ptr(openreg);
    if (!is_completed(CallStrPtr)) {
      if (warned == FALSE) {
	xsb_warn("Removing incomplete tables...");
	warned = TRUE;
      }
      remove_calls_and_returns(CallStrPtr);
    }
    openreg += COMPLFRAMESIZE;
  }
}

/*----------------------------------------------------------------------*/

/*
 * For creating interned tries via buitin "trie_intern".
 */

BTNptr whole_term_chk_ins(Cell term, BTNptr *hook, int *flagptr)
{
    Psc  psc;
    CPtr xtemp1;
    int  j, flag = 1;
    Cell tag = FREE, item;


    if ( IsNULL(*hook) )
      *hook = newBasicTrie(get_intern_psc(),INTERN_TRIE_TT);
    Paren = *hook;
    GNodePtrPtr = &BTN_Child(Paren);

    xtemp1 = (CPtr) term;
    cptr_deref(xtemp1);
    tag = cell_tag(xtemp1);

    mini_trail_top = (CPtr *)(& mini_trail[0]) - 1;
    ctr = 0;

    switch (tag) {
    case FREE: case REF1:
      if (! IsStandardizedVariable(xtemp1)) {
	StandardizeAndTrailVariable(xtemp1,ctr);
	one_node_chk_ins(flag,EncodeNewTrieVar(ctr),
			 INTERN_TRIE_TT);
	ctr++;
      } else {
	one_node_chk_ins(flag,
			 EncodeTrieVar(IndexOfStandardizedVariable(xtemp1)),
			 INTERN_TRIE_TT);
      }
      break;
    case STRING: case INT: case FLOAT:
      one_node_chk_ins(flag, EncodeTrieConstant(xtemp1), INTERN_TRIE_TT);
      break;
    case LIST:
      one_node_chk_ins(flag, EncodeTrieList(xtemp1), INTERN_TRIE_TT);
      pdlpush(cell(clref_val(xtemp1)+1));
      pdlpush(cell(clref_val(xtemp1)));
      recvariant_trie(flag,INTERN_TRIE_TT);
      break;
    case CS:
      one_node_chk_ins(flag, makecs(follow(cs_val(xtemp1))),INTERN_TRIE_TT);
      for (j = get_arity((Psc)follow(cs_val(xtemp1))); j >= 1 ; j--) {
	pdlpush(cell(clref_val(xtemp1)+j));
      }
      recvariant_trie(flag,INTERN_TRIE_TT);
      break;
    default:
      xsb_abort("Bad type tag in whole_term_check_ins()");
    }

    /*
     *  If an insertion was performed, do some maintenance on the new leaf.
     */
    if ( flag == 0 ) {
      MakeLeafNode(Paren);
      TN_UpgradeInstrTypeToSUCCESS(Paren,tag);
    }

    for (j = 0; j < ctr; j++) var_regs[j] = mini_trail[j];
    /*
     * Both global_num_vars and Last_Nod_Sav are needed by
     * get_lastnode_cs_retskel() (see trie_intern/5 in intern.P).
     */
    global_num_vars = num_vars_in_var_regs = ctr - 1;
    Last_Nod_Sav = Paren;
    simple_table_undo_bindings;

    *flagptr = flag;
    return(Paren);
}

/*----------------------------------------------------------------------*/
/* one_term_chk_ins(termptr,hook,flag)					*/
/*----------------------------------------------------------------------*/

/*
 * For creating asserted tries with builtin "trie_assert".
 */

BTNptr one_term_chk_ins(CPtr termptr, BTNptr root, int *flagptr)
{
    int  arity;
    CPtr cptr;
    CPtr xtemp1;
    int  i, j, flag = 1;
    Cell tag = FREE, item;
    Psc  psc;

    psc = term_psc((prolog_term)termptr);
    arity = get_arity(psc);
    cptr = (CPtr)cs_val(termptr);

    mini_trail_top = (CPtr *)(& mini_trail[0]) - 1;
    ctr = 0;
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
      cptr_deref(xtemp1);
      tag = cell_tag(xtemp1);
      switch (tag) {
	case FREE: case REF1:
	  if (! IsStandardizedVariable(xtemp1)) {
	    StandardizeAndTrailVariable(xtemp1,ctr);
	    one_node_chk_ins(flag, EncodeNewTrieVar(ctr),
			     ASSERT_TRIE_TT);
	    ctr++;
	  } else {
	    one_node_chk_ins(flag,
			     EncodeTrieVar(IndexOfStandardizedVariable(xtemp1)),
			     ASSERT_TRIE_TT);
	  }
          break;
	case STRING: case INT: case FLOAT:
	  one_node_chk_ins(flag, EncodeTrieConstant(xtemp1), ASSERT_TRIE_TT);
	  break;
	case LIST:
	  one_node_chk_ins(flag, EncodeTrieList(xtemp1), ASSERT_TRIE_TT);
	  pdlpush(cell(clref_val(xtemp1)+1));
	  pdlpush(cell(clref_val(xtemp1)));
	  recvariant_trie(flag,ASSERT_TRIE_TT);
	  break;
	case CS:
	  psc = (Psc) follow(cs_val(xtemp1));
	  one_node_chk_ins(flag, makecs(psc),ASSERT_TRIE_TT);
	  for (j = get_arity(psc); j >= 1 ; j--) {
	    pdlpush(cell(clref_val(xtemp1)+j));
	  }
	  recvariant_trie(flag,ASSERT_TRIE_TT);
	  break;
	default:
	  xsb_abort("Bad type tag in one_term_check_ins()");
      }
    }                
    resetpdl;                                                   

    simple_table_undo_bindings;

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

byte * trie_get_returns_for_call(void)
{
  SGFrame call_str_ptr;
  CPtr retskel, term1;
  int i;
  Psc psc_ptr;
  BTNptr ans_root_ptr;
  CPtr cptr;

#ifdef DEBUG_DELAYVAR
  xsb_dbgmsg(">>>> (at the beginning of trie_get_returns_for_call");
  xsb_dbgmsg(">>>> num_vars_in_var_regs = %d)", num_vars_in_var_regs);
#endif

  call_str_ptr = (SGFrame) ptoc_int(1);
  if ((ans_root_ptr = subg_ans_root_ptr(call_str_ptr)) == NULL)
    return (byte *)&fail_inst;
  else {
    retskel = (CPtr)ptoc_tag(2);
    term1 = retskel;
    cptr_deref(term1);
    num_vars_in_var_regs = -1;
    if (isconstr(term1)) {
      psc_ptr = get_str_psc(retskel);
      reg_arrayptr = reg_array -1;
      cptr = (CPtr)cs_val(retskel);
      for (i = get_arity(psc_ptr); i>=1; i--) {
	pushreg(cell(cptr+i));
      }
    }
#ifdef DEBUG_DELAYVAR
    xsb_dbgmsg(">>>> The end of trie_get_returns_for_call ==> go to answer trie");
#endif
    delay_it = 0;  /* Don't delay the answer. */
    return (byte *)ans_root_ptr;
  }
}

/*----------------------------------------------------------------------*/

byte * trie_get_calls(void)
{
   Cell call_term;
   Psc psc_ptr;
   TIFptr tip_ptr;
   BTNptr call_trie_root;
   CPtr cptr;
   int i;

   call_term = ptoc_tag(1);
   if ((psc_ptr = term_psc(call_term)) != NULL) {
     tip_ptr = get_tip(psc_ptr);
     if (tip_ptr == NULL) {
       xsb_abort("get_calls/3 called with non-tabled predicate");
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
	 xsb_dbgmsg(">>>> push one cell");
#endif
	 pushreg(cell(cptr+i));
       }
       return (byte *)call_trie_root;
     }
   }
   else
     return (byte *)&fail_inst;
}

/*----------------------------------------------------------------------*/

static void construct_ret(void)
{
    Pair sym;
    Cell term;
    int  arity, i, new;

    arity = global_num_vars + 1;
    if (arity == 0) {
      ctop_string(3, (char *) ret_psc[0]);
    } else {
      bind_cs((CPtr)ptoc_tag(3), hreg);
      sym = insert("ret", arity, (Psc)flags[CURRENT_MODULE], &new);
      new_heap_functor(hreg, sym->psc_ptr);
      for (i = 0; i < arity; i++) {
	term = (Cell)var_regs[i];
	nbldval(term);
      }
    }
}

/*
 * This function is changed from get_lastnode_and_retskel().  It is the
 * body of *inline* builtin GET_LASTNODE_CS_RETSKEL(LastNode, CallStr,
 * RetSkel). [1/9/1999]
 *
 * This function is called immediately after using the trie intructions
 * to traverse one branch of the call or answer trie.  A side-effect of
 * executing these instructions is that the leaf node of the branch is
 * left in a global variable "Last_Nod_Sav".  Function construct_ret()
 * will be called to construct a term with the return skeleton in the
 * third argument.
 *
 * One reason for writing it so is that it is important that the
 * construction of the return skeleton is an operation that cannot be
 * interrupted by garbage collection.
 */

void get_lastnode_cs_retskel(void)
{
  ctop_int(1, (Integer)Last_Nod_Sav);
  ctop_int(2, (Integer)BTN_Child(Last_Nod_Sav));
  construct_ret();		/* build RetSkel in the 3rd argument */
}

/*----------------------------------------------------------------------*/
/* creates an empty (dummy) answer.					*/
/*----------------------------------------------------------------------*/

ALNptr empty_return(void)
{
    ALNptr i;
  
    /* Used only in one context hence this abuse */
    New_ALN(i,&dummy_ans_node,NULL);
    return i;
}

/*----------------------------------------------------------------------*/
