/* File:      tr_utils.c
** Author(s): Prasad Rao, Juliana Freire, Kostis Sagonas
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


#include <stdio.h>
#include <stdlib.h>

#include "configs/config.h"
#include "debugs/debug.h"

/* Special debug includes */
#include "debugs/debug_tries.h"


#include "auxlry.h"
#include "cell.h"
#include "cinterf.h"
#include "binding.h"
#include "psc.h"
#include "heap.h"
#include "xsb_memory.h"
#include "register.h"
#include "deref.h"
#include "flags.h"
#include "trie_internals.h"
#if (!defined(WAM_TRAIL))
#include "cut.h"
#endif
#include "xmacro.h"
#include "sw_envs.h"
#include "choice.h"
#include "inst.h"
#include "xsberror.h"
#include "io_builtins.h"
#include "trassert.h"
#include "tr_utils.h"
#ifdef CHAT
#include "chat.h"
#endif

/*----------------------------------------------------------------------*/

#define dbind_ref_nth_var(addr,n) dbind_ref(addr,CallVarEnum[n])

#define MAX_VAR_SIZE	200

CPtr Temp_VarPosReg;
CPtr call_vars[MAX_VAR_SIZE];

extern Cell ptoc_tag(int);
#ifdef DEBUG
extern void printterm(Cell, byte, int);
#endif

/*----------------------------------------------------------------------*/

bool has_unconditional_answers(SGFrame subg)
{
  ALNptr node_ptr = subg_answers(subg);
 
  /* Either subgoal has no answers or it is completed */
  /* and its answer list has already been reclaimed. */
  /* In either case, the result is immediately obtained. */
 
  if (node_ptr <= COND_ANSWERS) return (node_ptr == UNCOND_ANSWERS);
 
  /* If the subgoal has not been completed, or is early completed but its */
  /* answer list has not been reclaimed yet, check each of its nodes. */
 
  while (node_ptr) {
    if (is_unconditional_answer(aln_answer_ptr(node_ptr))) return TRUE;
    node_ptr = aln_next_aln(node_ptr);
  }
  return FALSE;
}

/*----------------------------------------------------------------------*/

static int ctr;
static BTNptr  *GNodePtrPtr;

/*----------------------------------------------------------------------*/

#define IsInsibling_rdonly(wherefrom,Found,item) {		\
								\
   LocalNodePtr = wherefrom;					\
   while(LocalNodePtr && (BTN_Symbol(LocalNodePtr) != item))	\
     LocalNodePtr = Sibl(LocalNodePtr);				\
   if ( IsNULL(LocalNodePtr) )					\
     Found = 0;							\
   else {							\
     Paren = LocalNodePtr;					\
     GNodePtrPtr = &(Child(LocalNodePtr));  			\
   }  								\
 }

/*************************************************/
#define one_node_chk(Found,Item) {					\
									\
  BTNptr LocalNodePtr;							\
  Cell item;								\
									\
  item = (Cell) Item;							\
  if ( IsNULL(*GNodePtrPtr) ) {						\
    char message[80];							\
    Found = 0;								\
    sprintf(message,							\
	    "Inconsistency in one_node_chk() (GNodePtrPtr = 0x%p)",	\
	    GNodePtrPtr);				     		\
    xsb_exit(message);							\
  }									\
  if ( IsHashHeader(*GNodePtrPtr) )					\
    GNodePtrPtr =							\
      CalculateBucketForSymbol(((BTHTptr)*GNodePtrPtr),item);		\
  IsInsibling_rdonly(*GNodePtrPtr,Found,item);				\
}
/*********************************/


#define recvariant_call_rdonly(flag) {					      \
									      \
   CPtr  xtemp1;							      \
   int j;								      \
									      \
   while ( (!pdlempty) && flag ) {					      \
     xtemp1 = (CPtr) pdlpop;						      \
     cptr_deref( xtemp1);						      \
     switch(cell_tag(xtemp1)) {						      \
     case FREE:								      \
     case REF1: 							      \
       if (! IsStandardizedVariable(xtemp1)) {				      \
	 *(--Temp_VarPosReg) =(Cell) xtemp1;				      \
	 dbind_ref_nth_var(xtemp1,ctr);					      \
	 one_node_chk(flag,EncodeNewTrieVar(ctr));			      \
	 ctr++;								      \
       }								      \
       else								      \
	 one_node_chk(flag,						      \
		      EncodeTrieVar(IndexOfStandardizedVariable(xtemp1)));    \
       break;								      \
     case STRING:							      \
     case INT:								      \
     case FLOAT: 							      \
       one_node_chk(flag,xtemp1);					      \
       break;								      \
     case LIST:								      \
       one_node_chk(flag,LIST);						      \
       pdlpush( cell(clref_val(xtemp1)+1) );				      \
       pdlpush( cell(clref_val(xtemp1)) );				      \
       break;								      \
     case CS:								      \
       one_node_chk(flag,makecs(follow(cs_val(xtemp1))));		      \
       for(j=get_arity((Psc)follow(cs_val(xtemp1))); j>=1 ; j--)	      \
	 {pdlpush(cell(clref_val(xtemp1) +j));}				      \
       break;								      \
     default: 								      \
       xsb_abort("Bad tag in recvariant_call_rdonly");			      \
     }									      \
   }									      \
   resetpdl;								      \
}

/*----------------------------------------------------------------------*/

/*
 * Totally pirated from variant_call_search().  Notice how the place where
 * the variables and number of them are pushed goes from high to low mem
 * (`call_vars' is a static array).  And the fact that the pointer name is
 * reminiscent of the one used in variant_call_search() (Temp_VarPosReg).
 *
 * `argVector' is a pointer to the argument vector, in low-to-high memory
 * format, beginning with the pointer to the PSC record.
 *
 * `callTrie' is a ptr to the root of the call trie.
 *
 * If an entry is found in the call trie correspnding to the given
 * argument vector, a pointer to the trie representation, in the form of a
 * leaf, is returned.  Otherwise NULL is returned.
 */

static BTNptr variant_call_lookup(int arity, CPtr argVector, BTNptr callTrie) {

    CPtr *xtrbase,xtemp1;
    int i,j,flag = 1;

    xtrbase = trreg;                                
    ctr = 0;                                                    
    Temp_VarPosReg = (CPtr)call_vars + MAX_VAR_SIZE - 1;
    Paren = callTrie;
    if ( IsNonNULL(Paren) )
      GNodePtrPtr = &BTN_Child(Paren);
    else
      flag = 0;

    for (i = 1 ; (i<= arity) && flag ; i++) {                      
      xtemp1 = (CPtr) (argVector + i);            /* Note! */                  
      cptr_deref(xtemp1);                           
      switch (cell_tag(xtemp1)) {                              
        case FREE: case REF1:
	  if (! IsStandardizedVariable(xtemp1)) {
	    *(--Temp_VarPosReg) = (Cell) xtemp1;	
	    dbind_ref_nth_var(xtemp1,ctr);                 
	    one_node_chk(flag,EncodeNewTrieVar(ctr));           
	    ctr++;
	  } else {
	    one_node_chk(flag,EncodeTrieVar(IndexOfStandardizedVariable(xtemp1)));
	  }
	  break;
	case STRING: case INT: case FLOAT:            
	  one_node_chk(flag,xtemp1);                     
	  break;                                              
	case LIST:                                           
	  one_node_chk(flag,LIST);                       
	  pdlpush(cell(clref_val(xtemp1)+1));
	  pdlpush(cell(clref_val(xtemp1)));
	  recvariant_call_rdonly(flag);                      
	  break;                                              
	case CS: 
	  one_node_chk(flag,makecs(follow(cs_val(xtemp1))));     
	  for (j=get_arity((Psc)follow(cs_val(xtemp1))); j>=1; j--) {
	    pdlpush(cell(clref_val(xtemp1)+j));
	  }
	  recvariant_call_rdonly(flag);                      
	  break;                                              
	default:                                             
	  xsb_exit("Bad tag in variant_call_lookup()");
	}
    }                
    if ( (arity == 0) && (flag) ) {
      one_node_chk(flag,(Cell)0);
    }
    resetpdl;
    *(--Temp_VarPosReg) = ctr;
    table_undo_bindings(xtrbase);
    if ( flag )
      return Paren;
    else
      return NULL;
}


CPtr get_subgoal_ptr(Cell callTerm, TIFptr pTIF) {

  int arity;
  BTNptr pTrieRepOfCall;
  extern Psc term_psc(Cell);

  arity = get_arity(term_psc(callTerm));
  pTrieRepOfCall = variant_call_lookup(arity, (CPtr)cs_val(callTerm),
				       TIF_CallTrie(pTIF));
  if ( IsNonNULL(pTrieRepOfCall) )
    return (CPtr)CallTrieLeaf_GetSF(pTrieRepOfCall);
  else
    return NULL;
}

/*----------------------------------------------------------------------*/
/* This function resembles an analogous function in tries.c.  It is
 * supposed to be used only after variant_call_lookup() has been
 * called and the variables in the substitution factor have been left in
 * the Temp_VarPosReg location (containing the arity) and in #arity
 * locations upwards.
 *----------------------------------------------------------------------*/

void construct_ret_for_call(void)
{
    Pair sym;
    Cell var;
    int  arity, i, new;

    arity = cell(Temp_VarPosReg);
    if (arity == 0) {
      ctop_string(3, (char *) ret_psc[0]);
    } else {
      bind_cs((CPtr)ptoc_tag(1), hreg);
      sym = insert("ret", arity, (Psc)flags[CURRENT_MODULE], &new);
      new_heap_functor(hreg, sym->psc_ptr);
      for (i = arity; 0 < i; i--) {
	var = cell(Temp_VarPosReg+i);
	nbldval(var);
      }
    }
}

/*======================================================================*/

/*
 *                     D E L E T I N G   T R I E S
 *                     ===========================
 */


/* Stack for top-down traversing and freeing components of a trie
   -------------------------------------------------------------- */
struct freeing_stack_node{
  BTNptr item;
  struct freeing_stack_node *next;
};

#define push_node(node){\
     struct freeing_stack_node *temp;\
     temp = (struct freeing_stack_node *)malloc(sizeof(struct freeing_stack_node));\
     if (temp == NULL){\
       xsb_exit("Out of Memory");\
     } else {\
       temp->next   = node_stk_top;\
       temp->item   = node;\
       node_stk_top = temp;\
     }\
}

#define pop_node(node){\
    struct freeing_stack_node *temp;\
    if (node_stk_top == NULL) {\
       xsb_dbgmsg("DELETE_PREDICATE_TABLE: pop attempted from NULL");\
       return;\
    }\
    node         = node_stk_top->item;\
    temp         = node_stk_top;\
    node_stk_top = node_stk_top->next;\
    free(temp);\
}


static void free_trie_ht(BTHTptr ht) {

  TrieHT_RemoveFromAllocList(*smBTHT,ht);
  free(BTHT_BucketArray(ht));
  SM_DeallocateStruct(*smBTHT,ht);
}

/*----------------------------------------------------------------------*/
/* Given the address of a node, delete it and all nodes below it in the trie */
/*----------------------------------------------------------------------*/

/*
 * Called only by builtin ABOLISH_TABLE_PREDICATE with the root of a
 * Call Trie.
 */

void delete_predicate_table(BTNptr x)
{
  struct freeing_stack_node *node_stk_top = 0, *call_nodes_top = 0;
  BTNptr node, rnod, *Bkp; 
  BTHTptr ht;

  if (x == NULL)
    return;

  if ( ! ( IsTrieRoot(x) && IsInCallTrie(x) ) ) {
    xsb_dbgmsg("DELETE_PREDICATE_TABLE: object is not the root of a call trie");
    return;
  }

  push_node(x);
  while (node_stk_top != 0) {
    pop_node(node);
    if ( IsHashHeader(node) ) {
      ht = (BTHTptr) node;
      for (Bkp = BTHT_BucketArray(ht);
	   Bkp < BTHT_BucketArray(ht) + BTHT_NumBuckets(ht);
	   Bkp++) {
	if ( IsNonNULL(*Bkp) )
	  push_node(*Bkp);
      }
      free_trie_ht(ht);
    }
    else {
      if ( IsNonNULL(Sibl(node)) )
	push_node(Sibl(node));
      if ( IsNonNULL(BTN_Child(node)) ) {
	if ( IsLeafNode(node) ) {
	  /*
	   * Remove the subgoal frame and its dependent structures
	   */
	  SGFrame pSF = CallTrieLeaf_GetSF(node);
	  if ( IsNonNULL(subg_ans_root_ptr(pSF)) ) {
	    call_nodes_top = node_stk_top;
	    push_node((BTNptr)subg_ans_root_ptr(pSF));
	    while (node_stk_top != call_nodes_top) {
	      pop_node(rnod);
	      if ( IsHashHeader(rnod) ) {
		ht = (BTHTptr) rnod;
		for (Bkp = BTHT_BucketArray(ht);
		     Bkp < BTHT_BucketArray(ht) + BTHT_NumBuckets(ht);
		     Bkp++) {
		  if ( IsNonNULL(*Bkp) )
		    push_node(*Bkp);
		}
		free_trie_ht(ht);
	      }
	      else {
		if (Sibl(rnod)) 
		  push_node(Sibl(rnod));
		if ( ! IsLeafNode(rnod) )
		  push_node(Child(rnod));
		SM_DeallocateStruct(*smBTN,rnod);
	      }
	    }
	  } /* free answer trie */
	  free_answer_list(pSF);
	  FreeProducerSF(pSF);
	} /* is leaf */
	else 
	  push_node(Child(node));
      } /* there is a child of "node" */
      SM_DeallocateStruct(*smBTN,node);
    }
  }
}

/*----------------------------------------------------------------------*/

static int is_hash(BTNptr x) 
{
  if( x == NULL)
    return(0);
  else
    return( IsHashHeader(x) );
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 * Set values for "parent" -- the parent node of "current" -- and
 * "cur_hook" -- an address containing a pointer into "current"'s level
 * in the trie.  If there is no parent node, use the value of
 * "root_hook" to find the level.  If the hook is actually contained in
 * the parent of current (as its child field), then we've ascended as
 * far as we need to go.  Set parent to NULL to indicate this.
 */

static void set_parent_and_node_hook(BTNptr current, BTNptr *root_hook,
				     BTNptr *parent, BTNptr **cur_hook) {

  BTNptr par;

  if ( IsTrieRoot(current) )  /* defend against root having a set parent field */
    par = NULL;
  else {
    par = BTN_Parent(current);
    if ( IsNonNULL(par) && (root_hook == &BTN_Child(par)) )
      par = NULL;    /* stop ascent when hooking node is reached */
  }
  if ( IsNULL(par) )
    *cur_hook = root_hook;
  else
    *cur_hook = &BTN_Child(par);
  *parent = par;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 * Given some non-root node which is *not the first* (or only) sibling,
 * find the node which precedes it in the chain.  Should ONLY be used
 * when deleting trie components.  If a hash table is encountered, then
 * its number of contents is decremented.
 */
static BTNptr get_prev_sibl(BTNptr node)
{
  BTNptr sibling_chain;

  sibling_chain = BTN_Child(BTN_Parent(node));
  if ( IsHashHeader(sibling_chain) ) {
    BTHTptr ht = (BTHTptr)sibling_chain;
    BTHT_NumContents(ht)--;
    sibling_chain = *CalculateBucketForSymbol(ht,BTN_Symbol(node));
  }
  while(sibling_chain != NULL){
    if (BTN_Sibling(sibling_chain) == node)
      return(sibling_chain);
    sibling_chain = BTN_Sibling(sibling_chain);
  }  
  xsb_abort("Error in get_previous_sibling");
  return(NULL);
}

/*----------------------------------------------------------------------*/
/* deletes and reclaims a whole branch in the return trie               */
/*----------------------------------------------------------------------*/

/*
 * Delete a branch in the trie down from node `lowest_node_in_branch'
 * up to the level pointed to by the hook location, as pointed to by
 * `hook'.  Under normal use, the "hook" is either for the root of the
 * trie, or for the first level of the trie (is a pointer to the child
 * field of the root).
 */

void delete_branch(BTNptr lowest_node_in_branch, BTNptr *hook) {

  int num_left_in_hash;
  BTNptr prev, parent_ptr, *y1, *z;


  while ( IsNonNULL(lowest_node_in_branch) && 
	  ( Contains_NOCP_Instr(lowest_node_in_branch) ||
	    IsTrieRoot(lowest_node_in_branch) ) ) {
    /*
     *  Walk up a path with no branches, i.e., the nodes along this path
     *  have no siblings.  We know this because the instruction in the
     *  node is of the no_cp variety.
     */
    set_parent_and_node_hook(lowest_node_in_branch,hook,&parent_ptr,&y1);
    if (is_hash(*y1)) {
      z = CalculateBucketForSymbol((BTHTptr)(*y1),
				   BTN_Symbol(lowest_node_in_branch));
      if ( *z != lowest_node_in_branch )
	xsb_dbgmsg("DELETE_BRANCH: trie node not found in hash table");
      *z = NULL;
      num_left_in_hash = --BTHT_NumContents((BTHTptr)*y1);
      if (num_left_in_hash  > 0) {
	/*
	 * `lowest_node_in_branch' has siblings, even though they are not in
	 * the same chain.  Therefore we cannot delete the parent, and so
	 * we're done.
	 */
	SM_DeallocateStruct(*smBTN,lowest_node_in_branch);
	return;
      }
      else
	free_trie_ht((BTHTptr)(*y1));
    }
    /*
     *  Remove this node and continue.
     */
    SM_DeallocateStruct(*smBTN,lowest_node_in_branch);
    lowest_node_in_branch = parent_ptr;
  }
  if (lowest_node_in_branch == NULL)
    *hook = 0;
  else {
    if (Contains_TRY_Instr(lowest_node_in_branch)) {
      /* Alter sibling's instruction:  trust -> no_cp  retry -> try */
      Instr(Sibl(lowest_node_in_branch)) =
	Instr(Sibl(lowest_node_in_branch)) -1;
      y1 = &BTN_Child(BTN_Parent(lowest_node_in_branch));
      if (is_hash(*y1)) {
	z = CalculateBucketForSymbol((BTHTptr)(*y1),
				     BTN_Symbol(lowest_node_in_branch));
	num_left_in_hash = --BTHT_NumContents((BTHTptr)*y1);
      }
      else
	z = y1;
      *z = Sibl(lowest_node_in_branch);      
    }
    else { /* not the first in the sibling chain */
      prev = get_prev_sibl(lowest_node_in_branch);      
      Sibl(prev) = Sibl(lowest_node_in_branch);
      if (Contains_TRUST_Instr(lowest_node_in_branch))
	Instr(prev) -= 2; /* retry -> trust ; try -> nocp */
    }
    SM_DeallocateStruct(*smBTN,lowest_node_in_branch);
  }
}

/*----------------------------------------------------------------------*/

void safe_delete_branch(BTNptr lowest_node_in_branch) {

  byte choicepttype;

  MakeStatusDeleted(lowest_node_in_branch);
  choicepttype = 0x3 & Instr(lowest_node_in_branch);
  Instr(lowest_node_in_branch) = choicepttype | trie_no_cp_fail;
}

void undelete_branch(BTNptr lowest_node_in_branch) {

   byte choicepttype; 
   byte typeofinstr;

   if( IsDeletedNode(lowest_node_in_branch) ){
     choicepttype = 0x3 &  Instr(lowest_node_in_branch);
     typeofinstr = (~0x3) & BTN_Status(lowest_node_in_branch);

     Instr(lowest_node_in_branch) = choicepttype | typeofinstr;
     MakeStatusValid(lowest_node_in_branch);
   }
   else
     xsb_dbgmsg("Attempt to undelete a node that is not deleted");
 }

/*----------------------------------------------------------------------*/

void delete_trie(BTNptr root) {

  BTNptr sib, chil;  

  if ( IsNonNULL(root) ) {
    if ( IsHashHeader(root) ) {
      BTHTptr hhdr;
      BTNptr *base, *cur;

      hhdr = (BTHTptr)root;
      base = BTHT_BucketArray(hhdr);
      for ( cur = base; cur < base + BTHT_NumBuckets(hhdr); cur++ )
	delete_trie(*cur);
      free_trie_ht(hhdr);
    }
    else {
      sib  = Sibl(root);
      chil = Child(root);      
      /* Child nodes == NULL is not the correct test*/
      if (IsLeafNode(root)) {
	if (chil != NULL)
	  xsb_exit("Anomaly in delete_trie !");
      }
      else
	delete_trie(chil);
      delete_trie(sib);
      SM_DeallocateStruct(*smBTN,root);
    }
  }
}


/*======================================================================*/

/*
 *                  A N S W E R   O P E R A T I O N S
 *                  =================================
 */

/*----------------------------------------------------------------------*/

/* This does not reclaim space for deleted nodes, only marks
 * the node as deleted and changes the try instruction to fail.
 * The deleted node is then linked into the del_nodes_list
 * in the completion stack.
 */
void delete_return(BTNptr l, SGFrame sg_frame) 
{
  ALNptr a, n, next;
  NLChoice c;
#ifdef CHAT
  chat_init_pheader chat_ptr;
#else
#ifdef LOCAL_EVAL
  TChoice  tc;
#endif
#endif

#ifdef DEBUG_RECLAIM_DEL_RET
    xsb_dbgmsg("DELETE_NODE: %d - Par: %d", l, Parent(l));
#endif
  safe_delete_branch(l);
  if (!is_completed(sg_frame)) {
    n = subg_ans_list_ptr(sg_frame);
    /* Find previous sibling -pvr */
    while (aln_answer_ptr(aln_next_aln(n)) != l) {
      n = aln_next_aln(n);/* if a is not in that list a core dump will result */
    }
    if (n == NULL) {
      xsb_exit("Error in delete_return()");
    }
    a               = aln_next_aln(n);
    next            = aln_next_aln(a);
    aln_next_aln(a) = compl_del_ret_list(subg_compl_stack_ptr(sg_frame));
    compl_del_ret_list(subg_compl_stack_ptr(sg_frame)) = a;    

    aln_next_aln(n) = next;
    
    /* Make consumed answer field of consumers point to
       previous sibling if they point to a deleted answer */
#ifdef CHAT
    chat_ptr = (chat_init_pheader)compl_cons_copy_list(subg_compl_stack_ptr(sg_frame));
    while (chat_ptr != NULL) {
      c = (NLChoice)(&chat_get_cons_start(chat_ptr));
      if (nlcp_trie_return(c) == a) {
	nlcp_trie_return(c) = n;
      }
      chat_ptr = (chat_init_pheader)nlcp_prevlookup(c);
    }
#else
    c = (NLChoice) subg_asf_list_ptr(sg_frame);
    while(c != NULL){
      if(nlcp_trie_return(c) == a){
	nlcp_trie_return(c) = n;
      }
      c = (NLChoice)nlcp_prevlookup(c);
    }
#endif

#if (defined(LOCAL_EVAL) && !defined(CHAT))
      /* if gen-cons points to deleted answer, make it
       * point to previous sibling */
      tc = (TChoice)subg_cp_ptr(sg_frame);
      if (tcp_trie_return(tc) == a) {
	tcp_trie_return(tc) = n;
      }
#endif
   
    aln_next_aln(n) = next;
    if(next == NULL){ /* last answer */
      subg_ans_list_tail(sg_frame) = n;
    }      
  }
}

/*----------------------------------------------------------------------*/
/* Given a tabled subgoal, go through its list of deleted nodes (in the
 * completion stack), and reclaim the leaves and corresponding branches
 *----------------------------------------------------------------------*/

void  reclaim_del_ret_list(SGFrame sg_frame) {
  ALNptr x,y;
  
  x = compl_del_ret_list(subg_compl_stack_ptr(sg_frame));
  
  while (x != NULL) {
    y = x;
    x = aln_next_aln(x);
    delete_branch(aln_answer_ptr(y), &subg_ans_root_ptr(sg_frame));
    SM_DeallocateStruct(smALN,y);
  }
}
 
/*----------------------------------------------------------------------*/

/*
**   Used in aggregs.P to implement aggregates.
**   Takes:   breg (the place where choice point is saved) and arity.  
**   Returns: subgoal skeleton (i.e., ret(X,Y,Z), where X,Y,Z are all the 
**    	      	                distinct variables in the subgoal);
**   	      Pointer to the subgoal.
*/

void breg_retskel(void)
{
    Pair    sym;
    Cell    term;
    SGFrame sg_frame;
    CPtr    tcp, cptr, where;
    int     new, i;
#ifndef CHAT
    int     arity;
#endif
    Integer breg_offset, Nvars;

    breg_offset = ptoc_int(1);
    tcp = (CPtr)((Integer)(tcpstack.high) - breg_offset);
    sg_frame = (SGFrame)(tcp_subgoal_ptr(tcp));
#ifdef CHAT
    where = compl_hreg(subg_compl_stack_ptr(sg_frame));
    Nvars = int_val(cell(where));
    cptr = where - Nvars - 1;
#else
    arity = ptoc_int(2);
    where = tcp + TCP_SIZE + (Cell)arity;
    Nvars = int_val(cell(where));
    cptr = where + Nvars;
#endif
    if (Nvars == 0) {
      ctop_string(3, (char *) ret_psc[0]);
    } else {
/*
      sreg = hreg;
      bind_cs((CPtr)term, sreg);
      sym = insert("ret", Nvars, (Psc)flags[CURRENT_MODULE], &new);
      new_heap_functor(sreg, sym->psc_ptr);
#ifdef CHAT
      for (i = Nvars; i > 0; i--) {
	bind_copy(sreg, (Cell)(*(CPtr)(cptr+i)));
#else
      for (i = 0; i < Nvars; i++) {
	bind_copy(sreg, (Cell)(*(CPtr)(cptr-i)));
#endif
	sreg++;
      }
      hreg = sreg;
*/
      bind_cs((CPtr)ptoc_tag(3), hreg);
      sym = insert("ret", Nvars, (Psc)flags[CURRENT_MODULE], &new);
      new_heap_functor(hreg, sym->psc_ptr);
#ifdef CHAT
      for (i = Nvars; i > 0; i--) {
	term = (Cell)(*(CPtr)(cptr+i));
#else
      for (i = 0; i < Nvars; i++) {
	term = (Cell)(*(CPtr)(cptr-i));
#endif
        nbldval(term);
      }
    }
    ctop_int(4, (Integer)sg_frame);
}


/*======================================================================*/

/*
 *                    I N T E R N E D   T R I E S
 *                    ===========================
 */

#define ADJUST_SIZE 100

BTNptr *Set_ArrayPtr = NULL;
/*
 * first_free_set is the index of the first deleted set.  The deleted
 * tries are deleted in builtin DELETE_TRIE, and the corresponding
 * elements in Set_ArrayPtr are linked to form a list.  So
 * Set_ArrayPtr[first_free_set] contains the index of the next deleted
 * set, ..., the last one contains 0.  If first_free_set == 0, that
 * means no free set available.
 */
int first_free_set = 0;
int Set_ArraySz = 100;
/*
 * num_sets is the number of sets have been used (including the fixed
 * trie, Set_ArrayPtr[0] (see trie_intern/3)).  It is also the index for
 * the next element to use when no free element is available.
 */
int num_sets = 1;

/*----------------------------------------------------------------------*/

/* Allocate an array of handles to interned tries. */

void init_newtrie(void)
{
  Set_ArrayPtr = (BTNptr *) calloc(Set_ArraySz,sizeof(BTNptr));
}

/*----------------------------------------------------------------------*/

/* Returns a handle to an unused interned trie. */

void newtrie(void)
{
  int i;
  
  if (first_free_set != 0) {	/* a free set is available */
    i = first_free_set;		/* save it in i */
    ctop_int(1, first_free_set);
    first_free_set = (long) Set_ArrayPtr[first_free_set] >> 2;
    Set_ArrayPtr[i] = NULL;	/* must be reset to NULL */
  }
  else {
    if (num_sets == Set_ArraySz) { /* run out of elements */
      BTNptr *temp_arrayptr;

      temp_arrayptr = Set_ArrayPtr;
      Set_ArraySz += ADJUST_SIZE;  /* adjust the array size */
      Set_ArrayPtr = (BTNptr *) calloc(Set_ArraySz ,sizeof(BTNptr));
      if (Set_ArrayPtr == NULL)
	xsb_exit("Out of memory in new_trie/1");
      for (i = 0; i < num_sets; i++)
	Set_ArrayPtr[i] = temp_arrayptr[i];
      free(temp_arrayptr);
    }
    ctop_int(1, num_sets);
    num_sets++;
  }
}

/*----------------------------------------------------------------------*/

void trie_intern(void)
{
  prolog_term term;
  int RootIndex;
  int flag;
  BTNptr Leaf;

  term = ptoc_tag(1);
  RootIndex = ptoc_int(2);

#ifdef DEBUG_INTERN
  printf("Interning ");
  printterm(term,1,25);
  printf("In position %d\n", RootIndex);
#endif
  switch_to_trie_assert;
  Leaf = whole_term_chk_ins(term,&(Set_ArrayPtr[RootIndex]),&flag);
  switch_from_trie_assert;
  
  ctop_int(3,(Integer)Leaf);
  ctop_int(4,flag);
#ifdef DEBUG_INTERN
  printf("Exit flag %d\n",flag);
#endif
}

/*----------------------------------------------------------------------*/

int trie_interned(void)
{
  int RootIndex;
  int ret_val = FALSE;
  Cell Leafterm, trie_term;

  trie_term =  ptoc_tag(1);
  RootIndex = ptoc_int(2);
  Leafterm = ptoc_tag(3);
  
  /*
   * Only if Set_ArrayPtr[RootIndex] is a valid BTNptr can we run this
   * builtin.  That means Set_ArrayPtr[RootIndex] can neither be NULL,
   * nor a deleted set (deleted by builtin delete_trie/1).
   */
  if ((Set_ArrayPtr[RootIndex] != NULL) &&
      (!((long) Set_ArrayPtr[RootIndex] & 0x3))) {
    deref(trie_term);
    deref(Leafterm);
    if (isref(Leafterm)) {  
      reg_arrayptr = reg_array -1;
      num_vars_in_var_regs = -1;
      pushreg(trie_term);
      pcreg = (byte *)Set_ArrayPtr[RootIndex];
      ret_val =  TRUE;
    }
    else{
      xsb_exit("Not yet grd Leafterm!");
    }
  }
  return(ret_val);
}

/*----------------------------------------------------------------------*/

/*
 * This is builtin #162: TRIE_DISPOSE(+ROOT, +LEAF), to dispose a branch
 * of the trie rooted at Set_ArrayPtr[ROOT].
 */

void trie_dispose(void)
{
  BTNptr Leaf;
  long Rootidx;

  Rootidx = ptoc_int(1);
  Leaf = (BTNptr)ptoc_int(2);
  switch_to_trie_assert;
  delete_branch(Leaf, &(Set_ArrayPtr[Rootidx]));
  switch_from_trie_assert;
}

/*----------------------------------------------------------------------*/

#define DELETED_SET 1

void delete_interned_trie(int tmpval) {
  /*
   * We can only delete a valid BTNptr, so that only those sets
   * that were used before can be put into the free set list.
   */
  if ((Set_ArrayPtr[tmpval] != NULL) &&
      (!((long) Set_ArrayPtr[tmpval] & 0x3))) {
    switch_to_trie_assert;
    delete_trie(Set_ArrayPtr[tmpval]);
    switch_from_trie_assert;
    /*
     * Save the value of first_free_set into Set_ArrayPtr[tmpval].
     * Some simple encoding is needed, because in trie_interned/4 we
     * have to know this set is already deleted.
     */
    Set_ArrayPtr[tmpval] = (BTNptr) (first_free_set << 2 | DELETED_SET);
    first_free_set = tmpval;
  }
}
