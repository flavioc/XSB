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


#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>
#include <stdlib.h>

/* Special debug includes */
#include "debugs/debug_tries.h"


#include "auxlry.h"
#include "cell_xsb.h"
#include "cinterf.h"
#include "binding.h"
#include "psc_xsb.h"
#include "heap_xsb.h"
#include "memory_xsb.h"
#include "register.h"
#include "deref.h"
#include "flags_xsb.h"
#include "trie_internals.h"
#include "tst_aux.h"
#include "cut_xsb.h"
#include "macro_xsb.h"
#include "sw_envs.h"
#include "choice.h"
#include "inst_xsb.h"
#include "error_xsb.h"
#include "io_builtins_xsb.h"
#include "trassert.h"
#include "tr_utils.h"
#include "tst_utils.h"
#include "subp.h"
#include "debug_xsb.h"

/*----------------------------------------------------------------------*/

#define MAX_VAR_SIZE	200

#include "ptoc_tag_xsb_i.h"
#include "term_psc_xsb_i.h"

/*----------------------------------------------------------------------*/

xsbBool has_unconditional_answers(VariantSF subg)
{
  ALNptr node_ptr = subg_answers(subg);
 
  /* Either subgoal has no answers or it is completed */
  /* and its answer list has already been reclaimed. */
  /* In either case, the result is immediately obtained. */
 
  if (node_ptr <= COND_ANSWERS) return (node_ptr == UNCOND_ANSWERS);
 
  /* If the subgoal has not been completed, or is early completed but its */
  /* answer list has not been reclaimed yet, check each of its nodes. */
 
  while (node_ptr) {
    if (is_unconditional_answer(ALN_Answer(node_ptr))) return TRUE;
    node_ptr = ALN_Next(node_ptr);
  }
  return FALSE;
}

/*----------------------------------------------------------------------*/

/*
 * Given a subgoal of a variant predicate, returns its subgoal frame
 * if it has a table entry; returns NULL otherwise.  If requested, the
 * answer template is constructed on the heap as a ret/n term and
 * passed back via the last argument.
 */

VariantSF get_variant_sf(Cell callTerm, TIFptr pTIF, Cell *retTerm) {

  int arity;
  BTNptr root, leaf;
  Cell callVars[MAX_VAR_SIZE + 1];

  root = TIF_CallTrie(pTIF);
  if ( IsNULL(root) )
    return NULL;

  arity = get_arity(TIF_PSC(pTIF));
  leaf = variant_trie_lookup(root, arity, clref_val(callTerm) + 1, callVars);
  if ( IsNULL(leaf) )
    return NULL;
  if ( IsNonNULL(retTerm) )
    *retTerm = build_ret_term(callVars[0], &callVars[1]);
  return ( CallTrieLeaf_GetSF(leaf) );
}

/*----------------------------------------------------------------------*/

/*
 * Given a subgoal of a subsumptive predicate, returns the subgoal
 * frame of some producing table entry which subsumes it; returns NULL
 * otherwise.  The answer template with respect to this producer entry
 * is constructed on the heap as a ret/n term and passed back via the
 * last argument.
 */

SubProdSF get_subsumer_sf(Cell callTerm, TIFptr pTIF, Cell *retTerm) {

  BTNptr root, leaf;
  int arity;
  TriePathType path_type;
  SubProdSF sf;
  Cell ansTmplt[MAX_VAR_SIZE + 1];

  root = TIF_CallTrie(pTIF);
  if ( IsNULL(root) )
    return NULL;

  arity = get_arity(TIF_PSC(pTIF));
  leaf = subsumptive_trie_lookup(root, arity, clref_val(callTerm) + 1,
				 &path_type, ansTmplt);
  if ( IsNULL(leaf) )
    return NULL;
  sf = (SubProdSF)CallTrieLeaf_GetSF(leaf);
  if ( IsProperlySubsumed(sf) ) {
    sf = conssf_producer(sf);
    construct_answer_template(callTerm, sf, ansTmplt);
  }
  if ( IsNonNULL(retTerm) )
    *retTerm = build_ret_term(ansTmplt[0], &ansTmplt[1]);
  return ( sf );
}
  
/*----------------------------------------------------------------------*/

BTNptr get_trie_root(BTNptr node) {

  while ( IsNonNULL(node) ) {
    if ( IsTrieRoot(node) )
      return node;
    node = BTN_Parent(node);
  }
  /*
   * If the trie is constructed correctly, processing will not reach
   * here, other than if 'node' was originally NULL.
   */
  return NULL;
}

/*----------------------------------------------------------------------*/

/*
 * Given a vector of terms and their number, N, builds a ret/N structure
 * on the heap containing those terms.  Returns this constructed term.
 */

Cell build_ret_term(int arity, Cell termVector[]) {

  Pair sym;
  CPtr ret_term;
  int  i, is_new;

  if ( arity == 0 )
    return makestring(get_ret_string());  /* return as a term */
  else {
    ret_term = hreg;  /* pointer to where ret(..) will be built */
    sym = insert("ret", (byte)arity, (Psc)flags[CURRENT_MODULE], &is_new);
    new_heap_functor(hreg, pair_psc(sym));
    for ( i = 0; i < arity; i++ )
      nbldval(termVector[i]);
    return makecs(ret_term);  /* return as a term */
  }
}

/*----------------------------------------------------------------------*/

/*
 * Create the answer template for a subsumed call with the given producer.
 * The template is stored in an array supplied by the caller.
 */

void construct_answer_template(Cell callTerm, SubProdSF producer,
			       Cell templ[]) {

  Cell subterm, symbol;
  int  sizeAnsTmplt;

  /*
   * Store the symbols along the path of the more general call.
   */
  SymbolStack_ResetTOS;
  SymbolStack_PushPath(subg_leaf_ptr(producer));

  /*
   * Push the arguments of the subsumed call.
   */
  TermStack_ResetTOS;
  TermStack_PushFunctorArgs(callTerm);

  /*
   * Create the answer template while we process.  Since we know we have a
   * more general subsuming call, we can greatly simplify the "matching"
   * process: we know we either have exact matches of non-variable symbols
   * or a variable paired with some subterm of the current call.
   */
  sizeAnsTmplt = 0;
  while ( ! TermStack_IsEmpty ) {
    TermStack_Pop(subterm);
    XSB_Deref(subterm);
    SymbolStack_Pop(symbol);
    if ( IsTrieVar(symbol) && IsNewTrieVar(symbol) )
      templ[++sizeAnsTmplt] = subterm;
    else if ( IsTrieFunctor(symbol) )
      TermStack_PushFunctorArgs(subterm)
    else if ( IsTrieList(symbol) )
      TermStack_PushListArgs(subterm)
  }
  templ[0] = sizeAnsTmplt;
}

/*----------------------------------------------------------------------*/

/*
 * Given a term representing a tabled call, determine whether it is
 * recorded in the Call Table.  If it is, then return a pointer to its
 * subgoal frame and construct on the heap the answer template required
 * to retrieve answers for this call.  Place a reference to this term in
 * the location pointed to by the second argument.
 */

VariantSF get_call(Cell callTerm, Cell *retTerm) {

  Psc  psc;
  TIFptr tif;
  int arity;
  BTNptr root, leaf;
  VariantSF sf;
  Cell callVars[MAX_VAR_SIZE + 1];


  psc = term_psc(callTerm);
  if ( IsNULL(psc) ) {
    err_handle(TYPE, 1, "get_call", 3, "callable term", callTerm);
    return NULL;
  }

  tif = get_tip(psc);
  if ( IsNULL(tif) )
    xsb_abort("Predicate %s/%d is not tabled", get_name(psc), get_arity(psc));

  root = TIF_CallTrie(tif);
  if ( IsNULL(root) )
    return NULL;

  arity = get_arity(psc);
  leaf = variant_trie_lookup(root, arity, clref_val(callTerm) + 1, callVars);
  if ( IsNULL(leaf) )
    return NULL;
  else {
    sf = CallTrieLeaf_GetSF(leaf);
    if ( IsProperlySubsumed(sf) )
      construct_answer_template(callTerm, conssf_producer(sf), callVars);
    *retTerm = build_ret_term(callVars[0],&callVars[1]);
    return sf;
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
       xsb_dbgmsg((LOG_DEBUG,"DELETE_PREDICATE_TABLE: pop attempted from NULL"));\
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


static void delete_variant_table(BTNptr x) {

  struct freeing_stack_node *node_stk_top = 0, *call_nodes_top = 0;
  BTNptr node, rnod, *Bkp; 
  BTHTptr ht;


  if ( IsNULL(x) )
    return;

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
      if ( IsNonNULL(BTN_Sibling(node)) )
	push_node(BTN_Sibling(node));
      if ( IsNonNULL(BTN_Child(node)) ) {
	if ( IsLeafNode(node) ) {
	  /*
	   * Remove the subgoal frame and its dependent structures
	   */
	  VariantSF pSF = CallTrieLeaf_GetSF(node);
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
		if (BTN_Sibling(rnod)) 
		  push_node(BTN_Sibling(rnod));
		if ( ! IsLeafNode(rnod) )
		  push_node(BTN_Child(rnod));
		SM_DeallocateStruct(*smBTN,rnod);
	      }
	    }
	  } /* free answer trie */
	  free_answer_list(pSF);
	  FreeProducerSF(pSF);
	} /* is leaf */
	else 
	  push_node(BTN_Child(node));
      } /* there is a child of "node" */
      SM_DeallocateStruct(*smBTN,node);
    }
  }
}

/*----------------------------------------------------------------------*/
/* Delete the table for a given tabled predicate, specified as a TIF    */
/*----------------------------------------------------------------------*/

void delete_predicate_table(TIFptr tif) {

  if ( IsVariantPredicate(tif) )
    delete_variant_table(TIF_CallTrie(tif));
  else
    delete_subsumptive_table(tif);
  TIF_CallTrie(tif) = NULL;
  TIF_Subgoals(tif) = NULL;
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
	xsb_dbgmsg((LOG_DEBUG,"DELETE_BRANCH: trie node not found in hash table"));
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
      BTN_Instr(BTN_Sibling(lowest_node_in_branch)) =
	BTN_Instr(BTN_Sibling(lowest_node_in_branch)) -1;
      y1 = &BTN_Child(BTN_Parent(lowest_node_in_branch));
      if (is_hash(*y1)) {
	z = CalculateBucketForSymbol((BTHTptr)(*y1),
				     BTN_Symbol(lowest_node_in_branch));
	num_left_in_hash = --BTHT_NumContents((BTHTptr)*y1);
      }
      else
	z = y1;
      *z = BTN_Sibling(lowest_node_in_branch);      
    }
    else { /* not the first in the sibling chain */
      prev = get_prev_sibl(lowest_node_in_branch);      
      BTN_Sibling(prev) = BTN_Sibling(lowest_node_in_branch);
      if (Contains_TRUST_Instr(lowest_node_in_branch))
	BTN_Instr(prev) -= 2; /* retry -> trust ; try -> nocp */
    }
    SM_DeallocateStruct(*smBTN,lowest_node_in_branch);
  }
}

/*----------------------------------------------------------------------*/

void safe_delete_branch(BTNptr lowest_node_in_branch) {

  byte choicepttype;

  MakeStatusDeleted(lowest_node_in_branch);
  choicepttype = 0x3 & BTN_Instr(lowest_node_in_branch);
  BTN_Instr(lowest_node_in_branch) = choicepttype | trie_no_cp_fail;
}

void undelete_branch(BTNptr lowest_node_in_branch) {

   byte choicepttype; 
   byte typeofinstr;

   if( IsDeletedNode(lowest_node_in_branch) ){
     choicepttype = 0x3 &  BTN_Instr(lowest_node_in_branch);
     /* Status contains the original instruction that was in that trie node.
	here we extract the original instruction and the next statement
	makes it into the instruction associated with that node. */
     typeofinstr = (~0x3) & BTN_Status(lowest_node_in_branch);

     BTN_Instr(lowest_node_in_branch) = choicepttype | typeofinstr;
     /* This only sets the status field. It is also necessary to set the
	instruction field correctly, which is done above. */
     MakeStatusValid(lowest_node_in_branch);
   }
   else
     /* This is permitted, because we might bt_delete, then insert
	(non-backtrackably) and then backtrack.
     */
     xsb_dbgmsg((LOG_INTERN, "Undeleting a node that is not deleted"));
}


/*----------------------------------------------------------------------*/

#define DELETE_TRIE_STACK_INIT 100
#define MAX_DELETE_TRIE_STACK_SIZE 1000
#define DT_NODE 0
#define DT_DS 1
#define DT_HT 2

char *delete_trie_op = NULL;
BTNptr *delete_trie_node = NULL;
BTHTptr *delete_trie_hh = NULL;

int trie_op_size, trie_node_size, trie_hh_size;

#define push_delete_trie_node(node,op) {\
  trie_op_top++;\
  if (trie_op_top >= trie_op_size) {\
    trie_op_size = 2*trie_op_size;\
    delete_trie_op = (char *)realloc(delete_trie_op,trie_op_size*sizeof(char));\
    if (!delete_trie_op) xsb_exit("out of space for deleting trie");\
    /*xsb_dbgmsg((LOG_DEBUG,"realloc delete_trie_op to %d",trie_op_size));*/\
  }\
  delete_trie_op[trie_op_top] = op;\
  trie_node_top++;\
  if (trie_node_top >= trie_node_size) {\
    trie_node_size = 2*trie_node_size;\
    delete_trie_node = (BTNptr *)realloc(delete_trie_node,trie_node_size*sizeof(BTNptr));\
    if (!delete_trie_node) xsb_exit("out of space for deleting trie");\
    /*xsb_dbgmsg((LOG_DEBUG,"realloc delete_trie_node to %d",trie_node_size));*/\
  }\
  delete_trie_node[trie_node_top] = node;\
}  
#define push_delete_trie_hh(hh) {\
  trie_op_top++;\
  if (trie_op_top >= trie_op_size) {\
    trie_op_size = 2*trie_op_size;\
    delete_trie_op = (char *)realloc(delete_trie_op,trie_op_size*sizeof(char));\
    if (!delete_trie_op) xsb_exit("out of space for deleting trie");\
    /*xsb_dbgmsg((LOG_DEBUG,"realloc delete_trie_op to %d",trie_op_size));*/\
  }\
  delete_trie_op[trie_op_top] = DT_HT;\
  trie_hh_top++;\
  if (trie_hh_top >= trie_hh_size) {\
    trie_hh_size = 2*trie_hh_size;\
    delete_trie_hh = (BTHTptr *)realloc(delete_trie_hh,trie_hh_size*sizeof(BTHTptr));\
    if (!delete_trie_hh) xsb_exit("out of space for deleting trie");\
    /*xsb_dbgmsg((LOG_DEBUG,"realloc delete_trie_hh to %d",trie_hh_size));*/\
  }\
  delete_trie_hh[trie_hh_top] = hh;\
}  


void delete_trie(BTNptr iroot) {

  BTNptr root, sib, chil;  
  int trie_op_top = 0;
  int trie_node_top = 0;
  int trie_hh_top = -1;

  if (!delete_trie_op) {
    delete_trie_op = (char *)malloc(DELETE_TRIE_STACK_INIT*sizeof(char));
    delete_trie_node = (BTNptr *)malloc(DELETE_TRIE_STACK_INIT*sizeof(BTNptr));
    delete_trie_hh = (BTHTptr *)malloc(DELETE_TRIE_STACK_INIT*sizeof(BTHTptr));
    trie_op_size = trie_node_size = trie_hh_size = DELETE_TRIE_STACK_INIT;
  }

  delete_trie_op[0] = 0;
  delete_trie_node[0] = iroot;
  while (trie_op_top >= 0) {
    /*    xsb_dbgmsg((LOG_DEBUG,"top %d %d %d %p",trie_op_top,trie_hh_top,
	  delete_trie_op[trie_op_top],delete_trie_node[trie_node_top])); */
    switch (delete_trie_op[trie_op_top--]) {
    case DT_DS:
      root = delete_trie_node[trie_node_top--];
      SM_DeallocateStruct(*smBTN,root);
      break;
    case DT_HT:
      free_trie_ht(delete_trie_hh[trie_hh_top--]);
      break;
    case DT_NODE:
      root = delete_trie_node[trie_node_top--];
      if ( IsNonNULL(root) ) {
	if ( IsHashHeader(root) ) {
	  BTHTptr hhdr;
	  BTNptr *base, *cur;
	  hhdr = (BTHTptr)root;
	  base = BTHT_BucketArray(hhdr);
	  push_delete_trie_hh(hhdr);
	  for ( cur = base; cur < base + BTHT_NumBuckets(hhdr); cur++ ) {
	    if (IsNonNULL(*cur)) {
	      push_delete_trie_node(*cur,DT_NODE);
	    }
	  }
	}
	else {
	  sib  = BTN_Sibling(root);
	  chil = BTN_Child(root);      
	  /* Child nodes == NULL is not the correct test*/
	  if (IsLeafNode(root)) {
	    if (IsNonNULL(chil))
	      xsb_exit("Anomaly in delete_trie !");
	    push_delete_trie_node(root,DT_DS);
	    if (IsNonNULL(sib)) {
	      push_delete_trie_node(sib,DT_NODE);
	    }
	  }
	  else {
	    push_delete_trie_node(root,DT_DS);
	    if (IsNonNULL(sib)) {
	      push_delete_trie_node(sib,DT_NODE);
	    }
	    if (IsNonNULL(chil)) {
	      push_delete_trie_node(chil,DT_NODE);
	    }
	  }
	}
      } else
	printf("null node");
      break;
    }
  }
  if (trie_op_size > MAX_DELETE_TRIE_STACK_SIZE) {
    free(delete_trie_op); delete_trie_op = NULL;
    free(delete_trie_node); delete_trie_node = NULL;
    free(delete_trie_hh); delete_trie_hh = NULL;
    trie_op_size = 0; 
  }
}


/*======================================================================*/

/*
 *                  A N S W E R   O P E R A T I O N S
 *                  =================================
 */

/*----------------------------------------------------------------------*/

/*
 * This does not reclaim space for deleted nodes, only marks
 * the node as deleted and changes the try instruction to fail.
 * The deleted node is then linked into the del_nodes_list
 * in the completion stack.
 */
void delete_return(BTNptr l, VariantSF sg_frame) 
{
  ALNptr a, n, next;
  NLChoice c;
  int groundcall = FALSE;
#ifdef LOCAL_EVAL
  TChoice  tc;
#endif

    xsb_dbgmsg((LOG_INTERN, "DELETE_NODE: %d - Par: %d", l, BTN_Parent(l)));

    /* deleting an answer makes it false, so we have to deal with 
       delay lists */
    if (is_conditional_answer(l)) {
      ASI asi = Delay(l);
      release_all_dls(asi);
      /* TLS 12/00 changed following line from 
	 (l == subg_ans_root_ptr(sg_frame) && ..
	 so that negation failure simplification is properly performed */
      if (l == BTN_Child(subg_ans_root_ptr(sg_frame)) &&
	  IsEscapeNode(l))
	groundcall=TRUE; /* do it here, when l is still valid */
    }

  if (is_completed(sg_frame)) {
    safe_delete_branch(l);
  } else {
    delete_branch(l,&subg_ans_root_ptr(sg_frame));
    n = subg_ans_list_ptr(sg_frame);
    /* Find previous sibling -pvr */
    while (ALN_Answer(ALN_Next(n)) != l) {
      n = ALN_Next(n);/* if a is not in that list a core dump will result */
    }
    if (n == NULL) {
      xsb_exit("Error in delete_return()");
    }
    a               = ALN_Next(n);
    next            = ALN_Next(a);
    ALN_Answer(a)   = NULL; /* since we eagerly release trie nodes, this is
			       necessary to keep garbage collection sane */
    ALN_Next(a) = compl_del_ret_list(subg_compl_stack_ptr(sg_frame));
    compl_del_ret_list(subg_compl_stack_ptr(sg_frame)) = a;    

    ALN_Next(n) = next;
    
    /* Make consumed answer field of consumers point to
       previous sibling if they point to a deleted answer */
    c = (NLChoice) subg_asf_list_ptr(sg_frame);
    while(c != NULL){
      if(nlcp_trie_return(c) == a){
	nlcp_trie_return(c) = n;
      }
      c = (NLChoice)nlcp_prevlookup(c);
    }

#if (defined(LOCAL_EVAL))
      /* if gen-cons points to deleted answer, make it
       * point to previous sibling */
      tc = (TChoice)subg_cp_ptr(sg_frame);
      if (tcp_trie_return(tc) == a) {
	tcp_trie_return(tc) = n;
      }
#endif
   
    ALN_Next(n) = next;

    if(next == NULL){ /* last answer */
      subg_ans_list_tail(sg_frame) = n;
    }      
  }
  if (is_conditional_answer(l)) {
    simplify_pos_unsupported(l);
    if (groundcall) {
      mark_subgoal_failed(sg_frame);
      simplify_neg_fails(sg_frame);
    }
  }
}

/*----------------------------------------------------------------------*/
/* Given a tabled subgoal, go through its list of deleted nodes (in the
 * completion stack), and reclaim the leaves and corresponding branches
 *----------------------------------------------------------------------*/

void  reclaim_del_ret_list(VariantSF sg_frame) {
  ALNptr x,y;
  
  x = compl_del_ret_list(subg_compl_stack_ptr(sg_frame));
  
  while (x != NULL) {
    y = x;
    x = ALN_Next(x);
/*      delete_branch(ALN_Answer(y), &subg_ans_root_ptr(sg_frame)); */
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
    VariantSF sg_frame;
    CPtr    tcp, cptr, where;
    int     is_new, i;
    Integer breg_offset, Nvars;

    breg_offset = ptoc_int(1);
    tcp = (CPtr)((Integer)(tcpstack.high) - breg_offset);
    sg_frame = (VariantSF)(tcp_subgoal_ptr(tcp));
    where = tcp_template(tcp);
    Nvars = int_val(cell(where)) & 0xffff;
    cptr = where - Nvars - 1;
    if (Nvars == 0) {
      ctop_string(3, get_ret_string());
    } else {
      bind_cs((CPtr)ptoc_tag(3), hreg);
      sym = insert("ret", (byte)Nvars, (Psc)flags[CURRENT_MODULE], &is_new);
      new_heap_functor(hreg, sym->psc_ptr);
      for (i = Nvars; i > 0; i--) {
	term = (Cell)(*(CPtr)(cptr+i));
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
static int first_free_set = 0;
static int Set_ArraySz = 100;
/*
 * num_sets is the number of sets have been used (including the fixed
 * trie, Set_ArrayPtr[0] (see trie_intern/3)).  It is also the index for
 * the next element to use when no free element is available.
 */
static int num_sets = 1;

/*----------------------------------------------------------------------*/

/* Allocate an array of handles to interned tries. */

void init_newtrie(void)
{
  Set_ArrayPtr = (BTNptr *) calloc(Set_ArraySz,sizeof(BTNptr));
}

/*----------------------------------------------------------------------*/

/* Returns a handle to an unused interned trie. */

Integer newtrie(void)
{
  int i;
  Integer result;
  
  if (first_free_set != 0) {	/* a free set is available */
    i = first_free_set;		/* save it in i */
    result = (Integer)first_free_set;
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
    result = (Integer)num_sets;
    num_sets++;
  }
  return result;
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

  xsb_dbgmsg((LOG_INTERN, "Interning "));
  dbg_printterm(LOG_INTERN,stddbg,term,25);
  xsb_dbgmsg((LOG_INTERN, "In trie with root %d", RootIndex));

  switch_to_trie_assert;
  Leaf = whole_term_chk_ins(term,&(Set_ArrayPtr[RootIndex]),&flag);
  switch_from_trie_assert;
  
  ctop_int(3,(Integer)Leaf);
  ctop_int(4,flag);
  xsb_dbgmsg((LOG_INTERN, "Exit flag %d",flag));
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
    XSB_Deref(trie_term);
    XSB_Deref(Leafterm);
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


/*  
 Changes made by Prasad Rao. Jun 20th 2000

 The solution for reclaiming the garbage nodes resulting
 from trie dispose is as follows.
 Maintain a datastructure as follows
 1)  IGRhead -> Root1 -> Root2 -> Root3 -> null
                 |        |        |
                 |        |        |
                 v        v        v
                Leaf11   Leaf21   Leaf31
	         |        |        |  
                 |        |        |
                 V        v        v
                Leaf12    null    Leaf32        
                 |                 |
                 v                 |
                null               v
                                 Leaf33
                                   |
                                   v
                                  null
To reclaim all the garbage associated with a particular root
 a) remove the root from the root list
 b) remove all the garbage branches assoc with the root 
    by calling delete_branch(leaf,....)
   Done!!

*/

static IGRptr IGRhead = NULL;

static IGRptr newIGR(long root)
{
  IGRptr igr;
  
  igr = malloc(sizeof(InternGarbageRoot));
  igr -> root   = root;
  igr -> leaves = NULL;
  igr -> next   = NULL;
  return igr;
}

static IGLptr newIGL(BTNptr leafn)
{
  IGLptr igl;
  
  igl = malloc(sizeof(InternGarbageLeaf));
  igl -> leaf = leafn;
  igl -> next = NULL;
  return igl;
}

static IGRptr getIGRnode(long rootn)
{
  IGRptr p = IGRhead;  

  while(p != NULL){
    if(p -> root == rootn)
      return p;
    else
      p = p -> next;
  }  
  if(p != NULL)
    xsb_warn("Invariant p == NULL violated");

  p = newIGR(rootn);
  p -> next = IGRhead;
  IGRhead = p;    
  return p;
}

static IGRptr getAndRemoveIGRnode(long rootn)
{
  IGRptr p = IGRhead;  

  if(p == NULL)
    return NULL;
  else if(p -> root == rootn){
    IGRhead = p -> next;
    return p;
  }
  else{
    IGRptr q = p;
    p = p -> next;
    while(p != NULL){
      if(p -> root == rootn){
	q -> next = p -> next;
	return p;
      } else{
	q = p;
	p = p -> next;
      }
    }  
  }
  xsb_dbgmsg((LOG_INTERN, "Root node not found in Garbage List"));
  return NULL;
}



/*
 *  Insert "leafn" into the garbage list, "r".
 *  This is done when leafn is deleted so that we could undelete it or later
 *  garbage-collect it.
 */
static void insertLeaf(IGRptr r, BTNptr leafn)
{
  /* Just make sure that the leaf is not already there */
  IGLptr p;

  if(r == NULL)
    return;
  p = r -> leaves;
  while(p != NULL){
    /*    xsb_warn("loopd"); */
    if(p -> leaf == leafn){
      /* The following should be permitted, because we should be able to
	 backtrackably delete backtrackably deleted nodes (which should have no
	 effect)
      */
      if (IsDeletedNode(leafn))
	xsb_dbgmsg((LOG_INTERN,
		   "The leaf node being deleted has already been deleted"));
      return;
    }
    p = p -> next;
  }
  p = newIGL(leafn);
  p -> next = r -> leaves;
  r -> leaves = p;
}


/*
 * This is builtin : TRIE_DISPOSE_NR(+ROOT, +LEAF), to
 * mark for  disposal a branch
 * of the trie rooted at Set_ArrayPtr[ROOT].
 */
void trie_dispose_nr(void)
{
  BTNptr Leaf;
  long Rootidx;

  Rootidx = ptoc_int(1);
  Leaf = (BTNptr)ptoc_int(2);
  switch_to_trie_assert;
  insertLeaf(getIGRnode(Rootidx), Leaf);
  safe_delete_branch(Leaf);
  switch_from_trie_assert;
}


void reclaim_uninterned_nr(long rootidx)
{
  IGRptr r = getAndRemoveIGRnode(rootidx);
  IGLptr l, p;
  BTNptr leaf;

  if (r!=NULL)
    l = r-> leaves;
  else
    return;

  free(r);

  while(l != NULL){
    /* printf("Loop b %p\n", l); */
    leaf = l -> leaf;
    p = l -> next;
    free(l);
    switch_to_trie_assert;
    if(IsDeletedNode(leaf)) {
      delete_branch(leaf, &(Set_ArrayPtr[rootidx]));
    } else {
      /* This is allowed:
	 If we backtrack over a delete, the node that was marked for deletion
	 and placed in the garbage list is unmarked, but isn't removed from
	 the garbage list. So it is a non-deleted node on the garbage list.
	 It is removed from there only when we reclaim space.
      */
      xsb_dbgmsg("Non deleted interned node in garbage list - ok");
    }

    switch_from_trie_assert;
    l = p;
  }

}

/*----------------------------------------------------------------------*/

void trie_undispose(long rootIdx, BTNptr leafn)
{
  IGRptr r = getIGRnode(rootIdx);
  IGLptr p = r -> leaves;
  if(p == NULL){
    xsb_dbgmsg((LOG_INTERN,
   "In trie_undispose: The node being undisposed has been previously deleted"));
  } else{
    if(p -> leaf == leafn){
      r -> leaves = p -> next;
      free(p);
      if(r -> leaves == NULL){
	/* Do not want roots with no leaves hanging around */
	getAndRemoveIGRnode(rootIdx);
      }
    }
    undelete_branch(leafn);
  }
}
