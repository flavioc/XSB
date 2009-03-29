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
#include <string.h>

/* Special debug includes */
#include "debugs/debug_tries.h"


#include "auxlry.h"
#include "context.h"
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
#include "macro_xsb.h"
#include "sw_envs.h"
#include "choice.h"
#include "cut_xsb.h"
#include "inst_xsb.h"
#include "error_xsb.h"
#include "io_builtins_xsb.h"
#include "tr_utils.h"
#include "tst_utils.h"
#include "subp.h"
#include "rw_lock.h"
#include "debug_xsb.h"
#include "thread_xsb.h"
#include "storage_xsb.h"
#include "hash_xsb.h"
#include "tables.h"
#include "builtin.h"
#include "trie_defs.h"
#include "trassert.h"
#include "biassert_defs.h"

#include "call_graph_xsb.h" /* incremental evaluation */
#include "table_inspection_defs.h"

/*----------------------------------------------------------------------*/

extern void print_subgoal(CTXTdeclc FILE *, VariantSF);

#define MAX_VAR_SIZE	200

#include "ptoc_tag_xsb_i.h"
#include "term_psc_xsb_i.h"

/*----------------------------------------------------------------------*/
/* various utility predicates and macros */
/*----------------------------------------------------------------------*/

xsbBool varsf_has_unconditional_answers(VariantSF subg)
{
  ALNptr node_ptr = subg_answers(subg);
 
  /* Either subgoal has no answers or it is completed */
  /* and its answer list has already been reclaimed. */
  /* In either case, the result is immediately obtained. */
 
#ifndef CONC_COMPL
  if (node_ptr <= COND_ANSWERS) return (node_ptr == UNCOND_ANSWERS);
#else
  if (subg_tag(subg) <= COND_ANSWERS) return (subg_tag(subg) == UNCOND_ANSWERS);
#endif
 
  /* If the subgoal has not been completed, or is early completed but its */
  /* answer list has not been reclaimed yet, check each of its nodes. */
 
  while (node_ptr) {
    if (is_unconditional_answer(ALN_Answer(node_ptr))) return TRUE;
    node_ptr = ALN_Next(node_ptr);
  }
  return FALSE;
}

xsbBool varsf_has_conditional_answer(VariantSF subg)
{

  ALNptr node_ptr = subg_answers(subg);
 
  if (subg_is_complete(subg)) 
#ifndef CONC_COMPL
    return (node_ptr == COND_ANSWERS);
#else
    return (subg_tag(subg) == COND_ANSWERS);
#endif
 
  /* If the subgoal has not been completed, or is early completed but its */
  /* answer list has not been reclaimed yet, check each of its nodes. */
  else { 
    while (node_ptr) {
      if (is_conditional_answer(ALN_Answer(node_ptr))) return TRUE;
      node_ptr = ALN_Next(node_ptr);
    }
    return FALSE;
  }
}

/* This is needed to find an actual trie node from a CP -- hash-handle must be special-cased */
BTNptr TrieNodeFromCP(CPtr pCP) {							
    prolog_int i;	
    BTNptr pBTN;						
    if (*(byte *)*pCP == hash_handle) {					
      pBTN = (BTNptr) string_val(*(pCP+CP_SIZE+1));			
      for (i = 0 ; i < (prolog_int)BTHT_NumBuckets((BTHTptr) pBTN); i++) {		
	if (BTHT_BucketArray((BTHTptr) pBTN)[i] != 0) {			
	  return BTHT_BucketArray((BTHTptr) pBTN)[i];			
	}									
      }
      return NULL;
    }
    else return (BTNptr) *pCP;						
}


/*----------------------------------------------------------------------*/

/* get_call() and supporting code. */

/*----------------------------------------------------------------------*/

/*
 * Given a subgoal of a variant predicate, returns its subgoal frame
 * if it has a table entry; returns NULL otherwise.  If requested, the
 * answer template is constructed on the heap as a ret/n term and
 * passed back via the last argument.
 */

VariantSF get_variant_sf(CTXTdeclc Cell callTerm, TIFptr pTIF, Cell *retTerm) {

  int arity;
  BTNptr root, leaf;
  Cell callVars[MAX_VAR_SIZE + 1];

  root = TIF_CallTrie(pTIF);
  if ( IsNULL(root) )
    return NULL;

  arity = get_arity(TIF_PSC(pTIF));
  leaf = variant_trie_lookup(CTXTc root, arity, clref_val(callTerm) + 1, callVars);
  if ( IsNULL(leaf) )
    return NULL;
  if ( IsNonNULL(retTerm) )
    *retTerm = build_ret_term(CTXTc callVars[0], &callVars[1]);
  return ( CallTrieLeaf_GetSF(leaf) );
}

/*----------------------------------------------------------------------*/

/*
 * Given a subgoal of a subsumptive predicate, returns the subgoal
 * frame of some producing table entry which subsumes it; returns NULL
 * otherwise.  The answer template with respect to this producer entry
 * is constructed on the heap as a ret/n term and passed back via the
 * last argument.
 * 
 * Note that unlike get_variant_sf, the answer template is derived
 * from the subsuming tabled call and the call itself (via
 * construct_answer_template), before building the ret_term.
 */

SubProdSF get_subsumer_sf(CTXTdeclc Cell callTerm, TIFptr pTIF, Cell *retTerm) {

  BTNptr root, leaf;
  int arity;
  TriePathType path_type;
  SubProdSF sf;
  Cell ansTmplt[MAX_VAR_SIZE + 1];

  root = TIF_CallTrie(pTIF);
  if ( IsNULL(root) )
    return NULL;

  arity = get_arity(TIF_PSC(pTIF));
  leaf = subsumptive_trie_lookup(CTXTc root, arity, clref_val(callTerm) + 1,
				 &path_type, ansTmplt);
  if ( IsNULL(leaf) )
    return NULL;
  sf = (SubProdSF)CallTrieLeaf_GetSF(leaf);
  if ( IsProperlySubsumed(sf) ) {
    sf = conssf_producer(sf);
    construct_answer_template(CTXTc callTerm, sf, ansTmplt);
  }
  if ( IsNonNULL(retTerm) )
    *retTerm = build_ret_term(CTXTc ansTmplt[0], &ansTmplt[1]);
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

Cell build_ret_term(CTXTdeclc int arity, Cell termVector[]) {

  Psc sym_psc;
  CPtr ret_term;
  int  i;

  if ( arity == 0 )
    return makestring(get_ret_string());  /* return as a term */
  else {
    ret_term = hreg;  /* pointer to where ret(..) will be built */
    sym_psc = get_ret_psc((byte)arity);
    new_heap_functor(hreg, sym_psc);
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

void construct_answer_template(CTXTdeclc Cell callTerm, SubProdSF producer,
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

VariantSF get_call(CTXTdeclc Cell callTerm, Cell *retTerm) {

  Psc  psc;
  TIFptr tif;
  int arity;
  BTNptr root, leaf;
  VariantSF sf;
  Cell callVars[MAX_VAR_SIZE + 1];


  psc = term_psc(callTerm);
  if ( IsNULL(psc) ) {
    xsb_type_error(CTXTc "callable",callTerm,"get_call/3",1);
    return NULL;
  }

  tif = get_tip(CTXTc psc);
  if ( IsNULL(tif) )
    xsb_abort("Predicate %s/%d is not tabled", get_name(psc), get_arity(psc));

  root = TIF_CallTrie(tif);
  if ( IsNULL(root) )
    return NULL;

  arity = get_arity(psc);
  leaf = variant_trie_lookup(CTXTc root, arity, clref_val(callTerm) + 1, callVars);
  if ( IsNULL(leaf) )
    return NULL;
  else {

    sf = CallTrieLeaf_GetSF(leaf);

    /* incremental evaluation: check introduced as because of fact
       predicates  */
    
    if(IsNonNULL(sf) && get_incr(psc) && IsNULL(sf->callnode)) 
      return NULL;
    
    /* incremental evaluation end */

    if ( IsProperlySubsumed(sf) )
      construct_answer_template(CTXTc callTerm, conssf_producer(sf), callVars);


    *retTerm = build_ret_term(CTXTc callVars[0],&callVars[1]);

    return sf;
  }
}

/*======================================================================*/

/*
 *                     D E L E T I N G   T R I E S
 *                     ===========================
 */


/*----------------------------------------------------------------------*/
/* delete_predicate_table(), reclaim_deleted_predicate_table() 
 * and supporting code.
 * 
 * Used to delete/reclaim a predicate-level call and answer trie, works for
 * both call-variance and call subsumption. */
/*----------------------------------------------------------------------*/

/* Stack for top-down traversing and freeing components of a trie
   -------------------------------------------------------------- */

#define freeing_stack_increment 1000

#define push_node(node) {\
  if (node_stk_top >= freeing_stack_size) {\
    unsigned long old_freeing_stack_size = freeing_stack_size; \
    freeing_stack_size = freeing_stack_size + freeing_stack_increment;\
    freeing_stack = (BTNptr *)mem_realloc(freeing_stack,old_freeing_stack_size*sizeof(BTNptr),\
					  freeing_stack_size*sizeof(BTNptr),TABLE_SPACE);\
  }\
  freeing_stack[node_stk_top] = node;\
  node_stk_top++;\
  }

#define pop_node(node) {\
  node_stk_top--;\
  node = freeing_stack[node_stk_top];\
}


/* TLS: since this deallocates from smBTHT, make sure
   trie_allocation_type is set to private/shared before using this
   function. */

static void free_trie_ht(CTXTdeclc BTHTptr ht) {
#ifdef MULTI_THREAD
  if (BTHT_NumBuckets(ht) == TrieHT_INIT_SIZE 
      && threads_current_sm != SHARED_SM) {
    SM_DeallocateStruct(*private_smTableBTHTArray,BTHT_BucketArray(ht)); 
  }
  else
#endif
    mem_dealloc(BTHT_BucketArray(ht),BTHT_NumBuckets(ht)*sizeof(void *),
		TABLE_SPACE);
#ifdef MULTI_THREAD
  if( threads_current_sm == SHARED_SM )
	SM_Lock(*smBTHT);
#endif
  TrieHT_RemoveFromAllocList(*smBTHT,ht);
#ifdef MULTI_THREAD
  if( threads_current_sm == SHARED_SM )
	SM_Unlock(*smBTHT);
#endif
  SM_DeallocatePossSharedStruct(*smBTHT,ht); 
}

/* -------------------------------------------------------------- 
 * Space reclamation for Conditional Answers 
 * All routines assume that trie allocation type has been set (cf. struct_manager.h)
 * ------------------------------------------------------------ */

#ifdef MULTI_THREAD
#define GLOBAL_TABLE    (threads_current_sm == SHARED_SM)		
#endif

void release_any_pndes(CTXTdeclc PNDE firstPNDE) {
  PNDE lastPNDE;

#ifdef MULTI_THREAD  
  if (GLOBAL_TABLE)
#endif
    while (firstPNDE) {
      lastPNDE = firstPNDE;
      firstPNDE = pnde_next(firstPNDE);
      SYS_MUTEX_LOCK( MUTEX_DELAY ) ;
      release_entry(lastPNDE, released_pndes_gl, pnde_next);	
      SYS_MUTEX_UNLOCK( MUTEX_DELAY ) ;
    }
#ifdef MULTI_THREAD  
  else
    while (firstPNDE) {
      lastPNDE = firstPNDE;
      firstPNDE = pnde_next(firstPNDE);
      release_entry(lastPNDE, private_released_pndes, pnde_next);	
    }
#endif
}

/* TLS: a delay "trie" should be a simple chain, as I understand it --
   hence the error messages below. 
---------------------------------------------------------------------*/
void delete_delay_trie(CTXTdeclc BTNptr root) {
  int node_stk_top = 0;
  BTNptr rnod;
  //  BTNptr *Bkp; 
  //  BTHTptr ht;
  //  printf(" in delete delay trie\n");
  BTNptr *freeing_stack = NULL;
  int freeing_stack_size = 0;

  if ( IsNonNULL(root) ) {
    push_node(root);
    while (node_stk_top != 0) {
      pop_node(rnod);
      if ( IsHashHeader(rnod) ) {
	xsb_abort("encountered hashtable in delay trie?!?\n");

	//	ht = (BTHTptr) rnod;
	//	for (Bkp = BTHT_BucketArray(ht);
	//	     Bkp < BTHT_BucketArray(ht) + BTHT_NumBuckets(ht);
	//	     Bkp++) {
	//	  if ( IsNonNULL(*Bkp) )
	//	    push_node(*Bkp);
	//	}
	//	free_trie_ht(CTXTc ht);
      }
      else {
	if (BTN_Sibling(rnod)) 
	  xsb_abort("encountered sibling in delay trie?!?\n");
	//	  push_node(BTN_Sibling(rnod));
	if ( BTN_Child(rnod))
	  push_node(BTN_Child(rnod));
	SM_DeallocatePossSharedStruct(*smBTN,rnod);
      }
    }
  } /* free answer trie */
  mem_dealloc(freeing_stack,freeing_stack_size*sizeof(BTNptr),TABLE_SPACE);
  //  printf("leaving delete delay trie\n");
}

/* TLS: unlike release_all_dls() in slgdelay.c which is used for
   simplification, I am not releasing pndes of the subgoal or answer
   associated with the delay element.  Rather, a linked set of tables
   should be abolished in one fell swoop. 
---------------------------------------------------------------------*/
   
void abolish_release_all_dls_shared(CTXTdeclc ASI asi)
{
  //  ASI de_asi;
  DE de, tmp_de;
  DL dl, tmp_dl;

  dl = asi_dl_list(asi);
  SYS_MUTEX_LOCK( MUTEX_DELAY ) ;
  while (dl) {
    tmp_dl = dl_next(dl);
    de = dl_de_list(dl);
    while (de) {
      tmp_de = de_next(de);
      release_shared_de_entry(de);
      de = tmp_de; /* next DE */
    } /* while (de) */
    release_entry(dl, released_dls_gl, dl_next);
    dl = tmp_dl; /* next DL */
  }
  SYS_MUTEX_UNLOCK( MUTEX_DELAY ) ;
}

#ifdef MULTI_THREAD
void abolish_release_all_dls_private(CTXTdeclc ASI asi)
{
  DE de, tmp_de;
  DL dl, tmp_dl;

  dl = asi_dl_list(asi);
  while (dl) {
    tmp_dl = dl_next(dl);
    de = dl_de_list(dl);
    while (de) {
      tmp_de = de_next(de);
      release_private_de_entry(de);
      de = tmp_de; /* next DE */
    } /* while (de) */
    release_entry(dl, private_released_dls, dl_next);
    dl = tmp_dl; /* next DL */
  }
}
#endif

void abolish_release_all_dls(CTXTdeclc ASI asi)
{

#ifdef MULTI_THREAD  
  if (GLOBAL_TABLE)
#endif
    abolish_release_all_dls_shared(CTXTc asi);
#ifdef MULTI_THREAD
  else
    abolish_release_all_dls_private(CTXTc asi);
#endif
}

void release_conditional_answer_info(CTXTdeclc BTNptr node) {
  ASI asiptr;
  if ((asiptr = (ASI) BTN_Child(node))) {
    abolish_release_all_dls(CTXTc asiptr);		    
    release_any_pndes(CTXTc asi_pdes(asiptr)); // handles shared/private
#ifdef MULTI_THREAD  
  if (GLOBAL_TABLE)
#endif
    SM_DeallocateSharedStruct(smASI,asiptr)
#ifdef MULTI_THREAD  
  else
    SM_DeallocateStruct(*private_smASI,asiptr);
#endif
  }
}

/* delete_variant_sf_and_answers deletes and reclaims space for
   answers and their subgoal frame in a variant table, and is used by
   abolish_table_call (which does not work on subsumptive table).  It
   copies code from delete_variant_table, but uses its own stack.
   (Not easy to integrate due to macro usage.) */

/* 
 * TLS: since this deallocates from SMs, make sure
 * trie_allocation_type is set before using.
 */
void delete_variant_sf_and_answers(CTXTdeclc VariantSF pSF, xsbBool should_warn) {
  int node_stk_top = 0;
  BTNptr rnod, *Bkp; 
  BTHTptr ht;
  
  BTNptr *freeing_stack = NULL;
  int freeing_stack_size = 0;

  //  printf("delete_variant_sf %p\n",pSF);
  TRIE_W_LOCK();
  /* TLS: this checks whether any answer for this subgoal has a delay
     list: may overstate problems but will warn for any possible
     corruption. */
#ifndef CONC_COMPL
  if ( subg_answers(pSF) == COND_ANSWERS && should_warn) {
#else
    if ( subg_tag(pSF) == COND_ANSWERS && should_warn) {
#endif
      xsb_warn("abolish_table_call/1 is deleting a table entry for %s/%d with conditional"
                      " answers: delay dependencies may be corrupted.\n",	    
	       get_name(TIF_PSC(subg_tif_ptr(pSF))),get_arity(TIF_PSC(subg_tif_ptr(pSF))));
      should_warn = FALSE;
    }

  if ( IsNonNULL(subg_ans_root_ptr(pSF)) ) {
    push_node((BTNptr)subg_ans_root_ptr(pSF));
    while (node_stk_top != 0) {
      pop_node(rnod);
      if ( IsHashHeader(rnod) ) {
	ht = (BTHTptr) rnod;
	for (Bkp = BTHT_BucketArray(ht);
	     Bkp < BTHT_BucketArray(ht) + BTHT_NumBuckets(ht);
	     Bkp++) {
	  if ( IsNonNULL(*Bkp) )
	    push_node(*Bkp);
	}
	free_trie_ht(CTXTc ht);
      }
      else {
	if (BTN_Sibling(rnod)) 
	  push_node(BTN_Sibling(rnod));
	if ( ! IsLeafNode(rnod) )
	  push_node(BTN_Child(rnod))
	  else { /* leaf node */
	    release_conditional_answer_info(CTXTc rnod);
	  }
 	SM_DeallocatePossSharedStruct(*smBTN,rnod);
      }
    }
  } /* free answer trie */
  free_answer_list(pSF);
  FreeProducerSF(pSF);
  TRIE_W_UNLOCK();
  mem_dealloc(freeing_stack,freeing_stack_size*sizeof(BTNptr),TABLE_SPACE);
  //  printf("leaving delete_variant\n");
  }


/* Incremental recomputation seems to be implemented only for
   abolishing predicates, but not subgoals */

extern void hashtable1_destroy(void *, int);

 static void delete_variant_table(CTXTdeclc BTNptr x, int incr, xsbBool should_warn) {

   //   printf("in delete variant table\n");

  int node_stk_top = 0, call_nodes_top = 0;
  BTNptr node, rnod, *Bkp; 
  BTHTptr ht;

  BTNptr *freeing_stack = NULL;
  int freeing_stack_size = 0;

  if ( IsNULL(x) )
    return;

  TRIE_W_LOCK();
  push_node(x);
  while (node_stk_top > 0) {
    pop_node(node);
    if ( IsHashHeader(node) ) {
      ht = (BTHTptr) node;
      for (Bkp = BTHT_BucketArray(ht);
	   Bkp < BTHT_BucketArray(ht) + BTHT_NumBuckets(ht);
	   Bkp++) {
	if ( IsNonNULL(*Bkp) )
	  push_node(*Bkp);
      }
      free_trie_ht(CTXTc ht);
    }
    else {
      if ( IsNonNULL(BTN_Sibling(node)) )
	push_node(BTN_Sibling(node));
      if ( IsNonNULL(BTN_Child(node)) ) {
	if ( IsLeafNode(node) ) {
	  /**
	   * Remove the subgoal frame and its dependent structures
	   */
	  VariantSF pSF = CallTrieLeaf_GetSF(node);

	  /* TLS: this checks whether any answer for this subgoal has
	  a delay list: may overstate problems but will warn for any
	  possible corruption. */
#ifndef CONC_COMPL
	  if ( subg_answers(pSF) == COND_ANSWERS && should_warn) {
#else
	  if ( subg_tag(pSF) == COND_ANSWERS && should_warn) {
#endif
	    xsb_warn("abolish_table_pred/1 is deleting a table entry for %s/%d with conditional\
                      answers: delay dependencies may be corrupted flags %d.\n",	    
		     get_name(TIF_PSC(subg_tif_ptr(pSF))),get_arity(TIF_PSC(subg_tif_ptr(pSF))),flags[TABLE_GC_ACTION]);
	    /*
	    xsb_warn("abolish_table_pred/1 is deleting a table entry for %s/%d with conditional\
                      answers: delay dependencies may be corrupted.\n",	    
		     get_name(TIF_PSC(subg_tif_ptr(pSF))),get_arity(TIF_PSC(subg_tif_ptr(pSF))));
	    */
	    should_warn = FALSE;
	  }

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
		free_trie_ht(CTXTc ht);
	      }
	      else {
		if (BTN_Sibling(rnod)) 
		  push_node(BTN_Sibling(rnod));
		if ( ! IsLeafNode(rnod) )
		  push_node(BTN_Child(rnod))
		else { /* leaf node */
		  release_conditional_answer_info(CTXTc rnod);
		}
		SM_DeallocatePossSharedStruct(*smBTN,rnod);
	      }
	    }
	  } /* free answer trie */
	  /* this following is unsafe, if there are pointers to it... */
	  if (incr) hashtable1_destroy(pSF->callnode->outedges->hasht,0);
	  free_answer_list(pSF);
	  FreeProducerSF(pSF);
	} /* callTrie node is leaf */
	else 
	  push_node(BTN_Child(node));
      } /* there is a child of "node" */
      SM_DeallocatePossSharedStruct(*smBTN,node);
    }
  }
  TRIE_W_UNLOCK();

  mem_dealloc(freeing_stack,freeing_stack_size*sizeof(BTNptr),TABLE_SPACE);

}

/* called when it is now known whether a predicate is variant or subsumptive */
  void delete_predicate_table(CTXTdeclc TIFptr tif, xsbBool warn) {
  if ( TIF_CallTrie(tif) != NULL ) {
    SET_TRIE_ALLOCATION_TYPE_TIP(tif);
    if ( IsVariantPredicate(tif) ) {
      delete_variant_table(CTXTc TIF_CallTrie(tif),get_incr(TIF_PSC(tif)),warn);
    }
    else
      delete_subsumptive_table(CTXTc tif);
    TIF_CallTrie(tif) = NULL;
    TIF_Subgoals(tif) = NULL;
  }
}

void transitive_delete_predicate_table(CTXTdeclc TIFptr tif, xsbBool should_warn) {

  SET_TRIE_ALLOCATION_TYPE_TIP(tif);
  delete_variant_table(CTXTc TIF_CallTrie(tif),get_incr(TIF_PSC(tif)),should_warn);
  TIF_CallTrie(tif) = NULL;
  TIF_Subgoals(tif) = NULL;
}

/* - - - - - */

void reclaim_deleted_subsumptive_table(CTXTdeclc DelTFptr);

/* Just like delete_predicate_table, but called from gc sweeps with deltf_ptr.
   In addition, does not reset TIFs.*/
void reclaim_deleted_predicate_table(CTXTdeclc DelTFptr deltf_ptr) {
  TIFptr tif = subg_tif_ptr(DTF_Subgoals(deltf_ptr));

  SET_TRIE_ALLOCATION_TYPE_TIP(tif);
  if ( IsVariantPredicate(tif) ) {
    delete_variant_table(CTXTc DTF_CallTrie(deltf_ptr), get_incr(TIF_PSC(tif)),DTF_Warn(deltf_ptr));
  } else reclaim_deleted_subsumptive_table(CTXTc deltf_ptr);
}

/*----------------------------------------------------------------------*/
/* delete_branch(), safe_delete_branch(), undelete_branch() and
 * supporting code. */
/*----------------------------------------------------------------------*/

 /* 
 * Used for call tries (abolish_table_call/1), answer tries (within
 * delay handling routines), and by trie_retract.
 */

static int is_hash(BTNptr x) 
{
  if( x == NULL)
    return(0);
  else
    return( IsHashHeader(x) );
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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

/*---------------------------------------------------------*/

/*
 * Delete a branch in the trie down from node `lowest_node_in_branch'
 * up to the level pointed to by the hook location, as pointed to by
 * `hook'.  Under normal use, the "hook" is either for the root of the
 * trie, or for the first level of the trie (is a pointer to the child
 * field of the root).  */

/* 
 * TLS: since this deallocates from SMs, make sure
 * trie_allocation_type is set before using.
 *
 * In addition, it now works for call subsumption, so need to set the
 * structure manager to use (smBTN vs smTSTN)
 */
void delete_branch(CTXTdeclc BTNptr lowest_node_in_branch, BTNptr *hook,int eval_method) {

  int num_left_in_hash;
  BTNptr prev, parent_ptr, *y1, *z;
  Structure_Manager *smNODEptr;

  if (eval_method == VARIANT_EVAL_METHOD)
    smNODEptr = smBTN;
  else 
    smNODEptr = &smTSTN;

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
	SM_DeallocateStruct(*smNODEptr,lowest_node_in_branch);
	return;
      }
      else
	free_trie_ht(CTXTc (BTHTptr)(*y1));
    }
    /*
     *  Remove this node and continue.
     */
    //    printf("deleting %x\n",lowest_node_in_branch->symbol);
    SM_DeallocateStruct(*smNODEptr,lowest_node_in_branch);
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
    //    printf("deleting %x\n",lowest_node_in_branch->symbol);
    SM_DeallocateStruct(*smNODEptr,lowest_node_in_branch);
  }
}

/*------------------------------*/
void safe_delete_branch(BTNptr lowest_node_in_branch) {

  byte instruction;

  MakeStatusDeleted(lowest_node_in_branch);
  instruction = BTN_Instr(lowest_node_in_branch);
  instruction = (instruction & 0x3) | trie_no_cp_fail;
  BTN_Instr(lowest_node_in_branch) = instruction;
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
/* delete_trie() and supporting code.  
 * 
 * Code to support deletion of asserted or interned tries.
 * delete_trie() is used by gen_retractall (i.e. abolish or retractall
 * with an open atomic formula) to delete an entire asserted trie.
 * Its also called via the builtin DELETE_TRIE --
 * (delete_interned_trie()) to delete an interned trie or storage trie */
/*----------------------------------------------------------------------*/

#define DELETE_TRIE_STACK_INIT 100
#define MAX_DELETE_TRIE_STACK_SIZE 1000
#define DT_NODE 0
#define DT_DS 1
#define DT_HT 2

#define push_delete_trie_node(node,op) {\
  trie_op_top++;\
  if (trie_op_top >= trie_op_size) {\
    trie_op_size = 2*trie_op_size;\
    delete_trie_op = (char *)mem_realloc(delete_trie_op,(trie_op_size/2)*sizeof(char),trie_op_size*sizeof(char),TABLE_SPACE);\
    if (!delete_trie_op) xsb_exit(CTXTc "out of space for deleting trie");\
    /*xsb_dbgmsg((LOG_DEBUG,"realloc delete_trie_op to %d",trie_op_size));*/\
  }\
  delete_trie_op[trie_op_top] = op;\
  trie_node_top++;\
  if (trie_node_top >= trie_node_size) {\
    trie_node_size = 2*trie_node_size;\
    delete_trie_node = (BTNptr *)mem_realloc(delete_trie_node,(trie_node_size/2)*sizeof(BTNptr),trie_node_size*sizeof(BTNptr),TABLE_SPACE);\
    if (!delete_trie_node) xsb_exit(CTXTc "out of space for deleting trie");\
    /*xsb_dbgmsg((LOG_DEBUG,"realloc delete_trie_node to %d",trie_node_size));*/\
  }\
  delete_trie_node[trie_node_top] = node;\
}  
#define push_delete_trie_hh(hh) {\
  trie_op_top++;\
  if (trie_op_top >= trie_op_size) {\
    trie_op_size = 2*trie_op_size;\
    delete_trie_op = (char *)mem_realloc(delete_trie_op,(trie_op_size/2)*sizeof(char),trie_op_size*sizeof(char),TABLE_SPACE);\
    if (!delete_trie_op) xsb_exit(CTXTc "out of space for deleting trie");\
    /*xsb_dbgmsg((LOG_DEBUG,"realloc delete_trie_op to %d",trie_op_size));*/\
  }\
  delete_trie_op[trie_op_top] = DT_HT;\
  trie_hh_top++;\
  if (trie_hh_top >= trie_hh_size) {\
    trie_hh_size = 2*trie_hh_size;\
    delete_trie_hh = (BTHTptr *)mem_realloc(delete_trie_hh,(trie_hh_size/2)*sizeof(BTHTptr),trie_hh_size*sizeof(BTHTptr),TABLE_SPACE);\
    if (!delete_trie_hh) xsb_exit(CTXTc "out of space for deleting trie");\
    /*xsb_dbgmsg((LOG_DEBUG,"realloc delete_trie_hh to %d",trie_hh_size));*/\
  }\
  delete_trie_hh[trie_hh_top] = hh;\
}  

/*************************************************************************/
/* TLS: assumed for the purpose of MT storage managers, that
   delete_trie() is being called only to delete asserted tries --
   otherwise, need to set smBTN and smBTHT to private/shared */

void delete_trie(CTXTdeclc BTNptr iroot) {
  BTNptr root, sib, chil;  
  int trie_op_top = 0;
  int trie_node_top = 0;
  int trie_hh_top = -1;

  char *delete_trie_op = NULL;
  BTNptr *delete_trie_node = NULL;
  BTHTptr *delete_trie_hh = NULL;
  int trie_op_size, trie_node_size, trie_hh_size;

  if (!delete_trie_op) {
    delete_trie_op = (char *)mem_alloc(DELETE_TRIE_STACK_INIT*sizeof(char),TABLE_SPACE);
    delete_trie_node = (BTNptr *)mem_alloc(DELETE_TRIE_STACK_INIT*sizeof(BTNptr),TABLE_SPACE);
    delete_trie_hh = (BTHTptr *)mem_alloc(DELETE_TRIE_STACK_INIT*sizeof(BTHTptr),TABLE_SPACE);
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
      SM_DeallocatePossSharedStruct(*smBTN,root);
      break;
    case DT_HT:
      free_trie_ht(CTXTc delete_trie_hh[trie_hh_top--]);
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
	      xsb_exit(CTXTc "Anomaly in delete_trie !");
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
	fprintf(stdwarn,"null node encountered in delete_trie\n");
      break;
    }
  }
  mem_dealloc(delete_trie_op,trie_op_size*sizeof(char),TABLE_SPACE); delete_trie_op = NULL;
  mem_dealloc(delete_trie_node,trie_node_size*sizeof(BTNptr),TABLE_SPACE); delete_trie_node = NULL;
  mem_dealloc(delete_trie_hh,trie_hh_size*sizeof(BTHTptr),TABLE_SPACE); delete_trie_hh = NULL;
  trie_op_size = 0; 
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
 * 
 * TLS: I have a question about the simplification done at the end of
 * this predicate.  It should only be performed if the trie is completed.
 * 
 * TLS: put in some protection for simplification operations using
 * MUTEX_DELAY.  But I'm not sure that other parts of this function
 * are thread-safe.
 */
void delete_return(CTXTdeclc BTNptr l, VariantSF sg_frame,int eval_method) 
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
    SET_TRIE_ALLOCATION_TYPE_SF(sg_frame);
    if (is_conditional_answer(l)) {
      ASI asi = Delay(l);
      SYS_MUTEX_LOCK( MUTEX_DELAY ) ;
      release_all_dls(CTXTc asi);
      SYS_MUTEX_UNLOCK( MUTEX_DELAY ) ;
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
    delete_branch(CTXTc l,&subg_ans_root_ptr(sg_frame),eval_method);
    n = subg_ans_list_ptr(sg_frame);
    /* Find previous sibling -pvr */
    while (ALN_Answer(ALN_Next(n)) != l) {
      n = ALN_Next(n);/* if a is not in that list a core dump will result */
    }
    if (n == NULL) {
      xsb_exit(CTXTc "Error in delete_return()");
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
    SYS_MUTEX_LOCK( MUTEX_DELAY ) ;
    simplify_pos_unsupported(CTXTc l);
    if (groundcall) {
      mark_subgoal_failed(sg_frame);
      simplify_neg_fails(CTXTc sg_frame);
    }
    SYS_MUTEX_UNLOCK( MUTEX_DELAY ) ;
  }
}

/*----------------------------------------------------------------------*/
/* Given a tabled subgoal, go through its list of deleted nodes (in the
 * completion stack), and reclaim the leaves and corresponding branches
 *----------------------------------------------------------------------*/

void  reclaim_del_ret_list(CTXTdeclc VariantSF sg_frame) {
  ALNptr x,y;

  x = compl_del_ret_list(subg_compl_stack_ptr(sg_frame));
  
  while (x != NULL) {
    y = x;
    x = ALN_Next(x);
/*      delete_branch(ALN_Answer(y), &subg_ans_root_ptr(sg_frame)); */
#ifndef MULTI_THREAD
    SM_DeallocateStruct(smALN,y);
#else
   if (IsSharedSF(sg_frame)) {			
     SM_DeallocateSharedStruct(smALN,y);
   } else {
     SM_DeallocateStruct(*private_smALN,y);
   }
#endif
  }
}
 
/*----------------------------------------------------------------------*/

/*
**   Used in aggregs.P to implement aggregates.v
**   Takes:   breg (the place where choice point is saved) and arity.  
**   Returns: subgoal skeleton (i.e., ret(X,Y,Z), where X,Y,Z are all the 
**    	      	                distinct variables in the subgoal);
**   	      Pointer to the subgoal.
*/

void breg_retskel(CTXTdecl)
{
    Psc    psc;
    Cell    term;
    VariantSF sg_frame;
    CPtr    tcp, cptr, where;
    int     i;
    Integer breg_offset, Nvars;

    breg_offset = ptoc_int(CTXTc 1);
    tcp = (CPtr)((Integer)(tcpstack.high) - breg_offset);
    sg_frame = (VariantSF)(tcp_subgoal_ptr(tcp));
    where = tcp_template(tcp);
    Nvars = int_val(cell(where)) & 0xffff;
    cptr = where - Nvars - 1;
    if (Nvars == 0) {
      ctop_string(CTXTc 3, get_ret_string());
    } else {
      bind_cs((CPtr)ptoc_tag(CTXTc 3), hreg);
      psc = get_ret_psc((byte)Nvars);
      new_heap_functor(hreg, psc);
      for (i = Nvars; i > 0; i--) {
	term = (Cell)(*(CPtr)(cptr+i));
        nbldval(term);
      }
    }
    ctop_int(CTXTc 4, (Integer)sg_frame);
}


/*======================================================================*/

/*
 *                    I N T E R N E D   T R I E S
 *                    ===========================
 */

/* Allocate an array of handles to interned tries, and initialize
   global variables. 

   In MT-engine default is private tries -- thus make private trie
   code and single-threaded trie code identical, and require shared
   tries only for MT engine.*/

#define ISTRIENULL(Field) ((Field) == -1)
#define TRIENULL -1

#ifndef MULTI_THREAD
int itrie_array_first_free;
//int itrie_array_last_free;
int itrie_array_first_trie  = TRIENULL;
struct interned_trie_t* itrie_array;
#else
int shared_itrie_array_first_free;
struct shared_interned_trie_t* shared_itrie_array;
#endif

/* Need to expand trie array on demand -- this is high up on list -- TLS.*/

int max_interned_tries_glc;

void init_private_trie_table(CTXTdecl) {
  int i ;
  
  itrie_array = 
    mem_calloc(max_interned_tries_glc+1, sizeof(struct interned_trie_t), TABLE_SPACE);

  for( i = 0; i < max_interned_tries_glc; i++ ) {
    //    itrie_array[i].valid = FALSE;
    itrie_array[i].next_entry = i+1;
    itrie_array[i].root = NULL;
  }
  itrie_array[max_interned_tries_glc].next_entry = -1;

  /* Set to 1 to avoid problems with storage_xsb.c which ues this (it
     returns a new trie with a 0 value) */
  itrie_array_first_free = 1;
  itrie_array_first_trie = -1;
}

#ifdef MULTI_THREAD
void init_shared_trie_table() {
  int i ;
  
  shared_itrie_array = 
    mem_calloc(max_interned_tries_glc+1, sizeof(struct shared_interned_trie_t), TABLE_SPACE);

  for( i = 0; i < max_interned_tries_glc; i++ ) {
      shared_itrie_array[i].next_entry = i+1;
      pthread_mutex_init(&shared_itrie_array[i].trie_mutex, &attr_rec_gl ) ;
  }
    shared_itrie_array[max_interned_tries_glc].next_entry = -1;

    // Set to 1 to avoid problems with storage_xsb.c which ues this (it
    //  returns a new trie with a 0 value) 
  shared_itrie_array_first_free = 1;
}
#endif

/*----------------------------------------------------------------------*/
// get_trie_type is directly in dynamic_code_function

Integer new_private_trie(CTXTdeclc int type) {
  Integer result = 0;
  Integer index;

  if (itrie_array_first_free < 0) {
    xsb_resource_error(CTXTc "interned tries","newtrie",1);
    return 0;
  }
  else {
    itrie_array[itrie_array_first_free].valid = TRUE;
    itrie_array[itrie_array_first_free].root = NULL;
    itrie_array[itrie_array_first_free].type = type;  // needed for trie_property
    SET_TRIE_ID(itrie_array_first_free,type,result);
    index = itrie_array_first_free;

    itrie_array_first_free = itrie_array[itrie_array_first_free].next_entry;
    itrie_array[index].prev_entry = TRIENULL;
    if (!ISTRIENULL(itrie_array_first_trie)) {
      itrie_array[index].next_entry = itrie_array_first_trie;
      itrie_array[itrie_array_first_trie].prev_entry = index;
    }
    else  itrie_array[index].next_entry = TRIENULL;
    itrie_array_first_trie = index;
    return result;
  }
}

#ifdef MULTI_THREAD
Integer new_shared_trie(CTXTdeclc int type) {
  Integer result;

  if (shared_itrie_array_first_free < 0) {
    xsb_resource_error(CTXTc "shared interned tries","newtrie",1);
    return 0;
  }
  else {
    shared_itrie_array[itrie_array_first_free].valid = TRUE;
    shared_itrie_array[itrie_array_first_free].root = NULL;
    shared_itrie_array[itrie_array_first_free].type = type;
    SET_TRIE_ID(shared_itrie_array_first_free,SHAS_TRIE,result);
    shared_itrie_array_first_free = shared_itrie_array[shared_itrie_array_first_free].next_entry;
    return result;
  }
}
#endif

/*----------------------------------------------------------------------*/

/* TLS: Shared tries still need more testing, but I hope to have them
   implemented soon */

//  Legacy API
#ifdef MULTI_THREAD
Integer newtrie(CTXTdeclc int type) {
  if (PRIVATE_TRIE(type)) 
    return new_private_trie(CTXTc type);
  else return (int) new_shared_trie(CTXTc type);
}
#else
Integer newtrie(CTXTdeclc int type) {
  return new_private_trie(CTXTc type);
}
#endif

/*----------------------------------------------------------------------*/
/* i_trie_intern(_Term,_Root,_Leaf,_Flag,_Check_CPS,_Expand_or_not)     
* 
* If called from trie_intern(), we'll need to check to see whether its
* safe to expand -- hence check_cps_flag; if called from
* bulk_trie_intern() we don't need to check, and expand_flag will be
* set to tell us whether we can expand or not.
*/

void private_trie_intern(CTXTdecl) {
  prolog_term term;
  int Trie_id,index,type;
  int flag, check_cps_flag, expand_flag;
  BTNptr Leaf;

  Trie_id = iso_ptoc_int(CTXTc 1,"trie_intern/2");
  term = ptoc_tag(CTXTc 2);
  check_cps_flag = ptoc_int(CTXTc 5);
  expand_flag = ptoc_int(CTXTc 6);

  switch_to_trie_assert;
  SPLIT_TRIE_ID(Trie_id,index,type);
  if (itrie_array[index].root != NULL && BTN_Instr(itrie_array[index].root) == trie_no_cp_fail) {
    printf("Inserting into trie with trie_no_cp_fail root\n");
  }

  Leaf = whole_term_chk_ins(CTXTc term,&(itrie_array[index].root),
			    &flag,check_cps_flag,expand_flag);
  switch_from_trie_assert;
  
  ctop_int(CTXTc 3,(Integer)Leaf);
  ctop_int(CTXTc 4,flag);
}

#ifdef MULTI_THREAD
void shas_trie_intern(CTXTdecl) {
  prolog_term term;
  int Trie_id,index,type;
  BTNptr Leaf;
  int flag;

  Trie_id = iso_ptoc_int(CTXTc 2,"trie_intern/2");
  term = ptoc_tag(CTXTc 3);
  //  check_cps_flag = ptoc_int(CTXTc 6);
  //  expand_flag = ptoc_int(CTXTc 7);
  SPLIT_TRIE_ID(Trie_id,index,type);
  if (shared_itrie_array[index].root != NULL 
      && BTN_Instr(shared_itrie_array[index].root) == trie_no_cp_fail) {
    printf("Inserting into trie with trie_no_cp_fail root\n");
  }

  switch_to_shared_trie_assert(&(shared_itrie_array[index].trie_mutex));
  Leaf = whole_term_chk_ins(CTXTc term,&(shared_itrie_array[index].root),
			    &flag,NO_CPS_CHECK,EXPAND_HASHES);
  switch_from_shared_trie_assert(&(shared_itrie_array[index].trie_mutex));
  
  ctop_int(CTXTc 4,(Integer)Leaf);
  ctop_int(CTXTc 5,flag);

}
#endif

/*----------------------------------------------------------------------*/

int private_trie_interned(CTXTdecl) {
  int ret_val = FALSE;
  Cell Leafterm, trie_term;
  int Trie_id,index,type;
  BTNptr *trie_root_addr;

  Trie_id = iso_ptoc_int(CTXTc 1,"private_trie_interned/3");
  trie_term =  ptoc_tag(CTXTc 2);
  Leafterm = ptoc_tag(CTXTc 3);

  SPLIT_TRIE_ID(Trie_id,index,type);
  trie_root_addr = &(itrie_array[index].root);

  if ((*trie_root_addr != NULL) && (!((long) *trie_root_addr & 0x3))) {
    XSB_Deref(trie_term);
    XSB_Deref(Leafterm);
    if ( isref(Leafterm) ) {  
      reg_arrayptr = reg_array -1;
      num_vars_in_var_regs = -1;
      pushreg(trie_term); 
      pcreg = (byte *)*trie_root_addr;
      ret_val =  TRUE;
    }
    else{
      xsb_instantiation_error(CTXTc "trie_interned/[2,4]",3);
    }
  }
  return(ret_val);
}

#ifdef MULTI_THREAD  
/* TLS: ensuring that LEAF is uninstantiated */
int shas_trie_interned(CTXTdecl) {
  int ret_val = FALSE;
  Cell Leafterm, trie_term;
  int Trie_id,index,type;
  BTNptr *trie_root_addr;

  Trie_id = iso_ptoc_int(CTXTc 2,"shas_trie_interned/[3]");
  trie_term =  ptoc_tag(CTXTc 3);
  Leafterm = ptoc_tag(CTXTc 4);

  SPLIT_TRIE_ID(Trie_id,index,type);
  trie_root_addr = &(shared_itrie_array[index].root);

  if ((*trie_root_addr != NULL) && (!((long) *trie_root_addr & 0x3))) {
    XSB_Deref(trie_term);
    XSB_Deref(Leafterm);
    //    if ( isref(Leafterm) ) {  
      reg_arrayptr = reg_array -1;
      num_vars_in_var_regs = -1;
      pushreg(trie_term); 
      pcreg = (byte *)*trie_root_addr;
      ret_val =  TRUE;
      //    }
      //    else xsb_instantiation_error(CTXTc "trie_interned/[2,4]",3);
  }
  return(ret_val);
}
#endif

/*----------------------------------------------------------------------*/

/*
 * This is builtin #162: TRIE_UNINTERN(+ROOT, +LEAF), to dispose a branch
 * of the trie rooted at Set_ArrayPtr[ROOT].
 * 
 * If called within a trie_retractall(), the CPS check will already
 * have been done: if it isn't safe to reclaim trie_dispose_nr() would
 * have been called; otherwise its safe and we don't need to check
 * here.  If called from within a trie_unintern() (maybe we should get
 * the names straight) we do need to check.
 */

void private_trie_unintern(CTXTdecl)
{
  BTNptr Leaf;
  int disposalType;
  int Trie_id,index,type;
  BTNptr *trie_root_addr;

  Trie_id = iso_ptoc_int(CTXTc 1,"trie_unintern/2");
  Leaf = (BTNptr)iso_ptoc_int(CTXTc 2,"trie_unintern/2");
  disposalType = ptoc_int(CTXTc 3);
 
  SPLIT_TRIE_ID(Trie_id,index,type);
  trie_root_addr = &(itrie_array[index].root);
  switch_to_trie_assert;

  if (disposalType == NO_CPS_CHECK)     
    delete_branch(CTXTc Leaf, trie_root_addr,VARIANT_EVAL_METHOD);
  else {
    if (!interned_trie_cps_check(CTXTc *trie_root_addr)) {
      //          printf(" really deleting branch \n");
      delete_branch(CTXTc Leaf, trie_root_addr,VARIANT_EVAL_METHOD);
    }
    else {
      //           printf(" safely deleting branch\n");
      safe_delete_branch(Leaf);
    }
  }
  switch_from_trie_assert;
}

#ifdef MULTI_THREAD
// For shared, disposalType is always NO_CPS_CHECK
void shas_trie_unintern(CTXTdecl)
{
  BTNptr Leaf;
  int Trie_id,index,type;

  Trie_id = iso_ptoc_int(CTXTc 2,"trie_unintern/2");
  Leaf = (BTNptr)iso_ptoc_int(CTXTc 3,"trie_unintern/2");
  //  disposalType = ptoc_int(CTXTc 3);
 
  SPLIT_TRIE_ID(Trie_id,index,type);
  switch_to_shared_trie_assert(&(shared_itrie_array[index].trie_mutex));;

  delete_branch(CTXTc Leaf, &(shared_itrie_array[index].root),VARIANT_EVAL_METHOD);
  switch_from_shared_trie_assert(&(shared_itrie_array[index].trie_mutex));
}
#endif

/*----------------------------------------------------------------------*/
/* 
 * For interned tries, I'm checking whether there is a trie choice
 * point with the same root as the the trie that we want to do
 * something with.  
 */

#define CAN_RECLAIM 0
#define CANT_RECLAIM 1

#define is_trie_instruction(cp_inst) \
 ((int) cp_inst >= 0x5c && (int) cp_inst < 0x80) \
	   || ((int) cp_inst >= 0x90 && (int) cp_inst < 0x94) 

int interned_trie_cps_check(CTXTdeclc BTNptr root) 
{
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  BTNptr pLeaf, trieNode;

  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      // Below we want interned_trie_tt
      trieNode = TrieNodeFromCP(cp_top1);
      if (IsInInternTrie(trieNode)) {
	pLeaf = trieNode;
	while ( IsNonNULL(pLeaf) && (! IsTrieRoot(pLeaf)) && 
		((int) TN_Instr(pLeaf) != trie_fail_unlock) ) {
	  pLeaf = BTN_Parent(pLeaf);
	}
	if (pLeaf == root) {
	  return CANT_RECLAIM;
	}
      }
    }
    cp_top1 = cp_prevtop(cp_top1);
  }
  return CAN_RECLAIM;
}


/*----------------------------------------------------------------------*/

/* Here, we check to see that the trie is non empty (via its root) and
   also valid (so that the free list may be rearranged.  If it is a
   pras trie, we do not need a CPS check.

   Actually, if the trie is not valid we just succeed -- arguably an
   error might be thrown.
 */

void private_trie_truncate(CTXTdeclc struct interned_trie_t* trie_root_addr, 
			   int index, int cps_check) {
  if ((trie_root_addr->root != NULL) &&
      (!((Integer) trie_root_addr->root & 0x3))) {
    if (cps_check == CPS_CHECK) {
      if (!interned_trie_cps_check(CTXTc trie_root_addr->root)) {
	switch_to_trie_assert;
	delete_trie(CTXTc trie_root_addr->root);
	switch_from_trie_assert;
      }
      else xsb_abort("[DELETE_TRIE] Backtracking through trie to be abolished.");
    } else delete_trie(CTXTc trie_root_addr->root);
    trie_root_addr->root = NULL;
  }
}

#ifdef MULTI_THREAD
void shas_trie_truncate(CTXTdeclc struct shared_interned_trie_t* trie_root_addr, int index) {
  if ((trie_root_addr->root != NULL) &&
      (!((Integer) trie_root_addr->root & 0x3))) {
    switch_to_shared_trie_assert(&(shared_itrie_array[index].trie_mutex));;
    delete_trie(CTXTc trie_root_addr->root);
    switch_from_shared_trie_assert(&(shared_itrie_array[index].trie_mutex));
    trie_root_addr->root = NULL;
  }
}
#endif

void trie_truncate(CTXTdeclc Integer Trie_id) {
  int index,type;
  
  SPLIT_TRIE_ID(Trie_id,index,type);
#ifdef MULTI_THREAD  
  if (type == PRGE_TRIE) {
    private_trie_truncate(CTXTc &(itrie_array[index]),index,CPS_CHECK);
  } else if (type == PRAS_TRIE) {
    private_trie_truncate(CTXTc &(itrie_array[index]),index,NO_CPS_CHECK);
  } else 
      shas_trie_truncate(CTXTc &(shared_itrie_array[index]),index);
#else
  if (type == PRGE_TRIE) {
    private_trie_truncate(CTXTc &(itrie_array[index]),index,CPS_CHECK);
  } else if (type == PRAS_TRIE) {
    private_trie_truncate(CTXTc &(itrie_array[index]),index,NO_CPS_CHECK);
  }
#endif
}

static void private_trie_drop(CTXTdeclc int i ) {
  int j;
	/* delete from trie list */
  if( !ISTRIENULL(itrie_array[i].prev_entry) ) {
    j = itrie_array[i].prev_entry;
    itrie_array[j].next_entry = itrie_array[i].next_entry ;
  }
  if( !ISTRIENULL(itrie_array[i].next_entry)) {
    j = itrie_array[i].next_entry;
    itrie_array[j].prev_entry = itrie_array[i].prev_entry ;
  }
  if( itrie_array_first_trie == i )
    itrie_array_first_trie = itrie_array[i].next_entry ;

  /* add to free list */
  if (ISTRIENULL(itrie_array_first_free)) {
    itrie_array_first_free = i;
    itrie_array[i].next_entry = TRIENULL;
  }
  else {	
    itrie_array[i].next_entry = itrie_array_first_free;
    itrie_array_first_free = i;
  }
  itrie_array[i].valid = FALSE;
}

#ifdef MULTI_THREAD
// Chaining not yet implemented for shared tries
static void shared_trie_drop(CTXTdeclc int i ) {
  shared_itrie_array[i].valid = FALSE;
}
#endif

/* truncate trie and remove properties */
void trie_drop(CTXTdecl) {
  int index,type;

  int Trie_id = iso_ptoc_int(CTXTc 2,"trie_drop/1");
  trie_truncate(CTXTc Trie_id);
  SPLIT_TRIE_ID(Trie_id,index,type);
  if (PRIVATE_TRIE(type)) 
    private_trie_drop(CTXTc index);
#ifdef MULTI_THREAD
  else shared_trie_drop(CTXTc index);
#endif
}

void first_trie_property(CTXTdecl) {
  int index,type;
  Integer Trie_id;

  if (!ISTRIENULL(itrie_array_first_trie)) {
    index = (int) (itrie_array_first_trie);

    if( !itrie_array[index].valid ) 
      xsb_existence_error(CTXTc "trie", reg[2], "trie_property", 1,1);

    type = itrie_array[index].type;
    ctop_int(CTXTc 3, type);
    SET_TRIE_ID(index,type,Trie_id);
    ctop_int(CTXTc 2, Trie_id);
    ctop_int(CTXTc 4, itrie_array[index].next_entry);
  }
  else ctop_int(CTXTc 2, TRIENULL);
}

void next_trie_property(CTXTdecl) {
  int index = ptoc_int(CTXTc 2);
  ctop_int(CTXTc 3, itrie_array[index].type);
  ctop_int(CTXTc 4,itrie_array[index].next_entry);
}

/*----------------------------------------------------------------------*/
/*

TLS -- 4/07 I'm keeping these in for now, but they may be taken out
when/if the interned tries evolve.

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

#ifndef MULTI_THREAD
static IGRptr IGRhead = NULL;
#endif

static IGRptr newIGR(long root)
{
  IGRptr igr;
  
  igr = (IGRptr) mem_alloc(sizeof(InternGarbageRoot),TABLE_SPACE);
  igr -> root   = root;
  igr -> leaves = NULL;
  igr -> next   = NULL;
  return igr;
}

static IGLptr newIGL(BTNptr leafn)
{
  IGLptr igl;
  
  igl = (IGLptr) mem_alloc(sizeof(InternGarbageLeaf),TABLE_SPACE);
  igl -> leaf = leafn;
  igl -> next = NULL;
  return igl;
}

static IGRptr getIGRnode(CTXTdeclc long rootn)
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

static IGRptr getAndRemoveIGRnode(CTXTdeclc long rootn)
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
#ifdef UNDEFINED
  while(p != NULL){
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
#endif
  p = newIGL(leafn);
  p -> next = r -> leaves;
  r -> leaves = p;
}

/*
  This feature does not yet support shared tries or call subsumption.
 */
void reclaim_uninterned_nr(CTXTdeclc long rootidx)
{
  IGRptr r = getAndRemoveIGRnode(CTXTc rootidx);
  IGLptr l, p;
  BTNptr leaf;

  if (r!=NULL)
    l = r-> leaves;
  else
    return;

  mem_dealloc(r,sizeof(InternGarbageRoot),TABLE_SPACE);

  while(l != NULL){
    /* printf("Loop b %p\n", l); */
    leaf = l -> leaf;
    p = l -> next;
    mem_dealloc(l,sizeof(InternGarbageLeaf),TABLE_SPACE);
    switch_to_trie_assert;
    if(IsDeletedNode(leaf)) {
      //      SYS_MUTEX_LOCK(MUTEX_TRIE);
      delete_branch(CTXTc leaf, &(itrie_array[rootidx].root),VARIANT_EVAL_METHOD);
      //      SYS_MUTEX_UNLOCK(MUTEX_TRIE);
    } else {
      /* This is allowed:
	 If we backtrack over a delete, the node that was marked for deletion
	 and placed in the garbage list is unmarked, but isn't removed from
	 the garbage list. So it is a non-deleted node on the garbage list.
	 It is removed from there only when we reclaim space.
      */
      xsb_dbgmsg((LOG_INTERN,"Non deleted interned node in garbage list - ok"));
    }

    switch_from_trie_assert;
    l = p;
  }

}


/*----------------------------------------------------------------------*/
/*
 * This is builtin : TRIE_DISPOSE_NR(+ROOT, +LEAF), to
 * mark for disposal (safe delete) a branch
 * of the trie rooted at Set_ArrayPtr[ROOT].
 * This function does not yet support shared tries.
 */
void trie_dispose_nr(CTXTdecl)
{
  BTNptr Leaf;
  long Rootidx;

  Rootidx = iso_ptoc_int(CTXTc 1,"trie_unintern_nr/2");
  Leaf = (BTNptr)iso_ptoc_int(CTXTc 2,"trie_unintern_nr/2");
  switch_to_trie_assert;
  //  SYS_MUTEX_LOCK(MUTEX_TRIE);
  insertLeaf(getIGRnode(CTXTc Rootidx), Leaf);
  //  SYS_MUTEX_UNLOCK(MUTEX_TRIE);
  safe_delete_branch(Leaf);
  switch_from_trie_assert;
}

/*----------------------------------------------------------------------*/
/*
 * This is builtin : TRIE_UNDISPOSE_NR(+ROOT, +LEAF), to
 * unmark a safely deleted branch.
 * This function does not yet support shared tries.
 */

void trie_undispose(CTXTdeclc long rootIdx, BTNptr leafn)
{
  IGRptr r = getIGRnode(CTXTc rootIdx);
  IGLptr p = r -> leaves;
  if(p == NULL){
    xsb_dbgmsg((LOG_INTERN,
   "In trie_undispose: The node being undisposed has been previously deleted"));
  } else{
    if(p -> leaf == leafn){
      r -> leaves = p -> next;
      mem_dealloc(p,sizeof(InternGarbageLeaf),TABLE_SPACE);
      if(r -> leaves == NULL){
	/* Do not want roots with no leaves hanging around */
	getAndRemoveIGRnode(CTXTc rootIdx);
      }
    }
    undelete_branch(leafn);
  }
}

/*======================================================================*/

/*
 *              TABLE ABOLISHING AND GARBAGE COLLECTING 
 *                    ===========================
 */

/*----------------------------------------------------------------------*/
/* 

 * When a table T is abolished, various checks must be made before its
 * space can be reclaimed.  T must be completed, and it must be
 * ensured that there are not any trie choice points for T in the
 * choice point stack.  In addition, the heap delay list must be
 * checked for pointers to T.  And finally, a check must be made that
 * there is a single active thread in order to reclaim shared tables.
 *
 * In the case of abolish_all_tables, if there are any incomplete
 * tables, or if there are trie nodes for completed tables on the
 * choice point stack, an error is thrown.  In the case of
 * abolish_table_pred(P) (and other abolishes), if P is not completed
 * an error is thrown; while if trie choice points for P are on the
 * stack, P is "abolished" (pointers in the TIF are reset) but its
 * space is not yet reclaimed.  Rather, a deleted table frame (DelTF)
 * is set up so that P can later be reclaimed upon a call to
 * gc_tables/1.  The same action is also taken if P is shared and
 * there is more than one active thread.  Note that if we have to
 * create a DelTF for them, shared tables will not be gc'd until we're
 * down to a single thread, so its best to call these abolishes when
 * we dont have any more backtracking points.
 *
 * Later, on a call to gc_tables/1 (which affects shared tables only
 * if there is a single active thread), the choice point stacks may be
 * traversed to mark those DelTF frames corresponding to tables with
 * trie CPs in the CP stack.  Once this is done, the chain of DelTF
 * frames is traversed to reclaim tables for those unmarked DelTF
 * frames (and free the frames) as well as to unmark the marked DelTF
 * frames.
 * 
 * Note that all of these require SLG_GC to be defined as we need to
 * properly traverse the CPS.  So, probably we should take out SLG_GC.
 */

/*------------------------------------------------------------------*/
/* Utility Code */
/*------------------------------------------------------------------*/

/* used by mt engine for shared tables */
DelTFptr deltf_chain_begin = (DelTFptr) NULL;

/* - - - - - */

xsbBool is_completed_table(TIFptr tif) {
  VariantSF sf;

  for ( sf = TIF_Subgoals(tif);  IsNonNULL(sf);  
	sf = (VariantSF)subg_next_subgoal(sf) )
    if ( ! is_completed(sf) )
      return FALSE;
  return TRUE;
}

/* - - - - - */

Psc get_psc_for_answer_trie_cp(CTXTdeclc BTNptr pLeaf) 
{
  TIFptr tif_ptr;

  while ( IsNonNULL(pLeaf) && (! IsTrieRoot(pLeaf)) && 
			       ((int) TN_Instr(pLeaf) != trie_fail_unlock) ) {
    pLeaf = BTN_Parent(pLeaf);
  }

  if (TN_Parent(pLeaf)) { /* workaround till all roots pointing to subg's */
    tif_ptr = subg_tif_ptr(TN_Parent(pLeaf));
    //    printf("Predicate is %s/%d\n",get_name(TIF_PSC(tif_ptr)),
    //    get_arity(TIF_PSC(tif_ptr)));
    return TIF_PSC(tif_ptr);
  } else {
    fprintf(stderr,"Null parent ptr for TN Root Node type: %d Trie type %d\n",
	    TN_TrieType(pLeaf), TN_NodeType(pLeaf));
    return NULL;
  }
}

/* - - - - - */

VariantSF get_subgoal_frame_for_answer_trie_cp(CTXTdeclc BTNptr pLeaf) 
{

  while ( IsNonNULL(pLeaf) && (! IsTrieRoot(pLeaf)) && 
			       ((int) TN_Instr(pLeaf) != trie_fail_unlock) ) {
    pLeaf = BTN_Parent(pLeaf);
  }

  if (TN_Parent(pLeaf)) { /* workaround till all roots pointing to subg's */
    return (VariantSF) TN_Parent(pLeaf);
  } else {
    fprintf(stderr,"Null parent ptr for TN Root Node type: %d Trie type %d\n",
	    TN_TrieType(pLeaf), TN_NodeType(pLeaf));
    return NULL;
  }
}

/* - - - - - */

TIFptr get_tif_for_answer_trie_cp(CTXTdeclc BTNptr pLeaf)
{

  while ( IsNonNULL(pLeaf) && (! IsTrieRoot(pLeaf)) && 
			       ((int) TN_Instr(pLeaf) != trie_fail_unlock) ) {
    pLeaf = BTN_Parent(pLeaf);
  }
  return subg_tif_ptr(TN_Parent(pLeaf));
}

/* - - - - - */

BTNptr get_call_trie_from_subgoal_frame(CTXTdeclc VariantSF subgoal)
{
  
  BTNptr pLeaf = subg_leaf_ptr(subgoal);

  while ( IsNonNULL(BTN_Parent(pLeaf)) ) {
    pLeaf = BTN_Parent(pLeaf);
  }
  return pLeaf;
}

/* - - - - - */

/* Creating two doubly-linked chains -- one for all DelTf, the other
   for Deltfs for this predicate.  Depending on the value of
   *chain_begin this can be used for either private or shared
   predicates */
DelTFptr inline New_DelTF_Pred(CTXTdeclc TIFptr pTIF, DelTFptr *chain_begin, xsbBool Warn) {		      
  DelTFptr pDTF;
  
  pDTF = (DelTFptr)mem_alloc(sizeof(DeletedTableFrame),TABLE_SPACE);	
  if ( IsNULL(pDTF) )							
    xsb_abort("Ran out of memory in allocation of DeletedTableFrame");	
   DTF_CallTrie(pDTF) = TIF_CallTrie(pTIF);				
   DTF_Subgoals(pDTF) = TIF_Subgoals(pTIF);				
   DTF_Type(pDTF) = DELETED_PREDICATE;					
   DTF_Warn(pDTF) = (byte) Warn;					
   DTF_Mark(pDTF) = 0;                                                  
   DTF_PrevDTF(pDTF) = 0;						
   DTF_PrevPredDTF(pDTF) = 0;						
   DTF_NextDTF(pDTF) = *chain_begin;				
   DTF_NextPredDTF(pDTF) = TIF_DelTF(pTIF);				
   if (*chain_begin) DTF_PrevDTF(*chain_begin) = pDTF;	
   if (TIF_DelTF(pTIF))  DTF_PrevPredDTF(TIF_DelTF(pTIF)) = pDTF;	
   *chain_begin = pDTF;                                            
   TIF_DelTF(pTIF) = pDTF;                                              
   return pDTF;
}

DelTFptr inline New_DelTF_Subgoal(CTXTdeclc TIFptr pTIF, VariantSF pSubgoal,
			      DelTFptr *chain_begin, xsbBool Warn) { 
  DelTFptr pDTF;

  pDTF = (DelTFptr)mem_alloc(sizeof(DeletedTableFrame),TABLE_SPACE);	
   if ( IsNULL(pDTF) )							
     xsb_abort("Ran out of memory in allocation of DeletedTableFrame");	
   DTF_CallTrie(pDTF) = NULL;						
   DTF_Subgoal(pDTF) = pSubgoal;					
   DTF_Type(pDTF) = DELETED_SUBGOAL;					
   DTF_Mark(pDTF) = 0;                                                  
   DTF_Warn(pDTF) = (byte) Warn;					
   DTF_PrevDTF(pDTF) = 0;						
   DTF_PrevPredDTF(pDTF) = 0;						
   DTF_NextDTF(pDTF) = *chain_begin;			
   DTF_NextPredDTF(pDTF) = TIF_DelTF(pTIF);				
   if (*chain_begin) DTF_PrevDTF(*chain_begin) = pDTF;			
   if (TIF_DelTF(pTIF))  DTF_PrevPredDTF(TIF_DelTF(pTIF)) = pDTF;	
   *chain_begin = pDTF;					
   TIF_DelTF(pTIF) = pDTF;                                              
   return pDTF;
  }

/* If there is a deltf with same subgoals and arity (can this be) dont
   add; otherwise if there is a subgoal for this pred, delete the
   deltf (it must be for this generation of the table)
*/
void check_insert_global_deltf_pred(CTXTdeclc TIFptr tif, xsbBool Warning) { 
  DelTFptr dtf = TIF_DelTF(tif), next_dtf; 
  BTNptr call_trie = TIF_CallTrie(tif); 
  VariantSF subgoals = TIF_Subgoals(tif); 
  int found = 0;

  SYS_MUTEX_LOCK(MUTEX_TABLE);
  while ( dtf != 0 ) {
    next_dtf = DTF_NextPredDTF(dtf);
    if (DTF_Type(dtf) == DELETED_PREDICATE && 
	DTF_CallTrie(dtf) == call_trie && DTF_Subgoals(dtf) == subgoals)
      found = 1;
    if (DTF_Type(dtf) == DELETED_SUBGOAL) {
      //      fprintf(stderr,"Predicate over-riding subgoal for %s/%d\n",
      //      get_name(TIF_PSC(tif)),get_arity(TIF_PSC(tif)));
      Free_Global_DelTF_Subgoal(dtf,tif);
    }
    dtf = next_dtf;
  }
  if (!found) {
    New_DelTF_Pred(CTXTc tif,&deltf_chain_begin,Warning);
  }
  TIF_CallTrie(tif) = NULL;
  TIF_Subgoals(tif) = NULL;
  SYS_MUTEX_UNLOCK(MUTEX_TABLE);
}

void check_insert_global_deltf_subgoal(CTXTdeclc VariantSF subgoal,xsbBool Warning) {
  DelTFptr dtf;
  TIFptr tif;

  if (!DELETED_SUBGOAL_FRAME(subgoal)) {
      SYS_MUTEX_LOCK(MUTEX_TABLE);

      DELETE_SUBGOAL_FRAME(subgoal); /* set DELETED bit */

      tif = subg_tif_ptr(subgoal);
      //      printf("check_insert_subgoal s %p t %p\n",subgoal,tif);

      dtf = New_DelTF_Subgoal(CTXTc tif,subgoal,&deltf_chain_begin,Warning);

      if (subg_prev_subgoal(subgoal) != 0) 
	subg_prev_subgoal(subgoal) = subg_next_subgoal(subgoal);

      if (subg_next_subgoal(subgoal) != 0) 
	subg_next_subgoal(subgoal) = subg_prev_subgoal(subgoal);

      subg_deltf_ptr(subgoal) = dtf;

      SYS_MUTEX_UNLOCK(MUTEX_TABLE);
    }
}

#ifdef MULTI_THREAD

// extern void printTIF(TIFptr);

void check_insert_private_deltf_pred(CTXTdeclc TIFptr tif, xsbBool Warning) {
  DelTFptr dtf = TIF_DelTF(tif), next_dtf;
  BTNptr call_trie = TIF_CallTrie(tif);
  VariantSF subgoals = TIF_Subgoals(tif);	
  int found = 0;

  while ( dtf != 0 ) {
    next_dtf = DTF_NextPredDTF(dtf);
    if (DTF_Type(dtf) == DELETED_PREDICATE && 
	DTF_CallTrie(dtf) == call_trie && DTF_Subgoals(dtf) == subgoals)
      found = 1;
    if (DTF_Type(dtf) == DELETED_SUBGOAL) {
      //            fprintf(stderr,"Predicate over-riding subgoal for %s/%d\n",
      //            get_name(TIF_PSC(tif)),get_arity(TIF_PSC(tif)));
      Free_Private_DelTF_Subgoal(dtf,tif);
    }
    dtf = next_dtf;
  }
  if (!found) {
    //    New_Private_DelTF_Pred(CTXTc dtf,tif,Warning);
    New_DelTF_Pred(CTXTc tif,&private_deltf_chain_begin,Warning);
  }
  TIF_CallTrie(tif) = NULL;
  TIF_Subgoals(tif) = NULL;
}

#define check_insert_shared_deltf_pred(context, tif, warning)	\
  check_insert_global_deltf_pred(context, tif, warning)	 

/* * * * * * * */

void check_insert_private_deltf_subgoal(CTXTdeclc VariantSF subgoal,xsbBool Warning)
{
  DelTFptr dtf;
  TIFptr tif;

  if (!DELETED_SUBGOAL_FRAME(subgoal)) {

    DELETE_SUBGOAL_FRAME(subgoal); /* set DELETED bit */

    tif = subg_tif_ptr(subgoal);
    dtf = New_DelTF_Subgoal(CTXTc tif,subgoal,&private_deltf_chain_begin,Warning);

    if (subg_prev_subgoal(subgoal) != 0) 
      subg_prev_subgoal(subgoal) = subg_next_subgoal(subgoal);

    if (subg_next_subgoal(subgoal) != 0) 
      subg_next_subgoal(subgoal) = subg_prev_subgoal(subgoal);
    
    subg_deltf_ptr(subgoal) = dtf;
  }

}

#define check_insert_shared_deltf_subgoal(context, subgoal,Warning)	\
  check_insert_global_deltf_subgoal(context, subgoal,Warning)	 

#else /* not MULTI_THREAD */

#define check_insert_private_deltf_pred(tif,warning)	\
  check_insert_global_deltf_pred(tif,warning)	 

#define check_insert_private_deltf_subgoal(subgoal,Warning)	\
  check_insert_global_deltf_subgoal(subgoal,Warning)	 

#endif

/* - - - - - - - - - - */

/* used for transitive abolishes */
void inline mark_delaylist_tabled_preds(CTXTdeclc CPtr dlist) {
  Cell tmp_cell;
  VariantSF subgoal;

  //  if (dlist != NULL) {
  //    printf("checking list ");print_delay_list(CTXTc stddbg, dlist);
    while (islist(dlist)) {
      dlist = clref_val(dlist);
      // printf("\n checking element "); print_delay_element(CTXTc stddbg, cell(dlist));
      tmp_cell = cell((CPtr)cs_val(cell(dlist)) + 1);
      subgoal = (VariantSF) addr_val(tmp_cell);
      //      printf(" mark subgoal ");print_subgoal(stddbg, subgoal);
      gc_mark_tif(subg_tif_ptr(subgoal));
      dlist = (CPtr)cell(dlist+1);
    }
  }

void inline unmark_delaylist_tabled_preds(CTXTdeclc CPtr dlist) {
  Cell tmp_cell;
  VariantSF subgoal;

  while (islist(dlist)) {
    dlist = clref_val(dlist);
    tmp_cell = cell((CPtr)cs_val(cell(dlist)) + 1);
    subgoal = (VariantSF) addr_val(tmp_cell);
    gc_unmark_tif(subg_tif_ptr(subgoal));
    dlist = (CPtr)cell(dlist+1);
  }
}

void mark_cp_tabled_preds(CTXTdecl)
{
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  TIFptr tif;
  BTNptr trieNode;

  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      trieNode = TrieNodeFromCP(cp_top1);
      if (IsInAnswerTrie(trieNode)) {
	tif = get_tif_for_answer_trie_cp(CTXTc trieNode);
       gc_mark_tif(tif);
      }
    }
    mark_delaylist_tabled_preds(CTXTc cp_pdreg(cp_top1));
    cp_top1 = cp_prevtop(cp_top1);
  }
}

void unmark_cp_tabled_preds(CTXTdecl)
{
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  TIFptr tif;
  BTNptr trieNode;
  
  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      trieNode = TrieNodeFromCP(cp_top1);
      if (IsInAnswerTrie(trieNode)) {
	tif = get_tif_for_answer_trie_cp(CTXTc trieNode);
	gc_unmark_tif(tif);
      }
    }
    unmark_delaylist_tabled_preds(CTXTc cp_pdreg(cp_top1));
    cp_top1 = cp_prevtop(cp_top1);
  }
}

/*------------------------------------------------------------------*/
/* abolish_table_call() and supporting code */
/*------------------------------------------------------------------*/

/* Used when deleting a single subgoal
   Recurse through CP stack looking for trie nodes that match PSC.
*/
int abolish_table_call_cps_check(CTXTdeclc VariantSF subgoal) {
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  BTNptr trieNode;
  CPtr dlist;
  Cell tmp_cell;

  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      // Below we want basic_answer_trie_tt, ts_answer_trie_tt
      trieNode = TrieNodeFromCP(cp_top1);
      if (IsInAnswerTrie(trieNode)) {
	if (subgoal == get_subgoal_frame_for_answer_trie_cp(CTXTc trieNode)) 
	  return CANT_RECLAIM;
      }
    }
    /* now check the delay list for the call */
    dlist = cp_pdreg(cp_top1);
    while (islist(dlist)) {
      dlist = clref_val(dlist);
      // printf("\n checking element "); print_delay_element(CTXTc stddbg, cell(dlist));
      tmp_cell = cell((CPtr)cs_val(cell(dlist)) + 1);
      if (subgoal == (VariantSF) addr_val(tmp_cell)) 
	return CANT_RECLAIM;
      dlist = (CPtr)cell(dlist+1);
      }
    cp_top1 = cp_prevtop(cp_top1);
  }
  return CAN_RECLAIM;
}

/* used for transitive abolishes */
void inline mark_delaylist_tabled_subgoals(CTXTdeclc CPtr dlist) {
  Cell tmp_cell;
  VariantSF subgoal;

  //  if (dlist != NULL) {
  //    printf("checking list ");print_delay_list(CTXTc stddbg, dlist);
    while (islist(dlist)) {
      dlist = clref_val(dlist);
      // printf("\n checking element "); print_delay_element(CTXTc stddbg, cell(dlist));
      tmp_cell = cell((CPtr)cs_val(cell(dlist)) + 1);
      subgoal = (VariantSF) addr_val(tmp_cell);
      //      printf(" mark subgoal ");print_subgoal(stddbg, subgoal);
      GC_MARK_SUBGOAL(subgoal);
      dlist = (CPtr)cell(dlist+1);
    }
  }

void inline unmark_delaylist_tabled_subgoals(CTXTdeclc CPtr dlist) {
  Cell tmp_cell;
  VariantSF subgoal;

  while (islist(dlist)) {
    dlist = clref_val(dlist);
    tmp_cell = cell((CPtr)cs_val(cell(dlist)) + 1);
    subgoal = (VariantSF) addr_val(tmp_cell);
    //    printf(" unmark subgoal ");print_subgoal(stddbg, subgoal);
    GC_UNMARK_SUBGOAL(subgoal);
    dlist = (CPtr)cell(dlist+1);
  }
}

/* Mark and unmark are used when deleting a set of depending subgoals */
void mark_cp_tabled_subgoals(CTXTdecl) {
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  VariantSF subgoal;
  BTNptr trieNode;
  
  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      //  printf("found trie instruction %x %d\n",cp_inst,
      //                 TSC_TrieType(((BTNptr)string_val(*(cp_top1+CP_SIZE+1)))));
      trieNode = TrieNodeFromCP(cp_top1);
      if (IsInAnswerTrie(trieNode)) {
	//	printf("is in answer trie\n");
	subgoal = get_subgoal_frame_for_answer_trie_cp(CTXTc trieNode);
	//	printf("Marking ");print_subgoal(CTXTc stddbg, subgoal);printf("\n");
	GC_MARK_SUBGOAL(subgoal);
      }
    }
    mark_delaylist_tabled_subgoals(CTXTc cp_pdreg(cp_top1));
    cp_top1 = cp_prevtop(cp_top1);
  }
}

void unmark_cp_tabled_subgoals(CTXTdecl)
{
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  VariantSF subgoal;
  BTNptr trieNode;
  
  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      trieNode = TrieNodeFromCP(cp_top1);
      if (IsInAnswerTrie(trieNode)) {
	subgoal = get_subgoal_frame_for_answer_trie_cp(CTXTc trieNode);
	GC_UNMARK_SUBGOAL(subgoal);
      }
    }
    unmark_delaylist_tabled_subgoals(CTXTc cp_pdreg(cp_top1));
    cp_top1 = cp_prevtop(cp_top1);
  }
}


/* incremental */
int abolish_table_call_incr(CTXTdeclc VariantSF subgoal) {
  if(IsIncrSF(subgoal))
    abolish_incr_call(CTXTc subgoal->callnode);
  
  return TRUE;
}

/*------------------------------------------------------------------

In the presence of conditional answers, its not enough to simply
delete (or invalidate) a table T for a subgoal S -- in most
circumstances we should delete all other tables that depend on T along
with T.  To do this we need to find all conditional answers for T, and
check their back pointers transitively, and eventually obtain the set
of subgoals that depend on T.

The way the algorithm works is that pointers to all conditional
answers to T are put the an answer_stack, and S is marked as visited.
Back-pointers of these answes are traversed -- and if the newly
visited answers belong to an unvisited subgoal S', S' is marked as
visited and a pointer to S's subgoal frame is put on the done stack.
This continues until all answers and subgoals have been traversed.  Of
course, back-pointers from the visitied subgoals themselves are also
traversed.

So, at the end of find_answers_for_subgoal(S) the answer_stack is
incremented to include all unconditional answers for S.  At the end of
find_subgoal_backward_dependencies(), the done_stack contains all
visited subgoals, while the answer_stack contains all conditional
answers for all visited subgoals.

-----------------------------------------------------------------*/

// Answer stack utilities -------------------------------------

#ifndef MULTI_THREAD
int answer_stack_top = 0;
BTNptr * answer_stack = NULL;
int answer_stack_size = 0;
#endif

#define answer_stack_increment 1000

void reset_answer_stack(CTXTdecl) {
  answer_stack_top = 0;
  //  answer_stack_current_pos = 0;
}

#define push_answer_node(as_leaf) {				                  \
    if (answer_stack_top >= answer_stack_size) {			          \
      unsigned long old_answer_stack_size = answer_stack_size;		          \
      answer_stack_size = answer_stack_size + answer_stack_increment;	\
      answer_stack = (BTNptr *) mem_realloc(answer_stack,			  \
					  old_answer_stack_size*sizeof(BTNptr *), \
					  answer_stack_size*sizeof(BTNptr *),     \
					  TABLE_SPACE);			          \
    }									          \
    answer_stack[answer_stack_top] =     as_leaf;		                  \
    answer_stack_top++;							          \
  }

void print_answer_stack(CTXTdecl) {
  int frame = 0;
  while (frame < answer_stack_top) {
    printf("answer_frame %d answer %p ",frame, answer_stack[frame]);
    print_subgoal(CTXTc stddbg, asi_subgoal((ASI) Child(answer_stack[frame]))); printf("\n");
    frame++;
  }
  printf("Answer queue TOP = %d\n", answer_stack_top);
  //  printf("Answer queue CURRENT = %d\n", answer_stack_current_pos);
}

// End of answer stack utilities -------------------------------------

/* Trie traversal was copied from one of the delete routines. 
   There's probably a cleaner way to do this.
*/
int find_answers_for_subgoal(CTXTdeclc VariantSF subgoal) {

  BTNptr root, sib, chil;  
  int trie_op_top = 0;
  int trie_node_top = 0;
  int trie_hh_top = -1;
  int  num_leaves = 0;

  char *delete_trie_op = NULL;
  BTNptr *delete_trie_node = NULL;
  BTHTptr *delete_trie_hh = NULL;
  int trie_op_size, trie_node_size, trie_hh_size;

  if (!delete_trie_op) {
    delete_trie_op = (char *)mem_alloc(DELETE_TRIE_STACK_INIT*sizeof(char),TABLE_SPACE);
    delete_trie_node = (BTNptr *)mem_alloc(DELETE_TRIE_STACK_INIT*sizeof(BTNptr),TABLE_SPACE);
    delete_trie_hh = (BTHTptr *)mem_alloc(DELETE_TRIE_STACK_INIT*sizeof(BTHTptr),TABLE_SPACE);
    trie_op_size = trie_node_size = trie_hh_size = DELETE_TRIE_STACK_INIT;
  }

  delete_trie_op[0] = 0;
  if (subg_ans_root_ptr(subgoal)) {
    delete_trie_node[0] = subg_ans_root_ptr(subgoal);
    while (trie_op_top >= 0) {
      switch (delete_trie_op[trie_op_top--]) {
      case DT_DS:
	root = delete_trie_node[trie_node_top--];
	break;
      case DT_HT:
	trie_hh_top--;
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
	    if (IsLeafNode(root)) {
            // 12-05-07 
            // put one more condition for an answer leaf to be added: it must not be visited before
            // It might be visited before in find_single_backward_dependencies
            // minhdt - Do we need this change???
            // if ((is_conditional_answer(root)) && (!VISITED_ANSWER(root))){
	      if (is_conditional_answer(root)){
		push_answer_node(root);
		num_leaves++;
	      }
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
	} else {
	  fprintf(stdwarn,"null node encountered in find_answers_for_subgoal ");
	  print_subgoal(CTXTc stdwarn,subgoal); fprintf(stdwarn,"\n");
	  break;
	}
      }
    }
  }
  mem_dealloc(delete_trie_op,trie_op_size*sizeof(char),TABLE_SPACE); delete_trie_op = NULL;
  mem_dealloc(delete_trie_node,trie_node_size*sizeof(BTNptr),TABLE_SPACE); delete_trie_node = NULL;
  mem_dealloc(delete_trie_hh,trie_hh_size*sizeof(BTHTptr),TABLE_SPACE); delete_trie_hh = NULL;
  trie_op_size = 0; 
  //  print_answer_stack(CTXT);
  return num_leaves;
}

// Done stack utilities -------------------------------------

#ifndef MULTI_THREAD
int done_subgoal_stack_top = 0;
VariantSF *done_subgoal_stack = NULL;
int done_subgoal_stack_size = 0;
#endif

#define done_subgoal_stack_increment 1000

void inline push_done_subgoal_node(CTXTdeclc VariantSF Subgoal) {				
    if (done_subgoal_stack_top >= done_subgoal_stack_size) {		
      unsigned long old_done_subgoal_stack_size = done_subgoal_stack_size; 
      done_subgoal_stack_size = done_subgoal_stack_size + done_subgoal_stack_increment; 
      done_subgoal_stack = (VariantSF *) mem_realloc(done_subgoal_stack,	
						    old_done_subgoal_stack_size*sizeof(VariantSF *), 
						    done_subgoal_stack_size*sizeof(VariantSF *), 
						    TABLE_SPACE);	
    }									
    done_subgoal_stack[done_subgoal_stack_top] = Subgoal;	
    done_subgoal_stack_top++;						
  }

void print_done_subgoal_stack(CTXTdecl) {
  int frame = 0;
  while (frame < done_subgoal_stack_top) {
    printf("done_subgoal_frame %d ",frame);
    print_subgoal(CTXTc stddbg, done_subgoal_stack[frame]);
    printf("\n");
    frame++;
  }
}

void reset_done_subgoal_stack(CTXTdecl) {
  done_subgoal_stack_top = 0;
}

#define traverse_subgoal_ndes(subgoal)	{				\
    DL nde_delayList;							\
    BTNptr nde_as_prev;							\
    PNDE ndeElement;							\
    ndeElement = subg_nde_list(subgoal);				\
    while (ndeElement) {						\
      nde_delayList = pnde_dl(ndeElement);				\
      nde_as_prev = dl_asl(nde_delayList);				\
      if (nde_as_prev && !subg_deltf_ptr(asi_subgoal((ASI) Child(nde_as_prev))) \
	  && !VISITED_SUBGOAL(asi_subgoal((ASI) Child(nde_as_prev)))) {	\
	MARK_VISITED_SUBGOAL(asi_subgoal((ASI) Child(nde_as_prev)));	\
	push_done_subgoal_node(CTXTc asi_subgoal((ASI) Child(nde_as_prev)));	\
	find_answers_for_subgoal(CTXTc asi_subgoal((ASI) Child(nde_as_prev))); \
      }									\
      ndeElement = pnde_next(ndeElement);				\
    }									\
  }

/* Start by marking input subgoal as visited (and put it on the done
   stack).  Then find conditional answers for subgoal (putting them on
   answer stack), and put these answers on the answer stack.  Finally,
   traverse its ndes and set last_subgoal to subgoal.

   Thereafter, go through each answer on the answer stack. If the
   subgoal is different from the last subgoal, traverse the subgoal's
   ndes (this works because all answers for any subgoal will form a
   contiguous segment of the answer stack).  Also traverse the answers
   backpointers as well.

   Traversing either nde's or pde's marks new subgoals as they are
   visited, and puts all of their conditional answes onto the answer
   stack.

   Thus, we examine pde's for each answer during the search exactly
   once, and examine the nde's for each subgoal exactly once.  At the
   end, the answer stack contains all answers that we've traversed,
   and the done subgoal stack contains all subgoals that we've
   traversed.

   As of 5/07, fbsd() is the only system code that uses answer_stack.
   For reasons of concurrency, it doesn't deallocate the answer stack
   when its done -- I'm assuming that if an appliction gets abolishes
   tables with conditional answers once, its likely to do so again --
   if this is a bad assumption, a deallocate can be put in the end.
*/

int find_subgoal_backward_dependencies(CTXTdeclc VariantSF subgoal) {
    BTNptr as_leaf;
    PNDE pdeElement;
    DL delayList;
    BTNptr as_prev;
    VariantSF last_subgoal;
    int answer_stack_current_pos = 0;

    reset_done_subgoal_stack(CTXT);
    reset_answer_stack(CTXT);
    MARK_VISITED_SUBGOAL(subgoal);
    push_done_subgoal_node(CTXTc subgoal); 
    find_answers_for_subgoal(CTXTc subgoal);
    traverse_subgoal_ndes(subgoal);
    last_subgoal = subgoal;

    while (answer_stack_current_pos < answer_stack_top) {
      as_leaf = answer_stack[answer_stack_current_pos];
      if (asi_subgoal((ASI) Child(as_leaf)) != last_subgoal) {
	last_subgoal = asi_subgoal((ASI) Child(as_leaf));
	traverse_subgoal_ndes(last_subgoal);
      }
      pdeElement = asi_pdes((ASI) Child(as_leaf));
      while (pdeElement) {
        delayList = pnde_dl(pdeElement);
        as_prev = dl_asl(delayList);
	if (as_prev && !subg_deltf_ptr(asi_subgoal((ASI) Child(as_prev)))
	   && !VISITED_SUBGOAL(asi_subgoal((ASI) Child(as_prev)))) {
	  MARK_VISITED_SUBGOAL(asi_subgoal((ASI) Child(as_prev)));
	  subgoal = asi_subgoal((ASI) Child(as_prev));
	  push_done_subgoal_node(CTXTc subgoal);
	  find_answers_for_subgoal(CTXTc subgoal);
	}
        pdeElement = pnde_next(pdeElement);
      }
      answer_stack_current_pos++;
    }
    //    print_answer_stack(CTXT);
    reset_answer_stack(CTXT);
    //    print_done_subgoal_stack(CTXT);
    return done_subgoal_stack_top;
  }

// End of done stack utilities -------------------------------------

/* For use when abolishing tabled subgoals that do not have conditional answers */
void abolish_table_call_single(CTXTdeclc VariantSF subgoal) {

    TIFptr tif;
    Psc psc;
    int action;

    //    printf("in abolish_table_call_single\n");
    tif = subg_tif_ptr(subgoal);
    psc = TIF_PSC(tif);

    if (flags[NUM_THREADS] == 1 || !get_shared(psc)) {
      action = abolish_table_call_cps_check(CTXTc subgoal);
    } else action = CANT_RECLAIM;

    SET_TRIE_ALLOCATION_TYPE_SF(subgoal); // set smBTN to private/shared
    if (action == CAN_RECLAIM) {
      delete_branch(CTXTc subgoal->leaf_ptr, &tif->call_trie,VARIANT_EVAL_METHOD); /* delete call */
      delete_variant_sf_and_answers(CTXTc subgoal, TRUE); // (warn if cond)
    }
    else {
      //      fprintf(stderr,"Delaying abolish of call in use for: %s/%d\n",
      //      get_name(psc),get_arity(psc));
#ifndef MULTI_THREAD
      delete_branch(CTXTc subgoal->leaf_ptr, &tif->call_trie,VARIANT_EVAL_METHOD); /* delete call */
      check_insert_private_deltf_subgoal(CTXTc subgoal,TRUE);
#else
      if (!get_shared(psc)) {
	delete_branch(CTXTc subgoal->leaf_ptr, &tif->call_trie,VARIANT_EVAL_METHOD); /* delete call */
	check_insert_private_deltf_subgoal(CTXTc subgoal,TRUE);
      }
      else {
	safe_delete_branch(subgoal->leaf_ptr); 
	check_insert_shared_deltf_subgoal(CTXT, subgoal,TRUE);
      }
#endif
    }
}

/* Assuming no intermixing of shared and private tables */
void abolish_table_call_transitive(CTXTdeclc VariantSF subgoal) {

    TIFptr tif;
    Psc psc;
    int action;

    //    printf("in abolish_table_call_transitive\n");
    tif = subg_tif_ptr(subgoal);
    psc = TIF_PSC(tif);

    find_subgoal_backward_dependencies(CTXTc subgoal);

    if (flags[NUM_THREADS] == 1 || !get_shared(psc)) {
      mark_cp_tabled_subgoals(CTXT);
      action = CAN_RECLAIM;
    } else action = CANT_RECLAIM;

    SET_TRIE_ALLOCATION_TYPE_SF(subgoal); // set smBTN to private/shared
    while (done_subgoal_stack_top) {
      done_subgoal_stack_top--;
      subgoal = done_subgoal_stack[done_subgoal_stack_top];
      //      printf(" abolishing ");
      //      print_subgoal(CTXTc stddbg, done_subgoal_stack[done_subgoal_stack_top]);  printf("\n");
      tif = subg_tif_ptr(subgoal);
      if (action == CAN_RECLAIM && !GC_MARKED_SUBGOAL(subgoal) ) {
	//	printf("really abolishing\n");
	delete_branch(CTXTc subgoal->leaf_ptr, &tif->call_trie,VARIANT_EVAL_METHOD); /* delete call */
	delete_variant_sf_and_answers(CTXTc subgoal,FALSE); // delete answers (dont warn if cond)
      }
      else {
	//	printf("Mark %x GC %x\n",subgoal->visited,GC_MARKED_SUBGOAL(subgoal));
	if (!get_shared(psc)) {
	  delete_branch(CTXTc subgoal->leaf_ptr, &tif->call_trie,VARIANT_EVAL_METHOD); /* delete call */
	  check_insert_private_deltf_subgoal(CTXTc subgoal,FALSE);
	}
#ifdef MULTI_THREAD
	else {
	  safe_delete_branch(subgoal->leaf_ptr); 
	  check_insert_shared_deltf_subgoal(CTXT, subgoal,FALSE);
	}
#endif
      }
    }
    reset_done_subgoal_stack(CTXT);
    unmark_cp_tabled_subgoals(CTXT);
}

/* Took check for incomplete out -- its been done in tables.P.
However, we now need to check the default setting (settable in
xsb_flag) as well as an option set by the options list, if any. 

Currently, calls can be abolished only for call-variance -- hence
the use of varsf_has_conditional_answer()
*/
void abolish_table_call(CTXTdeclc VariantSF subgoal, int invocation_flag) {
  //  printf("in abolish_table_call\n");
  if (varsf_has_conditional_answer(subgoal) 
      && (invocation_flag == ABOLISH_TABLES_TRANSITIVELY 
	  || !(invocation_flag == ABOLISH_TABLES_DEFAULT 
	       && flags[TABLE_GC_ACTION] == ABOLISH_TABLES_SINGLY))) {
    //    printf("calling atc\n");
    abolish_table_call_transitive(CTXTc subgoal);
    }
  else {
    //    printf("calling ats\n");
    abolish_table_call_single(CTXTc subgoal);
  }
}

/*------------------------------------------------------------------*/
/* abolish_table_pred() and supporting code */
/*------------------------------------------------------------------*/

// done_tif_stack stack utilities -------------------------------------

#ifndef MULTI_THREAD
int done_tif_stack_top = 0;
TIFptr * done_tif_stack = NULL;
int done_tif_stack_size = 0;
#endif

#define done_tif_stack_increment 1000

void reset_done_tif_stack(CTXTdecl) {
  done_tif_stack_top = 0;
}

void unvisit_done_tifs(CTXTdecl) {
  int i;
  for (i = 0; i < done_tif_stack_top; i++) {
    TIF_Visited(done_tif_stack[i]) = 0;
  }
}

void inline push_done_tif_node(CTXTdeclc TIFptr node) {					\
    if (done_tif_stack_top >= done_tif_stack_size) {			\
      unsigned long old_done_tif_stack_size = done_tif_stack_size;	\
      done_tif_stack_size = done_tif_stack_size + done_tif_stack_increment;	\
      done_tif_stack = (TIFptr *) mem_realloc(done_tif_stack,			  \
					      old_done_tif_stack_size*sizeof(TIFptr *), \
					      done_tif_stack_size*sizeof(TIFptr *), \
					      TABLE_SPACE);		\
    }									\
    done_tif_stack[done_tif_stack_top] =     node;			\
    done_tif_stack_top++;						\
  }

void print_done_tif_stack(CTXTdecl) {
  int frame = 0;
  while (frame < done_tif_stack_top) {
    printf("done_tif_frame %d tif %p (%s/%d)\n",frame, done_tif_stack[frame],
	   get_name(TIF_PSC(done_tif_stack[frame])),get_arity(TIF_PSC(done_tif_stack[frame])));
    frame++;
  }
}

//----------------------------------------------------------------------

/* TLS: this doesn't yet work correctly for WF call subsumption */
static void find_subgoals_and_answers_for_pred(CTXTdeclc TIFptr tif) {

  VariantSF pSF;
  TRIE_W_LOCK();
  pSF = TIF_Subgoals(tif);
  if ( IsNULL(pSF) )   return;

  while (pSF) {
    if (varsf_has_conditional_answer(pSF)) {
      push_done_subgoal_node(CTXTc pSF);
      find_answers_for_subgoal(CTXTc pSF);
	} 
    pSF = subg_next_subgoal(pSF);
  } /* there is a child of "node" */
  TRIE_W_UNLOCK();
}

int find_pred_backward_dependencies(CTXTdeclc TIFptr tif) {
    BTNptr as_leaf;
    PNDE pdeElement, ndeElement;
    DL delayList, nde_delayList;
    BTNptr as_prev, nde_as_prev;
    VariantSF subgoal;
    int answer_stack_current_pos = 0;
    int done_subgoal_stack_current_pos = 0;

    reset_done_tif_stack(CTXT);
    if (!TIF_Visited(tif)) {
      TIF_Visited(tif) = 1;
      push_done_tif_node(CTXTc tif); 
      find_subgoals_and_answers_for_pred(CTXTc tif);

      //      print_answer_stack(CTXT);
    while (answer_stack_current_pos < answer_stack_top 
	   || done_subgoal_stack_current_pos < done_subgoal_stack_top) {
      while (answer_stack_current_pos < answer_stack_top) {
	as_leaf = answer_stack[answer_stack_current_pos];
	pdeElement = asi_pdes((ASI) Child(as_leaf));
	while (pdeElement) {
	  delayList = pnde_dl(pdeElement);
	  as_prev = dl_asl(delayList);
	  if (as_prev) tif = subg_tif_ptr(asi_subgoal((ASI) Child(as_prev)));
	  if (/*!tif_deltf_ptr(tif) &&*/ !TIF_Visited(tif)) {
	    TIF_Visited(tif) = 1;
	    push_done_tif_node(CTXTc tif);
	    find_subgoals_and_answers_for_pred(CTXTc tif);
	  }
	  pdeElement = pnde_next(pdeElement);
	}
	answer_stack_current_pos++;
      }
      while (done_subgoal_stack_current_pos < done_subgoal_stack_top) {
	subgoal = done_subgoal_stack[done_subgoal_stack_current_pos];
	ndeElement = subg_nde_list(subgoal);				
	while (ndeElement) {						
	  nde_delayList = pnde_dl(ndeElement);				
	  nde_as_prev = dl_asl(nde_delayList);				
	  if (nde_as_prev) tif = subg_tif_ptr(asi_subgoal((ASI) Child(nde_as_prev)));
	  if (/*!tif_deltf_ptr(tif) &&*/ !TIF_Visited(tif)) {
	    TIF_Visited(tif) = 1;
	    push_done_tif_node(CTXTc tif);
	    find_subgoals_and_answers_for_pred(CTXTc tif);
	  }								
	  ndeElement = pnde_next(ndeElement);				
	}								
	done_subgoal_stack_current_pos++;
      }
    }
    }

    //    print_answer_stack(CTXT);
    //    print_done_subgoal_stack(CTXT);
    //    print_done_tif_stack(CTXT);
    unvisit_done_tifs(CTXT);
    reset_answer_stack(CTXT);
    reset_done_subgoal_stack(CTXT);
    return done_tif_stack_top;
  }

/* 
   Recurse through CP stack looking for trie nodes that match PSC.
   Also look through delay list.
   Returns 1 if found a psc match, 0 if safe to delete now
*/
int abolish_table_pred_cps_check(CTXTdeclc Psc psc) 
{
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  BTNptr trieNode;
  CPtr dlist;
  Cell tmp_cell;
  VariantSF subgoal;

  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  /* First check the CP stack */
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      // Below we want basic_answer_trie_tt, ts_answer_trie_tt
      trieNode = TrieNodeFromCP(cp_top1);
      if (IsInAnswerTrie(trieNode)) {
	if (psc == get_psc_for_answer_trie_cp(CTXTc trieNode)) {
	  return CANT_RECLAIM;
	}
      }
    }
    /* Now check delaylist */
    dlist = cp_pdreg(cp_top1);
    while (islist(dlist) ) {
      dlist = clref_val(dlist);
      tmp_cell = cell((CPtr)cs_val(cell(dlist)) + 1);
      subgoal = (VariantSF) addr_val(tmp_cell);
      if (psc == TIF_PSC(subg_tif_ptr(subgoal))) 
	return CANT_RECLAIM;
      dlist = (CPtr)cell(dlist+1);
    }
    cp_top1 = cp_prevtop(cp_top1);
  }
  return CAN_RECLAIM;
}

/* Delays spece reclamation if the cps check does not pass OR if
   shared and more than 1 thread is active.

  abolish_table_predicate does not reclaim space for previously
 "abolished" tables in deltf frames.  Need to do gc tables for
  that. 

  Don't need a warning flag for this predicate -- it must always warn
*/
inline void abolish_table_pred_single(CTXTdeclc TIFptr tif, int cps_check_flag) {
  int action;

  if(get_incr(TIF_PSC(tif))) {  /* incremental */
    xsb_warn("[abolish_table_predicate] Abolish incremental table"
	     " of predicate %s/%d. This can cause unexpected behavior.\n", 
	     get_name(TIF_PSC(tif)), get_arity(TIF_PSC(tif)));
    free_incr_hashtables(tif);
  }

  if (IsVariantPredicate(tif) && IsNULL(TIF_CallTrie(tif))) {
    return ;
  }

  if ( ! is_completed_table(tif) ) {
      xsb_abort("[abolish_table_pred] Cannot abolish incomplete table"
		" of predicate %s/%d\n", get_name(TIF_PSC(tif)), get_arity(TIF_PSC(tif)));
  }

  if (flags[NUM_THREADS] == 1 || !get_shared(TIF_PSC(tif))) {
    if (cps_check_flag) action = abolish_table_pred_cps_check(CTXTc TIF_PSC(tif));
    else action = TIF_Mark(tif);  /* 1 = CANT_RECLAIM; 0 = CAN_RECLAIM */
  }
  else action = CANT_RECLAIM;
  if (action == CAN_RECLAIM) {
    delete_predicate_table(CTXTc tif,TRUE);
  }
  else {
    //        fprintf(stderr,"Delaying abolish of table in use: %s/%d\n",
    //        get_name(psc),get_arity(psc));
#ifndef MULTI_THREAD
    check_insert_private_deltf_pred(CTXTc tif,TRUE);
#else
    if (!get_shared(TIF_PSC(tif)))
      check_insert_private_deltf_pred(CTXTc tif,TRUE);
    else
      check_insert_shared_deltf_pred(CTXT, tif,TRUE);
#endif
  }
}  

inline void abolish_table_pred_transitive(CTXTdeclc TIFptr tif, int cps_check_flag) {
  int action;

  find_pred_backward_dependencies(CTXTc tif);

  if (flags[NUM_THREADS] == 1 || !get_shared(TIF_PSC(tif))) {
    if (cps_check_flag) mark_cp_tabled_preds(CTXT);
    action = CAN_RECLAIM;
  } else action = CANT_RECLAIM;

  while (done_tif_stack_top) {
    done_tif_stack_top--;
    tif = done_tif_stack[done_tif_stack_top];

    if(get_incr(TIF_PSC(tif))) {     /* incremental */
      xsb_warn("[abolish_table_predicate] Abolish incremental table"
	       " of predicate %s/%d. This can cause unexpected behavior.\n", 
	       get_name(TIF_PSC(tif)), get_arity(TIF_PSC(tif)));
      free_incr_hashtables(tif);
    }

    if ( ! is_completed_table(tif) ) 
      xsb_abort("[abolish_table_pred] Cannot abolish incomplete table"
		" of predicate %s/%d\n", get_name(TIF_PSC(tif)), get_arity(TIF_PSC(tif)));

    //        printf(" abolishing %s/%d\n",get_name(TIF_PSC(tif)),get_arity(TIF_PSC(tif)));
    if (action == CAN_RECLAIM && !TIF_Mark(tif) ) {
      //            printf("   really abolishing %s/%d\n",get_name(TIF_PSC(tif)),get_arity(TIF_PSC(tif)));
      transitive_delete_predicate_table(CTXTc tif,FALSE);
    }
    /* This check is needed to avoid makding a deltf frame multiple times for the same predicate, 
       when it is encountered more tha once by find_pred_backwared_dependencies() */
    else if (TIF_Subgoals(tif)) {
      //        fprintf(stderr,"Delaying abolish of table in use: %s/%d\n",
      //        get_name(psc),get_arity(psc));
#ifndef MULTI_THREAD
      //      print_deltf_chain(CTXT);
      //      printTIF(tif);
      check_insert_private_deltf_pred(CTXTc tif,FALSE);
#else
      if (!get_shared(TIF_PSC(tif)))
	check_insert_private_deltf_pred(CTXTc tif,FALSE);
      else
	check_insert_shared_deltf_pred(CTXT, tif,FALSE);
#endif
      }
  }
  if (cps_check_flag) unmark_cp_tabled_preds(CTXT);
}  

inline void abolish_table_predicate_switch(CTXTdeclc TIFptr tif, Psc psc, int invocation_flag, 
					  int cps_check_flag) {

  //  printf("atps %s/%d\n",get_name(psc),get_arity(psc));
  if (get_variant_tabled(psc)
      && (invocation_flag == ABOLISH_TABLES_TRANSITIVELY 
	  || (invocation_flag == ABOLISH_TABLES_DEFAULT 
	      && flags[TABLE_GC_ACTION] == ABOLISH_TABLES_TRANSITIVELY))) {
    abolish_table_pred_transitive(CTXTc tif, cps_check_flag);
  }
    else abolish_table_pred_single(CTXTc tif, cps_check_flag);
}

/* When calling a_t_p_switch, cps_check_flag is set to true, to ensure a check. 
 invocation_flag (default, transitive) has been set on Prolog side. */
inline void abolish_table_predicate(CTXTdeclc Psc psc, int invocation_flag) {
  TIFptr tif;

  tif = get_tip(CTXTc psc);

  gc_tabled_preds(CTXT);
  if ( IsNULL(tif) ) {
    xsb_abort("[abolish_table_pred] Attempt to delete non-tabled predicate (%s/%d)\n",
	      get_name(psc), get_arity(psc));
  }

  abolish_table_predicate_switch(CTXTc tif, psc, invocation_flag, TRUE);
}

/*------------------------------------------------------------------*/
/* Table gc and supporting code */
/*------------------------------------------------------------------*/

/* Go through and mark DelTfs to ensure that on sweep we dont abolish
  "active" predicates we're backtracking through.  Note that only the
  first DelTF in the pred-specific chain may be active in this sense.
  And its active only if calltrie and subgoals for tif are 0 -- if
  they are 0, the table has been abolished (even though we're
  backtracking through it).  If they aren't 0, we're backtracking
  through a different table altogether, and we needn't mark.
*/

void inline mark_deltfs(CTXTdeclc TIFptr tif, VariantSF subgoal) {
  BTNptr call_trie;
  DelTFptr dtf;

  call_trie = get_call_trie_from_subgoal_frame(CTXTc subgoal);
  //	printf("subgoal %p call_trie %p\n",subgoal,call_trie);
	
  dtf = TIF_DelTF(tif);
  // Cycle through all deltfs for this pred
  while (dtf) {
    if (DTF_CallTrie(dtf) == call_trie) {
      DTF_Mark(dtf) = 1;
      dtf = NULL;
    } else dtf = DTF_NextPredDTF(dtf);
  }

  //	if (TIF_CallTrie(tif) == NULL && TIF_Subgoals(tif) == NULL) {
  //	 //   && !get_shared(TIF_PSC(tif))) { 
  //	  dtf = TIF_DelTF(tif);
  //	  DTF_Mark(dtf) = 1;
  //	}
	
  /* Now check for subgoal DelTFs */
  if (is_completed(subgoal)) {
    if (subg_deltf_ptr(subgoal) != NULL) {
      DTF_Mark((DelTFptr) subg_deltf_ptr(subgoal)) = 1;
    }
  }
}

void inline gc_mark_delaylist_tabled_preds(CTXTdeclc CPtr dlist) {
  Cell tmp_cell;
  VariantSF subgoal;

  //  if (dlist != NULL) {
  //    printf("checking list ");print_delay_list(CTXTc stddbg, dlist);
    while (islist(dlist)) {
      dlist = clref_val(dlist);
      // printf("\n checking element "); print_delay_element(CTXTc stddbg, cell(dlist));
      tmp_cell = cell((CPtr)cs_val(cell(dlist)) + 1);
      subgoal = (VariantSF) addr_val(tmp_cell);
      mark_deltfs(CTXTc subg_tif_ptr(subgoal), subgoal);
      dlist = (CPtr)cell(dlist+1);
    }
  }

void mark_tabled_preds(CTXTdecl) { 
  CPtr cp_top1,cp_bot1 ; byte cp_inst;
  TIFptr tif;
  VariantSF subgoal;
  BTNptr trieNode;

  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but need to distinguish asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      trieNode = TrieNodeFromCP(cp_top1);
      if (IsInAnswerTrie(trieNode)) {

	/* Check for predicate DelTFs */
	tif = get_tif_for_answer_trie_cp(CTXTc trieNode);
	subgoal = get_subgoal_frame_for_answer_trie_cp(CTXTc trieNode);
	mark_deltfs(CTXTc tif, subgoal);
      }
    }

    /* Now check delaylist */
    gc_mark_delaylist_tabled_preds(CTXTc cp_pdreg(cp_top1));

    cp_top1 = cp_prevtop(cp_top1);
  }
}

/* Mark only private tables -- ignore shared tables. Used by mt system
   when gc-ing with more than 1 active thread -- and used in lieu
   of mark_tabled_preds() */
#ifdef MULTI_THREAD
void mark_private_tabled_preds(CTXTdecl) { 
  CPtr cp_top1,cp_bot1 ; byte cp_inst;
  TIFptr tif;
  VariantSF subgoal;
  BTNptr trieNode;
  
  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but need to distinguish asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      trieNode = TrieNodeFromCP(cp_top1);
      if (IsInAnswerTrie(trieNode)) {

	/* Check for predicate DelTFs */
	tif = get_tif_for_answer_trie_cp(CTXTc trieNode);
	if (!get_shared(TIF_PSC(tif))) {
	  subgoal = get_subgoal_frame_for_answer_trie_cp(CTXTc trieNode);
	  mark_deltfs(CTXTc tif, subgoal);
	}
      }
    }
    /* Now check delaylist */
    gc_mark_delaylist_tabled_preds(CTXTc cp_pdreg(cp_top1));

    cp_top1 = cp_prevtop(cp_top1);
  }
}

int sweep_private_tabled_preds(CTXTdecl) {
  DelTFptr deltf_ptr, next_deltf_ptr;
  int dtf_cnt = 0;
  TIFptr tif_ptr;

  deltf_ptr = private_deltf_chain_begin;
  SET_TRIE_ALLOCATION_TYPE_PRIVATE();
  while (deltf_ptr) {
    next_deltf_ptr = DTF_NextDTF(deltf_ptr);
    if (DTF_Mark(deltf_ptr)) {
      //      tif_ptr = subg_tif_ptr(DTF_Subgoals(deltf_ptr));
      //           fprintf(stderr,"Skipping: %s/%d\n",
      //   get_name(TIF_PSC(tif_ptr)),get_arity(TIF_PSC(tif_ptr)));
      DTF_Mark(deltf_ptr) = 0;
      dtf_cnt++;
    }
    else {
      if (DTF_Type(deltf_ptr) == DELETED_PREDICATE) {
	tif_ptr = subg_tif_ptr(DTF_Subgoals(deltf_ptr));
	//	fprintf(stderr,"Garbage Collecting Predicate: %s/%d\n",
	//		get_name(TIF_PSC(tif_ptr)),get_arity(TIF_PSC(tif_ptr)));
	reclaim_deleted_predicate_table(CTXTc deltf_ptr);
	Free_Private_DelTF_Pred(deltf_ptr,tif_ptr);
      } else 
	if (DTF_Type(deltf_ptr) == DELETED_SUBGOAL) {
	  tif_ptr = subg_tif_ptr(DTF_Subgoal(deltf_ptr));
	  //	 	  fprintf(stderr,"Garbage Collecting Subgoal: %s/%d\n",
	  //			  get_name(TIF_PSC(tif_ptr)),get_arity(TIF_PSC(tif_ptr)));
	  delete_variant_sf_and_answers(CTXTc DTF_Subgoal(deltf_ptr),DTF_Warn(deltf_ptr)); 
	  Free_Private_DelTF_Subgoal(deltf_ptr,tif_ptr);
	}
    }
    deltf_ptr = next_deltf_ptr;
  }
  return dtf_cnt;
}
#endif

/* No mutex on this predicate, as global portions can only be called
   with one active thread.  This actually reclaims both abolished
   predicates and subgoals */

int sweep_tabled_preds(CTXTdecl) {
  DelTFptr deltf_ptr, next_deltf_ptr;
  int dtf_cnt = 0;
  TIFptr tif_ptr;

  /* Free global deltfs */
  deltf_ptr = deltf_chain_begin;
  SET_TRIE_ALLOCATION_TYPE_SHARED();
  while (deltf_ptr) {
    next_deltf_ptr = DTF_NextDTF(deltf_ptr);
    if (DTF_Mark(deltf_ptr)) {
      //      tif_ptr = subg_tif_ptr(DTF_Subgoals(deltf_ptr));
      //      fprintf(stderr,"Skipping: %s/%d\n",
      //      get_name(TIF_PSC(tif_ptr)),get_arity(TIF_PSC(tif_ptr)));
      DTF_Mark(deltf_ptr) = 0;
      dtf_cnt++;
    }
    else {
      if (DTF_Type(deltf_ptr) == DELETED_PREDICATE) {
	tif_ptr = subg_tif_ptr(DTF_Subgoals(deltf_ptr));
	//  printf("fia\n");printTIF((TIFptr) 0x54c620);
	//	fprintf(stderr,"Garbage Collecting Predicate: %s/%d\n",
	//get_name(TIF_PSC(tif_ptr)),get_arity(TIF_PSC(tif_ptr)));
	reclaim_deleted_predicate_table(CTXTc deltf_ptr);
	Free_Global_DelTF_Pred(deltf_ptr,tif_ptr);
      } else 
	if (DTF_Type(deltf_ptr) == DELETED_SUBGOAL) {
	  tif_ptr = subg_tif_ptr(DTF_Subgoal(deltf_ptr));
	  //	    fprintf(stderr,"Garbage Collecting Subgoal: %s/%d\n",
	  //   get_name(TIF_PSC(tif_ptr)),get_arity(TIF_PSC(tif_ptr)));
	  delete_variant_sf_and_answers(CTXTc DTF_Subgoal(deltf_ptr),DTF_Warn(deltf_ptr)); 
	  Free_Global_DelTF_Subgoal(deltf_ptr,tif_ptr);
	}
    }
    deltf_ptr = next_deltf_ptr;
  }

#ifdef MULTI_THREAD
  dtf_cnt = dtf_cnt + sweep_private_tabled_preds(CTXT);
#endif

  return dtf_cnt;
}

/* * * * * * * * * * * * * * * 
 * In MT engine gcs does not gc shared tables if there is more than
 * one thread. 
 */

#ifndef MULTI_THREAD
int gc_tabled_preds(CTXTdecl) {
  //    printf("gc-ing tabled preds\n");
  //    print_deltf_chain(CTXT);
  mark_tabled_preds(CTXT);
  return sweep_tabled_preds(CTXT);
}
#else
int gc_tabled_preds(CTXTdecl) 
{

  if (flags[NUM_THREADS] == 1) {
    mark_tabled_preds(CTXT);
    return sweep_tabled_preds(CTXT);
  } else {
    mark_private_tabled_preds(CTXT);
    return sweep_private_tabled_preds(CTXT);
  } 
}
#endif

/*----------------------------------------------------------------------*/
/* abolish_module_tables() and supporting code */
/*------------------------------------------------------------------*/

/* - - - - - - - - - - */

int abolish_usermod_tables(CTXTdecl)
{
  unsigned long i;
  Pair pair;
  Psc psc;
  TIFptr tif;

  mark_cp_tabled_preds(CTXT);

  for (i=0; i<symbol_table.size; i++) {
    if ((pair = (Pair) *(symbol_table.table + i))) {
      byte type;
      
      psc = pair_psc(pair);
      type = get_type(psc);
      if (type == T_DYNA || type == T_PRED) 
	if (!get_data(psc) || isstring(get_data(psc)) ||
	    !strcmp(get_name(get_data(psc)),"usermod") ||
	    !strcmp(get_name(get_data(psc)),"global")) 
	  if (get_tabled(psc)) {
	    tif = get_tip(CTXTc psc);
	    abolish_table_predicate_switch(CTXTc tif, psc, ABOLISH_TABLES_SINGLY, FALSE);
	  }
    }
  }

  unmark_cp_tabled_preds(CTXT);

  return TRUE;
}

/* - - - - - - - - - - */

int abolish_module_tables(CTXTdeclc const char *module_name)
{
  Pair modpair, pair;
  byte type;
  Psc psc, module;
  TIFptr tif;
  
  mark_cp_tabled_preds(CTXT);
  modpair = (Pair) flags[MOD_LIST];
  
  while (modpair && 
	 strcmp(module_name,get_name(pair_psc(modpair))))
    modpair = pair_next(modpair);

  if (!modpair) {
    xsb_warn("[abolish_module_tables] Module %s not found.\n",
		module_name);
    return FALSE;
  }

  module = pair_psc(modpair);
  pair = (Pair) get_data(module);

  while (pair) {
    psc = pair_psc(pair);
    type = get_type(psc);
    if (type == T_DYNA || type == T_PRED) 
      if (get_tabled(psc)) {
	tif = get_tip(CTXTc psc);
	abolish_table_predicate_switch(CTXTc tif, psc, ABOLISH_TABLES_SINGLY, FALSE);
      }
    pair = pair_next(pair);
  }
  unmark_cp_tabled_preds(CTXT);
  return TRUE;
}

/*----------------------------------------------------------------------*/
/* abolish_private/shared_tables() and supporting code */
/*------------------------------------------------------------------*/

/* Freeing deltfs is necessary in abolish_all_*_tables so that we
   don't later try to reclaim a subgoal or predicate that had been
   reclaimed by abolish_all_tables */

static inline void free_global_deltfs(CTXTdecl) {

  DelTFptr next_deltf;
  DelTFptr deltf = deltf_chain_begin;
  deltf_chain_begin = NULL;

  while (deltf) {
    next_deltf = DTF_NextDTF(deltf);
    mem_dealloc(deltf,sizeof(DeletedTableFrame),TABLE_SPACE);		
    deltf = next_deltf;
  }
}

#ifdef MULTI_THREAD
static inline void thread_free_private_deltfs(CTXTdecl) {

  DelTFptr next_deltf;
  DelTFptr deltf = private_deltf_chain_begin;

  while (deltf) {
    next_deltf = DTF_NextDTF(deltf);
    mem_dealloc(deltf,sizeof(DeletedTableFrame),TABLE_SPACE);		
    deltf = next_deltf;
  }
}
#endif

#define check_for_incomplete_tables(PredName) \
  {					    \
    CPtr csf;						 \
    for ( csf = top_of_complstk;  csf != COMPLSTACKBOTTOM;	\
	  csf = csf + COMPLFRAMESIZE )				\
      if ( ! is_completed(compl_subgoal_ptr(csf)) ) {		   \
	xsb_table_error(CTXTc "["PredName"] Illegal table operation"	\
			"\n\t Cannot abolish incomplete tables");	\
      }									\
  }

#ifdef MULTI_THREAD

/* Checks to see whether there are any private tables in choice point
   stack */
int abolish_mt_tables_cps_check(CTXTdecl,xsbBool isPrivate) 
{
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  BTNptr trieNode;
  CPtr dlist;
  Cell tmp_cell;
  VariantSF subgoal;

  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;
  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      trieNode = TrieNodeFromCP(cp_top1);
      // Below we want basic_answer_trie_tt, ts_answer_trie_tt
      if (IsInAnswerTrie(trieNode)) {
	if (get_private(get_psc_for_answer_trie_cp(CTXTc trieNode)) == isPrivate) {
	  return CANT_RECLAIM;
	}
      }
    }
    /* Now check delaylist */
    dlist = cp_pdreg(cp_top1);
    while (islist(dlist) ) {
      dlist = clref_val(dlist);
      tmp_cell = cell((CPtr)cs_val(cell(dlist)) + 1);
      subgoal = (VariantSF) addr_val(tmp_cell);
      if (get_private(TIF_PSC(subg_tif_ptr(subgoal))) == isPrivate) 
	return CANT_RECLAIM;
      dlist = (CPtr)cell(dlist+1);
    }
    cp_top1 = cp_prevtop(cp_top1);
  }
  return CAN_RECLAIM;
}

/* will not reclaim space if more than one thread */
void abolish_shared_tables(CTXTdecl) {
  TIFptr abol_tif;

  mark_cp_tabled_preds(CTXT);

  for (abol_tif = tif_list.first ; abol_tif != NULL
	 ; abol_tif = TIF_NextTIF(abol_tif) ) {
	abolish_table_predicate_switch(CTXTc abol_tif, TIF_PSC(abol_tif), ABOLISH_TABLES_SINGLY, FALSE);
  }

  unmark_cp_tabled_preds(CTXT);

}

/* will not reclaim space if more than one thread (via fast_atp) */
void abolish_all_shared_tables(CTXTdecl) {
  TIFptr pTIF;

  // FALSE means we found a shared table
  if (flags[NUM_THREADS] != 1) {
    xsb_table_error(CTXTc 
		    "abolish_all_shared_tables/1 called with more than one active thread.");
  } else {
    
    check_for_incomplete_tables("abolish_all_shared_tables/0");
    if ( abolish_mt_tables_cps_check(CTXTc FALSE) ) 
      xsb_abort("[abolish_all_shared_tables/0] Illegal table operation"
		"\n\t Backtracking through tables to be abolished.");
    else {
      for ( pTIF = tif_list.first; IsNonNULL(pTIF) ; pTIF = TIF_NextTIF(pTIF) ) {
	  TIF_CallTrie(pTIF) = NULL;
    	  TIF_Subgoals(pTIF) = NULL;
      }

      free_global_deltfs(CTXT);
      SM_ReleaseResources(smTableBTN);
      TrieHT_FreeAllocatedBuckets(smTableBTHT);
      SM_ReleaseResources(smTableBTHT);
      SM_ReleaseResources(smALN);
      SM_ReleaseResources(smVarSF);
      SM_ReleaseResources(smASI);
      
      abolish_wfs_space(CTXT);
    }
  }
}

void abolish_private_tables(CTXTdecl) {
  TIFptr abol_tif;

  mark_cp_tabled_preds(CTXT);

  for (abol_tif = private_tif_list.first ; abol_tif != NULL
	 ; abol_tif = TIF_NextTIF(abol_tif) ) {
    //    printf("calling %s\n",get_name(TIF_PSC(abol_tif)));
    abolish_table_predicate_switch(CTXTc abol_tif, TIF_PSC(abol_tif), ABOLISH_TABLES_SINGLY, FALSE);
  }

  unmark_cp_tabled_preds(CTXT);

}

void abolish_all_private_tables(CTXTdecl) {

  TIFptr pTIF;

  check_for_incomplete_tables("abolish_all_private_tables/0");

  // TRUE means we found a private table
  if ( abolish_mt_tables_cps_check(CTXTc TRUE) ) 
    xsb_abort("[abolish_all_private_tables/0] Illegal table operation"
		  "\n\t Backtracking through tables to be abolished.");
  else {
    for ( pTIF = private_tif_list.first; IsNonNULL(pTIF)
	  			       ; pTIF = TIF_NextTIF(pTIF) ) {
	  TIF_CallTrie(pTIF) = NULL;
    	  TIF_Subgoals(pTIF) = NULL;
    }

    thread_free_private_deltfs(CTXT);
    SM_ReleaseResources(*private_smTableBTN);
    TrieHT_FreeAllocatedBuckets(*private_smTableBTHT);
    SM_ReleaseResources(*private_smTableBTHT);
    SM_ReleaseResources(*private_smTSTN);
    TrieHT_FreeAllocatedBuckets(*private_smTSTHT);
    SM_ReleaseResources(*private_smTSTHT);
    SM_ReleaseResources(*private_smTSIN);
    SM_ReleaseResources(*private_smALN);
    SM_ReleaseResources(*private_smVarSF);
    SM_ReleaseResources(*private_smProdSF);
    SM_ReleaseResources(*private_smConsSF);
    SM_ReleaseResources(*private_smASI);

    abolish_private_wfs_space(CTXT);
  }
}

extern struct TDispBlkHdr_t tdispblkhdr; // defined in loader

/* This function handles the case when one thread creates a private
   tif, exits, its xsb_thread_id is reused, and the new thread creates
   a private tif for the same table.  Mutex may not be needed here, as
   we're freeing private resources (nobody except the current thread
   will access &(tdispblk->Thread0))[xsb_thread_entry]) */

void thread_free_private_tifs(CTXTdecl) {
  struct TDispBlk_t *tdispblk;
  TIFptr tip;

  SYS_MUTEX_LOCK( MUTEX_TABLE );
  for (tdispblk=tdispblkhdr.firstDB 
	 ; tdispblk != NULL ; tdispblk=tdispblk->NextDB) {
    if (xsb_thread_entry <= tdispblk->MaxThread) {
      tip = (&(tdispblk->Thread0))[xsb_thread_entry];
      if (tip) {
	(&(tdispblk->Thread0))[xsb_thread_entry] = (TIFptr) NULL; {
	  Free_Private_TIF(tip);
	}
      }
    }
  }
  SYS_MUTEX_UNLOCK( MUTEX_TABLE );
}

void release_private_tabling_resources(CTXTdecl) {

  thread_free_private_deltfs(CTXT);
  thread_free_private_tifs(CTXT);
  SM_ReleaseResources(*private_smTableBTN);
  TrieHT_FreeAllocatedBuckets(*private_smTableBTHT);
  SM_ReleaseResources(*private_smTableBTHT);
  SM_ReleaseResources(*private_smTSTN);
  TrieHT_FreeAllocatedBuckets(*private_smTSTHT);
  SM_ReleaseResources(*private_smTSTHT);
  SM_ReleaseResources(*private_smTSIN);
  SM_ReleaseResources(*private_smALN);
  SM_ReleaseResources(*private_smVarSF);
  SM_ReleaseResources(*private_smProdSF);
  SM_ReleaseResources(*private_smConsSF);
  SM_ReleaseResources(*private_smASI);
}

#endif

/*----------------------------------------------------------------------*/
/* abolish_all_tables() and supporting code */
/*------------------------------------------------------------------*/

/*
 * Frees all the tabling space resources (with a hammer)
 * WFS stuff released elsewhere -- including smASI.
 */

void release_all_tabling_resources(CTXTdecl) {
  free_global_deltfs(CTXT);
  SM_ReleaseResources(smTableBTN);
  TrieHT_FreeAllocatedBuckets(smTableBTHT);
  SM_ReleaseResources(smTableBTHT);
  SM_ReleaseResources(smTSTN);
  //    SM_ReleaseResources(*private_smTSTN);
  TrieHT_FreeAllocatedBuckets(smTSTHT);
  SM_ReleaseResources(smTSTHT);
  SM_ReleaseResources(smTSIN);
  SM_ReleaseResources(smALN);
  SM_ReleaseResources(smVarSF);
  SM_ReleaseResources(smProdSF);
  SM_ReleaseResources(smConsSF);
  SM_ReleaseResources(smASI);

  /* In mt engine, also release private resources */
#ifdef MULTI_THREAD
    thread_free_private_deltfs(CTXT);
    SM_ReleaseResources(*private_smTableBTN);
    TrieHT_FreeAllocatedBuckets(*private_smTableBTHT);
    SM_ReleaseResources(*private_smTableBTHT);
    //    SM_ReleaseResources(*private_smTSTN);
    TrieHT_FreeAllocatedBuckets(*private_smTSTHT);
    SM_ReleaseResources(*private_smTSTHT);
    //    SM_ReleaseResources(*private_smTSIN);
    SM_ReleaseResources(*private_smALN);
    SM_ReleaseResources(*private_smVarSF);
    //    SM_ReleaseResources(*private_smProdSF);
    //    SM_ReleaseResources(*private_smConsSF);
    SM_ReleaseResources(*private_smASI);
#endif

}

/* TLS: Unlike the other abolishes, "all" aborts if it detects the
   presence of CPs for completed tables (incomplete tables are caught
   as before, by examining the completion stack).  It also aborts if
   called with more than one thread.
*/

void abolish_all_tables_cps_check(CTXTdecl) 
{
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  BTNptr trieNode;

  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    /* Check for trie instructions */
    if ( is_trie_instruction(cp_inst)) {
      trieNode = TrieNodeFromCP(cp_top1);
      /* Here, we want call_trie_tt,basic_answer_trie_tt,ts_answer_trie_tt"*/
      if (IsInAnswerTrie(trieNode)) {
	xsb_abort("[abolish_all_tables/0] Illegal table operation"
		  "\n\t Backtracking through tables to be abolished.");
      }
    }
    /* Now check delaylist */
    if ( cp_pdreg(cp_top1) != (CPtr) NULL) 
      xsb_abort("[abolish_all_tables/0] Illegal table operation"
		"\n\t tables to be abolished are in delay list.");
      cp_top1 = cp_prevtop(cp_top1);
  }
}

#if !defined(WIN_NT) || defined(CYGWIN) 
inline 
#endif
void abolish_all_tables(CTXTdecl)
{
  TIFptr pTIF;
  
  check_for_incomplete_tables("abolish_all_shared_tables/0");

  if (flags[NUM_THREADS] == 1) {
    abolish_all_tables_cps_check(CTXT) ;
  } else {
    xsb_table_error(CTXTc 
		    "abolish_all_tables/1 called with more than one active thread.");
  }
   
  for ( pTIF = tif_list.first; IsNonNULL(pTIF); pTIF = TIF_NextTIF(pTIF) ) {
    TIF_CallTrie(pTIF) = NULL;
    TIF_Subgoals(pTIF) = NULL;
  }

#ifdef MULTI_THREAD
    for ( pTIF = private_tif_list.first; IsNonNULL(pTIF)
  	  ; pTIF = TIF_NextTIF(pTIF) ) {
      TIF_CallTrie(pTIF) = NULL;
      TIF_Subgoals(pTIF) = NULL;
    }
#endif

  reset_freeze_registers;
  openreg = COMPLSTACKBOTTOM;
  hashtable1_destroy_all(0);  /* free all incr hashtables in use */
  release_all_tabling_resources(CTXT);
  abolish_wfs_space(CTXT); 

}

/*
* void abolish_if_tabled(CTXTdeclc Psc psc)
* {
*   CPtr ep;
* 
*   ep = (CPtr) get_ep(psc);
*   switch (*(pb)ep) {
*   case tabletry:
*   case tabletrysingle:
*     abolish_table_predicate(CTXTc psc);
*     break;
*   case test_heap:
*     if (*(pb)(ep+2) == tabletry || *(pb)(ep+2) == tabletrysingle)
*       abolish_table_predicate(CTXTc psc);
*     break;
*   case switchon3bound:
*   case switchonbound:
*   case switchonterm:
*     if (*(pb)(ep+3) == tabletry || *(pb)(ep+3) == tabletrysingle)
*       abolish_table_predicate(CTXTc psc);
*     break;
*   }
* }
*/

//----------------------------------------------------------------------
// Code from here to end of file is under development -- TLS

// Code for marking ASI scratchpad

#ifdef BITS64
#define VISITED_MASK 0xf000000000000000
#else
#define VISITED_MASK 0xf0000000
#endif

#ifdef BITS64
#define STACK_MASK 0xffffffffffffff
#else
#define STACK_MASK 0xffffff
#endif

#define VISITED_ANSWER(as_leaf)  (asi_scratchpad((ASI) Child(as_leaf)) & VISITED_MASK)
#define STACK_INDEX(as_leaf)  (asi_scratchpad((ASI) Child(as_leaf)) & STACK_MASK)
#define MARK_VISITED_ANSWER(as_leaf) {asi_scratchpad((ASI) Child(as_leaf)) = asi_scratchpad((ASI) Child(as_leaf)) | VISITED_MASK;}
#define MARK_STACK_INDEX(as_leaf,index) {			\
    asi_scratchpad((ASI) Child(as_leaf)) = ( VISITED_MASK | index );}

#define POP_MARK_STACK_INDEX(as_leaf,index) {			\
    asi_scratchpad((ASI) Child(as_leaf)) = ( POPPED_MASK | VISITED_MASK | index );}

static int dfn = 0;

//----------------------------------------------------------------------
//  Component stack is used for SCC detection and DFS

struct answer_dfn {
  BTNptr  answer;
  int     dfn;
  int     min_link;
} ;
typedef struct answer_dfn *answerDFN;

#define component_stack_increment 1000

int component_stack_top = 0;
answerDFN component_stack = NULL;
int component_stack_size = 0;

#ifdef BITS64
#define POPPED_MASK 0x0f00000000000000
#else
#define POPPED_MASK 0x0f000000
#endif

#define POPPED_ANSWER(as_leaf)  (asi_scratchpad((ASI) Child(as_leaf)) & POPPED_MASK)
#define	MARK_POPPED(answer_idx) ( \
(asi_scratchpad((ASI) Child(component_stack[answer_idx].answer))) \
= (asi_scratchpad((ASI) Child(component_stack[answer_idx].answer)) | POPPED_MASK))


#define push_comp_node(as_leaf,index) {				\
  if (component_stack_top >= component_stack_size) {\
    unsigned long old_component_stack_size = component_stack_size; \
    component_stack_size = component_stack_size + component_stack_increment;\
    component_stack = (answerDFN)mem_realloc(component_stack,		\
					     old_component_stack_size*sizeof(struct answer_dfn), \
					     component_stack_size*sizeof(struct answer_dfn), \
					     TABLE_SPACE);		\
  }\
  component_stack[component_stack_top].dfn = dfn;		\
  component_stack[component_stack_top].min_link = dfn++;	\
  component_stack[component_stack_top].answer = as_leaf;	\
  index = component_stack_top;					\
  MARK_STACK_INDEX(as_leaf,index);				\
  component_stack_top++;					\
  }

/* for use when you don't need the index returned */
#define push_comp_node_1(as_leaf) {			\
    int index;						\
    MARK_VISITED_ANSWER(as_leaf);				\
    push_comp_node(as_leaf,index);			\
  }

#define pop_comp_node(node) {				\
    component_stack_top--;				\
    node = component_stack[component_stack_top];	\
  }

#define copy_pop_comp_node(node) {				\
    push_done_node((component_stack_top-1),			\
		   (component_stack[component_stack_top-1].dfn));	\
    component_stack_top--;					\
    node = component_stack[component_stack_top].answer;		\
  }

void print_comp_stack(CTXTdecl) {
  int frame = 0;
  while (frame < component_stack_top) {
    printf("comp_frame %d answer %p dfn %d min_link %d  ",frame,component_stack[frame].answer,
	   component_stack[frame].dfn,component_stack[frame].min_link);
    print_subgoal(CTXTc stddbg, asi_subgoal((ASI) Child(component_stack[frame].answer)));
    printf("\n");
    frame++;
  }
}

#define deallocate_comp_stack	{					\
  mem_dealloc(component_stack,component_stack_size*sizeof(struct answer_dfn), \
		TABLE_SPACE);						\
  component_stack_size = 0;						\
  component_stack = 0;							\
  }

//----------------------------------------------------------------------

struct done_answer_dfn {
  BTNptr  answer;
  int     scc;
  int     checked;
} ;

typedef struct done_answer_dfn *doneAnswerDFN;

int done_answer_stack_top = 0;
doneAnswerDFN done_answer_stack = NULL;
int done_answer_stack_size = 0;
#define done_answer_stack_increment 1000

#define push_done_node(index,dfn_num) {					\
    if (done_answer_stack_top >= done_answer_stack_size) {				\
      unsigned long old_done_answer_stack_size = done_answer_stack_size;		\
      done_answer_stack_size = done_answer_stack_size + done_answer_stack_increment; \
      done_answer_stack = (doneAnswerDFN) mem_realloc(done_answer_stack,		\
					       old_done_answer_stack_size*sizeof(struct done_answer_dfn), \
					  done_answer_stack_size*sizeof(struct done_answer_dfn), \
					  TABLE_SPACE);			\
    }									\
    done_answer_stack[done_answer_stack_top].scc = dfn_num;				\
    done_answer_stack[done_answer_stack_top].checked = 0;				\
    done_answer_stack[done_answer_stack_top].answer = component_stack[index].answer;	\
    POP_MARK_STACK_INDEX(component_stack[index].answer,done_answer_stack_top);	\
    done_answer_stack_top++;							\
  }

void print_done_answer_stack(CTXTdecl) {
  int frame = 0;
  while (frame < done_answer_stack_top) {
    if (done_answer_stack[frame].answer) {
      printf("done_frame %d answer %p scc %d checked %d ",frame, 
	     done_answer_stack[frame].answer,done_answer_stack[frame].scc,done_answer_stack[frame].checked);
      print_subgoal(CTXTc stddbg, asi_subgoal((ASI) Child(done_answer_stack[frame].answer)));
      printf("\n");
      frame++;
    }
  }
}

// reset the scratchpad for each answer in done stack
void reset_done_answer_stack() {
  int frame = 0;
  while (frame < done_answer_stack_top) {
    asi_scratchpad((ASI) Child(done_answer_stack[frame].answer)) = 0;
    frame++;
  }
}

//----------------------------------------------------------------------
/* Returns -1 when no answer found (not 0, as 0 can be an index */
int visited_answer(BTNptr as_leaf) {		
  if (!VISITED_ANSWER(as_leaf)) return -1;
  else return STACK_INDEX(as_leaf);
}

/* Negative DEs dont have answer pointer -- so need to obtain it from subgoal */
BTNptr traverse_subgoal(VariantSF pSF) {
  BTNptr cur_node = 0;

  if ( subg_answers(pSF) == COND_ANSWERS && IsNonNULL(subg_ans_root_ptr(pSF))) {
    cur_node = subg_ans_root_ptr(pSF);
    while (!IsLeafNode(cur_node)) {
      cur_node = BTN_Child(cur_node);
    } 
    return cur_node;
  }
  return 0;
}

#define  update_minlink_minlink(from_answer,to_answer) { \
    if (component_stack[to_answer].min_link < component_stack[from_answer].min_link)	 \
      component_stack[from_answer].min_link = component_stack[to_answer].min_link;	 \
  }							 \

#define  update_minlink_dfn(from_answer,to_answer) { \
    if (component_stack[to_answer].dfn < component_stack[from_answer].min_link) { \
      printf(" to_answer dfn %d from answer minlink %d\n",component_stack[to_answer].dfn,component_stack[from_answer].min_link); \
      component_stack[from_answer].min_link = component_stack[to_answer].dfn;	} \
  }							 \

int table_component_check(CTXTdeclc NODEptr from_answer) {
  DL delayList;
  DE delayElement;
  BTNptr to_answer;
  int to_answer_idx, from_answer_idx, component_num;

  //  if (is_conditional_answer(from_answer)) {
    push_comp_node(from_answer,from_answer_idx);
    //    printf("starting: %d %d ; ",VISITED_ANSWER(from_answer),STACK_INDEX(from_answer)); 
    printf("tcc: ");print_subgoal(CTXTc stddbg, asi_subgoal((ASI) Child(from_answer)));printf("\n"); 
    printf("*");    print_comp_stack(CTXT); printf("\n");
      
    //    print_comp_stack(CTXT);
    delayList = asi_dl_list((ASI) Child(from_answer));
    while (delayList) {
      delayElement = dl_de_list(delayList);
      while (delayElement) {
	if (de_ans_subst(delayElement)) to_answer = de_ans_subst(delayElement);
	else to_answer = traverse_subgoal(de_subgoal(delayElement));
	if  (0 >  (to_answer_idx = visited_answer(to_answer))) {
	  printf("doing table component check  ");
	  printf("*");    print_comp_stack(CTXT); printf("\n");
	  to_answer_idx = table_component_check(CTXTc to_answer);
	  update_minlink_minlink(from_answer_idx,to_answer_idx);
	} else {
	  if (!POPPED_ANSWER(to_answer)) {
	    printf("updating from dfn to_answer %p %x\n",to_answer,asi_scratchpad((ASI) Child(to_answer)));
	    update_minlink_dfn(from_answer_idx,to_answer_idx);
	  }
	}
	delayElement = de_next(delayElement);
      }
      delayList = de_next(delayList);
    }
      printf("checking: "); 
      print_subgoal(CTXTc stddbg, asi_subgoal((ASI) Child(from_answer)));printf("\n");

    if (component_stack[from_answer_idx].dfn 
	== component_stack[from_answer_idx].min_link) {
      component_num = component_stack[from_answer_idx].dfn;
      while (from_answer_idx < component_stack_top) {
	MARK_POPPED(from_answer_idx);
	push_done_node(component_stack_top-1,component_num);
	component_stack_top--;
      }
    }
    print_comp_stack(CTXT); printf("\n");
    return from_answer_idx;
    // }
}

void unfounded_component(CTXTdecl) {
  int founded = 0;
  int index;
  int starting_index = 0;
  int starting_scc = done_answer_stack[starting_index].scc;
  DL delayList;
  DE delayElement;
  BTNptr cur_answer = 0;  // TLS: compiler (rightly) complained about it being uninit.

  while (starting_index < done_answer_stack_top) {
    founded = 0; 
    index = starting_index;
    starting_scc = done_answer_stack[index].scc;
    while (!founded && done_answer_stack[index].scc == starting_scc 
	   && index < done_answer_stack_top) {
      cur_answer = done_answer_stack[index].answer;
      delayList = asi_dl_list((ASI) Child(cur_answer));
      print_subgoal(CTXTc stddbg, asi_subgoal((ASI) Child(cur_answer)));printf("\n");
      while (!founded && delayList) {
	delayElement = dl_de_list(delayList);
	while (!founded && delayElement) {
	  if (!de_ans_subst(delayElement)) 
	    founded = 1;
	  else {
	    if (done_answer_stack[STACK_INDEX(de_ans_subst(delayElement))].checked == 1)
	      founded = 1;
	  }
	  delayElement = de_next(delayElement);
	}
	delayList = de_next(delayList);
      }
      index++;
    }
    if (!founded) {
      for ( ; done_answer_stack[starting_index].scc == starting_scc && starting_index < done_answer_stack_top ; starting_index++) {

      printf("SCC %d is unfounded!\n",starting_scc);
      // start simplification operations

      /* This function is not used much, and does not handle call subsumption */ 
      //    SYS_MUTEX_LOCK( MUTEX_DELAY ) ;
      //      subgoal = asi_subgoal(Delay(cur_answer));
      release_all_dls(CTXTc Delay(cur_answer));
      //    SET_TRIE_ALLOCATION_TYPE_SF(subgoal);
      handle_unsupported_answer_subst(CTXTc cur_answer);
      //    SYS_MUTEX_UNLOCK( MUTEX_DELAY ) ;

      done_answer_stack[starting_index].answer = 0;
      } 
    }
    else {
      printf("SCC %d is founded\n",starting_scc);
      for ( ; done_answer_stack[starting_index].scc == starting_scc && starting_index < done_answer_stack_top ; starting_index++) 
	done_answer_stack[starting_index].checked = 1;
    }
  }
}

#ifdef UNDEFINED

#include "system_xsb.h"

xsbBool checkSupportedAnswer(BTNptr answer_leaf) {
  int index, answer_supported, delaylist_supported;
  DL DelayList;
  DE DelayElement;  

  if (VISITED_ANSWER(answer_leaf)) {
    if (FALSE) /* answer_leaf is in completion stack */
      return FALSE;
    else return TRUE;
  }
  else { /* not visited */
    MARK_VISITED_ANSWER(answer_leaf);
    push_comp_node(answer_leaf,index);
    DelayList = asi_dl_list(Delay(answer_leaf));
    answer_supported = UNKNOWN;
    while (DelayList && answer_supported != TRUE) {
      DelayElement = dl_de_list(DelayList);
      delaylist_supported = TRUE; /* UNKNOWN? */
      while (DelayElement && delaylist_supported != FALSE) {
	if (de_positive(DelayElement) /* && Is in SCC */) {
	  if (!checkSupportedAnswer((BTNptr) de_ans_subst(DelayElement)))
	    delaylist_supported = FALSE;
	  DelayElement = de_next(DelayElement);
	}
        if (delaylist_supported == FALSE) { /* does this propagate */
	  if (!remove_dl_from_dl_list(CTXTc DelayList, Delay(answer_leaf)))
	    simplify_pos_unsupported(CTXTc (NODEptr) answer_leaf);
	}
	else answer_supported = TRUE;
      }
      DelayList = dl_next(DelayList);
    }
    if (IsValidNode(answer_leaf)) return TRUE;
    else return FALSE;
  }
}


void resetStack() {
}

void answer_completion(CTXTdeclc CPtr cs_ptr) {
  VariantSF compl_subg;
  CPtr ComplStkFrame = cs_ptr; 
  ALNptr answerPtr;
  BTNptr answer_leaf;

  printf("calling answer completion\n");

  /* For each subgoal S in SCC */
  while (ComplStkFrame >= openreg) {
    compl_subg = compl_subgoal_ptr(ComplStkFrame);
    answerPtr = subg_ans_list_ptr(compl_subg);
    while (answerPtr ) {
      answer_leaf = ALN_Answer(answerPtr);
      checkSupportedAnswer(answer_leaf);
      resetStack();
      answerPtr = ALN_Next(answerPtr);
    }

    ComplStkFrame = next_compl_frame(ComplStkFrame);
  }

}

#endif
//----------------------------------------------------------------------
int table_inspection_function( CTXTdecl ) {
  switch (ptoc_int(CTXTc 1)) {

  case FIND_COMPONENTS: {
    dfn = 0;
    component_stack_top = 0;
    done_answer_stack_top = 0;
    table_component_check(CTXTc (NODEptr) ptoc_int(CTXTc 2));
    print_done_answer_stack(CTXT);
    unfounded_component(CTXT);
    printf("done w. unfounded component\n");
    //    print_done_answer_stack(CTXT);
    break;
  }

  case FIND_FORWARD_DEPENDENCIES: {
  DL delayList;
  DE delayElement;
  BTNptr as_leaf, new_answer;
  struct answer_dfn stack_node;

  done_answer_stack_top = 0; dfn = 0;
  as_leaf = (NODEptr) ptoc_int(CTXTc 2);
  if (is_conditional_answer(as_leaf)) {
    push_comp_node_1(as_leaf);
    while (component_stack_top != 0) {
      //      print_comp_stack(CTXT);
      pop_comp_node(stack_node);
      as_leaf = stack_node.answer;
      push_done_node((component_stack_top),component_stack[component_stack_top].dfn);
      // print_subgoal(CTXTc stddbg, asi_subgoal((ASI) Child(as_leaf)));printf("\n");
      delayList = asi_dl_list((ASI) Child(as_leaf));
      while (delayList) {
	delayElement = dl_de_list(delayList);
	while (delayElement) {
	  if (de_ans_subst(delayElement)) {
	    if (!VISITED_ANSWER(de_ans_subst(delayElement)))
	      push_comp_node_1(de_ans_subst(delayElement));
	  } else {
	    new_answer = traverse_subgoal(de_subgoal(delayElement));
	    if (!VISITED_ANSWER(new_answer)) 
	      push_comp_node_1(new_answer);
	    }
	    delayElement = de_next(delayElement);
	  }
	  delayList = de_next(delayList);
	}
      }
    printf("component stack %p\n",component_stack);
    deallocate_comp_stack;
    print_done_answer_stack(CTXT);
    // Don't deallocte done stack until done with its information.
    reset_done_answer_stack();
    return 0;
  }
  else  printf("found unconditional answer %p\n",as_leaf);
    break;
  }
  
  case FIND_ANSWERS: {
    return find_pred_backward_dependencies(CTXTc get_tip(CTXTc term_psc(ptoc_tag(CTXTc 2))));

  }

/********************************************************************

The following builtin serves as an analog to SLG_NOT for call
subsumption when we the negated subgoal Subgoal is not a producer.
Because there is no direct connection between a consumer for Subgoal
(which is ground) and its answer, we either start here with the answer
leaf pointer as produced by get_returns/3, or with a null leaf pointer
(e.g. in case the function is invoked by delaying after
is_incomplete).  Obtaining the leaf pointer should probably be moved
into this function, but for now we start with an indication of whether
a return has been found, and handle our cases separately.  

A separate case is that we may not have a consumer subgoal frame if
Subgoal is subsumed by Producer and Producer is completed (the call
subsumption algorithm does not create a subgoal frame in this case).
So we have to handle that situation also.  For now, if we don't have a
consumer SF, I add tnot(Producer) to the delay list -- although
creating a new consumer SF so that we can add tnot(Consumer) might be
better.
*********************************************************************/

case CALL_SUBS_SLG_NOT: {

  VariantSF producerSF, consumerSF;
  BTNptr answerLeaf;
  int hasReturn;

  producerSF = (VariantSF) ptoc_int(CTXTc 2);
  hasReturn =  ptoc_int(CTXTc 4);
  consumerSF = (VariantSF) ptoc_int(CTXTc 5);

  if (hasReturn) {
    answerLeaf = (BTNptr) ptoc_int(CTXTc 3);
    /* Return with unconditional answer: fail */
    if ( is_unconditional_answer(answerLeaf) ) {
      //      fprintf(stddbg,"failing : ");print_subgoal(stddbg,consumerSF),printf("\n");
      return FALSE;
    }
    else {
    /* Return with conditional answer: propagate delay */
      //      fprintf(stddbg,"delaying (w) : ");print_subgoal(stddbg,consumerSF),printf("\n");
      if (IsNonNULL(consumerSF)) delay_negatively(consumerSF)
      else delay_negatively(producerSF);
      return TRUE;
    }
  } else { /* has no return: we havent resumed to delay */
    if   (is_completed(producerSF) || neg_delay == FALSE) {
      //      fprintf(stddbg,"succeeding: ");print_subgoal(stddbg,consumerSF),printf("\n");
      return TRUE;
    }
    else { /* has no return: but function was invoked for delaying */
      //     fprintf(stddbg,"delaying (wo): ");print_subgoal(stddbg,consumerSF),printf("\n");
      if (IsNonNULL(consumerSF)) delay_negatively(consumerSF)
	else delay_negatively(producerSF);
      return TRUE;
    }
  }
}

  }
  return TRUE;
}

