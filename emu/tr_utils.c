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
#include "rw_lock.h"
#include "debug_xsb.h"
#include "thread_xsb.h"
#include "storage_xsb.h"
#include "hash_xsb.h"
#include "tables.h"
#include "builtin.h"

#include "call_graph_xsb.h" /* incremental evaluation */
#include "table_inspection_defs.h"

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
    err_handle(CTXTc TYPE, 1, "get_call", 3, "callable term", callTerm);
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
  SM_DeallocateStruct(*smBTHT,ht); 
#ifdef MULTI_THREAD
  if( threads_current_sm == SHARED_SM )
	SM_Unlock(*smBTHT);
#endif
}

/* -------------------------------------------------------------- 
 * Space reclamation for Conditional Answers 
 * All routines assume that trie allocation type has been set (cf. struct_manager.h)
 * ------------------------------------------------------------ */

#ifdef MULTI_THREAD
#define GLOBAL_TABLE    (threads_current_sm == PRIVATE_SM)		
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
void delete_variant_sf_and_answers(CTXTdeclc VariantSF pSF) {
  int node_stk_top = 0;
  BTNptr rnod, *Bkp; 
  BTHTptr ht;
  xsbBool should_warn = TRUE;
  
  BTNptr *freeing_stack = NULL;
  int freeing_stack_size = 0;

  TRIE_W_LOCK();

  /* TLS: this checks whether any answer for this subgoal has a delay
     list: may overstate problems but will warn for any possible
     corruption. */
#ifndef CONC_COMPL
  if ( subg_answers(pSF) == COND_ANSWERS && should_warn) {
#else
    if ( subg_tag(pSF) == COND_ANSWERS && should_warn) {
#endif
      xsb_warn("abolish_table_call/1 is deleting a table entry for %s/%d with conditional\
                      answers: delay dependencies may be corrupted.\n",	    
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
}

/* Incremental recomputation seems to be implemented only for
   abolishing predicates, but not subgoals */

extern void hashtable1_destroy(void *, int);

static void delete_variant_table(CTXTdeclc BTNptr x, int incr) {

  int node_stk_top = 0, call_nodes_top = 0;
  BTNptr node, rnod, *Bkp; 
  BTHTptr ht;
  xsbBool should_warn = TRUE;

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
                      answers: delay dependencies may be corrupted.\n",	    
		     get_name(TIF_PSC(subg_tif_ptr(pSF))),get_arity(TIF_PSC(subg_tif_ptr(pSF))));
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

void delete_predicate_table(CTXTdeclc TIFptr tif) {
  /*  printf("smBTN %x smTableBTN %x private_smTableBTN %x\n",
      smBTN, &smTableBTN,private_smTableBTN);
      printf("smBTHT %x smTableBTHT %x private_smTableBTHT %x\n",
      smBTHT, &smTableBTHT,private_smTableBTHT);*/
  
  if ( TIF_CallTrie(tif) != NULL ) {
    SET_TRIE_ALLOCATION_TYPE_TIP(tif);
    if ( IsVariantPredicate(tif) ) {
      delete_variant_table(CTXTc TIF_CallTrie(tif),get_incr(TIF_PSC(tif)));
    }
    else
      delete_subsumptive_table(CTXTc tif);
    TIF_CallTrie(tif) = NULL;
    TIF_Subgoals(tif) = NULL;
  }
}

/* - - - - - */

void reclaim_deleted_subsumptive_table(CTXTdeclc DelTFptr);

/* Just like delete_predicate_table, but called from gc sweeps with deltf_ptr.
   In addition, does not reset TIFs.*/
void reclaim_deleted_predicate_table(CTXTdeclc DelTFptr deltf_ptr) {
  TIFptr tif = subg_tif_ptr(DTF_Subgoals(deltf_ptr));

  /*  printf("smBTN %x smTableBTN %x private_smTableBTN %x\n",
      smBTN, &smTableBTN,private_smTableBTN);
      printf("smBTHT %x smTableBTHT %x private_smTableBTHT %x\n",
      smBTHT, &smTableBTHT,private_smTableBTHT);*/

  if ( IsVariantPredicate(tif) ) {
    delete_variant_table(CTXTc DTF_CallTrie(deltf_ptr), get_incr(TIF_PSC(tif)));
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
 */
void delete_branch(CTXTdeclc BTNptr lowest_node_in_branch, BTNptr *hook) {

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
	free_trie_ht(CTXTc (BTHTptr)(*y1));
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

/*------------------------------*/

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
/* delete_trie() and supporting code.  
 * 
 * Code to support deletion of asserted or interned tries.
 * delete_trie() is used by gen_retractall (i.e. abolish or retractall
 * with an open atomic formula) to delete an entire asserted trie.
 * Its also called via the builtin DELETE_TRIE --
 * delete_interned_trie() to delete an interned trie or storage trie */
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
      SM_DeallocateSharedStruct(*smBTN,root);
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
	printf("null node");
      break;
    }
  }
  mem_dealloc(delete_trie_op,trie_op_size,TABLE_SPACE); delete_trie_op = NULL;
  mem_dealloc(delete_trie_node,trie_node_size,TABLE_SPACE); delete_trie_node = NULL;
  mem_dealloc(delete_trie_hh,trie_hh_size,TABLE_SPACE); delete_trie_hh = NULL;
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
void delete_return(CTXTdeclc BTNptr l, VariantSF sg_frame) 
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
    delete_branch(CTXTc l,&subg_ans_root_ptr(sg_frame));
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
**   Used in aggregs.P to implement aggregates.
**   Takes:   breg (the place where choice point is saved) and arity.  
**   Returns: subgoal skeleton (i.e., ret(X,Y,Z), where X,Y,Z are all the 
**    	      	                distinct variables in the subgoal);
**   	      Pointer to the subgoal.
*/

void breg_retskel(CTXTdecl)
{
    Pair    sym;
    Cell    term;
    VariantSF sg_frame;
    CPtr    tcp, cptr, where;
    int     is_new, i;
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
      sym = insert("ret", (byte)Nvars, (Psc)flags[CURRENT_MODULE], &is_new);
      new_heap_functor(hreg, sym->psc_ptr);
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

#define ADJUST_SIZE 100

#ifndef MULTI_THREAD
BTNptr *Set_ArrayPtr;
/*
 * first_free_set is the index of the first deleted set.  The deleted
 * tries are deleted in builtin DELETE_TRIE, and the corresponding
 * elements in Set_ArrayPtr are linked to form a list.  So
 * Set_ArrayPtr[first_free_set] contains the index of the next deleted
 * set, ..., the last one contains 0.  If first_free_set == 0, that
 * means no free set available.
 */
Integer first_free_set;
int Set_ArraySz;
/*
 * num_sets is the number of sets have been used (including the fixed
 * trie, Set_ArrayPtr[0] (see trie_intern/3)).  It is also the index for
 * the next element to use when no free element is available.
 */
int num_sets;
#endif

/*----------------------------------------------------------------------*/

/* Allocate an array of handles to interned tries, and initialize
   global variables. */

void init_newtrie(CTXTdecl)
{
  first_free_set = 0;
  Set_ArraySz = 10;  /* must be at least num_sets */
  num_sets = 1;
  Set_ArrayPtr = (BTNptr *) mem_calloc(Set_ArraySz,sizeof(BTNptr),TABLE_SPACE);

  bt_storage_hash_table.length = STORAGE_TBL_SIZE;
  bt_storage_hash_table.bucket_size = sizeof(STORAGE_HANDLE);
  bt_storage_hash_table.initted = FALSE;
  bt_storage_hash_table.table = NULL;
}

/*----------------------------------------------------------------------*/

/* Returns a handle to an unused interned trie. */

Integer newtrie(CTXTdecl)
{
  Integer i;
  Integer result;
  
  if (first_free_set != 0) {	/* a free set is available */
    i = first_free_set;		/* save it in i */
    result = first_free_set;
    first_free_set = (Integer) Set_ArrayPtr[first_free_set] >> 2;
    Set_ArrayPtr[i] = NULL;	/* must be reset to NULL */
  }
  else {
    if (num_sets == Set_ArraySz) { /* run out of elements */
      BTNptr *temp_arrayptr;
      unsigned long temp_arraysz;

      temp_arrayptr = Set_ArrayPtr;
      temp_arraysz = Set_ArraySz;
      Set_ArraySz += ADJUST_SIZE;  /* adjust the array size */
      Set_ArrayPtr = (BTNptr *) mem_calloc(Set_ArraySz ,sizeof(BTNptr),TABLE_SPACE);
      if (Set_ArrayPtr == NULL)
	xsb_exit(CTXTc "Out of memory in new_trie/1");
      for (i = 0; i < num_sets; i++)
	Set_ArrayPtr[i] = temp_arrayptr[i];
      mem_dealloc(temp_arrayptr,temp_arraysz,TABLE_SPACE);
    }
    result = (Integer)num_sets;
    num_sets++;
  }
  return result;
}

/*----------------------------------------------------------------------*/
/* i_trie_intern(_Term,_Root,_Leaf,_Flag,_Check_CPS,_Expand_or_not)     
* 
* If called from trie_intern(), we'll need to check to see whether its
* safe to expand -- hence check_cps_flag; if called from
* bulk_trie_intern() we don't need to check, and expand_flag will be
* set to tell us whether we can expand or not.
*/

void trie_intern(CTXTdecl)
{
  prolog_term term;
  int RootIndex;
  int flag, check_cps_flag, expand_flag;
  BTNptr Leaf;

  term = ptoc_tag(CTXTc 1);
  RootIndex = ptoc_int(CTXTc 2);
  check_cps_flag = ptoc_int(CTXTc 5);
  expand_flag = ptoc_int(CTXTc 6);
  xsb_dbgmsg((LOG_INTERN, "Interning "));
  dbg_printterm(LOG_INTERN,stddbg,term,25);
  xsb_dbgmsg((LOG_INTERN, "In trie with root %d", RootIndex));

  switch_to_trie_assert;
  SYS_MUTEX_LOCK(MUTEX_TRIE);
  Leaf = whole_term_chk_ins(CTXTc term,&(Set_ArrayPtr[RootIndex]),&flag,check_cps_flag,expand_flag);
  SYS_MUTEX_UNLOCK(MUTEX_TRIE);
  switch_from_trie_assert;
  
  ctop_int(CTXTc 3,(Integer)Leaf);
  ctop_int(CTXTc 4,flag);
  xsb_dbgmsg((LOG_INTERN, "Exit flag %d",flag));
}

/*----------------------------------------------------------------------*/

int trie_interned(CTXTdecl)
{
  int RootIndex;
  int ret_val = FALSE;
  Cell Leafterm, trie_term;
#ifdef MULTI_THREAD_RWL
   CPtr tbreg;
#ifdef SLG_GC
   CPtr old_cptop;
#endif
#endif

  trie_term =  ptoc_tag(CTXTc 1);
  RootIndex = ptoc_int(CTXTc 2);
  Leafterm = ptoc_tag(CTXTc 3);
  
  /*
   * Only if Set_ArrayPtr[RootIndex] is a valid BTNptr can we run this
   * builtin.  That means Set_ArrayPtr[RootIndex] can neither be NULL,
   * nor a deleted set (deleted by builtin delete_trie/1).
   */
  if ((Set_ArrayPtr[RootIndex] != NULL) &&
      (!((long) Set_ArrayPtr[RootIndex] & 0x3))) {
    XSB_Deref(trie_term);
    XSB_Deref(Leafterm);
    if ( isref(Leafterm) ) {  
      reg_arrayptr = reg_array -1;
      num_vars_in_var_regs = -1;
      pushreg(trie_term);
#ifdef MULTI_THREAD_RWL
/* save choice point for trie_unlock instruction */
       save_find_locx(ereg);
       tbreg = top_of_cpstack;
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
      pcreg = (byte *)Set_ArrayPtr[RootIndex];
      ret_val =  TRUE;
    }
    else{
      xsb_instantiation_error(CTXTc "trie_interned",4,3,"non-attributed and non-ground");
    }
  }
  return(ret_val);
}

/*----------------------------------------------------------------------*/

/*
 * This is builtin #162: TRIE_DISPOSE(+ROOT, +LEAF), to dispose a branch
 * of the trie rooted at Set_ArrayPtr[ROOT].
 * 
 * If called within a trie_retractall(), the CPS check will already
 * have been done: if it isn't safe to reclaim trie_dispose_nr() would
 * have been called; otherwise its safe and we don't need to check
 * here.  If called from within a trie_unintern() (maybe we should get
 * the names straight) we do need to check.
 */

void trie_dispose(CTXTdecl)
{
  BTNptr Leaf;
  long Rootidx;
  int disposalType;

  Rootidx = ptoc_int(CTXTc 1);
  Leaf = (BTNptr)ptoc_int(CTXTc 2);
  disposalType = ptoc_int(CTXTc 3);
  SYS_MUTEX_LOCK(MUTEX_TRIE);
  switch_to_trie_assert;
  SYS_MUTEX_UNLOCK(MUTEX_TRIE);
  if (disposalType == NO_CPS_CHECK)     delete_branch(CTXTc Leaf, &(Set_ArrayPtr[Rootidx]));
  else {
    if (!interned_trie_cps_check(CTXTc Set_ArrayPtr[Rootidx])) {
      //          printf(" really deleting branch \n");
      delete_branch(CTXTc Leaf, &(Set_ArrayPtr[Rootidx]));
    }
    else {
      //          printf(" safely deleting branch \n");
      safe_delete_branch(Set_ArrayPtr[Rootidx]);
    }
  }
  switch_from_trie_assert;
}

/*----------------------------------------------------------------------*/
/* 
 * For interned tries, I'm checking whether there is a trie choice
 * point with the same root as the the trie that we want to do
 * something with.  
 */

#define is_trie_instruction(cp_inst) \
 ((int) cp_inst >= 0x5c && (int) cp_inst < 0x80) \
	   || ((int) cp_inst >= 0x90 && (int) cp_inst < 0x94) 

int interned_trie_cps_check(CTXTdeclc BTNptr root) 
{
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  int found_interned;
  BTNptr pLeaf;

  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  found_interned = 0;
  while ( cp_top1 < cp_bot1 && !(found_interned)) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      // Below we want basic_answer_trie_tt, ts_answer_trie_tt
      if (IsInInternTrie(((BTNptr) *cp_top1))) {
	//	printf(" found interned trie instruction\n");
	pLeaf = (BTNptr) *cp_top1;
	while ( IsNonNULL(pLeaf) && (! IsTrieRoot(pLeaf)) && 
		((int) TN_Instr(pLeaf) != trie_fail_unlock) ) {
	  pLeaf = BTN_Parent(pLeaf);
	}
	if (pLeaf == root) {
	  //	  printf(" found root!\n");
	  found_interned = 1;
	}
      }
    }
    cp_top1 = cp_prevtop(cp_top1);
  }
  return found_interned;
}


/*----------------------------------------------------------------------*/
/* 
 * The check of whether this trie is safe to delete has already been done.
 */

#define DELETED_SET 1

void delete_interned_trie(CTXTdeclc Integer tmpval) {
  /*
   * We can only delete a valid BTNptr, so that only those sets
   * that were used before can be put into the free set list.
   */
  if ((Set_ArrayPtr[tmpval] != NULL) &&
      (!((Integer) Set_ArrayPtr[tmpval] & 0x3))) {
    switch_to_trie_assert;
    SYS_MUTEX_LOCK(MUTEX_TRIE);
    delete_trie(CTXTc Set_ArrayPtr[tmpval]);
    SYS_MUTEX_UNLOCK(MUTEX_TRIE);
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
      SYS_MUTEX_LOCK(MUTEX_TRIE);
      delete_branch(CTXTc leaf, &(Set_ArrayPtr[rootidx]));
      SYS_MUTEX_UNLOCK(MUTEX_TRIE);
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
 */
void trie_dispose_nr(CTXTdecl)
{
  BTNptr Leaf;
  long Rootidx;

  Rootidx = ptoc_int(CTXTc 1);
  Leaf = (BTNptr)ptoc_int(CTXTc 2);
  switch_to_trie_assert;
  SYS_MUTEX_LOCK(MUTEX_TRIE);
  insertLeaf(getIGRnode(CTXTc Rootidx), Leaf);
  SYS_MUTEX_UNLOCK(MUTEX_TRIE);
  safe_delete_branch(Leaf);
  switch_from_trie_assert;
}

/*----------------------------------------------------------------------*/
/*
 * This is builtin : TRIE_UNDISPOSE_NR(+ROOT, +LEAF), to
 * unmark a safely deleted branch.
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
 * When a table is abolished, various checks must be made before its
 * space can be reclaimed.  First, the table must be completed, and
 * second it must be ensured that there are not any trie choice points
 * for the table in the choice point stack.  Third, if the table is
 * shared, a check must be made that there is a single active thread.
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
 * create a DelTF for them, even private tables will not be gc'd until
 * we're down to a single thread, so its best to call the abolishes
 * when we dont have any more backtracking points.
 *
 * Later, on a call to gc_tables/1 (which works only if there is a
 * single active thread), the choice point stacks may be traversed to
 * mark those DelTF frames corresponding to tables with trie CPs in
 * the CP stack.  Once this is done, the chain of DelTF frames is
 * traversed to reclaim tables for those unmarked DelTF frames (and
 * free the frames) as well as to unmark the marked DelTF frames.
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

/* If there is a deltf with same subgoals and arity (can this be) dont
   add; otherwise if there is a subgoal for this pred, delete the
   deltf (it must be for this generation of the table)
*/
void check_insert_global_deltf_pred(CTXTdeclc TIFptr tif) { 
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
    New_Global_DelTF_Pred(dtf,tif);
  }
  TIF_CallTrie(tif) = NULL;
  TIF_Subgoals(tif) = NULL;
  SYS_MUTEX_UNLOCK(MUTEX_TABLE);
}

/* Dont think I need to check for deleted subgoals. */
void check_insert_global_deltf_subgoal(CTXTdeclc VariantSF subgoal) {
  DelTFptr dtf;
  TIFptr tif;

  SYS_MUTEX_LOCK(MUTEX_TABLE);

  tif = subg_tif_ptr(subgoal);

  New_Global_DelTF_Subgoal(dtf,tif,subgoal);

  if (subg_prev_subgoal(subgoal) != 0) 
    subg_prev_subgoal(subgoal) = subg_next_subgoal(subgoal);

  if (subg_next_subgoal(subgoal) != 0) 
    subg_next_subgoal(subgoal) = subg_prev_subgoal(subgoal);

  subg_deltf_ptr(subgoal) = dtf;

  SYS_MUTEX_UNLOCK(MUTEX_TABLE);
}

#ifdef MULTI_THREAD

// extern void printTIF(TIFptr);

void check_insert_private_deltf_pred(CTXTdeclc TIFptr tif) {
  DelTFptr dtf = TIF_DelTF(tif), next_dtf;
  BTNptr call_trie = TIF_CallTrie(tif);
  VariantSF subgoals = TIF_Subgoals(tif);	
  int found = 0;

  //  printf("\n.........starting cipdp\n");
  //  printTIF(tif);

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
    New_Private_DelTF_Pred(dtf,tif);
  }
  TIF_CallTrie(tif) = NULL;
  TIF_Subgoals(tif) = NULL;
  //  printf(".........ending\n");
  //  printTIF(tif);
}

#define check_insert_shared_deltf_pred(context, tif)	\
  check_insert_global_deltf_pred(context, tif)	 

/* * * * * * * */

void check_insert_private_deltf_subgoal(CTXTdeclc VariantSF subgoal)
{
  DelTFptr dtf;
  TIFptr tif = subg_tif_ptr(subgoal);

  New_Private_DelTF_Subgoal(dtf,tif,subgoal);

  if (subg_prev_subgoal(subgoal) != 0) 
    subg_prev_subgoal(subgoal) = subg_next_subgoal(subgoal);

  if (subg_next_subgoal(subgoal) != 0) 
    subg_next_subgoal(subgoal) = subg_prev_subgoal(subgoal);

  subg_deltf_ptr(subgoal) = dtf;
}

#define check_insert_shared_deltf_subgoal(context, subgoal)	\
  check_insert_global_deltf_subgoal(context, subgoal)	 

#else /* not MULTI_THREAD */

#define check_insert_private_deltf_pred(tif)	\
  check_insert_global_deltf_pred(tif)	 

#define check_insert_private_deltf_subgoal(subgoal)	\
  check_insert_global_deltf_subgoal(subgoal)	 

#endif

/* - - - - - - - - - - */

/* Assumes cps check has already been done, so that mark bit is set on
 * TIFs.  Assumes TIF is non-null.  Tif chain is not changed,
 * therefore no need for mutex.  Reclaims space for shared tables only
 * if 1 active thread.
 */  

int fast_abolish_table_predicate(CTXTdeclc Psc psc)
{
  TIFptr tif;

  gc_tabled_preds(CTXT);

  tif = get_tip(CTXTc psc);

  if (IsVariantPredicate(tif) && IsNULL(TIF_CallTrie(tif))) {
    return 1;
  }

  if ( ! is_completed_table(tif) ) {
      xsb_abort("[abolish_table_pred] Cannot abolish incomplete table"
		" of predicate %s/%d\n", get_name(psc), get_arity(psc));
  }

  /* incremental evaluation */
  if(get_incr(psc)) {
    xsb_warn("[abolish_table_pred] Abolish incremental table"
		" of predicate %s/%d. This can cause unexpected behavior.\n", get_name(psc), get_arity(psc));
  }

  if (!TIF_Mark(tif) && (!get_shared(psc) || flags[NUM_THREADS] == 1)) {
    delete_predicate_table(CTXTc tif);
  }  else {
    //    fprintf(stderr,"Delaying abolish of table in use: %s/%d\n",
    //    get_name(psc),get_arity(psc));
#ifndef MULTI_THREAD
    check_insert_private_deltf_pred(CTXTc tif);
#else
    if (!get_shared(psc))
      check_insert_private_deltf_pred(CTXTc tif);
    else
      check_insert_shared_deltf_pred(CTXT,tif);
#endif
  }
return 1;
}

/* - - - - - - - - - - */

void mark_cp_tables(CTXTdecl)
{
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  TIFptr tif;
  
  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      if (IsInAnswerTrie((BTNptr) *cp_top1)) {
	tif = get_tif_for_answer_trie_cp(CTXTc (BTNptr) *cp_top1);
	cps_check_mark_tif(tif);
      }
    }
    cp_top1 = cp_prevtop(cp_top1);
  }
}

void unmark_cp_tables(CTXTdecl)
{
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  TIFptr tif;
  
  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      if (IsInAnswerTrie((BTNptr) *cp_top1)) {
	tif = get_tif_for_answer_trie_cp(CTXTc (BTNptr) *cp_top1);
	cps_check_unmark_tif(tif);
      }
    }
    cp_top1 = cp_prevtop(cp_top1);
  }
}

/*------------------------------------------------------------------*/
/* abolish_table_call() and supporting code */
/*------------------------------------------------------------------*/

/* 
   Recurse through CP stack looking for trie nodes that match PSC.
   Returns 1 if found a psc match, 0 if safe to delete now
*/

int abolish_table_call_cps_check(CTXTdeclc VariantSF subgoal) 
{
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  int found_subgoal_match;

  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  found_subgoal_match = 0;
  while ( cp_top1 < cp_bot1 && !(found_subgoal_match)) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      // Below we want basic_answer_trie_tt, ts_answer_trie_tt
      if (IsInAnswerTrie(((BTNptr) *cp_top1))) {
	if (subgoal == 
	    get_subgoal_frame_for_answer_trie_cp(CTXTc (BTNptr) *cp_top1)) {
	  found_subgoal_match = 1;
	}
      }
    }
    cp_top1 = cp_prevtop(cp_top1);
  }
  return found_subgoal_match;
}

/* incremental */
int abolish_table_call_incr(CTXTdeclc VariantSF subgoal) {
  if(IsIncrSF(subgoal))
    abolish_incr_call(CTXTc subgoal->callnode);
  
  return TRUE;
}


int abolish_table_call(CTXTdeclc VariantSF subgoal) {

    TIFptr tif;
    Psc psc;
    int action;

    //    subgoal = (VariantSF) ptoc_int(CTXTc 1);
    tif = subg_tif_ptr(subgoal);
    psc = TIF_PSC(tif);

    if (!is_completed(subgoal)) {
      xsb_abort("[abolish_table_call] Cannot abolish incomplete tabled call"
		" of predicate %s/%d\n",get_name(psc),get_arity(psc));
    }

    if (flags[NUM_THREADS] == 1 || !get_shared(psc)) {
      action = abolish_table_call_cps_check(CTXTc subgoal);
    } else action = 1;

    SET_TRIE_ALLOCATION_TYPE_SF(subgoal); // set smBTN to private/shared
    if (!action) {
      delete_branch(CTXTc subgoal->leaf_ptr, &tif->call_trie); /* delete call */
      delete_variant_sf_and_answers(CTXTc subgoal); // delete answers
      return TRUE;
    }
    else {
      //      fprintf(stderr,"Delaying abolish of call in use for: %s/%d\n",
      //      get_name(psc),get_arity(psc));
#ifndef MULTI_THREAD
      delete_branch(CTXTc subgoal->leaf_ptr, &tif->call_trie); /* delete call */
      check_insert_private_deltf_subgoal(CTXTc subgoal);
#else
      if (!get_shared(psc)) {
	delete_branch(CTXTc subgoal->leaf_ptr, &tif->call_trie); /* delete call */
	check_insert_private_deltf_subgoal(CTXTc subgoal);
      }
      else {
	safe_delete_branch(subgoal->leaf_ptr); 
	check_insert_shared_deltf_subgoal(CTXT, subgoal);
      }
#endif
      return TRUE;
    }
}

/*------------------------------------------------------------------*/
/* abolish_table_pred() and supporting code */
/*------------------------------------------------------------------*/

/* 
   Recurse through CP stack looking for trie nodes that match PSC.
   Returns 1 if found a psc match, 0 if safe to delete now
*/

int abolish_table_pred_cps_check(CTXTdeclc Psc psc) 
{
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  int found_psc_match;

  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  found_psc_match = 0;
  while ( cp_top1 < cp_bot1 && !(found_psc_match)) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      // Below we want basic_answer_trie_tt, ts_answer_trie_tt
      if (IsInAnswerTrie(((BTNptr) *cp_top1))) {
	if (psc == get_psc_for_answer_trie_cp(CTXTc (BTNptr) *cp_top1)) {
	  found_psc_match = 1;
	}
      }
    }
    cp_top1 = cp_prevtop(cp_top1);
  }
  return found_psc_match;
}

/* Delays spece reclamation if the cps check does not pass OR if
   shared and more than 1 thread is active.

  abolish_table_predicate does not reclaim space for previously
 "abolished" tables in deltf frames.  Need to do gc tables for
  that. */

inline int abolish_table_predicate(CTXTdeclc Psc psc)
{
  TIFptr tif;
  int action;

  //  printf("\n.........starting atp\n");
  tif = get_tip(CTXTc psc);
  //  printTIF(tif);
  //  print_private_deltfs(CTXT);
  gc_tabled_preds(CTXT);
  if ( IsNULL(tif) ) {
    xsb_abort("[abolish_table_pred] Attempt to delete non-tabled predicate (%s/%d)\n",
	      get_name(psc), get_arity(psc));
  }
  /* incremental */
  if(get_incr(psc)) {
    xsb_warn("[abolish_table_predicate] Abolish incremental table"
		" of predicate %s/%d. This can cause unexpected behavior.\n", get_name(psc), get_arity(psc));
    free_incr_hashtables(tif);
  }

  if (IsVariantPredicate(tif) && IsNULL(TIF_CallTrie(tif))) {
    return 1;
  }

  if ( ! is_completed_table(tif) ) {
      xsb_abort("[abolish_table_pred] Cannot abolish incomplete table"
		" of predicate %s/%d\n", get_name(psc), get_arity(psc));
  }

  if (flags[NUM_THREADS] == 1 || !get_shared(psc)) {
    action = abolish_table_pred_cps_check(CTXTc psc);
  }
  else action = 1;
  if (!action) {
    delete_predicate_table(CTXTc tif);
    return 1;
  }
  else {
    //        fprintf(stderr,"Delaying abolish of table in use: %s/%d\n",
    //        get_name(psc),get_arity(psc));
#ifndef MULTI_THREAD
    check_insert_private_deltf_pred(CTXTc tif);
#else
    if (!get_shared(psc))
      check_insert_private_deltf_pred(CTXTc tif);
    else
      check_insert_shared_deltf_pred(CTXT, tif);
#endif
    return 1; 
  }
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

void mark_tabled_preds(CTXTdecl) { 
  CPtr cp_top1,cp_bot1 ; byte cp_inst;
  TIFptr tif;
  VariantSF subgoal;
  BTNptr call_trie;

  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      if (IsInAnswerTrie((BTNptr) *cp_top1)) {
	DelTFptr dtf;

	/* Check for predicate DelTFs */
	tif = get_tif_for_answer_trie_cp(CTXTc (BTNptr) *cp_top1);

	subgoal = get_subgoal_frame_for_answer_trie_cp(CTXTc (BTNptr) *cp_top1);
	call_trie = get_call_trie_from_subgoal_frame(CTXTc subgoal);
	//	printf("subgoal %p call_trie %p\n",subgoal,call_trie);
	
	dtf = TIF_DelTF(tif);
	while (dtf) {
	  if (DTF_CallTrie(dtf) == call_trie) {
	    DTF_Mark(dtf) = 1;
	    dtf = NULL;
	  } else dtf = DTF_NextPredDTF(dtf);
	}

	//	if (TIF_CallTrie(tif) == NULL && TIF_Subgoals(tif) == NULL) {
	//	  dtf = TIF_DelTF(tif);
	//	  DTF_Mark(dtf) = 1;
	//	}
	
	/* Now check for subgoal DelTFs */
	subgoal = get_subgoal_frame_for_answer_trie_cp(CTXTc (BTNptr) *cp_top1);
	if (is_completed(subgoal)) {
	  if (subg_deltf_ptr(subgoal) != NULL) {
	    DTF_Mark((DelTFptr) subg_deltf_ptr(subgoal)) = 1;
	  }
	}
      }
    }
    cp_top1 = cp_prevtop(cp_top1);
  }
}

/* Mark only private tables -- ignore shared tables. Used by mt system
   when gc-ing with more than 1 active thread -- and used in lieu
   of mark_tabled_preds()
*/
void mark_private_tabled_preds(CTXTdecl) { 
  CPtr cp_top1,cp_bot1 ; byte cp_inst;
  TIFptr tif;
  VariantSF subgoal;
  BTNptr call_trie;
  
  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      if (IsInAnswerTrie((BTNptr) *cp_top1)) {
	DelTFptr dtf;

	/* Check for predicate DelTFs */
	tif = get_tif_for_answer_trie_cp(CTXTc (BTNptr) *cp_top1);

	if (!get_shared(TIF_PSC(tif))) {
	  subgoal = get_subgoal_frame_for_answer_trie_cp(CTXTc (BTNptr) *cp_top1);
	  call_trie = get_call_trie_from_subgoal_frame(CTXTc subgoal);
	  //	printf("subgoal %p call_trie %p\n",subgoal,call_trie);
	
	  dtf = TIF_DelTF(tif);
	  while (dtf) {
	    if (DTF_CallTrie(dtf) == call_trie) {
	      DTF_Mark(dtf) = 1;
	      dtf = NULL;
	    } else dtf = DTF_NextPredDTF(dtf);
	  }

	}

	if (TIF_CallTrie(tif) == NULL && TIF_Subgoals(tif) == NULL 
	    && !get_shared(TIF_PSC(tif))) { 
	  dtf = TIF_DelTF(tif);
	  DTF_Mark(dtf) = 1;
	}

	/* Now check for subgoal DelTFs */
	subgoal = get_subgoal_frame_for_answer_trie_cp(CTXTc (BTNptr) *cp_top1);
	if (is_completed(subgoal) 
	    && !get_shared(TIF_PSC(subg_tif_ptr(subgoal)))) {
	  if (subg_deltf_ptr(subgoal) != NULL) {
	    DTF_Mark((DelTFptr) subg_deltf_ptr(subgoal)) = 1;
	  }
	}
      }
    }
    cp_top1 = cp_prevtop(cp_top1);
  }
}

#ifdef MULTI_THREAD
int sweep_private_tabled_preds(CTXTdecl) {
  DelTFptr deltf_ptr, next_deltf_ptr;
  int dtf_cnt = 0;
  TIFptr tif_ptr;

  deltf_ptr = private_deltf_chain_begin;
  SET_TRIE_ALLOCATION_TYPE_PRIVATE();
  while (deltf_ptr) {
    next_deltf_ptr = DTF_NextDTF(deltf_ptr);
    if (DTF_Mark(deltf_ptr)) {
      tif_ptr = subg_tif_ptr(DTF_Subgoals(deltf_ptr));
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
	  //	  fprintf(stderr,"Garbage Collecting Subgoal: %s/%d\n",
	  //	  get_name(TIF_PSC(tif_ptr)),get_arity(TIF_PSC(tif_ptr)));
	  delete_variant_sf_and_answers(CTXTc DTF_Subgoal(deltf_ptr)); 
	  Free_Private_DelTF_Subgoal(deltf_ptr,tif_ptr);
	}
    }
    deltf_ptr = next_deltf_ptr;
  }
  return dtf_cnt;
}
#endif

/* No mutex on this predicate, as global portions can only be called
   with one active thread */

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
      tif_ptr = subg_tif_ptr(DTF_Subgoals(deltf_ptr));
      //      fprintf(stderr,"Skipping: %s/%d\n",
      //      get_name(TIF_PSC(tif_ptr)),get_arity(TIF_PSC(tif_ptr)));
      DTF_Mark(deltf_ptr) = 0;
      dtf_cnt++;
    }
    else {
      if (DTF_Type(deltf_ptr) == DELETED_PREDICATE) {
	tif_ptr = subg_tif_ptr(DTF_Subgoals(deltf_ptr));
	//	fprintf(stderr,"Garbage Collecting Predicate: %s/%d\n",
	//get_name(TIF_PSC(tif_ptr)),get_arity(TIF_PSC(tif_ptr)));
	reclaim_deleted_predicate_table(CTXTc deltf_ptr);
	Free_Global_DelTF_Pred(deltf_ptr,tif_ptr);
      } else 
	if (DTF_Type(deltf_ptr) == DELETED_SUBGOAL) {
	  tif_ptr = subg_tif_ptr(DTF_Subgoal(deltf_ptr));
	  //	  fprintf(stderr,"Garbage Collecting Subgoal: %s/%d\n",
	  //  get_name(TIF_PSC(tif_ptr)),get_arity(TIF_PSC(tif_ptr)));
	  delete_variant_sf_and_answers(CTXTc DTF_Subgoal(deltf_ptr)); 
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
int gc_tabled_preds(CTXTdecl) 
{
    mark_tabled_preds(CTXT);
    return sweep_tabled_preds(CTXT);
  return 0;
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

  mark_cp_tables(CTXT);

  for (i=0; i<symbol_table.size; i++) {
    if ((pair = (Pair) *(symbol_table.table + i))) {
      byte type;
      
      psc = pair_psc(pair);
      type = get_type(psc);
      if (type == T_DYNA || type == T_PRED) 
	if (!get_data(psc) ||
	    !strcmp(get_name(get_data(psc)),"usermod") ||
	    !strcmp(get_name(get_data(psc)),"global")) 
	  if (get_tabled(psc)) {
	    fast_abolish_table_predicate(CTXTc psc);
	  }
    }
  }

  unmark_cp_tables(CTXT);

  return TRUE;
}

/* - - - - - - - - - - */

int abolish_module_tables(CTXTdeclc const char *module_name)
{
  Pair modpair, pair;
  byte type;
  Psc psc, module;
  
  mark_cp_tables(CTXT);
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
	fast_abolish_table_predicate(CTXTc psc);
      }
    pair = pair_next(pair);
  }
  unmark_cp_tables(CTXT);
  return TRUE;
}

/*----------------------------------------------------------------------*/
/* abolish_private/shared_tables() and supporting code */
/*------------------------------------------------------------------*/

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

int abolish_mt_tables_cps_check(CTXTdecl,xsbBool isPrivate) 
{
  CPtr cp_top1,cp_bot1 ;
  byte cp_inst;
  int found_match;

  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;
  cp_top1 = breg ;				 
  found_match = 0;
  while ( cp_top1 < cp_bot1 && !(found_match)) {
    cp_inst = *(byte *)*cp_top1;
    // Want trie insts, but will need to distinguish from
    // asserted and interned tries
    if ( is_trie_instruction(cp_inst) ) {
      // Below we want basic_answer_trie_tt, ts_answer_trie_tt
      if (IsInAnswerTrie(((BTNptr) *cp_top1))) {
	if (get_private(get_psc_for_answer_trie_cp(CTXTc (BTNptr) *cp_top1)) == isPrivate) {
	  found_match = 1;
	}
      }
    }
    cp_top1 = cp_prevtop(cp_top1);
  }
  return found_match;
}

/* will not reclaim space if more than one thread */
void abolish_shared_tables(CTXTdecl) {
  TIFptr abol_tif;

  mark_cp_tables(CTXT);

  for (abol_tif = tif_list.first ; abol_tif != NULL
	 ; abol_tif = TIF_NextTIF(abol_tif) ) {
      fast_abolish_table_predicate(CTXTc TIF_PSC(abol_tif));
  }

  unmark_cp_tables(CTXT);

}

/* will not reclaim space if more than one thread (via fast_atp) */
void abolish_all_shared_tables(CTXTdecl) {

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

  mark_cp_tables(CTXT);

  for (abol_tif = private_tif_list.first ; abol_tif != NULL
	 ; abol_tif = TIF_NextTIF(abol_tif) ) {
      fast_abolish_table_predicate(CTXTc TIF_PSC(abol_tif));
  }

  unmark_cp_tables(CTXT);

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

/* TLS: mutex may not be needed here, as we're freeing private
   resources.  This function handles the case when one thread creates
   a private tif, exits, its xsb_thread_id is reused, and the new
   thread creates a private tif for the same table.  */

void thread_free_private_tifs(CTXTdecl) {
  struct TDispBlk_t *tdispblk;
  TIFptr tip;

  SYS_MUTEX_LOCK( MUTEX_TABLE );
  for (tdispblk=tdispblkhdr.firstDB 
	 ; tdispblk != NULL ; tdispblk=tdispblk->NextDB) {
    if (xsb_thread_entry <= tdispblk->MaxThread) {
      tip = (&(tdispblk->Thread0))[xsb_thread_entry];
      if (tip) {
	(&(tdispblk->Thread0))[xsb_thread_entry] = (TIFptr) NULL;
	Free_Private_TIF(tip);
      }
    }
  }
  SYS_MUTEX_UNLOCK( MUTEX_TABLE );
}

static inline void thread_free_private_deltfs(CTXTdecl) {

  DelTFptr next_deltf;
  DelTFptr deltf = private_deltf_chain_begin;

  while (deltf) {
    next_deltf = DTF_NextDTF(deltf);
    mem_dealloc(deltf,sizeof(DeletedTableFrame),TABLE_SPACE);		
    deltf = next_deltf;
  }
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
  SM_ReleaseResources(smTableBTN);
  TrieHT_FreeAllocatedBuckets(smTableBTHT);
  SM_ReleaseResources(smTableBTHT);
  SM_ReleaseResources(smTSTN);
  TrieHT_FreeAllocatedBuckets(smTSTHT);
  SM_ReleaseResources(smTSTHT);
  SM_ReleaseResources(smTSIN);
  SM_ReleaseResources(smALN);
  SM_ReleaseResources(smVarSF);
  SM_ReleaseResources(smProdSF);
  SM_ReleaseResources(smConsSF);
  SM_ReleaseResources(smASI);
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
  int trie_type;

  cp_bot1 = (CPtr)(tcpstack.high) - CP_SIZE;

  cp_top1 = breg ;				 
  while ( cp_top1 < cp_bot1 ) {
    cp_inst = *(byte *)*cp_top1;
    /* Check for trie instructions */
    if ( is_trie_instruction(cp_inst)) {
      trie_type = (int) TN_TrieType((BTNptr) *cp_top1);
      /* Here, we want call_trie_tt,basic_answer_trie_tt,
	 ts_answer_trie_tt","delay_trie_tt */
      if (IsInAnswerTrie(((BTNptr) *cp_top1))) {
	xsb_abort("[abolish_all_tables/0] Illegal table operation"
		  "\n\t Backtracking through tables to be abolished.");
      }
    }
      cp_top1 = cp_prevtop(cp_top1);
  }
}

#if !defined(WIN_NT) || defined(CYGWIN) 
inline 
#endif
void abolish_table_info(CTXTdecl)
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
// This code under development -- TLS

#ifdef BITS64
#define VISITED_MASK 0xf000000000000000
#else
#define VISITED_MASK 0xf0000000
#endif

#ifdef BITS64
#define STACK_MASK 0xfffffffffffffff
#else
#define STACK_MASK 0xfffffff
#endif

#define VISITED(as_leaf)  (asi_scratchpad((ASI) Child(as_leaf)) & VISITED_MASK)
#define STACK_INDEX(as_leaf)  (asi_scratchpad((ASI) Child(as_leaf)) & STACK_MASK)
#define MARK_VISITED(as_leaf) {asi_scratchpad((ASI) Child(as_leaf)) = VISITED_MASK;}

extern void print_subgoal(CTXTdeclc FILE *, VariantSF);

static int dfn = 0;

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
  index = component_stack_top;				\
  component_stack_top++;\
  }

/* for use when you don't need the index returned */
#define push_comp_node_1(as_leaf) {			\
    int index;						\
    MARK_VISITED(as_leaf);				\
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

//----------------------------------------------------------------------
struct done_answer_dfn {
  BTNptr  answer;
  int     scc;
} ;
typedef struct done_answer_dfn *doneAnswerDFN;

int done_stack_top = 0;
doneAnswerDFN done_stack = NULL;
int done_stack_size = 0;

#define push_done_node(index,dfn_num) {					\
    if (done_stack_top >= done_stack_size) {				\
      unsigned long old_done_stack_size = done_stack_size;		\
      done_stack_size = done_stack_size + component_stack_increment;		\
      done_stack = (doneAnswerDFN) mem_realloc(done_stack,		\
					       old_done_stack_size*sizeof(struct done_answer_dfn), \
					  done_stack_size*sizeof(struct done_answer_dfn), \
					  TABLE_SPACE);			\
    }									\
    done_stack[done_stack_top].scc = dfn_num;				\
    done_stack[done_stack_top].answer = component_stack[index].answer;	\
    done_stack_top++;							\
  }

void print_done_stack(CTXTdecl) {
  int frame = 0;
  while (frame < done_stack_top) {
    printf("done_frame %d answer %p scc %d  ",frame, 
	   done_stack[frame].answer,done_stack[frame].scc);
    print_subgoal(CTXTc stddbg, asi_subgoal((ASI) Child(done_stack[frame].answer)));
    printf("\n");
    frame++;
  }
}

// reset the scratchpad for each answer in done stack
void reset_done_stack() {
  int frame = 0;
  while (frame < done_stack_top) {
    asi_scratchpad((ASI) Child(done_stack[frame].answer)) = 0;
    frame++;
  }
}

//----------------------------------------------------------------------
/* Returns -1 when no answer found (not 0, as 0 can be an index */
int visited_answer(BTNptr as_leaf) {		
  int found = -1;
  int cur_stack_frame = 0;

  while (found < 0 && cur_stack_frame < done_stack_top) {
    if (done_stack[cur_stack_frame].answer == as_leaf)
      found = cur_stack_frame;
    cur_stack_frame++;
  } 

  cur_stack_frame = 0;
  while (found < 0 && cur_stack_frame < component_stack_top) {
    if (component_stack[cur_stack_frame].answer == as_leaf)
      found = cur_stack_frame;
    cur_stack_frame++;
  } 
  return found;
}

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
    if (component_stack[to_answer].dfn < component_stack[from_answer].min_link)	 \
      component_stack[from_answer].min_link = component_stack[to_answer].dfn;	 \
  }							 \



int table_component_check(CTXTdeclc NODEptr from_answer) {
  DL delayList;
  DE delayElement;
  BTNptr to_answer;
  int to_answer_idx, from_answer_idx, component_num;

  //  if (is_conditional_answer(from_answer)) {
    push_comp_node(from_answer,from_answer_idx);
       printf("starting: "); 
       print_subgoal(CTXTc stddbg, asi_subgoal((ASI) Child(from_answer)));printf("\n");
    //    print_comp_stack(CTXT);
    delayList = asi_dl_list((ASI) Child(from_answer));
    while (delayList) {
      delayElement = dl_de_list(delayList);
      while (delayElement) {
	if (de_ans_subst(delayElement)) to_answer = de_ans_subst(delayElement);
	else to_answer = traverse_subgoal(de_subgoal(delayElement));
	if  (0 >  (to_answer_idx = visited_answer(to_answer))) {
	  to_answer_idx = table_component_check(CTXTc to_answer);
	  update_minlink_minlink(from_answer_idx,to_answer_idx);
	} else update_minlink_dfn(from_answer_idx,to_answer_idx);
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
	push_done_node(component_stack_top-1,component_num);
	component_stack_top--;
      }
    }
    print_comp_stack(CTXT); printf("\n");
    return from_answer_idx;
    // }
}

xsbBool table_inspection_function( CTXTdecl ) 
{
  switch (ptoc_int(CTXTc 1)) {

  case FIND_COMPONENTS: {
    table_component_check(CTXTc (NODEptr) ptoc_int(CTXTc 2));
    print_done_stack(CTXT);
    break;
  }

  case FIND_FORWARD_DEPENDENCIES: {
  DL delayList;
  DE delayElement;
  BTNptr as_leaf, new_answer;
  struct answer_dfn stack_node;

  done_stack_top = 0; dfn = 0;
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
	    if (!VISITED(de_ans_subst(delayElement)))
	      push_comp_node_1(de_ans_subst(delayElement));
	  } else {
	    new_answer = traverse_subgoal(de_subgoal(delayElement));
	    if (!VISITED(new_answer)) 
	      push_comp_node_1(new_answer);
	    }
	    delayElement = de_next(delayElement);
	  }
	  delayList = de_next(delayList);
	}
      }
    mem_dealloc(component_stack,component_stack_size*sizeof(struct answer_dfn),
		TABLE_SPACE);
    print_done_stack(CTXT);
    // Don't deallocte done stack until done with its information.
    reset_done_stack();
  }
  else  printf("found unconditional answer %p\n",as_leaf);
    break;
  }

  case FIND_BACKWARD_DEPENDENCIES: {
    break;
  }

  }

  return TRUE;
  }

