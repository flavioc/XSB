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
#include "memory.h"
#include "register.h"
#include "deref.h"
#include "flags.h"
#include "tries.h"
#if (!defined(WAM_TRAIL))
#include "cut.h"
#endif
#include "xmacro.h"
#include "sw_envs.h"
#include "choice.h"
#include "inst.h"
#include "xsberror.h"
#include "trassert.h"
#include "tr_utils.h"
#ifdef CHAT
#include "chat.h"
#endif

/*----------------------------------------------------------------------*/

#define MAX_VAR_SIZE	200

CPtr Temp_VarPosReg;
CPtr call_vars[MAX_VAR_SIZE];

extern Cell ptoc_tag(int);
#ifdef DEBUG
extern void printterm(Cell, byte, int);
#endif

/*----------------------------------------------------------------------*/

void trie_node_element(void)
{
  NODEptr i;

  i = (NODEptr) ptoc_int(1);
  ctop_int(2, (Integer)Child(i));
}

/*----------------------------------------------------------------------*/

bool has_unconditional_answers(SGFrame subg)
{
  ALPtr node_ptr = subg_answers(subg);
 
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
static NODEptr  *GNodePtrPtr;

/*----------------------------------------------------------------------*/

#define IsInsibling_rdonly(wherefrom,Found,item)\
{\
  LocalNodePtr = wherefrom;\
  while(LocalNodePtr && (Atom(LocalNodePtr) != item)) {\
    LocalNodePtr = Sibl(LocalNodePtr);\
  }\
  if (!LocalNodePtr) {\
    Found = 0;\
  }  else Paren = LocalNodePtr;\
}

/*************************************************/
#define one_node_chk(Found,Item)\
{\
  NODEptr LocalNodePtr, *Cnodeptr;\
  struct HASHhdr *hh;\
  Cell item;\
  char message[80];\
  item = (Cell) Item;\
  if (!(*GNodePtrPtr)) {\
    Found = 0;\
    sprintf(message, "Inconsistency in one_node_chk() (GNodePtrPtr = %p)",\
		     GNodePtrPtr);\
    xsb_exit(message);\
  } else {\
    if (Sibl(*GNodePtrPtr) == (NODEptr)-1) { \
      Cnodeptr = (NODEptr *)Child(*GNodePtrPtr);\
      hh = (struct HASHhdr *)Cnodeptr - 1;\
      GNodePtrPtr = Cnodeptr + HASH(item,hh->HASHmask);\
    }\
    IsInsibling_rdonly(*GNodePtrPtr,Found,item);\
    GNodePtrPtr = &(Child(LocalNodePtr)); \
  }\
}
/*********************************/


#define recvariant_call_rdonly(flag,t_pcreg)\
{\
	CPtr  xtemp1;\
	int j;\
	while ((!pdlempty) && flag ) {\
	  xtemp1 = (CPtr) pdlpop;\
	  cptr_deref( xtemp1);\
	  switch(cell_tag(xtemp1)) {\
	    case FREE: case REF1: \
	      if(is_VarEnumerator(xtemp1)){\
	      *(--Temp_VarPosReg) =(Cell) xtemp1;\
	      /*printf("var[%lx] at %lx is %d\n",ctr,Temp_VarPosReg -1, xtemp1);*/\
	      dbind_ref_nth_var(xtemp1,ctr);\
	      one_node_chk(flag,maketrievar((ctr | 0x10000)));\
	      ctr++;\
					  }\
	    else{\
              one_node_chk(flag,maketrievar(trie_var_num(xtemp1)));\
	       }\
              break;\
	    case STRING: case INT:  case FLOAT: \
	      one_node_chk(flag,xtemp1);\
	      break;\
	    case LIST:\
	      one_node_chk(flag,LIST);\
	      pdlpush( cell(clref_val(xtemp1)+1) );/* changed */\
              pdlpush( cell(clref_val(xtemp1)) );\
	      break;\
	    case CS:\
	      one_node_chk(flag,makecs(follow(cs_val(xtemp1)))); /*put root in trie */\
	      for(j=get_arity((Psc)follow(cs_val(xtemp1))); j>=1 ; j--)\
			{pdlpush(cell(clref_val(xtemp1) +j));}\
	      break;\
	    default: \
              xsb_abort("Bad tag in recvariant_call_rdonly");\
	  }\
	  }\
        resetpdl;\
}

/*----------------------------------------------------------------------*/

void variant_call_search_rdonly(int arity, CPtr cptr,
				CPtr *curcallptr, int *flagptr, byte *t_pcreg)
{
    CPtr *xtrbase,xtemp1;
    int i,j,flag = 1;

    xtrbase = trreg;                                
    ctr = 0;                                                    
    Temp_VarPosReg = (CPtr)call_vars + MAX_VAR_SIZE - 1;
    Paren = NOPAR;
    GNodePtrPtr = (NODEptr *)curcallptr;
    if (*GNodePtrPtr == 0) {
      flag = 0;
    }

    for (i = 1 ; (i<= arity) && flag ; i++) {                      
      xtemp1 = (CPtr) (cptr + i);            /*Note! */                  
      cptr_deref(xtemp1);                           
      switch (cell_tag(xtemp1)) {                              
        case FREE: case REF1:
	  if (is_VarEnumerator(xtemp1)) {
	    *(--Temp_VarPosReg) = (Cell) xtemp1;	
	    dbind_ref_nth_var(xtemp1,ctr);                 
	    one_node_chk(flag,maketrievar(ctr | 0x10000));           
	    ctr++;
	  } else {
	    one_node_chk(flag,maketrievar(trie_var_num(xtemp1)));
	  }
	  break;
	case STRING: case INT: case FLOAT:            
	  one_node_chk(flag,xtemp1);                     
	  break;                                              
	case LIST:                                           
	  one_node_chk(flag,LIST);                       
	  pdlpush(cell(clref_val(xtemp1)+1));  /* changed */
	  pdlpush(cell(clref_val(xtemp1))) ;                 
	  recvariant_call_rdonly(flag,t_pcreg);                      
	  break;                                              
	case CS: 
	  one_node_chk(flag,makecs(follow(cs_val(xtemp1))));     
	  /* put root in trie */                               
	  for (j=get_arity((Psc)follow(cs_val(xtemp1))); j>=1; j--) {
	    pdlpush(cell(clref_val(xtemp1)+j));
	  }
	  recvariant_call_rdonly(flag,t_pcreg);                      
	  break;                                              
	default:                                             
	  xsb_exit("Bad tag in variant_call_search_rdonly()");
	}
    }                
    resetpdl;
    *(--Temp_VarPosReg) = ctr;
    table_undo_bindings(xtrbase);
    if (flag) {
      if (arity == 0) {
	one_node_chk(flag,(Cell)0);
      }
      *curcallptr = (CPtr) (&(Child(Paren)));
    }
    *flagptr = flag;
}

/*----------------------------------------------------------------------*/
/* This function resembles an analogous function in tries.c.  It is
 * supposed to be used only after variant_call_search_rdonly() has been
 * called and the variables in the substitution factor have been left in
 * the Temp_VarPosReg location (containing the arity) and in #arity
 * locations upwards.
 *----------------------------------------------------------------------*/

void construct_ret_for_call(void)
{
    Pair sym;
    Cell var;
    Cell term; /* the function assumes that term is free on call ! */
    CPtr sreg;
    int  arity, i, new;

    arity = cell(Temp_VarPosReg);
    if (arity == 0) {
      ctop_string(3, (char *) ret_psc[0]);
    } else {
      term = ptoc_tag(1);
      sreg = hreg;
      bind_cs((CPtr)term, sreg);
      sym = insert("ret", arity, (Psc)flags[CURRENT_MODULE], &new);
      new_heap_functor(sreg, sym->psc_ptr);
      for (i = arity; 0 < i; i--) {
	var = cell(Temp_VarPosReg+i);
	bind_copy(sreg, var);
	sreg++;
      }
      hreg = sreg;
    }
}

/*----------------------------------------------------------------------*/

struct freeing_stack_node{
  NODEptr item;
  struct freeing_stack_node *next;
};

#define free_node(node) {\
     /*printf("Freeing node at %x\n",node);*/\
     Sibl(node) = free_trie_nodes;\
     free_trie_nodes = node;\
}

#define free_anslistnode(node) {\
     /*printf("Freeing node at %x\n",node);*/\
     aln_next_aln(node) = free_answer_list_nodes;\
     free_answer_list_nodes = node;\
}

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
       fprintf(stderr,"pop attempted from NULL\n");\
       return;\
    }\
    node         = node_stk_top->item;\
    temp         = node_stk_top;\
    node_stk_top = node_stk_top->next;\
    free(temp);\
}

/*----------------------------------------------------------------------*/
/* Given the address of a node, delete it and all nodes below it in the trie */
/*----------------------------------------------------------------------*/

void delete_table_trie(NODEptr x)
{
  struct freeing_stack_node *node_stk_top = 0, *call_nodes_top = 0;
  NODEptr *Bkp;
  struct HASHhdr *thdr;
  NODEptr node, rnod; 

  push_node(x);
  while (node_stk_top != 0) {
    pop_node(node);
    if (Sibl(node) == (NODEptr)-1) { /* hash node */
      thdr = ((struct HASHhdr *)Child(node)) - 1;
      for (Bkp = (NODEptr *)Child(node);
	   Bkp <= (NODEptr *)Child(node)+(thdr->HASHmask); Bkp++) {
	if (*Bkp) 
	  push_node(*Bkp);
      }
      thdr->prev->next = thdr->next;
      if (thdr->next) 
	thdr->next->prev = thdr->prev;
      free(thdr);
    }
    else {
      if (Sibl(node)) 
	push_node(Sibl(node));
      if ((Instr(node) == trie_proceed) || ftagged(Parent(node))) {
	/* free call str and return subtrie */
	if(Child(node) != NULL){
	  if (subg_ans_root_ptr((NODEptr *)Child(node))) {
	    call_nodes_top = node_stk_top;
	    push_node((NODEptr)subg_ans_root_ptr((SGFrame)Child(node)));
	    free_subgoal_frame((SGFrame)Child(node));
	    /* Here the call structure should be freed, when possible */
	    while (node_stk_top != call_nodes_top) {
	      pop_node(rnod);
	      if (Sibl(rnod) == (NODEptr)-1) {
		thdr = ((struct HASHhdr *)(Child(rnod))) - 1;
		for (Bkp=(NODEptr *)Child(rnod);
		     Bkp <= (NODEptr *)Child(rnod)+(thdr->HASHmask); Bkp++) {
		  if (*Bkp) 
		    push_node(*Bkp);
		}
		thdr->prev->next = thdr->next;
		if (thdr->next) 
		  thdr->next->prev = thdr->prev;
		free(thdr);
	      }
	      else {
		if (Sibl(rnod)) 
		  push_node(Sibl(rnod));
		if ((Instr(rnod) != trie_proceed) && !ftagged(Parent(rnod)))
		  push_node(Child(rnod));
	      }
	      free_node(rnod);
	    }
	  }
	}
      }
      else 
	push_node(Child(node));
    }
    free_node(node);
  }
}

/*----------------------------------------------------------------------*/

void delete_predicate_table(void)
{
/* r1 contains pointer to node that is root of call trie to free */ 

  delete_table_trie(((NODEptr)ptoc_int(1)));
}

/*----------------------------------------------------------------------*/

static int is_hash(NODEptr x) 
{
  if( x == NULL)
    return(0);
  else
    return((Sibl(x) == (NODEptr) -1));
}

/*----------------------------------------------------------------------*/

static NODEptr gParent(NODEptr y)
{
  if(ftagged(Parent(y)))
    return(unftag(Parent(y)));
  else   
    return(Parent(y));
}

/*----------------------------------------------------------------------*/

static NODEptr *parents_childptr(NODEptr y, NODEptr *hook)
{
  NODEptr x;

  if ((x = gParent(y)) == NULL)
    return((NODEptr *)hook);
  else
    return(&(Child(x)));
}

/*----------------------------------------------------------------------*/

static NODEptr *get_headptr_of_list(NODEptr x, Cell Item)
{
  NODEptr *z;
  struct HASHhdr *hh;

  z = (NODEptr *)Child(x);
  hh = (struct HASHhdr *)z -1;
  return(z + HASH(Item,hh->HASHmask));
}

/*----------------------------------------------------------------------*/

static int decr_num_in_hashhdr(NODEptr y)
{
  return((--  ((struct HASHhdr *)Child(y) -1)->numInHash) + 1);
}

/*----------------------------------------------------------------------*/

static NODEptr get_prev_sibl(NODEptr y, NODEptr *hook)
{
  NODEptr x,tempx;

  x = *parents_childptr(y,hook);
  if(is_hash(x)){
    tempx = *get_headptr_of_list(x,Atom(y));
    decr_num_in_hashhdr(x);
    x = tempx;
  }
  while(x != NULL){
    if(Sibl(x) == y)
      return(x);
    x = Sibl(x);
  }  
  xsb_abort("Error in get_previous_sibling");
  return(NULL);
}

/*----------------------------------------------------------------------*/

static void free_hash_hdr(struct HASHhdr *hh)
{
  if(hh-> prev != NULL)
    hh -> prev-> next = hh->next;
  if(hh->next != NULL)
    hh->next->prev = hh->prev;
  free(hh);
}

/*----------------------------------------------------------------------*/

static void free_pointed_hash_hdr(NODEptr x)
{  
  free_hash_hdr((struct HASHhdr *)Child(x) - 1);
}

/*----------------------------------------------------------------------*/
/* deletes and reclaims a whole branch in the return trie               */
/*----------------------------------------------------------------------*/

void delete_branch(NODEptr lowest_node_in_branch, NODEptr *hook)
{
  int num_left_in_hash;
  NODEptr prev, parent_ptr, *y1, *z;
    
  while((lowest_node_in_branch != NULL) && (is_no_cp(lowest_node_in_branch))){
    parent_ptr = gParent(lowest_node_in_branch);   
    y1 = parents_childptr(lowest_node_in_branch, hook);
    if (is_hash(*y1)) {
      z = get_headptr_of_list(*y1,Atom(lowest_node_in_branch));
      *z = NULL;
      num_left_in_hash = decr_num_in_hashhdr(*y1);
      if (num_left_in_hash  > 0) {
	mark_leaf_node_del(lowest_node_in_branch); /* mark node as deleted */
	free_node(lowest_node_in_branch);
	return; /* like a try or a retry or trust node with siblings */ 
      }
      else
	free_pointed_hash_hdr(*y1);
    }
    mark_leaf_node_del(lowest_node_in_branch); /* mark node as deleted */
    free_node(lowest_node_in_branch);
    lowest_node_in_branch = parent_ptr;
  }
  if (lowest_node_in_branch == NULL) {
    *hook = 0;
  } else {
    if(is_try(lowest_node_in_branch)){
      Instr(Sibl(lowest_node_in_branch)) = Instr(Sibl(lowest_node_in_branch)) -1;/* trust -> no_cp  retry -> try */
      y1 = parents_childptr(lowest_node_in_branch,hook);
      if (is_hash(*y1)) {
	z = get_headptr_of_list(*y1,Atom(lowest_node_in_branch));
	num_left_in_hash =decr_num_in_hashhdr(*y1);
      } else
	z = y1;
      *z =Sibl(lowest_node_in_branch);      
    } else { 
      prev = get_prev_sibl(lowest_node_in_branch,hook);      
      Sibl(prev) = Sibl(lowest_node_in_branch);
      if (is_trust(lowest_node_in_branch)){
	Instr(prev) -= 2; /* retry -> trust ; try -> nocp */
      }
    }
    free_node(lowest_node_in_branch);
  }
}

/*----------------------------------------------------------------------*/

void safe_delete_branch(NODEptr lowest_node_in_branch)
{
  byte choicepttype;

  mark_leaf_node_del(lowest_node_in_branch); /* mark node as deleted */
  choicepttype = 0x3 & Instr(lowest_node_in_branch);
  Instr(lowest_node_in_branch) = choicepttype | 0x90; 
  /*mark_leaf_node_del(lowest_node_in_branch);*/ /* mark node as deleted */
  /* The following is a hack and is not working --- Kostis
  Atom(lowest_node_in_branch) = Atom(lowest_node_in_branch) ^ 0x100000;
    */

/* Here I am assuming that the HASH mask is < 0x100000 (65536)
   if it is not, the node will hash into another bucket, resulting  
   in inappropriate behavior on deletion */
}

void undelete_branch(NODEptr lowest_node_in_branch){
   byte choicepttype; 
   byte typeofinstr;
 
   if(is_deleted(lowest_node_in_branch)){
     choicepttype = 0x3 &  Instr(lowest_node_in_branch);
     typeofinstr = (~0x3) & DelFlag(lowest_node_in_branch);

     Instr(lowest_node_in_branch) = choicepttype | typeofinstr;
     DelFlag(lowest_node_in_branch) = 0;
     /*Atom(lowest_node_in_branch) = Atom(lowest_node_in_branch) ^ 0x100000;*/
   }
   else{
     fprintf(stderr,"system warning:Attempt to undelete a node that is not deleted\n");
   }
 }

/*----------------------------------------------------------------------*/

/* This does not reclaim space for deleted nodes, only marks
 * the node as deleted (setting the del_flag), and change the
 * try instruction to fail.
 * The deleted node is then linked into the del_nodes_list
 * in the completion stack.
 */
void delete_return(NODEptr l, SGFrame sg_frame) 
{
  ALPtr a, n, next;
  NLChoice c;
#ifdef CHAT
  chat_init_pheader chat_ptr;
#else
#ifdef LOCAL_EVAL
  TChoice  tc;
#endif
#endif

#ifdef DEBUG_RECLAIM_DEL_RET
    fprintf(stderr,"Delete node: %d - Par: %d\n", l, Parent(l));
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
  ALPtr x,y;
  
  x = compl_del_ret_list(subg_compl_stack_ptr(sg_frame));
  
  while (x != NULL) {
    y = x;
    x = aln_next_aln(x);
    delete_branch(aln_answer_ptr(y), &subg_ans_root_ptr(sg_frame));
    free_anslistnode(y);
  }
}
 
/*----------------------------------------------------------------------*/

void reclaim_ans_list_nodes(SGFrame sg_frame)
{
  ALPtr x, y;
 
  x = subg_answers(sg_frame);
 
  if (x <= COND_ANSWERS) return;        /* already reclaimed... */
 
  subg_answers(sg_frame) = UNCOND_ANSWERS;
 
  while (x != NULL) {
    if (is_conditional_answer(aln_answer_ptr(x)))
      subg_answers(sg_frame) = COND_ANSWERS;
    y = x;
    x = aln_next_aln(x);
    free_anslistnode(y);
  }
}

/*----------------------------------------------------------------------*/

void breg_retskel(void)
{
    Pair    sym;
    Cell    term; /* the function assumes that term is free on call ! */
    SGFrame sg_frame;
    CPtr    tcp, cptr, where, sreg;
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
      term = ptoc_tag(3);
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
    }
    ctop_int(4, (Integer)sg_frame);
}

/*----------------------------------------------------------------------*/

#define ADJUST_SIZE 100

NODEptr *Set_ArrayPtr = NULL;
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

void init_newtrie(void)
{
  Set_ArrayPtr = (NODEptr *) calloc(Set_ArraySz,sizeof(NODEptr));
}

/*----------------------------------------------------------------------*/

void newtrie(void)
{
  int i;
  
  if (first_free_set != 0) {	/* a free set is available */
    i = first_free_set;		/* save it in i */
    ctop_int(1, first_free_set);
    first_free_set = (int) Set_ArrayPtr[first_free_set] >> 2;
    Set_ArrayPtr[i] = NULL;	/* must be reset to NULL */
  }
  else {
    if (num_sets == Set_ArraySz) { /* run out of elements */
      NODEptr *temp_arrayptr;

      temp_arrayptr = Set_ArrayPtr;
      Set_ArraySz += ADJUST_SIZE;  /* adjust the array size */
      Set_ArrayPtr = (NODEptr *) calloc(Set_ArraySz ,sizeof(NODEptr));
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
  NODEptr Leaf;

  switch_to_trie_assert;
  term = ptoc_tag(1);
  RootIndex = ptoc_int(2);

#ifdef DEBUG_INTERN
  printf("Interning ");
  printterm(term,1,25);
  printf("In position %d\n", RootIndex);
#endif
  Leaf = whole_term_chk_ins(term,(CPtr) &(Set_ArrayPtr[RootIndex]),&flag);
  
  ctop_int(3,(Integer)Leaf);
  ctop_int(4,flag);
#ifdef DEBUG_INTERN
  printf("Exit flag %d\n",flag);
#endif
  switch_from_trie_assert;
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
   * Only if Set_ArrayPtr[RootIndex] is a valid NODEptr can we run this
   * builtin.  That means Set_ArrayPtr[RootIndex] can neither be NULL,
   * nor a deleted set (deleted by builtin delete_trie/1).
   */
  if ((Set_ArrayPtr[RootIndex] != NULL) &&
      (!((int) Set_ArrayPtr[RootIndex] & 0x3))) {
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
 * of the trie rooted at Set_Array[ROOT].
 */

void trie_dispose(void)
{
  NODEptr Leaf;
  long Rootidx;

  switch_to_trie_assert;
  Rootidx = ptoc_int(1);
  Leaf = (NODEptr)ptoc_int(2);
  delete_branch(Leaf, &(Set_ArrayPtr[Rootidx]));
  switch_from_trie_assert;
}

/*----------------------------------------------------------------------*/

#define isleaf(x) ((Instr(x) == trie_proceed) || ftagged(Parent(x)))

void delete_all_buckets(NODEptr hashnode){
  NODEptr *Arrayptr;
  struct HASHhdr *hh;
  int i;
    
  Arrayptr = (NODEptr *)Child(hashnode);
  hh       = (struct HASHhdr *)Arrayptr -1;
  for (i = 0; i <= hh-> HASHmask; i++) {
    delete_trie(Arrayptr[i]);
  }
  free_hash_hdr(hh);
}

/*----------------------------------------------------------------------*/

void delete_trie(NODEptr root){
  NODEptr sib, chil;  
  
  if (root != NULL) {
    if (is_hash(root)) {
      delete_all_buckets(root);
    } else {
      sib  = Sibl(root);
      chil = Child(root);      
    /* Child nodes == NULL is not the correct test*/
      if (isleaf(root)) {
	if (chil != NULL)
	  xsb_exit("Anomaly in delete_trie !");
      } else {
	delete_trie(chil);
      }
      delete_trie(sib);
    }
    free_node(root);
  }
}

/*----------------------------------------------------------------------*/
/* defined here because used by biassert.                               */
/*----------------------------------------------------------------------*/

void free_node_function(NODEptr n)
{
   free_node(n); 
}

