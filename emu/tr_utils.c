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
#include "memory.h"
#include "register.h"
#include "deref.h"
#include "tries.h"
#include "xmacro.h"
#include "choice.h"
#include "inst.h"
#include "xsberror.h"
#include "switch.h"
#include "tr_utils.h"

#define MAX_VAR_SIZE	200

CPtr Temp_VarPosReg;
CPtr call_vars[MAX_VAR_SIZE];

#ifdef DEBUG
extern void printterm(Cell, byte, int);
#endif

/*----------------------------------------------------------------------*/

void trie_node_element(void)
{
  NODEptr i;
  Cell tag;

  i = (NODEptr) ptoc_int(1);
  ctop_int(2,(Integer)Sibl(i));
  ctop_int(3,(Integer)Child(i));
  ctop_int(4,((Integer)Parent(i) & 0x3));
  ctop_int(5,((Integer)Parent(i) & ~0x3L));
  tag = cell_tag(Atom(i));
  ctop_int(6,(Integer)tag);
  switch (tag) {
  case TrieVar:
    ctop_int(7,int_val(Atom(i)));
    break;
  case STRING: case INT: case FLOAT:
    ctop_tag(7,Atom(i));
    break;
  case LIST:
    ctop_int(7,LIST);
    break;
  case CS:
    ctop_int(7,(Integer)cs_val(Atom(i)));
    break;
  default:
    fprintf(stderr,"ERROR in trie-node element, tag(%lx). Shouldn't happen.\n",
	    tag);
    ctop_int(7,0);
  }
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
				CPtr *curcallptr, int *flagptr,byte *t_pcreg)         
{
	CPtr *xtrbase,xtemp1;
	int i,j,flag = 1;
	xtrbase =trreg;                                
        ctr = 0;                                                    
	Temp_VarPosReg = (CPtr)call_vars + MAX_VAR_SIZE - 1;
	Paren = NOPAR;
	GNodePtrPtr = (NODEptr *)curcallptr;
	if (*GNodePtrPtr == 0)
	{
		flag = 0;
	}

	for (i = 1 ; (i<= arity) && flag ; i++) {                      
	  xtemp1 = (CPtr) (cptr + i);            /*Note! */                  
	  cptr_deref(xtemp1);                           
	  switch (cell_tag(xtemp1)) {                              
	    case FREE: case REF1:
	    if(is_VarEnumerator(xtemp1)){
            /*    one_node_chk(flag,FREE);                        */
		*(--Temp_VarPosReg) = (Cell) xtemp1;	
		dbind_ref_nth_var(xtemp1,ctr);                 
		one_node_chk(flag,maketrievar(ctr | 0x10000));           
		ctr++;                                              
	      }	    
	    else{
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
                /*put root in trie */                               
		for(j=get_arity((Psc)follow(cs_val(xtemp1))); j>=1; j--)
		  {pdlpush(cell(clref_val(xtemp1)+j));}              
		recvariant_call_rdonly(flag,t_pcreg);                      
		break;                                              
  	    default:                                             
		printf("Bad tag in variant_call_search_rdonly\n");
		exit(1);
	    }
         }                
        resetpdl;
	*(--Temp_VarPosReg) = ctr;
	table_undo_bindings(xtrbase, xtemp1);
	if(flag){
	  if(arity == 0){
	    one_node_chk(flag,(Cell)0);
	  }
	  *curcallptr = (CPtr) (&(Child(Paren)));
	}
	*flagptr = flag;
}

/*----------------------------------------------------------------------*/

struct freeing_stack_node{
NODEptr item;
struct freeing_stack_node *next;
};

#ifdef PVPROF
#define free_node(node) {\
     /*printf("Freeing node at %x\n",node);*/\
     Sibl(node) = free_trie_nodes;\
     free_trie_nodes = node;\
     freenodes++;\
}
#else
#define free_node(node) {\
     /*printf("Freeing node at %x\n",node);*/\
     Sibl(node) = free_trie_nodes;\
     free_trie_nodes = node;\
}
#endif

#define free_anslistnode(node) {\
     /*printf("Freeing node at %x\n",node);*/\
     aln_next_aln(node) = free_answer_list_nodes;\
     free_answer_list_nodes = node;\
}



#define push_node(node){\
     struct freeing_stack_node *temp;\
     temp = (struct freeing_stack_node *)malloc(sizeof(struct freeing_stack_node));\
     if(temp == NULL){\
         printf("Out of Memory\n");\
         return;\
     }\
     else{\
          temp->next   = node_stk_top;\
          temp->item   = node;\
          node_stk_top = temp;\
     }\
}

#define pop_node(node){\
    struct freeing_stack_node *temp;\
    if(node_stk_top == NULL){\
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
void delete_table_trie(NODEptr x){

  struct freeing_stack_node *node_stk_top = 0, *call_nodes_top = 0;  

  NODEptr *Bkp; /*was bucketptr */
  struct HASHhdr *thdr;
  NODEptr node, rnod; 

  push_node(x);
  while (node_stk_top != 0) {
    pop_node(node);
    if (Sibl(node) == (NODEptr)-1) { /* hash node */
      thdr = ((struct HASHhdr *)Child(node)) - 1;
      for (Bkp = (NODEptr *)Child(node);Bkp <= (NODEptr *)Child(node)+(thdr->HASHmask);Bkp++) {
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
		for(Bkp=(NODEptr *)Child(rnod);Bkp <= (NODEptr *)Child(rnod)+(thdr->HASHmask);Bkp++) {
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

/***************************************************************************/

/*________________________________________________*/
static int is_hash(NODEptr x) 
{
  if( x == NULL)
    return(0);
  else
    return((Sibl(x) == (NODEptr) -1));
}
/*________________________________________________*/
static NODEptr gParent(NODEptr y)
{
  if(ftagged(Parent(y)))
    return(unftag(Parent(y)));
  else   
    return(Parent(y));
}
/*________________________________________________*/	
static NODEptr *parents_childptr(NODEptr y, CPtr hook)
{

  NODEptr x;
  if((x =gParent(y)) == NULL)
    return((NODEptr *)hook);
  else
    return(&(Child(x)));
}
/*________________________________________________*/
static NODEptr *get_headptr_of_list(NODEptr x, Cell Item)
{
  NODEptr *z;

  struct HASHhdr *hh;
  z = (NODEptr *)Child(x);
  hh = (struct HASHhdr *)z -1;
  return(z + HASH(Item,hh->HASHmask));
  
}
/*________________________________________________*/	
static int decr_num_in_hashhdr(NODEptr y)
{
  return((--  ((struct HASHhdr *)Child(y) -1)->numInHash) + 1);
}
/*________________________________________________*/
static NODEptr get_prev_sibl(NODEptr y, CPtr hook)
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
/* Old error message: (stderr, "Node %x not a child of its parent\n", (int)y); */

}
/*________________________________________________*/
static void free_hash_hdr(struct HASHhdr *hh)
{
  if(hh-> prev != NULL)
    hh -> prev-> next = hh->next;
  if(hh->next != NULL)
    hh->next->prev = hh->prev;
  free(hh);
}
/*________________________________________________*/

static void free_pointed_hash_hdr(NODEptr x)
{  
  free_hash_hdr((struct HASHhdr *)Child(x) - 1);
}
/*________________________________________________*/
/* deletes and reclaims a whole branch in the return trie */
int delete_branch(NODEptr lowest_node_in_branch, CPtr hook)
{
  int num_left_in_hash;
  NODEptr prev,parent_ptr, *y1, *z;
    
  while((lowest_node_in_branch != NULL) && (is_no_cp(lowest_node_in_branch))){
    parent_ptr = gParent(lowest_node_in_branch);   
    y1 = parents_childptr(lowest_node_in_branch,hook);
    if(is_hash(*y1)){
      z = get_headptr_of_list(*y1,Atom(lowest_node_in_branch));
      *z = NULL;
      num_left_in_hash = decr_num_in_hashhdr(*y1);
      if(num_left_in_hash  > 0) {
	mark_leaf_node_del(lowest_node_in_branch); /* mark node as deleted */
	free_node(lowest_node_in_branch);
	return(0); /* like a try or a retry or trust node with siblings */ 
      }
      else
	free_pointed_hash_hdr(*y1);
    }
    mark_leaf_node_del(lowest_node_in_branch); /* mark node as deleted */
    free_node(lowest_node_in_branch);
    lowest_node_in_branch = parent_ptr;
  }
  if(lowest_node_in_branch == NULL){
    *hook = 0;
  }
  else{
    if(is_try(lowest_node_in_branch)){
      Instr(Sibl(lowest_node_in_branch)) = Instr(Sibl(lowest_node_in_branch)) -1;/* trust -> no_cp  retry -> try */
      y1 = parents_childptr(lowest_node_in_branch,hook);
      if(is_hash(*y1)){
	z = get_headptr_of_list(*y1,Atom(lowest_node_in_branch));
	num_left_in_hash =decr_num_in_hashhdr(*y1);
      }
      else
	z = y1;
      *z =Sibl(lowest_node_in_branch);      
    }
    else{ 
      prev = get_prev_sibl(lowest_node_in_branch,hook);      
      Sibl(prev) = Sibl(lowest_node_in_branch);
      if (is_trust(lowest_node_in_branch)){
	Instr(prev) -= 2; /* retry -> trust ; try -> nocp */
      }
    }
    free_node(lowest_node_in_branch);
  }
  return(0);
}

void safe_delete_branch(NODEptr lowest_node_in_branch)
{
  byte choicepttype;

  choicepttype = 0x3 & Instr(lowest_node_in_branch);
  Instr(lowest_node_in_branch) = choicepttype | 0x90; 
  mark_leaf_node_del(lowest_node_in_branch); /* mark node as deleted */
  Atom(lowest_node_in_branch) = Atom(lowest_node_in_branch) ^ 0x100000;

/* Here I am assuming that the HASH mask is < 0x100000 (65536)
   if it is not, the node will hash into another bucket, resulting  
   in inappropriate behavior on deletion */

}



/* This does not reclaim space for deleted nodes, only marks
 * the node as deleted (setting the del_flag), and change the
 * try instruction to fail.
 * The deleted node is then linked into the del_nodes_list
 * in the completion stack.
 */
void delete_return(NODEptr l, CPtr hook) 
{
  ALPtr a;
  SGFrame sg_frame;
  ALPtr  n, next;
  NLChoice c;
#ifdef LOCAL_EVAL
  TChoice  tc;
#endif


#ifdef DEBUG_RECLAIM_DEL_RET
  fprintf(stderr,"Delete node: %d - Par: %d\n",l,
	  Parent(l));
#endif
  sg_frame = (SGFrame) hook;
  safe_delete_branch(l);
  if(!is_completed(sg_frame)){
    n = subg_ans_list_ptr(sg_frame);
    /* Find previous sibling -pvr */
    while(aln_answer_ptr(aln_next_aln(n)) != l){
      n  = aln_next_aln(n);  /* if a is not in that list a core dump will result */
    }
    if(n == NULL){
      xsb_exit("Error in delete answer\n");
    }
    a               = aln_next_aln(n);
    next            = aln_next_aln(a);
    aln_next_aln(a) = compl_del_ret_list(subg_compl_stack_ptr(sg_frame));
    compl_del_ret_list(subg_compl_stack_ptr(sg_frame)) =a;    

    aln_next_aln(n) = next;
    
    /* Make active nodes point to previous sibling
       if they point to deleted answer -pvr  */
    c = (NLChoice) subg_asf_list_ptr(sg_frame);
    while(c != NULL){
      if(nlcp_trie_return(c) == a){
	nlcp_trie_return(c) = n;
      }
      c = (NLChoice)nlcp_prevlookup(c);
    }

#ifdef LOCAL_EVAL
      /* if gen-cons points to deleted answer, make it
       * point to previous sibling */
      tc = (TChoice)subg_cp_ptr(sg_frame);
      if(tcp_trie_return(tc) == a) {
	tcp_trie_return(tc) = n;
      }
#endif
   
    aln_next_aln(n) = next;
    if(next == NULL){ /* last answer */
      subg_ans_list_tail(sg_frame) = n;
    }      
  }
}

/* Given a tabled subgoal, go through its list of deleted nodes (in the
 * completion stack), and reclaim the leaves and corresponding branches
 */
/**************************************************/
void  reclaim_del_ret_list(SGFrame sg_frame) {
  ALPtr x,y;
  
  x = compl_del_ret_list(subg_compl_stack_ptr(sg_frame));
  
  while(x != NULL){
    y = x;
    x = aln_next_aln(x);
    delete_branch(aln_answer_ptr(y),(CPtr)&subg_ans_root_ptr(sg_frame));
    free_anslistnode(y);
  }
}
/**************************************************/
 
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
 


/**************************************************/
/*
 * NOTE:  The actual breg value is being passed, NOT just an offset!
 */
void aux_breg_retskel(void)
{
  prolog_term retterm;
  CPtr var_ctr, t_breg, cptr;
  Psc psc_ptr;
  int Arity, Nvars, i;

  retterm = ptoc_tag(1);
  t_breg = (CPtr)ptoc_int(2);
  Arity = ptoc_int(3);
  Nvars = ptoc_int(4);
  ctop_int(5,(Integer)tcp_subgoal_ptr((TChoice)t_breg));
  if(Nvars != 0){
    var_ctr = t_breg +( TCP_SIZE +Arity +Nvars);
/* Now with the retskel */
    deref(retterm);
    if(cell_tag(retterm) != CS){
      xsb_abort("Non cs tag in retterm");
    }
    else {
      psc_ptr = get_str_psc(retterm);
      cptr = (CPtr)cs_val(retterm);
      for(i = 0; i < Nvars; i++){
	*(cptr +i +1) = *(CPtr)(var_ctr -i);
      }
    }
  }
}


/***************************************************/

NODEptr *Set_ArrayPtr = NULL;
int Set_ArraySz = 100;
int num_sets = 1;


void init_newtrie(){
  Set_ArrayPtr  = (NODEptr *)calloc(Set_ArraySz ,sizeof(NODEptr));
}


void newtrie()
{
  int i;


  if(Set_ArraySz == num_sets){
    NODEptr *temp_arrayptr;
    
    temp_arrayptr = Set_ArrayPtr;
    Set_ArraySz   +=  100;
    Set_ArrayPtr  = (NODEptr *)calloc(Set_ArraySz ,sizeof(NODEptr));
    /* A 100 more sets */
    if(Set_ArrayPtr == NULL){
      xsb_exit("Out of memory in Newtrie/1\n");
    }
    for(i = 0; i < num_sets; i++){
      Set_ArrayPtr[i] = temp_arrayptr[i];
    }
    free(temp_arrayptr); 
  }
  ctop_int(1,num_sets);
  num_sets ++;
}

/***************************************************/
void trie_intern()
{
  prolog_term term;
  long RootIndex;
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
/***************************************************/
int trie_interned()
{
  long RootIndex;
  int ret_val = FALSE;
  Cell Leafterm, trie_term;
  trie_term =  ptoc_tag(1);
  RootIndex = ptoc_int(2);
  Leafterm = ptoc_tag(3);
  
  if(Set_ArrayPtr[RootIndex] != NULL){
    deref(trie_term);
    deref(Leafterm);
    if(isref(Leafterm)){  
      reg_arrayptr = reg_array  -1;
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
/***************************************************/
void trie_dispose()
{
  NODEptr Leaf;
  long Rootidx;

  switch_to_trie_assert;
  Rootidx = ptoc_int(1);
  Leaf = (NODEptr)ptoc_int(2);
  delete_branch(Leaf,(CPtr)&(Set_ArrayPtr[Rootidx]));
  switch_from_trie_assert;
  
}
/***************************************************/
void clear_interned_tries()
{
  int i;

  num_sets = 1;
  
  for(i = 0; i < Set_ArraySz; i++)
    Set_ArrayPtr  = NULL;

}
/***************************************************/
#define isleaf(x) ((Instr(x) == trie_proceed) || ftagged(Parent(x)))

void delete_all_buckets(NODEptr hashnode){
  NODEptr *Arrayptr;
  struct HASHhdr *hh;
  int i;

    
  Arrayptr = (NODEptr *)Child(hashnode);
  hh       = (struct HASHhdr *)Arrayptr -1;
  for( i = 0; i <= hh-> HASHmask; i++){
    delete_trie(Arrayptr[i]);
  }
  free_hash_hdr(hh);
}
/***************************************************/
void delete_trie(NODEptr root){
  NODEptr sib, chil;
  
  
  if(root != NULL){
    if(is_hash(root)){
      delete_all_buckets(root);
    }
    else{
      sib  = Sibl(root);
      chil = Child(root);      
    /* Child nodes == NULL is not the correct test*/
      if(isleaf(root)){
	if(chil != NULL)
	  printf("Anomaly!\n");
      }

      if(!isleaf(root))
	delete_trie(chil);
      delete_trie(sib);	
      
    }
    free_node(root);
    }
}



void free_node_function(NODEptr n)
{
 free_node(n); 
}

