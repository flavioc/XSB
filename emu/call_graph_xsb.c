/* File:      call_graph_xsb.c
** Author(s): Diptikalyan Saha, C. R. Ramakrishnan
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
** $$
** 
*/
 

#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Special debug includes */
/* Remove the includes that are unncessary !!*/
#include "hashtable.h"
#include "hashtable_itr.h"
#include "debugs/debug_tries.h"
#include "auxlry.h"
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
#include "subp.h"
#include "cinterf.h"
#include "error_xsb.h"
#include "tr_utils.h"
#include "hash_xsb.h" 
#include "cell_xsb.h"
#include "call_xsb_i.h"
#include "tst_aux.h"
#include "tst_utils.h"
#include "token_xsb.h"
#include "call_graph_xsb.h"
#include "thread_xsb.h"
#include "loader_xsb.h" /* for ZOOM_FACTOR, used in stack expansion */
#include "tries.h"
#include "tr_utils.h"

#define INCR_DEBUG2


/********************** STATISTICS *****************************/
#define build_subgoal_args(SUBG)	\
	load_solution_trie(CTXTc arity, 0, &cell_array1[arity-1], subg_leaf_ptr(SUBG))



int cellarridx;
int maximum_dl=0;

//callnodeptr callq[20000000];
//int callqptr=0;
calllistptr affected=NULL,changed=NULL;
int no_add_call_edge=0;
callnodeptr old_call=NULL;
int saved_call=0,unchanged_call=0,call_count=0;
int call_node_count=0,call_edge_count=0;
int factcount=0;

static Cell cell_array1[500];

call2listptr marked_list=NULL; /* required for abolishing incremental calls */ 
calllistptr assumption_list=NULL;  /* required for abolishing incremental calls */ 

BTNptr old_answer_table=NULL;

void incr_eval_statistics(CTXTdecl){
  
	

}


Structure_Manager smCallNode  =  SM_InitDecl(CALL_NODE,CALLNODE_PER_BLOCK,"CallNode");
Structure_Manager smCallList  =  SM_InitDecl(CALLLIST,CALLLIST_PER_BLOCK,"CallList");
Structure_Manager smCall2List =  SM_InitDecl(CALL2LIST,CALL2LIST_PER_BLOCK,"Call2List");
Structure_Manager smKey	      =  SM_InitDecl(KEY,KEY_PER_BLOCK,"HashKey");
Structure_Manager smOutEdge   =  SM_InitDecl(OUTEDGE,OUTEDGE_PER_BLOCK,"Outedge");



DEFINE_HASHTABLE_INSERT(insert_some, KEY, CALL_NODE);
DEFINE_HASHTABLE_SEARCH(search_some, KEY, callnodeptr);
DEFINE_HASHTABLE_REMOVE(remove_some, KEY, callnodeptr);
DEFINE_HASHTABLE_ITERATOR_SEARCH(search_itr_some, KEY);


/*****************************************************************************/
static unsigned int hashfromkey(void *ky)
{
    KEY *k = (KEY *)ky;
    return (int)(k->goal);
}

static int equalkeys(void *k1, void *k2)
{
    return (0 == memcmp(k1,k2,sizeof(KEY)));
}



/*****************************************************************************/

void printcall(callnodeptr c){
  //  printf("%d",c->id);
  if(IsNonNULL(c->goal))
    sfPrintGoal(stdout,c->goal,NO);
  else
    printf("fact");
  return;
}

/******************** GENERATION OF CALLED_BY GRAPH ********************/

/* Creates a call node */

callnodeptr makecallnode(VariantSF sf){
  
  callnodeptr cn;

  SM_AllocateStruct(smCallNode,cn);
  cn->deleted = 0;
  cn->changed = 0;
  cn->no_of_answers = 0;
  cn->falsecount = 0;
  cn->prev_call = NULL;
  cn->aln = NULL;
  cn->inedges = NULL;
  cn->goal = sf;
  cn->outedges=NULL;
  cn->id=call_count++; 
  cn->outcount=0;

#ifdef INCR_DEBUG1
  printcall(cn);printf("\n");
#endif


  call_node_count++;

  return cn;
}


void deleteinedges(callnodeptr callnode){
  calllistptr tmpin,in;
  
  KEY *ownkey;
  struct hashtable* hasht;
  SM_AllocateStruct(smKey, ownkey);
  ownkey->goal=callnode->id;	
	
  in = callnode->inedges;
  
  while(IsNonNULL(in)){
    tmpin = in->next;
    hasht = in->prevnode->hasht;
    if (remove_some(hasht,ownkey) == NULL) {
      xsb_abort("BUG: key not found for removal\n");
    }
    call_edge_count--;
    SM_DeallocateStruct(smCallList, in);      
    in = tmpin;
  }
  SM_DeallocateStruct(smKey, ownkey);      
  return;
}

void deletecallnode(callnodeptr callnode){


  call_node_count--;
   
  if(callnode->outcount==0){
    hashtable1_destroy(callnode->outedges->hasht,0);
    SM_DeallocateStruct(smOutEdge, callnode->outedges);      
    SM_DeallocateStruct(smCallNode, callnode);        
  }else
    xsb_abort("outcount is nonzero\n");
  
  return;
}







void initoutedges(callnodeptr cn){

	
  outedgeptr out;

#ifdef INCR_DEBUG
	printf("Initoutedges %d\n",cn->id);
#endif

  SM_AllocateStruct(smOutEdge,out);
  cn->outedges = out;
  out->callnode = cn; 	  
  out->hasht =create_hashtable1(HASH_TABLE_SIZE, hashfromkey, equalkeys);
  return;
}


void deallocatecall(callnodeptr callnode){
  
  calllistptr tmpin,in;
  
  KEY *ownkey;
  /*  callnodeptr  prevnode; */
  struct hashtable* hasht;
  SM_AllocateStruct(smKey, ownkey);
  ownkey->goal=callnode->id;	
	
  in = callnode->inedges;
  call_node_count--;
  
  while(IsNonNULL(in)){
    tmpin = in->next;
    hasht = in->prevnode->hasht;
    if (remove_some(hasht,ownkey) == NULL) {
      /*
      prevnode=in->prevnode->callnode;
      if(IsNonNULL(prevnode->goal)){
	sfPrintGoal(stdout,(VariantSF)prevnode->goal,NO); printf("(%d)",prevnode->id);
      }
      if(IsNonNULL(callnode->goal)){
	sfPrintGoal(stdout,(VariantSF)callnode->goal,NO); printf("(%d)",callnode->id);
      }
      */
      xsb_abort("BUG: key not found for removal\n");
    }
    in->prevnode->callnode->outcount--;
    call_edge_count--;
    SM_DeallocateStruct(smCallList, in);      
    in = tmpin;
  }
  
  SM_DeallocateStruct(smCallNode, callnode);      
  SM_DeallocateStruct(smKey, ownkey);      
}

/*
propagate_no_change(c)
	for c->c'
	  if(c'.deleted=false)
	     c'->falsecount>0
	       c'->falsecount--
	       if(c'->falsecount==0)
		 propagate_no_change(c')		
*/

/*

When invalidation is done a parameter 'falsecount' is maintained with
each call which signifies that these many predecessor calls have been
affected. So if a call A has two pred node B and C and both of them
are affected then A's falsecount is 2. Now when B is reevaluated and
turns out it has not been changed (its old and new answer table is
same) completion of B calls propagate_no_change(B) which reduces the
falsecount of A by 1. If for example turns out that C was also not
changed falsecount of A is going to be reduced to 0. Now when call A
is executed it's just going to do answer clause resolution.

*/


void propagate_no_change(callnodeptr c){
  callnodeptr cn;
  struct hashtable *h;	
  struct hashtable_itr *itr;
  if(IsNonNULL(c)){
    h=c->outedges->hasht;
    itr = hashtable1_iterator(h);
    if (hashtable1_count(h) > 0){
      do {
	cn= hashtable1_iterator_value(itr);
	if(cn->falsecount>0){ /* this check is required for the new dependencies that can arise bcoz of the re-evaluation */
	  cn->falsecount--;
	  if(cn->falsecount==0){
	    cn->deleted = 0;
	    propagate_no_change(cn);
	  }
	}
      } while (hashtable1_iterator_advance(itr));
    }
  }		
}


/* Enter a call to calllist */

static void inline ecall(calllistptr *list, callnodeptr item){
  calllistptr  temp;
  SM_AllocateStruct(smCallList,temp);
  temp->item=item;
  temp->next=*list;
  *list=temp;  
}

static void inline ecall2(calllistptr *list, outedgeptr item){
  calllistptr  temp;
  SM_AllocateStruct(smCallList,temp);
  temp->prevnode=item;
  temp->next=*list;
  *list=temp;  
}

static void inline ecall3(calllistptr *list, call2listptr item){
  calllistptr  temp;
  SM_AllocateStruct(smCallList,temp);
  temp->item2=item;
  temp->next=*list;
  *list=temp;  
}





void addcalledge(callnodeptr fromcn, callnodeptr tocn){
  
  KEY *k1;
  SM_AllocateStruct(smKey, k1);
  k1->goal = tocn->id;
  
#ifdef INCR_DEBUG
  printf("%d-->%d",fromcn->id,tocn->id);	
#endif
  
  
  if (NULL == search_some(fromcn->outedges->hasht,k1)) {
    insert_some(fromcn->outedges->hasht,k1,tocn);

#ifdef INCR_DEBUG	
    printf("Inedges of %d = ",tocn->id);
    temp=tocn->inedges;
    while(temp!=NULL){
      printf("\t%d",temp->prevnode->callnode->id);
      temp=temp->next;
    }
    printf("\n");
#endif
    
    ecall2(&(tocn->inedges),fromcn->outedges);      
    call_edge_count++;
    fromcn->outcount++;
    
#ifdef INCR_DEBUG		
    if(IsNonNULL(fromcn->goal)){
      sfPrintGoal(stdout,(VariantSF)fromcn->goal,NO);printf("(%d)",fromcn->id);
    }else
      printf("(%d)",fromcn->id);
    
    if(IsNonNULL(tocn->goal)){
      printf("-->");	
      sfPrintGoal(stdout,(VariantSF)tocn->goal,NO);printf("(%d)",tocn->id);
    }
    printf("\n");	
#endif
    
    
  }

#ifdef INCR_DEBUG
	printf("Inedges of %d = ",tocn->id);
	temp=tocn->inedges;
	while(temp!=NULL){
		printf("\t%d",temp->prevnode->callnode->id);
		temp=temp->next;
	}
	printf("\n---------------------------------\n");
#endif
}

#define EMPTY NULL

calllistptr eneetq(){ 
  
  calllistptr  temp;
  SM_AllocateStruct(smCallList,temp);
  temp->item = (callnodeptr)EMPTY;
  temp->next = NULL;

  return temp;
}


void nq(calllistptr *cl,callnodeptr c){
  ecall(cl,c);
}

callnodeptr dq(calllistptr *cl){   
  
  calllistptr tmp;
  callnodeptr c;
  
  c = (*cl)->item;
  tmp = *cl;
  *cl = (*cl)->next;
  SM_DeallocateStruct(smCallList, tmp);      
  
  return c;  
}


void dfs(callnodeptr call1){
  callnodeptr cn;
  struct hashtable *h;	
  struct hashtable_itr *itr;
  
  if(IsNonNULL(call1->goal) && !subg_is_complete((VariantSF)call1->goal)){
    xsb_warn("Incompleted tabled is invalidated! Unexpected result will occur\n");    
  }
  call1->deleted = 1;
  h=call1->outedges->hasht;
  
  itr = hashtable1_iterator(h);       
  if (hashtable1_count(h) > 0){
    do {
      cn = hashtable1_iterator_value(itr);
      cn->falsecount++;
      if(cn->deleted==0)
	dfs(cn);
    } while (hashtable1_iterator_advance(itr));
  }
  nq(&affected,call1);		
}


void invalidate_call(callnodeptr c){

#ifdef MULTI_THREAD
  xsb_abort("Incremental Maintenance of tables in not available for multithreaded engine\n");
#endif

  if(c->deleted==0){
    c->falsecount++;
    dfs(c);
  }
}


int create_call_list(CTXTdecl){

  callnodeptr call1;
  VariantSF subgoal;
  TIFptr tif;
  int j,count=0,arity;
  Psc psc;
  CPtr oldhreg=NULL, head;
  Cell term;
  
  reg[3] = makelist(hreg);  // reg 3 first not-used, use reg in case of stack expanson
  while((call1 = dq(&affected)) != EMPTY){
    subgoal = (VariantSF) call1->goal;      
    if(IsNULL(subgoal)){ /* fact predicates */
      call1->deleted = 0; 
      continue;
    }
    count++;
    tif = (TIFptr) subgoal->tif_ptr;
    psc = TIF_PSC(tif);
    arity = get_arity(psc);
    check_glstack_overflow(3,pcreg,2+arity*200); // don't know how much for build_subgoal_args...
    oldhreg = hreg;
    if(arity>0){
      head = hreg+2;
      sreg = head;
      follow(oldhreg++) = term = makecs(head);
      hreg += arity + 3;  // had 10, why 10?  why not 3? 2 for list, 1 for functor (dsw)
      new_heap_functor(sreg, psc);
      for (j = 1; j <= arity; j++) {
	new_heap_free(sreg);
	cell_array1[arity-j] = cell(sreg-1);
      }
      build_subgoal_args(subgoal);		
    }else{
      follow(oldhreg++) = makestring(get_name(psc));
      hreg += 2;	
    }
    follow(oldhreg) = makelist(hreg);
  }
  if(count > 0)
    follow(oldhreg) = makenil;
  else
    reg[3] = makenil;
    
  //  printterm(stdout,call_list,100);

  return unify(CTXTc reg_term(CTXTc 2),reg_term(CTXTc 3));

  /*
    int i;
    for(i=0;i<callqptr;i++){
      if(IsNonNULL(callq[i]) && (callq[i]->deleted==1)){
    sfPrintGoal(stdout,(VariantSF)callq[i]->goal,NO);
    printf(" %d %d\n",callq[i]->falsecount,callq[i]->deleted);
    }
    }
  printf("-----------------------------\n");
  */
}




int create_changed_call_list(CTXTdecl){
  callnodeptr call1;
  VariantSF subgoal;
  TIFptr tif;
  int j, count = 0,arity;
  Psc psc;
  CPtr oldhreg = NULL, head;
  Cell term;

  reg[3] = makelist(hreg);
  while ((call1 = dq(&changed)) != EMPTY){
    count++;
    subgoal = (VariantSF) call1->goal;      
    tif = (TIFptr) subgoal->tif_ptr;
    psc = TIF_PSC(tif);
    arity = get_arity(psc);
    check_glstack_overflow(3,pcreg,2+arity*200); // don't know how much for build_subgoal_args...
    oldhreg = hreg;
    if(arity>0){
      head = hreg+2;
      sreg = head;
      follow(oldhreg++) = term = makecs(head);
      hreg += arity + 3;
      new_heap_functor(sreg, psc);
      for (j = 1; j <= arity; j++) {
	new_heap_free(sreg);
	cell_array1[arity-j] = cell(sreg-1);
      }
      build_subgoal_args(subgoal);		
    }else{
      follow(oldhreg++) = makestring(get_name(psc));
      hreg+=2;	
    }
    follow(oldhreg) = makelist(hreg);
  }
  if (count>0)
    follow(oldhreg) = makenil;
  else
    reg[3] = makenil;
    
 
  return unify(CTXTc reg_term(CTXTc 2),reg_term(CTXTc 3));

  /*
    int i;
    for(i=0; i<callqptr; i++){
      if(IsNonNULL(callq[i]) && (callq[i]->deleted==1)){
    sfPrintGoal(stdout,(VariantSF)callq[i]->goal,NO);
    printf(" %d %d\n",callq[i]->falsecount,callq[i]->deleted);
    }
    }
  printf("-----------------------------\n");
  */
}


int imm_depend_list(CTXTdeclc callnodeptr call1){


 
  VariantSF subgoal;
  TIFptr tif;
  int j, count = 0,arity;
  Psc psc;
  CPtr oldhreg = NULL, head;
  Cell term;
  struct hashtable *h;	
  struct hashtable_itr *itr;
  callnodeptr cn;
    
  reg[4] = makelist(hreg);
  
  if(IsNonNULL(call1)){ /* This can be called from some non incremental predicate */
    h=call1->outedges->hasht;
    
    itr = hashtable1_iterator(h);       
    if (hashtable1_count(h) > 0){
      do {
	cn = hashtable1_iterator_value(itr);
	if(IsNonNULL(cn->goal)){
	  count++;
	  subgoal = (VariantSF) cn->goal;      
	  tif = (TIFptr) subgoal->tif_ptr;
	  psc = TIF_PSC(tif);
	  arity = get_arity(psc);
	  check_glstack_overflow(4,pcreg,2+arity*200); // don't know how much for build_subgoal_args...
	  oldhreg=hreg;
	  if(arity>0){
	    head = hreg+2;
	    sreg = head;
	    follow(oldhreg++) = term = makecs(head);
	    hreg += arity + 3;
	    new_heap_functor(sreg, psc);
	    for (j = 1; j <= arity; j++) {
	      new_heap_free(sreg);
	      cell_array1[arity-j] = cell(sreg-1);
	    }
	    build_subgoal_args(subgoal);		
	  }else{
	    follow(oldhreg++) = makestring(get_name(psc));
	    hreg += 2;	
	  }
	  follow(oldhreg) = makelist(hreg);
	}
      } while (hashtable1_iterator_advance(itr));
    }
    
    
    
    if (count>0)
      follow(oldhreg) = makenil;
    else
      reg[4] = makenil;
  }else{
    xsb_warn("Called with non-incremental predicate\n");
    reg[4] = makenil;
  }

  //  printterm(stdout,call_list,100);
  return unify(CTXTc reg_term(CTXTc 3),reg_term(CTXTc 4));
}



/*

Finds for a callnode call1 the list of callnode on which call1
depends.


*/


int imm_dependent_on_list(CTXTdeclc callnodeptr call1){

  VariantSF subgoal;
  TIFptr tif;
  int j, count = 0,arity;
  Psc psc;
  CPtr oldhreg = NULL, head;
  Cell term;
  calllistptr cl;
  reg[4] = makelist(hreg);
  
  if(IsNonNULL(call1)){ /* This can be called from some non incremental predicate */
    cl= call1->inedges;
    
    
    while(IsNonNULL(cl)){
      subgoal = (VariantSF) cl->prevnode->callnode->goal;    
      if(IsNonNULL(subgoal)){/* fact check */
	count++;
	tif = (TIFptr) subgoal->tif_ptr;
	psc = TIF_PSC(tif);
	arity = get_arity(psc);
		check_glstack_overflow(4,pcreg,2+arity*200); // don't know how much for build_subgoal_args...
		oldhreg = hreg;
		if(arity>0){
			head = hreg+2;
			sreg = head;
			follow(oldhreg++) = term = makecs(head);
			hreg += arity + 3;
			new_heap_functor(sreg, psc);
			for (j = 1; j <= arity; j++) {
				new_heap_free(sreg);
				cell_array1[arity-j] = cell(sreg-1);
			}		
			build_subgoal_args(subgoal);		
		}else{
			follow(oldhreg++) = makestring(get_name(psc));
			hreg += 2;	
		}
		follow(oldhreg) = makelist(hreg);
      }
      cl=cl->next;
    }

  
    if (count>0)
      follow(oldhreg) = makenil;
    else
      reg[4] = makenil;
  }else{
    xsb_warn("Called with non-incremental predicate\n");
    reg[4] = makenil;
  }
  return unify(CTXTc reg_term(CTXTc 3),reg_term(CTXTc 4));
}


void print_call_node(callnodeptr call1){
  // not implemented
}


void print_call_graph(){
	// not implemented
}


/* Abolish Incremental Calls: 

To abolish a call for an incremental predicate it requires to check
whether any call is dependent on this call or not. If there are calls
that are dependent on this call (not cyclically) then this call should
not be deleted. Otherwise this call is deleted and the calls that are
supporting this call will also be checked for deletion. So deletion of
an incremental call can have a cascading effect.  

As cyclicity check has to be done we have a two phase algorithm to
deal with this problem. In the first phase we mark all the calls that
can be potentially deleted. In the next phase we unmarking the calls -
which should not be deleted. 

*/

void mark(callnodeptr);
void check_assumption_list(void);
void delete_calls(CTXTdecl);
call2listptr create_cdbllist(void);

void abolish_incr(CTXTdeclc callnodeptr p){


  marked_list=create_cdbllist();

#ifdef INCR_DEBUG1
  printf("marking phase starts\n");
#endif
  
  mark(p);
  check_assumption_list();
#ifdef INCR_DEBUG1
  printf("assumption check ends \n");
#endif

  delete_calls(CTXT);


#ifdef INCR_DEBUG1
  printf("delete call ends\n");
#endif
}



/* Double Linked List functions */

call2listptr create_cdbllist(void){
  call2listptr l;
  SM_AllocateStruct(smCall2List,l);
  l->next=l->prev=l;
  l->item=NULL;
  return l;    
}

call2listptr insert_cdbllist(call2listptr cl,callnodeptr n){
  call2listptr l;
  SM_AllocateStruct(smCall2List,l);
  l->next=cl->next;
  l->prev=cl;
  l->item=n;    
  cl->next=l;
  l->next->prev=l;  
  return l;
}

void delete_callnode(call2listptr n){
  n->next->prev=n->prev;
  n->prev->next=n->next;

  return;
}

/*

Input: takes a callnode Output: puts the callnode to the marked list
and sets the deleted bit; puts it to the assumption list if it has any
dependent calls not marked 

*/

void mark(callnodeptr c){
  calllistptr in=c->inedges;
  call2listptr markedlistptr;
  callnodeptr c1;

#ifdef INCR_DEBUG1 
      printf("marking ");printcall(c);printf("\n");
#endif

  
  c->deleted=1;
  markedlistptr=insert_cdbllist(marked_list,c);
  if(c->outcount)
    ecall3(&assumption_list,markedlistptr);
    
  while(IsNonNULL(in)){
    c1=in->prevnode->callnode;
    c1->outcount--;
    if(c1->deleted==0){
      mark(c1);
    }
    in=in->next;
  }  
  return;
}


void delete_calls(CTXTdecl){

  call2listptr  n=marked_list->next,temp;
  callnodeptr c;
  VariantSF goal;

  /* first iteration to delete inedges */
  
  while(n!=marked_list){    
    c=n->item;
    if(c->deleted){
      /* facts are not deleted */       
      if(IsNonNULL(c->goal)){
	deleteinedges(c);
      }
    }
    n=n->next;
  }

  /* second iteration is to delete outedges and callnode */

  n=marked_list->next;
  while(n!=marked_list){    
    temp=n->next;
    c=n->item;
    if(c->deleted){
      /* facts are not deleted */       
      if(IsNonNULL(c->goal)){

	goal=c->goal;
	deletecallnode(c);
	
	abolish_table_call(CTXTc goal);
      }
    }
    SM_DeallocateStruct(smCall2List,n);
    n=temp;
  }
  
  SM_DeallocateStruct(smCall2List,marked_list);
  marked_list=NULL;
  return;
}




void unmark(callnodeptr c){
  callnodeptr c1;
  calllistptr in=c->inedges;

#ifdef INCR_DEBUG1
      printf("unmarking ");printcall(c);printf("\n");
#endif
  
  c->deleted=0;
  while(IsNonNULL(in)){
    c1=in->prevnode->callnode;
    c1->outcount++;
    if(c1->deleted)
      unmark(c1);
    in=in->next;
  }  
  
  return;
}




void check_assumption_list(void){
  calllistptr tempin,in=assumption_list;
  call2listptr marked_ptr;
  callnodeptr c;

  while(in){
    tempin=in->next;
    marked_ptr=in->item2;
    c=marked_ptr->item;
    if(c->outcount>0){
      delete_callnode(marked_ptr);
      SM_DeallocateStruct(smCall2List,marked_ptr);

#ifdef INCR_DEBUG1
      printf("in check assumption ");printcall(c);printf("\n");
#endif
      
      if(c->deleted)   
	unmark(c);      
    }
    SM_DeallocateStruct(smCallList, in);      
    in=tempin;
  }
  
  
  assumption_list=NULL;
  return;
}


