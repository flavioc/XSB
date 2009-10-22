/* File:      tr_code_xsb_i.h
** Author(s): Prasad Rao, Kostis Sagonas, Baoqiu Cui
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


#define opatom Atom(NodePtr)
#define opsucc ((byte *)(Child(NodePtr)))
#define opfail ((byte *)(Sibl(NodePtr)))

#define FIRST_HASH_NODE  -1
#define NO_MORE_IN_HASH  -2
#define HASH_IS_FREE	 -3
#define HASH_IS_NOT_FREE -4

/*----------------------------------------------------------------------*/

/* These are used only in instruction "hash_handle"
   ------------------------------------------------ */
/*
 *  Calculate the bucket number in which Subterm would be located,
 *  should it exist in the trie.
 */
#define hash_nonvar_subterm(Subterm, pBTHT, BucketNum) {	\
								\
   Cell symbol = 0;	/* eliminate compiler warning */	\
								\
   switch (cell_tag(Subterm)) {					\
   case XSB_STRING:						\
   case XSB_INT:						\
   case XSB_FLOAT:	      	  				\
     symbol = EncodeTrieConstant(Subterm);			\
     break;							\
   case XSB_LIST:						\
     symbol = EncodeTrieList(Subterm);				\
     break;							\
   case XSB_STRUCT:						\
     symbol = EncodeTrieFunctor(Subterm);			\
     break;							\
   default:							\
     fprintf(stderr,"Bad tag :Type %ld ",cell_tag(Subterm));	\
     xsb_exit(CTXTc "In instruction hash_handle");		\
     break;							\
   }								\
   BucketNum = TrieHash(symbol,BTHT_GetHashSeed(pBTHT));	\
 }

#define find_next_nonempty_bucket(pBTHT, pTable, BucketNum) {	\
   long TableSize = BTHT_NumBuckets(pBTHT);			\
								\
   while (TRUE) {						\
     BucketNum++;						\
     if (BucketNum >= TableSize) {				\
       BucketNum = NO_MORE_IN_HASH;				\
       break;							\
     }								\
     else if ( IsNonNULL(*(pTable + BucketNum)) )		\
       break;							\
   }								\
 }

/*----------------------------------------------------------------------*/

/*
 * Decide how to proceed from current node.  Used in variable-containing
 * nodes since it is unclear from the context (embedded instruction)
 * whether we are at a leaf node.  Only variables or constants can be
 * leaves of the trie, but constants have special instructions when they
 * appear as leaves.
 */
#define next_lpcreg {				\
   if ( IsLeafNode(NodePtr) )			\
     proceed_lpcreg				\
   else						\
     non_ftag_lpcreg;				\
 }

/*
 * Use when current node is known to be a leaf of the trie.  If we're in
 * an answer trie, then check for and handle conditional answers.
 */
#define proceed_lpcreg {			\
   if( IsInAnswerTrie(NodePtr) && delay_it )	\
     handle_conditional_answers;		\
   global_num_vars = num_vars_in_var_regs;	\
   num_vars_in_var_regs = -1;			\
   Last_Nod_Sav = NodePtr;			\
   lpcreg = cpreg;				\
   TRIE_R_UNLOCK();				\
 }

/*
 * Use when the current node is known NOT to be a leaf of the trie.
 * Continue by going to the child of the current node.
 */
#define non_ftag_lpcreg		lpcreg = opsucc

/*----------------------------------------------------------------------*/
/* Global variables -- should really be made local ones...              */
/*----------------------------------------------------------------------*/

#ifndef MULTI_THREAD

/* TLS: 08/05 documentation of reg_array and var_regs.
 * 
 * reg_array is a stack used for unificiation by trie instructions
 * from a completed table and asserted tries.  In the former case, the
 * reg_array is init'd by tabletry; in the latter by trie_assert_inst.
 * After initialization, the values of reg_array point to the
 * dereferenced values of the answer_template (for tables) or of the
 * registers of the call (for asserted tries).  These values are
 * placed in reg_array in reverse order, so that at the end of
 * initialization the first argument of the call or answer template is
 * at the top of the stack.  Actions are then as follows: 
 * 
 * 1) When a structure/list is encountered, and the symbol unifies
 * with the top of reg_array, additional cells are pushed onto
 * reg_array.  In the case where the trie is unifying with a variable,
 * a WAM build-type operation is performed so that these new reg_array
 * cells point to new heap cells.  In the case where the trie is
 * unifying with a structure on the heap, the new cells point to the
 * arguments of the structure, in preparation for further
 * unifications. 
 * 
 * 2) When a constant/number is encountered, an attempt is made to
 * unify this value with the referent of reg_array.  If it unifies,
 * the cell is popped off of reg_array, and the algorithm continues.
 * 
 * 3) When a variable is encountered in the trie it is handled like a
 * constant from the perspective of reg_array, but now the var_regs
 * array comes into play.
 * 
 * Variables in the path of a trie are numbered sequentially in the
 * order of their occurrence in a LR traversal of the trie.  Trie
 * instructions distinguish first occurrences (_vars) from subsequent
 * occurrences (_vals).  When a _var numbered N is encountered while
 * traversing a trie path, the Nth cell of var_regs is set to the
 * value of the top of the reg_array stack, and the unification
 * (binding) performed.  If a _val N is later encountered, a
 * unification is attempted between the top of the reg_array stack,
 * and the value of var_regs(N).
 */

Cell *reg_array;
CPtr reg_arrayptr;
int  reg_array_size = DEFAULT_ARRAYSIZ;

#define MAX_TRIE_REGS 500
CPtr var_regs[MAX_TRIE_REGS];
int  num_vars_in_var_regs = -1;

BTNptr NodePtr, Last_Nod_Sav;

/*
 * Variable delay_it decides whether we should delay an answer after we
 * have gone though a branch of an answer trie and reached the answer
 * leaf.  If delay_it == 1, then macro handle_conditional_answers() will
 * be called (in proceed_lpcreg).
 *
 * In return_table_code, we need to set delay_it to 1. But in
 * get_returns/2, we need to set it to 0.
 */
int     delay_it;

#endif /* MULTI_THREAD */

/*----------------------------------------------------------------------*/

#define restore_regs_and_vars(tbreg,offset)	\
    undo_bindings(tbreg);			\
    delayreg = cp_pdreg(tbreg);                 \
    restore_some_wamregs(tbreg, ereg);	        \
    restore_trie_registers(tbreg + offset) 

/*----------------------------------------------------------------------*/
/* Garbage collection strongly prefers tagged integers in CP stack...   */
/*       PLEASE PRESERVE THIS IVNARIANT --- Kostis & Bart !             */
/*----------------------------------------------------------------------*/

#define save_trie_registers(tbreg) {				\
  CPtr temp_arrayptr;						\
  int reg_count = 0, i;						\
								\
  i = num_vars_in_var_regs;					\
  while (i >= 0) {						\
    *(--tbreg) = (Cell)var_regs[i];				\
    i--;							\
  }								\
  *(--tbreg) = makeint(num_vars_in_var_regs);			\
  temp_arrayptr = reg_arrayptr;					\
  while (temp_arrayptr >= reg_array) {				\
    /* INV: temp_array_ptr + reg_count == reg_arrayptr */	\
    *(--tbreg) = *temp_arrayptr;				\
    reg_count++;						\
    temp_arrayptr--;						\
  }								\
  (*--tbreg) = makeint(reg_count);				\
}

#define restore_trie_registers(temp) {			\
    int i;						\
    CPtr treg = temp;					\
    i = cell(treg);					\
    i = int_val(i);					\
    while (i > 0) {					\
      reg_arrayptr++;					\
      *reg_arrayptr = *(++treg);			\
      i--;						\
    }							\
    i = *(++treg);					\
    num_vars_in_var_regs = int_val(i);			\
    for (i = 0; i <= num_vars_in_var_regs; i++) {	\
      var_regs[i] = (CPtr)*(++treg);			\
    }							\
}

/*----------------------------------------------------------------------*/

#define unify_with_trie_numcon {					\
  XSB_Deref(*reg_arrayptr);					       	\
  if (isref(*reg_arrayptr)) {						\
    bind_ref((CPtr)*reg_arrayptr, opatom);				 \
  }									\
  else if (isattv(*reg_arrayptr)) {					\
    attv_dbgmsg(">>>> add_interrupt in unify_with_trie_numcon\n");	\
    add_interrupt(CTXTc cell(((CPtr)dec_addr(*reg_arrayptr) + 1)), opatom);\
    bind_int_tagged((CPtr)dec_addr(*reg_arrayptr), opatom);          		\
  }									\
  else {								\
    if (*reg_arrayptr != opatom) {					\
      Fail1;								\
      XSB_Next_Instr();							\
    }									\
  }									\
}

#define unify_with_trie_str {					\
  Psc psc;							\
  int i, arity;							\
								\
  XSB_Deref(*reg_arrayptr);			       		\
  psc = (Psc) cs_val(opatom);					\
  arity = (int) get_arity(psc);					\
  will_overflow_reg_array(reg_arrayptr + arity);		\
  if (isref(*reg_arrayptr)) {					\
    bind_ref((CPtr) *reg_arrayptr, makecs(hreg));		\
    reg_arrayptr--;						\
    *(hreg++) = (Cell) psc;					\
    for (i = arity; i >= 1; i--) {				\
      *(reg_arrayptr + i) = (Cell) hreg;			\
      new_heap_free(hreg);					\
    }								\
    reg_arrayptr += arity;					\
    check_glstack_overflow(0,pcreg,0);				\
  }								\
  else if (isattv(*reg_arrayptr)) {				\
    attv_dbgmsg(">>>> add_interrupt in unify_with_trie_str\n");	\
    add_interrupt(CTXTc cell(((CPtr)dec_addr(*reg_arrayptr) + 1)), makecs(hreg+INT_REC_SIZE));	\
    bind_copy((CPtr)dec_addr(*reg_array), makecs(hreg));        \
    reg_arrayptr--;						\
    *(hreg++) = (Cell) psc;					\
    for (i = arity; i >= 1; i--) {				\
      *(reg_arrayptr + i) = (Cell) hreg;			\
      new_heap_free(hreg);					\
    }								\
    reg_arrayptr += arity;					\
    check_glstack_overflow(0,pcreg,0);				\
  }   								\
  else {							\
    CPtr temp = (CPtr)*reg_arrayptr;				\
    if ((isconstr(temp)) && (psc == get_str_psc(temp))) {	\
      reg_arrayptr--;						\
      temp = (CPtr)cs_val(temp);				\
      for (i = arity; i >= 1; i--) {				\
	*(reg_arrayptr+i) = *(temp+arity-i+1);			\
      }								\
      reg_arrayptr += arity;					\
    }								\
    else {							\
      Fail1;							\
      XSB_Next_Instr();						\
    }								\
  }								\
}

#define unify_with_trie_list {						\
  XSB_Deref(*reg_arrayptr);    						\
  if (isref(*reg_arrayptr)) {						\
    bind_ref((CPtr) *reg_arrayptr, (Cell) makelist(hreg));		\
    *reg_arrayptr = (Cell)(hreg+1);         /* head of list */		\
    will_overflow_reg_array(reg_arrayptr + 1);				\
    *(++reg_arrayptr) = (Cell) hreg;        /* tail of list */		\
    new_heap_free(hreg);						\
    new_heap_free(hreg);						\
    check_glstack_overflow(0,pcreg,0);					\
  }									\
  else if (isattv(*reg_arrayptr)) {					\
    attv_dbgmsg(">>>> add_interrupt in unify_with_trie_list\n");	\
    add_interrupt(CTXTc cell(((CPtr)dec_addr(*reg_arrayptr) + 1)), makelist(hreg+INT_REC_SIZE));	\
    bind_copy((CPtr)dec_addr(*reg_arrayptr), makelist(hreg));       \
    *reg_arrayptr = (Cell)(hreg+1);         /* tail of list */		\
    will_overflow_reg_array(reg_arrayptr + 1);				\
    *(++reg_arrayptr) = (Cell) hreg;        /* head of list */		\
    new_heap_free(hreg);						\
    new_heap_free(hreg);						\
    check_glstack_overflow(0,pcreg,0);					\
  }									\
  else {								\
    CPtr temp = (CPtr)*reg_arrayptr;					\
    if (islist(temp)) {							\
      will_overflow_reg_array(reg_arrayptr + 1);			\
      *reg_arrayptr++ = (Cell)(clref_val(temp)+1);			\
      *reg_arrayptr = (Cell)(clref_val(temp));				\
    } else {								\
      Fail1;								\
      XSB_Next_Instr();							\
    }									\
  }									\
}

/*
 * In clp(Q,R), most (or all) of the attvs in the call are updated in the
 * answer.  So we have a set of *new* attvs in the answer trie.  This set
 * of new attvs will be copied into the answer trie when the *first* attv
 * in the call is copied into the answer trie (since most/all of the other
 * attvs are related to the first one).  The later occurrences of the
 * *other* attvs are encoded as `unify_with_trie_val', but we don't want
 * to trigger attv interrupts when we update the attvs in the call.
 */

#define unify_with_trie_val {						\
  Cell cell2deref;							\
  XSB_Deref(*reg_arrayptr);    						\
  if (isref(*reg_arrayptr)) {						\
    cell2deref = (Cell)var_regs[(int)int_val(opatom)];			\
    XSB_Deref(cell2deref);	       					\
    if (cell2deref != *reg_arrayptr)					\
      bind_ref((CPtr) *reg_arrayptr, cell2deref);			\
  }									\
  else if (isattv(*reg_arrayptr)) {					\
    cell2deref = (Cell) var_regs[(int)int_val(opatom)];			\
    XSB_Deref(cell2deref);     						\
    if (*reg_arrayptr != cell2deref) {					\
      /* Do not trigger attv interrupt! */				\
      bind_ref(clref_val(*reg_arrayptr), cell2deref);			\
    }									\
    else {								\
      attv_dbgmsg(">>>> keep old attr in unify_with_trie_val\n");	\
    }									\
  }									\
  else {								\
    op1 = (Cell)*reg_arrayptr;						\
    op2 = (Cell) var_regs[(int)int_val(opatom)];			\
    if (unify(CTXTc op1,op2) == FALSE) {				\
      Fail1;								\
      XSB_Next_Instr();							\
    }									\
  }									\
  reg_arrayptr--;							\
}

/* TLS: 
   reg_arrayptr is ptr to the call that we are unifying the answer with
*/
#define unify_with_trie_attv {						\
  XSB_Deref(*reg_arrayptr);			       			\
  num_vars_in_var_regs = (int)int_val(opatom) &0xffff;			\
  if (isref(*reg_arrayptr)) {						\
    bind_ref((CPtr) *reg_arrayptr, makeattv(hreg));			\
  }									\
  else if (isattv(*reg_arrayptr)) {					\
    add_interrupt(CTXTc cell(((CPtr)dec_addr(*reg_arrayptr) + 1)),makeattv(hreg+INT_REC_SIZE));   \
    bind_ref((CPtr)dec_addr(*reg_arrayptr), makeattv(hreg));	\
  }									\
  else {								\
    add_interrupt(CTXTc makeattv(hreg+INT_REC_SIZE+1), *reg_arrayptr);	\
    bind_copy((hreg+INT_REC_SIZE),*reg_arrayptr);			\
  }									\
  var_regs[num_vars_in_var_regs] = (CPtr) *reg_arrayptr;		\
  new_heap_free(hreg);							\
  *reg_arrayptr = (Cell) hreg;						\
  new_heap_free(hreg);							\
  check_glstack_overflow(0,pcreg,0);					\
}

#ifdef UNDEFINED
#define unify_with_trie_attv {						\
  XSB_Deref(*reg_arrayptr);			       			\
  num_vars_in_var_regs = (int)int_val(opatom) &0xffff;			\
  if (isref(*reg_arrayptr)) {						\
    bind_ref((CPtr) *reg_arrayptr, makeattv(hreg));			\
  }									\
  else if (isattv(*reg_arrayptr)) {					\
    add_interrupt(CTXTc cell(((CPtr)dec_addr(*reg_arrayptr) + 1)),makeattv(hreg+INT_REC_SIZE));   \
    bind_ref((CPtr)dec_addr(*reg_arrayptr), makeattv(hreg));	\
  }									\
  else {								\
    printf("%p >>>> add_interrupt in unify_with_trie_attv\n",*reg_arrayptr);		\
    makeattv((hreg+INT_REC_SIZE));					\
    add_interrupt(CTXTc makeattv(hreg+INT_REC_SIZE+1), *reg_arrayptr);	\
    bind_copy((hreg+INT_REC_SIZE),*reg_arrayptr);			\
  }									\
  var_regs[num_vars_in_var_regs] = *reg_arrayptr;			\
  new_heap_free(hreg);							\
  *reg_arrayptr = (Cell) hreg;						\
  new_heap_free(hreg);							\
  check_glstack_overflow(0,pcreg,0);					\
}
#endif

/*----------------------------------------------------------------------*/
