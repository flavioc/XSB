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
   case STRING:  case INT:  case FLOAT:				\
     symbol = EncodeTrieConstant(Subterm);			\
     break;							\
   case LIST:							\
     symbol = EncodeTrieList(Subterm);				\
     break;							\
   case CS:							\
     symbol = EncodeTrieFunctor(Subterm);			\
     break;							\
   default:							\
     fprintf(stderr,"Bad tag :Type %ld ",cell_tag(Subterm));	\
     xsb_exit("In instruction hash_handle");			\
     break;							\
   }								\
   BucketNum = TrieHash(symbol,BTHT_GetHashSeed(pBTHT));	\
 }

#define find_next_nonempty_bucket(pBTHT, pTable, BucketNum) {	\
   word TableSize = BTHT_NumBuckets(pBTHT);			\
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
 }

/*
 * Use when the current node is known NOT to be a leaf of the trie.
 * Continue by going to the child of the current node.
 */
#define non_ftag_lpcreg		lpcreg = opsucc

/*----------------------------------------------------------------------*/
/* Global variables -- should really be made local ones...              */
/*----------------------------------------------------------------------*/

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
							\
    reg_arrayptr = reg_array - 1;			\
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
  deref(*reg_arrayptr);							\
  if (isref(*reg_arrayptr)) {						\
    bind_ref((CPtr)*reg_arrayptr, opatom);				\
  }									\
  else if (isattv(*reg_arrayptr)) {					\
    attv_dbgmsg(">>>> add_interrupt in unify_with_trie_numcon\n");	\
    add_interrupt(*reg_arrayptr, opatom);				\
  }									\
  else {								\
    if (*reg_arrayptr != opatom) {					\
      Fail1;								\
      goto contcase;							\
    }									\
  }									\
}

#define unify_with_trie_str {					\
  Psc psc;							\
  int i, arity;							\
								\
  deref(*reg_arrayptr);						\
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
  }								\
  else if (isattv(*reg_arrayptr)) {				\
    attv_dbgmsg(">>>> add_interrupt in unify_with_trie_str\n");	\
    add_interrupt(*reg_arrayptr, makecs(hreg));			\
    reg_arrayptr--;						\
    *(hreg++) = (Cell) psc;					\
    for (i = arity; i >= 1; i--) {				\
      *(reg_arrayptr + i) = (Cell) hreg;			\
      new_heap_free(hreg);					\
    }								\
    reg_arrayptr += arity;					\
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
      goto contcase;						\
    }								\
  }								\
}

#define unify_with_trie_list {						\
  deref(*reg_arrayptr);							\
  if (isref(*reg_arrayptr)) {						\
    bind_ref((CPtr) *reg_arrayptr, (Cell) makelist(hreg));		\
    *reg_arrayptr = (Cell)(hreg+1);         /* head of list */		\
    will_overflow_reg_array(reg_arrayptr + 1);				\
    *(++reg_arrayptr) = (Cell) hreg;        /* tail of list */		\
    new_heap_free(hreg);						\
    new_heap_free(hreg);						\
  }									\
  else if (isattv(*reg_arrayptr)) {					\
    attv_dbgmsg(">>>> add_interrupt in unify_with_trie_list\n");	\
    add_interrupt(*reg_arrayptr, makelist(hreg));			\
    *reg_arrayptr = (Cell)(hreg+1);         /* tail of list */		\
    will_overflow_reg_array(reg_arrayptr + 1);				\
    *(++reg_arrayptr) = (Cell) hreg;        /* head of list */		\
    new_heap_free(hreg);						\
    new_heap_free(hreg);						\
  }									\
  else {								\
    CPtr temp = (CPtr)*reg_arrayptr;					\
    if (islist(temp)) {							\
      will_overflow_reg_array(reg_arrayptr + 1);			\
      *reg_arrayptr++ = (Cell)(clref_val(temp)+1);			\
      *reg_arrayptr = (Cell)(clref_val(temp));				\
    } else {								\
      Fail1;								\
      goto contcase;							\
    }									\
  }									\
}

#define unify_with_trie_val {						\
  Cell cell2deref;							\
  deref(*reg_arrayptr);							\
  if (isref(*reg_arrayptr)) {						\
    cell2deref = (Cell)var_regs[(int)int_val(opatom)];			\
    deref(cell2deref);							\
    if (cell2deref != *reg_arrayptr)					\
      bind_ref((CPtr) *reg_arrayptr, cell2deref);			\
  }									\
  else if (isattv(*reg_arrayptr)) {					\
    cell2deref = (Cell) var_regs[(int)int_val(opatom)];			\
    deref(cell2deref);							\
    if (*reg_arrayptr != cell2deref) {					\
      attv_dbgmsg(">>>> add_interrupt in unify_with_trie_val\n");	\
      add_interrupt(*reg_arrayptr, cell2deref);				\
    }									\
    else {								\
      attv_dbgmsg(">>>> keep old attr in unify_with_trie_val\n");	\
    }									\
  }									\
  else {								\
    op1 = (Cell)*reg_arrayptr;						\
    op2 = (Cell) var_regs[(int)int_val(opatom)];			\
    if (unify(op1,op2) == FALSE) {					\
      Fail1;								\
      goto contcase;							\
    }									\
  }									\
  reg_arrayptr--;							\
}

#define unify_with_trie_attv {						\
  CPtr temp;								\
  deref(*reg_arrayptr);							\
  num_vars_in_var_regs = (int)int_val(opatom) &0xffff;			\
  if (isref(*reg_arrayptr)) {						\
    bind_ref((CPtr) *reg_arrayptr, makeattv(hreg));			\
  }									\
  else if (isattv(*reg_arrayptr)) {					\
    temp = clref_val(*reg_arrayptr);					\
    bind_ref(temp, makeattv(hreg));					\
  }									\
  else {								\
    attv_dbgmsg(">>>> add_interrupt in unify_with_trie_attv\n");	\
    add_interrupt(makeattv(hreg), *reg_arrayptr);			\
  }									\
  var_regs[num_vars_in_var_regs] = (CPtr) makeattv(hreg);		\
  new_heap_free(hreg);							\
  *reg_arrayptr = (Cell) hreg;						\
  new_heap_free(hreg);							\
}

/*----------------------------------------------------------------------*/
