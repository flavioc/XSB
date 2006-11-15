/* File:      slgdelay.c
** Author(s): Kostis Sagonas, Baoqiu Cui
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


#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>
#include <stdlib.h>

/* special debug includes */
#include "debugs/debug_delay.h"

#include "auxlry.h"
#include "cell_xsb.h"
#include "psc_xsb.h"
#include "register.h"
#include "trie_internals.h"
#include "memory_xsb.h"
#include "choice.h"
#include "macro_xsb.h"
#include "tr_utils.h"
#include "inst_xsb.h"
#include "error_xsb.h"
#include "io_builtins_xsb.h"
#include "thread_xsb.h"

static void simplify_neg_succeeds(CTXTdeclc VariantSF);
extern void simplify_pos_unsupported(CTXTdeclc NODEptr);
static void simplify_pos_unconditional(CTXTdeclc NODEptr);

Structure_Manager smASI      = SM_InitDecl(ASI_Node, ASIs_PER_BLOCK,
					    "Answer Substitution Info Node");

/*
 * Global variables for shared/sequential tables.
 */

/* Constants */
/* If you change the size of the blocks, please update info in
   trie_internals.h also */

static unsigned long de_block_size_glc = 2048 * sizeof(struct delay_element);
static unsigned long dl_block_size_glc = 2048 * sizeof(struct delay_list);
static unsigned long pnde_block_size_glc = 2048 *sizeof(struct pos_neg_de_list);

/* Rest of globals protected by MUTEX_DELAY (I hope) */
char *current_de_block_gl = NULL;
char *current_dl_block_gl = NULL;
char *current_pnde_block_gl = NULL;

static DE released_des_gl = NULL;	/* the list of released DEs */
static DL released_dls_gl = NULL;	/* the list of released DLs */
static PNDE released_pndes_gl = NULL;	/* the list of released PNDEs */

static DE next_free_de_gl = NULL;	/* next available free DE space */
static DL next_free_dl_gl = NULL;	/* next available free DL space */
static PNDE next_free_pnde_gl = NULL;	/* next available free PNDE space */

static DE current_de_block_top_gl = NULL;      /* the top of current DE block */
static DL current_dl_block_top_gl = NULL;      /* the top of current DL block */
static PNDE current_pnde_block_top_gl = NULL; /* the top of current PNDE block*/


/*
 * A macro definition for allocating a new entry of DE, DL, or PNDE.  To
 * release such an entry, use definition release_entry (see below).
 *
 * new_entry(NEW_ENTRY,            -- pointer to the new entry
 * 	     RELEASED,             -- pointer to the released entries
 * 	     NEXT_FREE,            -- pointer to the next free entry
 * 	     CURRENT_BLOCK,        -- pointer to the current block
 * 	     CURRENT_BLOCK_TOP,    -- pointer to the current block top
 * 	     NEXT_FUNCTION,        -- next function (eg. de_next)
 * 	     ENTRY_TYPE,           -- type of the entry (eg. DE)
 * 	     BLOCK_SIZE,           -- block size (for malloc a new block)
 * 	     ABORT_MESG)           -- xsb_abort mesg when no enough memory
 *
 * No mutexes needed at this level, as shared structures will be
 * protected by MUTEX_DELAY.
 */

#define new_entry(NEW_ENTRY,						\
		  RELEASED,						\
		  NEXT_FREE,						\
		  CURRENT_BLOCK,					\
		  CURRENT_BLOCK_TOP,					\
		  NEXT_FUNCTION,					\
		  ENTRY_TYPE,						\
		  BLOCK_SIZE,						\
		  ABORT_MESG)						\
  if (RELEASED) {							\
    NEW_ENTRY = RELEASED;						\
    RELEASED = NEXT_FUNCTION(RELEASED);					\
  }									\
  else if (NEXT_FREE < CURRENT_BLOCK_TOP)				\
    NEW_ENTRY = NEXT_FREE++;						\
  else {								\
    char *new_block;							\
    if ((new_block = (char *) mem_alloc(BLOCK_SIZE + sizeof(Cell),TABLE_SPACE)) == NULL)\
      xsb_abort(ABORT_MESG);						\
    *(char **) new_block = CURRENT_BLOCK;				\
    CURRENT_BLOCK = new_block;						\
    NEXT_FREE = (ENTRY_TYPE)(new_block + sizeof(Cell));			\
    CURRENT_BLOCK_TOP = (ENTRY_TYPE)(new_block + sizeof(Cell) + BLOCK_SIZE);\
    NEW_ENTRY = NEXT_FREE++;						\
  }

#define release_entry(ENTRY_TO_BE_RELEASED,				\
		      RELEASED,						\
		      NEXT_FUNCTION) {					\
  NEXT_FUNCTION(ENTRY_TO_BE_RELEASED) = RELEASED;			\
  RELEASED = ENTRY_TO_BE_RELEASED;					\
}

/* * * * */

/*
 * remove_pnde(PNDE_HEAD, PNDE_ITEM) removes PNDE_ITEM from the
 * corresponding doubly-linked PNDE list.  If PNDE_ITEM is the first one
 * in the list, resets PNDE_HEAD to point to the next one.
 *
 * One principle: Whenever we remove a DE, its PDE (or NDE) must be
 * removed from the PNDE list *first* using remove_pnde().
 */

#define remove_pnde(PNDE_HEAD, PNDE_ITEM) {		\
  PNDE *pnde_head_ptr;					\
  PNDE next;						\
							\
  pnde_head_ptr = &(PNDE_HEAD);				\
  next = pnde_next(PNDE_ITEM);				\
  if (*pnde_head_ptr == PNDE_ITEM)			\
    *pnde_head_ptr = next;				\
  else {						\
    pnde_next(pnde_prev(PNDE_ITEM)) = next;		\
    if (next)						\
      pnde_prev(next) = pnde_prev(PNDE_ITEM);		\
  }							\
  release_entry(PNDE_ITEM, released_pndes_gl, pnde_next);	\
}

/* * * * */

/* 
 * TLS: AS info had been using malloc directly, so I changed it to use
 * the structure managers that are more common to the rest of the
 * system, rather than the new_entry/release_entry methods. 
 *
 * Allocate shared structure is not needed, as shared structures will be
 * protected by MUTEX_DELAY.
 */

#define create_as_info(ST_MAN,ANS, SUBG)			\
  {								\
    SM_AllocateStruct(ST_MAN,( asi));				\
    Child(ANS) = (NODEptr) asi;					\
    asi_pdes(asi) = NULL;					\
    asi_subgoal(asi) = SUBG;					\
    asi_dl_list(asi) = NULL;					\
  }

/*
 * The following functions are used for statistics.  If changing their
 * usage pattern, make sure you adjust the mutexes.  (MUTEX_DELAY is
 * non-recursive.)   Use of NOERROR mutexes is ok here.
 */


unsigned long allocated_de_space(char * current_de_block,int * num_blocks)
{
  int size = 0;
  char *t = current_de_block;

  *num_blocks = 0;
  while (t) {
    (*num_blocks)++;
    size =+ (de_block_size_glc + sizeof(Cell));
    t = *(char **)t;
  }
  return size;
}

static int released_de_num(DE released_des)
{
  int i = 0;
  DE p;

  p = released_des;
  while (p != NULL) {
    i++;
    p = de_next(p);
  }
  return(i);
}

unsigned long unused_de_space(void)
{
  return (current_de_block_top_gl
	  - next_free_de_gl
	  + released_de_num(released_des_gl)) * sizeof(struct delay_element);
}

#ifdef MULTI_THREAD
unsigned long unused_de_space_private(CTXTdecl)
{
  return (private_current_de_block_top
	  - private_next_free_de
	  + released_de_num(private_released_des)) * sizeof(struct delay_element);
}
#endif

/* * * * * */

unsigned long allocated_dl_space(char * current_dl_block,int * num_blocks)
{
  int size = 0;
  char *t = current_dl_block;

  *num_blocks = 0;
  while (t) {
    (*num_blocks)++;
    size =+ (dl_block_size_glc + sizeof(Cell));
    t = *(char **)t;
  }
  return size;
}

static int released_dl_num(DL released_dls)
{
  int i = 0;
  DL p;

  p = released_dls;
  while (p != NULL) {
    i++;
    p = dl_next(p);
  }
  return(i);
}

unsigned long unused_dl_space(void)
{
  return (current_dl_block_top_gl
	  - next_free_dl_gl
	  + released_dl_num(released_dls_gl)) * sizeof(struct delay_list);
}

#ifdef MULTI_THREAD
unsigned long unused_dl_space_private(CTXTdecl)
{
  return (private_current_dl_block_top
	  - private_next_free_dl
	  + released_dl_num(private_released_dls)) * sizeof(struct delay_list);
}
#endif

/* * * * * */

unsigned long allocated_pnde_space(char * current_pnde_block, int * num_blocks)
{
  int size = 0;
  char *t = current_pnde_block;

  *num_blocks = 0;
  while (t) {
    (*num_blocks)++;
    size =+ (pnde_block_size_glc + sizeof(Cell));
    t = *(char **)t;
  }
  return size;
}

static int released_pnde_num(PNDE released_pndes)
{
  int i = 0;
  PNDE p;

  p = released_pndes;
  while (p != NULL) {
    i++;
    p = dl_next(p);
  }
  return(i);
}

unsigned long unused_pnde_space(void)
{
  return (current_pnde_block_top_gl
	  - next_free_pnde_gl
	  + released_pnde_num(released_pndes_gl)) * sizeof(struct pos_neg_de_list);
}

#ifdef MULTI_THREAD
unsigned long unused_pnde_space_private(CTXTdecl)
{
  return (private_current_pnde_block_top
	  - private_next_free_pnde
	  + released_pnde_num(private_released_pndes)) * sizeof(struct pos_neg_de_list);
}
#endif

/* * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * *
 * Assign one entry for delay_elem in the current DE (Delay Element)
 * block.  A new block will be allocated if necessary.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
  
static DE intern_delay_element(CTXTdeclc Cell delay_elem)
{
  DE de;
  CPtr cptr = (CPtr) cs_val(delay_elem);
  /*
   * All the following information about delay_elem is set in
   * delay_negatively() or delay_positively().  Note that cell(cptr) is
   * the delay_psc ('DL').
   */
  VariantSF subgoal;
  NODEptr ans_subst;
  CPtr ret_n = 0;
  int arity;
  Cell tmp_cell;

  tmp_cell = cell(cptr + 1);
  subgoal = (VariantSF) addr_val(tmp_cell);
  tmp_cell = cell(cptr + 2);
  ans_subst = (NODEptr) addr_val(tmp_cell);
  tmp_cell = cell(cptr + 3);
  
  /*
   * cell(cptr + 3) can be one of the following:
   *   1. integer 0 (NEG_DELAY), for a negative DE;
   *   2. string "ret", for a positive DE with arity 0;
   *   3. constr ret/n, for a positive DE with arity >=1.
   */
  if (isinteger(tmp_cell) || isstring(tmp_cell))
    arity = 0;
  else {
    ret_n = (CPtr) cs_val(tmp_cell);
    arity = get_arity((Psc) get_str_psc(cell(cptr + 3)));
  }

  if (!was_simplifiable(subgoal, ans_subst)) {
    new_entry(de,
	      released_des_gl,
	      next_free_de_gl,
	      current_de_block_gl,
	      current_de_block_top_gl,
	      de_next,
	      DE,
	      de_block_size_glc,
	      "Not enough memory to expand DE space");
    de_subgoal(de) = subgoal;
    de_ans_subst(de) = ans_subst; /* Leaf of the answer (substitution) trie */

#ifdef DEBUG_DELAYVAR
    de_subs_fact(de) = NULL;
#ifndef IGNORE_DELAYVAR
    if (arity != 0) {
      de_subs_fact_leaf(de) = delay_chk_insert(arity, ret_n + 1,
					       (CPtr *) &de_subs_fact(de));
    }
#endif /* IGNORE_DELAYVAR */
#else
#ifndef IGNORE_DELAYVAR
    if (arity != 0) {
      CPtr hook = NULL;
      de_subs_fact_leaf(de) = delay_chk_insert(CTXTc arity, ret_n + 1,
					       &hook);
    }
#endif /* IGNORE_DELAYVAR */
#endif
    return de;
  }
  else
    return NULL;
}

/* * * * * *
 * Private version of above predicate (took out ignore/debug
 * delayvars)
 * * * * * */

#ifdef MULTI_THREAD
static DE intern_delay_element_private(CTXTdeclc Cell delay_elem)
{
  DE de;
  CPtr cptr = (CPtr) cs_val(delay_elem);
  /*
   * All the following information about delay_elem is set in
   * delay_negatively() or delay_positively().  Note that cell(cptr) is
   * the delay_psc ('DL').
   */
  VariantSF subgoal;
  NODEptr ans_subst;
  CPtr ret_n = 0;
  int arity;
  Cell tmp_cell;

  tmp_cell = cell(cptr + 1);
  subgoal = (VariantSF) addr_val(tmp_cell);
  tmp_cell = cell(cptr + 2);
  ans_subst = (NODEptr) addr_val(tmp_cell);
  tmp_cell = cell(cptr + 3);
  
  /*
   * cell(cptr + 3) can be one of the following:
   *   1. integer 0 (NEG_DELAY), for a negative DE;
   *   2. string "ret", for a positive DE with arity 0;
   *   3. constr ret/n, for a positive DE with arity >=1.
   */
  if (isinteger(tmp_cell) || isstring(tmp_cell))
    arity = 0;
  else {
    ret_n = (CPtr) cs_val(tmp_cell);
    arity = get_arity((Psc) get_str_psc(cell(cptr + 3)));
  }

  if (!was_simplifiable(subgoal, ans_subst)) {
    new_entry(de,
	      private_released_des,
	      private_next_free_de,
	      private_current_de_block,
	      private_current_de_block_top,
	      de_next,
	      DE,
	      de_block_size_glc,
	      "Not enough memory to expand DE space");
    de_subgoal(de) = subgoal;
    de_ans_subst(de) = ans_subst; /* Leaf of the answer (substitution) trie */

    if (arity != 0) {
      CPtr hook = NULL;
      de_subs_fact_leaf(de) = delay_chk_insert(CTXTc arity, ret_n + 1,
					       &hook);
    }
    return de;
  }
  else
    return NULL;
}
#endif // (MULTI_THREAD)

/*
 * Construct a delay list according to dlist.  Assign an entry in the
 * current DL block for it.  A new DL block will be allocated if
 * necessary.
 */

static DL intern_delay_list(CTXTdeclc CPtr dlist) /* assumes that dlist != NULL	*/
{
  DE head = NULL, de;
  DL dl = NULL;

  while (islist(dlist)) {
    dlist = clref_val(dlist);
    if ((de = intern_delay_element(CTXTc cell(dlist))) != NULL) {
      de_next(de) = head;
      head = de;
    }
    dlist = (CPtr) cell(dlist+1);
  }
  if (head) {
    new_entry(dl,
	      released_dls_gl,
	      next_free_dl_gl,
	      current_dl_block_gl,
	      current_dl_block_top_gl,
	      dl_next,
	      DL,
	      dl_block_size_glc,
	      "Not enough memory to expand DL space");
    dl_de_list(dl) = head;
    dl_asl(dl) = NULL;
    return dl;
  }
  else return NULL;
}

#ifdef MULTI_THREAD
static DL intern_delay_list_private(CTXTdeclc CPtr dlist) /* assumes that dlist != NULL	*/
{
  DE head = NULL, de;
  DL dl = NULL;

  while (islist(dlist)) {
    dlist = clref_val(dlist);
    if ((de = intern_delay_element_private(CTXTc cell(dlist))) != NULL) {
      de_next(de) = head;
      head = de;
    }
    dlist = (CPtr) cell(dlist+1);
  }
  if (head) {
    new_entry(dl,
	      private_released_dls,
	      private_next_free_dl,
	      private_current_dl_block,
	      private_current_dl_block_top,
	      dl_next,
	      DL,
	      dl_block_size_glc,
	      "Not enough memory to expand DL space");
    dl_de_list(dl) = head;
    dl_asl(dl) = NULL;
    return dl;
  }
  else return NULL;
}
#endif

/*
 * For each delay element de in delay list dl, do one of the following
 * things:
 *
 * 1) (If de is a negative DE) Add a NDE `pnde' to the nde_list of de's
 *    subgoal frame.  Point de_pnde(de) to `pnde', and point
 *    pnde_dl(pnde) to dl, pnde_de(pnde) to de.
 *    
 * 2) (If de is a positive DE) Add a PDE `pnde' to the pdes of the
 *    Delay Info node of de's answer substitution leaf.  Point
 *    de_pnde(de) to `pnde', and point pnde_dl(pnde) to dl, pnde_de(pnde)
 *    to de.
 */

static void record_de_usage(DL dl)
{
  DE de;
  PNDE pnde, current_first;
  NODEptr as_leaf;
#ifdef DEBUG_DELAYVAR
  PNDE tmp;
#endif
 
  de = dl_de_list(dl);
  while (de) {
    new_entry(pnde,
	      released_pndes_gl,
	      next_free_pnde_gl,
	      current_pnde_block_gl,
	      current_pnde_block_top_gl,
	      pnde_next,
	      PNDE,
	      pnde_block_size_glc,
	      "Not enough memory to expand PNDE space");
    pnde_dl(pnde) = dl;
    pnde_de(pnde) = de;
    pnde_prev(pnde) = NULL;
    if ((as_leaf = de_ans_subst(de)) == NULL) {	/* a negative DE */
      current_first = subg_nde_list(de_subgoal(de));
#ifdef DEBUG_DELAYVAR
      tmp = current_first;
      while (tmp) {
	if (pnde_de(tmp) == de) {
	  printf(">>>> ERROR: tmp = %p, tmp->de = %p\n",
		 tmp, pnde_de(tmp));
	}
	tmp = pnde_next(tmp);
      }
#endif      
      pnde_next(pnde) = current_first;
      if (current_first)
	pnde_prev(current_first) = pnde;
      subg_nde_list(de_subgoal(de)) = pnde;
    }
    else {					/* a positive DE */
      current_first = asi_pdes(Delay(as_leaf));
      pnde_next(pnde) = current_first;
      if (current_first)
	pnde_prev(current_first) = pnde;
      asi_pdes(Delay(as_leaf)) = pnde;
    }
    de_pnde(de) = pnde;	/* record */
    de = de_next(de);
  }
}

/* * * * * *
 * Private version of above predicate (took out ignore/debug
 * delayvars)
 * * * * * */
#ifdef MULTI_THREAD
static void record_de_usage_private(CTXTdeclc DL dl)
{
  DE de;
  PNDE pnde, current_first;
  NODEptr as_leaf;
 
  de = dl_de_list(dl);
  while (de) {
    new_entry(pnde,private_released_pndes,
	      private_next_free_pnde,private_current_pnde_block,
	      private_current_pnde_block_top,pnde_next,
	      PNDE,pnde_block_size_glc,
	      "Not enough memory to expand PNDE space");
    pnde_dl(pnde) = dl;
    pnde_de(pnde) = de;
    pnde_prev(pnde) = NULL;
    if ((as_leaf = de_ans_subst(de)) == NULL) {	/* a negative DE */
      current_first = subg_nde_list(de_subgoal(de));
      pnde_next(pnde) = current_first;
      if (current_first)
	pnde_prev(current_first) = pnde;
      subg_nde_list(de_subgoal(de)) = pnde;
    }
    else {					/* a positive DE */
      current_first = asi_pdes(Delay(as_leaf));
      pnde_next(pnde) = current_first;
      if (current_first)
	pnde_prev(current_first) = pnde;
      asi_pdes(Delay(as_leaf)) = pnde;
    }
    de_pnde(de) = pnde;	/* record */
    de = de_next(de);
  }
}
#endif

/*
 * For predicates using call variance, do_delay_stuff() is called in
 * table_answer_search(), immediately after variant_answer_search(),
 * regardless of whether the answer is new (conditional answer
 * handling is not implemented for predicates using call subsumption),
 * Here, `as_leaf' is the leaf node of the answer trie (the
 * return value of variant_answer_search), `subgoal' is the subgoal
 * frame of the current call, and `sf_exists' tells whether the
 * non-conditional part of this answer is new or not. 
 *
 * At call time, `delayreg' is the delay register of the _current_
 * execution state.  If delayreg is not NULL, then it means this
 * answer has some delay elements and is conditional.  (If non-NULL,
 * delayreg points to a delay list of delay elements on the heap).
 * This function assumes that conditional answers have previously been
 * checked to ensure none of their delay elements is non-satisfiable
 * (done in new_answer_dealloc).  At the same time, do_delay_stuff()
 * will recheck the delay elements to remove any that have become
 * unconditional.  Thus, two traversals of the delay list are
 * required.
 *
 * Function intern_delay_list() will be called to save the delay list
 * information in the Delay Info node of current call's answer leaf.  It
 * will call intern_delay_element(), which will call delay_chk_insert().
 * A delay trie is created by delay_chk_insert() for the corresponding
 * delay element.
 *
 * When the delay trie has been created, and a pointer in the delay
 * element (saved in the answer trie) has been set, we can say the
 * conditional answer is tabled.
 * 
 * TLS: moved mutexes into conditionals.  This avoids locking the
 * delay mutex when adding an answer for a LRD stratified program.
 * For non-LRD programs the placement of mutexes may mean that we lock
 * the mutex more than once per answer, but such cases will be
 * uncommon for the most part (as two of the cases engender
 * simplification).  
 */

void do_delay_stuff_shared(CTXTdeclc NODEptr as_leaf, VariantSF subgoal, xsbBool sf_exists)
{
    ASI	asi;
    DL dl = NULL;

    SYS_MUTEX_LOCK( MUTEX_DELAY ) ;
    if (delayreg && (!sf_exists || is_conditional_answer(as_leaf))) {
      if ((dl = intern_delay_list(CTXTc delayreg)) != NULL) {
	mark_conditional_answer(as_leaf, subgoal, dl, smASI);
	record_de_usage(dl);
      }
    }
    SYS_MUTEX_UNLOCK( MUTEX_DELAY ) ;
    /*
     * Check for the derivation of an unconditional answer.
     */
    if (sf_exists && is_conditional_answer(as_leaf) &&
	(!delayreg || !dl)) {
      /*
       * Initiate positive simplification in places where this answer
       * substitution has already been returned.
       */
      SYS_MUTEX_LOCK( MUTEX_DELAY ) ;
      simplify_pos_unconditional(CTXTc as_leaf);
      SYS_MUTEX_UNLOCK( MUTEX_DELAY ) ;
    }
    if (is_unconditional_answer(as_leaf) && subg_nde_list(subgoal)) {
      SYS_MUTEX_LOCK( MUTEX_DELAY ) ;
      simplify_neg_succeeds(CTXTc subgoal);
      SYS_MUTEX_UNLOCK( MUTEX_DELAY ) ;
    }
}

#ifdef MULTI_THREAD
void do_delay_stuff_private(CTXTdeclc NODEptr as_leaf, VariantSF subgoal, xsbBool sf_exists)
{
    ASI	asi;
    DL dl = NULL;

    if (delayreg && (!sf_exists || is_conditional_answer(as_leaf))) {
      if ((dl = intern_delay_list_private(CTXTc delayreg)) != NULL) {
	mark_conditional_answer(as_leaf, subgoal, dl, *private_smASI);
	record_de_usage_private(CTXTc dl);
      }
    }
    /*
     * Check for the derivation of an unconditional answer.
     */
    if (sf_exists && is_conditional_answer(as_leaf) &&
	(!delayreg || !dl)) {
      /*
       * Initiate positive simplification in places where this answer
       * substitution has already been returned.
       */
      SYS_MUTEX_LOCK( MUTEX_DELAY ) ;
      simplify_pos_unconditional(CTXTc as_leaf);
      SYS_MUTEX_UNLOCK( MUTEX_DELAY ) ;
    }
    if (is_unconditional_answer(as_leaf) && subg_nde_list(subgoal)) {
      SYS_MUTEX_LOCK( MUTEX_DELAY ) ;
      simplify_neg_succeeds(CTXTc subgoal);
      SYS_MUTEX_UNLOCK( MUTEX_DELAY ) ;
    }
}
#endif

void do_delay_stuff(CTXTdeclc NODEptr as_leaf, VariantSF subgoal, xsbBool sf_exists)
{

#ifdef MULTI_THREAD  
  if (threads_current_sm == PRIVATE_SM) {
    do_delay_stuff_private(CTXTc as_leaf, subgoal, sf_exists);
  } else {
    do_delay_stuff_shared(CTXTc as_leaf, subgoal, sf_exists);
    }
#else
  do_delay_stuff_shared(CTXTc as_leaf, subgoal, sf_exists);
#endif

}

/*----------------------------------------------------------------------*/

xsbBool answer_is_junk(CPtr dlist)	  /* assumes that dlist != NULL */
{
    CPtr    cptr;
    VariantSF subgoal;
    NODEptr ans_subst;
    Cell tmp_cell;

    while (islist(dlist)) {
      dlist = clref_val(dlist);
      cptr = (CPtr) cs_val(cell(dlist));
      tmp_cell = cell(cptr + 1);
      subgoal = (VariantSF) addr_val(tmp_cell);
      tmp_cell = cell(cptr + 2);
      ans_subst = (NODEptr) addr_val(tmp_cell);
      if (is_failing_delay_element(subgoal,ans_subst)) {
	return TRUE;
      }
      dlist = (CPtr) cell(dlist+1);
    }
    return FALSE;
}

/*
 * Function remove_de_from_dl(de, dl) removes de from dl when de is
 * positive and succeeds, or negative and fails.  It is used in
 * simplify_pos_unconditional() and simplify_neg_fails().
 */

static xsbBool remove_de_from_dl(DE de, DL dl)
{
  DE current = dl_de_list(dl);
  DE prev_de = NULL;

#ifdef DEBUG_DELAYVAR
  fprintf(stddbg, ">>>> start remove_de_from_dl()\n");
#endif

  while (current != de) {
    prev_de = current;
    current = de_next(current);
  }
  if (prev_de == NULL)		/* to remove the first DE */
    dl_de_list(dl) = de_next(current);
  else
    de_next(prev_de) = de_next(current);
  release_entry(current, released_des_gl, de_next);
  return (NULL != dl_de_list(dl));
}

/*
 * Function remove_dl_from_dl_list(dl, asi) removes dl from the DL list
 * which is pointed by asi.  Called when a DE in dl is negative and
 * succeeds, or positive and unsupported (in functions
 * simplify_neg_succeeds() and simplify_pos_unsupported()).
 */

static xsbBool remove_dl_from_dl_list(DL dl, ASI asi)
{
  DL current = asi_dl_list(asi);
  DL prev_dl = NULL;

#ifdef DEBUG_DELAYVAR
  fprintf(stddbg, ">>>> start remove_dl_from_dl_list()\n");
#endif

  while (current != dl) {
    prev_dl = current;
    current = dl_next(current);
  }
  if (prev_dl == NULL)		/* to remove the first DL */
    asi_dl_list(asi) = dl_next(current);
  else
    dl_next(prev_dl) = dl_next(current);

  release_entry(current, released_dls_gl, dl_next);
  return (NULL != asi_dl_list(asi));
}

/*
 * When a DL becomes empty (after remove_de_from_dl()), the answer
 * substitution which uses this DL becomes unconditional.  Further
 * simplification operations go on ...
 */

static void handle_empty_dl_creation(CTXTdeclc DL dl)
{
  NODEptr as_leaf = dl_asl(dl);
  ASI asi = Delay(as_leaf);
  VariantSF subgoal;

#ifdef DEBUG_DELAYVAR
  fprintf(stddbg, ">>>> start handle_empty_dl_creation()\n");
#endif
  /*
   * Only when `as_leaf' is still a conditional answer can we do
   * remove_dl_from_dl_list(), simplify_pos_unconditional(), and
   * simplify_neg_succeeds() here.
   *
   * If `as_leaf' is already marked UNCONDITIONAL (by
   * unmark_conditional_answer(as_leaf) in simplify_pos_unconditional()),
   * that means this is the second time when `as_leaf' becomes
   * unconditional. So we don't need do anything.  All the DLs have been
   * released in the first time.
   */
  if (is_conditional_answer(as_leaf)) {	/* if it is still conditional */
    remove_dl_from_dl_list(dl, asi);
    subgoal = asi_subgoal(Delay(as_leaf));
#ifdef DEBUG_DELAYVAR
    xsb_dbgmsg((LOG_DEBUG, ">>>> the subgoal is:"));
    dbg_print_subgoal(LOG_DEBUG,stddbg, subgoal); 
    xsb_dbgmsg((LOG_DEBUG, "\n"));
#endif
    /*
     * simplify_pos_unconditional(as_leaf) will release all other DLs for
     * as_leaf, and mark as_leaf as UNCONDITIONAL.
     */
    simplify_pos_unconditional(CTXTc as_leaf);
    /*-- perform early completion if necessary; please preserve invariants --*/
    if (!is_completed(subgoal) && most_general_answer(as_leaf)) {
      perform_early_completion(subgoal, subg_cp_ptr(subgoal));
      subg_compl_susp_ptr(subgoal) = NULL;
    }
    simplify_neg_succeeds(CTXTc subgoal);
  }
}

/*
 * Run further simplifications when an answer substitution leaf,
 * as_leaf, has no supported conditional answers.  This happens when
 * all DLs of as_leaf are removed (by remove_dl_from_dl_list)
 */

static void handle_unsupported_answer_subst(CTXTdeclc NODEptr as_leaf)
{
  ASI unsup_asi = Delay(as_leaf);
  VariantSF unsup_subgoal = asi_subgoal(unsup_asi);

#ifdef DEBUG_DELAYVAR
  fprintf(stddbg, ">>>> start handle_unsupported_answer_subst()\n");
#endif

  SET_TRIE_ALLOCATION_TYPE_SF(unsup_subgoal);
  delete_branch(CTXTc as_leaf, &subg_ans_root_ptr(unsup_subgoal));
  simplify_pos_unsupported(CTXTc as_leaf);
  if (is_completed(unsup_subgoal)) {
    if (subgoal_fails(unsup_subgoal)) {
      simplify_neg_fails(CTXTc unsup_subgoal);
    }
  }
  //  mem_dealloc(unsup_asi,sizeof(struct AS_info),TABLE_SPACE);
#ifdef MULTI_THREAD
  if (!get_private(TIF_PSC(subg_tif_ptr(asi_subgoal(unsup_asi))))) {
    SM_DeallocateSharedStruct(smASI,unsup_asi);
  }
  else {
    SM_DeallocateStruct(*private_smASI,unsup_asi);
  }
#else
  SM_DeallocateStruct(smASI,unsup_asi);
#endif
}


/*
 * To release all the DLs (and their DEs) in the DelayInfo node `asi'.
 */

void release_all_dls(ASI asi)
{
  ASI de_asi;
  DE de, tmp_de;
  DL dl, tmp_dl;

  dl = asi_dl_list(asi);
  while (dl) {
    tmp_dl = dl_next(dl);
    de = dl_de_list(dl);
    while (de) {
      tmp_de = de_next(de);
      if (de_ans_subst(de) == NULL) { /* is NDE */
	remove_pnde(subg_nde_list(de_subgoal(de)), de_pnde(de));
      }
      else {
	de_asi = Delay(de_ans_subst(de));
	remove_pnde(asi_pdes(de_asi), de_pnde(de));
      }
      release_entry(de, released_des_gl, de_next);
      de = tmp_de; /* next DE */
    } /* while (de) */
    release_entry(dl, released_dls_gl, dl_next);
    dl = tmp_dl; /* next DL */
  }
}

/*
 * When the answers substitution gets an unconditional answer, remove
 * the positive delay literals of this answer substitution from the
 * delay lists that contain them.
 */

static void simplify_pos_unconditional(CTXTdeclc NODEptr as_leaf)
{
  ASI asi = Delay(as_leaf);
  PNDE pde;
  DE de;
  DL dl;

#ifdef DEBUG_DELAYVAR
  fprintf(stddbg, ">>>> start simplify_pos_unconditional()\n");
#endif

  release_all_dls(asi);
  
  unmark_conditional_answer(as_leaf);

  while ((pde = asi_pdes(asi))) {
    de = pnde_de(pde);
    dl = pnde_dl(pde);
    remove_pnde(asi_pdes(asi), pde);
    if (!remove_de_from_dl(de, dl))
      handle_empty_dl_creation(CTXTc dl);
  }
  /*
   * Now this DelayInfo `asi' does not contain any useful info, so we can
   * free it, and really mark `as_leaf' as an unconditional answer.
   */
  Child(as_leaf) = NULL;

  //  mem_dealloc(asi,sizeof(struct AS_info),TABLE_SPACE);
#ifdef MULTI_THREAD
  if (!get_private(TIF_PSC(subg_tif_ptr(asi_subgoal(asi))))) {
    SM_DeallocateSharedStruct(smASI,asi);
  }
  else {
    SM_DeallocateStruct(*private_smASI,asi);
  }
#else
  SM_DeallocateStruct(smASI,asi);
#endif
}

/*
 * When the subgoal fails (is completed without any answers), remove
 * the negative delay literals of this subgoal from the delay lists
 * that contain them.
 */

void simplify_neg_fails(CTXTdeclc VariantSF subgoal)
{
  PNDE nde;
  DE de;
  DL dl;

#ifdef DEBUG_DELAYVAR
  xsb_dbgmsg((LOG_DEBUG, ">>>> start simplify_neg_fails()\n"));
  xsb_dbgmsg((LOG_DEBUG, ">>>> the subgoal is: "));
  dbg_print_subgoal(LOG_DEBUG,stddbg, subgoal); 
  xsb_dbgmsg((LOG_DEBUG, "\n"));
#endif

  while ((nde = subg_nde_list(subgoal))) {
    de = pnde_de(nde);
    dl = pnde_dl(nde);
    remove_pnde(subg_nde_list(subgoal), nde);
    if (!remove_de_from_dl(de, dl))
      handle_empty_dl_creation(CTXTc dl);
  }

#ifdef DEBUG_DELAYVAR
  fprintf(stddbg, ">>>> end simplify_neg_fails()\n");
#endif
  
}

/*
 * On occasion that the subgoal succeeds (gets an unconditional	
 * answer that is identical to the subgoal), it deletes all delay	
 * lists that contain a negative delay element with that subgoal.
 * 
 * Before remove_dl_from_dl_list(), all the DEs in the DL to be
 * removed have to be released, and each P(N)DE which points to a DE
 * in the DL has also to be released.
 */

static void simplify_neg_succeeds(CTXTdeclc VariantSF subgoal)
{
  PNDE nde;
  DL dl;
  DE de, tmp_de;
  ASI used_asi, de_asi;
  NODEptr used_as_leaf;

#ifdef DEBUG_DELAYVAR
  fprintf(stddbg, ">>>> start simplify_neg_succeeds()\n");
#endif

  while ((nde = subg_nde_list(subgoal))) {
    dl = pnde_dl(nde); /* dl: to be removed */
    used_as_leaf = dl_asl(dl);
    if (IsValidNode(used_as_leaf) &&
	(used_asi = Delay(used_as_leaf)) != NULL) {
      de = dl_de_list(dl); /* to release all DEs in dl */
      while (de) {
	tmp_de = de_next(de);
	if (de_ans_subst(de) == NULL) { /* is NDE */
	  remove_pnde(subg_nde_list(de_subgoal(de)), de_pnde(de));
	}
	else {
	  de_asi = Delay(de_ans_subst(de));
	  remove_pnde(asi_pdes(de_asi), de_pnde(de));
	}
#ifdef DEBUG_DELAYVAR
	fprintf(stddbg, ">>>> release DE (in simplify_neg_succeeds)\n");
#endif
	release_entry(de, released_des_gl, de_next);
	de = tmp_de; /* next DE */
      } /* while */
      if (!remove_dl_from_dl_list(dl, used_asi)) {
	handle_unsupported_answer_subst(CTXTc used_as_leaf);
      }
    } /* if */
  } /* while */
}

/*
 * On occasion that an AnswerSubstitution at `as_leaf' looses all its
 * conditional answers (all its DLs have been removed),
 * simplify_pos_unsupported() deletes all delay lists (of other
 * predicates' conditional answers) that contain a positive delay element
 * pointing to that AnswerSubstitution.
 */

void simplify_pos_unsupported(CTXTdeclc NODEptr as_leaf)
{
  ASI asi = Delay(as_leaf);
  PNDE pde;
  DL dl;
  DE de, tmp_de;
  ASI used_asi, de_asi;
  NODEptr used_as_leaf;

#ifdef DEBUG_DELAYVAR
  fprintf(stddbg, ">>>> start simplify_pos_unsupported()\n");
#endif

  while ((pde = asi_pdes(asi))) {
    dl = pnde_dl(pde); /* dl: to be removed */
    used_as_leaf = dl_asl(dl);
    if (IsValidNode(used_as_leaf) &&
	(used_asi = Delay(used_as_leaf)) != NULL) {
      de = dl_de_list(dl); /* to release all DEs in dl */
      while (de) {
	tmp_de = de_next(de);
	if (de_ans_subst(de) == NULL) { /* is NDE */
	  remove_pnde(subg_nde_list(de_subgoal(de)), de_pnde(de));
	}
	else {			  /* is PDE */
	  de_asi = Delay(de_ans_subst(de));
	  remove_pnde(asi_pdes(de_asi), de_pnde(de));
	}
#ifdef DEBUG_DELAYVAR
	fprintf(stddbg, ">>>> release DE (in simplify_pos_unsupported)");
#endif
	release_entry(de, released_des_gl, de_next);
	de = tmp_de; /* next DE */
      } /* while */
      if (!remove_dl_from_dl_list(dl, used_asi)) {
	handle_unsupported_answer_subst(CTXTc used_as_leaf);
      }
    } /* if */
  } /* while */
}

/* Can currently be called with only one active thread: otherwise put
   mutexes back in. */
void abolish_wfs_space(CTXTdecl)
{
  char *last_block;

#ifndef LOCAL_EVAL
    extern void abolish_edge_space();
#endif

  /* clear DE blocks */
  while (current_de_block_gl) {
    last_block = *(char **) current_de_block_gl;
    mem_dealloc(current_de_block_gl,de_block_size_glc + sizeof(Cell),TABLE_SPACE);
    current_de_block_gl = last_block;
  }

  /* clear DL blocks */
  while (current_dl_block_gl) {
    last_block = *(char **) current_dl_block_gl;
    mem_dealloc(current_dl_block_gl,dl_block_size_glc + sizeof(Cell),TABLE_SPACE);
    current_dl_block_gl = last_block;
  }

  /* clear PNDE blocks */
  while (current_pnde_block_gl) {
    last_block = *(char **) current_pnde_block_gl;
    mem_dealloc(current_pnde_block_gl,pnde_block_size_glc + sizeof(Cell),TABLE_SPACE);
    current_pnde_block_gl = last_block;
  }

  /* reset some pointers */
  
  released_des_gl = NULL;
  released_dls_gl = NULL;
  released_pndes_gl = NULL;

  next_free_de_gl = NULL;
  next_free_dl_gl = NULL;
  next_free_pnde_gl = NULL;

  current_de_block_top_gl = NULL;
  current_dl_block_top_gl = NULL;
  current_pnde_block_top_gl = NULL;

  SM_ReleaseResources(smASI);

#ifndef LOCAL_EVAL
    abolish_edge_space();
#endif
}

#ifdef MULTI_THREAD
void abolish_private_wfs_space(CTXTdecl)
{
  char *last_block;

// #ifndef LOCAL_EVAL
//    extern void abolish_edge_space();
// #endif

  /* clear DE blocks */
  while (private_current_de_block) {
    last_block = *(char **) private_current_de_block;
    mem_dealloc(private_current_de_block,de_block_size_glc + sizeof(Cell),TABLE_SPACE);
    private_current_de_block = last_block;
  }

  /* clear DL blocks */
  while (private_current_dl_block) {
    last_block = *(char **) private_current_dl_block;
    mem_dealloc(private_current_dl_block,dl_block_size_glc + sizeof(Cell),TABLE_SPACE);
    private_current_dl_block = last_block;
  }

  /* clear PNDE blocks */
  while (private_current_pnde_block) {
    last_block = *(char **) private_current_pnde_block;
    mem_dealloc(private_current_pnde_block,pnde_block_size_glc + sizeof(Cell),TABLE_SPACE);
    private_current_pnde_block = last_block;
  }

  SM_ReleaseResources(*private_smASI);

  /* reset some pointers */
  private_released_des = NULL;
  private_released_dls = NULL;
  private_released_pndes = NULL;

  private_next_free_de = NULL;
  private_next_free_dl = NULL;
  private_next_free_pnde = NULL;

  private_current_de_block_top = NULL;
  private_current_dl_block_top = NULL;
  private_current_pnde_block_top = NULL;

  // #ifndef LOCAL_EVAL
  //    abolish_edge_space();
  //#endif
}

#endif

/*
 * Two functions added for builtin force_truth_value/2.
 */

void force_answer_true(CTXTdeclc NODEptr as_leaf)
{
  VariantSF subgoal;
  
  if (is_conditional_answer(as_leaf)) {
    SYS_MUTEX_LOCK( MUTEX_DELAY ) ;
    subgoal = asi_subgoal(Delay(as_leaf));
    simplify_pos_unconditional(CTXTc as_leaf);
    simplify_neg_succeeds(CTXTc subgoal);
    SYS_MUTEX_UNLOCK( MUTEX_DELAY ) ;
  }
}

void force_answer_false(CTXTdeclc NODEptr as_leaf)
{
  ASI asi = Delay(as_leaf);
  VariantSF subgoal;

  if (is_conditional_answer(as_leaf)) {
    SYS_MUTEX_LOCK( MUTEX_DELAY ) ;
    subgoal = asi_subgoal(asi);
    release_all_dls(asi);
    SET_TRIE_ALLOCATION_TYPE_SF(subgoal);
    delete_branch(CTXTc as_leaf, &subg_ans_root_ptr(subgoal));
    simplify_pos_unsupported(CTXTc as_leaf);
    mark_subgoal_failed(subgoal);
    simplify_neg_fails(CTXTc subgoal);
    SYS_MUTEX_UNLOCK( MUTEX_DELAY ) ;
  }
}

/*---------------------- end of file slgdelay.c ------------------------*/
