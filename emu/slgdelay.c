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


#include <stdio.h>
#include <stdlib.h>

#include "configs/config.h"
#include "debugs/debug.h"

/* special debug includes */
#include "debugs/debug_delay.h"

#include "auxlry.h"
#include "cell.h"
#include "psc.h"
#include "register.h"
#include "tries.h"
#include "memory.h"
#include "choice.h"
#include "xmacro.h"
#include "tr_utils.h"
#include "inst.h"
#include "subinst.h"
#include "xsberror.h"

static void simplify_neg_succeeds(SGFrame);
static void simplify_pos_unsupported(NODEptr);
static void simplify_pos_unconditional(NODEptr);

/*
 * Some new global variables ...
 */

unsigned long de_block_size = 2048 * sizeof(struct delay_element);
unsigned long dl_block_size = 2048 * sizeof(struct delay_list);
unsigned long pnde_block_size = 2048 * sizeof(struct pos_neg_de_list);

char *current_de_block = NULL;
char *current_dl_block = NULL;
char *current_pnde_block = NULL;

DE released_des = NULL;		/* the list of released DEs */
DL released_dls = NULL;		/* the list of released DLs */
PNDE released_pndes = NULL;	/* the list of released PNDEs */

DE next_free_de = NULL;		/* next available free DE space */
DL next_free_dl = NULL;		/* next available free DL space */
PNDE next_free_pnde = NULL;	/* next available free PNDE space */

DE current_de_block_top = NULL;	/* the top of current DE block */
DL current_dl_block_top = NULL;	/* the top of current DL block */
PNDE current_pnde_block_top = NULL; /* the top of current PNDE block */

char *new_block;		/* used in new_entry() */

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
    if ((new_block = (char *)malloc(BLOCK_SIZE + sizeof(Cell))) == NULL)\
      xsb_abort(ABORT_MESG);						\
    *(char **)new_block = CURRENT_BLOCK;				\
    CURRENT_BLOCK = new_block;						\
    NEXT_FREE = (ENTRY_TYPE)(new_block + sizeof(Cell));			\
    CURRENT_BLOCK_TOP = (ENTRY_TYPE)(new_block + sizeof(Cell) + BLOCK_SIZE);\
    NEW_ENTRY = NEXT_FREE++;						\
  }

#define release_entry(ENTRY_TO_BE_RELEASED,				\
		      RELEASED,						\
		      NEXT_FUNCTION)					\
  NEXT_FUNCTION(ENTRY_TO_BE_RELEASED) = RELEASED;			\
  RELEASED = ENTRY_TO_BE_RELEASED

/*
 * Assign one entry for delay_elem in the current DE (Delay Element)
 * block.  A new block will be allocate if necessary.
 */
  
static DE intern_delay_element(Cell delay_elem)
{
  DE de;
  CPtr cptr = (CPtr) cs_val(delay_elem);
  /*
   * All the following information about delay_elem is set in
   * delay_negatively() or delay_positively().  Note that cell(cptr) is
   * the delay_psc ('DL').
   */
  SGFrame subgoal;
  NODEptr ans_subst;
  CPtr ret_n;
  int arity;
  Cell tmp_cell;
#if !defined(DEBUG_DELAYVAR)
  CPtr hook = NULL;
#endif

  tmp_cell = cell(cptr + 1);
  subgoal = (SGFrame) string_val(tmp_cell);
  tmp_cell = cell(cptr + 2);
  ans_subst = (NODEptr) string_val(tmp_cell);
  tmp_cell = cell(cptr + 3);
  ret_n = (CPtr) cs_val(tmp_cell);
  
  if (ret_n == NEG_DELAY)
    arity = 0;
  else
    arity = get_arity((Psc) get_str_psc(cell(cptr + 3)));

#ifdef DEBUG_DELAYVAR
  fprintf(stderr, ">>>> "); print_delay_list(stderr, delayreg);
  fprintf(stderr, "\n");
  fprintf(stderr, ">>>> (Intern ONE de) arity of answer subsf = %d\n", arity);
#endif

  if (!was_simplifiable(subgoal, ans_subst)) {
    new_entry(de,
	      released_des,
	      next_free_de,
	      current_de_block,
	      current_de_block_top,
	      de_next,
	      DE,
	      de_block_size,
	      "No enough memory to expand DE space");
    de_subgoal(de) = subgoal;
    de_ans_subst(de) = ans_subst; /* Leaf of the answer (substitution) trie */
#ifdef DEBUG_DELAYVAR
    de_subs_fact(de) = NULL;
    if (arity != 0) {
      de_subs_fact_leaf(de) = delay_chk_insert(arity, ret_n + 1,
					       (CPtr *) &de_subs_fact(de));
    }
#else
    if (arity != 0) {
      de_subs_fact_leaf(de) = delay_chk_insert(arity, ret_n + 1,
					       &hook);
    }
#endif
    return de;
  }
  else return NULL;
}

/*
 * Construct a delay list according to dlist.  Assign an entry in the
 * current DL block for it.  A new DL block will be allocated if
 * necessary.
 */

static DL intern_delay_list(CPtr dlist) /* assumes that dlist != NULL	*/
{
  DE head = NULL, de;
  DL dl = NULL;

  while (islist(dlist)) {
    dlist = clref_val(dlist);
    if ((de = intern_delay_element(cell(dlist))) != NULL) {
      de_next(de) = head;
      head = de;
    }
    dlist = (CPtr)cell(dlist+1);
  }
  if (head != NULL) {
    new_entry(dl,
	      released_dls,
	      next_free_dl,
	      current_dl_block,
	      current_dl_block_top,
	      dl_next,
	      DL,
	      dl_block_size,
	      "No enough memory to expand DL space");
    dl_de_list(dl) = head;
    dl_asl(dl) = NULL;
    return dl;
  }
  else return NULL;
}

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
  PNDE pnde;
  NODEptr as_leaf;
 
  de = dl_de_list(dl);
  while (de != NULL) {
    new_entry(pnde,
	      released_pndes,
	      next_free_pnde,
	      current_pnde_block,
	      current_pnde_block_top,
	      pnde_next,
	      PNDE,
	      pnde_block_size,
	      "No enough memory to expand PNDE space");
    pnde_dl(pnde) = dl;
    pnde_de(pnde) = de;
    pnde_prev(pnde) = NULL;
    if ((as_leaf = de_ans_subst(de)) == NULL) {	/* a negative DE */
      pnde_next(pnde) = subg_nde_list(de_subgoal(de));
      subg_nde_list(de_subgoal(de)) = pnde;
    }
    else {					/* a positive DE */
      pnde_next(pnde) = asi_pdes((ASI)Delay(as_leaf));
      asi_pdes((ASI)Delay(as_leaf)) = pnde;
    }
    de_pnde(de) = pnde;	/* record */
    de = de_next(de);
  }
}

/*
 * Function do_delay_stuff() is called in the SLG instruction
 * `new_answer_dealloc', when an answer (new or not) is returned for the
 * current call.  Here, `as_leaf' is the leaf node of the answer trie
 * (TrieRetPtr, the return value of variant_trie_search), `subgoal' is
 * the subgoal frame of the current call, and `sf_exists' tells whether
 * this answer is new or not.
 *
 * At this time, `delayreg' is the delay register of the _current_ call.
 * If it is not NULL, then it means this answer has some delay elements
 * and is a conditional one.  (Remember, all the delay elements under
 * this current call have been processed by delay_positively() or
 * delay_negatively(), and `delayreg' has been updated and pointed to the
 * delay list.  All the information about the delay list is still saved
 * on the heap.)
 *
 * Function intern_delay_list() will be called to save the delay list
 * information in the Delay Info node of current call's answer leaf.  It
 * will call interned_delay_element(), which will call
 * delay_chk_insert().  A delay trie is created by delay_chk_insert() for
 * the corresponding delay element.
 *
 * When the delay trie has been created, and a pointer in the delay
 * element (saved in the answer trie) has been set, we can say the
 * conditional answer is now tabled.
 */

void do_delay_stuff(NODEptr as_leaf, SGFrame subgoal, bool sf_exists)
{
    ASI	asi;
    DL dl = NULL;

#ifdef DEBUG_DELAYVAR
    fprintf(stderr, ">>>> Start do_delay_stuff ...\n");
    fprintf(stderr, ">>>> The delay list for this subgoal itself is:\n");
    fprintf(stderr, ">>>> "); print_delay_list(stderr, delayreg);
    fprintf(stderr, "\n");
#endif

    if (delayreg != NULL && (!sf_exists || is_conditional_answer(as_leaf))) {
      if ((dl = intern_delay_list(delayreg)) != NULL) {
	mark_conditional_answer(as_leaf, subgoal, dl);
	record_de_usage(dl);
      }
    }
    /*
     * Check for the derivation of an unconditional answer.
     */
    if (sf_exists && is_conditional_answer(as_leaf) &&
	(delayreg == NULL || dl == NULL)) {
      /*
       * Initiate positive simplification in places where this answer
       * substitution has already been returned.
       */
      simplify_pos_unconditional(as_leaf);
    }
    if (is_unconditional_answer(as_leaf) && subg_nde_list(subgoal) != NULL) {
      simplify_neg_succeeds(subgoal);
    }
}

/*----------------------------------------------------------------------*/

bool answer_is_junk(CPtr dlist)		  /* assumes that dlist != NULL */
{
    CPtr    cptr;
    SGFrame subgoal;
    NODEptr ans_subst;
    Cell tmp_cell;

    while (islist(dlist)) {
      dlist = clref_val(dlist);
      cptr = (CPtr)cs_val(cell(dlist));
      tmp_cell = cell(cptr + 1);
      subgoal = (SGFrame) string_val(tmp_cell);
      tmp_cell = cell(cptr + 2);
      ans_subst = (NODEptr) string_val(tmp_cell);
      if (is_failing_delay_element(subgoal,ans_subst)) {
#ifdef PROFILE
	if (ans_subst == NULL) 
	  subinst_table[NEW_ANSWER_SIMPL_NEG_FAIL][1]++;
	else 
	  subinst_table[NEW_ANSWER_SIMPL_POS_UNS][1]++;
#endif
	return TRUE;
      }
      dlist = (CPtr)cell(dlist+1);
    }
    return FALSE;
}

/*
 * Function remove_de_from_dl(de, dl) removes de from dl when de is
 * positive and succeeds, or negative and fails.  It is used in
 * simplify_pos_unconditional() and simplify_neg_fails().
 */

static bool remove_de_from_dl(DE de, DL dl)
{
  DE current = dl_de_list(dl);
  DE prev_de = NULL;

#ifdef DEBUG_DELAYVAR
  fprintf(stderr, ">>>> start remove_de_from_dl()\n");
#endif

  while (current != de) {
    prev_de = current;
    current = de_next(current);
  }
  if (prev_de == NULL)		/* to remove the first DE */
    dl_de_list(dl) = de_next(current);
  else {
    de_next(prev_de) = de_next(current);
    release_entry(current, released_des, de_next);
  }
  return (NULL != dl_de_list(dl));
}

/*
 * Function remove_dl_from_dl_list(dl, asi) removes dl from the DL list
 * which is pointed by asi.  Called when a DE in dl is negative and
 * succeeds, or positive and unsupported (in functions
 * simplify_neg_succeeds() and simplify_pos_unsupported()).
 */

static bool remove_dl_from_dl_list(DL dl, ASI asi)
{
  DL current = asi_dl_list(asi);
  DL prev_dl = NULL;

#ifdef DEBUG_DELAYVAR
  fprintf(stderr, ">>>> start remove_dl_from_dl_list()\n");
#endif

  while (current != dl) {
    prev_dl = current;
    current = dl_next(current);
  }
  if (prev_dl == NULL)		/* to remove the first DL */
    asi_dl_list(asi) = dl_next(current);
  else
    dl_next(prev_dl) = dl_next(current);

  release_entry(current, released_dls, dl_next);
  return (NULL != asi_dl_list(asi));
}

/*
 * When a DL becomes empty (after remove_de_from_dl()), the answer
 * substitution which uses this DL becomes unconditional.  Further
 * simplification operations go on ...
 *
 * Remember: release_dl
 */

static void handle_empty_dl_creation(DL dl)
{
  NODEptr as_leaf = dl_asl(dl);
  SGFrame subgoal;

#ifdef DEBUG_DELAYVAR
  fprintf(stderr, ">>>> start handle_empty_dl_creation()\n");
#endif

  release_entry(dl, released_dls, dl_next);
  if (is_conditional_answer(as_leaf)) {	/* if it is still conditional */
    subgoal = asi_subgoal((ASI)Delay(as_leaf));
    simplify_pos_unconditional(as_leaf);
    /*--- perform early completion if necessary ---*/
    if (!is_completed(subgoal) && most_general_answer(as_leaf)) {
      perform_early_completion(subgoal, subg_cp_ptr(subgoal));
    }
    simplify_neg_succeeds(subgoal);
  }
}

/*
 * Run further simplifications when an answer substitution leaf,
 * as_leaf, has no supported conditional answers.  This happens when
 * all DLs of as_leaf are removed (by remove_dl_from_dl_list)
 */

static void handle_unsupported_answer_subst(NODEptr as_leaf)
{
  ASI unsup_asi = (ASI)Delay(as_leaf);
  SGFrame unsup_subgoal = asi_subgoal(unsup_asi);

#ifdef DEBUG_DELAYVAR
  fprintf(stderr, ">>>> start handle_unsupported_answer_subst()\n");
#endif

  delete_branch(as_leaf, (CPtr)&subg_ans_root_ptr(unsup_subgoal));
  simplify_pos_unsupported(as_leaf);
  if (is_completed(unsup_subgoal)) {
    if (subgoal_fails(unsup_subgoal)) {
      mark_subgoal_failed(unsup_subgoal);
      simplify_neg_fails(unsup_subgoal);
    }
  }
}

/*
 * When the answers substitution gets an unconditional answer, remove
 * the positive delay literals of this answer substitution from the
 * delay lists that contain them.
 */

static void simplify_pos_unconditional(NODEptr as_leaf)
{
  ASI asi = (ASI)Delay(as_leaf);
  PNDE pde = asi_pdes(asi), tmp;
  DE de;
  DL dl;

#ifdef DEBUG_DELAYVAR
  fprintf(stderr, ">>>> start simplify_pos_unconditional()\n");
#endif

  unmark_conditional_answer(as_leaf);
  while (pde != NULL) {
    de = pnde_de(pde);
    dl = pnde_dl(pde);
    tmp = pnde_next(pde);
    release_entry(pde, released_pndes, pnde_next);
    pde = tmp;	/* the next PDE */
    if (!remove_de_from_dl(de, dl))
      handle_empty_dl_creation(dl);
  }
  asi_pdes(asi) = NULL;		/* forget this PDE list */
}

/*
 * When the subgoal fails (is completed without any answers), remove
 * the negative delay literals of this subgoal from the delay lists
 * that contain them.
 */

void simplify_neg_fails(SGFrame subgoal)
{
  PNDE nde = subg_nde_list(subgoal), tmp;
  DE de;
  DL dl;

#ifdef DEBUG_DELAYVAR
  fprintf(stderr, ">>>> start simplify_neg_fails()\n");
#endif

  subg_nde_list(subgoal) = NULL; /* forget this NDE list */
  while (nde != NULL) {
    de = pnde_de(nde); dl = pnde_dl(nde);
    tmp = pnde_next(nde);
    release_entry(nde, released_pndes, pnde_next);
    nde = tmp;	/* the next NDE */
    if (!remove_de_from_dl(de, dl))
      handle_empty_dl_creation(dl);
  }
}

#define remove_pnde(PNDE_HEAD, PNDE)					\
  if (PNDE_HEAD == PNDE)						\
    PNDE_HEAD = pnde_next(PNDE);					\
  else									\
    pnde_next(pnde_prev(PNDE)) = pnde_next(PNDE);			\
  release_entry(PNDE, released_pndes, pnde_next)

/*
 * On occasion that the subgoal succeeds (gets an unconditional	
 * answer that is identical to the subgoal), it deletes all delay	
 * lists that contain a negative delay element with that subgoal.
 * 
 * Before remove_dl_from_dl_list(), all the DEs in the DL to be
 * removed have to be released, and each P(N)DE which points to a DE
 * in the DL has also to be released.
 */

static void simplify_neg_succeeds(SGFrame subgoal)
{
  PNDE nde = subg_nde_list(subgoal), tmp_nde;
  DL dl;
  DE de, tmp_de;
  ASI used_asi, de_asi;
  NODEptr used_as_leaf;

#ifdef DEBUG_DELAYVAR
  fprintf(stderr, ">>>> start simplify_neg_succeeds()\n");
#endif

  /*
   * First, set this NDE list as NULL, because further simplification
   * operations (invoked by handle_unsupported_answer_subst) may visit
   * this NDE list again.  At that time, since this pointer is already
   * NULL, nothing will be done.
   */
  subg_nde_list(subgoal) = NULL;

  while (nde != NULL) {
    dl = pnde_dl(nde); /* dl: to be removed */
    used_as_leaf = dl_asl(dl);
    if (is_not_deleted(used_as_leaf) &&
	(used_asi = (ASI)Delay(used_as_leaf)) != NULL) {
      de = dl_de_list(dl); /* to release all DEs in dl */
      while (de != NULL) {
	tmp_de = de_next(de);
	if (de != pnde_de(nde)) { /*
				   * except this NDE, which will be
				   * released later (within the first
				   * while loop) anyway
				   */
	  if (de_ans_subst(de) == NULL) { /* is NDE */
	    remove_pnde(subg_nde_list(de_subgoal(de)), de_pnde(de));
	  }
	  else {
	    de_asi = (ASI)Delay(de_ans_subst(de));
 	    remove_pnde(asi_pdes(de_asi), de_pnde(de));
	  }
	}
#ifdef DEBUG_DELAYVAR
	fprintf(stderr, ">>>> release DE (in simplify_neg_succeeds)");
#endif
	release_entry(de, released_des, de_next);
	de = tmp_de; /* next DE */
      } /* while */

      if (!remove_dl_from_dl_list(dl, used_asi)) {
	handle_unsupported_answer_subst(used_as_leaf);
      }
    } /* if */
    tmp_nde = pnde_next(nde); /* release unused NDE */
    release_entry(nde, released_pndes, pnde_next);
    nde = tmp_nde;
  } /* while */
}

/*
 * On occasion that an AnswerSubstitution looses all its conditional
 * answers, it deletes all delay lists that contain a positive delay
 * element pointing to that AnswerSubstitution.
 */

static void simplify_pos_unsupported(NODEptr as_leaf)
{
  ASI asi = (ASI)Delay(as_leaf);
  PNDE pde = asi_pdes(asi), tmp_pde;
  DL dl;
  DE de, tmp_de;
  ASI used_asi, de_asi;
  NODEptr used_as_leaf;

#ifdef DEBUG_DELAYVAR
  fprintf(stderr, ">>>> start simplify_pos_unsupported()\n");
#endif

  /* First, set this PDE list as NULL */
  asi_pdes(asi) = NULL;

  while (pde != NULL) {
    dl = pnde_dl(pde); /* dl: to be removed */
    used_as_leaf = dl_asl(dl);
    if (is_not_deleted(used_as_leaf) &&
	(used_asi = (ASI)Delay(used_as_leaf)) != NULL) {
      de = dl_de_list(dl); /* to release all DEs in dl */
      while (de != NULL) {
	tmp_de = de_next(de);
	if (de != pnde_de(pde)) {
	  if (de_ans_subst(de) == NULL) { /* is NDE */
	    remove_pnde(subg_nde_list(de_subgoal(de)), de_pnde(de));
	  }
	  else {			  /* is PDE */
	    de_asi = (ASI)Delay(de_ans_subst(de));
	    remove_pnde(asi_pdes(de_asi), de_pnde(de));
	  }
	}
#ifdef DEBUG_DELAYVAR
	fprintf(stderr, ">>>> release DE (in simplify_pos_unsupported)");
#endif
	release_entry(de, released_des, de_next);
	de = tmp_de; /* next DE */
      } /* while */

      if (!remove_dl_from_dl_list(dl, used_asi)) {
	handle_unsupported_answer_subst(used_as_leaf);
      }
    } /* if */
    tmp_pde = pnde_next(pde); /* release unused PDE */
    release_entry(pde, released_pndes, pnde_next);
    pde = tmp_pde;
  } /* while */
}

void abolish_wfs_space(void)
{
  char *last_block;

#ifndef LOCAL_EVAL
    extern void abolish_edge_space();
#endif

  /* clear DE blocks */

  while (current_de_block) {
    last_block = *(char **)current_de_block;
    free(current_de_block);
    current_de_block = last_block;
  }

  /* clear DL blocks */

  while (current_dl_block) {
    last_block = *(char **)current_dl_block;
    free(current_dl_block);
    current_dl_block = last_block;
  }

  /* clear PNDE blocks */
  
  while (current_pnde_block) {
    last_block = *(char **)current_pnde_block;
    free(current_pnde_block);
    current_pnde_block = last_block;
  }

  /* reset some pointers */
  
  released_des = NULL;
  released_dls = NULL;
  released_pndes = NULL;

  next_free_de = NULL;
  next_free_dl = NULL;
  next_free_pnde = NULL;

  current_de_block_top = NULL;
  current_dl_block_top = NULL;
  current_pnde_block_top = NULL;

#ifndef LOCAL_EVAL
    abolish_edge_space();
#endif
}

/*---------------------- end of file slgdelay.c ------------------------*/
