/* File:      slgdelay.h
** Author(s): Kostis Sagonas, Juliana Freire, Baoqiu Cui
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

#ifndef __SLGDELAY_H__
#define __SLGDELAY_H__

/* special debug includes */
#include "debugs/debug_delay.h"

#define NEG_DELAY	0

/*
 * Definitions of delay operations to be used while clause or answer
 * resolution is performed.
 */

#define delay_negatively(SUBGOAL) {					\
  Cell new_delay_cons_cell;						\
									\
  new_delay_cons_cell = makelist(hreg);					\
  sreg = hreg+2;							\
  follow(hreg++) = makecs(sreg);					\
  follow(hreg++) = (delayreg == NULL) ? makenil : (Cell) delayreg;	\
  new_heap_functor(sreg, delay_psc);					\
  cell(sreg) = makeaddr(SUBGOAL); sreg++;				\
  cell(sreg) = makeaddr(NEG_DELAY); sreg++;				\
  cell(sreg) = makeaddr(NEG_DELAY); sreg++; /* NOT STRINGS */		\
  hreg = sreg;								\
  delayreg = (CPtr) new_delay_cons_cell;				\
}
    
/*
 * When delay_positively() is called, SUBGOAL is the subgoal frame of the
 * delayed subgoal, ANSWER is the answer node in the trie, and MAKE_SUBSF
 * is the pointer to the substitution factor (the ret/n functor built on
 * the heap, or a string ret_psc[0]) of the answer of the delayed subgoal
 * call.
 *
 * A delay element will be built on the heap according to the value in
 * MAKE_SUBSF, and it is inserted at the head of delay list of the parent
 * predicate (pointed by delayreg).
 */

#define delay_positively(SUBGOAL, ANSWER, MAKE_SUBSF) {			\
  Cell new_delay_cons_cell;						\
									\
  new_delay_cons_cell = makelist(hreg);					\
  sreg = hreg + 2;							\
  follow(hreg++) = makecs(sreg);					\
  follow(hreg++) = (delayreg == NULL) ? makenil : (Cell) delayreg;	\
  new_heap_functor(sreg, delay_psc);					\
  cell(sreg++) = makeaddr(SUBGOAL);					\
  cell(sreg++) = makeaddr(ANSWER);					\
  follow(sreg++) = MAKE_SUBSF;						\
  hreg = sreg;								\
  delayreg = (CPtr) new_delay_cons_cell;				\
}

/*--------------------------------------------------------------------*/

typedef struct delay_element	*DE;
typedef struct delay_list	*DL;
typedef struct pos_neg_de_list	*PNDE;

/*--------------------------------------------------------------------*/

/* hangs off of answer escape node, which is main access to it, along
   with PDES; function is vaguely analogous to subgoal frame. */

typedef struct AS_info {
  PNDE	  pdes;		/* pos DEs that refer to this answer substitution */
  VariantSF subgoal;	/* subgoal to which this answer substitution belongs */
  DL	  dl_list;	/* delay lists that this answer substitution has */
} *ASI;
typedef struct AS_info ASI_Node;

#define asi_pdes(X)	(X) -> pdes
#define asi_subgoal(X)	(X) -> subgoal
#define asi_dl_list(X)	(X) -> dl_list

#define ASIs_PER_BLOCK  256

/*
#define create_as_info(ANS, SUBG)		\
    asi = (ASI) mem_alloc(sizeof(struct AS_info),TABLE_SPACE);	\
    Child(ANS) = (NODEptr) asi;			\
    asi_pdes(asi) = NULL;			\
    asi_subgoal(asi) = SUBG;			\
    asi_dl_list(asi) = NULL
*/

/*--------------------------------------------------------------------*/

struct delay_element {
  VariantSF subgoal;	/* pointer to the subgoal frame of this DE */
  NODEptr ans_subst;	/* pointer to an answer substitution leaf */
  DE	  next;		/* pointer to the next DE in the same DL */
  PNDE	  pnde;		/* pointer to the element in PDE list or NDE
			 * list, depending on what DE it is (positive or
			 * negative).  Will be set in record_de_usage()
			 */
#ifdef DEBUG_DELAYVAR
  NODEptr subs_fact;	/* root of the delay trie for this DE */
#endif
  NODEptr subs_fact_leaf;
} ;

#define de_subgoal(X)	 (X) -> subgoal
#define de_ans_subst(X)  (X) -> ans_subst
#define de_next(X)	 (X) -> next
#define de_pnde(X)	 (X) ->	pnde
#ifdef DEBUG_DELAYVAR
#define de_subs_fact(X)	     (X) -> subs_fact
#endif
#define de_subs_fact_leaf(X) (X) -> subs_fact_leaf

/*--------------------------------------------------------------------*/

struct delay_list {
  DE      de_list;	
  NODEptr asl;		/* answer substitution leaf */
  DL      next;		/* next DL for the same AS */
} ;

#define dl_de_list(X)	(X) -> de_list
#define dl_next(X)	(X) -> next
#define dl_asl(X)	(X) -> asl

/*--------------------------------------------------------------------*/

struct pos_neg_de_list {
  DL	dl;
  DE	de;
  PNDE	prev, next;
} ;

#define pnde_dl(X)	(X) -> dl
#define pnde_de(X)	(X) -> de
#define pnde_prev(X)	(X) -> prev
#define pnde_next(X)	(X) -> next


/*
 * Handling of conditional answers.	     			      
 */

#define UNCONDITIONAL_MARK 0x3

#define Delay(X) (ASI) ((word) (TN_Child(X)) & ~UNCONDITIONAL_MARK)

#define is_conditional_answer(ANS) \
  (Child(ANS) && !((word) (Child(ANS)) & UNCONDITIONAL_MARK))

#define is_unconditional_answer(ANS) \
  (!Child(ANS) || ((word) (Child(ANS)) & UNCONDITIONAL_MARK))

/*
 * Checks whether a delay element that is about to be interned was
 * simplifiable (simplifications were already initiated for this DE).
 * More specifically, negative delay elements were simplifiable if their
 * subgoal failed (was completed without any answers), while positive
 * delay elements are simplifiable if their answer substitution became
 * unconditional.
 */

#define was_simplifiable(SUBG, ANS)					\
    ((ANS == NULL) ? (is_completed(SUBG) && subgoal_fails(SUBG))	\
    		   : (is_unconditional_answer(ANS)))

#define is_failing_delay_element(SUBG, ANS)				\
    ((ANS == NULL) ? (is_completed(SUBG) && has_answer_code(SUBG) &&	\
		      subgoal_unconditionally_succeeds(SUBG))		\
		   : (IsDeletedNode(ANS)))

/*
 * mark_conditional_answer(ANS, SUBG, NEW_DL) will add a new delay list,
 * NEW_DL, into the list of DLs for answer ANS, which is the answer
 * substitution leaf in answer trie.  If ANS does not have a Delay Info
 * node, then a Delay Info node, `asi', has to be created first (that's
 * why we call this macro definition mark_conditional_answer).  `asi' has
 * a pointer to the list of DLs for ANS.
 */

#define mark_conditional_answer(ANS, SUBG, NEW_DL)			\
  if (Child(ANS) == NULL) {						\
    create_as_info(ANS, SUBG);						\
  }									\
  else {								\
    asi = Delay(ANS);							\
  }									\
  dl_next(NEW_DL) = asi_dl_list(asi);					\
  asi_dl_list(asi) = NEW_DL;						\
  dl_asl(NEW_DL) = ANS

#define unmark_conditional_answer(ANS) /*-- NEEDS CHANGE --*/		\
    Child(ANS) = (NODEptr) ((word) (Child(ANS)) | UNCONDITIONAL_MARK)

#define most_general_answer(ANS) IsEscapeNode(ANS)

/*
 * Variables used in other parts of the system.
 */

extern xsbBool neg_delay;


/*
 * Procedures used in other parts of the system.
 */

/* TLS: because of include dependencies (context -> macro_xsb ->
   slgdelay), context.h cannot be included until the code is
   refactored.  Therefore, the CTXT-style declarations cannot yet be
   used. */

extern xsbBool answer_is_junk(CPtr);
#ifndef MULTI_THREAD
extern void abolish_wfs_space(void);
extern void simplify_neg_fails(VariantSF);
extern void do_delay_stuff(NODEptr, VariantSF, xsbBool);
#else
struct th_context;
extern void abolish_wfs_space(struct th_context *);
extern void simplify_neg_fails(struct th_context *, VariantSF);
extern void do_delay_stuff(struct th_context *, NODEptr, VariantSF, xsbBool);
#endif
extern unsigned long allocated_de_space(int * num_blocks);
extern unsigned long unused_de_space(void);
extern unsigned long allocated_dl_space(int * num_blocks);
extern unsigned long unused_dl_space(void);
#ifndef MULTI_THREAD
extern void simplify_pos_unsupported(NODEptr);
#else
extern void simplify_pos_unsupported(struct th_context *, NODEptr);
#endif
extern void release_all_dls(ASI);


/*---------------------- end of file slgdelay.h ------------------------*/

#endif /* __SLGDELAY_H__ */
