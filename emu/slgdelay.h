/* File:      slgdelay.h
** Author(s): Kostis Sagonas, Juliana Freire
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


/* special debug includes */
#include "debugs/debug_delay.h"

#define NEG_DELAY 0


/*----------------------------------------------------------------------*/
/*  Definitions of delay operations to be used while clause or answer	*/
/*  resolution is performed.						*/
/*----------------------------------------------------------------------*/

#define delay_negatively(SUBGOAL) { \
    Cell new_delay_cons_cell;						\
									\
    new_delay_cons_cell = makelist(hreg);				\
    sreg = hreg+2;							\
    follow(hreg++) = makecs(sreg);					\
    /* fprintf(stderr, "NEGATIVE DEL delayreg = %p\n", delayreg); */	\
    follow(hreg++) = (delayreg == NULL) ? makenil : (Cell)delayreg;	\
    hreg += 3;	/* need (arity+1) new cells */				\
    new_heap_functor(sreg, delay_psc);					\
    cell(sreg) = (Cell)SUBGOAL; sreg++;					\
    cell(sreg) = (Cell)NEG_DELAY; sreg++;				\
    delayreg = (CPtr) new_delay_cons_cell;				\
}

#define delay_positively(SUBGOAL, ANSWER) { \
    Cell new_delay_cons_cell;						\
									\
    new_delay_cons_cell = makelist(hreg);				\
    sreg = hreg+2;							\
    follow(hreg++) = makecs(sreg);					\
    /* fprintf(stderr, "POSITIVE DEL delayreg = %p\n", delayreg); */	\
    follow(hreg++) = (delayreg == NULL) ? makenil : (Cell)delayreg;	\
    hreg += 3;	/* need (arity+1) new cells */				\
    new_heap_functor(sreg, delay_psc);					\
    cell(sreg) = (Cell)SUBGOAL; sreg++;					\
    cell(sreg) = (Cell)ANSWER; sreg++;					\
    delayreg = (CPtr) new_delay_cons_cell;				\
}

/*----------------------------------------------------------------------*/
/*  Definitions of delay data structures.				*/
/*----------------------------------------------------------------------*/

typedef struct IDL_list  *IDLs;
typedef struct IDL_LIST  *IDLL;
typedef struct IDE_list  *IDEs;
typedef struct IDL_usage *IDLUs;
typedef struct idl_table_entry *IDLT;
typedef struct interned_delay_element *IDE;

/*----------------------------------------------------------------------*/

typedef struct AS_info {
  IDE     pos_ide;	/* pos. ide that refers to this answer substitution */
  IDLs	  idl_list;	/* delay lists that this answer substitution has */
  SGFrame subgoal;	/* subgoal to which this answer substitution belongs */
} *ASI;

#define asi_pide(X)		(X)->pos_ide
#define asi_idl_list(X)		(X)->idl_list
#define asi_subgoal(X)		(X)->subgoal

#define create_as_info(ANS, SUBG)	\
    asi = (ASI) malloc(sizeof(struct AS_info));	\
    Delay(ANS) = (NODEptr) asi;			\
    asi_pide(asi) = NULL;			\
    asi_idl_list(asi) = NULL;			\
    asi_subgoal(asi) = SUBG

/*----------------------------------------------------------------------*/

struct interned_delay_element {
  IDE     next_ide;
  SGFrame subgoal;	/* pointer to a subgoal frame of the ide */
  NODEptr ans_subst;	/* pointer to an answer substitution leaf */
  IDLs	  idl_list;	/* delay lists that contain this ide */
} ;

#define ide_next_ide(X)		(X)->next_ide
#define ide_subgoal(X)		(X)->subgoal
#define ide_ans_subst(X)	(X)->ans_subst
#define ide_idl_list(X)		(X)->idl_list

#define is_pos_ide(X)		(ide_ans_subst(X) != NULL)
#define is_neg_ide(X)		(ide_ans_subst(X) == NULL)

/*----------------------------------------------------------------------*/

struct IDE_list {
  IDE	ide;
  IDEs	next_ide;
} ;

#define ides_ide(X)		(X)->ide
#define ides_next_ide(X)	(X)->next_ide

#define new_ide_node(IDE_TO_POINT, NEXT_IDL_NODE)	\
    new_idl = (IDEs) malloc(sizeof(struct IDE_list));	\
    ides_ide(new_idl) = IDE_TO_POINT;			\
    ides_next_ide(new_idl) = NEXT_IDL_NODE;		\
    NEXT_IDL_NODE = new_idl

#define new_unique_ide_node(IDE_TO_POINT, IDL_SO_FAR)	\
    for (idl_var = IDL_SO_FAR;					\
	 idl_var != NULL && ides_ide(idl_var) != IDE_TO_POINT;	\
	 idl_var = ides_next_ide(idl_var)) ;			\
    if (idl_var == NULL) {					\
      new_ide_node(IDE_TO_POINT, IDL_SO_FAR);			\
    }

/*----------------------------------------------------------------------*/

struct IDL_list {
  IDLT	idl_entry;
  IDLs	next_idl;
} ;

#define idl_list_idl(X)		(X)->idl_entry
#define idl_list_next(X)	(X)->next_idl

#define new_idl_list_node(NEW_IDL_TAB_ENTRY, OLD_IDL_LIST)	\
   new_idls = (IDLs) malloc(sizeof(struct IDL_list));		\
   idl_list_idl(new_idls) = NEW_IDL_TAB_ENTRY;			\
   idl_list_next(new_idls) = OLD_IDL_LIST;			\
   OLD_IDL_LIST = new_idls

#define new_unique_idl_list_node(NEW_IDL_TAB_ENTRY, IDL_LIST_SO_FAR)	\
   for (idls_var = IDL_LIST_SO_FAR;					\
	idls_var != NULL && idl_list_idl(idls_var) != NEW_IDL_TAB_ENTRY;\
	idls_var = idl_list_next(idls_var)) ;				\
   if (idls_var == NULL) {						\
     new_idl_list_node(NEW_IDL_TAB_ENTRY, IDL_LIST_SO_FAR);		\
   }

/*----------------------------------------------------------------------*/

struct IDL_usage {
  NODEptr asl;
  IDLUs	  next_usage;
} ;

#define idlu_asl(X)		(X)->asl
#define idlu_next_usage(X)	(X)->next_usage

#define record_new_idl_usage(OLD_IDLU, ANS)	\
   new_idlu = (IDLUs) malloc(sizeof(struct IDL_usage));	\
   idlu_asl(new_idlu) = ANS;				\
   idlu_next_usage(new_idlu) = OLD_IDLU;		\
   OLD_IDLU = new_idlu

#define record_new_idl_usage_uniquely(OLD_IDLU, ANS)	\
    for (idlu_var = OLD_IDLU;				\
	 idlu_var != NULL && idlu_asl(idlu_var) != ANS;	\
	 idlu_var = idlu_next_usage(idlu_var)) ;	\
    if (idlu_var == NULL) {				\
      record_new_idl_usage(OLD_IDLU, ANS);		\
    }

/*----------------------------------------------------------------------*/

/*
struct IDL_LIST {
  IDLs idls;
  IDLL next_idll;
} ;

#define idll_idls(X)		(X)->idls
#define idll_next_idll(X)	(X)->next_idll

#define create_new_idll(APPEARS_IN, OLD_IDLL)	\
   new_idll = (IDLL) malloc(sizeof(struct IDL_LIST));	\
   idll_idls(new_idll) = APPEARS_IN;			\
   idll_next_idll(new_idll) = OLD_IDLL;			\
   OLD_IDLL = new_idll

#define create_new_unique_idll(APPEARS_IN, IDLL_SO_FAR)	\
    for (idll_var = IDLL_SO_FAR;				\
	 idll_var != NULL && idll_idls(idll_var) != APPEARS_IN;	\
	 idll_var = idll_next_idll(idll_var)) ;			\
    if (idll_var == NULL) {					\
      create_new_idll(APPEARS_IN, IDLL_SO_FAR);			\
    }
 */
/*----------------------------------------------------------------------*/

struct idl_table_entry {
  IDLT  next_idlt;
  IDEs	idl;	/* points to the current delay list of the entry */
  IDLUs	usage;	/* list of AS of the answers where this IDL is used */
} ;

#define idlt_next_idlt(X)	(X)->next_idlt
#define idlt_idl(X)		(X)->idl
#define idlt_usage(X)		(X)->usage

/*----------------------------------------------------------------------*/
/*  Handling of conditional answers.					*/
/*----------------------------------------------------------------------*/

#define is_conditional_answer(ANS)		(Delay(ANS) != NULL)
#define is_unconditional_answer(ANS)		(Delay(ANS) == NULL)

#define mark_conditional_answer(ANS, SUBG, NEW_IDL_TAB_ENTRY)	\
    if (is_unconditional_answer(ANS)) {				\
      create_as_info(ANS, SUBG);				\
    } else {							\
      asi = (ASI) Delay(ANS);					\
    }								\
    new_idl_list_node(NEW_IDL_TAB_ENTRY, asi_idl_list(asi));	\
    record_new_idl_usage_uniquely(idlt_usage(NEW_IDL_TAB_ENTRY), ANS)
#define unmark_conditional_answer(ANS) /*-- NEEDS CHANGE --*/	\
    Delay(ANS) = NULL

/*----------------------------------------------------------------------*/
/*  Checks whether a delay element that is about to be interned was	*/
/*  simplifiable (simplifications were already initiated for this DE).	*/
/*  More specifically, negative delay elements were simplifiable if	*/
/*  their subgoal failed (was completed without any answers), while	*/
/*  positive delay elements are simplifiable if their answer		*/
/*  substitution became unconditional.					*/
/*----------------------------------------------------------------------*/

#define was_simplifiable(SUBG, ANS)	\
    ((ANS == NULL) ? (is_completed(SUBG) && subgoal_fails(SUBG))	\
    		   : (is_unconditional_answer(ANS)))

#define is_failing_delay_element(SUBG, ANS)	\
    ((ANS == NULL) ? (is_completed(SUBG) && has_answer_code(SUBG) &&	\
		      subgoal_unconditionally_succeeds(SUBG))	\
		   : (DelFlag(ANS) == 1))

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#define most_general_answer(ANS)	is_escape_node(ANS)

/*----- Variables used in other parts of the system --------------------*/

extern bool neg_delay;
extern int  ide_count, idl_count;
extern int  ide_chk_ins, idl_chk_ins;

/*----- Procedures used in other parts of the system -------------------*/

#ifdef IDE_TABLE_DEBUG
extern void print_ide_tab(FILE *);
extern void print_idl_tab(FILE *);
#endif

extern bool answer_is_junk(CPtr);
extern void abolish_wfs_space(void);
extern void simplify_neg_fails(SGFrame);
extern void do_delay_stuff(NODEptr, SGFrame, bool);

/*---------------------- end of file slgdelay.h ------------------------*/
