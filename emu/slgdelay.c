/* File:      slgdelay.c
** Author(s): Kostis Sagonas
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

/*----------------------------------------------------------------------*/

#ifdef DEBUG
#define IDE_TABLE_DEBUG
#endif

#ifdef IDE_TABLE_DEBUG
extern void print_subgoal(FILE *, SGFrame);
#endif


static void simplify_neg_succeeds(SGFrame);
static void simplify_pos_unsupported(NODEptr);
static void simplify_pos_unconditional(NODEptr);

/*----------------------------------------------------------------------*/
/*  Global variables defined for delay element table and delay list	*/
/*  table manipulation.							*/
/*----------------------------------------------------------------------*/

#define MAX_IDL_NUM	10177
#define MAX_IDE_NUM	10177

int ide_count = 0;
int ide_chk_ins = 0;

static struct interned_delay_element **ide_tab;

int idl_count = 0;
int idl_chk_ins = 0;

static struct idl_table_entry **idl_tab;

/*----------------------------------------------------------------------*/
/* Routines defined for internal debugging.                             */
/*----------------------------------------------------------------------*/

#ifdef IDE_TABLE_DEBUG
static void fprint_ide(FILE *fp, IDE ide)
{
    fprintf(fp, " < ");
    print_subgoal(fp, ide_subgoal(ide));
    fprintf(fp, ", %p", ide_ans_subst(ide));
    fprintf(fp, " > ");
}

static void fprint_idl(FILE *fp, IDEs idl)
{
    fprintf(fp, "[");
    while (idl != NULL) {
      fprintf(fp, "%p", ides_ide(idl));
      if ((idl = ides_next_ide(idl)) != NULL) fprintf(fp, ", ");
    }
    fprintf(fp, "]");
}

void print_ide_tab(FILE *fp)
{
    int  i,j;
    IDLs idl;
    IDE  ide;

    fprintf(fp, "<======================= IDE Table ======================>\n");
    if (ide_count) {
      for (i = 0; i < MAX_IDE_NUM; i++) {
	ide = (IDE)(*(ide_tab + i));
	if (ide != NULL) {
	  fprintf(fp, "Bucket #%4d:", i);
	  while (ide != NULL) {
	    fprint_ide(fp, ide);
	    for (j = 0, idl = ide_idl_list(ide);
		 idl != NULL; j++, idl = idl_list_next(idl))
	      /* fprintf(fp, " --> %p", idl_list_idl(idl)) */;
	    fprintf(fp, " (used in %d IDLs)\n", j);
	    if ((ide = ide_next_ide(ide)) != NULL) fprintf(fp, "\t     ");
	  }
	}
      }
    }
    fprintf(fp, "<========================================================>\n");
}

void print_idl_tab(FILE *fp)
{
    int   i;
    IDLT  idlt;
    IDLUs idlu;

    fprintf(fp, "<======================= IDL Table ======================>\n");
    if (idl_count) {
      for (i = 0; i < MAX_IDL_NUM; i++) {
	idlt = (IDLT)(*(idl_tab + i));
	if (idlt != NULL) {
	  fprintf(fp, "Bucket #%4d: ", i);
	  while (idlt != NULL) {
	    fprint_idl(fp, idlt_idl(idlt));
	    fprintf(fp, " IDL of ");
	    for (idlu = idlt_usage(idlt);
		 idlu != NULL;
		 idlu = idlu_next_usage(idlu))
	      fprintf(fp, "%p  ", idlu_asl(idlu));
	    fprintf(fp, "\n");
	    if ((idlt = idlt_next_idlt(idlt)) != NULL) fprintf(fp, "\t      ");
	  }
	}
      }
    }
    fprintf(fp, "<========================================================>\n");
}
#endif

/*----------------------------------------------------------------------*/
/* Memory management variables, routines, and macros.                   */
/*----------------------------------------------------------------------*/

#define idet_alloc_chunk_size (512 * sizeof(struct interned_delay_element))
#define idlt_alloc_chunk_size (1024 * sizeof(struct idl_table_entry))

char *idet_space_chunk_ptr = NULL;
char *idlt_space_chunk_ptr = NULL;

static IDE  free_idets = NULL, free_idet_space = NULL, top_idet_space = NULL;
static IDLT free_idlts = NULL, free_idlt_space = NULL, top_idlt_space = NULL;


static IDE alloc_more_idet_space(void)
{
    char *t;

    if ((t = (char *)malloc(idet_alloc_chunk_size+sizeof(Cell))) == NULL)
      xsb_abort("No space to allocate more IDE table entries");
    *(char **)t = idet_space_chunk_ptr;
    idet_space_chunk_ptr = t;
    free_idet_space = (IDE)(t+sizeof(Cell));
    top_idet_space = (IDE)(t+idet_alloc_chunk_size+sizeof(Cell));
    return free_idet_space++;
}

#define New_IDE_Entry(theIDE,theSUBG,theANS_SUBST) \
    if (free_idets) {\
      theIDE = free_idets; \
      free_idets = ide_next_ide(free_idets); \
    } else { \
      if (free_idet_space < top_idet_space) { \
	theIDE = free_idet_space++; \
      } else { \
        theIDE = alloc_more_idet_space(); \
      } \
    } \
    ide_next_ide(theIDE) = NULL; \
    ide_subgoal(theIDE) = theSUBG; \
    ide_ans_subst(theIDE) = theANS_SUBST; \
    ide_idl_list(theIDE) = NULL;


static IDLT alloc_more_idlt_space(void)
{
    char *t;

#ifdef IDE_TABLE_DEBUG
    fprintf(stderr, "+++ Allocating more IDL space...\n");
#endif
    if ((t = (char *)malloc(idlt_alloc_chunk_size+sizeof(Cell))) == NULL)
      xsb_abort("No space to allocate more IDL table entries");
    *(char **)t = idlt_space_chunk_ptr;
    idlt_space_chunk_ptr = t;
    free_idlt_space = (IDLT)(t+sizeof(Cell));
    top_idlt_space = (IDLT)(t+idlt_alloc_chunk_size+sizeof(Cell));
    return free_idlt_space++;
}

#define New_IDLT_Entry(theIDLT,theIDL) \
    if (free_idlts) {\
      theIDLT = free_idlts; \
      free_idlts = idlt_next_idlt(free_idlts); \
    } else { \
      if (free_idlt_space < top_idlt_space) { \
	theIDLT = free_idlt_space++; \
      } else { \
        theIDLT = alloc_more_idlt_space(); \
      } \
    } \
    idlt_next_idlt(theIDLT) = NULL; \
    idlt_idl(theIDLT) = theIDL; \
    idlt_usage(theIDLT) = NULL;


void abolish_wfs_space(void)
{
    char *t;
#ifndef LOCAL_EVAL
    extern void abolish_edge_space();
#endif

  /* abolish IDE Table and IDL Table */
    if (ide_tab) { free(ide_tab); ide_tab = NULL; }
    if (idl_tab) { free(idl_tab); idl_tab = NULL; }
    ide_count = idl_count = 0;

    while (idet_space_chunk_ptr) {
      t = *(char **)idet_space_chunk_ptr;
      free(idet_space_chunk_ptr);
      idet_space_chunk_ptr = t;
    }
    free_idets = free_idet_space = top_idet_space = NULL;
    while (idlt_space_chunk_ptr) {
      t = *(char **)idlt_space_chunk_ptr;
      free(idlt_space_chunk_ptr);
      idlt_space_chunk_ptr = t;
    }
    free_idlts = free_idlt_space = top_idlt_space = NULL;

#ifndef LOCAL_EVAL
    abolish_edge_space();
#endif
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

static IDE intern_delay_element(Cell delay_elem)
{
    Cell    bucket;
    IDE     idet_entry, ide;
    CPtr    cptr = (CPtr)cs_val(delay_elem);
    SGFrame subgoal = (SGFrame)cell(cptr+1);
    NODEptr ans_subst = (NODEptr)cell(cptr+2);

    if (!was_simplifiable(subgoal, ans_subst)) {
      if (ide_count == 0) {
	ide_tab = calloc(1, MAX_IDE_NUM * sizeof(IDE));
	if (!ide_tab) xsb_exit("No memory for allocating IDE Table");
      }
      ide_chk_ins++;
      bucket = ( ans_subst ? ihash(ans_subst, MAX_IDE_NUM)
			   : ihash(subgoal, MAX_IDE_NUM) );
#ifdef SERIOUS_PERFORMANCE_DEBUG
      fprintf(stderr, "IDE Bucket # is %d\t(subg = %p, ans = %p)\n",
		      bucket, subgoal, ans_subst);
#endif
      idet_entry = (IDE)(ide_tab + bucket);
      for (ide = *((IDE *)idet_entry);
	   ide != NULL;
	   idet_entry = ide, ide = ide_next_ide(ide)) {
	if (ide_subgoal(ide) == subgoal &&
	    ide_ans_subst(ide) == ans_subst) {
#ifdef SERIOUS_IDE_TABLE_DEBUG
	  fprintf(stderr, "++Delay element found already interned\n");
#endif
	  return ide;
	}
      }
      /* Does not exist in table: Insert it */
      New_IDE_Entry(ide, subgoal, ans_subst);
      *((IDE *)idet_entry) = ide;
#ifdef SERIOUS_IDE_TABLE_DEBUG
      fprintf(stderr, "++Delay element is new and now interned at %p\n", ide);
#endif
      ide_count++;
      return ide;
    } else { /* was simplifiable */
      return NULL;
    }
}

/*----------------------------------------------------------------------*/

static bool idlcmp(IDEs idl1, IDEs idl2)
{
   for ( ; idl1 != NULL && idl2 != NULL;
	idl1 = ides_next_ide(idl1), idl2 = ides_next_ide(idl2))
     if (ides_ide(idl1) != ides_ide(idl2)) return FALSE;
   return (idl1 == NULL && idl2 == NULL);
}

/*----------------------------------------------------------------------*/

static IDLT intern_delay_list(CPtr dlist) /* assumes that dlist != NULL	*/
{
    Cell bucket = 0;
    IDE  ide = NULL;
    IDEs idl = NULL;
    IDLT idlt_entry, idlt;
    IDEs new_idl, idl_var;

    /*--- Intern the elements of the DL and create the IDL ---*/
#ifdef _DEBUG
    fprintf(stderr, "IDL = [");
#endif
    while (islist(dlist)) {
      dlist = clref_val(dlist);
      if ((ide = intern_delay_element(cell(dlist))) != NULL) {
#ifdef _DEBUG
       fprintf(stderr, " %p", ide);
#endif
	bucket += ((Cell)ide % 10177);
	new_unique_ide_node(ide, idl);
      }
      dlist = (CPtr)cell(dlist+1);
    }
#ifdef _DEBUG
    fprintf(stderr, " ] ... ");
#endif
    if (idl != NULL) {	/* Not all delay literals of the DL were simplifiable */
      if (idl_count == 0) {
	idl_tab = calloc(1, MAX_IDL_NUM * sizeof(IDLT));
	if (!idl_tab) xsb_exit("No memory for allocating IDL Table");
      }
      idl_chk_ins++;
      bucket = ihash(bucket, MAX_IDL_NUM);
#ifdef SERIOUS_PERFORMANCE_DEBUG
      fprintf(stderr, "IDL Bucket # is %ld\n", bucket);
#endif
      /*--- Insert the created IDL in the IDL Table ---*/
      idlt_entry = (IDLT)(idl_tab + bucket);
      for (idlt = *((IDLT *)idlt_entry);
	   idlt != NULL;
	   idlt_entry = idlt, idlt = idlt_next_idlt(idlt)) {
	if (idlcmp(idlt_idl(idlt), idl)) return idlt;
      }
      /* Does not exist in table: Insert it */
      New_IDLT_Entry(idlt, idl);
      *((IDLT *)idlt_entry) = idlt;
      idl_count++;
      return idlt;
    } else return NULL;
}

/*----------------------------------------------------------------------*/

static void record_ide_usage(IDLT idl_tab_entry)
{
    IDE	    ide;
    IDEs    ides;
    NODEptr as_leaf;
    IDLs    new_idls, idls_var;

    for (ides = idlt_idl(idl_tab_entry);
	 ides != NULL; ides = ides_next_ide(ides)) {
      ide = ides_ide(ides);
      new_unique_idl_list_node(idl_tab_entry, ide_idl_list(ide));
      if ((as_leaf = ide_ans_subst(ide)) == NULL) {  /* is negative IDE */
#ifdef SERIOUS_IDE_TABLE_DEBUG
	fprintf(stderr, "\tNegative ide");
	fprint_ide(stderr, ide);
	fprintf(stderr, "interned at %p\n", ide);
#endif
	subg_nide(ide_subgoal(ide)) = ide;
      } else {
#ifdef SERIOUS_IDE_TABLE_DEBUG
	fprintf(stderr, "\tPositive ide");
	fprint_ide(stderr, ide);
	fprintf(stderr, "interned at %p\n", ide);
#endif
	asi_pide(((ASI)Delay(as_leaf))) = ide;
      }
    }
}

/*----------------------------------------------------------------------*/

void do_delay_stuff(NODEptr as_leaf, SGFrame subgoal, bool sf_exists)
{
    ASI	  asi;
    IDLs  new_idls;
    IDLUs new_idlu, idlu_var;
    IDLT  idl_tab_entry = NULL;

    if (delayreg != NULL && (!sf_exists || is_conditional_answer(as_leaf))) {
      if ((idl_tab_entry = intern_delay_list(delayreg)) != NULL) {
	mark_conditional_answer(as_leaf, subgoal, idl_tab_entry);
	record_ide_usage(idl_tab_entry);
      }
    }
    /*--- Check for the derivation of an unconditional answer ---*/
    if (sf_exists && is_conditional_answer(as_leaf) &&
	(delayreg == NULL || idl_tab_entry == NULL)) {
#ifdef __SIMPLIFICATION_DEBUG
      fprintf(stderr, "----------------------------------------------------\n");
#ifdef IDE_TABLE_DEBUG
      fprintf(stderr, "Subgoal "); print_subgoal(stderr, subgoal);
#endif
      fprintf(stderr,
	      "had conditional answers... now derived an unconditional one\n");
      if (asi_pide(((ASI)Delay(as_leaf))) != NULL)
	fprintf(stderr, "\t(already returned in interned delay lists)\n");
      else fprintf(stderr, "\t(not interned yet)\n");
      fprintf(stderr, "----------------------------------------------------\n");
#endif
    /*-- Initiate positive simplification in places where
	 this answer substitution has already been returned --*/
      simplify_pos_unconditional(as_leaf);
    }
    if (is_unconditional_answer(as_leaf) && subg_nide(subgoal) != NULL) {
      simplify_neg_succeeds(subgoal);
    }
}

/*----------------------------------------------------------------------*/

bool answer_is_junk(CPtr dlist)		  /* assumes that dlist != NULL */
{
    CPtr    cptr;
    SGFrame subgoal;
    NODEptr ans_subst;

    while (islist(dlist)) {
      dlist = clref_val(dlist);
      cptr = (CPtr)cs_val(cell(dlist));
      subgoal = (SGFrame)cell(cptr+1);
      ans_subst = (NODEptr)cell(cptr+2);
      if (is_failing_delay_element(subgoal,ans_subst)) {
	return TRUE;
      }
      dlist = (CPtr)cell(dlist+1);
    }
    return FALSE;
}

/*----------------------------------------------------------------------*/
/*  From here on simplification starts...				*/
/*----------------------------------------------------------------------*/

static bool remove_ide_from_idl(IDE ide, IDLT *idl_tab_entry)
{
    IDEs idl = idlt_idl(*idl_tab_entry);
    IDEs prev_idl = NULL;

#ifdef __SIMPLIFICATION_DEBUG
#ifdef IDE_TABLE_DEBUG
    print_ide_tab(stderr); print_idl_tab(stderr);
#endif
#endif
#ifdef PROFILE
    subinst_table[SIMPL_REMOVE_DE][1]++;
#endif 
    for ( ; ides_ide(idl) != ide; prev_idl = idl, idl = ides_next_ide(idl))
      ;
#ifdef __SIMPLIFICATION_DEBUG
    if (ides_ide(idl) == ide) {
      fprintf(stderr, "! FOUND (idl = %p, next = %p)\n",idl,ides_next_ide(idl));
    }
#endif
    if (prev_idl != NULL) { ides_next_ide(prev_idl) = ides_next_ide(idl); }
    if (idl == idlt_idl(*idl_tab_entry)) {
      idlt_idl(*idl_tab_entry) = ides_next_ide(idl);
    }
    return (bool) idlt_idl(*idl_tab_entry);
}

/*----------------------------------------------------------------------*/

static bool remove_idl_from_idl_list(IDLT idl_tab_entry, ASI *asi)
{
    IDLs idls = asi_idl_list(*asi);
    IDLs prev_idls = NULL;

#ifdef __SIMPLIFICATION_DEBUG
#ifdef IDE_TABLE_DEBUG
    print_ide_tab(stderr); print_idl_tab(stderr);
#endif
#endif
#ifdef PROFILE
    subinst_table[SIMPL_REMOVE_DL][1]++;
#endif 
    for ( ; idl_list_idl(idls) != idl_tab_entry; idls = idl_list_next(idls))
      prev_idls = idls;
    if (prev_idls != NULL) { idl_list_next(prev_idls) = idl_list_next(idls); }
    if (idls == asi_idl_list(*asi)) {
      asi_idl_list(*asi) = idl_list_next(idls);
    }
    return (bool) asi_idl_list(*asi);
}

/*----------------------------------------------------------------------*/

static void handle_empty_idl_creation(IDLT *idl_tab_entry)
{
    IDLUs   idlu = idlt_usage(*idl_tab_entry);
    NODEptr as_leaf;
    SGFrame subgoal;

/*  print_ide_tab(stderr); print_idl_tab(stderr); */

#ifdef __SIMPLIFICATION_DEBUG
    fprintf(stderr, "Resulting IDL (%p) is empty; ", *idl_tab_entry);
    fprintf(stderr, "further simplifications to follow...\n");
#endif
    for ( ; idlu != NULL; idlu = idlu_next_usage(idlu)) {
      as_leaf = idlu_asl(idlu);
      if (is_conditional_answer(as_leaf)) { /* if it is still conditional */
	subgoal = asi_subgoal((ASI) Delay(as_leaf));
	simplify_pos_unconditional(as_leaf);
	/*--- perform early completion if necessary ---*/
	if (!is_completed(subgoal) && most_general_answer(as_leaf)) {
/*
	  if (subg_compl_susp_ptr(subgoal) != NULL) {
	    fprintf(stderr, "SUBGOAL STILL HAS NEGATION SUSPENSIONS\n");
	  }
 */
/*	  fprintf(stderr, "EARLY COMPLETION SHOULD HAPPEN HERE\n"); */
	  tcp_pcreg(subg_cp_ptr(subgoal)) = (byte *) &check_complete_inst;
	  mark_as_completed(subgoal);
	}
	simplify_neg_succeeds(subgoal);
      }
    }
#ifdef __SIMPLIFICATION_DEBUG
    fprintf(stderr, "about to forget usage... (current = %p, ",
		    idlt_usage(*idl_tab_entry));
#endif
    idlt_usage(*idl_tab_entry) = NULL;	/* forget these uses */
#ifdef __SIMPLIFICATION_DEBUG
    fprintf(stderr, " new = %p)\n", idlt_usage(*idl_tab_entry));
#endif
}

/*----------------------------------------------------------------------*/

static void handle_unsupported_answer_subst(NODEptr *as_leaf)
{
    ASI     unsup_asi = (ASI) Delay(*as_leaf);
    SGFrame unsup_subgoal = asi_subgoal(unsup_asi);

#ifdef __SIMPLIFICATION_DEBUG
    fprintf(stderr, "Answer substitution of %s subgoal ",
		    (is_completed(unsup_subgoal) ? "completed" : "incomplete"));
    print_subgoal(stderr, unsup_subgoal);
    fprintf(stderr, " has no supported conditional answers;\n");
    fprintf(stderr, "further simplifications to follow...\n");
#endif
    delete_branch(*as_leaf, (CPtr)&subg_ans_root_ptr(unsup_subgoal));
    simplify_pos_unsupported(*as_leaf);
    if (is_completed(unsup_subgoal)) {
      if (subgoal_fails(unsup_subgoal)) {
	mark_subgoal_failed(unsup_subgoal);
	simplify_neg_fails(unsup_subgoal);
      }
    }
}

/*----------------------------------------------------------------------*/
/*  simplify_pos_unconditional(AnswerSubstitution)			*/
/*	When the AnswerSubstitution gets an unconditional answer, it	*/
/*	removes the positive delay element of an AnswerSubstitution	*/
/*	from the delay lists that contain it.				*/
/*----------------------------------------------------------------------*/

static void simplify_pos_unconditional(NODEptr as_leaf)
{
    IDLs idls;
    IDLT idl_tab_entry;
    ASI  asi = (ASI) Delay(as_leaf);	/* never NULL when here */
    IDE  pide = asi_pide(asi);		/* so this is always OK */

    /*-- First forget the delay lists of this answer substitution --*/
    unmark_conditional_answer(as_leaf);

#ifdef __SIMPLIFICATION_DEBUG
    fprintf(stderr, "About to simplify pos. DE of AnsSubst %p...\n", as_leaf);
#endif
    if (pide != NULL) {
      for (idls = ide_idl_list(pide); idls!=NULL; idls = idl_list_next(idls)) {
	idl_tab_entry = idl_list_idl(idls);
	if (idlt_idl(idl_tab_entry) != NULL) {
	  if (!remove_ide_from_idl(pide, &idl_tab_entry)) {
	    handle_empty_idl_creation(&idl_tab_entry);
	  }
	}
      }
      ide_idl_list(pide) = NULL;	/* Forget places containing pide */
    }
}

/*----------------------------------------------------------------------*/
/*  simplify_neg_fails(SubGoal)						*/
/*	When the SubGoal fails (is completed without any answers), it	*/
/*	removes the negative delay element of SubGoal from the delay	*/
/*	lists that contain it.						*/
/*----------------------------------------------------------------------*/

void simplify_neg_fails(SGFrame subgoal)
{
    IDLs idls;
    IDLT idl_tab_entry;
    IDE	 nide = subg_nide(subgoal);

#ifdef __SIMPLIFICATION_DEBUG
    if (nide != NULL) {
      fprintf(stderr, "About to simplify failing neg. DE of subgoal ");
#ifdef IDE_TABLE_DEBUG
      print_subgoal(stderr, subgoal);
#endif
      fprintf(stderr, " ...\n");
    }
#endif
    if (nide != NULL) {
      subg_nide(subgoal) = NULL;	/* forget this nide */
      for (idls = ide_idl_list(nide); idls!=NULL; idls = idl_list_next(idls)) {
	idl_tab_entry = idl_list_idl(idls);
	if (idlt_idl(idl_tab_entry) != NULL) {
	  if (!remove_ide_from_idl(nide, &idl_tab_entry)) {
	    handle_empty_idl_creation(&idl_tab_entry);
	  }
	}
      }
      ide_idl_list(nide) = NULL;	/* Forget places containing nide */
    }
}

/*----------------------------------------------------------------------*/
/*  simplify_neg_succeeds(SubGoal)					*/
/*	On occasion that the SubGoal succeeds (gets an unconditional	*/
/*	answer that is identical to the subgoal), it deletes all delay	*/
/*	lists that contain the negative delay element of that SubGoal.	*/
/*----------------------------------------------------------------------*/

static void simplify_neg_succeeds(SGFrame subgoal)
{
    IDLs    idls;
    IDLUs   idlu;
    ASI     used_asi;
    NODEptr used_as_leaf;
    IDLT    idl_tab_entry;
    IDE	    nide = subg_nide(subgoal);

#ifdef __SIMPLIFICATION_DEBUG
    if (nide != NULL) {
      fprintf(stderr, "About to simplify successful neg. DE of subgoal ");
#ifdef IDE_TABLE_DEBUG
      print_subgoal(stderr, subgoal);
#endif
      fprintf(stderr, " ...\n");
    }
#endif
    if (nide != NULL) {
      subg_nide(subgoal) = NULL;	/* forget this nide */
      for (idls = ide_idl_list(nide); idls!=NULL; idls = idl_list_next(idls)) {
	idl_tab_entry = idl_list_idl(idls);
	for (idlu = idlt_usage(idl_tab_entry);
	     idlu != NULL;
	     idlu = idlu_next_usage(idlu)) {
	  used_as_leaf = idlu_asl(idlu);
	  if (is_not_deleted(used_as_leaf) &&
	      (used_asi = (ASI) Delay(used_as_leaf)) != NULL) {
	    if (!remove_idl_from_idl_list(idl_tab_entry, &used_asi)) {
	      handle_unsupported_answer_subst(&used_as_leaf);
	    }
	  }
	}
      }
      ide_idl_list(nide) = NULL;	/* forget usage of this ide */
    }
}

/*----------------------------------------------------------------------*/
/*  simplify_pos_unsupported(AnswerSubstitution)			*/
/*	On occasion that an AnswerSubstitution looses all its		*/
/*	conditional answers, it deletes all delay lists that contain	*/
/*	the positive delay element pointing to that AnswerSubstitution.	*/
/*----------------------------------------------------------------------*/

static void simplify_pos_unsupported(NODEptr as_leaf)
{
    IDLs    idls;
    IDLUs   idlu;
    ASI     used_asi;
    NODEptr used_as_leaf;
    IDLT    idl_tab_entry;
    ASI     asi = (ASI) Delay(as_leaf);	/* never NULL when here */
    IDE     pide = asi_pide(asi);	/* so this is always OK */

#ifdef __SIMPLIFICATION_DEBUG
    if (pide != NULL) {
      fprintf(stderr,
	      "About to simplify unsupported pos. DE of AnsSubst %p...\n",
	      as_leaf);
    }
#endif
    if (pide != NULL) {
      for (idls = ide_idl_list(pide); idls!=NULL; idls = idl_list_next(idls)) {
	idl_tab_entry = idl_list_idl(idls);
	for (idlu = idlt_usage(idl_tab_entry);
	     idlu != NULL;
	     idlu = idlu_next_usage(idlu)) {
	  used_as_leaf = idlu_asl(idlu);
	  if (is_not_deleted(used_as_leaf) &&
	      (used_asi = (ASI) Delay(used_as_leaf)) != NULL) {
	    if (!remove_idl_from_idl_list(idl_tab_entry, &used_asi)) {
	      handle_unsupported_answer_subst(&used_as_leaf);
	    }
	  }
	}
      }
      ide_idl_list(pide) = NULL;	/* forget usage of this ide */
    }
}

/*---------------------- end of file slgdelay.c ------------------------*/
