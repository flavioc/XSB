/* File:      tr_utils.h
** Author(s): Prasad Rao, Kostis Sagonas
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

#ifndef __TR_UTILS_H__
#define __TR_UTILS_H__

extern VariantSF get_variant_sf(Cell, TIFptr, Cell *);
extern SubProdSF get_subsumer_sf(Cell, TIFptr, Cell *);
extern BTNptr get_trie_root(BTNptr);
extern VariantSF get_call(Cell, Cell *);
extern Cell build_ret_term(int, Cell[]);
extern void construct_answer_template(Cell, SubProdSF, Cell[]);
extern void breg_retskel(void);
extern void delete_predicate_table(TIFptr);
extern void reclaim_del_ret_list(VariantSF);
extern void delete_return(BTNptr, VariantSF);
extern void init_newtrie(void);
extern void delete_branch(BTNptr, BTNptr *);
extern void safe_delete_branch(BTNptr);
extern void undelete_branch(BTNptr);
extern void reclaim_uninterned_nr(long rootidx);
extern void delete_trie(BTNptr);

extern xsbBool has_unconditional_answers(VariantSF);

extern Integer  newtrie(void);
extern void trie_intern(void);
extern int  trie_interned(void);
extern void trie_dispose(void);
extern void trie_dispose_nr(void);
extern void delete_interned_trie(Integer);
extern void trie_undispose(long, BTNptr);
extern xsbBool check_table_cut;

/* Prasad's changes */

typedef struct InternGarbageLeafFrame *IGLptr;
typedef struct InternGarbageRootFrame *IGRptr;

typedef struct InternGarbageLeafFrame{
  BTNptr leaf;
  IGLptr next;  
} InternGarbageLeaf;

typedef struct InternGarbageRootFrame{
  long root;
  IGLptr leaves;
  IGRptr next;
} InternGarbageRoot;

#endif /* __TR_UTILS_H__ */
