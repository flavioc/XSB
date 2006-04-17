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

#include "context.h"

extern VariantSF get_variant_sf(CTXTdeclc Cell, TIFptr, Cell *);
extern SubProdSF get_subsumer_sf(CTXTdeclc Cell, TIFptr, Cell *);
extern BTNptr get_trie_root(BTNptr);
extern VariantSF get_call(CTXTdeclc Cell, Cell *);
extern Cell build_ret_term(CTXTdeclc int, Cell[]);
extern void construct_answer_template(CTXTdeclc Cell, SubProdSF, Cell[]);
extern void breg_retskel(CTXTdecl);
extern void delete_predicate_table(CTXTdeclc TIFptr);
extern void reclaim_del_ret_list(CTXTdeclc VariantSF);
extern void delete_return(CTXTdeclc BTNptr, VariantSF);
extern void init_newtrie(CTXTdecl);
extern void delete_branch(CTXTdeclc BTNptr, BTNptr *);
extern void safe_delete_branch(BTNptr);
extern void undelete_branch(BTNptr);
extern void reclaim_uninterned_nr(CTXTdeclc long rootidx);
extern void delete_trie(CTXTdeclc BTNptr);
extern xsbBool is_completed_table(TIFptr);

extern xsbBool has_unconditional_answers(VariantSF);

extern Integer  newtrie(CTXTdecl);
extern void trie_intern(CTXTdecl);
extern int  trie_interned(CTXTdecl);
extern void trie_dispose(CTXTdecl);
extern void trie_dispose_nr(CTXTdecl);
extern void delete_interned_trie(CTXTdeclc Integer);
extern void trie_undispose(CTXTdeclc long, BTNptr);
// extern xsbBool check_table_cut;

extern int abolish_table_predicate(CTXTdeclc Psc);
extern int abolish_table_call(CTXTdeclc VariantSF);
extern void abolish_private_tables(CTXTdecl);
extern void abolish_shared_tables(CTXTdecl);
extern int gc_tabled_preds(CTXTdecl);
extern void delete_variant_sf_and_answers(CTXTdeclc VariantSF pSF);
extern void abolish_table_info(CTXTdecl);
extern int abolish_usermod_tables(CTXTdecl);
extern int abolish_module_tables(CTXTdeclc const char *module_name);


#endif /* __TR_UTILS_H__ */
