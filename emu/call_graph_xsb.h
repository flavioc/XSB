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
#ifndef PUBLIC_CALL_GRAPH_DEFS
#define PUBLIC_CALL_GRAPH_DEFS
#define INCR
#define CALLNODE_PER_BLOCK 10000
#define CALLLIST_PER_BLOCK 10000
#define CALL2LIST_PER_BLOCK 1000
#define KEY_PER_BLOCK 10000
#define OUTEDGE_PER_BLOCK 100
#define HASH_TABLE_SIZE 1

extern int maximum_dl,factcount;
extern calllistptr affected,changed,leader;
extern int no_add_call_edge;
extern callnodeptr old_call;
extern int callqptr;
extern int saved_call,unchanged_call;
extern BTNptr old_answer_table;
extern int call_node_count,call_edge_count;
extern void initoutedges(callnodeptr cn);
extern callnodeptr makecallnode(VariantSF);
extern void deallocatecall(callnodeptr);
extern void propagate_no_change(callnodeptr);
extern void addcalledge(callnodeptr,callnodeptr);
extern void incr_eval_statistics(CTXTdecl);
extern calllistptr eneetq();
extern void invalidate_call(callnodeptr c);
extern int create_call_list(CTXTdecl);
extern int create_changed_call_list(CTXTdecl);
extern int imm_depend_list(CTXTdeclc callnodeptr);
extern int imm_dependent_on_list(CTXTdeclc callnodeptr call1);
extern void print_call_node(callnodeptr);
extern void nq(calllistptr *,callnodeptr);
extern void abolish_incr_call(CTXTdeclc callnodeptr);
extern void free_incr_hashtables(TIFptr);
#endif
