/* File:      tries.h
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


/* mark node as deleted */
#define mark_leaf_node_del(X)  DelFlag(X) = 1
#define del_ret_node(X)  DelFlag(aln_answer_ptr(X)) = 1
#define is_del_ret_node(X)  DelFlag(aln_answer_ptr(X)) == 1 

#define is_not_deleted(X)      DelFlag(X) != 1

struct HASHhdr {
  struct HASHhdr *next, *prev;
  Integer numInHash;
  Cell HASHmask;
};

/*----------------------------------------------------------------------*/

typedef struct NODE *NODEptr;
struct NODE {
  byte instr;
  byte nreg;
  byte del_flag;
  byte node_type;
#ifdef BITS64
  byte padding[4];
#endif
  NODEptr sibling;
  NODEptr child;
  NODEptr parent;
  Cell atom;
};

#define Instr(X)	((X)->instr)
#define Nregs(X)	((X)->nregs)
#define DelFlag(X)	((X)->del_flag)
#define NodeType(X)     ((X)->node_type)
#define Child(X)	((X)->child)
#define Sibl(X)		((X)->sibling)
#define Atom(X)		((X)->atom)
#define Parent(X)	((X)->parent)

#define NODE_TYPE_UNKNOWN ((byte)0)
#define NODE_TYPE_ANSWER_LEAF  ((byte)1)
#define is_answer_leaf(x) (NodeType(x) == NODE_TYPE_ANSWER_LEAF)

#define Delay(X)	Child(X)

#define is_escape_node(X)	(Instr(X) == trie_proceed)

/*----------------------------------------------------------------------*/

typedef struct answer_list_node *ALPtr;
struct answer_list_node {
  NODEptr answer_ptr;
  ALPtr   next_aln;
} ;

#define aln_answer_ptr(ALN)	(ALN)->answer_ptr
#define aln_next_aln(ALN)	(ALN)->next_aln

/*----------------------------------------------------------------------*/

#define NOPAR NULL

#define makeftag(X)	(NODEptr)(((Cell)(X)) | 0x1)
#define ftagged(X)	((Cell)(X) & 0x1)
#define unftag(X)	(NODEptr)(((Cell)(X)) & ~0x1)

/*----------------------------------------------------------------------*/
/* when table chain exceeds LENGTHLIMIT, a hash table is made */
#define LENGTHLIMIT 9

/* initial hashtab size; must be power of 2 and > LENGTHLIMIT*/
#define HASHLENGTH 64

#define HASH(X,HASHMASK) ((cell_tag((Cell)X) == TrieVar)?0:((X>>4) & HASHMASK))

/*-- exported trie functions ------------------------------------------*/

extern byte *	trie_get_calls(void);
extern int	free_trie_size(void);
extern int	allocated_trie_size(void);
extern int	allocated_trie_hash_size(void);
extern byte *	trie_get_returns_for_call(void);
extern void	abolish_trie(void);
extern void	aux_call_info(void);
extern void	prolog_newnode(void);
extern void	remove_open_tries(CPtr);
extern void     init_trie_aux_areas(void);
extern void	get_lastnode_cs_retskel(void);
extern void     load_solution_trie(int, CPtr, NODEptr);
extern bool     variant_call_search(int, CPtr, CPtr *);
extern NODEptr  one_term_chk_ins(CPtr,CPtr,int *);
extern NODEptr  whole_term_chk_ins(Cell, CPtr, int *);
extern NODEptr	get_next_trie_solution(ALPtr *);
extern NODEptr	variant_trie_search(int, CPtr, CPtr, int *);
extern NODEptr  delay_chk_insert(int, CPtr, CPtr *);
extern void     undo_answer_bindings(void);
extern void	load_delay_trie(int, CPtr, NODEptr);
extern void     bottom_up_unify(void);

/*---------------------------------------------------------------------*/

/* slg variables */
extern CPtr VarPosReg;
extern CPtr ans_var_pos_reg;
extern int  num_vars_in_var_regs;
extern int  global_num_vars;

/* used for statistics */
extern long subg_chk_ins, subg_inserts, ans_chk_ins, ans_inserts;

/* trie routine variables */
extern NODEptr free_trie_nodes, Last_Nod_Sav, Paren;
extern ALPtr   free_answer_list_nodes;

/* registers for trie backtracking */
extern CPtr reg_arrayptr, var_regs[];

/*----------------------------------------------------------------------*/
/* allocate an array for easy expansion */ 
#define alloc_arr(AArrType,AArrayNam,AArraySz){\
    AArrayNam = (AArrType *)malloc(sizeof(AArrType) * AArraySz);\
    if (AArrayNam == NULL) {\
      xsb_exit("No More memory for reallocating Array");\
    }\
}

/* expand the array by doubling its size */
#define trie_expand_array(ArrType,ArrayNam, ArraySz, Nam) {\
    ArrType *Temp;\
    int Siz;\
    int i;\
\
    Temp = ArrayNam;\
    Siz  = ArraySz;\
    ArraySz = 2 * ArraySz;\
    alloc_arr(ArrType,ArrayNam,ArraySz);\
    for (i = 0; i < Siz; i++) {\
      ArrayNam[i] = Temp[i];\
    }\
    free(Temp);\
}

#define will_overflow_reg_array(x) {\
   if (x >= reg_array+reg_array_size) {\
     int idx = reg_arrayptr - reg_array;\
     trie_expand_array(Cell,reg_array,reg_array_size,"reg_array");\
     reg_arrayptr = reg_array + idx;\
   }\
}

#define pushreg(X) {\
   will_overflow_reg_array(reg_arrayptr+1);\
   (*(++reg_arrayptr)) = (Cell) X;\
}
/*----------------------------------------------------------------------*/

extern struct HASHhdr HASHroot;
extern struct HASHhdr *HASHrootptr;

extern CPtr *var_addr;
extern int  var_addr_arraysz;

extern Cell VarEnumerator[];
extern int  num_heap_term_vars;

/*----------------------------------------------------------------------*/
/* has to go into trie_code.i --- but for now */
#define is_no_cp(x) (((Cell)Instr(x) & 0x3)== 0)
#define is_trust(x) (((Cell)Instr(x) & 0x3)== 1)
#define is_try(x)   (((Cell)Instr(x) & 0x3)== 2)
#define is_retry(x) (((Cell)Instr(x) & 0x3)== 3)

/*----------------------------------------------------------------------*/

#define dbind_ref_nth_var(addr,n) dbind_ref(addr,VarEnumerator[n])
#define trie_var_num(x)  ((((Cell) x) - VarEnumerator[0])/sizeof(Cell))
#define is_VarEnumerator(x) (!((((Cell) x) >= VarEnumerator[0]) && (((Cell) x) <= VarEnumerator[NUM_TRIEVARS-1])) )

extern Cell * reg_array;
extern int reg_array_size;
extern int delay_it;

#define NUM_TRIEVARS 400
#define DEFAULT_ARRAYSIZ 512 

extern CPtr *copy_of_var_addr;
extern int  copy_of_num_heap_term_vars;
