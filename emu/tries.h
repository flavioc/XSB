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


#define exit_if_null(x) {\
  if( x == NULL){\
   printf("Malloc Failed !\n");\
   xsb_exit("Bye");\
}\
}

#define is_not_deleted(X)	DelFlag(X) != 1

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
extern int num_vars_in_var_regs;
extern int global_num_vars;

/* used for statistics */
extern long subg_chk_ins, subg_inserts, ans_chk_ins, ans_inserts;

/* trie routine variables */
extern NODEptr free_trie_nodes,
               Last_Nod_Sav,
               TrieRetPtr,               
	       Paren;
extern ALPtr   free_answer_list_nodes;



/* allocate an array for easy expansion */
 
#define alloc_arr(AArrType,AArrayNam,AArraySz){\
    AArrayNam = (AArrType *)malloc(sizeof(AArrType) * AArraySz);\
    if(AArrayNam == NULL){\
       fprintf(stderr,"No More memory for reallocating Array");\
       xsb_exit("Bye");\
    }\
}

/* expand the array */

#define xpand_array(ArrType,ArrayNam, ArraySz, Nam){\
    ArrType *Temp;\
    int Siz;\
    int i;\
\
    Temp = ArrayNam;\
    Siz  = ArraySz;\
\
    ArraySz = 2 * ArraySz;\
    alloc_arr(ArrType,ArrayNam,ArraySz);\
    fprintf(stderr, "**Warning:Dynamically Expanding Array(%s) To Size %ld**\n",Nam,(long)ArraySz);\
    \
    for( i = 0; i < Siz; i ++){\
       ArrayNam[i] = Temp[i];\
    }\
    fprintf(stderr,"**Done\n");\
    free(Temp);\
 }



/* term stack */
#define MAXSTACK 10000
extern int term_stackptr;
extern Cell *term_stack;
extern long term_stacksize;
#define pop_term  term_stack[term_stackptr--]
#define push_term(T){\
    if(term_stackptr +1 == term_stacksize){\
       xpand_array(Cell,term_stack,term_stacksize,"term_stack");\
    }\
  term_stack[++term_stackptr] = ((Cell) T);\
}

#define pop_term_trust term_stackptr--
#define not_empty_term  term_stackptr != -1


/* registers for trie backtracking */
#define MaxTrieRegs 500
extern CPtr reg_arrayptr,
       var_regs[];
extern Cell CallNumVar;

#define will_overflow_reg_array(x) {\
   int idx; \
   if( x >= reg_array +reg_array_size ){\
   idx = reg_arrayptr - reg_array;\
    xpand_array(Cell,reg_array,reg_array_size,"reg_array");\
   reg_arrayptr = reg_array + idx;\
   }\
}

#define pushreg(X){\
        will_overflow_reg_array(reg_arrayptr+1);\
	(*(++reg_arrayptr)) = (Cell) X;\
}

extern CPtr *var_addr;
extern int  var_addr_arraysz;

/* Safe assignment -- can be generalized by type.
  CPtr can be abstracted out */

#define safe_assign(ArrayNam,Index,Value,ArraySz) {\
   if( Index >=  ArraySz){\
     xpand_array(CPtr,ArrayNam,ArraySz,"var_addr");\
   }\
   ArrayNam[Index] = Value;\
}
     
     

extern Cell VarEnumerator[];
extern int  num_heap_term_vars;

/* has to go into trie_code.i --- but for now */
#define is_no_cp(x) (((Cell)Instr(x) & 0x3)== 0)
#define is_trust(x) (((Cell)Instr(x) & 0x3)== 1)
#define is_try(x)   (((Cell)Instr(x) & 0x3)== 2)
#define is_retry(x) (((Cell)Instr(x) & 0x3)== 3)

#ifdef DEBUG

#define print_trie_atom(X){\
 if(cell_tag(X) == STRING)\
   printf("atom(%s)",string_val(X));\
 else if(cell_tag(X) == CS) \
   printf("atom(%s/%d)",get_name((Psc)dec_addr(X)), get_arity((Psc)dec_addr(X))) ;\
 else if(cell_tag(X) == INT)\
   printf("atom(%d)",int_val(X));\
 else if(cell_tag(X) == LIST)\
   printf("./2");\
 else\
  printf("Unk(%x)",(int)X);\
 }
 
#define print_trie_node(X) {\
 printf("%x,I(%x),A(%x),P(%x),S(%x)",(int)X,Instr(X),(int)Atom(X),(int)Parent(X),(int)Sibl(X));\
 if(cell_tag(Atom(X)) == STRING)\
   printf("atom(%s)\n",string_val(Atom(X)));\
 else if(cell_tag(Atom(X)) == CS) \
   printf("atom(%s/%d)\n",get_name((Psc)dec_addr(Atom(X))), get_arity((Psc)dec_addr(Atom(X)))) ;\
 else if(cell_tag(Atom(X)) == INT)\
   printf("atom(%d)\n",int_val(Atom(X)));\
 if(cell_tag(Atom(X)) == LIST)\
   printf("./2\n");}
#endif

/* to avoid warnings */
extern Cell ptoc_tag(int);
extern void ctop_tag(int, Cell);

#define NUM_TRIEVARS 400



#define simple_dbind_ref_nth_var(addr,n)  simple_dbind_ref(addr,VarEnumerator[n])
#define bld_nth_var(addr,n)  bld_ref(addr,VarEnumerator[n])
#define dbind_ref_nth_var(addr,n) dbind_ref(addr,VarEnumerator[n])
#define trie_var_num(x)  ((((Cell) x) - VarEnumerator[0])/sizeof(Cell))
#define is_VarEnumerator(x) (!((((Cell) x) >=  VarEnumerator[0]) && (((Cell) x) <=  VarEnumerator[NUM_TRIEVARS -1])) )



extern CPtr * Addr_Stack;
extern int addr_stack_size; 
extern Cell * reg_array;
extern int reg_array_size;
extern int delay_it;

#define DEFAULT_ARRAYSIZ 512 

extern CPtr *copy_of_var_addr;
extern int copy_of_num_heap_term_vars;
extern void printterm(Cell, byte, int);
