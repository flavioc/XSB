/* File:      tries.h
** Author(s): Ernie Johnson, Prasad Rao, Kostis Sagonas
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


#ifndef PUBLIC_TRIE_DEFS


#define PUBLIC_TRIE_DEFS



/*===========================================================================*/

/*
 *                      Trie Component Info Field
 *                      =========================
 *
 *  Defines a field one word in size which contains miscellaneous info
 *  needed for maximizing trie use: An instruction field allows tries to
 *  be traversed for term unification with speeds comparable to code
 *  execution for facts.  A status field signifies whether a particular
 *  node represents a valid subbranch of the trie.  Nodes (paths) may
 *  temporarily be marked as deleted until such time they can be safely
 *  removed from the trie.  Two type fields contain annotations which
 *  describe the type of trie in which a structure resides and the role
 *  the structure plays within this trie, respectively.
 *
 *  Each of the two main components of a trie contain this field: trie
 *  nodes and hash table headers.
 *
 *  For valid trie-embedded instruction values, see file "inst.h".  */

typedef struct InstructionPlusTypeFrame {
  byte instr;		/* contains compiled trie code */
  byte status;		/* whether the node has been deleted */
  byte trie_type;	/* global info: what type of trie is this? */
  byte node_type;	/* local info: what is the role of this struct? */
#ifdef BITS64
  byte padding[4];
#endif
} InstrPlusType;

#define IPT_Instr(IPT)		((IPT).instr)
#define IPT_Status(IPT)		((IPT).status)
#define IPT_TrieType(IPT)	((IPT).trie_type)
#define IPT_NodeType(IPT)	((IPT).node_type)


/*===========================================================================*/

/*
 *                         B A S I C   T R I E S
 *                         =====================
 *
 *  Each symbol in a term is maintained in a separate node in a trie.
 *  Paths from the root to the leaves trace out the terms stored in the
 *  trie.  Sharing between terms takes place only high-up within a trie,
 *  corresponding to left-to-right term factoring.
 *
 *  Children of a particular node are maintained in a linked-list pointed
 *  to by the Child field.  This list is maintained through the Sibling
 *  fields of the child nodes.  To facilitate rapid access when the
 *  number of children becomes large, a hash table is employed.  In this
 *  case, the parent node's Child field points to a header structure for
 *  the hash table, rather than directly to one of the children.
 *
 *  When used to build Call Tries, the Child field of a leaf trie node
 *  points to a SubgoalFrame which contains additional info for that call.
 */

/*----------------------------------------------------------------------*/

/*
 *                          Basic Trie Node
 *                          ---------------
 */

typedef struct Basic_Trie_Node *BTNptr;
typedef struct Basic_Trie_Node {
  InstrPlusType info;
  BTNptr sibling;
  BTNptr child;
  BTNptr parent;
  Cell symbol;
} BasicTrieNode;

/* - - Preferred macros - - - - */
#define BTN_Instr(pBTN)		IPT_Instr((pBTN)->info)
#define BTN_Status(pBTN)	IPT_Status((pBTN)->info)
#define BTN_TrieType(pBTN)	IPT_TrieType((pBTN)->info)
#define BTN_NodeType(pBTN)	IPT_NodeType((pBTN)->info)
#define BTN_Parent(pBTN)	( (pBTN)->parent )
#define BTN_Child(pBTN)		( (pBTN)->child )
#define BTN_Sibling(pBTN)	( (pBTN)->sibling )
#define BTN_Symbol(pBTN)	( (pBTN)->symbol )

/* - - For backwards compatibility - - - - */
typedef struct Basic_Trie_Node *NODEptr;
#define Instr(X)	BTN_Instr(X)
#define TrieType(X)     BTN_TrieType(X)
#define NodeType(X)     BTN_NodeType(X)
#define Parent(X)	BTN_Parent(X)
#define Child(X)	BTN_Child(X)
#define Sibl(X)		BTN_Sibling(X)
#define Atom(X)		BTN_Symbol(X)


#define Delay(X) (ASI) ((word) (BTN_Child(X)) & ~UNCONDITIONAL_MARK)

/*----------------------------------------------------------------------*/

/*
 *                      Basic Trie Hash Tables
 *                      ----------------------
 */

typedef struct Basic_Trie_HashTable *BTHTptr;
typedef struct Basic_Trie_HashTable {
  InstrPlusType info;
  unsigned long  numContents;      /* used to be numInHash */
  unsigned long  numBuckets;       /* used to be (HASHmask + 1) */
  BTNptr *pBucketArray;
  BTHTptr prev, next;		   /* DLL needed for deletion */
} BasicTrieHT;

/* Field Access Macros
   ------------------- */
#define BTHT_Instr(pTHT)		IPT_Instr((pTHT)->info)
#define BTHT_Status(pTHT)		IPT_Status((pTHT)->info)
#define BTHT_TrieType(pTHT)		IPT_TrieType((pTHT)->info)
#define BTHT_NodeType(pTHT)		IPT_NodeType((pTHT)->info)
#define BTHT_NumBuckets(pTHT)		((pTHT)->numBuckets)
#define BTHT_NumContents(pTHT)		((pTHT)->numContents)
#define BTHT_BucketArray(pTHT)		((pTHT)->pBucketArray)
#define BTHT_PrevBTHT(pTHT)		((pTHT)->prev)
#define BTHT_NextBTHT(pTHT)		((pTHT)->next)

#define BTHT_GetHashMask(pTHT)		( BTHT_NumBuckets(pTHT) - 1 )

/*===========================================================================*/

/*
 *                             Answer List Node
 *                             ----------------
 *
 *  A global resource for ALNs is currently implemented.  Blocks of
 *  memory for ALN storage are allocated whenever this resource is
 *  depleted.  All ALNs are allocated from this resource.  An "ALN
 *  Structure Manager" maintains the list of unused ALNs and the list
 *  of blocks of memory allocated for them.  To allow rapid deallocation
 *  of these block-malloc'ed structures, the first word in the structure
 *  must contain the field used to link them into a chain when in
 *  use.
 */

typedef struct Answer_List_Node *ALNptr;
typedef struct Answer_List_Node {
  ALNptr link;
  BTNptr answer_leaf;
} AnsListNode;

/* - - Preferred macros - - - - */
#define ALN_Next(pALN)		((pALN)->link)
#define ALN_Answer(pALN)	((pALN)->answer_leaf)

/* - - For backwards compatibility - - - - */
#define aln_answer_ptr(ALN)	ALN_Answer(ALN)
#define aln_next_aln(ALN)	ALN_Next(ALN)

/*----------------------------------------------------------------------*/

/*
 *                         Call Lookup Structures
 *                         ======================
 *
 *  Data structures for parameter passing to and from the call
 *  check/insert routines.
 */

typedef struct Call_Info_For_Trie_Insertion {
  struct Table_Info_Frame *table_info_record;
  int call_arity;
  CPtr arg_vector;
  CPtr var_vector_loc;     /* location to store the call var vector */
} CallInfoRecord;

#define CallInfo_TableInfo(CallInfo)	( (CallInfo).table_info_record )
#define CallInfo_CallArity(CallInfo)	( (CallInfo).call_arity )
#define CallInfo_Arguments(CallInfo)	( (CallInfo).arg_vector )
#define CallInfo_VarVectorLoc(CallInfo)	( (CallInfo).var_vector_loc )


typedef struct Call_Check_Insert_Results {
  BTNptr call_trie_term;
  struct subgoal_frame *subsumers_sgf;
  int variant_found;
  CPtr var_vector;         /* pointer to the vector of call variables */
} CallLookupResults;

#define CallLUR_Leaf(CLUR)		( (CLUR).call_trie_term )
#define CallLUR_Subsumer(CLUR)		( (CLUR).subsumers_sgf )
#define CallLUR_VariantFound(CLUR)	( (CLUR).variant_found )
#define CallLUR_VarVector(CLUR)		( (CLUR).var_vector )


/*-- exported trie functions ------------------------------------------*/

extern BTNptr newBasicTrie(Psc,int);
extern byte * trie_get_calls(void);
extern int    free_trie_size(void);
extern int    allocated_trie_size(void);
extern int    allocated_trie_hash_size(void);
extern byte * trie_get_returns_for_call(void);
extern void   abolish_trie(void);
extern void   aux_call_info(void);
extern void   remove_open_tries(CPtr);
extern void   init_trie_aux_areas(void);
extern void   get_lastnode_cs_retskel(void);
extern void   load_solution_trie(int, CPtr, BTNptr);
extern void   variant_call_search(CallInfoRecord *, CallLookupResults *);
extern BTNptr one_term_chk_ins(CPtr,BTNptr,int *);
extern BTNptr whole_term_chk_ins(Cell, BTNptr *, int *);
extern BTNptr get_next_trie_solution(ALNptr *);
extern BTNptr variant_trie_search(int, CPtr, CPtr, int *);
extern BTNptr delay_chk_insert(int, CPtr, CPtr *);
extern void   undo_answer_bindings(void);
extern void   load_delay_trie(int, CPtr, BTNptr);
extern bool   bottom_up_unify(void);

/*---------------------------------------------------------------------*/

/* slg variables */
extern CPtr ans_var_pos_reg;
extern int  num_vars_in_var_regs;
extern int  global_num_vars;

/* used for statistics */
extern long subg_chk_ins, subg_inserts, ans_chk_ins, ans_inserts;

/* trie routine variables */
extern BTNptr Last_Nod_Sav, Paren;

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

extern CPtr *var_addr;
extern int  var_addr_arraysz;

extern Cell VarEnumerator[];
extern int  num_heap_term_vars;

/*----------------------------------------------------------------------*/

extern Cell * reg_array;
extern int reg_array_size;
extern int delay_it;

#define NUM_TRIEVARS 400
#define DEFAULT_ARRAYSIZ 512 

extern CPtr *copy_of_var_addr;
extern int  copy_of_num_heap_term_vars;


#endif
