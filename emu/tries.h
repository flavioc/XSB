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
 *	A B S T R A C T E D   T R I E   R E P R E S E N T A T I O N
 *	===========================================================
 *
 *  There are several types of operations which are common to any trie,
 *  either to the structure as a whole, or to its components.  Likewise,
 *  there are places in the engine where it won't be apparent what type
 *  of trie (respectively, component) one is inspecting.  Although this
 *  can be ascertained, adherence to the following guidelines will make
 *  this unnecessary, as well as the need for any type-dependent special
 *  processing.  A set of macros are supplied for use in these "don't
 *  know" or "don't care" situations.
 *
 *  All types of trie nodes must be laid out in the same fashion, with
 *  common fields appearing in the same order.  Fields which point to
 *  like-component records may have different types, but the others
 *  (Info and Symbol) are fixed.  Currently, the layout is as indicated
 *  below.  Except for the Info field which MUST be first, the ordering
 *  of the other fields is arbitrary.  Field names have also been
 *  standardized to help ensure conformity.
 *
 *  We use the following notation:
 *    TN_*         Trie Node operation
 *    TrieHT_*     Trie Hash Table operation  (see trie_internals.h)
 *    TSC_*        Trie SubComponent operation (applicable to a TN or THT)
 *    pTN          pointer to a Trie Node
 *    pTHT         pointer to a Trie Hash Table
 *    pTSC pointer to a Trie SubComponent
 */

#define TN_Instr(pTN)		TSC_Instr(pTN)
#define TN_Status(pTN)		TSC_Status(pTN)
#define TN_TrieType(pTN)	TSC_TrieType(pTN)
#define TN_NodeType(pTN)	TSC_NodeType(pTN)
#define TN_Parent(pTN)		( (pTN)->parent )
#define TN_Child(pTN)		( (pTN)->child )
#define TN_Sibling(pTN)		( (pTN)->sibling )
#define TN_Symbol(pTN)		( (pTN)->symbol )

#define TSC_Instr(pTSC)		IPT_Instr((pTSC)->info)
#define TSC_Status(pTSC)	IPT_Status((pTSC)->info)
#define TSC_TrieType(pTSC)	IPT_TrieType((pTSC)->info)
#define TSC_NodeType(pTSC)	IPT_NodeType((pTSC)->info)


#define TN_SetHashHdr(pTN,pTHT)		TN_Child(pTN) = (void *)(pTHT)
#define TN_GetHashHdr(pTN)		TN_Child(pTN)

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
 *  For valid trie-embedded instruction values, see file "inst_xsb.h".  */

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

typedef struct Basic_Trie_Node *BTNptr;
typedef struct Basic_Trie_Node {
  InstrPlusType info;
  BTNptr sibling;
  BTNptr child;
  BTNptr parent;
  Cell symbol;
} BasicTrieNode;

/* - - Preferred macros - - - - */
#define BTN_Instr(pBTN)		TN_Instr(pBTN)
#define BTN_Status(pBTN)	TN_Status(pBTN)
#define BTN_TrieType(pBTN)	TN_TrieType(pBTN)
#define BTN_NodeType(pBTN)	TN_NodeType(pBTN)
#define BTN_Parent(pBTN)	TN_Parent(pBTN)
#define BTN_Child(pBTN)		TN_Child(pBTN)
#define BTN_Sibling(pBTN)	TN_Sibling(pBTN)
#define BTN_Symbol(pBTN)	TN_Symbol(pBTN)

/* - - For backwards compatibility - - - - */
typedef struct Basic_Trie_Node *NODEptr;
#define Instr(X)	BTN_Instr(X)
#define TrieType(X)     BTN_TrieType(X)
#define NodeType(X)     BTN_NodeType(X)
#define Parent(X)	BTN_Parent(X)
#define Child(X)	BTN_Child(X)
#define Sibl(X)		BTN_Sibling(X)
#define Atom(X)		BTN_Symbol(X)

/*===========================================================================*/

/*
 *                  T I M E - S T A M P E D   T R I E S
 *                  ===================================
 *
 *  Similar in construction and maintenance to normal tries, these extend
 *  the basic design in order to support answer subsumption.  Conditions
 *  under which hash tables are created and the way symbols are stored in
 *  the nodes are identical.  (See the file tries_priv.h for details.)
 *
 *  A timestamp is maintained in each node of the Time-Stamped Trie (TST).
 *  When used for Answer Tables, the timestamp kept in each node is the
 *  maximum of the timestamps of its children.  Since timestamps
 *  monotonically increase as terms are entered, this property can be
 *  easily maintained by propagating the timestamp of a newly interned
 *  term from the leaf to the root.  Hence, the root ALWAYS contains the
 *  timestamp of the largest-timestamped answer contained in the Answer
 *  Table.
 *
 *  For facilitating certain subsumptive operations, it is important to
 *  quickly identify nodes having a timestamp greater than a given one.
 *  When a sibling chain becomes long, it is no longer acceptable to
 *  perform a linear scan in order to identify these timestamp-valid
 *  nodes.  Therefore, when we create a hash table for a group of nodes,
 *  we also create an auxiliary structure which maintains the nodes in
 *  decreasing timestamp order.  Each node's TimeStamp field is then used
 *  for pointing to an associated frame in this structure, where the
 *  timestamp is now kept.  The hash header is extended -- over the basic
 *  trie hash header -- to contain fields for maintaining these frames in
 *  a doubly linked list.  Once the Answer Table is completed, these
 *  structures can be disposed.  To facilitate this, hash tables, within
 *  a particular TST, are chained together from the root, accessible from
 *  its Sibling field.  Lazy evaluation...
 */

typedef unsigned long  TimeStamp;

typedef struct Time_Stamped_Trie_Node *TSTNptr;
typedef struct Time_Stamped_Trie_Node {
  InstrPlusType info;
  TSTNptr sibling;
  TSTNptr child;
  TSTNptr parent;
  Cell symbol;
  TimeStamp ts;
} TS_TrieNode;

/* Field Access Macros
   ------------------- */
#define TSTN_Instr(pTSTN)	TN_Instr(pTSTN)
#define TSTN_Status(pTSTN)	TN_Status(pTSTN)
#define TSTN_TrieType(pTSTN)	TN_TrieType(pTSTN)
#define TSTN_NodeType(pTSTN)	TN_NodeType(pTSTN)
#define TSTN_Parent(pTSTN)	TN_Parent(pTSTN)
#define TSTN_Child(pTSTN)	TN_Child(pTSTN)
#define TSTN_Sibling(pTSTN)	TN_Sibling(pTSTN)
#define TSTN_Symbol(pTSTN)	TN_Symbol(pTSTN)
#define TSTN_TimeStamp(pTSTN)	( (pTSTN)->ts )

/*===========================================================================*/

/*
 *                             Answer List Node
 *                             ================
 *
 *  A global resource for ALNs is currently implemented.  Blocks of memory
 *  for ALN storage are allocated whenever this resource is depleted.  All
 *  ALNs are allocated from this resource.  To allow rapid deallocation of
 *  these block-malloc'ed structures, the first word in the structure must
 *  contain the field used to link them into a chain when in use.
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

/*===========================================================================*/

/*
 *                      Tabled-Call Lookup Structures
 *                      =============================
 *
 *  Data structures for parameter passing to and from the call
 *  check/insert routines.
 */

typedef struct Tabled_Call_Info_Record {
  struct Table_Info_Frame *table_info_record;
  int call_arity;
  CPtr arg_vector;
  CPtr var_vector_loc;     /* location to store the call var vector */
} TabledCallInfo;

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

/*===========================================================================*/


/*-- exported trie functions ------------------------------------------*/

extern BTNptr   newBasicTrie(Psc,int);
extern byte *	trie_get_calls(void);
extern Cell	get_lastnode_cs_retskel(Cell);
extern byte *	trie_get_returns_for_call(void);
extern void	remove_open_tries(CPtr);
extern void     init_trie_aux_areas(void);
extern void     load_solution_trie(int, int, CPtr, BTNptr);
extern void     variant_call_search(TabledCallInfo *, CallLookupResults *);
extern BTNptr   one_term_chk_ins(CPtr, BTNptr, int *);
extern BTNptr   whole_term_chk_ins(Cell, BTNptr *, int *);
extern BTNptr	get_next_trie_solution(ALNptr *);
extern BTNptr	variant_answer_search(int, int, CPtr, struct subgoal_frame *,
				      bool *);
extern BTNptr   delay_chk_insert(int, CPtr, CPtr *);
extern void     undo_answer_bindings(void);
extern void	load_delay_trie(int, CPtr, BTNptr);
extern bool     bottom_up_unify(void);

void	subsumptive_call_search(TabledCallInfo *, CallLookupResults *);
TSTNptr	subsumptive_answer_search(int,CPtr,struct subgoal_frame *,bool *);
void	consume_subsumptive_answer(BTNptr,int,CPtr);
ALNptr	retrieve_unifying_answers(TSTNptr,TimeStamp,int,CPtr);
void	delete_subsumptive_table(BTNptr);

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

extern int  num_heap_term_vars;
extern CPtr *var_addr;
extern int  var_addr_arraysz;

/*----------------------------------------------------------------------*/

extern Cell * reg_array;
extern int reg_array_size;
extern int delay_it;

#define NUM_TRIEVARS 400
#define DEFAULT_ARRAYSIZ 512 

extern CPtr *copy_of_var_addr;
extern int  copy_of_num_heap_term_vars;

/*=========================================================================*/


#endif
