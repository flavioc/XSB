/* File:      tr_code.i
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


#define opatom Atom(NodePtr)
#define opsucc ((byte *)(Child(NodePtr)))
#define opfail ((byte *)(Sibl(NodePtr)))

#define FIRST_HASH_NODE  -1
#define NO_MORE_IN_HASH  -2
#define HASH_IS_FREE	 -3
#define HASH_IS_NOT_FREE -4

/*----------------------------------------------------------------------*/

#define hash_size(x)	(((struct HASHhdr *) x) - 1)->HASHmask

#define trie_based_cell_hash(this_cell,y)\
    switch(cell_tag(this_cell)){\
      case STRING: case INT: case FLOAT:\
	  y = HASH(this_cell,hash_size(hash_base));\
	  break;\
      case LIST:\
	  y = HASH((Cell)LIST,hash_size(hash_base));\
	  break;\
      case CS:\
          y = HASH(makecs(follow(cs_val(this_cell))),hash_size(hash_base));\
	  break;\
      default:\
	  fprintf(stderr,"Bad tag :Type %d ", (int)cell_tag((Cell)this_cell));\
	  xsb_exit("In instruction hash_handle");\
      }

#define get_next_nonempty_hash_offset(hash_base, hash_offset)	\
    while (TRUE) {\
	hash_offset++;\
	if (hash_offset > (hash_size(hash_base))) {\
	    hash_offset = NO_MORE_IN_HASH;\
	    break;\
	} else {\
	    if ((*(hash_base + hash_offset)) != 0) break;\
	}\
    }

/*----------------------------------------------------------------------*/

#define proceed_lpcreg	\
    if (is_answer_leaf(NodePtr)) {\
      handle_conditional_answers; \
      num_vars_in_var_regs = -1;  \
    } \
    Last_Nod_Sav = NodePtr;	  \
    lpcreg = cpreg

#define non_ftag_lpcreg	lpcreg = opsucc

#define next_lpcreg	\
    if (!ftagged(Parent(NodePtr))) {	/* If it is a success node */\
      non_ftag_lpcreg;\
    } else {	/* do a "proceed" */\
      proceed_lpcreg; \
    }

/*----------------------------------------------------------------------*/

#define reg_array_overflowed (reg_arrayptr >= reg_array + MaxTrieRegs)

Cell *reg_array;
#ifdef LOCAL_EVAL
CPtr cptr;
#endif
CPtr reg_arrayptr;
int  reg_array_size = DEFAULT_ARRAYSIZ;

#define MaxTrieRegs 500
CPtr var_regs[MaxTrieRegs];
int  num_vars_in_var_regs = -1;

/*----------------------------------------------------------------------*/

CPtr    tbreg;
#ifdef LOCAL_EVAL
NODEptr TrieRootPtr;
#endif
NODEptr NodePtr,Last_Nod_Sav;
NODEptr *hash_base; 
CPtr    temp_ptr_for_hash;
int     hash_offset, hashed_hash_offset;

#define restore_regs_and_vars(tbreg,offset)	\
    undo_bindings(tbreg);			\
    delayreg = cp_pdreg(tbreg);                 \
    restore_some_wamregs(tbreg, ereg);	        \
    restore_trie_registers(tbreg + offset) 

/*----------------------------------------------------------------------*/
/* Garbage collection strongly prefers tagged integers in CP stack...   */
/*       PLEASE PRESERVE THIS IVNARIANT --- Kostis & Bart !             */
/*----------------------------------------------------------------------*/

#define save_trie_registers(tbreg) {\
    CPtr temp_arrayptr;\
    int reg_count = 0, i;\
    i = num_vars_in_var_regs;\
    while (i >= 0) {\
        *(--tbreg) = (Cell)var_regs[i];\
        i--;\
    }\
    *(--tbreg) = makeint(num_vars_in_var_regs);\
    temp_arrayptr = reg_arrayptr;\
    while (temp_arrayptr >= reg_array) {\
      /* INV: temp_array_ptr + reg_count == reg_arrayptr */\
        *(--tbreg) = *temp_arrayptr;\
        reg_count++;\
        temp_arrayptr--;\
    }\
    (*--tbreg) = makeint(reg_count);\
}

#define restore_trie_registers(temp) {\
    int i;\
    CPtr treg = temp;\
    reg_arrayptr = reg_array - 1;\
    i = cell(treg);\
    i = int_val(i);\
    while (i > 0) {\
        reg_arrayptr++;\
        *reg_arrayptr = *(++treg);\
        i--;\
    }\
    i = *(++treg);\
    num_vars_in_var_regs = int_val(i);\
    for (i = 0; i <= num_vars_in_var_regs; i++) {\
        var_regs[i] = (CPtr)*(++treg);\
    }\
}

/*----------------------------------------------------------------------*/

#define unify_with_trie_numcon {\
    deref(*reg_arrayptr);\
    if (isref(*reg_arrayptr)) {\
        bind_ref((CPtr)*reg_arrayptr, opatom);\
    } else {\
        if (*reg_arrayptr != opatom) {\
	    Fail1;\
	    goto contcase;\
        }\
    }\
}

#define unify_with_trie_str {\
   deref(*reg_arrayptr);\
   psc = (Psc) cs_val(opatom);\
   arity = (int) get_arity(psc);\
   will_overflow_reg_array(reg_arrayptr + arity);\
   if (isref(*reg_arrayptr)) {\
       bind_ref((CPtr) *reg_arrayptr, makecs(hreg));\
       reg_arrayptr--;\
       *(hreg++) = (Cell) psc;\
       for (i = arity; i >= 1; i--) {\
	 *(reg_arrayptr + i) = (Cell) hreg;\
	 new_heap_free(hreg);\
       }\
       reg_arrayptr += arity;\
    } else {\
	CPtr temp;\
	temp = (CPtr)*reg_arrayptr;\
        if ((isconstr(temp)) && (psc == get_str_psc(temp))) {\
          reg_arrayptr--;\
          temp = (CPtr)cs_val(temp);\
          for (i = arity; i >= 1; i--) {\
            *(reg_arrayptr+i) = *(temp+arity-i+1);\
          }\
          reg_arrayptr += arity;\
        }\
        else {\
          Fail1;\
          goto contcase;\
        }\
    }\
}

#define unify_with_trie_list {\
   deref(*reg_arrayptr);\
   if (isref(*reg_arrayptr)) {\
        bind_ref((CPtr) *reg_arrayptr, (Cell) makelist(hreg));\
        *reg_arrayptr = (Cell)(hreg+1);         /* head of list */\
        will_overflow_reg_array(reg_arrayptr + 1);\
        *(++reg_arrayptr) = (Cell) hreg;        /* tail of list */\
        new_heap_free(hreg);\
        new_heap_free(hreg);\
   } else {\
        CPtr temp;\
        temp = (CPtr)*reg_arrayptr;\
        if (islist(temp)) {\
          will_overflow_reg_array(reg_arrayptr + 1);\
          *reg_arrayptr++ = (Cell)(clref_val(temp)+1);\
          *reg_arrayptr = (Cell)(clref_val(temp));\
        } else {\
          Fail1;\
          goto contcase;\
        }\
   }\
}

#define unify_with_trie_val {\
    Cell cell2deref;\
    deref(*reg_arrayptr);\
    if (isref(*reg_arrayptr)) {\
        cell2deref = (Cell)var_regs[(int)int_val(opatom)];\
        deref(cell2deref);\
        if (cell2deref != *reg_arrayptr)\
          bind_ref((CPtr) *reg_arrayptr, cell2deref);\
   } else {\
        op1 = (Cell)*reg_arrayptr;\
        op2 = (Cell) var_regs[(int)int_val(opatom)];\
        if (unify(op1,op2) == FALSE){\
            Fail1;\
            goto contcase;\
        }\
   }\
   reg_arrayptr--;\
}

/*----------------------------------------------------------------------*/
