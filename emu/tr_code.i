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
#define opsucc ((CPtr) (Child(NodePtr)))
#define opfail ((CPtr) (Sibl(NodePtr)))

#define FIRST_HASH_NODE -1
#define NO_MORE_IN_HASH -2
#define HASH_IS_FREE	-3
#define HASH_IS_NOT_FREE -4
/*----------------------------------------------------------------------*/


#define hash_size(x)	(((struct HASHhdr *) x) - 1)->HASHmask

#define trie_based_cell_hash(this_cell,y)\
	switch(cell_tag(this_cell)){\
		case STRING:case INT:case FLOAT:\
			y = HASH(this_cell,hash_size(hash_base));\
			break;\
		case LIST:\
			y = HASH((Cell)LIST,hash_size(hash_base));\
			break;\
		case CS: y = HASH(makecs(follow(cs_val(this_cell))),hash_size(hash_base));\
			break;\
		default:\
			fprintf(stderr,"Bad tag :Type %d ",\
					(int)cell_tag((Cell)this_cell));\
			xsb_exit("In instruction hash_handle");\
		}



#define get_next_nonempty_hash_offset(hash_base, hash_offset)	\
	while (TRUE) {\
	  hash_offset++;\
	  if (hash_offset > (hash_size(hash_base))) {\
	    hash_offset = NO_MORE_IN_HASH;\
	    break;\
	  }\
	  else {\
	    if ((*(hash_base + hash_offset)) != 0) break;\
	  }\
	}

/*----------------------------------------------------------------------*/

#define proceed_lpcreg	\
    if(is_answer_leaf(NodePtr)){handle_conditional_answers;}	\
    Last_Nod_Sav = NodePtr;	\
    lpcreg = cpreg

#define non_ftag_lpcreg	lpcreg = (byte *) opsucc

#define next_lpcreg	\
    if (!ftagged(Parent(NodePtr))) {	/* If it is a success node */\
      non_ftag_lpcreg;\
    } else {	/* do a "proceed" */\
      proceed_lpcreg; \
    }

/*----------------------------------------------------------------------*/

#define reg_array_overflowed ( reg_arrayptr >= reg_array + MaxTrieRegs)


#define MaxTrieRegs 500
Cell *reg_array;
int  reg_array_size = DEFAULT_ARRAYSIZ;
CPtr cptr, reg_arrayptr;

int  num_vars_in_var_regs = -1;
CPtr var_regs[MaxTrieRegs];

/*----------------------------------------------------------------------*/

CPtr tbreg;
NODEptr TrieRootPtr, NodePtr,Last_Nod_Sav;
NODEptr *hash_base; 
CPtr temp_ptr_for_hash;
int hash_offset,hashed_hash_offset;
Psc psc_ptr;

#ifdef SLG_WITH_DELAY
#define restore_regs_and_vars(tbreg,offset)		\
		switch_envs(tbreg);			\
		delayreg = cp_pdreg(tbreg);		\
		restore_some_wamregs(tbreg, ereg);	\
		restore_trie_registers(tbreg + offset) 
#else
#define restore_regs_and_vars(tbreg,offset)		\
		switch_envs(tbreg);			\
		restore_some_wamregs(tbreg, ereg);	\
		restore_trie_registers(tbreg + offset) 
#endif

#define save_trie_registers(tbreg)			\
{ \
	CPtr temp_arrayptr;\
	int reg_count = 0, i;\
	i = num_vars_in_var_regs;\
	while (i >= 0) {\
/*	      fprintf(stderr, "+");*/\
	      *(--tbreg) = (Cell)var_regs[i];\
	      i--;\
	}\
	*(--tbreg) = num_vars_in_var_regs;\
	temp_arrayptr = reg_arrayptr;\
	while (temp_arrayptr >= reg_array) {\
/*	  fprintf(stderr, "*");*/\
	  /* INV:temp_array_ptr + reg_count == reg_arrayptr */\
	  *(--tbreg) = *temp_arrayptr;\
/*	  fprintf(stderr, "ST tbreg = %x, arrayptr = %x\n", tbreg, temp_arrayptr);*/\
	  reg_count++;\
	  temp_arrayptr--;\
	}\
	(*--tbreg) = reg_count;\
}

#define restore_trie_registers(temp)	       \
{\
	int i;\
	CPtr treg = temp;\
	reg_arrayptr = reg_array - 1;\
	i = *treg;\
	while (i > 0)\
	    {\
/*	      fprintf(stderr, "#");*/\
		reg_arrayptr++;\
/*	      fprintf(stderr, "RT tbreg = %x, arrayptr = %x\n", tbreg, reg_arrayptr);*/\
		*reg_arrayptr = *(++treg);\
		i--;\
	    }\
	num_vars_in_var_regs = *(++treg);\
	for(i = 0; i<= num_vars_in_var_regs;i++)\
	   {\
/*	      fprintf(stderr, "$");*/\
		var_regs[i] = (CPtr)*(++treg);\
	   }\
}



#define unify_with_trie_numcon {\
	deref(*reg_arrayptr);\
	if(isref(*reg_arrayptr)){\
		bind_ref((CPtr) *reg_arrayptr, opatom);\
}\
	else{\
		if(*reg_arrayptr != opatom){\
			Fail1;\
			goto contcase;}\
		}\
}


#define unify_with_trie_str {\
   deref(*reg_arrayptr);\
   psc_ptr = (Psc) cs_val(opatom);\
   will_overflow_reg_array(reg_arrayptr + ((int) get_arity((struct psc_rec *) psc_ptr)));\
   if(isref(*reg_arrayptr)){\
	bind_ref((CPtr) *reg_arrayptr, makecs(hreg));\
	reg_arrayptr--;\
	*(hreg++) = (Cell) psc_ptr;\
	for (i = (int) get_arity((struct psc_rec *) psc_ptr); i >= 1; i--) {\
	  *(reg_arrayptr + i) = (Cell) hreg;\
	  new_heap_free(hreg);\
	}\
	reg_arrayptr += (int) get_arity((struct psc_rec *) psc_ptr);\
    }\
    else{\
	CPtr temp,temp1;\
	Psc psc;\
	temp = (CPtr)*reg_arrayptr;\
	if(cell_tag(temp)!= CS){\
		Fail1;\
		goto contcase;\
	}\
	if((psc = get_str_psc(temp))!= psc_ptr){\
		Fail1;\
		goto contcase;\
	}\
	reg_arrayptr--;\
	temp1 = (CPtr)cs_val(temp);\
	for (i = (int) get_arity((struct psc_rec *) psc_ptr); i >= 1; i--) {\
		*(reg_arrayptr +i) = *(temp1+ (int) get_arity((struct psc_rec *) psc_ptr)- i +1);\
		}\
	reg_arrayptr += (int) get_arity((struct psc_rec *) psc_ptr);\
   }\
}





#define unify_with_trie_list {\
   deref(*reg_arrayptr);\
   if(isref(*reg_arrayptr)){\
	bind_ref((CPtr) *reg_arrayptr, (Cell) makelist(hreg));\
	*reg_arrayptr = (Cell)(hreg +1);		/* head of list */\
        will_overflow_reg_array(reg_arrayptr + 1);\
	*(++reg_arrayptr) = (Cell) hreg;	/* tail of list */\
	new_heap_free(hreg);\
	new_heap_free(hreg);\
   }\
   else{\
	CPtr temp;\
	temp = (CPtr)*reg_arrayptr;\
	if(cell_tag(temp)!= LIST){\
		Fail1;\
		goto contcase;\
	}\
	else{\
                will_overflow_reg_array(reg_arrayptr + 1);\
		*reg_arrayptr++ = (Cell)(clref_val(temp) +1);\
		*reg_arrayptr = (Cell)(clref_val(temp));\
	}\
   }\
}

#define unify_with_trie_val {\
   Cell cell2deref;\
   deref(*reg_arrayptr);\
   if(isref(*reg_arrayptr)){\
	cell2deref = (Cell)var_regs[(int)int_val(opatom)];\
	deref(cell2deref);\
	if(cell2deref != *reg_arrayptr)\
	   bind_ref((CPtr) *reg_arrayptr,cell2deref);\
	}\
   else{\
	op1 = (Cell)*reg_arrayptr;\
	op2 = (Cell) var_regs[(int)int_val(opatom)];\
	if(unify(op1,op2) == FALSE){\
		Fail1;\
		goto contcase;\
	}\
   }\
   reg_arrayptr--;\
}




