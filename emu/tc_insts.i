/* File:      tc_insts.i
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



#include "debugs/debug_tries.h"



/*----------------------------------------------------------------------*/
/* The following is the list of Trie-Code instructions.			*/
/*----------------------------------------------------------------------*/

case trie_no_cp_str:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_no_cp_str\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	unify_with_trie_str;
	non_ftag_lpcreg;
	goto contcase;

case trie_try_str:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_try_str\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	save_find_locx(ereg);
	set_min(tbreg,breg,bfreg);
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	unify_with_trie_str;
	non_ftag_lpcreg;
	goto contcase;

case trie_retry_str:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_retry_str\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) =  (byte *)opfail;
	unify_with_trie_str;
	non_ftag_lpcreg;
	goto contcase;

case trie_trust_str:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_trust_str\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);	/* Remove this CP */
	restore_trail_condition_registers(breg);
	unify_with_trie_str;
	non_ftag_lpcreg;
	goto contcase;

/*----------------------------------------------------------------------*/

case trie_no_cp_numcon:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_no_cp_numcon:\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	unify_with_trie_numcon;
	reg_arrayptr--;
	non_ftag_lpcreg;
	goto contcase;

case trie_try_numcon:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_try_numcon\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	save_find_locx(ereg);
	set_min(tbreg,breg,bfreg);
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	unify_with_trie_numcon;
	reg_arrayptr--;
	non_ftag_lpcreg;
	goto contcase;

case trie_retry_numcon:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_retry_numcon\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	unify_with_trie_numcon;
	reg_arrayptr--;
	non_ftag_lpcreg;
	goto contcase;

case trie_trust_numcon:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_trust_numcon\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);
	restore_trail_condition_registers(breg);
	unify_with_trie_numcon;
	reg_arrayptr--;
	non_ftag_lpcreg;
	goto contcase;

/*----------------------------------------------------------------------*/

case trie_no_cp_numcon_succ:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_no_cp_numcon_succ\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	unify_with_trie_numcon;
	reg_arrayptr--;
	proceed_lpcreg;
	goto contcase;

case trie_try_numcon_succ:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_try_numcon_succ\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	save_find_locx(ereg);
	set_min(tbreg,breg,bfreg);
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	unify_with_trie_numcon;
	reg_arrayptr--;
	proceed_lpcreg;
	goto contcase;

case trie_retry_numcon_succ:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_retry_numcon_succ\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	unify_with_trie_numcon;
	reg_arrayptr--;
	proceed_lpcreg;
	goto contcase;

case trie_trust_numcon_succ:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_trust_numcon_succ\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);
	restore_trail_condition_registers(breg);
	unify_with_trie_numcon;
	reg_arrayptr--;
	proceed_lpcreg;
	goto contcase;

/*----------------------------------------------------------------------*/

case trie_no_cp_var:		
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_no_cp_var\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	num_vars_in_var_regs = (int)int_val(opatom) & 0xffff;
	var_regs[(int)int_val(opatom) & 0xffff] = (CPtr) *reg_arrayptr;
	reg_arrayptr--;
	next_lpcreg;
	goto contcase;

case trie_try_var:		
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_try_var\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	save_find_locx(ereg);
	set_min(tbreg, breg,bfreg);
	save_find_locx(ereg);
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	num_vars_in_var_regs = (int)int_val(opatom) & 0xffff;
	var_regs[(int)int_val(opatom) & 0xffff] = (CPtr) *reg_arrayptr;
	reg_arrayptr--;
	next_lpcreg;
	goto contcase;

case trie_retry_var:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_retry_var\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	num_vars_in_var_regs = (int)int_val(opatom) & 0xffff;
	var_regs[(int)int_val(opatom) & 0xffff] = (CPtr) *reg_arrayptr;
	reg_arrayptr--;
	next_lpcreg;
	goto contcase;

case trie_trust_var:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_trust_var\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);	/* Remove this CP */
	restore_trail_condition_registers(breg);
	num_vars_in_var_regs = (int)int_val(opatom) & 0xffff;
	var_regs[(int)int_val(opatom) & 0xffff] = (CPtr) *reg_arrayptr;
	reg_arrayptr--;
	next_lpcreg;
	goto contcase;

/*----------------------------------------------------------------------*/

case trie_no_cp_val:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_no_cp_val\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	unify_with_trie_val;
	next_lpcreg;
	goto contcase;

case trie_try_val:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_try_val\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	save_find_locx(ereg);
	set_min(tbreg,breg,bfreg);
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	unify_with_trie_val;
	next_lpcreg;
	goto contcase;

case trie_retry_val:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_retry_val\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	unify_with_trie_val;
	next_lpcreg;
	goto contcase;

case trie_trust_val:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_trust_val\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);	/* Remove this CP */
	restore_trail_condition_registers(breg);
	unify_with_trie_val;
	next_lpcreg;
	goto contcase;

/*----------------------------------------------------------------------*/

case trie_no_cp_list:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_no_cp_list\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	unify_with_trie_list;
	non_ftag_lpcreg;
	goto contcase;

case trie_try_list:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_try_list\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	save_find_locx(ereg);
	set_min(tbreg, breg, bfreg);
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	unify_with_trie_list;
	non_ftag_lpcreg;
	goto contcase;

case trie_retry_list:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_retry_list:\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	unify_with_trie_list;
	non_ftag_lpcreg;
	goto contcase;

case trie_trust_list:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_trust_list\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);	/* Remove this CP */
	restore_trail_condition_registers(breg);
	unify_with_trie_list;
	non_ftag_lpcreg;
	goto contcase;

/* jf: fail insts for deleted nodes -  reclaim deleted returns at completion */
case trie_no_cp_fail:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_no_cp_fail\n");
#endif	
	lpcreg = (byte *) & fail_inst;
	goto contcase;

case trie_trust_fail:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_trust_fail\n");
#endif	
NodePtr = (NODEptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);	/* Remove this CP */
	restore_trail_condition_registers(breg);
	lpcreg = (byte *) & fail_inst;
	goto contcase;

case	trie_try_fail:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_try_fail\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	save_find_locx(ereg);
	set_min(tbreg, breg,bfreg);
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	lpcreg = (byte *) & fail_inst;
	goto contcase;

case 	trie_retry_fail:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_retry_fail\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	lpcreg = (byte *) & fail_inst;
	goto contcase;

/*----------------------------------------------------------------------*/

case hash_opcode:
#ifdef PVR_DEBUG_TC_INSTS
	printf("hash_opcode\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	save_find_locx(ereg);
	set_min(tbreg, breg,bfreg);
	save_trie_registers(tbreg);
	temp_ptr_for_hash = (CPtr)*reg_arrayptr;
	cptr_deref(temp_ptr_for_hash);
	if(isref(temp_ptr_for_hash))
		*(--tbreg) = HASH_IS_FREE;
	else
		*(--tbreg) = HASH_IS_NOT_FREE;
	*(--tbreg) = (Cell) Child(NodePtr);
	*(--tbreg) = FIRST_HASH_NODE;
	save_choicepoint(tbreg,ereg,(byte *)&hash_handle_inst,breg);
	breg = tbreg;
	hbreg = hreg;
	lpcreg = (byte *) &hash_handle_inst;
	goto contcase;


case hash_handle:
#ifdef PVR_DEBUG_TC_INSTS
	printf("hash_handle\n");
#endif
	hash_base = (NODEptr *) *(breg+CP_SIZE+1);
	hash_offset = *(breg+CP_SIZE);
	if(*(breg + CP_SIZE + 2) == HASH_IS_NOT_FREE){
	if((hash_offset != FIRST_HASH_NODE)&& (hash_offset != NO_MORE_IN_HASH)){/* old hash_offset */
	    		tbreg = breg;
	    		restore_regs_and_vars(tbreg, CP_SIZE+3);
	  	}
		
	deref(*reg_arrayptr);

	if(!isref(*reg_arrayptr)){
		trie_based_cell_hash(*reg_arrayptr,hashed_hash_offset);
		if(hash_offset == FIRST_HASH_NODE){
			if(*hash_base == NULL){ /* No Variables in hash */
				breg = cp_prevbreg(breg);	
				if(*(hash_base + hashed_hash_offset) == NULL)/* hash bucket 0*/
					lpcreg = (byte *) cp_pcreg(breg);
				else
					lpcreg = (byte *)*(hash_base + hashed_hash_offset);

			}
			else{
				if(((*(hash_base + hashed_hash_offset) == NULL) )||( hashed_hash_offset == 0))
					breg = cp_prevbreg(breg);
				else
					*(breg + CP_SIZE) = hashed_hash_offset;
				lpcreg = (byte *) *hash_base;
			}
			
		}
		else if(hash_offset == hashed_hash_offset){
			lpcreg = (byte *)*(hash_base + hash_offset);
			breg = cp_prevbreg(breg);
			}
		else{
			fprintf(stderr," Hash Offset %d, HHO %d\n",hash_offset,hashed_hash_offset);
			xsb_exit("error_condition in hash_handl\n");
		}
	}
	else{
	xsb_exit("error_condition in hash_handl\n");
	}

}
else{
	hash_base = (NODEptr *) *(breg+CP_SIZE+1);
	hash_offset = *(breg+CP_SIZE);
	get_next_nonempty_hash_offset(hash_base,hash_offset);
	if (hash_offset == NO_MORE_IN_HASH) {
	  breg = cp_prevbreg(breg);
	  lpcreg = (byte *) cp_pcreg(breg);
	}
	else {
	  if (*(breg+CP_SIZE) != FIRST_HASH_NODE) {	/* old hash_offset */
	    tbreg = breg;
	    restore_regs_and_vars(tbreg, CP_SIZE+3);
	  }
	  lpcreg = (byte *) *(hash_base + hash_offset);
	  *(breg+CP_SIZE) = hash_offset;
	}
}
	goto contcase;




case trie_proceed:	/* This is essentially a "proceed" */
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_proceed:\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
  	num_vars_in_var_regs = -1;
	proceed_lpcreg;
	goto contcase;


case trie_assert_inst:{
	Psc psc_ptr;
	int arity,i;

	NodePtr = (NODEptr) (lpcreg - 1);
	if(Child(NodePtr) != NULL){
	psc_ptr = (Psc)Sibl(NodePtr);
	reg_arrayptr = reg_array -1;
  	num_vars_in_var_regs = -1;
  	save_find_locx(ereg);
  	arity = get_arity(psc_ptr);
  	for(i = arity; i >= 1;i--){
    		pushreg(*(rreg+i));
  	}		


		lpcreg = (byte *) Child(NodePtr);
	}
	else
		lpcreg = (byte *) &fail_inst;
	}
	goto contcase;
