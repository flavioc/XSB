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
	var_regs[num_vars_in_var_regs] = (CPtr) *reg_arrayptr;
        { int i = num_vars_in_var_regs;
	  if ((isref(var_regs[i])) &&
            ((var_regs[i] < (CPtr)glstack.low) || (var_regs[i] >= hreg)) &&
	    ((var_regs[i] < top_of_localstk) || (var_regs[i] >= (CPtr) glstack.high)))
	  { fprintf(stderr,
		    "tc_insts.i (no_cp): var reg assigned bad 0x%p %d 0x%p\n",
		    hreg, i, var_regs[i]); }
	} 
	reg_arrayptr--;
	next_lpcreg;
	goto contcase;

case trie_try_var:		
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_try_var\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	save_find_locx(ereg);
	set_min(tbreg,breg,bfreg);
	save_find_locx(ereg);
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	num_vars_in_var_regs = (int)int_val(opatom) & 0xffff;
	var_regs[num_vars_in_var_regs] = (CPtr) *reg_arrayptr;
        { int i = num_vars_in_var_regs;
	  if ((isref(var_regs[i])) &&
            ((var_regs[i] < (CPtr)glstack.low) || (var_regs[i] >= hreg)) &&
	    ((var_regs[i] < top_of_localstk) || (var_regs[i] >= (CPtr) glstack.high)))
	  { fprintf(stderr,
		    "tc_insts.i (try): var reg assigned bad 0x%p %d 0x%p\n",
		    hreg, i, var_regs[i]); }
	} 
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
	var_regs[num_vars_in_var_regs] = (CPtr) *reg_arrayptr;
        { int i = num_vars_in_var_regs;
	  if ((isref(var_regs[i])) &&
            ((var_regs[i] < (CPtr)glstack.low) || (var_regs[i] >= hreg)) &&
	    ((var_regs[i] < top_of_localstk) || (var_regs[i] >= (CPtr) glstack.high)))
	  { fprintf(stderr,
		    "tc_insts.i (retry): var reg assigned bad 0x%p %d 0x%p\n",
		    hreg, i, var_regs[i]); }
	} 
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
	var_regs[num_vars_in_var_regs] = (CPtr) *reg_arrayptr;
        { int i = num_vars_in_var_regs;
	  if ((isref(var_regs[i])) &&
            ((var_regs[i] < (CPtr)glstack.low) || (var_regs[i] >= hreg)) &&
	    ((var_regs[i] < top_of_localstk) || (var_regs[i] >= (CPtr) glstack.high)))
	  { fprintf(stderr,
		    "tc_insts.i (trust): var reg assigned bad 0x%p %d 0x%p\n",
		    hreg, i, var_regs[i]); }
	} 
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
	set_min(tbreg,breg,bfreg);
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

case trie_try_fail:
#ifdef PVR_DEBUG_TC_INSTS
	printf("trie_try_fail\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	save_find_locx(ereg);
	set_min(tbreg,breg,bfreg);
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	lpcreg = (byte *) & fail_inst;
	goto contcase;

case trie_retry_fail:
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
/* The following code, that handles hashing in coded tries, has been	*/
/* modified for garbage collection.  Choice points for hashes in tries,	*/
/* besides the usual trie argument registers (see file tr_code.h), also	*/
/* contain 3 fields with certain information about the hash bucket.	*/
/* The first and third of these fields are predefined integers and are	*/
/* now encoded as such.  The second field contains a malloc-ed address	*/
/* which is encoded as a STRING (that's how addresses are encoded in	*/
/* XSB -- see file cell.h) to prevent garbage collection from treating	*/
/* it as a reference either to a WAM stack or to a CHAT area.		*/
/*----------------------------------------------------------------------*/

case hash_opcode:
#ifdef PVR_DEBUG_TC_INSTS
	printf("hash_opcode\n");
#endif
	NodePtr = (NODEptr) (lpcreg - 1);
	save_find_locx(ereg);
	set_min(tbreg,breg,bfreg);
	save_trie_registers(tbreg);
	temp_ptr_for_hash = (CPtr)*reg_arrayptr;
	cptr_deref(temp_ptr_for_hash);
	if (isref(temp_ptr_for_hash)) {
	  cell(--tbreg) = makeint(HASH_IS_FREE);
	} else {
	  cell(--tbreg) = makeint(HASH_IS_NOT_FREE);
	}
	temp_ptr_for_hash = (CPtr) Child(NodePtr);
	cell(--tbreg) = makestring(temp_ptr_for_hash);
	cell(--tbreg) = makeint(FIRST_HASH_NODE);
	save_choicepoint(tbreg,ereg,(byte *)&hash_handle_inst,breg);
	breg = tbreg;
	hbreg = hreg;
	lpcreg = (byte *) &hash_handle_inst;
	goto contcase;

case hash_handle:
#ifdef PVR_DEBUG_TC_INSTS
	printf("hash_handle\n");
#endif
	i = (int)cell(breg+CP_SIZE);
	hash_offset = int_val(i);
	hash_base = (NODEptr *) cell(breg+CP_SIZE+1);
	hash_base = (NODEptr *) string_val(hash_base);
	i = (int)cell(breg + CP_SIZE + 2);
	i = int_val(i);
	if (i == HASH_IS_NOT_FREE)
	{
	  if ((hash_offset != FIRST_HASH_NODE) &&
	      (hash_offset != NO_MORE_IN_HASH)) {/* old hash_offset */
	    tbreg = breg;
	    restore_regs_and_vars(tbreg, CP_SIZE+3);
	  }
	  deref(*reg_arrayptr);

	  if (!isref(*reg_arrayptr)) {
	    trie_based_cell_hash(*reg_arrayptr,hashed_hash_offset);
	    if (hash_offset == FIRST_HASH_NODE) {
	      if (*hash_base == NULL) { /* No Variables in hash */
	        breg = cp_prevbreg(breg);	
	        if (*(hash_base+hashed_hash_offset) == NULL) {/* hash bucket 0*/
		  lpcreg = (byte *) cp_pcreg(breg);
	        } else {
		  lpcreg = (byte *) *(hash_base + hashed_hash_offset);
	        }
	      } else {
	        if (((*(hash_base + hashed_hash_offset) == NULL)) ||
		     (hashed_hash_offset == 0)) {
		  breg = cp_prevbreg(breg);
	        } else {
		  cell(breg + CP_SIZE) = makeint(hashed_hash_offset);
	        }
	        lpcreg = (byte *) *hash_base;
	      }
	    } else 
		 if (hash_offset == hashed_hash_offset) {
		   lpcreg = (byte *)*(hash_base + hash_offset);
		   breg = cp_prevbreg(breg);
		 } else {
		   fprintf(stderr,"Hash Offset %d, HHO %d\n",
				  hash_offset,hashed_hash_offset);
		   xsb_exit("error_condition in hash_handl\n");
		 }
	  } else { xsb_exit("error_condition in hash_handl\n"); }
	} else {
	  get_next_nonempty_hash_offset(hash_base,hash_offset);
	  if (hash_offset == NO_MORE_IN_HASH) {
	    breg = cp_prevbreg(breg);
	    lpcreg = (byte *) cp_pcreg(breg);
	  } else {
	    i = cell(breg+CP_SIZE);
	    i = int_val(i);	/* i now has the value of old hash_offset */
	    if (i != FIRST_HASH_NODE) {
	      tbreg = breg;
	      restore_regs_and_vars(tbreg, CP_SIZE+3);
	    }
	    lpcreg = (byte *) *(hash_base + hash_offset);
	    cell(breg+CP_SIZE) = makeint(hash_offset);
	  }
	}
	goto contcase;

/*----------------------------------------------------------------------*/

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
	int i;

	NodePtr = (NODEptr) (lpcreg - 1);
	if (Child(NodePtr) != NULL) {
	  psc_ptr = (Psc)Sibl(NodePtr);
	  reg_arrayptr = reg_array -1;
  	  num_vars_in_var_regs = -1;
  	  save_find_locx(ereg);
  	  for (i = get_arity(psc_ptr); i >= 1; i--) { pushreg(*(rreg+i)); }
	  lpcreg = (byte *) Child(NodePtr);
	} else lpcreg = (byte *) &fail_inst;
	}
	goto contcase;

/*----------------------------------------------------------------------*/

