/*  -*-c-*-  Make sure this file comes up in the C mode of emacs */ 
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
	xsb_dbgmsg("trie_no_cp_str");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	unify_with_trie_str;
	non_ftag_lpcreg;
	goto contcase;

case trie_try_str: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_try_str");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	save_find_locx(ereg);
	tbreg = top_of_cpstack;
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	unify_with_trie_str;
	non_ftag_lpcreg;
	goto contcase;
}

case trie_retry_str: {
        CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_retry_str");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) =  (byte *)opfail;
	unify_with_trie_str;
	non_ftag_lpcreg;
	goto contcase;
}

case trie_trust_str: {
        CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_trust_str");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);	/* Remove this CP */
	restore_trail_condition_registers(breg);
	unify_with_trie_str;
	non_ftag_lpcreg;
	goto contcase;
}

/*----------------------------------------------------------------------*/

case trie_no_cp_numcon:
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_no_cp_numcon:");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	unify_with_trie_numcon;
	reg_arrayptr--;
	non_ftag_lpcreg;
	goto contcase;

case trie_try_numcon: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_try_numcon");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	save_find_locx(ereg);
	tbreg = top_of_cpstack;
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	unify_with_trie_numcon;
	reg_arrayptr--;
	non_ftag_lpcreg;
	goto contcase;
}

case trie_retry_numcon: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_retry_numcon");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	unify_with_trie_numcon;
	reg_arrayptr--;
	non_ftag_lpcreg;
	goto contcase;
}

case trie_trust_numcon: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_trust_numcon");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);
	restore_trail_condition_registers(breg);
	unify_with_trie_numcon;
	reg_arrayptr--;
	non_ftag_lpcreg;
	goto contcase;
}

/*----------------------------------------------------------------------*/

case trie_no_cp_numcon_succ:
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_no_cp_numcon_succ");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	unify_with_trie_numcon;
	reg_arrayptr--;
	proceed_lpcreg;
	goto contcase;

case trie_try_numcon_succ: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_try_numcon_succ");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	save_find_locx(ereg);
	tbreg = top_of_cpstack;
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	unify_with_trie_numcon;
	reg_arrayptr--;
	proceed_lpcreg;
	goto contcase;
}

case trie_retry_numcon_succ: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_retry_numcon_succ");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	unify_with_trie_numcon;
	reg_arrayptr--;
	proceed_lpcreg;
	goto contcase;
}

case trie_trust_numcon_succ: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_trust_numcon_succ");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);
	restore_trail_condition_registers(breg);
	unify_with_trie_numcon;
	reg_arrayptr--;
	proceed_lpcreg;
	goto contcase;
}

/*----------------------------------------------------------------------*/

case trie_no_cp_var:
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_no_cp_var");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	num_vars_in_var_regs = DecodeTrieVar(opatom);
	var_regs[num_vars_in_var_regs] = (CPtr) *reg_arrayptr;
#ifdef DEBUG
        { int i = num_vars_in_var_regs;
	  if ((isref(var_regs[i])) &&
            ((var_regs[i] < (CPtr)glstack.low) || (var_regs[i] >= hreg)) &&
	    ((var_regs[i] < top_of_localstk) || (var_regs[i] >= (CPtr) glstack.high))) {
	    xsb_dbgmsg("tc_insts.i (no_cp): var reg assigned bad 0x%p %d 0x%p",
		       hreg, i, var_regs[i]); }
	} 
#endif
	reg_arrayptr--;
	next_lpcreg;
	goto contcase;

case trie_try_var: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_try_var");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	save_find_locx(ereg);
	tbreg = top_of_cpstack;
	save_find_locx(ereg);
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	num_vars_in_var_regs = DecodeTrieVar(opatom);
	var_regs[num_vars_in_var_regs] = (CPtr) *reg_arrayptr;
#ifdef DEBUG
        { int i = num_vars_in_var_regs;
	  if ((isref(var_regs[i])) &&
            ((var_regs[i] < (CPtr)glstack.low) || (var_regs[i] >= hreg)) &&
	    ((var_regs[i] < top_of_localstk) || (var_regs[i] >= (CPtr) glstack.high))) {
	    xsb_dbgmsg("tc_insts.i (try): var reg assigned bad 0x%p %d 0x%p",
		       hreg, i, var_regs[i]);
	  }
	} 
#endif
	reg_arrayptr--;
	next_lpcreg;
	goto contcase;
}

case trie_retry_var: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_retry_var");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	num_vars_in_var_regs = DecodeTrieVar(opatom);
	var_regs[num_vars_in_var_regs] = (CPtr) *reg_arrayptr;
#ifdef DEBUG
        { int i = num_vars_in_var_regs;
	  if ((isref(var_regs[i])) &&
            ((var_regs[i] < (CPtr)glstack.low) || (var_regs[i] >= hreg)) &&
	    ((var_regs[i] < top_of_localstk) || (var_regs[i] >= (CPtr) glstack.high))) {
	    xsb_dbgmsg("tc_insts.i (retry): var reg assigned bad 0x%p %d 0x%p",
		       hreg, i, var_regs[i]);
	  }
	} 
#endif
	reg_arrayptr--;
	next_lpcreg;
	goto contcase;
}

case trie_trust_var:  {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_trust_var");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);	/* Remove this CP */
	restore_trail_condition_registers(breg);
	num_vars_in_var_regs = DecodeTrieVar(opatom);
	var_regs[num_vars_in_var_regs] = (CPtr) *reg_arrayptr;
#ifdef DEBUG
        { int i = num_vars_in_var_regs;
	  if ((isref(var_regs[i])) &&
            ((var_regs[i] < (CPtr)glstack.low) || (var_regs[i] >= hreg)) &&
	    ((var_regs[i] < top_of_localstk) || (var_regs[i] >= (CPtr) glstack.high))) {
	     xsb_dbgmsg("tc_insts.i (trust): var reg assigned bad 0x%p %d 0x%p",
			hreg, i, var_regs[i]);
	  }
	} 
#endif
	reg_arrayptr--;
	next_lpcreg;
	goto contcase;
}

/*----------------------------------------------------------------------*/

case trie_no_cp_val:
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_no_cp_val");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	unify_with_trie_val;
	next_lpcreg;
	goto contcase;

case trie_try_val: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_try_val");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	save_find_locx(ereg);
	tbreg = top_of_cpstack;
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	unify_with_trie_val;
	next_lpcreg;
	goto contcase;
}

case trie_retry_val: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_retry_val");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	unify_with_trie_val;
	next_lpcreg;
	goto contcase;
}

case trie_trust_val: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_trust_val");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);	/* Remove this CP */
	restore_trail_condition_registers(breg);
	unify_with_trie_val;
	next_lpcreg;
	goto contcase;
}

/*----------------------------------------------------------------------*/

case trie_no_cp_list:
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_no_cp_list");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	unify_with_trie_list;
	non_ftag_lpcreg;
	goto contcase;

case trie_try_list: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_try_list");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	save_find_locx(ereg);
	tbreg = top_of_cpstack;
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	unify_with_trie_list;
	non_ftag_lpcreg;
	goto contcase;
}

case trie_retry_list: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_retry_list:");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	unify_with_trie_list;
	non_ftag_lpcreg;
	goto contcase;
}

case trie_trust_list: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_trust_list");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);	/* Remove this CP */
	restore_trail_condition_registers(breg);
	unify_with_trie_list;
	non_ftag_lpcreg;
	goto contcase;
}

/*----------------------------------------------------------------------*/
/* fail insts for deleted nodes - reclaim deleted returns at completion */
/*----------------------------------------------------------------------*/

case trie_no_cp_fail:
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_no_cp_fail");
#endif	
	lpcreg = (byte *) & fail_inst;
	goto contcase;

case trie_trust_fail: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_trust_fail");
#endif	
	NodePtr = (BTNptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);	/* Remove this CP */
	restore_trail_condition_registers(breg);
	lpcreg = (byte *) & fail_inst;
	goto contcase;
}

case trie_try_fail: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_try_fail");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	save_find_locx(ereg);
	tbreg = top_of_cpstack;
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
	breg = tbreg;
	hbreg = hreg;
	lpcreg = (byte *) & fail_inst;
	goto contcase;
}

case trie_retry_fail: {
	CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_retry_fail");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	lpcreg = (byte *) & fail_inst;
	goto contcase;
}

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

/* Structure of the CPF created by hash_opcode:

             +-------------+
             |             |   LOW MEMORY
             |    Trail    |
             |             |
             |      |      |
             |      V      |
             |             |
             |             |
             |      ^      |
             |      |      |
             |             |
             |  CP Stack   |
             |             |
             |             |
             |=============|
             | Rest of CPF |--- Basic CPF (no argument registers)
             |-------------|
             | HASH index  | - last bucket explored
             |  ht header  | - ptr to HashTable Header structure
             | HASH_IS flag| - var/nonvar status of topmost term
             |-------------|    (the next to be unified with the trie)
             |     n+1     |_
             |reg_array[n] | \
             |      .      |  |
             |      .      |  |- Subterms to be unified with trie
             |      .      |  |
             |reg_array[0] |_/
             |-------------|
             |      m      |_
             | var_regs[m] | \
             |      .      |  |
             |      .      |  |- Variables encountered so far along trie path
             |      .      |  |   (m is -1 if no variables were encountered)
             | var_regs[0] |_/
             |=============|
             |      .      |
             |      .      |
             |      .      |    HIGH MEMORY
             +-------------+
*/

case hash_opcode: {
	CPtr tbreg, temp_ptr_for_hash;;
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("hash_opcode");
#endif
   /*
    *  Under new trie structure, NodePtr is actually pointing at a
    *  Hash Table Header struct.
    */
	NodePtr = (BTNptr) (lpcreg - 1);
	save_find_locx(ereg);
	tbreg = top_of_cpstack;
	save_trie_registers(tbreg);
	temp_ptr_for_hash = (CPtr)*reg_arrayptr;
	cptr_deref(temp_ptr_for_hash);
	if (isref(temp_ptr_for_hash))
	  cell(--tbreg) = makeint(HASH_IS_FREE);
	else
	  cell(--tbreg) = makeint(HASH_IS_NOT_FREE);
    /*
     *  For normal trie nodes, this next CP value was given as the beginning
     *  of the hash table (bucket array).  With the new trie structure, I
     *  instead pass in the header, allowing access to all needed fields,
     *  including the bucket array.
     */
	cell(--tbreg) = makestring(NodePtr);
	cell(--tbreg) = makeint(FIRST_HASH_NODE);
	save_choicepoint(tbreg,ereg,(byte *)&hash_handle_inst,breg);
	breg = tbreg;
	hbreg = hreg;
	lpcreg = (byte *) &hash_handle_inst;
	goto contcase;
}

/*
 *  Since this instruction is called immediately after 'hash_opcode' and
 *  is also backtracked to while exploring a bucket chain, a mechanism is
 *  needed to distinguish between the two cases.  Hence the use of the
 *  FIRST_HASH_NODE flag in the CPS.
 */
case hash_handle:
  {
    CPtr    tbreg;
    BTHTptr hash_hdr, *hash_base;
    int     hash_offset, hashed_hash_offset;

#ifdef PVR_DEBUG_TC_INSTS
    xsb_dbgmsg("hash_handle");
#endif
    hash_offset = int_val(cell(breg+CP_SIZE));
    hash_hdr = (BTHTptr) string_val(cell(breg+CP_SIZE+1));
    hash_base = (BTHTptr *) BTHT_BucketArray(hash_hdr);
    if ( int_val(cell(breg + CP_SIZE + 2)) == HASH_IS_NOT_FREE ) {
      /* Unify with nonvar */
      if ( (hash_offset != FIRST_HASH_NODE) &&
	   (hash_offset != NO_MORE_IN_HASH) ) {
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE+3);
      }
      deref(*reg_arrayptr);
      if (isref(*reg_arrayptr))   /* sanity check */
	xsb_exit("error_condition in hash_handle\n");

      hash_nonvar_subterm(*reg_arrayptr,hash_hdr,hashed_hash_offset);
      if (hash_offset == FIRST_HASH_NODE) {
	if (*hash_base == NULL) { /* No Variables in hash table */
	  breg = cp_prevbreg(breg);   /* dealloc this CPF */
	  if(*(hash_base + hashed_hash_offset) == NULL)
	    /* fail to previous CPF */
	    lpcreg = (byte *) cp_pcreg(breg);
	  else
	    /* execute code of tries in this bucket */
	    lpcreg = (byte *) *(hash_base + hashed_hash_offset);
	}
	else {   /* save hashed-to bucket, explore bucket 0 */
	  if ( (*(hash_base + hashed_hash_offset) == NULL) ||
	       (hashed_hash_offset == 0) )
	    breg = cp_prevbreg(breg);   /* dealloc this CPF */
	  else
	    cell(breg + CP_SIZE) = makeint(hashed_hash_offset);
	  lpcreg = (byte *) *hash_base;
	}
      }
      else if (hash_offset == hashed_hash_offset) {
	/* explore hashed-to bucket */
	lpcreg = (byte *)*(hash_base + hash_offset);
	breg = cp_prevbreg(breg);
      }
      else {
	xsb_error("Hash Offset %d, HHO %d",
		  hash_offset, hashed_hash_offset);
	xsb_exit("error_condition in hash_handle\n");
      }
    }
    else {  /* unification of trie with variable term */
      find_next_nonempty_bucket(hash_hdr,hash_base,hash_offset);
      if (hash_offset == NO_MORE_IN_HASH) {
	breg = cp_prevbreg(breg);
	lpcreg = (byte *) cp_pcreg(breg);
      }
      else {
	if ( int_val(cell(breg+CP_SIZE)) != FIRST_HASH_NODE ) {
	  tbreg = breg;
	  restore_regs_and_vars(tbreg, CP_SIZE+3);
	}
	lpcreg = (byte *) *(hash_base + hash_offset);
	cell(breg+CP_SIZE) = makeint(hash_offset);
      }
    }
    goto contcase;
  }

/*----------------------------------------------------------------------*/

case trie_proceed:	/* This is essentially a "proceed" */
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_proceed:");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	num_vars_in_var_regs = -1;
	proceed_lpcreg;
	goto contcase;

case trie_root:      /* A no-op; begin processing with child */
#ifdef PVR_DEBUG_TC_INSTS
	xsb_dbgmsg("trie_root:");
#endif
	NodePtr = (BTNptr) (lpcreg - 1);
	lpcreg = (byte *) BTN_Child(NodePtr);
	goto contcase;

/*
 * This is the embedded-trie instruction which is placed in the root of
 * asserted tries.  It looks a lot like both "return_table_code", which
 * prepares the engine to walk an answer trie, and "get_calls", which
 * prepares the engine to walk a call trie.  Maybe there's a way to
 * "unify" these operations now that all tries contain root nodes.
 */
case trie_assert_inst:
{
  Psc psc_ptr;
  int i;

  NodePtr = (BTNptr) (lpcreg - 1);
  if (Child(NodePtr) != NULL) {
    psc_ptr = DecodeTriePSC(BTN_Symbol(NodePtr));
    reg_arrayptr = reg_array -1;
    num_vars_in_var_regs = -1;
    save_find_locx(ereg);
    for (i = get_arity(psc_ptr); i >= 1; i--) { pushreg(*(rreg+i)); }
    lpcreg = (byte *) Child(NodePtr);
  }
  else
    lpcreg = (byte *) &fail_inst;
  goto contcase;
}	

case trie_no_cp_attv:
{
#ifdef PVR_DEBUG_TC_INSTS
  xsb_dbgmsg("trie_no_cp_attv");
#endif
  NodePtr = (BTNptr) (lpcreg - 1);
  unify_with_trie_attv;
  next_lpcreg
  goto contcase;
}

case trie_try_attv:
{
  CPtr tbreg;
#ifdef PVR_DEBUG_TC_INSTS
  xsb_dbgmsg("trie_try_attv");
#endif
  NodePtr = (BTNptr) (lpcreg - 1);
  save_find_locx(ereg);
  tbreg = top_of_cpstack;
  save_trie_registers(tbreg);
  save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
  breg = tbreg;
  hbreg = hreg;
  unify_with_trie_attv;
  next_lpcreg;
  goto contcase;
}

/*----------------------------------------------------------------------*/
