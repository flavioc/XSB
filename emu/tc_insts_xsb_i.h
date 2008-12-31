/* File:      tc_insts_xsb_i.h
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

XSB_Start_Instr(trie_no_cp_str,_trie_no_cp_str)
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_no_cp_str"));
	NodePtr = (BTNptr) lpcreg;
	unify_with_trie_str;
	non_ftag_lpcreg;
XSB_End_Instr()

/* TLS: opfail below is actually the sibling trie node */
XSB_Start_Instr(trie_try_str,_trie_try_str) 
	CPtr tbreg;
#ifdef SLG_GC
	CPtr old_cptop;
#endif
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_try_str"));
	NodePtr = (BTNptr) lpcreg;
	save_find_locx(ereg);
	tbreg = top_of_cpstack;
#ifdef SLG_GC
	old_cptop = tbreg;
#endif
	save_trie_registers(tbreg);                        
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);  
#ifdef SLG_GC                                              
        cp_prevtop(tbreg) = old_cptop;
#endif                                                     
	breg = tbreg;
	hbreg = hreg;
	unify_with_trie_str;
	non_ftag_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_retry_str,_trie_retry_str) 
        CPtr tbreg;
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_retry_str"));
	NodePtr = (BTNptr) lpcreg;
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) =  (byte *)opfail;
	unify_with_trie_str;
	non_ftag_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_trust_str,_trie_trust_str) 
        CPtr tbreg;
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_trust_str"));
	NodePtr = (BTNptr) lpcreg;
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);	/* Remove this CP */
	restore_trail_condition_registers(breg);
	unify_with_trie_str;
	non_ftag_lpcreg;
XSB_End_Instr()

/*----------------------------------------------------------------------*/

XSB_Start_Instr(trie_no_cp_numcon,_trie_no_cp_numcon)
	TRIE_R_LOCK();
//        fprintf(stddbg, "trie_no_cp_numcon\n");
	NodePtr = (BTNptr) lpcreg;
	unify_with_trie_numcon;
	reg_arrayptr--;
	non_ftag_lpcreg;
XSB_End_Instr()


#ifdef UNDEFINED
%---------------------------------------------------------------------
Play area for experimenting with trie code -- there should be some optimizations that we can make.

XSB_Start_Instr(trie_no_cp_numcon,_trie_no_cp_numcon)
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_no_cp_numcon:"));
	NodePtr = (BTNptr) lpcreg;
	non_ftag_lpcreg;
	unify_with_trie_numcon;
	reg_arrayptr--;
XSB_End_Instr()

XSB_Start_Instr(trie_no_cp_numcon,_trie_no_cp_numcon)
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_no_cp_numcon:"));
	NodePtr = (BTNptr) lpcreg;
	non_ftag_lpcreg;
    while (isref(*reg_arrayptr)) {							
    if (*reg_arrayptr == follow(*reg_arrayptr))						
      goto TRIE_NUMCON_VAR;						
    *reg_arrayptr = follow(*reg_arrayptr);						    
  }									
  if (*reg_arrayptr != opatom) {						    
    Fail1;								
    XSB_Next_Instr();							
  }									
  reg_arrayptr--;							
  XSB_Next_Instr();							
									
TRIE_NUMCON_VAR:							
  while (isattv(*reg_arrayptr)) {							
    if (cell((CPtr) dec_addr(*reg_arrayptr)) == dec_addr(*reg_arrayptr))		     
      break; /* end of an attv */					
    else {								
      *reg_arrayptr = cell((CPtr) dec_addr(*reg_arrayptr));				    
      while (isref(*reg_arrayptr)) {						
	if (*reg_arrayptr == follow(*reg_arrayptr))					    
          break;							
	*reg_arrayptr = follow(*reg_arrayptr);						
      }									
    }									
  }									
  if (isref(*reg_arrayptr)) {							
    bind_ref((CPtr)*reg_arrayptr, opatom);					    
  }									
  else if (isattv(*reg_arrayptr)) {						
    attv_dbgmsg(">>>> add_interrupt in unify_with_trie_numcon\n");	
    add_interrupt(CTXTc cell(((CPtr)dec_addr(*reg_arrayptr) + 1)), opatom);	
    bind_int_tagged((CPtr)dec_addr(*reg_arrayptr), opatom);          		
  }									
  reg_arrayptr--;							
  XSB_End_Instr();							
End of play area
%---------------------------------------------------------------------
#endif

XSB_Start_Instr(trie_try_numcon,_trie_try_numcon) 
	CPtr tbreg;
#ifdef SLG_GC
	CPtr old_cptop;
#endif
	TRIE_R_LOCK();
//	fprintf(stddbg, "trie_try_numcon\n");
	NodePtr = (BTNptr) lpcreg;
	save_find_locx(ereg);
	tbreg = top_of_cpstack;
#ifdef SLG_GC
	old_cptop = tbreg;
#endif
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
#ifdef SLG_GC
	cp_prevtop(tbreg) = old_cptop;
#endif
	breg = tbreg;
	hbreg = hreg;
	non_ftag_lpcreg;
	unify_with_trie_numcon;
	reg_arrayptr--;
XSB_End_Instr()

XSB_Start_Instr(trie_retry_numcon,_trie_retry_numcon) 
	CPtr tbreg;
	TRIE_R_LOCK();
//	fprintf(stddbg, "trie_retry_numcon\n");
	NodePtr = (BTNptr) lpcreg;
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	non_ftag_lpcreg;
	unify_with_trie_numcon;
	reg_arrayptr--;
XSB_End_Instr()

XSB_Start_Instr(trie_trust_numcon,_trie_trust_numcon) 
	CPtr tbreg;
	TRIE_R_LOCK();
//	fprintf(stddbg, "trie_trust_numcon\n");
	NodePtr = (BTNptr) lpcreg;
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);
	restore_trail_condition_registers(breg);
	non_ftag_lpcreg;
	unify_with_trie_numcon;
	reg_arrayptr--;
XSB_End_Instr()

/*----------------------------------------------------------------------*/

XSB_Start_Instr(trie_no_cp_numcon_succ,_trie_no_cp_numcon_succ)
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_no_cp_numcon_succ"));
	NodePtr = (BTNptr) lpcreg;
	unify_with_trie_numcon;
	reg_arrayptr--;
	proceed_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_try_numcon_succ,_trie_try_numcon_succ) 
	CPtr tbreg;
#ifdef SLG_GC
	CPtr old_cptop;
#endif
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_try_numcon_succ"));
	NodePtr = (BTNptr) lpcreg;
	save_find_locx(ereg);
	tbreg = top_of_cpstack;
#ifdef SLG_GC
	old_cptop = tbreg;
#endif
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
#ifdef SLG_GC
	cp_prevtop(tbreg) = old_cptop;
#endif
	breg = tbreg;
	hbreg = hreg;
	unify_with_trie_numcon;
	reg_arrayptr--;
	proceed_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_retry_numcon_succ,_trie_retry_numcon_succ) 
	CPtr tbreg;
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_retry_numcon_succ"));
	NodePtr = (BTNptr) lpcreg;
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	unify_with_trie_numcon;
	reg_arrayptr--;
	proceed_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_trust_numcon_succ,_trie_trust_numcon_succ) 
	CPtr tbreg;
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_trust_numcon_succ"));
	NodePtr = (BTNptr) lpcreg;
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);
	restore_trail_condition_registers(breg);
	unify_with_trie_numcon;
	reg_arrayptr--;
	proceed_lpcreg;
XSB_End_Instr()

/*----------------------------------------------------------------------*/

XSB_Start_Instr(trie_no_cp_var,_trie_no_cp_var)
	TRIE_R_LOCK();
//        fprintf(stddbg,"trie_no_cp_var\n");
	NodePtr = (BTNptr) lpcreg;
	num_vars_in_var_regs = DecodeTrieVar(opatom);
        xsb_dbgmsg((LOG_TRIE_INSTR, "symbol number is %d\n",num_vars_in_var_regs));
	var_regs[num_vars_in_var_regs] = (CPtr) *reg_arrayptr;
#ifdef DEBUG_ASSERTIONS
        { int i = num_vars_in_var_regs;
	  if ((isref(var_regs[i])) &&
            ((var_regs[i] < (CPtr)glstack.low) || (var_regs[i] >= hreg)) &&
	    ((var_regs[i] < top_of_localstk) || (var_regs[i] >= (CPtr) glstack.high))) {
	    xsb_dbgmsg((LOG_DEBUG, "tc_insts_xsb_i.h (no_cp): var reg assigned bad 0x%p %d 0x%p",
		       hreg, i, var_regs[i])); }
	} 
#endif
	reg_arrayptr--;
	next_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_try_var,_trie_try_var) 
	CPtr tbreg;
#ifdef SLG_GC
	CPtr old_cptop;
#endif
	TRIE_R_LOCK();
//        fprintf(stddbg, "trie_try_var\n");
	NodePtr = (BTNptr) lpcreg;
	save_find_locx(ereg);
	tbreg = top_of_cpstack;
	/*	save_find_locx(ereg);*/
#ifdef SLG_GC
	old_cptop = tbreg;
#endif
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
#ifdef SLG_GC
	cp_prevtop(tbreg) = old_cptop;
#endif
	breg = tbreg;
	hbreg = hreg;
	num_vars_in_var_regs = DecodeTrieVar(opatom);
	var_regs[num_vars_in_var_regs] = (CPtr) *reg_arrayptr;
#ifdef DEBUG_ASSERTIONS
        { int i = num_vars_in_var_regs;
	  if ((isref(var_regs[i])) &&
            ((var_regs[i] < (CPtr)glstack.low) || (var_regs[i] >= hreg)) &&
	    ((var_regs[i] < top_of_localstk) || (var_regs[i] >= (CPtr) glstack.high))) {
	    xsb_dbgmsg((LOG_DEBUG, "tc_insts_xsb_i.h (try): var reg assigned bad 0x%p %d 0x%p",
		       hreg, i, var_regs[i]));
	  }
	} 
#endif
	reg_arrayptr--;
	next_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_retry_var,_trie_retry_var) 
	CPtr tbreg;
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_retry_var"));
	NodePtr = (BTNptr) lpcreg;
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	num_vars_in_var_regs = DecodeTrieVar(opatom);
	var_regs[num_vars_in_var_regs] = (CPtr) *reg_arrayptr;
#ifdef DEBUG_ASSERTIONS
        { int i = num_vars_in_var_regs;
	  if ((isref(var_regs[i])) &&
            ((var_regs[i] < (CPtr)glstack.low) || (var_regs[i] >= hreg)) &&
	    ((var_regs[i] < top_of_localstk) || (var_regs[i] >= (CPtr) glstack.high))) {
	    xsb_dbgmsg((LOG_DEBUG, "tc_insts_xsb_i.h (retry): var reg assigned bad 0x%p %d 0x%p",
		       hreg, i, var_regs[i]));
	  }
	} 
#endif
	reg_arrayptr--;
	next_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_trust_var,_trie_trust_var)  
	CPtr tbreg;
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_trust_var"));
	NodePtr = (BTNptr) lpcreg;
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);	/* Remove this CP */
	restore_trail_condition_registers(breg);
	num_vars_in_var_regs = DecodeTrieVar(opatom);
	var_regs[num_vars_in_var_regs] = (CPtr) *reg_arrayptr;
#ifdef DEBUG_ASSERTIONS
        { int i = num_vars_in_var_regs;
	  if ((isref(var_regs[i])) &&
            ((var_regs[i] < (CPtr)glstack.low) || (var_regs[i] >= hreg)) &&
	    ((var_regs[i] < top_of_localstk) || (var_regs[i] >= (CPtr) glstack.high))) {
	     xsb_dbgmsg((LOG_DEBUG, "tc_insts_xsb_i.h (trust): var reg assigned bad 0x%p %d 0x%p",
			hreg, i, var_regs[i]));
	  }
	} 
#endif
	reg_arrayptr--;
	next_lpcreg;
XSB_End_Instr()

/*----------------------------------------------------------------------*/

/* Documentation on trie_xxx_val instructions (TLS).  Documentation
   refers to trie_no_cp_val below, but appllies also to
   unify_with_trie_val macro.

These instructions can be somewhat confusing as they are, to some
extent, overcoded.  They can most easily be explained using reasoning
by cases.  First, note that tries are used in 3 main ways: a) variant
tabling, b) subsumptive tabling, and c) asserted tries, and the
behavior of the val instructions differs somewhat in each case.  

a) Variant Tabling.  In this case, the routine variant_answer_search()
distinguishes between usage 1) an attv of the call that is unchanged
by the answer; usage 2) an attv of the call that is changed to another
attv in the answer; usage 3) a variable in the call that is bound to
an attv; usage 4) a variable in the call that is bound to another
variable in the call by a binding in the answer.  The routine
variant_answer_search() generates trie_xxx_val instructions only
usages 1 and 4 above.  In usages 2 and 3 a trie_xxx_attv instruction
is generated.  Thus in variant tabling *reg_arrayptr will dereference
only to an attv iff trie_xxx_val dereferences to that same attv (case
b.2 in the code below corresponding to usage 1 ); and reg_arrayptr
will dereference to a ref vanilla variable iff the associated symbol
is a ref (case a corresponding to usage 4)

b) Subsumptive tabling: need to check

c) Asserted tries.  For asserted tries, the trie_xxx_val instructions
are generated only for variables (I think) -- but the corresponding
position can be anything you like: a vanilla variable (case a), an
attributed variable (case b), or a constant/structure/number/list
(case c).  Case b.2 will never happen as this depends on the trie
position being an attv, which, as stated, wont happen.

*/

XSB_Start_Instr(trie_no_cp_val,_trie_no_cp_val)
  Def2ops
  TRIE_R_LOCK();
  xsb_dbgmsg((LOG_TRIE_INSTR, "trie_no_cp_val"));
  NodePtr = (BTNptr) lpcreg;
{
  Cell cell2deref;		
  //  printf("*reg_arrayptr %p\n",*reg_arrayptr);
  XSB_Deref(*reg_arrayptr);    						
  if (isref(*reg_arrayptr)) {		                 // case a 
    cell2deref = (Cell)var_regs[(int)int_val(opatom)];			
    XSB_Deref(cell2deref);	       					
    if (cell2deref != *reg_arrayptr)					
      bind_ref((CPtr) *reg_arrayptr, cell2deref);			
  }									
  else if (isattv(*reg_arrayptr)) {			 // case b 		
    xsb_dbgmsg((LOG_TRIE_INSTR, "symbol number is %d\n",(int)int_val(opatom)));
    cell2deref = (Cell) var_regs[(int)int_val(opatom)];			
    XSB_Deref(cell2deref);     						
    if (*reg_arrayptr != cell2deref) {			// case b.1		
      /* Do not trigger attv interrupt! */			      
      
      bind_ref(clref_val(*reg_arrayptr), cell2deref);			
    }									
    else {					       // case b.2		
      attv_dbgmsg(">>>> keep old attr in unify_with_trie_val\n");	
    }									
  }									
  else {					      // case c			
    op1 = (Cell)*reg_arrayptr;						
    op2 = (Cell) var_regs[(int)int_val(opatom)];			
    if (unify(CTXTc op1,op2) == FALSE) {					
      Fail1;								
      XSB_Next_Instr();							
    }									
  }									
  reg_arrayptr--;							
}
/*   unify_with_trie_val; */
  next_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_try_val,_trie_try_val) 
  Def2ops
  CPtr tbreg;
#ifdef SLG_GC
	CPtr old_cptop;
#endif
  TRIE_R_LOCK();
  xsb_dbgmsg((LOG_TRIE_INSTR, "trie_try_val"));
  NodePtr = (BTNptr) lpcreg;
  save_find_locx(ereg);
  tbreg = top_of_cpstack;
#ifdef SLG_GC
	old_cptop = tbreg;
#endif
  save_trie_registers(tbreg);
  save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
#ifdef SLG_GC
	cp_prevtop(tbreg) = old_cptop;
#endif
  breg = tbreg;
  hbreg = hreg;
  unify_with_trie_val;
  next_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_retry_val,_trie_retry_val) 
  Def2ops
  CPtr tbreg;
  TRIE_R_LOCK();
  xsb_dbgmsg((LOG_TRIE_INSTR, "trie_retry_val"));
  NodePtr = (BTNptr) lpcreg;
  tbreg = breg;
  restore_regs_and_vars(tbreg, CP_SIZE);
  cp_pcreg(breg) = (byte *) opfail;
  unify_with_trie_val;
  next_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_trust_val,_trie_trust_val) 
  Def2ops
  CPtr tbreg;
  TRIE_R_LOCK();
  xsb_dbgmsg((LOG_TRIE_INSTR, "trie_trust_val"));
  NodePtr = (BTNptr) lpcreg;
  tbreg = breg;
  restore_regs_and_vars(tbreg, CP_SIZE);
  breg = cp_prevbreg(breg);	/* Remove this CP */
  restore_trail_condition_registers(breg);
  unify_with_trie_val;
  next_lpcreg;
XSB_End_Instr()

/*----------------------------------------------------------------------*/

XSB_Start_Instr(trie_no_cp_list,_trie_no_cp_list)
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_no_cp_list"));
	NodePtr = (BTNptr) lpcreg;
	unify_with_trie_list;
	non_ftag_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_try_list,_trie_try_list) 
	CPtr tbreg;
#ifdef SLG_GC
	CPtr old_cptop;
#endif
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_try_list"));
	NodePtr = (BTNptr) lpcreg;
	save_find_locx(ereg);
	tbreg = top_of_cpstack;
#ifdef SLG_GC
	old_cptop = tbreg;
#endif
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
#ifdef SLG_GC
	cp_prevtop(tbreg) = old_cptop;
#endif
	breg = tbreg;
	hbreg = hreg;
	unify_with_trie_list;
	non_ftag_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_retry_list,_trie_retry_list) 
	CPtr tbreg;
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_retry_list:"));
	NodePtr = (BTNptr) lpcreg;
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	unify_with_trie_list;
	non_ftag_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_trust_list,_trie_trust_list) 
	CPtr tbreg;
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_trust_list"));
	NodePtr = (BTNptr) lpcreg;
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);	/* Remove this CP */
	restore_trail_condition_registers(breg);
	unify_with_trie_list;
	non_ftag_lpcreg;
XSB_End_Instr()

/*----------------------------------------------------------------------*/
/* fail insts for deleted nodes - reclaim deleted returns at completion */
/*----------------------------------------------------------------------*/

XSB_Start_Instr(trie_no_cp_fail,_trie_no_cp_fail)
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_no_cp_fail"));
	lpcreg = (byte *) & fail_inst;
XSB_End_Instr()

XSB_Start_Instr(trie_trust_fail,_trie_trust_fail) 
	CPtr tbreg;
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_trust_fail"));
	NodePtr = (BTNptr) lpcreg;
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	breg = cp_prevbreg(breg);	/* Remove this CP */
	restore_trail_condition_registers(breg);
	lpcreg = (byte *) & fail_inst;
XSB_End_Instr()

XSB_Start_Instr(trie_try_fail,_trie_try_fail) 
	CPtr tbreg;
#ifdef SLG_GC
	CPtr old_cptop;
#endif
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_try_fail"));
	NodePtr = (BTNptr) lpcreg;
	save_find_locx(ereg);
	tbreg = top_of_cpstack;
#ifdef SLG_GC
	old_cptop = tbreg;
#endif
	save_trie_registers(tbreg);
	save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
#ifdef SLG_GC
	cp_prevtop(tbreg) = old_cptop;
#endif
	breg = tbreg;
	hbreg = hreg;
	lpcreg = (byte *) & fail_inst;
XSB_End_Instr()

XSB_Start_Instr(trie_retry_fail,_trie_retry_fail) 
	CPtr tbreg;
	TRIE_R_LOCK();
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_retry_fail"));
	NodePtr = (BTNptr) lpcreg;
	tbreg = breg;
	restore_regs_and_vars(tbreg, CP_SIZE);
	cp_pcreg(breg) = (byte *) opfail;
	lpcreg = (byte *) & fail_inst;
XSB_End_Instr()

/*----------------------------------------------------------------------*/
/* The following code, that handles hashing in coded tries, has been	*/
/* modified for garbage collection.  Choice points for hashes in tries,	*/
/* besides the usual trie argument registers (see file tr_code.h), also	*/
/* contain 3 fields with certain information about the hash bucket.	*/
/* The first and third of these fields are predefined integers and are	*/
/* now encoded as such.  The second field contains a malloc-ed address	*/
/* which is encoded as a STRING (that's how addresses are encoded in	*/
/* XSB -- see file cell_xsb.h) to prevent garbage collection from       */
/* treating it as a reference either to a WAM stack or to a CHAT area.	*/
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

XSB_Start_Instr(hash_opcode,_hash_opcode) 
	CPtr tbreg, temp_ptr_for_hash;
#ifdef SLG_GC
	CPtr old_cptop;
#endif
	BTHTptr hash_header;
	BTHTptr *hash_base;
	int hashed_hash_offset;

	xsb_dbgmsg((LOG_TRIE_INSTR, "hash_opcode"));
   /*
    *  Under new trie structure, NodePtr is actually pointing at a
    *  Hash Table Header struct.
    */
    hash_header = (BTHTptr) lpcreg;
    hash_base = (BTHTptr *) BTHT_BucketArray(hash_header);

	temp_ptr_for_hash = (CPtr)*reg_arrayptr;
        XSB_CptrDeref(temp_ptr_for_hash);
        if (!isref(temp_ptr_for_hash) 
	    && (*hash_base == NULL)){ 
            /* Ground call and no variables in hash table */
	    hash_nonvar_subterm(temp_ptr_for_hash,hash_header,
				hashed_hash_offset);
	    if(*(hash_base + hashed_hash_offset) == NULL)
	      /* fail to previous CPF */
	      lpcreg = (byte *) cp_pcreg(breg);
	    else
	      /* execute code of tries in this bucket */
	      lpcreg = (byte *) *(hash_base + hashed_hash_offset);
	}
        else {  
	  save_find_locx(ereg);
	  tbreg = top_of_cpstack;
#ifdef SLG_GC
	  old_cptop = tbreg;
#endif
	  save_trie_registers(tbreg);
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
	  //	  printf("makeaddr: tc_insts_xsb_i.h: %p\n",hash_header); //dsw!!!
	  cell(--tbreg) = makeaddr(hash_header); /* BUT NOT A STRING */
	  cell(--tbreg) = makeint(FIRST_HASH_NODE);
	  save_choicepoint(tbreg,ereg,(byte *)&hash_handle_inst,breg);
#ifdef SLG_GC
	  cp_prevtop(tbreg) = old_cptop;
#endif
	  breg = tbreg;
	  hbreg = hreg;
	  lpcreg = (byte *) &hash_handle_inst;
	} 
XSB_End_Instr()

/*
 *  Since this instruction is called immediately after 'hash_opcode' and
 *  is also backtracked to while exploring a bucket chain, a mechanism is
 *  needed to distinguish between the two cases.  Hence the use of the
 *  FIRST_HASH_NODE flag in the CPS.
 */
XSB_Start_Instr(hash_handle,_hash_handle)
    CPtr    tbreg;
    BTHTptr hash_hdr, *hash_base;
    int     hash_offset, hashed_hash_offset;

    xsb_dbgmsg((LOG_TRIE_INSTR, "hash_handle"));
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
      XSB_Deref(*reg_arrayptr);
      if (isref(*reg_arrayptr))   /* sanity check */
	xsb_exit(CTXTc "error_condition in hash_handle\n");

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
	xsb_exit(CTXTc "error_condition in hash_handle\n");
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
XSB_End_Instr()

/*----------------------------------------------------------------------*/

XSB_Start_Instr(trie_proceed,_trie_proceed)	/* This is essentially a "proceed" */
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_proceed:"));
	NodePtr = (BTNptr) lpcreg;
	proceed_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_root,_trie_root)      /* A no-op; begin processing with child */
	TRIE_R_LOCK()
	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_root:"));
	NodePtr = (BTNptr) lpcreg;
	lpcreg = (byte *) BTN_Child(NodePtr);
XSB_End_Instr()

XSB_Start_Instr(trie_fail_unlock,_trie_fail_unlock)
 	xsb_dbgmsg((LOG_TRIE_INSTR, "trie_fail_unlock"));
	TRIE_R_UNLOCK()
        breg = cp_prevbreg(breg);       /* Remove this CP */
        restore_trail_condition_registers(breg);
	lpcreg = (byte *) & fail_inst;
XSB_End_Instr()
/*
 * This is the embedded-trie instruction which is placed in the root of
 * asserted tries.  It looks a lot like both "return_table_code", which
 * prepares the engine to walk an answer trie, and "get_calls", which
 * prepares the engine to walk a call trie.  Maybe there's a way to
 * "unify" these operations now that all tries contain root nodes.
 */
XSB_Start_Instr(trie_assert_inst,_trie_assert_inst)
  Psc psc_ptr;
  int i;
#ifdef MULTI_THREAD_RWL
  CPtr tbreg;
#ifdef SLG_GC
  CPtr old_cptop;
#endif
#endif

  xsb_dbgmsg((LOG_TRIE_INSTR, "trie_assert"));
  NodePtr = (BTNptr) lpcreg;
  if (Child(NodePtr) != NULL) {
    TRIE_R_LOCK()
    psc_ptr = DecodeTrieFunctor(BTN_Symbol(NodePtr));
    reg_arrayptr = reg_array -1;
    num_vars_in_var_regs = -1;
    for (i = get_arity(psc_ptr); i >= 1; i--) { pushreg(*(rreg+i)); }
    lpcreg = (byte *) Child(NodePtr);
#ifdef MULTI_THREAD_RWL
/* save choice point for trie_unlock instruction */
    save_find_locx(ereg);
    tbreg = top_of_cpstack;
#ifdef SLG_GC
    old_cptop = tbreg;
#endif
    save_choicepoint(tbreg,ereg,(byte *)&trie_fail_unlock_inst,breg);
#ifdef SLG_GC
    cp_prevtop(tbreg) = old_cptop;
#endif
    breg = tbreg;
    hbreg = hreg;
#endif
  }
  else
    lpcreg = (byte *) &fail_inst;
XSB_End_Instr()

XSB_Start_Instr(trie_no_cp_attv,_trie_no_cp_attv)
  TRIE_R_LOCK();
  xsb_dbgmsg((LOG_TRIE_INSTR, "trie_no_cp_attv"));
  NodePtr = (BTNptr) lpcreg;
  unify_with_trie_attv;
  next_lpcreg
XSB_End_Instr()

XSB_Start_Instr(trie_try_attv,_trie_try_attv)
  CPtr tbreg;
#ifdef SLG_GC
	CPtr old_cptop;
#endif
  TRIE_R_LOCK();
  xsb_dbgmsg((LOG_TRIE_INSTR, "trie_try_attv"));
  NodePtr = (BTNptr) lpcreg;
  save_find_locx(ereg);
  tbreg = top_of_cpstack;
#ifdef SLG_GC
	old_cptop = tbreg;
#endif
  save_trie_registers(tbreg);
  save_choicepoint(tbreg,ereg,(byte *)opfail,breg);
#ifdef SLG_GC
	cp_prevtop(tbreg) = old_cptop;
#endif
  breg = tbreg;
  hbreg = hreg;
  unify_with_trie_attv;
  next_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_retry_attv,_trie_retry_attv) 
  CPtr tbreg;
  TRIE_R_LOCK();
  xsb_dbgmsg((LOG_TRIE_INSTR, "trie_retry_attv:"));
  NodePtr = (BTNptr) lpcreg;
  tbreg = breg;
  restore_regs_and_vars(tbreg, CP_SIZE);
  cp_pcreg(breg) = (byte *) opfail;
  unify_with_trie_attv;
  next_lpcreg;
XSB_End_Instr()

XSB_Start_Instr(trie_trust_attv,_trie_trust_attv) 
  CPtr tbreg;
  TRIE_R_LOCK();
  xsb_dbgmsg((LOG_TRIE_INSTR, "trie_trust_attv"));
  NodePtr = (BTNptr) lpcreg;
  tbreg = breg;
  restore_regs_and_vars(tbreg, CP_SIZE);
  breg = cp_prevbreg(breg);	/* Remove this CP */
  restore_trail_condition_registers(breg);
  unify_with_trie_attv;
  next_lpcreg;
XSB_End_Instr()

/*----------------------------------------------------------------------*/
