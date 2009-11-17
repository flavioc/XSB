/* File:      incr_xsb.c  -- support for incremental evaluation
** Author(s): Diptikalyan Saha, C.R. Ramakrishnan
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 2001,2002
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

#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>
#include <stdlib.h>

#include "auxlry.h"
#include "context.h"
#include "cell_xsb.h"
#include "psc_xsb.h"
#include "cinterf.h"
#include "deref.h"
#include "memory_xsb.h"
#include "heap_xsb.h"
#include "register.h"
#include "tries.h"
#include "trie_internals.h"
#include "tab_structs.h"
#include "choice.h"
#include "subp.h"
#include "error_xsb.h"
#include "tr_utils.h"
#include "incr_xsb.h"
#include "debug_xsb.h"
#include "flags_xsb.h"
#include "ptoc_tag_xsb_i.h"
#include "call_graph_xsb.h"
#include "term_psc_xsb_i.h"
#include "rw_lock.h"
#include "token_xsb.h"
#include "binding.h"
#include "tst_utils.h"

/* This is already defined in builtin.c; need to factor this out */
#define ptoc_addr(regnum)	(void *)ptoc_int(CTXTc regnum)

/*******
 ** Builtin for handling incremental evaluation 
 *******/
extern TIFptr get_tip(CTXTdeclc Psc);

xsbBool incr_eval_builtin(CTXTdecl)
{
  int builtin_number = ptoc_int(CTXTc 1);

  
  switch(builtin_number) {
  case GET_AFFECTED_CALLS: {
    /*
      This builtin creates a (prolog) list which contains all the
      affected calls in postorder.     
    */
    int rc = create_call_list(CTXT);
    affected=eneetq();
    changed=eneetq();
    return rc;
    break;
  }
  case GET_CHANGED_CALLS: {
    /*
      This builtin creates a (prolog) list which contains all the
      changed calls.     
    */
    return create_changed_call_list(CTXT);    
    break;
  }
    
  case GET_CALL_GRAPH: {

    printf("<%d,%d>",call_node_count,call_edge_count);
    break;  
  }
    
  case INVALIDATE_SF: {
    /*
      Find all affected calls and put that in a list.

    */
       
    const int sfreg=2;
    VariantSF sf=ptoc_addr(sfreg);
    callnodeptr c=subg_callnode_ptr(sf);
    invalidate_call(c); 
    
    break;
  }
  case INVALIDATE_CALLNODE: {
    /*
      Find all affected calls and put that in a list.

    */
    
    
    const int callreg=2;
    callnodeptr c=ptoc_addr(callreg);
    invalidate_call(c); 
    
    break;
  }
  
  

    
  case  GET_CALLNODEPTR_INCR:{
    const int regLeafChild=3; 
    if(IsNULL(BTN_Child(Last_Nod_Sav))){
      xsb_warn("Callnodeptr is NULL! Invalid incrdynamic predicate.");
      return FALSE;
    }
    ctop_int(CTXTc regLeafChild, (Integer)BTN_Child(Last_Nod_Sav));    
    break;
  }
  case INCR_STATISTICS: {        
    printf("Not Implemented\n");
    break;
  }
  case PRINT_CALL: {        
    const int regSF=2;
    VariantSF sf=ptoc_addr(regSF);
    if(IsIncrSF(sf))
      print_call_node(sf->callnode);
    else{
      sfPrintGoal(stdout,sf,NO);printf(" is not incrementally tabled\n"); 
    }
      
    break;
  }
  case PSC_SET_INCR: {
    Psc psc = (Psc)ptoc_addr(2);   
    if (get_tabled(psc) != T_TABLED_SUB) {
      set_incr(psc,ptoc_int(CTXTc 3));
    //    printf("%s/%d:%u incr set to %d\n",get_name(psc),get_arity(psc),psc,ptoc_int(3));
    } else {
      xsb_abort("Cannot incrementally maintain a subsumptive table (%s/%d)",get_name(psc),get_arity(psc));
    }
    break;
  }

  case PSC_GET_INCR: {
    Psc psc = (Psc)ptoc_addr(2);   
    ctop_int(CTXTc 3,get_incr(psc));
    break;
  }

  case IMM_DEPEND_LIST: {
    const int sfreg=2;
    VariantSF sf=ptoc_addr(sfreg);

    return(imm_depend_list(CTXTc sf->callnode));
      
    break;
  }


  case IMM_DEPENDENT_ON_LIST: {
    const int sfreg=2;
    VariantSF sf=ptoc_addr(sfreg);
    return imm_dependent_on_list(CTXTc sf->callnode);
    
    break;
  }


  case IS_AFFECTED: {
    const int sfreg=2;
    VariantSF sf=ptoc_addr(sfreg);
    if(IsNonNULL(sf)){
      callnodeptr c=sf->callnode;
      if(IsNonNULL(c)&&(c->falsecount!=0))
	return TRUE;
      else
	return FALSE;
    }else
      return FALSE;
    
    break;
  }
    

  default:
    xsb_exit(CTXTc "Unknown Incremental Evaluation Builtin: %d\n.", builtin_number);
    break;
  }
  return TRUE;
}

