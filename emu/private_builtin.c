/* File:      private_builtin.c
** Author(s): Work of thousands
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1999
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


/* This file is never committed into the repository, so you can put
   experimental code here.
   It is also a place for private builtins that you might be using for
   debugging. 
   Note: even though this is a single builtin, YOU CAN SIMULATE ANY
   NUMBER OF BUILTINS WITH IT.  */

#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <setjmp.h>
#include <stdlib.h>
#include <stdarg.h>
#ifndef WIN_NT
#include <unistd.h> 
#endif
#include <sys/stat.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "cinterf.h"
#include "memory_xsb.h"
#include "psc_xsb.h"
#include "heap_xsb.h"
#include "register.h"
#include "tries.h"
#include "choice.h"  
#include "flags_xsb.h"
#include "deref.h"
#include "ptoc_tag_xsb_i.h"

CPtr pmember_trust_addr, abduce_trust_addr;

#define SET_ABDUCTION_CP  1
#define NON_CHRONO_BACKTRACK 2
#define GET_BREG 3
#define CHECK_BREG 4
#define PRINT_CP 5
#define HASH_LIST 6

#ifdef TLS_DEBUG
#define xsb_dbgmsg(a)                               \
      tls_dbgmsg1 a
#else
#define xsb_dbgmsg(a)
#endif

 void tls_dbgmsg1(int log_level, char *description, ...)
{
  va_list args;

    va_start(args, description);
    fprintf(stddbg, description, args);
    va_end(args);
    fprintf(stddbg, "\n");
}

/*-------------------------------------------------------------------*/

xsbBool hash_list(Cell term)	
{
  int hash = 0;
  Cell addr, car;

  addr = term;
  XSB_Deref(addr);
  while (islist(addr)) { 
    car = cell(clref_val(addr));
    XSB_Deref(car);
    hash = hash +  int_val(car);
    addr = cell(clref_val(addr)+1);
    XSB_Deref(addr);
  }
  ctop_int(4, ihash(hash,ptoc_int(3)));
  return TRUE;
}

/*-------------------------------------------------------------------*/

/* TLS: for the selective backtracking to work, we can only bt over
   the abduction choice points themselves; not over Prolog or tabled
   choicepoints.  This function sets up these values. */

xsbBool  set_abduction_cp(void) {
  Psc pmember_psc, abduct_psc;
  int new_indicator;

  pmember_psc = 
    pair_psc(insert("private_member", (byte) 2, global_mod, &new_indicator));
  xsb_dbgmsg(("pmember = %x (%d) ep = %x %x %x\n",pmember_psc,
	 new_indicator,*get_ep(pmember_psc),
	 *(get_ep(pmember_psc) + 8),
	 *(get_ep(pmember_psc)+ 16)));
  pmember_trust_addr = (CPtr)(get_ep(pmember_psc)+ 16); 
  xsb_dbgmsg(("pmember_trust_addr = %x\n",pmember_trust_addr));

  abduct_psc = 
    pair_psc(insert("abduce_1", (byte) 6, global_mod, &new_indicator));
  abduce_trust_addr = (CPtr)(get_ep(abduct_psc)+ 16); 
  xsb_dbgmsg(("abduce_trust_addr = %x\n",abduce_trust_addr));
  return TRUE;
}

/*----------------------------------------------------------------*/

/* Start out by making new_breg equal the previous breg.  Then keep
    going back until you hit the target, or until you hit a choice
    point that doesn't point to "private_member".  At that stage you
    just fail.  Note that you don't have to do trail compaction as the
    failure takes care of the trail. */

xsbBool non_chrono_backtrack() {
  CPtr  new_breg, target_breg;
  target_breg = (CPtr) ptoc_int(2);
  if ((int) target_breg != -1) {
    xsb_dbgmsg(("    breg %x (%d %s) target_breg %x (%d %s) \n",
	   breg,breg,get_name(((Choice)(breg))->psc),
	   target_breg, target_breg,get_name(((Choice)(target_breg))->psc)));

    new_breg = breg;
 
    while ((new_breg < target_breg) 
	   && ((CPtr) *(new_breg) == pmember_trust_addr
	       || (CPtr) *(new_breg) == abduce_trust_addr)) {
      new_breg = (CPtr) cp_prevbreg(new_breg);                    
      xsb_dbgmsg(("    newbreg %x (%d %s)  \n",
	     new_breg,new_breg,get_name(((Choice)(new_breg))->psc)));
    }
    breg = new_breg;
    xsb_dbgmsg(("   final breg %x (%d)\n",new_breg,new_breg));
  } 
    pcreg = (pb)&fail_inst;
    return TRUE;
}

xsbBool check_breg() {
  if ((CPtr) *breg == pmember_trust_addr) 
    xsb_dbgmsg(("breg is abd\n"));
  else
    xsb_dbgmsg(("breg is not abd\n"));
  return TRUE;
}
    
xsbBool private_builtin(void)
{
  switch(ptoc_int(1)) {
  case SET_ABDUCTION_CP: 
    return set_abduction_cp();

  case NON_CHRONO_BACKTRACK: 
    return non_chrono_backtrack();

  case GET_BREG: {
    ctop_int(2,(int) breg);
    return TRUE;
  }
  case CHECK_BREG: 
    return check_breg();

  case PRINT_CP: {
    print_cp(1) ; return TRUE ;
  }

  case HASH_LIST: {
    return hash_list(ptoc_tag(2));
  }
  
  }
  return TRUE;
}
