/* File:      builtin.c
** Author(s): Xu, Warren, Sagonas, Swift
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1999
** Copyright (C) ECRC, Germany, 1990
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




static inline int prolog_call0(Cell term)
{
    Psc  psc;
    if (isconstr(term)) {
      int  disp;
      char *addr;
      psc = get_str_psc(term);
      addr = (char *)(clref_val(term));
      for (disp = 1; disp <= (int)get_arity(psc); ++disp) {
	bld_copy(reg+disp, cell((CPtr)(addr)+disp));
      }
    } else if (isstring(term)) {
      int  value;
      Pair sym = insert(string_val(term),0,(Psc)flags[CURRENT_MODULE],&value);
      psc = pair_psc(sym);
    } else {
      if (isnonvar(term))
	err_handle(TYPE, 1, "call", 1, "callable term", term);
      else err(INSTANTIATION, 1, "call", 1);
      pcreg = (pb)&fail_inst;
      return FALSE;
    }
    switch (get_type(psc)) {
    case T_PRED:
    case T_DYNA:
    case T_UDEF:
    default:
    case T_FORN:
      pcreg = get_ep(psc);
      break;
/****    case T_FORN:
#ifdef FOREIGN
      proc_ptr = (PFI) get_ep(psc);
      if (proc_ptr())
	pcreg = cpreg;
      else
	pcreg = (pb)&fail_inst;	 
#else
      xsb_exit("Foreign call in configuration that does not support it !");
#endif
      break;
    case T_UDEF:
    default:
      psc = synint_proc(psc, MYSIG_UNDEF, NULL);
      if (psc) pcreg = get_ep(psc);
      break;
*****/
    }
    if (call_intercept) intercept(psc);

    return TRUE;
}


static inline int prolog_code_call(Cell term, int value)
{
  Psc  psc;
  if (isconstr(term)) {
    int  disp;
    char *addr;
    psc = get_str_psc(term);
    addr = (char *)(clref_val(term));
    for (disp = 1; disp <= (int)get_arity(psc); ++disp) {
      bld_copy(reg+disp, cell((CPtr)(addr)+disp));
    }
    bld_int(reg+get_arity(psc)+1, value);
  } else psc = NULL; 
  return TRUE;
}
