/* File:      sub_tables_xsb_i.h
** Author(s): Flávio Cruz
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

#ifdef SUBSUMPTION_XSB
#define CreateHeapFunctor(Symbol) { \
  CPtr heap_var_ptr;  \
  int arity, i; \
  Psc symbolPsc;  \
  symbolPsc = (Psc)cs_val(Symbol);  \
  arity = get_arity(symbolPsc); \
  bld_cs(hreg, hreg + 1); \
  bld_functor(++hreg, symbolPsc); \
  for(heap_var_ptr = hreg + arity, i = 0; \
      i < arity;  \
      heap_var_ptr--, i++) {  \
    bld_free(heap_var_ptr); \
    TermStack_Push((Cell)heap_var_ptr); \
  } \
  hreg = hreg + arity + 1;  \
}
#define CreateHeapList() {  \
  bld_list(hreg, hreg + 1); \
  hreg = hreg + 3;  \
  bld_free(hreg - 1); \
  TermStack_Push((Cell)(hreg - 1)); \
  bld_free(hreg - 2); \
  TermStack_Push((Cell)(hreg - 2)); \
}
#else
#define CreateHeapFunctor(Symbol) { \
  Functor functor;													\
	int arity, i;													\
  CPtr old_h = H++;               \
																	\
	functor = (Functor)RepAppl(Symbol);	\
	arity = ArityOfFunctor(functor);  \
	Term tf = Yap_MkNewApplTerm(functor,arity);	\
  *old_h = tf;        \
	for (i = arity; i >= 1; i--)			{		\
    Cell c = *(RepAppl(tf) + i);  \
		TermStack_Push(c);	\
	} \
}
#define CreateHeapList() {  \
  CPtr old_h = H++;  \
  Term tl = Yap_MkNewPairTerm();	\
  *old_h = tl;  \
	TermStack_Push(*(RepPair(tl) + 1));	\
	TermStack_Push(*(RepPair(tl)));	\
}

/* define WAM registers */
#define trreg TR
#define hreg H
#define hbreg HB
#define ereg E
#define trfreg TR_FZ
#define cpreg CP
#define top_of_trail ((trreg > trfreg) ? trreg : trfreg)

#define unify(TERM1, TERM2) Yap_unify(TERM1, TERM2)

#endif