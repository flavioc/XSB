/* File:      residual.c
** Author(s): Kostis Sagonas
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


#include <stdio.h>

#include "configs/config.h"
#include "debugs/debug.h"


/* special debug includes */
#include "debugs/debug_residual.h"

#include "auxlry.h"
#include "cell.h"
#include "psc.h"
#include "register.h"
#include "heap.h"
#include "binding.h"
#include "tries.h"
#include "xmacro.h"

/*----------------------------------------------------------------------*/

#ifdef DEBUG_RESIDUAL
extern void print_subgoal(FILE *, SGFrame);
#endif

/*----------------------------------------------------------------------*/

static Cell cell_array[500];

/*----------------------------------------------------------------------*/

#define build_subgoal_args(SUBG)	\
	load_solution_trie(arity, &cell_array[arity-1], subg_leaf_ptr(SUBG))

/*----------------------------------------------------------------------*/

void build_delay_list(CPtr delay_list, IDEs idl)
{
    IDE  ide;
    Psc  psc;
    int  i, j, arity;
    CPtr head, tail;
    SGFrame subg;
    NODEptr ans_subst;

#ifdef DEBUG_HEAP
    fprintf(stderr, "delay_list starts at %p\n", hreg);
#endif
    i = 0;
    if (idl != NULL && !isnil(idl)) {
      head = hreg+2;
      tail = hreg+1;
      bind_list(delay_list, hreg);
      ide = ides_ide(idl);
      subg = ide_subgoal(ide);
      psc = ti_psc_ptr(subg_tip_ptr(subg));
      arity = get_arity(psc);
      if ((ans_subst = ide_ans_subst(ide)) == NULL) {
	follow(hreg++) = makecs(head);
	new_heap_functor(head, tnot_psc);
	if (arity == 0) {
	  bind_string(head, get_name(psc));
	  hreg += 3;
	} else {
	  sreg = head+1;
	  follow(head++) = makecs(sreg);
	  hreg += arity+4; /* need arity(tnot)+2+arity(psc)+1 new cells */
	  new_heap_functor(sreg, psc);
	  for (j = 1; j <= arity; j++) {
	    new_heap_free(sreg);
	    cell_array[arity-j] = cell(sreg-1);
	  }
	  build_subgoal_args(subg);
	}
      } else {	/* positive delayed element */
	if (arity == 0) {
          new_heap_string(hreg, get_name(psc));
	  hreg++;
        } else {
	  sreg = head;
	  follow(hreg++) = makecs(head);
	  hreg += arity+2;
#ifdef DEBUG_HEAP
	  fprintf(stderr, "Put a new functor %p at loc %p\n", psc, sreg);
#endif
	  new_heap_functor(sreg, psc);
	  for (j = 1; j <= arity; j++) {
	    new_heap_free(sreg);
#ifdef DEBUG_HEAP
	    fprintf(stderr, "Binding cell_array[%d] to %p\n",
			     arity-j, (CPtr)cell(sreg-1));
#endif
	    cell_array[arity-j] = cell(sreg-1);
	  }
	  build_subgoal_args(subg);
	  for (i = 0, j = num_heap_term_vars-1; j >= 0; j--) {
	    cell_array[i++] = (Cell)var_addr[j];
	  }
	  load_solution_trie(i, &cell_array[i-1], ans_subst);
	}
      }
      build_delay_list(tail, ides_next_ide(idl));
    } else {
      bind_nil(delay_list);
    }
}

/*----------------------------------------------------------------------*/

#ifdef DEBUG_RESIDUAL

void fprint_delay_list(FILE *fp, IDEs idl)
{
    IDE ide;

    fprintf(fp, "[");
    while (idl != NULL) {
      ide = ides_ide(idl);
      if (ide_ans_subst(ide) == NULL) {
	fprintf(fp, "tnot(");
	print_subgoal(fp, ide_subgoal(ide));
	fprintf(fp, ")");
      } else {
	print_subgoal(fp, ide_subgoal(ide));
      }
      if ((idl = ides_next_ide(idl)) != NULL) fprintf(fp, ", ");
    }
    fprintf(fp, "]");
}

#endif	/* DEBUG_RESIDUAL */

/*---------------------- end of file residual.c ------------------------*/
