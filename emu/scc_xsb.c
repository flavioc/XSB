/* File:      scc_xsb.c
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


#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "register.h"
#include "psc_xsb.h"
#include "tries.h"
#include "macro_xsb.h"

#if (!defined(LOCAL_EVAL))

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

static void DFS_DGT_visit(ComplStackFrame u)
{
    EPtr eptr;

    compl_visited(u) = TRUE;
    for (eptr=compl_DGT_edges(u); eptr != NULL; eptr=next_edge(eptr)) {
      if (!compl_visited(edge_to_node(eptr)))
	DFS_DGT_visit(edge_to_node(eptr));
    }
}

ComplStackFrame DFS_DGT(ComplStackFrame leader)
{
    ComplStackFrame u, max_u = NULL;

    for (u = leader; u >= (ComplStackFrame)openreg; u--)
      if (!compl_visited(u)) {
	DFS_DGT_visit(u); max_u = u;
      }
    return max_u;
}

/*----------------------------------------------------------------------*/
/*  find_independent_scc(ComplStackFrame)				*/
/*	Finds the subgoals in the same SCC as the subgoal that is	*/
/*	given as input.  The subgoals are indicated by marking the	*/
/*	"visited" field of their completion stack frame.		*/
/*----------------------------------------------------------------------*/

void find_independent_scc(ComplStackFrame u)
{
    EPtr eptr;

    compl_visited(u) = TRUE;
    for (eptr=compl_DG_edges(u); eptr != NULL; eptr=next_edge(eptr)) {
      if (!compl_visited(edge_to_node(eptr)))
	find_independent_scc(edge_to_node(eptr));
    }
}

/*----------------------------------------------------------------------*/

#endif
