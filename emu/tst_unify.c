/* File:      tst_unify.c
** Author(s): Ernie Johnson
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
#include "context.h"
#include "cell_xsb.h"
#include "register.h"
#include "memory_xsb.h"
#include "binding.h"
#include "psc_xsb.h"
#include "deref.h"
#include "subp.h"          /* xsbBool unify(Cell, Cell) */
#include "table_stats.h"
#include "trie_internals.h"
#include "tab_structs.h"
#include "choice.h"
#include "tst_aux.h"
#include "tst_utils.h"
#include "error_xsb.h"
#include "tst_common.h"


#include "xsb.unify.c"