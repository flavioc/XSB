/* File:      cutils.c
** Author(s): Terrance Swift, Kostis Sagonas
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
#include <string.h>

#include "configs/config.h"
#include "debugs/debug.h"

#include "auxlry.h"
#include "cell.h"
#include "xsberror.h"
#include "register.h"
#include "binding.h"
#include "memory.h"
#include "tries.h"
#include "choice.h"
#include "deref.h"
#include "psc.h"
#include "xmacro.h"

/*======================================================================*/
/* The following routines copy a term into some memory area, referred
   to as Buffer.  The exported routine (buff_copyterm) is used mainly
   in findall/3, and in copyterm/4.  It is passed as input:
     R1: +The Term to copy.
     R2: +The Buffer in which the copy will be put.
     R3: +WordDisp: the word of the Buffer in which to put the copy
	  (or a pointer to the copy).
     R4: +Start: offset of the next free location in the Buffer,
	  before the copy is performed.
     R5: +Size: length of the Buffer (in bytes).

   and the routine returns:
	 -End: offset of the first free location after the copying.

   The memory area (Buffer) to copy the term into, and its size could
   be global, and that's the purpose of buff_copy_term().

   Variables are copied into the Buffer and the copied variables are
   pointed into the buffer and trailed.  Thus later binding of these
   `outside' variables will cause the copied variables to be changed,
   too.  If, however, the copyterm call is failed over, the variables
   in the Buffer will be ``disconnected'' from the outer variables.	*/
/*======================================================================*/

static pb addr;
long  size;
long  size_lim;
static int buff_copy_term(Cell, long, long); /* JF: added static */

/*----------------------------------------------------------------------*/

Cell buff_copyterm(Cell term, pb addr_arg,
		   long disp, long start, long size_arg)
{
    addr = addr_arg; size = size_arg; size_lim = size-258*sizeof(Cell);
    return buff_copy_term(term, disp, start);
}

/*----------------------------------------------------------------------*/

static int buff_copy_term(Cell term, long disp, long start)
{
    char message[80];
    int  arg, arity;
    long end, newdisp, newstart;

    deref(term);	/* I wish this were unnecessary, but it is not!	*/

    switch(cell_tag(term)) {
    case FREE: case REF1:
/*	fprintf(stderr, "Copying a variable\t");	*/
	if ((pb)term < addr || (pb)term > addr+size) {
		/* var not in buffer, trail */
	   bld_free(vptr(addr+disp));
           bind_ref(vptr(term), (CPtr)(addr+disp));
	} else {            /* already in buffer */
	   bld_ref((CPtr)(addr+disp), vptr(term));
	}
	end = start;
	break;
    case INT: case STRING: case FLOAT:
/*	fprintf(stderr, "Copying an integer, string or float\t");	*/
	bld_copy(vptr(addr+disp), term);
	end = start;
	break;
    case LIST:
/*	fprintf(stderr, "Copying a list\t");	*/
	bld_list(vptr(addr+disp), (CPtr)(addr+start));
	newstart = start + 2*sizeof(Cell);
	end = buff_copy_term(cell(clref_val(term)), start, newstart);
	if (end < 0) return end;
	newstart = start + sizeof(Cell);
	end = buff_copy_term(cell(clref_val(term)+1), newstart, end);
	if (end < 0) return end;
	break;
    case CS:
/*	fprintf(stderr, "Copying a structure (%s/%d)\t",
		get_name(get_str_psc(term)), get_arity(get_str_psc(term))); */
	bld_cs(vptr(addr+disp), (Pair)(addr+start));
	*(pw)(addr+start) = (Cell)get_str_psc(term);
	newdisp = start + sizeof(Cell);
	arity = get_arity(get_str_psc(term));
	newstart = newdisp + arity*sizeof(Cell);
	for (arg=1; arg <= arity; arg++) {
	    newstart = buff_copy_term(cell(clref_val(term)+arg),
				      newdisp, newstart);
	    if (newstart < 0) return newstart;
	    newdisp = newdisp + sizeof(Cell);
	}
	end = newstart;
	break;
    default:
	sprintf(message, "Term type (tag = %ld) not handled by buff_copyterm.",
		(Cell)cell_tag(term));
	xsb_abort(message);
	end = 0;
	break;
    }

    if (end < size_lim) return end; else return -4;
}

