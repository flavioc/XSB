/* File:      basictypes.h
** Author(s): kifer
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



#ifndef BASIC_TYPES_INCLUDED

#ifdef BITS64
typedef long prolog_int ;
#else
typedef int prolog_int ;
#endif

typedef double prolog_float ;
typedef int reg_num;

/* CELL and PROLOG_TERM are defined identically.
   However, CELL is used to refer to elements of (slg-)WAM stacks, while
   PROLOG_TERM is used in the C interface to point to a cell containing 
   the outer functor of a prolog term. */
typedef unsigned long prolog_term;

typedef short  xsbBool;

#ifndef __RPCNDR_H__
typedef unsigned char byte;
#endif
typedef unsigned int counter;
typedef unsigned long word;
typedef byte *pb;
typedef word *pw;
typedef int (*PFI)(void);
// typedef int *int_ptr;


#endif /* BASIC_TYPES_INCLUDED */

#define BASIC_TYPES_INCLUDED
