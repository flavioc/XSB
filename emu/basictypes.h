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
#include "xsb_config.h"
#include "cell_def_xsb.h"

/*******************
* Definitions for the basic Integer and Floating point types. 
*   Each type varies depending on whether the BITS64 and FAST_FLOATS flags are set.
*   These types should be used in place of 'float' and 'int'
*******************/
#ifdef BITS64
typedef long prolog_int ;
typedef long Integer ;
typedef unsigned long UInteger ;
#define MY_MAXINT ((long)0x7fffffffffffff)
#else
typedef int prolog_int ;
typedef int Integer ;
typedef unsigned int UInteger ;
#define MY_MAXINT ((int)0x7fffffff)	/* Modified by Kostis */
#endif
#define MY_MININT (-MY_MAXINT - 1)
typedef double prolog_float;

#ifndef FAST_FLOATS
typedef double Float;
#else
typedef float Float;
#endif
/*******************************/


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


/*******************
* Definitions for converter types. 
*   These types are used as bit manipulation tools, exploiting the shared memory of a union.

* constraints: each member of the union should have equal size, in bytes.
*******************/
typedef union float_conv {
  Cell i;
  float f;
} FloatConv;

typedef union float_to_ints_conv {
   Float float_val;
   struct {
    UInteger high;
    UInteger low;
   }int_vals;
    
} FloatToIntsConv;

typedef union cell_to_bytes_conv {
   Cell cell_val;
   struct {
    byte b1;
    byte b2;
    byte b3;
    byte b4;
   }byte_vals;
    
} CellToBytesConv;


#endif /* BASIC_TYPES_INCLUDED */

#define BASIC_TYPES_INCLUDED
