/* File:      cell_xsb.h
** Author(s): David S. Warren, Jiyang Xu, Terrance Swift
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1993-19989
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


#ifndef CONFIG_INCLUDED
#error "File xsb_config.h must be included before this file"
#endif

/*======================================================================*/
/* CELL: an element in the local stack or global stack (heap).		*/
/*									*/
/* Corresponds to the basic word type of the physical machine.		*/
/* This module also defines the basic INTEGER and REAL types to use	*/
/* by the Prolog interpreter						*/
/*									*/
/* Interface routines							*/
/*	They are put in the different files indicated below, according  */
/*	to the tagging scheme you used.					*/
/* The functions (macros, indeed) include:				*/
/*	cell_tag(cell):		give any cell, return its tag		*/
/*	isnonvar(dcell):	check if the cell is a ref or a var.	*/
/*				(an index in VARASINDEX)		*/
/*				when used with derefed cell, check if	*/
/*				the cell is instanciated 		*/
/*	int_val(dcell):		assume derefed integer cell, return its */
/*				value in machine format			*/
/*	clref_val(dcell):	assume derefed cs or list cell, return  */
/*				a CPtr to the address it points to.	*/
/*	cs_val(dcell):		assume derefed cs cell, return Pair	*/
/*	bld_int(addr, val):	build a new integer cell		*/
/*	bld_float(addr, val):	build a new float cell			*/
/*	bld_ref(addr, val):	build a new reference cell		*/
/*	bld_cs(addr, str):	build a new cs cell			*/
/*	bld_string(addr, str):	build a new string cell			*/
/*	bld_list(addr, list):	build a new list cell			*/
/*	bld_functor(addr, psc):	build a functor cell for a new structure*/
/*	bld_free(addr):		build a new free variable 		*/
/*	bld_copy(dest, source): build a copy of the given cell.		*/
/*			if the source is a free var, the copy is indeed */
/*			a ref cell. Need special checking when free var */
/*			is not a pointer to itself but a special tag.	*/
/*	bld_copy0(dest, src):   the same as bld_copy except assume      */
/*                    non-var, or where semantics is to resume/set.	*/
/*                    (in set CP and resume CP)				*/
/*                    For variable as selfpointer, no differnce.    	*/
/*======================================================================*/

/* ==== types of cells =================================================*/

#include "celltags_xsb.h"

/*======================================================================*/
/* CELL: an element in the local stack or global stack (heap).		*/
/*======================================================================*/

#include "cell_def_xsb.h"

typedef Cell *CPtr;

#ifdef BITS64
typedef long Integer ;
#else
typedef int Integer ;
#endif
 
#ifdef BITS64
typedef double Float ;
#else
typedef float Float ;
#endif

#define cell(cptr) *(cptr)
#define follow(cell) (*(CPtr)(cell))

extern Float asfloat(Cell);
extern Cell  makefloat(Float);
extern Float getfloatval(Cell);

#define isref(cell)  (!((word)(cell)&0x3))
#define isnonvar(cell) ((word)(cell)&0x3)		/* dcell -> xsbBool */

#define cell_tag(cell) ((word)(cell)&0x7)

/*======================================================================*/
/*======================================================================*/

/* for HP machines (works for HP700 & 9000 series) take bits 0-1, 28, 29, 31 */
#if defined(HP700)
#define enc_int(val) ( ((Integer)(val) & 0xb0000003) ?\
			(((Integer)(val) << 5) | 0x10) :\
			(((Integer)(val) << 1) & 0x80000000) |\
			(((Integer)(val) << 3) & 0x7fffffe0) )
#define dec_int(dcell) ( ((Integer)(dcell) & 0x10) ?\
                        ((Integer)(dcell) >> 5) :\
			(((Integer)(dcell) >> 1) & 0x40000000) |\
			(((Integer)(dcell) >> 3) & 0x0ffffffc) )

/* Fewer bit representation of pointers (take bits 0-1, 28-29 only) */
#define enc_addr(addr) (((Cell)(addr) & 0xc0000000) |\
			 (((Cell)(addr) << 2) & 0x3ffffff0))
#define dec_addr(dcell) (((Cell)(dcell) & 0xc0000000) |\
			 (((Cell)(dcell) >> 2) & 0x0ffffffc))

#elif defined(IBM)
/* for IBM machines (like RS-6000) take bits 0-1, 27, 30-31 */
#define enc_int(val) ( ((Integer)(val) & 0xc8000003) ?\
			(((Integer)(val) << 5) | 0x10) :\
			(((Integer)(val) << 2) & 0xc0000000) |\
			(((Integer)(val) << 3) & 0x3fffffe0) )
#define dec_int(dcell) ( ((Integer)(dcell) & 0x10) ?\
			((Integer)(dcell) >> 5) :\
			(((Integer)(dcell) >> 2) & 0x30000000) |\
			(((Integer)(dcell) >> 3) & 0x07fffffc) )
/* Fewer bit representation of pointers */
#define enc_addr(addr) ((Cell)(addr) << 2)
#define dec_addr(dcell) (((Cell)(dcell) >> 2) & 0x3ffffffc)

#elif (defined(MIPS_BASED) || defined(SOLARIS_x86)) 
#ifdef BITS64
/* 64 bit, take bits 0-1, 61-63 */
/* Encoded integers/addresses */
#define enc_int(val) ( ((Integer)(val) & 0xe000000000000003) ?\
	(((Integer)(val) << 5) | 0x10) :\
	((Integer)(val) << 3) )
#define dec_int(dcell) ( ((Integer)(dcell) & 0x10) ?\
	((Integer)(dcell) >> 5) :\
	(((Integer)(dcell) >> 3) & 0x1ffffffffffffffc) )

/* Fewer bit representation of pointers */
#define enc_addr(addr) ((Cell)(addr) << 2)
#define dec_addr(dcell) (((Cell)(dcell) >> 2) & 0x3ffffffffffffffc)
#else
/* take bits 0-1, 29-31 */
/* Encoded integers/addresses */
#define enc_int(val) ( ((Integer)(val) & 0xe0000003) ?\
	(((Integer)(val) << 5) | 0x10) :\
	((Integer)(val) << 3) )
#define dec_int(dcell) ( ((Integer)(dcell) & 0x10) ?\
	((Integer)(dcell) >> 5) :\
	(((Integer)(dcell) >> 3) & 0x1ffffffc) )

/* Fewer bit representation of pointers */
#define enc_addr(addr) ((Cell)(addr) << 2)
#define dec_addr(dcell) (((Cell)(dcell) >> 2) & 0x3ffffffc)
#endif

#elif (defined(BIG_MEM) || defined(WIN_NT) || defined(LINUX_ELF))
/* take bits 0-1, 30-31 */
/* BIG_MEM allows Solaris/Sun machines to use 1 gig of memory */

#define enc_int(val) ( ((Integer)(val) & 0xc0000003) ?\
		       (((Integer)(val) << 4) | 0x08) :\
		       ((Integer)(val) << 2) )
#define dec_int(val) ( ((Integer)(val) & 0x08) ?\
		       ((Integer)(val) >> 4) :\
		       (((Integer)(val) >> 2) & 0x3ffffffc) )

#define enc_addr(addr) ((Cell)(addr) << 1)
#define dec_addr(dcell) (((Cell)(dcell) >> 1) & 0x7ffffffc)

#else
/* standard representation of integers */
#define enc_int(val) ((Integer)(val) << 3)
#define dec_int(val) ((Integer)(val) >> 3)

/* standard encoding of pointers */
#define enc_addr(addr) ((Cell)(addr) << 3)
#define dec_addr(dcell) ((Cell)(dcell) >> 3)
#endif

/*======================================================================*/

/* integer manipulation */
#define int_val(dcell) (Integer)dec_int(dcell)
#define makeint(val) (Cell)((enc_int(val)) | XSB_INT)

/* string manipulation */
#define string_val(dcell) (char *)dec_addr(dcell)
#define makestring(str) (Cell)(enc_addr(str) | XSB_STRING)

/* special-case strings [] */
#define	makenil		makestring(nil_sym)

/* pointer manipulation */
#define cs_val(dcell) (Pair)dec_addr(dcell)
#define makecs(str) (Cell)(enc_addr(str) | XSB_STRUCT)
#define clref_val(dcell) (CPtr)dec_addr(dcell)
#define makelist(list) (Cell)(enc_addr(list) | XSB_LIST)
#define makeattv(attv) (Cell)(enc_addr(attv) | XSB_ATTV)
#define trievar_val(dcell) (Integer)dec_int(dcell)
#define maketrievar(val) (Cell)(enc_int(val) | XSB_TrieVar)

#define addr_val(dcell) int_val(dcell)
#define makeaddr(val) makeint(val)

/* common representations */
#define vptr(dcell) (CPtr)(dcell)
#define float_val(dcell) getfloatval(dcell)
#define ref_val(dcell) (CPtr)(dcell)

#define bld_nil(addr) cell(addr) = makenil
#define bld_string(addr, str) cell(addr) = makestring(str)
#ifdef TAG_ON_LOAD
#define bld_int_tagged(addr, val) cell(addr) = val
#define bld_int(addr, val) cell(addr) = makeint(val)
#define bld_float_tagged(addr, val) cell(addr) = val
#define bld_float(addr, val) cell(addr) = makefloat((float)val)
#else
#define bld_int(addr, val) cell(addr) = makeint(val)
#define bld_int_tagged(addr, val) cell(addr) = makeint(val)
#define bld_float(addr, val) cell(addr) = makefloat((float)val)
#define bld_float_tagged(addr, val) cell(addr) = makefloat((float)val)
#endif
#define bld_ref(addr, val) cell(addr) = (Cell)(val)
#define bld_cs(addr, str) cell(addr) = makecs(str)
#define bld_list(addr, list) cell(addr) = makelist(list)
#define bld_attv(addr, attv) cell(addr) = makeattv(attv)
#define bld_functor(addr, psc) cell(addr) = (word)psc
#define bld_copy0(addr, val) cell(addr) = val
#define bld_copy(addr, val) cell(addr) = val
/* tls -- this bld_copy wont work for VARASINDEX */
#define bld_free(addr) cell(addr) = (Cell)(addr) /* CPtr => XSB_FREE cell */

#define isinteger(dcell) (cell_tag(dcell)==XSB_INT)	/* dcell -> xsbBool */
#define isfloat(dcell) (cell_tag(dcell)==XSB_FLOAT)	/* dcell -> xsbBool */
#define isconstr(dcell) (cell_tag(dcell)==XSB_STRUCT)	/* dcell -> xsbBool */
#define islist(dcell) (cell_tag(dcell)==XSB_LIST)	/* dcell -> xsbBool */
#define isattv(dcell) (cell_tag(dcell)==XSB_ATTV)	/* dcell -> xsbBool */

#define isstring(dcell) (cell_tag(dcell)==XSB_STRING)
#define numequal(num1, num2) num1 == num2

#define isnumber(dcell)	((isinteger(dcell)) || (isfloat(dcell)))
#define isconstant(dcell)  ( isstring(dcell) || isnumber(dcell) )
#define isatom(dcell)	((isstring(dcell)) || \
			 (isconstr(dcell) && get_arity(get_str_psc(dcell))==0))
#define isatomic(dcell)	((isstring(dcell)) || (isnumber(dcell)) || \
			 (isconstr(dcell) && get_arity(get_str_psc(dcell))==0))

#define isnil(dcell) (isstring(dcell) && (char *)string_val(dcell) == nil_sym)

#define get_str_psc(dcell) ((cs_val(dcell))->psc_ptr)

/*======================================================================*/
/* Miscellaneous							*/
/*======================================================================*/

#define MAX_ARITY	255

#define arity_integer(dcell) \
			(isinteger(dcell) && int_val(dcell) >= 0 \
					  && int_val(dcell) <= MAX_ARITY)

#define CELL_DEFS_INCLUDED
