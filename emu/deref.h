/* File:      deref.h
** Author(s): Jiyang Xu, Terrance Swift, Kostis Sagonas
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
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


/* deref expects argument of type Cell */

#define XSB_Deref(op) XSB_Deref2(op,break)

/*
#define XSB_Deref2(op,stat) while (isref(op)) { \
                    if (op == follow(op)) \
                        stat; \
                    op = follow(op); }
*/

/* XSB_Deref2 is changed to consider attributed variables */
#define XSB_Deref2(op, stat) {				\
  while (isref(op)) {					\
    if (op == follow(op))				\
      stat;						\
    op = follow(op);					\
  }							\
  while (isattv(op)) {					\
    if (cell((CPtr) dec_addr(op)) == dec_addr(op)) 	\
      break; /* end of an attv */			\
    else {						\
      op = cell((CPtr) dec_addr(op));			\
      while (isref(op)) {				\
	if (op == follow(op))				\
          stat;						\
	op = follow(op);				\
      }							\
    }							\
  }							\
}

/*
#define XSB_CptrDeref(op) while (isref(op)) { \
			 if (op == (CPtr) cell(op)) \
  			     break; \
			 op = (CPtr) cell(op); }
*/

#define XSB_CptrDeref(op) {				\
  while (isref(op)) {					\
    if (op == (CPtr) cell(op)) break;			\
    op = (CPtr) cell(op);				\
  }							\
  while (isattv(op)) {					\
    if (cell((CPtr) dec_addr(op)) == dec_addr(op))	\
      break;						\
    else {						\
      op = (CPtr) cell((CPtr) dec_addr(op));		\
      while (isref(op)) {				\
	if (op == (CPtr) cell(op)) break;		\
	op = (CPtr) cell(op);				\
      }							\
    }							\
  }							\
}

#define printderef(op) while (isref(op) && op > 0) { \
			 if (op==follow(op)) \
  			    break; \
			 op=follow(op); }
