/* File:      sw_envs.h
** Author(s): Terry Swift, Rui Marques, Kostis Sagonas
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1998
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


#if (!defined(CHAT))
#define freeze_and_switch_envs(tbreg, CPsize)\
    if (bfreg > breg) {\
      bfreg = breg + CPsize;\
      if (trfreg < trreg)  trfreg = trreg;  \
      if (hfreg < hreg)  hfreg = hreg; \
      xtemp1 = top_of_localstk; \
      if (efreg > xtemp1) efreg = xtemp1;\
    }\
    switch_envs(tbreg)
#endif

#ifdef CHAT
#define switch_envs(tbreg)	undo_bindings(tbreg)
#else
#define switch_envs(tbreg) {				\
							\
   CPtr *start_trreg, *end_trreg;			\
							\
   start_trreg = trreg;					\
   end_trreg = cp_trreg(tbreg);				\
   trreg = cp_trreg(tbreg);				\
   if (start_trreg != end_trreg) {			\
     do {						\
       while (start_trreg > end_trreg) {		\
	 untrail((CPtr) trail_variable(start_trreg));	\
	 start_trreg = trail_parent(start_trreg);	\
       }						\
       while (end_trreg > start_trreg) {		\
	 end_trreg = trail_parent(end_trreg);		\
       }						\
     } while (start_trreg != end_trreg);		\
     end_trreg = trreg;					\
     while (end_trreg > start_trreg) {			\
       cell((CPtr) trail_variable(end_trreg)) =		\
	 (Cell) trail_value(end_trreg);			\
       end_trreg = trail_parent(end_trreg);		\
     }							\
   }							\
  }
#endif

#ifdef MEASURE_WAM_STUFF
#define undo_bindings(TBREG)	\
    undbind_num++;\
    xtemp1 = (CPtr) cp_trreg(TBREG);\
    table_undo_bindings(xtemp1);
#else
#define undo_bindings(TBREG)	\
    xtemp1 = (CPtr) cp_trreg(TBREG);\
    table_undo_bindings(xtemp1);
#endif

#ifdef WAM_TRAIL
#define table_undo_bindings(old_trreg) \
    while (trreg > (CPtr *) old_trreg) {\
      CPtr temp_trreg = *(--trreg); \
      untrail(temp_trreg); \
    }
#else
#define table_undo_bindings(old_trreg) \
    while (trreg > (CPtr *) old_trreg) {\
      untrail((CPtr) trail_variable(trreg));\
      trreg = trail_parent(trreg);\
    }
#endif

