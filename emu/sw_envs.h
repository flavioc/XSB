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
#define freeze_and_switch_envs(tbreg, CPsize)	\
  if (bfreg > breg) {				\
    CPtr local_top;				\
    bfreg = breg + CPsize;			\
    if (trfreg < trreg)  trfreg = trreg;	\
    if (hfreg < hreg)  hfreg = hreg;		\
    local_top = top_of_localstk;		\
    if (efreg > local_top) efreg = local_top;	\
  }						\
  switch_envs(tbreg)
#endif

#ifdef CHAT
#define switch_envs(tbreg)	undo_bindings(tbreg)
#else
/*
 * If PRE_IMAGE_TRAIL is never used, the line of untrail2 in the following
 * macro should be changed to:
 * 	untrail((CPtr) trail_variable(start_trreg));
 */
#define switch_envs(tbreg) {						\
  CPtr *start_trreg, *end_trreg, *parent, *tmp;				\
									\
  start_trreg = trreg;							\
  end_trreg = trreg = cp_trreg(tbreg);					\
  parent = trail_parent(end_trreg);					\
  if (start_trreg != end_trreg) {					\
    do {								\
      while (start_trreg > end_trreg) {					\
	untrail2(start_trreg, (Cell) trail_variable(start_trreg));	\
	start_trreg = trail_parent(start_trreg);			\
      }									\
      while (end_trreg > start_trreg) {					\
	tmp = parent;							\
	parent = trail_parent(parent);					\
	*tmp = (CPtr) end_trreg;					\
	end_trreg = tmp;						\
      }									\
    } while (start_trreg != end_trreg);					\
    tmp = trail_parent(end_trreg);					\
    *end_trreg = (CPtr) parent;						\
    parent = tmp;							\
    while (end_trreg < trreg) {						\
      tmp = parent;							\
      cell((CPtr)((Cell)trail_variable(tmp) & ~PRE_IMAGE_MARK)) =	\
	(Cell) trail_value(tmp);					\
      parent = trail_parent(parent);					\
      *tmp = (CPtr) end_trreg;						\
      end_trreg = tmp;							\
    }									\
  }									\
}
#endif

#ifdef MEASURE_WAM_STUFF
#define undo_bindings(TBREG) {			\
   CPtr *old_trreg = cp_trreg(TBREG);		\
   undbind_num++;				\
   table_undo_bindings(old_trreg);		\
}
#else
#define undo_bindings(TBREG) {			\
   CPtr *old_trreg = cp_trreg(TBREG);		\
   table_undo_bindings(old_trreg);		\
}
#endif

#ifdef WAM_TRAIL
/*
 * If PRE_IMAGE_TRAIL is never used, the line of untrail2 in the following
 * macro should be changed to:
 * 	untrail(temp_trreg);
 */
#define table_undo_bindings(old_trreg) {	\
  while (trreg > (CPtr *) old_trreg) {		\
    CPtr temp_trreg = *(--trreg);		\
    untrail2(trreg, (Cell)temp_trreg);		\
  }						\
}
#else
/*
 * If PRE_IMAGE_TRAIL is never used, the line of untrail2 in the following
 * macro should be changed to:
 * 	untrail((CPtr) trail_variable(trreg));
 */
#define table_undo_bindings(old_trreg) {		\
  while (trreg > (CPtr *) old_trreg) {			\
    untrail2(trreg, (Cell) trail_variable(trreg));	\
    trreg = trail_parent(trreg);			\
  }							\
}
#endif

