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


#define STARTTRREG xtemp1
#define ENDTRREG xtemp2

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
#define switch_envs(tbreg)	{\
    STARTTRREG = (CPtr) trreg;\
    ENDTRREG = (CPtr) cp_trreg(tbreg);\
    trreg = cp_trreg(tbreg);\
    if (STARTTRREG != ENDTRREG) {\
      do {\
	while (STARTTRREG > ENDTRREG) {\
	  untrail((CPtr) trail_variable(STARTTRREG));\
	  STARTTRREG = (CPtr) trail_parent(STARTTRREG);\
	}\
	while (ENDTRREG > STARTTRREG) {\
	  ENDTRREG = (CPtr) trail_parent(ENDTRREG);\
	}\
      } while (STARTTRREG != ENDTRREG);\
      ENDTRREG = (CPtr) trreg;\
      while (ENDTRREG > STARTTRREG) {\
	cell((CPtr) trail_variable(ENDTRREG)) = (Cell) trail_value(ENDTRREG);\
	ENDTRREG = (CPtr) trail_parent(ENDTRREG);\
      }\
    }\
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

