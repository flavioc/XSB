/************************************************************************/
/*                                                                      */
/* XSB System                                                           */
/* Copyright (C) SUNY at Stony Brook, 1993                              */
/*                                                                      */
/* Everyone is granted permission to copy, modify and redistribute XSB, */
/* but only under the conditions described in the XSB Licence Agreement.*/
/* A copy of this licence is supposed to have been given to you along   */
/* with XSB so you can know your rights and responsibilities.           */
/* It should be in a file named LICENCE.                                */
/* Among other things, this notice must be preserved on all copies.     */
/*                                                                      */
/************************************************************************/

/*======================================================================
  File                  :  sw_envs.h
  Author(s)		:  Kostis Sagonas
  Last modification	:  October 13, 1998
========================================================================*/


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
/*    swenv_num++; */\
    STARTTRREG = (CPtr) trreg;\
    ENDTRREG = (CPtr) cp_trreg(tbreg);\
    trreg = cp_trreg(tbreg);\
    if (STARTTRREG != ENDTRREG) {\
      do {\
/*	while_num++; */\
	while (STARTTRREG > ENDTRREG) {\
/*	  btr_num++; */\
/*	  fprintf(stderr, "Untrailing: %p\n", trail_variable(STARTTRREG)); */\
	  untrail((CPtr) trail_variable(STARTTRREG));\
	  STARTTRREG = (CPtr) trail_parent(STARTTRREG);\
	}\
	while (ENDTRREG > STARTTRREG) {\
	  ENDTRREG = (CPtr) trail_parent(ENDTRREG);\
	}\
      } while (STARTTRREG != ENDTRREG);\
      ENDTRREG = (CPtr) trreg;\
      while (ENDTRREG > STARTTRREG) {\
/*	ftr_num++; */\
/*	fprintf(stderr, "Forward trailing: %p\n", trail_variable(ENDTRREG)); */\
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
extern CPtr temp_trreg;

#define table_undo_bindings(old_trreg) \
    while (trreg > (CPtr *) old_trreg) {\
      temp_trreg = *(--trreg); \
      untrail(temp_trreg); \
    }
#else
#define table_undo_bindings(old_trreg) \
    while (trreg > (CPtr *) old_trreg) {\
      untrail((CPtr) trail_variable(trreg));\
      trreg = trail_parent(trreg);\
    }
#endif

