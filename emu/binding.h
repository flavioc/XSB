/* File:      binding.h
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


/* --- trailing ------------------------------------------------------- */

/*
 *  Structure of a (forward) trail frame is
 *
 * low mem
 *   |   Address  (of trailed variable->a non-tagged ptr)
 *   |   Value  (to which the variable is being bound->tagged)
 *   V   Dynamic Link  (ptr to link field of prev frame)
 * high mem
 *
 *  TRreg and TRFreg always point to the Dynamic Link field of a trail frame.
 */

#ifdef WAM_TRAIL

#define pushtrail0(addr,val)  \
   if ((char *)(top_of_trail) > ((char *)(top_of_cpstack) - 10)) {\
     handle_tcpstack_overflow();\
   }\
   *(trreg++) = addr

#else

#define TRAIL_FRAME_SIZE  3

#define pushtrail0(addr,val)  \
   if (trfreg > trreg) {\
     if ((char *)trfreg > ((char *)(top_of_cpstack) - 10)) {\
       handle_tcpstack_overflow();\
     }\
     *(trfreg+3) = (CPtr) trreg;\
     trreg = trfreg + 3;\
     *(trreg-1) = (CPtr) val;\
     *(trreg-2) = addr;\
   }\
   else {\
     if ((char *)trreg > ((char *)(top_of_cpstack) - 10)) {\
       handle_tcpstack_overflow();\
     }\
     trreg = trreg+3;\
     *trreg = (CPtr) trreg-3;\
     *(trreg-1) = (CPtr) val;\
     *(trreg-2) = addr;\
   }
#endif

#ifdef CHAT
#define conditional(a)	( ((a) >= ebreg) || ((a) < hbreg) )
#else
#define conditional(a)	( ((a) >= ebreg || (a) >= efreg) || \
			  ((a) < hbreg  || (a) < hfreg) )
#endif

#define pushtrail(a,v)	if (conditional(a)) { pushtrail0(a,v); }
#define dpushtrail(a,v) pushtrail0(a,v)

/* --- binding -------------------------------------------------------- */

#define bind_int(addr, val)	pushtrail(addr, makeint(val));\
				bld_int(addr, val)

#define bind_float(addr, val)	pushtrail(addr, (Cell) makefloat(val)); \
				bld_float(addr, val)

#define bind_ref(addr, val)	pushtrail(addr, val);\
				bld_ref(addr, val)

#define dbind_ref(addr, val)	dpushtrail(addr, val);\
				bld_ref(addr, val)

#define bind_cs(addr, str)	pushtrail(addr, makecs(str));\
				bld_cs(addr, str)

#define bind_string(addr, str)	pushtrail(addr, makestring(str));\
				bld_string(addr,str)

#define bind_nil(addr)		pushtrail(addr, makenil);\
				bld_nil(addr)

#define bind_list(addr, list)	pushtrail(addr, makelist(list));\
				bld_list(addr, list)

#define bind_copy0(addr, val)	pushtrail(addr, val); *(addr) = val

#define bind_copy(addr, val)	pushtrail(addr, val); *(addr) = val

/* value trail MUST be used because first CP cell has cp_trreg = 0 !!!! */

#define untrail(addr) bld_free(addr)

/* --- testing location of variable ----------------------------------- */

#define ereg_on_top(t_ereg)	t_ereg < ebreg

#define efreg_on_top(t_ereg) efreg < ebreg  && efreg < t_ereg

/* --- for building vals on the heap ---------------------------------- */

#define nbldval(OP1)\
    deref(OP1);\
    if (isnonvar(OP1) ||\
	(/*(CPtr)OP1 >= glstack.low &&*/ (CPtr)OP1 <= top_of_heap)) {\
      new_heap_node(hreg, OP1);\
    }\
    else {	/* local stack vars point to heap vars and not vice-versa! */\
      bind_ref((CPtr)(OP1), hreg);\
      new_heap_free(hreg);\
    }
