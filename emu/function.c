/* File:      function.c
** Author(s): Jiyang Xu
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


#include "xsb_config.h"
#include "xsb_debug.h"

#include <math.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "register.h"
#include "memory_xsb.h"
#include "deref.h"
#include "heap_xsb.h"
#include "binding.h"
#include "context.h"

#define FUN_PLUS   1
#define FUN_MINUS  2
#define FUN_TIMES  3
#define FUN_DIVIDE 4
#define FUN_AND    5
#define FUN_OR     6
#define FUN_sin    9
#define FUN_cos   10
#define FUN_tan   11

#define FUN_float 13
#define FUN_floor 14
#define FUN_exp   15
#define FUN_log   16
#define FUN_log10 17
#define FUN_sqrt  18
#define FUN_asin  19
#define FUN_acos  20
#define FUN_atan  21
#define FUN_abs  22
#define FUN_truncate  23
#define FUN_round  24
#define FUN_ceiling  25
#define FUN_max  26
#define FUN_min  27

/* --- returns 1 when succeeds, and returns 0 when there is an error --	*/

#define set_fvalue_from_value  \
    if (isinteger(value)) fvalue = (Float) int_val(value); \
    else if (isfloat(value)) fvalue = float_val(value);   \
    else if (isboxedinteger(value)) fvalue = (Float) boxedint_val(value); \
    else return 0


int  unifunc_call(CTXTdeclc int funcnum, CPtr regaddr)
{
  Cell value;
  Float fvalue; 
  prolog_int ivalue;

  value = cell(regaddr);
  XSB_Deref(value);
  switch (funcnum) {
  case FUN_float:
    set_fvalue_from_value;
    bld_float(regaddr, fvalue);
    break;
  case FUN_floor:
    set_fvalue_from_value;
    ivalue = (prolog_int) floor(fvalue);
    bld_oint(regaddr, ivalue);
    break;
  case FUN_PLUS:
  case FUN_MINUS:
  case FUN_TIMES:
  case FUN_DIVIDE:
  case FUN_AND:
  case FUN_OR:
    return 0;		/* should not come here */
      case FUN_sin:
	  set_fvalue_from_value;
	  bld_float(regaddr, sin(fvalue));
	  break;
      case FUN_cos:
	  set_fvalue_from_value;
	  bld_float(regaddr, cos(fvalue));
	  break;
      case FUN_tan:
	  set_fvalue_from_value;
	  bld_float(regaddr, tan(fvalue));
	  break;
      case FUN_exp:
          set_fvalue_from_value;
          bld_float(regaddr, exp(fvalue));
          break;
      case FUN_log:
          set_fvalue_from_value;
	  bld_float(regaddr, log(fvalue));
	  break;
      case FUN_log10:
          set_fvalue_from_value;
	  bld_float(regaddr, log10(fvalue));
	  break;
      case FUN_sqrt:
          set_fvalue_from_value;
	  bld_float(regaddr, sqrt(fvalue));
	  break;
      case FUN_asin:
          set_fvalue_from_value;
	  bld_float(regaddr, asin(fvalue));
	  break;
  case FUN_acos:
    set_fvalue_from_value;
    bld_float(regaddr, acos(fvalue));
    break;
  case FUN_atan:
    set_fvalue_from_value;
    bld_float(regaddr, atan(fvalue));
    break;
  case FUN_abs:
    if (isinteger(value)) { 
      ivalue = int_val(value);
      if (ivalue > 0) 
	bld_int(regaddr,ivalue);
      else bld_oint(regaddr,-ivalue)
    } 
    else if (isboxedinteger(value)) {
      ivalue = boxedint_val(value);
      if (ivalue > 0) 
	{bld_oint(regaddr,ivalue)}
      else bld_oint(regaddr,-ivalue)
    } 
    else if (isfloat(value)) {
      fvalue = float_val(value);
      if (fvalue > 0) 
	bld_float(regaddr,fvalue);
      else bld_float(regaddr,-fvalue);
    } else return 0;
    break;
  case FUN_truncate:
    if (isinteger(value)) { 
      ivalue = int_val(value);
      bld_int(regaddr,ivalue);
    }
    else if (isboxedinteger(value)) { 
      ivalue = boxedint_val(value);
      bld_oint(regaddr,ivalue);
    }
    else if (isfloat(value)) {
      fvalue = float_val(value);
      if (fvalue > 0) 
	bld_float(regaddr,floor(fvalue));
      else bld_float(regaddr,-floor(-fvalue));
    } else return 0;
    break;
  case FUN_round:
    if (isinteger(value)) { 
      ivalue = int_val(value);
      bld_int(regaddr,ivalue);
    }
    else if (isboxedinteger(value)) { 
      ivalue = boxedint_val(value);
      bld_oint(regaddr,ivalue);
    }
    else if (isfloat(value)) {
      fvalue = float_val(value);
      bld_float(regaddr,floor(fvalue+0.5));
    } else return 0;
    break;
  case FUN_ceiling:
    if (isinteger(value)) { 
      ivalue = int_val(value);
      bld_int(regaddr,ivalue);
    }
    else if (isboxedinteger(value)) { 
      ivalue = boxedint_val(value);
      bld_oint(regaddr,ivalue);
    }
    else if (isfloat(value)) {
      fvalue = float_val(value);
      bld_float(regaddr,-floor(-fvalue));
    } else return 0;
    break;
  default:  return 0;
  }
  return 1;
}
