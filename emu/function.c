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


#include <math.h>

#include "configs/xsb_config.h"
#include "debugs/xsb_debug.h"

#include "auxlry.h"
#include "cell_xsb.h"
#include "register.h"
#include "memory_xsb.h"
#include "deref.h"

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

/* --- returns 1 when succeeds, and returns 0 when there is an error --	*/

int  unifunc_call(int funcnum, CPtr regaddr)
{
  Cell value;
  float fvalue; 

  value = cell(regaddr);
  XSB_Deref(value);
  switch (funcnum) {
      case FUN_float:
	  if (isinteger(value)) fvalue = (float) int_val(value);
	  else if (isfloat(value)) fvalue = float_val(value);
	  else return 0;
	  bld_float(regaddr, fvalue);
	  break;
      case FUN_floor:
	  if (isinteger(value)) fvalue =  (float) int_val(value);
	  else if (isfloat(value)) fvalue = float_val(value);
	  else return 0;
	  bld_int(regaddr, floor(fvalue));
	  break;
      case FUN_PLUS:
      case FUN_MINUS:
      case FUN_TIMES:
      case FUN_DIVIDE:
      case FUN_AND:
      case FUN_OR:
	  return 0;		/* should not come here */
      case FUN_sin:
	  if (isinteger(value)) fvalue =  (float) int_val(value);
	  else if (isfloat(value)) fvalue = float_val(value);
	  else return 0;
	  bld_float(regaddr, sin(fvalue));
	  break;
      case FUN_cos:
	  if (isinteger(value)) fvalue =  (float) int_val(value);
	  else if (isfloat(value)) fvalue = float_val(value);
	  else return 0;
	  bld_float(regaddr, cos(fvalue));
	  break;
      case FUN_tan:
	  if (isinteger(value)) fvalue =  (float) int_val(value);
	  else if (isfloat(value)) fvalue = float_val(value);
	  else return 0;
	  bld_float(regaddr, tan(fvalue));
	  break;
      case FUN_exp:
	  if (isinteger(value)) fvalue =  (float) int_val(value);
	  else if (isfloat(value)) fvalue = float_val(value);
	  else return 0;
	  bld_float(regaddr, exp(fvalue));
	  break;
      case FUN_log:
	  if (isinteger(value)) fvalue =  (float) int_val(value);
	  else if (isfloat(value)) fvalue = float_val(value);
	  else return 0;
	  bld_float(regaddr, log(fvalue));
	  break;
      case FUN_log10:
	  if (isinteger(value)) fvalue =  (float) int_val(value);
	  else if (isfloat(value)) fvalue = float_val(value);
	  else return 0;
	  bld_float(regaddr, log10(fvalue));
	  break;
      case FUN_sqrt:
	  if (isinteger(value)) fvalue =  (float) int_val(value);
	  else if (isfloat(value)) fvalue = float_val(value);
	  else return 0;
	  bld_float(regaddr, sqrt(fvalue));
	  break;
      case FUN_asin:
	  if (isinteger(value)) fvalue =  (float) int_val(value);
	  else if (isfloat(value)) fvalue = float_val(value);
	  else return 0;
	  bld_float(regaddr, asin(fvalue));
	  break;
      case FUN_acos:
	  if (isinteger(value)) fvalue =  (float) int_val(value);
	  else if (isfloat(value)) fvalue = float_val(value);
	  else return 0;
	  bld_float(regaddr, asin(fvalue));
	  break;
      case FUN_atan:
	  if (isinteger(value)) fvalue =  (float) int_val(value);
	  else if (isfloat(value)) fvalue = float_val(value);
	  else return 0;
	  bld_float(regaddr, atan(fvalue));
	  break;
      default:  return 0;
  }
  return 1;
}
