/* File:      random_xsb.c
** Author(s): Baoqiu Cui
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

/*
 * This is the implementation of Wichmann-Hill Random Number Generator:
 *
 *     B. A. Wichmann and I. D. Hill.  Algorithm AS 183: An efficient and
 *     portable pseudo-random number generator.  Applied Statistics 31
 *     (1982), 188--190.
 *
 * See also:
 *
 *     Correction to Algorithm AS 183.  Applied Statistics 33 (1984) 123,
 *
 *     A. I. McLeod.  A remark on Algorithm AS 183.  Applied Statistics 34
 *     (1985), 198--200.
 */

#include "configs/xsb_config.h"
#include "cell_xsb.h"
#include "cinterf.h"
#include "deref.h"
#include "register.h"
#include "ptoc_tag_xsb_i.h"

static short IX = 6293;
static short IY = 21877;
static short IZ = 7943;

static double TX = 1.0/30269.0;
static double TY = 1.0/30307.0;
static double TZ = 1.0/30323.0;

/*
 * Returns a float number within the range [0.0, 1.0) in reg 2.
 * ret_random() returns -1 if there is an error, 0 if everything is OK.
 */
int ret_random() {
  Float X, Y, Z, T;
  Cell term;

  X = (IX*171) % 30269;
  Y = (IY*172) % 30307;
  Z = (IZ*170) % 30323;
  T = X*TX + Y*TY + Z*TZ;
  IX = X;
  IY = Y;
  IZ = Z;
  
  term = ptoc_tag(2);
  if (isref(term)) {
    ctop_float(2, T - (int)T);
    return 0;
  }
  else return -1;
}    

/*
 * Unifies reg 2,3,4 with the random seeds IX, IY, IZ.  
 * getrand() returns -1 if there is an error, 0 if everything is OK.
 */
int getrand() {
  Cell term;

  term = ptoc_tag(2);
  if (isref(term))
    ctop_int(2, IX);
  else if (!isinteger(term) || (int_val(term) != IX))
    return -1;

  term = ptoc_tag(3);
  if (isref(term))
    ctop_int(3, IY);
  else if (!isinteger(term) || (int_val(term) != IY))
    return -1;

  term = ptoc_tag(4);
  if (isref(term))
    ctop_int(4, IZ);
  else if (!isinteger(term) || (int_val(term) != IZ))
    return -1;

  return 0;
}

/*
 * Sets the random seeds IX, IY and IZ using reg 2,3,4.  The type and
 * range checking are done in random.P.
 */
void setrand() {
  IX = ptoc_int(2);
  IY = ptoc_int(3);
  IZ = ptoc_int(4);
}
