/* File:      celltags.h
** Author(s): David S. Warren, Jiyang Xu, Terrance Swift
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



/* ==== types of cells =================================================*/

#define FREE	0	/* Free variable */
#define REF	0	/* Reference */
#define CS	1	/* Structure */
#define INT     2	/* integer */
#define LIST	3	/* List */
#define REF1	4	/* REF */
#define STRING  5	/* Non-Numeric Constant (Atom) */
#define FLOAT	6	/* Floating point number */
/* Tag 7 is unused */

