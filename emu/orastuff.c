/* File:      orastuff.c
** Author(s): The XSB Group
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


/* Result Sets Interface */
#ifndef SQL_CRSR
#  define SQL_CRSR
  struct sql_cursor
  {
    unsigned int curocn;
    void *ptr1;
    void *ptr2;
    unsigned long magic;
  };
  typedef struct sql_cursor sql_cursor;
  typedef struct sql_cursor SQL_CURSOR;
#endif /* SQL_CRSR */

/* Thread Safety */
typedef void * sql_context;
typedef void * SQL_CONTEXT;

/* File name & Package Name */
struct sqlcxp
{
  unsigned short fillen;
           char  filnam[12];
};
static struct sqlcxp sqlfpn =
{
    11,
    "orastuff.pc"
};


static unsigned long sqlctx = 160331;


static struct sqlexd {
   unsigned int   sqlvsn;
   unsigned int   arrsiz;
   unsigned int   iters;
   unsigned int   offset;
   unsigned short selerr;
   unsigned short sqlety;
   unsigned int   unused;
            short *cud;
   unsigned char  *sqlest;
            char  *stmt;
   unsigned char  **sqphsv;
   unsigned int   *sqphsl;
            short **sqpind;
   unsigned int   *sqparm;
   unsigned int   **sqparc;
   unsigned char  *sqhstv[3];
   unsigned int   sqhstl[3];
            short *sqindv[3];
   unsigned int   sqharm[3];
   unsigned int   *sqharc[3];
} sqlstm = {8,3};

/* Prototypes */
extern sqlcxt (/*_ void **, unsigned long *,
                   struct sqlexd *, struct sqlcxp * _*/);
extern sqlcx2t(/*_ void **, unsigned long *,
                   struct sqlexd *, struct sqlcxp * _*/);
extern sqlbuft(/*_ void **, char * _*/);
extern sqlgs2t(/*_ void **, char * _*/);
extern sqlorat(/*_ void **, unsigned long *, void * _*/);

/* Forms Interface */
static int IAPSUCC = 0;
static int IAPFAIL = 1403;
static int IAPFTL  = 535;
extern sqliem(/*_ char *, int * _*/);

typedef struct { unsigned short len; unsigned char arr[1]; } VARCHAR;
typedef struct { unsigned short len; unsigned char arr[1]; } varchar;

/* cud (compilation unit data) array */
static short sqlcud0[] =
{8,4130,
2,0,0,1,0,0,273,10,0,1,1,0,1,0,1,97,0,0,
20,0,0,2,0,0,273,12,0,1,1,0,1,0,1,97,0,0,
38,0,0,3,0,0,273,14,0,1,1,0,1,0,1,97,0,0,
56,0,0,4,0,0,273,16,0,1,1,0,1,0,1,97,0,0,
74,0,0,5,0,0,273,18,0,1,1,0,1,0,1,97,0,0,
92,0,0,6,0,0,273,20,0,1,1,0,1,0,1,97,0,0,
110,0,0,7,0,0,273,22,0,1,1,0,1,0,1,97,0,0,
128,0,0,8,0,0,273,24,0,1,1,0,1,0,1,97,0,0,
146,0,0,9,0,0,273,26,0,1,1,0,1,0,1,97,0,0,
164,0,0,10,0,0,273,28,0,1,1,0,1,0,1,97,0,0,
182,0,0,11,0,0,273,30,0,1,1,0,1,0,1,97,0,0,
200,0,0,12,0,0,273,32,0,1,1,0,1,0,1,97,0,0,
218,0,0,13,0,0,273,34,0,1,1,0,1,0,1,97,0,0,
236,0,0,14,0,0,273,36,0,1,1,0,1,0,1,97,0,0,
254,0,0,15,0,0,273,38,0,1,1,0,1,0,1,97,0,0,
272,0,0,16,0,0,273,40,0,1,1,0,1,0,1,97,0,0,
290,0,0,17,0,0,273,42,0,1,1,0,1,0,1,97,0,0,
308,0,0,18,0,0,273,44,0,1,1,0,1,0,1,97,0,0,
326,0,0,19,0,0,273,46,0,1,1,0,1,0,1,97,0,0,
344,0,0,20,0,0,273,48,0,1,1,0,1,0,1,97,0,0,
362,0,0,21,0,0,273,50,0,1,1,0,1,0,1,97,0,0,
380,0,0,1,0,0,275,11,0,1,0,0,1,0,2,32,0,0,
398,0,0,2,0,0,275,12,0,1,0,0,1,0,2,32,0,0,
416,0,0,3,0,0,275,13,0,1,0,0,1,0,2,32,0,0,
434,0,0,4,0,0,275,14,0,1,0,0,1,0,2,32,0,0,
452,0,0,5,0,0,275,15,0,1,0,0,1,0,2,32,0,0,
470,0,0,6,0,0,275,16,0,1,0,0,1,0,2,32,0,0,
488,0,0,7,0,0,275,17,0,1,0,0,1,0,2,32,0,0,
506,0,0,8,0,0,275,18,0,1,0,0,1,0,2,32,0,0,
524,0,0,9,0,0,275,19,0,1,0,0,1,0,2,32,0,0,
542,0,0,10,0,0,275,20,0,1,0,0,1,0,2,32,0,0,
560,0,0,11,0,0,275,21,0,1,0,0,1,0,2,32,0,0,
578,0,0,12,0,0,275,22,0,1,0,0,1,0,2,32,0,0,
596,0,0,13,0,0,275,23,0,1,0,0,1,0,2,32,0,0,
614,0,0,14,0,0,275,24,0,1,0,0,1,0,2,32,0,0,
632,0,0,15,0,0,275,25,0,1,0,0,1,0,2,32,0,0,
650,0,0,16,0,0,275,26,0,1,0,0,1,0,2,32,0,0,
668,0,0,17,0,0,275,27,0,1,0,0,1,0,2,32,0,0,
686,0,0,18,0,0,275,28,0,1,0,0,1,0,2,32,0,0,
704,0,0,19,0,0,275,29,0,1,0,0,1,0,2,32,0,0,
722,0,0,20,0,0,275,30,0,1,0,0,1,0,2,32,0,0,
740,0,0,21,0,0,275,31,0,1,0,0,1,0,2,32,0,0,
758,0,0,1,0,0,276,10,0,1,0,0,1,0,2,32,0,0,
776,0,0,2,0,0,276,11,0,1,0,0,1,0,2,32,0,0,
794,0,0,3,0,0,276,12,0,1,0,0,1,0,2,32,0,0,
812,0,0,4,0,0,276,13,0,1,0,0,1,0,2,32,0,0,
830,0,0,5,0,0,276,14,0,1,0,0,1,0,2,32,0,0,
848,0,0,6,0,0,276,15,0,1,0,0,1,0,2,32,0,0,
866,0,0,7,0,0,276,16,0,1,0,0,1,0,2,32,0,0,
884,0,0,8,0,0,276,17,0,1,0,0,1,0,2,32,0,0,
902,0,0,9,0,0,276,18,0,1,0,0,1,0,2,32,0,0,
920,0,0,10,0,0,276,19,0,1,0,0,1,0,2,32,0,0,
938,0,0,11,0,0,276,20,0,1,0,0,1,0,2,32,0,0,
956,0,0,12,0,0,276,21,0,1,0,0,1,0,2,32,0,0,
974,0,0,13,0,0,276,22,0,1,0,0,1,0,2,32,0,0,
992,0,0,14,0,0,276,23,0,1,0,0,1,0,2,32,0,0,
1010,0,0,15,0,0,276,24,0,1,0,0,1,0,2,32,0,0,
1028,0,0,16,0,0,276,25,0,1,0,0,1,0,2,32,0,0,
1046,0,0,17,0,0,276,26,0,1,0,0,1,0,2,32,0,0,
1064,0,0,18,0,0,276,27,0,1,0,0,1,0,2,32,0,0,
1082,0,0,19,0,0,276,28,0,1,0,0,1,0,2,32,0,0,
1100,0,0,20,0,0,276,29,0,1,0,0,1,0,2,32,0,0,
1118,0,0,21,0,0,276,30,0,1,0,0,1,0,2,32,0,0,
1136,0,0,1,0,0,267,12,0,1,1,0,1,0,1,32,0,0,
1154,0,0,2,0,0,267,15,0,1,1,0,1,0,1,32,0,0,
1172,0,0,3,0,0,267,18,0,1,1,0,1,0,1,32,0,0,
1190,0,0,4,0,0,267,21,0,1,1,0,1,0,1,32,0,0,
1208,0,0,5,0,0,267,24,0,1,1,0,1,0,1,32,0,0,
1226,0,0,6,0,0,267,27,0,1,1,0,1,0,1,32,0,0,
1244,0,0,7,0,0,267,30,0,1,1,0,1,0,1,32,0,0,
1262,0,0,8,0,0,267,33,0,1,1,0,1,0,1,32,0,0,
1280,0,0,9,0,0,267,36,0,1,1,0,1,0,1,32,0,0,
1298,0,0,10,0,0,267,39,0,1,1,0,1,0,1,32,0,0,
1316,0,0,11,0,0,267,42,0,1,1,0,1,0,1,32,0,0,
1334,0,0,12,0,0,267,45,0,1,1,0,1,0,1,32,0,0,
1352,0,0,13,0,0,267,48,0,1,1,0,1,0,1,32,0,0,
1370,0,0,14,0,0,267,51,0,1,1,0,1,0,1,32,0,0,
1388,0,0,15,0,0,267,54,0,1,1,0,1,0,1,32,0,0,
1406,0,0,16,0,0,267,57,0,1,1,0,1,0,1,32,0,0,
1424,0,0,17,0,0,267,60,0,1,1,0,1,0,1,32,0,0,
1442,0,0,18,0,0,267,63,0,1,1,0,1,0,1,32,0,0,
1460,0,0,19,0,0,267,66,0,1,1,0,1,0,1,32,0,0,
1478,0,0,20,0,0,267,69,0,1,1,0,1,0,1,32,0,0,
1496,0,0,21,0,0,267,72,0,1,1,0,1,0,1,32,0,0,
1514,0,0,1,0,0,270,3,0,1,0,0,1,0,2,32,0,0,
1532,0,0,2,0,0,270,5,0,1,0,0,1,0,2,32,0,0,
1550,0,0,3,0,0,270,7,0,1,0,0,1,0,2,32,0,0,
1568,0,0,4,0,0,270,9,0,1,0,0,1,0,2,32,0,0,
1586,0,0,5,0,0,270,11,0,1,0,0,1,0,2,32,0,0,
1604,0,0,6,0,0,270,13,0,1,0,0,1,0,2,32,0,0,
1622,0,0,7,0,0,270,15,0,1,0,0,1,0,2,32,0,0,
1640,0,0,8,0,0,270,17,0,1,0,0,1,0,2,32,0,0,
1658,0,0,9,0,0,270,19,0,1,0,0,1,0,2,32,0,0,
1676,0,0,10,0,0,270,21,0,1,0,0,1,0,2,32,0,0,
1694,0,0,11,0,0,270,23,0,1,0,0,1,0,2,32,0,0,
1712,0,0,12,0,0,270,25,0,1,0,0,1,0,2,32,0,0,
1730,0,0,13,0,0,270,27,0,1,0,0,1,0,2,32,0,0,
1748,0,0,14,0,0,270,29,0,1,0,0,1,0,2,32,0,0,
1766,0,0,15,0,0,270,31,0,1,0,0,1,0,2,32,0,0,
1784,0,0,16,0,0,270,33,0,1,0,0,1,0,2,32,0,0,
1802,0,0,17,0,0,270,35,0,1,0,0,1,0,2,32,0,0,
1820,0,0,18,0,0,270,37,0,1,0,0,1,0,2,32,0,0,
1838,0,0,19,0,0,270,39,0,1,0,0,1,0,2,32,0,0,
1856,0,0,20,0,0,270,41,0,1,0,0,1,0,2,32,0,0,
1874,0,0,21,0,0,270,43,0,1,0,0,1,0,2,32,0,0,
1892,0,0,1,0,0,271,9,0,0,0,0,1,0,
1906,0,0,2,0,0,271,10,0,0,0,0,1,0,
1920,0,0,3,0,0,271,11,0,0,0,0,1,0,
1934,0,0,4,0,0,271,12,0,0,0,0,1,0,
1948,0,0,5,0,0,271,13,0,0,0,0,1,0,
1962,0,0,6,0,0,271,14,0,0,0,0,1,0,
1976,0,0,7,0,0,271,15,0,0,0,0,1,0,
1990,0,0,8,0,0,271,16,0,0,0,0,1,0,
2004,0,0,9,0,0,271,17,0,0,0,0,1,0,
2018,0,0,10,0,0,271,18,0,0,0,0,1,0,
2032,0,0,11,0,0,271,19,0,0,0,0,1,0,
2046,0,0,12,0,0,271,20,0,0,0,0,1,0,
2060,0,0,13,0,0,271,21,0,0,0,0,1,0,
2074,0,0,14,0,0,271,22,0,0,0,0,1,0,
2088,0,0,15,0,0,271,23,0,0,0,0,1,0,
2102,0,0,16,0,0,271,24,0,0,0,0,1,0,
2116,0,0,17,0,0,271,25,0,0,0,0,1,0,
2130,0,0,18,0,0,271,26,0,0,0,0,1,0,
2144,0,0,19,0,0,271,27,0,0,0,0,1,0,
2158,0,0,20,0,0,271,28,0,0,0,0,1,0,
2172,0,0,21,0,0,271,29,0,0,0,0,1,0,
2186,0,0,22,0,0,283,1321,0,3,3,0,1,0,1,9,0,0,1,9,0,0,1,10,0,0,
2212,0,0,23,0,0,280,1629,0,1,1,0,1,0,1,97,0,0,
2230,0,0,24,0,0,286,1934,0,0,0,0,1,0,
};


/************************************************************************/
/*									*/
/* XSB System								*/
/* Copyright (C) SUNY at Stony Brook, 1993				*/
/*									*/
/* Everyone is granted permission to copy, modify and redistribute XSB, */
/* but only under the conditions described in the XSB Licence Agreement.*/
/* A copy of this licence is supposed to have been given to you along   */
/* with XSB so you can know your rights and responsibilities.		*/
/* It should be in a file named LICENCE.				*/
/* Among other things, this notice must be preserved on all copies.	*/
/*									*/
/************************************************************************/

/*======================================================================
  File			:  orastuff.pc
  Author(s)		:  Ernie Johnson
  Last modification	:  August 25, 1998
========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#include "config.h"
#include "cell.h"
#include "flags.h"

/* EXEC SQL INCLUDE "orastuff.h";
 */ 
/************************************************************************/
/*									*/
/* XSB System								*/
/* Copyright (C) SUNY at Stony Brook, 1993				*/
/*									*/
/* Everyone is granted permission to copy, modify and redistribute XSB, */
/* but only under the conditions described in the XSB Licence Agreement.*/
/* A copy of this licence is supposed to have been given to you along   */
/* with XSB so you can know your rights and responsibilities.		*/
/* It should be in a file named LICENCE.				*/
/* Among other things, this notice must be preserved on all copies.	*/
/*									*/
/************************************************************************/

/*======================================================================
  File			:  orastuff.h
  Author(s)		:  Ernie Johnson
  Last modification	:  August 25, 1998
========================================================================*/



/*
 *                   M I S C E L L A N E O U S
 *                   =========================
 */

/* SQL Communications Area
   ----------------------- */
 
#define SQLCA_INIT
/* EXEC SQL INCLUDE sqlca;
 */ 
/*
 * $Header$ sqlca.h 
 */

/* Copyright (c) 1985,1986 by Oracle Corporation. */
 
/*
NAME
  SQLCA : SQL Communications Area.
FUNCTION
  Contains no code. Oracle fills in the SQLCA with status info
  during the execution of a SQL stmt.
NOTES
  **************************************************************
  ***                                                        ***
  *** This file is SOSD.  Porters must change the data types ***
  *** appropriately on their platform.  See notes/pcport.doc ***
  *** for more information.                                  ***
  ***                                                        ***
  **************************************************************

  If the symbol SQLCA_STORAGE_CLASS is defined, then the SQLCA
  will be defined to have this storage class. For example:
 
    #define SQLCA_STORAGE_CLASS extern
 
  will define the SQLCA as an extern.
 
  If the symbol SQLCA_INIT is defined, then the SQLCA will be
  statically initialized. Although this is not necessary in order
  to use the SQLCA, it is a good pgming practice not to have
  unitialized variables. However, some C compilers/OS's don't
  allow automatic variables to be init'd in this manner. Therefore,
  if you are INCLUDE'ing the SQLCA in a place where it would be
  an automatic AND your C compiler/OS doesn't allow this style
  of initialization, then SQLCA_INIT should be left undefined --
  all others can define SQLCA_INIT if they wish.

  If the symbol SQLCA_NONE is defined, then the SQLCA variable will
  not be defined at all.  The symbol SQLCA_NONE should not be defined
  in source modules that have embedded SQL.  However, source modules
  that have no embedded SQL, but need to manipulate a sqlca struct
  passed in as a parameter, can set the SQLCA_NONE symbol to avoid
  creation of an extraneous sqlca variable.
 
MODIFIED
    jbasu      12/12/94 -  Bug 217878: note this is an SOSD file
    losborne   08/11/92 -  No sqlca var if SQLCA_NONE macro set 
  Clare      12/06/84 - Ch SQLCA to not be an extern.
  Clare      10/21/85 - Add initialization.
  Bradbury   01/05/86 - Only initialize when SQLCA_INIT set
  Clare      06/12/86 - Add SQLCA_STORAGE_CLASS option.
*/
 
#ifndef SQLCA
#define SQLCA 1
 
struct   sqlca
         {
         /* ub1 */ char    sqlcaid[8];
         /* b4  */ long    sqlabc;
         /* b4  */ long    sqlcode;
         struct
           {
           /* ub2 */ unsigned short sqlerrml;
           /* ub1 */ char           sqlerrmc[70];
           } sqlerrm;
         /* ub1 */ char    sqlerrp[8];
         /* b4  */ long    sqlerrd[6];
         /* ub1 */ char    sqlwarn[8];
         /* ub1 */ char    sqlext[8];
         };

#ifndef SQLCA_NONE 
#ifdef   SQLCA_STORAGE_CLASS
SQLCA_STORAGE_CLASS struct sqlca sqlca
#else
         struct sqlca sqlca
#endif
 
#ifdef  SQLCA_INIT
         = {
         {'S', 'Q', 'L', 'C', 'A', ' ', ' ', ' '},
         sizeof(struct sqlca),
         0,
         { 0, {0}},
         {'N', 'O', 'T', ' ', 'S', 'E', 'T', ' '},
         {0, 0, 0, 0, 0, 0},
         {0, 0, 0, 0, 0, 0, 0, 0},
         {0, 0, 0, 0, 0, 0, 0, 0}
         }
#endif
         ;
#endif
 
#endif
 
/* end SQLCA */


/* ------------------------------------------------------------------------ */

/*
 *  Number of cursors we can handle at once.
 */
#define NUM_CURSORS   21


/*
 *  Statuses returned to Prolog from the Oracle Interace Primitives.
 */
#define ORACLE_EXCEPTION     2
#define INTERFACE_EXCEPTION  1
#define SUCCESS              0
#define INTERFACE_ERROR     -1
#define ORACLE_ERROR        -2

#define IsExceptionStatus(Status)   (Status > SUCCESS)
#define IsSuccessStatus(Status)     (Status == SUCCESS)
#define IsFailureStatus(Status)     (Status < SUCCESS)


/*
 *  Boolean data type and its values.
 */
typedef enum Boolean {
  FALSE = 0, TRUE = 1,
  NO = 0, YES = 1
} bool;


#define IsNULL(Ptr)      (Ptr == NULL)
#define IsNonNULL(Ptr)   (Ptr != NULL)


/* ======================================================================== */

/*
 *              X S B   F U N C T I O N   P R O T O T Y P E S
 *		=============================================
 */

/* from builtin.c */
int ptoc_int(int regnum);
char *ptoc_string(int regnum);

void ctop_int(int regnum, int value);
void ctop_float(int regnum, float value);
void ctop_string(int regnum, char *value);

/* from psc.c */
char* string_find(char *, int);

/* from xsberror.c */
void xsb_abort(char *);


/* ======================================================================== */

/*
 *                   T A L K I N G   T O   O R A C L E
 *                   =================================
 */

/*
 *  Oracle Descriptors
 *  ==================
 *  The use of DB cursors center around the use of so-called descriptors.
 *  These are data structures defined by Oracle which allow certain
 *  information to be conveyed, between the program and the DB, about the
 *  query.  A descriptor can be dedicated to either input or output.  Input
 *  descriptors are only necessary when using SQL query templates: an SQL
 *  statement with placeholders where values must be inserted.  The use of
 *  templates allows for re-use of DB parsing info, but it requires the
 *  overhead of descriptor manipulation.  SQL statements which are passed
 *  with values intact as a string to Oracle can be executed immediately.
 *  All SQL SELECT statements require a descriptor to handle DB output.
 */


/* SQL Descriptor Area (SQLDA)
   --------------------------- */

/* EXEC SQL INCLUDE sqlda;
 */ 
/*
 * $Header$ sqlda.h 
 */

/***************************************************************
*      The SQLDA descriptor definition			       *
*--------------------------------------------------------------*
*      VAX/3B Version					       *
*                                                              *
*  Copyright (c) 1987 by Oracle Corporation                    *
***************************************************************/


/* NOTES
  **************************************************************
  ***                                                        ***
  *** This file is SOSD.  Porters must change the data types ***
  *** appropriately on their platform.  See notes/pcport.doc ***
  *** for more information.                                  ***
  ***                                                        ***
  **************************************************************
*/

/*  MODIFIED
    jbasu      01/29/95 -  correct typo
    jbasu      01/27/95 -  correct comment - ub2->sb2
    jbasu      12/12/94 - Bug 217878: note this is an SOSD file
    Morse      12/01/87 - undef L and S for v6 include files
    Richey     07/13/87 - change int defs to long 
    Clare      09/13/84 - Port: Ch types to match SQLLIB structs
    Clare      10/02/86 - Add ifndef SQLDA
*/

#ifndef SQLDA_
#define SQLDA_ 1
 
#ifdef T
# undef T
#endif
#ifdef F
# undef F
#endif

#ifdef S
# undef S
#endif
#ifdef L
# undef L
#endif
 
struct SQLDA {
  /* ub4    */ long	  N; /* Descriptor size in number of entries        */
  /* text** */ char	**V; /* Ptr to Arr of addresses of main variables   */
  /* ub4*   */ long	 *L; /* Ptr to Arr of lengths of buffers	    */
  /* sb2*   */ short     *T; /* Ptr to Arr of types of buffers	            */
  /* sb2**  */ short    **I; /* Ptr to Arr of addresses of indicator vars   */
  /* sb4    */ long	  F; /* Number of variables found by DESCRIBE       */
  /* text** */ char	**S; /* Ptr to Arr of variable name pointers	    */
  /* ub2*   */ short     *M; /* Ptr to Arr of max lengths of var. names     */
  /* ub2*   */ short     *C; /* Ptr to Arr of current lengths of var. names */
  /* text** */ char	**X; /* Ptr to Arr of ind. var. name pointers       */
  /* ub2*   */ short     *Y; /* Ptr to Arr of max lengths of ind. var. names */
  /* ub2*   */ short     *Z; /* Ptr to Arr of cur lengths of ind. var. names */
  };
 
typedef struct SQLDA SQLDA;
 
#endif



/* SQLDA Access Macros
   ------------------- */

#define SQLDA_NumEntriesAlloced(SQLDA)             ((SQLDA)->N)
#define SQLDA_ItemValueArrayBase(SQLDA,Index)      ((SQLDA)->V[Index])
#define SQLDA_ItemValueArrayWidth(SQLDA,Index)     ((SQLDA)->L[Index])
#define SQLDA_ItemValueArrayType(SQLDA,Index)      ((SQLDA)->T[Index])
#define SQLDA_IndValueArrayBase(SQLDA,Index)       ((SQLDA)->I[Index])
#define SQLDA_NumEntriesFound(SQLDA)               ((SQLDA)->F)
#define SQLDA_ItemNameBuffer(SQLDA,Index)          ((SQLDA)->S[Index])
#define SQLDA_ItemNameBufLen(SQLDA,Index)          ((SQLDA)->M[Index])
#define SQLDA_ItemNameLength(SQLDA,Index)          ((SQLDA)->C[Index])
#define SQLDA_IndNameBuffer(SQLDA,Index)           ((SQLDA)->X[Index])
#define SQLDA_IndNameBufLen(SQLDA,Index)           ((SQLDA)->Y[Index])
#define SQLDA_IndNameLength(SQLDA,Index)           ((SQLDA)->Z[Index])


/* SQLDA Methods        (taken from sqlcpr.h)
   ------------- */

/* Allocates SQL Descriptor Area */
extern SQLDA *sqlald( int, unsigned int, unsigned int );

/* Deallcates SQL Descriptor Area */
extern void sqlclu( SQLDA * );

/* Checks and resets the high order bit of an SQLDA type field after a
   DESCRIBE of the SLIs  This bit indicates whether a column has been
   specified as NON NULL: 1 = allows NULLs, 0 = disallows NULLs. */
extern void sqlnul( short*, short*, int* );

/* Extracts precision (number of significant digits) and scale (where
   rounding occurs) from the length field of an SQLDA associated with a
   NUMBER datatype. */
extern void sqlprc( long*, int*, int* );


/* Descriptor Constants
   -------------------- */

/*
 * Maximum number of input placeholders or output columns allowed.
 * (There's a real limit set by Oracle; find this constant.)
 */

#define MAX_NUM_IOVALUES  100


/*
 * Limits on the maximum length of the *names* of the select-list
 * items and bind variables.
 * Bind variables are called "bindX" where X is a counter.
 * SLI names depend on the table's column names, or the computation
 * to be performed.
 */

#define BINDVAR_NAME_BUFFER_SIZE  8
#define SLI_NAME_BUFFER_SIZE     30


/*
 * ORACLE recognizes two classes of datatypes: internal and external.
 * Internal datatypes specify how ORACLE (a) stores data in database
 * columns, and (b) represents database pseudocolumns.  External datatypes
 * specify how data is stored in host variables.  External datatypes
 * include the internal datatypes plus other language-specific types.
 * Below, a description of all the Internal and the relevant External
 * datatypes are given.
 *
 * A description containing the term "binary" means that the data would be
 * received in some non-native-language format, either because it is a
 * representation used internally by Oracle (as in the case of NUMBER and
 * DATE) or because the data has a structure imposed by some external
 * influence.  ORACLE, therefore, does not know the format of this data
 * and hence does not attempt to interpret it, as in the case of the RAW
 * and LONG RAW datatypes.
 *
 * A description containing the term "fixed-length" means that every byte
 * of the input value or column field is significant.  In particular, a
 * column value of this type contains blank padding when extracted, even
 * if the host variable is declared to be of a "variable length" type.
 */

enum OracleDataTypes {

  /* Internal (and External) DataTypes */

  VARCHAR2_ODT = 1,         /* Variable-length character string */
  NUMBER_ODT = 2,           /* Binary Number */
  LONG_ODT = 8,             /* Fixed-length character string */
  ROWID_ODT = 11,           /* Binary value */
  DATE_ODT = 12,            /* Fixed-length date/time value */
  RAW_ODT = 23,             /* Fixed-length binary data */
  LONGRAW_ODT = 24,         /* Fixed-length binary data */
  CHAR_ODT = 96,            /* Fixed-length character string */

  /* External DataTypes (cont'd) */

  INTEGER_ODT = 3,          /* Signed Integer */
  FLOAT_ODT = 4,            /* Floating point number */
  STRING_ODT = 5,           /* NULL-terminated character string */
  VARNUM_ODT = 6,           /* Variable-length binary number */
  VARCHAR_ODT = 9,          /* Variable-length character string */
  VARRAW_ODT = 15,          /* Variable-length binary data */
  UNSIGNED_ODT = 68,        /* Unsigned integer */
  LONGVARCHAR_ODT = 94,     /* Variable-length character string */
  LONGVARRAW_ODT = 95,      /* Variable-length binary data */
  CHARZ_ODT = 97            /* C fixed-length, NULL-terminated char string */
};

/* Some data types have a fixed or limited representation as strings... */
#define ROWID_TO_STRING_BUFSIZE    20
#define DATE_TO_STRING_BUFSIZE     10

/* Others give no hint as to its size, so we set our own limit */
#define LONG_TO_STRING_BUFSIZE    100

/* ------------------------------------------------------------------------ */

/*
 *  Local Extensions for Host Variable Arrays
 *  =========================================
 *  Constants, Types, and Data Structures for managing Oracle descriptors
 *  which allow for the use of multi-tuple input and output.  The size of
 *  these arrays are obtained from XSB's global flags.
 *  (In the literature, input placeholders are referred to as "bind
 *   variables" and elements of an output tuple are referred to as
 *   "select list items".)
 */

typedef unsigned int  uint;


typedef struct Bind_Variable_Specification {
  SQLDA *descriptor;       /* Oracle Bind Descriptor */
  uint array_length;       /* number of elements in the input arrays */
  uint cur_entry_index;    /* which element of the BV arrays are to be filled;
			      valid range: [0..(array_length-1)] */
} BindVarSpec;


/*
 *  Original details about a table's columns, as reported by
 *  DESCRIBE SELECT LIST.
 */
typedef struct Table_Column_Attributes {
  short datatype;    /* after the null-bit has been cleared */
  short not_null;    /* TRUE = NULLs not allowed; FALSE = NULLs allowed */
  long size;         /* width of column, may be 0 if none given at creation */
} ColumnSpec;

typedef struct Select_List_Item_Specification {
  SQLDA *descriptor;       /* Oracle Select Descriptor */
  uint array_length;       /* number of elements in the output arrays */
  ColumnSpec *column_specs;   /* array of details about the selected columns */
  uint cur_row_index;      /* indicates the element across all arrays to be
			      read; valid range: [0..(array_length-1)] */
  uint cur_col_index;      /* denotes which SLI should next be read by Prolog;
			      valid range: [0..(numSLIs-1)] */
  int cur_row_number;      /* RowID of next tuple to return to Prolog */
  uint total_rows_recvd;   /* total num rows received from Oracle so far */
  bool end_of_active_set;  /* flag indicating whether the last of the active
			      set has been returned from Oracle. */
} SLI_Spec;  


/*
 *  Default array widths in bytes for select list items (output) and bind
 *  variables (input).  Actual values can be set by the user and are
 *  stored in flags ORA_INPUTARRAY_LENGTH and ORA_OUTPUTARRAY_LENGTH.
 *  Whenever the values of these flags are <= 0, the defaults are used.
 */
#define DEFAULT_INPUTARRAY_LENGTH    200
#define DEFAULT_INPUTARRAY_WIDTH     30
#define DEFAULT_OUTPUTARRAY_LENGTH   200


/* ======================================================================== */

/*
 *                S T A T E M E N T   P R O P E R T I E S
 *                =======================================
 *
 *
 *  Reusing a DB cursor can greatly reduce the time needed to process a
 *  statement.  An SQL statement can be considered to have a particular
 *  "template": syntactically valid SQL constructs together with
 *  placeholders for input and ouput values.  XSB determines and maintains
 *  templates for SQL statements established through many Oracle-interface
 *  predicates, and in doing so, assigns to each distinct template a unique
 *  number.  This number allows us to reuse internal and database
 *  structures created while processing a query with the same template.
 *
 *  Together with this number, the text string representing the statement
 *  itself and a tag indicating the type of statement are all maintained
 *  for an issued Oracle query.
 */


typedef unsigned int TemplateNumber;


/* SQL Statement Type
 *  ------------------
 *  We enumerate only classes of statments, where each one requires unique
 *  handling.  (Explicit numbering for coordination with Prolog.)
 */
typedef enum SQL_Stmt_Type_Classes {
  SELECT_SQL_STMT = 0,      /* Uses output arrays */
  INSERT_SQL_STMT = 1,      /* Uses input arrays */
  CURSOR_DAMAGING_SQL_STMT = 2,   /* Transaction Control and Data Def'n SQL
			    stmts (which trigger COMMITs) invalidate cursors */
  OTHER_SQL_STMT = 3        /* Other Data Manipulation, and Session and System
			   Control SQL stmts which are benign... we think */
} SqlStmtType;


typedef struct SQL_Statement_Specification {
  TemplateNumber template;
  SqlStmtType type;
  char *string;
} SqlStmtSpec;


/* ======================================================================== */

/*
 *                    C U R S O R   C O M P O N E N T S
 *                    =================================
 *
 *
 *  The following data structure tracks the allocation of Oracle cursors, as
 *  well as local structures, to submitted SQL statements.  Handles for
 *  these resources, represented as indices into this array, are maintained
 *  for the last NUM_CURSORS active statements.
 *
 *  In Oracle, cursors are destroyed when a scheme-altering operation is
 *  executed.  Therefore, locally we must break ties between the templates
 *  and the now defunct cursors (subsequent statement calls will have to
 *  rebuild these structures).  We do so by invalidating all active cursor
 *  handles and resetting all inactive cursor ones to the unused state.  We
 *  must also establish tests to ensure that operations are performed only
 *  on valid cursors.
 */


typedef int CursorHandle;

typedef enum Cursor_Status {
  UNUSED_CURSOR_STATUS,       /* Has no allocated internal structures */
  ACTIVE_CURSOR_STATUS,       /* Currently in use */
  INACTIVE_CURSOR_STATUS,     /* Not in use; maintains internal structures */
  INVALID_CURSOR_STATUS       /* ACTIVE cursor destroyed by execution of a
			         Transaction Control SQL statement */
} CursorStatus;

#define INVALID_CURSOR_HANDLE -1


typedef struct Cursor_Components {
  CursorStatus status;
  SqlStmtSpec stmt;
  BindVarSpec bv;
  SLI_Spec sli;
} Cursor;





/* Pro*C Precompiler Options
   ------------------------- */

/* EXEC ORACLE OPTION (MAXOPENCURSORS = 200); */ 

/* EXEC ORACLE OPTION (HOLD_CURSOR = YES); */ 

/* EXEC ORACLE OPTION (RELEASE_CURSOR = NO); */ 




/* Local Cursors
   ------------- */

static Cursor cursors[NUM_CURSORS];


/* ======================================================================== */

/*
 *               Debugging and Error/Warning Reporting
 *               =====================================
 */


/* Debugging Messages - to enable, remove leading '_'
   ------------------ */
#define _VIEW_SQL_AREAS
#define _VIEW_BVs
#define _VIEW_SLIs
#define _VIEW_FETCH
#define _VIEW_GET


/* Error/Warning Messages
   ---------------------- */

static void oracle_error(char *formatStr, ... ) {

  va_list args;

  va_start(args, formatStr);
  fprintf(stderr,"\nError in Oracle Interface:  DB operation\n");
  vfprintf(stderr, formatStr, args);
  fprintf(stderr,"\n\n%.70s\n\n",sqlca.sqlerrm.sqlerrmc);
  va_end(args);

  /* Oracle already rolls back the work of the failed statement. */
}  


static void interface_error(char *formatStr, ... ) {

  va_list args;

  va_start(args, formatStr);
  fprintf(stderr,"\nError in Oracle Interface\n");
  vfprintf(stderr, formatStr, args);
  fprintf(stderr,"\n\n");
  va_end(args);
}


static void interface_warning(char *formatStr, ... ) {

  va_list args;

  va_start(args, formatStr);
  fprintf(stderr,"\nOracle Interface Warning\n");
  vfprintf(stderr, formatStr, args);
  fprintf(stderr,"\n\n");
  va_end(args);
}

/* ------------------------------------------------------------------------ */

/* Diagnostic Routines
   ------------------- */

#ifdef VIEW_SQL_AREAS

static void print_sqlca() {
  /*
   *  The SQLCA should be initialized to avoid erroneous output.
   */

  int i;

  printf("SQLCA structure:\n");
  printf("\tsqlcaid: %-.8s\n", sqlca.sqlcaid);
  printf("\tsqlabc:  %ld\n", sqlca.sqlabc);
  printf("\tsqlcode: %ld\n", sqlca.sqlcode);
  printf("\tsqlerrp: %-.8s\n", sqlca.sqlerrp);
  printf("\tsqlerrd:");
  for (i = 0; i < 6; i++)
    printf(" %ld", sqlca.sqlerrd[i]);
  printf("\n");
  printf("\tsqlwarn:");
  for (i = 0; i < 8; i++)
    printf(" %d", sqlca.sqlwarn[i]);
  printf("\n");
  printf("\tsqlext: %-.8s\n", sqlca.sqlext);
}


static void print_sqlda(SQLDA *d, int items, int item_len, int ind_len) {

  int i;

  printf("Descriptor allocation:  sqlald(%d, %d, %d)\n",
	 items, item_len, ind_len);
  printf("\tN: %li\n", d->N);
  
  printf("\tV: %p", d->V);
  if (d->V != NULL) {
    printf("\tCell values: ");
    for (i = 0; i < items; i++)
      printf("  %p", d->V[i]);
  }
  printf("\n");
  
  printf("\tL: %p\n", d->L);
  
  printf("\tT: %p\n", d->T);
  
  printf("\tI: %p", d->I);
  if (d->I != NULL) {
    printf("\tCell values: ");
    for (i = 0; i < items; i++)
      printf("  %p", d->I[i]);
  }
  printf("\n");
  
  printf("\tF: %li\n", d->F);
  
  printf("\tS: %p", d->S);
  if (d->S != NULL) {
    printf("\tCell values: ");
    for (i = 0; i < items; i++)
      printf("  %p", d->S[i]);
  }
  printf("\n");
  
  printf("\tM: %p\t", d->M);
  if (d->M != NULL) {
    short ok = 1;
    for (i = 0; i < items; i++)
      if (d->M[i] != item_len)
	ok = 0;
    if (ok == 0)
      printf("NOT ");
    printf("All entries set to %d\n", item_len);
  }
  
  printf("\tC: %p\n", d->C);
  
  printf("\tX: %p", d->X);
  if (d->X != NULL) {
    printf("\tCell values: ");
    for (i = 0; i < items; i++)
      printf("  %p", d->X[i]);
  }
  printf("\n");
  
  printf("\tY: %p\t", d->Y);
  if (d->Y != NULL) {
    short ok = 1;
    for (i = 0; i < items; i++)
      if (d->Y[i] != ind_len)
	ok = 0;
    if (ok == 0)
      printf("NOT ");
    printf("All entries set to %d\n", ind_len);
  }
  
  printf("\tZ: %p\n", d->Z);
}

#endif


/* ======================================================================== */

/*
 *                            U T I L I T I E S
 *                            =================
 */

/*
 *  Capitalize the first few letters of 'wordStr', storing the sequence in
 *  'capStr' as a string, whose total size is 'capStrLen'.
 */

static void capitalize_word(char wordStr[], char capStr[], int capStrLen) {

  int i;

  for (i = 0; (i < capStrLen - 1) && isalpha((int)wordStr[i]); i++)
    capStr[i] = (char)toupper((int)wordStr[i]);
  capStr[i] = '\0';
}


/*
 *  Remove trailing blanks from a sequence of characters of length 'strlen'
 *  by placing a null after the last non-space character.
 */

static void remove_blank_padding(char *string, int strlen) {

  char *tail;

  tail = string + strlen - 1;
  while ( (*tail == ' ') && (tail >= string) )
    tail--;
  *(tail + 1) = '\0';
}


/*
 *  Determine the type of SQL statement and return a code indicating this
 *  type.  See p.2-2 of the "Programmer's Guide to the Oracle
 *  Precompilers".  Transaction Control statements may adversely effect
 *  cursors: see Chapter 6: Defining and Controlling Transactions.
 */

static SqlStmtType statement_type(char stmtString[]) {

  char stmtTypeString[10];


  capitalize_word(stmtString, stmtTypeString, 10);

  /* Statements needing output arrays */

  if (strcmp("SELECT",stmtTypeString) == 0)
    return SELECT_SQL_STMT;

  /* Statements needing input arrays */

  else if (strcmp("INSERT",stmtTypeString) == 0)
    return INSERT_SQL_STMT;

  /* Other Data Manipulation statements */

  else if (strcmp("DELETE",stmtTypeString) == 0)
    return OTHER_SQL_STMT;
  else if (strcmp("EXPLAIN",stmtTypeString) == 0)
    return OTHER_SQL_STMT;
  else if (strcmp("LOCK",stmtTypeString) == 0)
    return OTHER_SQL_STMT;
  else if (strcmp("UPDATE",stmtTypeString) == 0)
    return OTHER_SQL_STMT;

  /* Session and System Control statements... benign? */

  else if (strcmp("ALTER",stmtTypeString) == 0) {
    capitalize_word(stmtString + 6,stmtTypeString,10);
    if ( (strcmp("SESSION",stmtTypeString) == 0) ||
	 (strcmp("SYSTEM",stmtTypeString) == 0) )
      return OTHER_SQL_STMT;
    else
      return CURSOR_DAMAGING_SQL_STMT;
  }
  else if (strcmp("SET",stmtTypeString) == 0) {
    capitalize_word(stmtString + 4,stmtTypeString,10);
    if (strcmp("ROLE",stmtTypeString) == 0)
      return OTHER_SQL_STMT;
    else
      return CURSOR_DAMAGING_SQL_STMT;
  }

  /* The rest are either Transaction Control SQL statements or they
     trigger a TCS.  Either way, cursors may be (partially) damaged. */

  else
    return CURSOR_DAMAGING_SQL_STMT;
}


/*
 * The following functions take an internal code and return a short
 * description of what the code represents.
 */

static char *cursor_status_as_string(CursorStatus status) {

  switch (status) {
  case UNUSED_CURSOR_STATUS:    return ("UNUSED"); break;
  case ACTIVE_CURSOR_STATUS:    return ("ACTIVE"); break;
  case INACTIVE_CURSOR_STATUS:  return ("INACTIVE"); break;
  case INVALID_CURSOR_STATUS:   return ("INVALID"); break;
  default:                      return ("UNKNOWN ???"); break;
  }
}


static char *oracle_datatype_as_string(int datatype) {

  switch(datatype) {
  case VARCHAR2_ODT:      return("VARCHAR2"); break;
  case NUMBER_ODT:        return("NUMBER"); break;
  case INTEGER_ODT:       return("INTEGER"); break;
  case FLOAT_ODT:         return("FLOAT"); break;
  case STRING_ODT:        return("STRING"); break;
  case VARNUM_ODT:        return("VARNUM"); break;
  case LONG_ODT:          return("LONG"); break;
  case VARCHAR_ODT:       return("VARCHAR"); break;
  case ROWID_ODT:         return("ROWID"); break;
  case DATE_ODT:          return("DATE"); break;
  case VARRAW_ODT:        return("VARRAW"); break;
  case RAW_ODT:           return("RAW"); break;
  case LONGRAW_ODT:       return("LONG RAW"); break;
  case UNSIGNED_ODT:      return("UNSIGNED"); break;
  case LONGVARCHAR_ODT:   return("LONG VARCHAR"); break;
  case LONGVARRAW_ODT:    return("LONG VARRAW"); break;
  case CHAR_ODT:          return("CHAR"); break;
  case CHARZ_ODT:         return("CHARZ"); break;
  default:                return ("UNKNOWN ???"); break;
  }
}


static int validateArrayDimension(int arrayType) {

  if ( (int)flags[arrayType] <= 0 )
    switch ( arrayType ) {
    case ORA_INPUTARRAY_LENGTH:
      interface_warning("Invalid input array length (%d) -> "
			"Using default value (%d)", (int)flags[arrayType],
			DEFAULT_INPUTARRAY_LENGTH);
      flags[arrayType] = DEFAULT_INPUTARRAY_LENGTH;
      break;
    case ORA_INPUTARRAY_WIDTH:
      interface_warning("Invalid input array width (%d) -> "
			"Using default value (%d)", (int)flags[arrayType],
			DEFAULT_INPUTARRAY_WIDTH);
      flags[arrayType] = DEFAULT_INPUTARRAY_WIDTH;
      break;
    case ORA_OUTPUTARRAY_LENGTH:
      interface_warning("Invalid output array length (%d) -> "
			"Using default value (%d)", (int)flags[arrayType],
			DEFAULT_OUTPUTARRAY_LENGTH);
      flags[arrayType] = DEFAULT_OUTPUTARRAY_LENGTH;
      break;
    default:
      break;
    }
  return( (int)flags[arrayType] );
}


/* ======================================================================== */

/*
 *            O R A C L E   C U R S O R   P R O C E S S I N G
 *            ===============================================
 */

/*
 *  PREPAREs an SQL statement and DECLAREs a cursor for it.  Locally,
 *  symbolic names for SQL statements and cursors are created and
 *  associated with one another.  At Oracle, the statement is parsed and a
 *  cursor created (or at least *some* memory is reserved to store the
 *  parsing info).
 *
 *  Oracle note: PREPAREing requires that the query string be a host
 *  variable.
 */

static int oracleCursor_prepare_and_declare(CursorHandle cursorHandle,
					    char *stmtString) {

  /* EXEC SQL BEGIN DECLARE SECTION; */ 

    char *sql_stmt;
  /* EXEC SQL END DECLARE SECTION; */ 


  sql_stmt = stmtString;

  /* EXEC SQL WHENEVER SQLERROR GOTO error_handler; */ 

/*   EXEC SQL INCLUDE orastuff/prepdecl.i;
 */ 
/*
 * PREPARE and DECLARE a Cursor for a SQL Statement
 * ------------------------------------------------
 *  PREPARE parses the stmt and associates a symbolic name with it.
 *  DECLARE defines a symbolic name for a cursor and associates it with
 *  the specified SQL statement.
 */

  switch (cursorHandle) {
  case  0:  /* EXEC SQL PREPARE S0 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )2;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C0 CURSOR FOR S0; */ 
  break;
  case  1:  /* EXEC SQL PREPARE S1 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )20;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C1 CURSOR FOR S1; */ 
  break;
  case  2:  /* EXEC SQL PREPARE S2 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )38;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C2 CURSOR FOR S2; */ 
  break;
  case  3:  /* EXEC SQL PREPARE S3 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )56;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C3 CURSOR FOR S3; */ 
  break;
  case  4:  /* EXEC SQL PREPARE S4 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )74;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C4 CURSOR FOR S4; */ 
  break;
  case  5:  /* EXEC SQL PREPARE S5 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )92;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C5 CURSOR FOR S5; */ 
  break;
  case  6:  /* EXEC SQL PREPARE S6 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )110;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C6 CURSOR FOR S6; */ 
  break;
  case  7:  /* EXEC SQL PREPARE S7 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )128;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C7 CURSOR FOR S7; */ 
  break;
  case  8:  /* EXEC SQL PREPARE S8 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )146;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C8 CURSOR FOR S8; */ 
  break;
  case  9:  /* EXEC SQL PREPARE S9 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )164;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C9 CURSOR FOR S9; */ 
  break;
  case 10:  /* EXEC SQL PREPARE S10 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )182;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C10 CURSOR FOR S10; */ 
  break;
  case 11:  /* EXEC SQL PREPARE S11 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )200;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C11 CURSOR FOR S11; */ 
  break;
  case 12:  /* EXEC SQL PREPARE S12 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )218;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C12 CURSOR FOR S12; */ 
  break;
  case 13:  /* EXEC SQL PREPARE S13 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )236;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C13 CURSOR FOR S13; */ 
  break;
  case 14:  /* EXEC SQL PREPARE S14 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )254;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C14 CURSOR FOR S14; */ 
  break;
  case 15:  /* EXEC SQL PREPARE S15 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )272;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C15 CURSOR FOR S15; */ 
  break;
  case 16:  /* EXEC SQL PREPARE S16 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )290;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C16 CURSOR FOR S16; */ 
  break;
  case 17:  /* EXEC SQL PREPARE S17 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )308;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C17 CURSOR FOR S17; */ 
  break;
  case 18:  /* EXEC SQL PREPARE S18 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )326;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C18 CURSOR FOR S18; */ 
  break;
  case 19:  /* EXEC SQL PREPARE S19 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )344;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C19 CURSOR FOR S19; */ 
  break;
  case 20:  /* EXEC SQL PREPARE S20 FROM :sql_stmt; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.stmt = "";
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )362;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)sql_stmt;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}


             /* EXEC SQL DECLARE C20 CURSOR FOR S20; */ 
  break;
  }


  return SUCCESS;

 error_handler:
  oracle_error("Parse error, cursor %d, at character offset %li in SQL stateme"
	       "nt\n\n\t%s", cursorHandle, sqlca.sqlerrd[4] + 1, stmtString);
  return ORACLE_ERROR;
}

/* ------------------------------------------------------------------------ */

/*
 * Allocate and DESCRIBE a Bind Variable Descriptor.
 * ------------------------------------------------
 *  To allocate a descriptor, use the function sqlald():
 *    sqlald( MAX_NUM_BVorSLI, MAX_BVorSLI_NAMELEN, MAX_INDICATOR_NAMELEN );
 *  No indicator names are maintained, so always call with third arg 0.
 *
 *  An SQL DESCRIBE of the bind variables performs the following:
 *  1) sets the number of BVs found in F;
 *  2) enters names of placeholders and the length of each name;
 *  3) sets the types of the input values to 0.
 *
 *  Oracle note: the descriptor should NOT be a host variable when
 *  DESCRIBEing.
 *
 *  F is supposed to be negative if there are more BVs than room was
 *  allocated for.  However, this isn't the case (big surprise), so an
 *  explicit comparison between what was expected and what was found is
 *  needed.  Should the original space be inadequate, the descriptor is
 *  reallocated with enough room and the bind variables are reDESCRIBEd.
 *  The descriptor is then set in the cursor structure.
 */

static int oracleCursor_describe_BVs(CursorHandle cursorHandle, int numBVs) {

  SQLDA *descriptor;
  bool successful_describe;


  do {
    successful_describe = TRUE;
    descriptor = sqlald(numBVs,BINDVAR_NAME_BUFFER_SIZE,0);
    if ( IsNULL(descriptor) ) {
      interface_error("Cannot allocate Bind Descriptor!");
      return INTERFACE_ERROR;
    }

    /* EXEC SQL WHENEVER SQLERROR GOTO error_handler; */ 

/*     EXEC SQL INCLUDE orastuff/desc_bv.i;
 */ 
/*
 * DESCRIBE the SQL Statement's Bind Variables
 * -------------------------------------------
 *  Initilizes the bind descriptor after examining the placeholders in
 *  the PREPAREd dynamic SQL statement to determine the name(s) and
 *  length(s) of the placeholders, as well as the data type of the
 *  associated input host variable(s).
 */

  switch (cursorHandle) {
  case 0:   /* EXEC SQL DESCRIBE BIND VARIABLES FOR S0 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )380;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 1:   /* EXEC SQL DESCRIBE BIND VARIABLES FOR S1 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )398;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 2:   /* EXEC SQL DESCRIBE BIND VARIABLES FOR S2 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )416;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 3:   /* EXEC SQL DESCRIBE BIND VARIABLES FOR S3 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )434;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 4:   /* EXEC SQL DESCRIBE BIND VARIABLES FOR S4 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )452;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 5:   /* EXEC SQL DESCRIBE BIND VARIABLES FOR S5 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )470;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 6:   /* EXEC SQL DESCRIBE BIND VARIABLES FOR S6 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )488;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 7:   /* EXEC SQL DESCRIBE BIND VARIABLES FOR S7 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )506;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 8:   /* EXEC SQL DESCRIBE BIND VARIABLES FOR S8 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )524;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 9:   /* EXEC SQL DESCRIBE BIND VARIABLES FOR S9 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )542;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 10:  /* EXEC SQL DESCRIBE BIND VARIABLES FOR S10 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )560;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 11:  /* EXEC SQL DESCRIBE BIND VARIABLES FOR S11 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )578;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 12:  /* EXEC SQL DESCRIBE BIND VARIABLES FOR S12 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )596;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 13:  /* EXEC SQL DESCRIBE BIND VARIABLES FOR S13 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )614;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 14:  /* EXEC SQL DESCRIBE BIND VARIABLES FOR S14 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )632;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 15:  /* EXEC SQL DESCRIBE BIND VARIABLES FOR S15 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )650;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 16:  /* EXEC SQL DESCRIBE BIND VARIABLES FOR S16 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )668;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 17:  /* EXEC SQL DESCRIBE BIND VARIABLES FOR S17 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )686;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 18:  /* EXEC SQL DESCRIBE BIND VARIABLES FOR S18 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )704;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 19:  /* EXEC SQL DESCRIBE BIND VARIABLES FOR S19 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )722;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  case 20:  /* EXEC SQL DESCRIBE BIND VARIABLES FOR S20 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )740;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

   break;
  }


    if (SQLDA_NumEntriesFound(descriptor) < 0) {
      #ifdef VIEW_BVs
        printf("Number of BVs found is negative: %ld\n",
	       SQLDA_NumEntriesFound(descriptor));
      #endif
      numBVs = -SQLDA_NumEntriesFound(descriptor);
      successful_describe = FALSE;
      sqlclu(descriptor);
    }
    else if (SQLDA_NumEntriesFound(descriptor) > numBVs) {
      #ifdef VIEW_BVs
        printf("Number of BVs found is larger (%ld) than allocated (%d)\n",
	       SQLDA_NumEntriesFound(descriptor), numBVs);
      #endif
      numBVs = SQLDA_NumEntriesFound(descriptor);
      successful_describe = FALSE;
      sqlclu(descriptor);
    }
  } while (successful_describe == FALSE);

  SQLDA_NumEntriesAlloced(descriptor) = SQLDA_NumEntriesFound(descriptor);
  cursors[cursorHandle].bv.descriptor = descriptor;
  return SUCCESS;

 error_handler:
  oracle_error("DESCRIBE of Bind Variables failed for cursor %d",
	       cursorHandle);
  return ORACLE_ERROR;
}

/* ------------------------------------------------------------------------ */

/*
 * Allocate and DESCRIBE a Select List Item Descriptor.
 * ---------------------------------------------------
 *  An SQL DESCRIBE of the select list items performs the following:
 *  1) sets the number of SLIs found in F;
 *  2) enters names of SLIs and the length of each name;
 *  3) sets the types of the SLIs, as defined in the database.
 *
 *  Oracle note: the descriptor should NOT be a host variable when
 *  DESCRIBEing.
 *
 *  If more SLIs exist than room was allocated for, then the number found
 *  has a negative sign.  Should the original space be inadequate, the
 *  descriptor is reallocated with enough room and the select list items
 *  are reDESCRIBEd.  The descriptor is then set in the cursor structure.
 */

static int oracleCursor_describe_SLIs(CursorHandle cursorHandle, int numSLIs) {

  SQLDA *descriptor;
  bool successful_describe;


  do {
    successful_describe = TRUE;
    descriptor = sqlald(numSLIs,SLI_NAME_BUFFER_SIZE,0);
    if ( IsNULL(descriptor) ) {
      interface_error("Cannot allocate Select Descriptor!");
      return INTERFACE_ERROR;
    }

    /* EXEC SQL WHENEVER SQLERROR GOTO error_handler; */ 

/*     EXEC SQL INCLUDE orastuff/desc_sli.i;
 */ 
/*
 * DESCRIBE the SQL Statement's Select List Items
 * ----------------------------------------------
 *  Initilizes the select list descriptor after examining the select list
 *  items in the PREPAREd dynamic SQL statement to determine each item's
 *  name, datatype, constraints, length, scale, and precision.
 */

  switch (cursorHandle) {
  case 0:   /* EXEC SQL DESCRIBE SELECT LIST FOR S0 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )758;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 1:   /* EXEC SQL DESCRIBE SELECT LIST FOR S1 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )776;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 2:   /* EXEC SQL DESCRIBE SELECT LIST FOR S2 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )794;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 3:   /* EXEC SQL DESCRIBE SELECT LIST FOR S3 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )812;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 4:   /* EXEC SQL DESCRIBE SELECT LIST FOR S4 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )830;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 5:   /* EXEC SQL DESCRIBE SELECT LIST FOR S5 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )848;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 6:   /* EXEC SQL DESCRIBE SELECT LIST FOR S6 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )866;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 7:   /* EXEC SQL DESCRIBE SELECT LIST FOR S7 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )884;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 8:   /* EXEC SQL DESCRIBE SELECT LIST FOR S8 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )902;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 9:   /* EXEC SQL DESCRIBE SELECT LIST FOR S9 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )920;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 10:  /* EXEC SQL DESCRIBE SELECT LIST FOR S10 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )938;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 11:  /* EXEC SQL DESCRIBE SELECT LIST FOR S11 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )956;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 12:  /* EXEC SQL DESCRIBE SELECT LIST FOR S12 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )974;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 13:  /* EXEC SQL DESCRIBE SELECT LIST FOR S13 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )992;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 14:  /* EXEC SQL DESCRIBE SELECT LIST FOR S14 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )1010;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 15:  /* EXEC SQL DESCRIBE SELECT LIST FOR S15 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )1028;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 16:  /* EXEC SQL DESCRIBE SELECT LIST FOR S16 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )1046;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 17:  /* EXEC SQL DESCRIBE SELECT LIST FOR S17 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )1064;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 18:  /* EXEC SQL DESCRIBE SELECT LIST FOR S18 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )1082;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 19:  /* EXEC SQL DESCRIBE SELECT LIST FOR S19 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )1100;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  case 20:  /* EXEC SQL DESCRIBE SELECT LIST FOR S20 INTO descriptor; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 8;
            sqlstm.arrsiz = 1;
            sqlstm.iters = (unsigned int  )1;
            sqlstm.offset = (unsigned int  )1118;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (unsigned char  *)descriptor;
            sqlstm.sqhstl[0] = (unsigned int  )0;
            sqlstm.sqindv[0] = (         short *)0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqphsv = sqlstm.sqhstv;
            sqlstm.sqphsl = sqlstm.sqhstl;
            sqlstm.sqpind = sqlstm.sqindv;
            sqlstm.sqparm = sqlstm.sqharm;
            sqlstm.sqparc = sqlstm.sqharc;
            sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
            if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
  }


    if (SQLDA_NumEntriesFound(descriptor) < 0) {
      #ifdef VIEW_SLIs
        printf("Number of SLIs found is negative: %ld\n",
	       SQLDA_NumEntriesFound(descriptor));
      #endif
      numSLIs = -SQLDA_NumEntriesFound(descriptor);
      successful_describe = FALSE;
      sqlclu(descriptor);
    }
    if (SQLDA_NumEntriesFound(descriptor) > numSLIs) {
      #ifdef VIEW_SLIs
        printf("Number of SLIs found (%ld) is larger than allocated (%d)\n",
	       SQLDA_NumEntriesFound(descriptor), numSLIs);
      #endif
      numSLIs = SQLDA_NumEntriesFound(descriptor);
      successful_describe = FALSE;
      sqlclu(descriptor);
    }
  } while (successful_describe == FALSE);

  SQLDA_NumEntriesAlloced(descriptor) = SQLDA_NumEntriesFound(descriptor);
  cursors[cursorHandle].sli.descriptor = descriptor;
  return SUCCESS;

 error_handler:
  oracle_error("DESCRIBE of Select List Items failed for cursor %d",
	       cursorHandle);
  return ORACLE_ERROR;
}

/* ------------------------------------------------------------------------ */

/*
 * OPEN the Cursor to Execute the Statement
 * ----------------------------------------
 *  When a cursor is OPENed, the statement is executed using the current
 *  values of the input host variables.  These values usually form just a
 *  single tuple.  However, arrayed inputs are also supported, with the
 *  number of valid tuples that may take part identified by the subfield
 *  'cur_entry_index'.  If the statement is a SELECT, the active set is
 *  identified at this time.  If the OPEN is successful, this field is
 *  reset to 0, which readies it to accept another batch of inputs.
 *
 *  Oracle notes:
 *    Oracle complains if there isn't at least one valid tuple during an
 *      OPEN.
 *    OPENing a cursor may acquire additional resources (above those
 *      acquired during the PREPARE) at the database.
 */

static int oracleCursor_execute(CursorHandle cursorHandle) {

  SQLDA *bindDesc;
  /* EXEC SQL BEGIN DECLARE SECTION; */ 

    int numTuples;
  /* EXEC SQL END DECLARE SECTION; */ 


  numTuples = cursors[cursorHandle].bv.cur_entry_index;
  if ( numTuples > 0 ) {
    bindDesc = cursors[cursorHandle].bv.descriptor;

    /* EXEC SQL WHENEVER SQLERROR GOTO error_handler; */ 

/*     EXEC SQL INCLUDE orastuff/open.i;
 */ 
/*
 * OPEN a Cursor
 * -------------
 *  OPEN allocates an Oracle cursor, binds input values to placeholders,
 *  executes the associated SQL statement, and if this statement is a
 *  query, identifies the active set and positions the cursor on its
 *  first row.  The rows-processed count of the SQLCA is also zeroed-out.
 */

  switch (cursorHandle) {
  case  0:
    /* EXEC SQL FOR :numTuples OPEN C0 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1136;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case  1:
    /* EXEC SQL FOR :numTuples OPEN C1 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1154;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case  2:
    /* EXEC SQL FOR :numTuples OPEN C2 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1172;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case  3:
    /* EXEC SQL FOR :numTuples OPEN C3 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1190;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case  4:
    /* EXEC SQL FOR :numTuples OPEN C4 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1208;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case  5:
    /* EXEC SQL FOR :numTuples OPEN C5 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1226;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case  6:
    /* EXEC SQL FOR :numTuples OPEN C6 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1244;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case  7:
    /* EXEC SQL FOR :numTuples OPEN C7 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1262;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case  8:
    /* EXEC SQL FOR :numTuples OPEN C8 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1280;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case  9:
    /* EXEC SQL FOR :numTuples OPEN C9 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1298;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case 10:
    /* EXEC SQL FOR :numTuples OPEN C10 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1316;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case 11:
    /* EXEC SQL FOR :numTuples OPEN C11 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1334;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case 12:
    /* EXEC SQL FOR :numTuples OPEN C12 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1352;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case 13:
    /* EXEC SQL FOR :numTuples OPEN C13 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1370;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case 14:
    /* EXEC SQL FOR :numTuples OPEN C14 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1388;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case 15:
    /* EXEC SQL FOR :numTuples OPEN C15 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1406;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case 16:
    /* EXEC SQL FOR :numTuples OPEN C16 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1424;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case 17:
    /* EXEC SQL FOR :numTuples OPEN C17 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1442;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case 18:
    /* EXEC SQL FOR :numTuples OPEN C18 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1460;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case 19:
    /* EXEC SQL FOR :numTuples OPEN C19 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1478;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  case 20:
    /* EXEC SQL FOR :numTuples OPEN C20 USING DESCRIPTOR bindDesc; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 8;
    sqlstm.arrsiz = 1;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )numTuples;
    sqlstm.offset = (unsigned int  )1496;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (unsigned char  *)bindDesc;
    sqlstm.sqhstl[0] = (unsigned int  )0;
    sqlstm.sqindv[0] = (         short *)0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto error_handler;
}


    break;
  }

  }
  cursors[cursorHandle].bv.cur_entry_index = 0;
  return SUCCESS;

 error_handler:
  oracle_error("OPEN of cursor %d failed", cursorHandle);
  return ORACLE_ERROR;
}

/* ------------------------------------------------------------------------ */

/*
 *  FETCH Output Values from the Active Set
 *
 *  Batched (or arrayed) FETCHes are supported for bringing several rows
 *  from the database on a single request.  The Select Descriptor must
 *  already be primed for receiving this data: buffers allocated and
 *  desired format of output specified as an External Datatype.  When the
 *  last of the rows have been returned, a flag is set in the cursor
 *  substructure.
 */

static int oracleCursor_fetch(CursorHandle cursorHandle) {

  SQLDA *sliDesc;
  /* EXEC SQL BEGIN DECLARE SECTION; */ 

    uint arrayLength;
  /* EXEC SQL END DECLARE SECTION; */ 



  sliDesc = cursors[cursorHandle].sli.descriptor;
  arrayLength = cursors[cursorHandle].sli.array_length;

  /* EXEC SQL WHENEVER NOT FOUND GOTO nodata_handler; */ 

  /* EXEC SQL WHENEVER SQLERROR GOTO error_handler; */ 

/*   EXEC SQL INCLUDE orastuff/fetch.i;
 */ 
switch (cursorHandle) {
 case 0:
  /* EXEC SQL FOR :arrayLength FETCH C0 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1514;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 1:
  /* EXEC SQL FOR :arrayLength FETCH C1 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1532;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 2:
  /* EXEC SQL FOR :arrayLength FETCH C2 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1550;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 3:
  /* EXEC SQL FOR :arrayLength FETCH C3 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1568;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 4:
  /* EXEC SQL FOR :arrayLength FETCH C4 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1586;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 5:
  /* EXEC SQL FOR :arrayLength FETCH C5 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1604;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 6:
  /* EXEC SQL FOR :arrayLength FETCH C6 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1622;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 7:
  /* EXEC SQL FOR :arrayLength FETCH C7 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1640;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 8:
  /* EXEC SQL FOR :arrayLength FETCH C8 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1658;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 9:
  /* EXEC SQL FOR :arrayLength FETCH C9 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1676;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 10:
  /* EXEC SQL FOR :arrayLength FETCH C10 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1694;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 11:
  /* EXEC SQL FOR :arrayLength FETCH C11 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1712;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 12:
  /* EXEC SQL FOR :arrayLength FETCH C12 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1730;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 13:
  /* EXEC SQL FOR :arrayLength FETCH C13 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1748;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 14:
  /* EXEC SQL FOR :arrayLength FETCH C14 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1766;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 15:
  /* EXEC SQL FOR :arrayLength FETCH C15 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1784;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 16:
  /* EXEC SQL FOR :arrayLength FETCH C16 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1802;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 17:
  /* EXEC SQL FOR :arrayLength FETCH C17 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1820;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 18:
  /* EXEC SQL FOR :arrayLength FETCH C18 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1838;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 19:
  /* EXEC SQL FOR :arrayLength FETCH C19 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1856;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
 case 20:
  /* EXEC SQL FOR :arrayLength FETCH C20 USING DESCRIPTOR sliDesc; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 1;
  sqlstm.iters = (unsigned int  )arrayLength;
  sqlstm.offset = (unsigned int  )1874;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)sliDesc;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto nodata_handler;
  if (sqlca.sqlcode < 0) goto error_handler;
}

  break;
}

  /* EXEC SQL WHENEVER NOT FOUND CONTINUE; */ 
   /* turn trap off for rest of file */

  return SUCCESS;


 nodata_handler:
  /*
   *  A "no data found" warning is generated whenever the incremented
   *  cursor becomes positioned after the last row of the active set.
   *  However, since we are using arrays, *some* data may have been
   *  returned.  If the new total returned from Oracle is the same as our
   *  count set from the last retrieval, then no (new) data was returned.
   *  Otherwise, the remaining rows of the active set were returned.
   */

  if (sqlca.sqlerrd[2] == cursors[cursorHandle].sli.total_rows_recvd) {
    #ifdef VIEW_FETCH
      printf("\tNo more rows to retrieve.\n");
    #endif
    return ORACLE_EXCEPTION;
  }
  else {
    cursors[cursorHandle].sli.end_of_active_set = TRUE;
    return SUCCESS;
  }

 error_handler:
  oracle_error("FETCH failed for cursor %d", cursorHandle);
  return ORACLE_ERROR;
}

/* ------------------------------------------------------------------------ */

/*
 *  Close a Database Cursor
 *
 *  The active set becomes undefined and the resources acquired on OPENing
 *  the cursor are freed.  The memory acquired during the PREPARE may not
 *  be released, but the parse lock is.
 */

static int oracleCursor_close(CursorHandle cursorHandle) {

  /* EXEC SQL WHENEVER SQLERROR GOTO close_error; */ 

/*   EXEC SQL INCLUDE orastuff/close.i;
 */ 
/*
 * CLOSE a Cursor
 * --------------
 *  CLOSEing a cursor disables it.  What resources are freed depends on
 *  the settings of the HOLD_CURSOR and RELEASE_CURSOR precompiler options.
 */

switch (cursorHandle) {
 case  0:  /* EXEC SQL CLOSE C0; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )1892;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case  1:  /* EXEC SQL CLOSE C1; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )1906;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case  2:  /* EXEC SQL CLOSE C2; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )1920;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case  3:  /* EXEC SQL CLOSE C3; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )1934;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case  4:  /* EXEC SQL CLOSE C4; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )1948;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case  5:  /* EXEC SQL CLOSE C5; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )1962;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case  6:  /* EXEC SQL CLOSE C6; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )1976;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case  7:  /* EXEC SQL CLOSE C7; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )1990;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case  8:  /* EXEC SQL CLOSE C8; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )2004;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case  9:  /* EXEC SQL CLOSE C9; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )2018;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case 10:  /* EXEC SQL CLOSE C10; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )2032;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case 11:  /* EXEC SQL CLOSE C11; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )2046;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case 12:  /* EXEC SQL CLOSE C12; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )2060;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case 13:  /* EXEC SQL CLOSE C13; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )2074;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case 14:  /* EXEC SQL CLOSE C14; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )2088;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case 15:  /* EXEC SQL CLOSE C15; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )2102;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case 16:  /* EXEC SQL CLOSE C16; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )2116;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case 17:  /* EXEC SQL CLOSE C17; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )2130;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case 18:  /* EXEC SQL CLOSE C18; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )2144;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case 19:  /* EXEC SQL CLOSE C19; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )2158;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
 case 20:  /* EXEC SQL CLOSE C20; */ 

{
           struct sqlexd sqlstm;

           sqlstm.sqlvsn = 8;
           sqlstm.arrsiz = 1;
           sqlstm.iters = (unsigned int  )1;
           sqlstm.offset = (unsigned int  )2172;
           sqlstm.cud = sqlcud0;
           sqlstm.sqlest = (unsigned char  *)&sqlca;
           sqlstm.sqlety = (unsigned short)0;
           sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
           if (sqlca.sqlcode < 0) goto close_error;
}

  break;
}

  return SUCCESS;

 close_error:
  oracle_error("CLOSE failed for cursor %d", cursorHandle);
  return ORACLE_ERROR;
}


/* ======================================================================== */

/*
 *                  C U R S O R   M A I N T E N A N C E
 *                  ===================================
 */


/* Interface Initialization
 * ------------------------
 *  Initialize cursors and institute default settings.
 */

static void initialize_interface() {

  CursorHandle ch;

  for(ch = 0; ch < NUM_CURSORS; ch++) {
    cursors[ch].status = UNUSED_CURSOR_STATUS;
    cursors[ch].bv.descriptor = NULL;
    cursors[ch].sli.descriptor = NULL;
  }
  flags[ORA_INPUTARRAY_LENGTH] = DEFAULT_INPUTARRAY_LENGTH;
  flags[ORA_INPUTARRAY_WIDTH] = DEFAULT_INPUTARRAY_WIDTH;
  flags[ORA_OUTPUTARRAY_LENGTH] = DEFAULT_OUTPUTARRAY_LENGTH;
}

/* ------------------------------------------------------------------------ */

/* Freeing Cursor Resources
 * ------------------------
 *  Set the cursor to the UNUSED state, release any currently held
 *  resources (deallocate local substructures and CLOSE Oracle cursor),
 *  and set the descriptor pointers to NULL.  This amounts to a
 *  reinitialization of the cursor.
 */

static int free_cursor_resources(CursorHandle cursorHandle) {

  int ithBV;
  SQLDA *descriptor;


  cursors[cursorHandle].status = UNUSED_CURSOR_STATUS;

  descriptor = cursors[cursorHandle].bv.descriptor;
  if ( IsNonNULL(descriptor) ) {
    for (ithBV = 0; ithBV < SQLDA_NumEntriesFound(descriptor); ithBV++) {
      free( SQLDA_ItemValueArrayBase(descriptor,ithBV) );
      free( SQLDA_IndValueArrayBase(descriptor,ithBV) );
    }
    sqlclu(descriptor);
    cursors[cursorHandle].bv.descriptor = NULL;
  }

  descriptor = cursors[cursorHandle].sli.descriptor;
  if ( IsNonNULL(descriptor) ) {
    for (ithBV = 0; ithBV < SQLDA_NumEntriesFound(descriptor); ithBV++) {
      free( SQLDA_ItemValueArrayBase(descriptor,ithBV) );
      free( SQLDA_IndValueArrayBase(descriptor,ithBV) );
    }
    sqlclu(descriptor);
    cursors[cursorHandle].sli.descriptor = NULL;
  }
  free(cursors[cursorHandle].sli.column_specs);

  return oracleCursor_close(cursorHandle);
}

/* ------------------------------------------------------------------------ */

/* Interface Cleanup
 * -----------------
 *  CLOSE and deallocate the resources of opened cursors.
 */

static int reinitialize_cursors() {

  CursorHandle ch;
  int freeStatus, finalStatus;


  finalStatus = SUCCESS;
  for (ch = 0; ch < NUM_CURSORS; ch++)
    switch (cursors[ch].status) {
    case ACTIVE_CURSOR_STATUS:
    case INACTIVE_CURSOR_STATUS:
    case INVALID_CURSOR_STATUS:
      /*
       * The following test is good enough since free_cursor_resources()
       * currently only reports errors and not exceptions.
       */
      freeStatus = free_cursor_resources(ch);
      if ( freeStatus < finalStatus )
	finalStatus = freeStatus;
      break;
    default:
      break;
    }
  return finalStatus;
}

/* ------------------------------------------------------------------------ */

/* Invalidation of Cursors
 * -----------------------
 *  To be used when a cursor-destroying statement is executed.  Any open
 *  cursors are invalidated.
 *
 *  Not incorporated into interface yet.  I'm not convinced that this
 *  really occurs.  How cursors are treated is dependent upon several
 *  factors, the most important being the MODE.  I think the MODE is
 *  ORACLE, in which case the cursors aren't closed automatically, but
 *  instead some locks are lost.  And I don't yet understand the
 *  ramifications of this.
 */

static int invalidate_active_cursors() {

  CursorHandle ch;
  int num_invalidated;

  num_invalidated = 0;
  for (ch = 0; ch < NUM_CURSORS; ch ++)
    if (cursors[ch].status == ACTIVE_CURSOR_STATUS) {
      cursors[ch].status = INVALID_CURSOR_STATUS;
      num_invalidated++;
    }
  return num_invalidated;
}

/* ------------------------------------------------------------------------ */

/* Choosing a Cursor
 * -----------------
 *  Locate a suitable cursor for a query, using the following preferences:
 *  1) Re-use a cursor (select INACTIVE cursor with same template number),
 *  2) Choose an unused cursor,
 *  3) Steal an INACTIVE cursor (established for another template),
 *  4) Clobber an INVALID cursor (won't be able to recover it).
 *
 *  A returned cursor always has one of two forms:
 *  1) has no associated resources, in which case its status is UNUSED, or
 *  2) has resources suitable for reuse (consistent with 'templateNumber'),
 *       in which case its status is INACTIVE.
 */

static CursorHandle locate_cursor(TemplateNumber templateNumber) {

  CursorHandle cursorHandle;


  /* Look for Reusable Cursor */

  for (cursorHandle = 0; cursorHandle < NUM_CURSORS; cursorHandle++)
    if ( (cursors[cursorHandle].stmt.template == templateNumber) &&
	 (cursors[cursorHandle].status == INACTIVE_CURSOR_STATUS) )
      return cursorHandle;


  /* Look for Unused Cursor */

  for (cursorHandle = 0; cursorHandle < NUM_CURSORS; cursorHandle++)
    if (cursors[cursorHandle].status == UNUSED_CURSOR_STATUS)
      return cursorHandle;


  /* Steal an INACTIVE Cursor */

  for (cursorHandle = 0; cursorHandle < NUM_CURSORS; cursorHandle++)
    if (cursors[cursorHandle].status == INACTIVE_CURSOR_STATUS) {
      free_cursor_resources(cursorHandle);
      return cursorHandle;
    }

  /* Cannot Find a Suitable Cursor */

  return INVALID_CURSOR_HANDLE;
}

/* ------------------------------------------------------------------------ */

/* Initialize Input Specifications
 * -------------------------------
 *  Initialize information needed for SQL statement input:
 *  1) allocate and initialize (DESCRIBE) a bind variable descriptor
 *  2) allocate buffers for this descriptor and assign input datatypes
 *  3) initialize cursor fields of 'bv' substructure
 *
 *  Return a status code.
 */

static int init_input_specification(CursorHandle cursorHandle,
				    int numBindVars) {

  int ithBV, status;
  int arrayLength, arrayWidth;
  SQLDA *descriptor;


  status = oracleCursor_describe_BVs(cursorHandle,numBindVars);
  if ( IsFailureStatus(status) )
    return status;

  descriptor = cursors[cursorHandle].bv.descriptor;

  /* Initialize C-Structure Fields */

  if ( cursors[cursorHandle].stmt.type == INSERT_SQL_STMT ) {
    cursors[cursorHandle].bv.array_length =
      validateArrayDimension(ORA_INPUTARRAY_LENGTH);
  }
  else
    cursors[cursorHandle].bv.array_length = 1;

  cursors[cursorHandle].bv.cur_entry_index = 0;


  /* Allocate Buffers for Descriptor.  Increase width by 1 to accommodate
     null terminator for STRING_ODT datatype. */

  arrayWidth = validateArrayDimension(ORA_INPUTARRAY_WIDTH) + 1;

  arrayLength = cursors[cursorHandle].bv.array_length;
  for (ithBV = 0; ithBV < SQLDA_NumEntriesFound(descriptor); ithBV++) {
    SQLDA_ItemValueArrayBase(descriptor,ithBV) =
      (char *) malloc( arrayLength * arrayWidth );
    SQLDA_ItemValueArrayWidth(descriptor,ithBV) = arrayWidth;
    SQLDA_ItemValueArrayType(descriptor,ithBV) = STRING_ODT;
    SQLDA_IndValueArrayBase(descriptor,ithBV) =
      (short *) malloc( arrayLength * sizeof(short) );
    if ( IsNULL(SQLDA_ItemValueArrayBase(descriptor,ithBV)) ||
	 IsNULL(SQLDA_IndValueArrayBase(descriptor,ithBV)) ) {
      interface_error("Ran out of memory while allocating buffers for\n"
		      "Bind Descriptor of cursor %d.", cursorHandle);
      return INTERFACE_ERROR;
    }
  }
  #ifdef VIEW_BVs
    printf("Allocated Bind Descriptor with %ld arrays (%d X %d)\n",
	   SQLDA_NumEntriesFound(descriptor), arrayLength, arrayWidth);
  #endif
  return SUCCESS;
}

/* ------------------------------------------------------------------------ */

/* Initialize Output Specifications
 * --------------------------------
 *  Initialize information needed for SQL statement (SELECT) output:
 *  1) allocate and initialize (DESCRIBE) a select list descriptor
 *  2) determine the best form (datatype) in which to accept the output,
 *    and allocate appropriately-sized buffers based on this type
 *  3) initialize the other fields of the SLI specification
 *
 *  Return a status code.
 */

static int init_output_specification(CursorHandle cursorHandle, int numSLIs) {

  int null_status,     /* 1 = allows NULL, 0 = disallows NULL */
      status;
  int ithSLI, arrayLength;
  int precision,       /* max number of digits in NUMBER */
      scale;           /* displacement from decimal pt where rounding occurs */
  SQLDA *descriptor;


  status = oracleCursor_describe_SLIs(cursorHandle,numSLIs);
  if ( IsFailureStatus(status) )
    return status;

  descriptor = cursors[cursorHandle].sli.descriptor;

  /* Initialize C-Structure Fields
     ----------------------------- */
  cursors[cursorHandle].sli.column_specs = (ColumnSpec *)
    malloc(SQLDA_NumEntriesFound(descriptor) * sizeof(ColumnSpec) );
  if ( IsNULL(cursors[cursorHandle].sli.column_specs) ) {
    interface_error("Allocation of internal structure failed");
    return INTERFACE_ERROR;
  }

  /* array_length set during cursor allocation */
  cursors[cursorHandle].sli.cur_row_index = 0;
  cursors[cursorHandle].sli.cur_col_index = 0;
  cursors[cursorHandle].sli.cur_row_number = 0;
  cursors[cursorHandle].sli.total_rows_recvd = 0;
  cursors[cursorHandle].sli.end_of_active_set = FALSE;

  /* Alter datatypes and allocate buffer storage
     ------------------------------------------- */
  arrayLength = cursors[cursorHandle].sli.array_length;
  for (ithSLI = 0; ithSLI < SQLDA_NumEntriesFound(descriptor); ithSLI++) {
    /*
     * Must always turn off high-order bit of datatype.
     */
    sqlnul(&SQLDA_ItemValueArrayType(descriptor,ithSLI), /* original */
	   &SQLDA_ItemValueArrayType(descriptor,ithSLI), /* type w/bit unset */
	   &null_status);  /* bit value */

    cursors[cursorHandle].sli.column_specs[ithSLI].datatype =
      SQLDA_ItemValueArrayType(descriptor,ithSLI);
    cursors[cursorHandle].sli.column_specs[ithSLI].not_null = ! null_status;
    cursors[cursorHandle].sli.column_specs[ithSLI].size =
      SQLDA_ItemValueArrayWidth(descriptor,ithSLI);

    /*
     * When using a STRING or CHARZ external datatype, given a N-byte
     * buffer, the Nth byte is used by ORACLE for the null terminator.
     */
    switch ( SQLDA_ItemValueArrayType(descriptor,ithSLI) ) {
    case VARCHAR2_ODT:
    case CHAR_ODT:
      SQLDA_ItemValueArrayType(descriptor,ithSLI) = STRING_ODT;
      SQLDA_ItemValueArrayWidth(descriptor,ithSLI)++;
      break;

    case NUMBER_ODT:
      SQLDA_ItemValueArrayType(descriptor,ithSLI) = STRING_ODT;
      sqlprc(& SQLDA_ItemValueArrayWidth(descriptor,ithSLI),
	     &precision, &scale);
      if (precision == 0)
	precision = 40;
      if (scale > 0)    /* leave room for sign and decimal point */
	SQLDA_ItemValueArrayWidth(descriptor,ithSLI) = precision + 2;
      else    /* leave room for sign */
	SQLDA_ItemValueArrayWidth(descriptor,ithSLI) = precision + 1;
      SQLDA_ItemValueArrayWidth(descriptor,ithSLI)++;  /* room for null */
      break;

    case LONG_ODT:
      SQLDA_ItemValueArrayType(descriptor,ithSLI) = STRING_ODT;
      SQLDA_ItemValueArrayWidth(descriptor,ithSLI) = LONG_TO_STRING_BUFSIZE;
      break;
      
    case ROWID_ODT:
      SQLDA_ItemValueArrayType(descriptor,ithSLI) = STRING_ODT;
      SQLDA_ItemValueArrayWidth(descriptor,ithSLI) = ROWID_TO_STRING_BUFSIZE;
      break;

    case DATE_ODT:
      /*
       * STRING or VARCHAR2 should coerce DATE into default format
       */
      SQLDA_ItemValueArrayType(descriptor,ithSLI) = STRING_ODT;
      SQLDA_ItemValueArrayWidth(descriptor,ithSLI) = DATE_TO_STRING_BUFSIZE;
      break;

    /*
     * RAW data may be problematic for returning to Prolog since it does
     * not necessarily contain character strings, but any binary data.
     */
    case RAW_ODT:
    case LONGRAW_ODT:
    default:
      {
	short datatype = SQLDA_ItemValueArrayType(descriptor,ithSLI);

	interface_error("Cursor %d, SLI %d: datatype %s not supported by "
			"interface\n", cursorHandle, ithSLI,
			oracle_datatype_as_string(datatype));
	return INTERFACE_ERROR;
      }
      break;
    }

    SQLDA_ItemValueArrayBase(descriptor,ithSLI) = (char *)
      malloc(arrayLength * SQLDA_ItemValueArrayWidth(descriptor,ithSLI));

    SQLDA_IndValueArrayBase(descriptor,ithSLI) =
      (short *) malloc(arrayLength * sizeof(short));

    if ( IsNULL(SQLDA_ItemValueArrayBase(descriptor,ithSLI)) ||
	 IsNULL(SQLDA_IndValueArrayBase(descriptor,ithSLI)) ) {
      interface_error("Allocation of %dth SLI buffer for cursor %d failed!\n",
		   ithSLI, cursorHandle);
      return INTERFACE_ERROR;
    }
  }
  return SUCCESS;
}

/* ------------------------------------------------------------------------ */

/* Suspending a Cursor
 * -------------------
 *  Don't release resources, but mark as having completed the statement.
 */

static int suspend_cursor(CursorHandle cursorHandle) {

  cursors[cursorHandle].status = INACTIVE_CURSOR_STATUS;
  return SUCCESS;
}		

/* ------------------------------------------------------------------------ */

/* Resuming a Cursor
 * -----------------
 *  Initialization needed when reusing a cursor.  Every statement must
 *  have a bind descriptor, but only SELECT statements have select
 *  descriptors.  Perform needed changes on BOTH descriptors: the user may
 *  now have specified different array dimensions from those at the time
 *  of the original allocation.  Redimension the appropriate substructures
 *  of the descriptor(s) as needed.
 */

static int resume_cursor(CursorHandle cursorHandle) {

  int oldLength, newLength, newWidth, newSize;
  int ithItem, status;
  SQLDA *descriptor;


  descriptor = cursors[cursorHandle].bv.descriptor;
  if ( IsNULL(descriptor) ) {
    interface_error("Resuming cursor (%d) with NULL bind descriptor",
		    cursorHandle);
    return INTERFACE_ERROR;
  }
  cursors[cursorHandle].bv.cur_entry_index = 0;

  /* Alter input buffer sizes as directed by user flags... */

  if ( SQLDA_NumEntriesFound(descriptor) > 0 ) {

    if (cursors[cursorHandle].stmt.type == INSERT_SQL_STMT) {
      newLength = validateArrayDimension(ORA_INPUTARRAY_LENGTH);
    }
    else
      newLength = 1;

    /* add 1 for STRING datatype */
    newWidth = validateArrayDimension(ORA_INPUTARRAY_WIDTH) + 1;

    newSize = newLength * newWidth;  

    oldLength = cursors[cursorHandle].bv.array_length;
    if (newLength != oldLength) {
      cursors[cursorHandle].bv.array_length = newLength;
      for (ithItem = 0; ithItem < SQLDA_NumEntriesFound(descriptor);
	   ithItem++) {
	SQLDA_IndValueArrayBase(descriptor,ithItem) =
	  (short *) realloc( SQLDA_IndValueArrayBase(descriptor,ithItem),
			     (newLength * sizeof(short)) );
	if ( IsNULL(SQLDA_IndValueArrayBase(descriptor,ithItem)) ) {
	  interface_error("Ran out of memory while reallocating buffers for\n"
			  "Bind Descriptor of cursor %d.", cursorHandle);
	  return INTERFACE_ERROR;
	}
      }
    }
    for (ithItem = 0; ithItem < SQLDA_NumEntriesFound(descriptor);
	 ithItem++) {
      SQLDA_ItemValueArrayWidth(descriptor,ithItem) = newWidth;
      SQLDA_ItemValueArrayBase(descriptor,ithItem) = (char *)
	realloc( SQLDA_ItemValueArrayBase(descriptor,ithItem), newSize );

      if ( IsNULL(SQLDA_ItemValueArrayBase(descriptor,ithItem)) ) {
	interface_error("Ran out of memory while reallocating buffers for\n"
			"Bind Descriptor of cursor %d.", cursorHandle);
	return INTERFACE_ERROR;
      }
    }
    #ifdef VIEW_BVs
      printf("Reusing Bind Descriptor: %ld BVs, array dims: %d X %d\n",
	     SQLDA_NumEntriesFound(descriptor), newLength, newWidth);
    #endif
  }

  status = SUCCESS;
  if ( cursors[cursorHandle].stmt.type == SELECT_SQL_STMT ) {
    descriptor = cursors[cursorHandle].sli.descriptor;
    if ( IsNULL(descriptor) )
      /*
       * This cursor was allocated but never made it to execution before
       * being deallocated.  Not the norm, but we handle this contingency
       * anyway.
       */
      status = init_output_specification(cursorHandle,1);
    else {
      cursors[cursorHandle].sli.cur_row_index = 0;
      cursors[cursorHandle].sli.cur_row_number = 0;
      cursors[cursorHandle].sli.total_rows_recvd = 0;
      cursors[cursorHandle].sli.end_of_active_set = FALSE;
      
      oldLength = cursors[cursorHandle].sli.array_length;
      
      newLength = validateArrayDimension(ORA_OUTPUTARRAY_LENGTH);
      
      if (newLength != oldLength) {
	cursors[cursorHandle].sli.array_length = newLength;
	for (ithItem = 0; ithItem < SQLDA_NumEntriesFound(descriptor);
	     ithItem++) {
	  newWidth = SQLDA_ItemValueArrayWidth(descriptor,ithItem);
	  SQLDA_ItemValueArrayBase(descriptor,ithItem) =
	    (char *)realloc(SQLDA_ItemValueArrayBase(descriptor,ithItem),
			    (newLength * newWidth));
	  
	  SQLDA_IndValueArrayBase(descriptor,ithItem) =
	    (short *)realloc(SQLDA_IndValueArrayBase(descriptor,ithItem),
			     (newLength * sizeof(short)));

	  if ( (SQLDA_ItemValueArrayBase(descriptor,ithItem) == NULL) ||
	      (SQLDA_IndValueArrayBase(descriptor,ithItem) == NULL) ) {
	    interface_error("Ran out of memory while reallocating buffers for"
			    "\nSelect Descriptor of cursor %d.", cursorHandle);
	    return INTERFACE_ERROR;
	  }
	}
      }
    #ifdef VIEW_SLIs
      printf("Reusing Select Descriptor: %ld SLIs, %d elements per array\n",
	     SQLDA_NumEntriesFound(descriptor), newLength);
    #endif
    }
  }
  return status;
}


/* ======================================================================== */

/*
 *           O R A C L E   I N T E R F A C E   B U I L T I N S
 *           =================================================
 */


/* Argument Validating Macros
 * --------------------------
 *  Actually, we should do an even better job than this.  The ptoc_*
 *  functions of builtin.c do a pitiful job of conveying errors to the
 *  caller.  ptoc_int returns 0 and ptoc_string returns "" when an error
 *  is encountered.  This could have been made to work, except that
 *  ptoc_string aborts the execution while ptoc_int does not.  And
 *  aborting currently isn't the way we want to go.
 *
 *  The alternative is to write routines ourselves. To do this, we'd need
 *  config.h, cell.h, deref.h, and register.h (at least).  But if we want
 *  this stuff bullet-proof, it'll have to be done.  Until then, be VERY
 *  careful when calling these primitives...
 */

static bool isValidArg_CursorHandle(CursorHandle ch, int status_reg) {

  if ( (ch >= 0) && (ch < NUM_CURSORS) ) {
    if (cursors[ch].status == ACTIVE_CURSOR_STATUS)
      return TRUE;
    else
      interface_error("Cursor %d is not currently active (status: %s)", ch,
		      cursor_status_as_string(cursors[ch].status));
  }
  else
    interface_error("Invalid cursor handle: %d", ch);

  ctop_int(status_reg,INTERFACE_ERROR);
  return FALSE;
}


static bool isValidArg_TemplateNumber(TemplateNumber template,
				      int status_reg) {

  if (template >= 0)
    return TRUE;
  else {
    interface_error("Invalid template number: %d", template);
    ctop_int(status_reg,INTERFACE_ERROR);
    return FALSE;
  }
}


/*
 * 'stringDesc' should be a brief description of the string being checked,
 * e.g., "SQL Statement".
 */

static bool isValidArg_String(char *inputString, char stringDesc[],
			      int status_reg) {

  if ( strcmp(inputString,"") != 0 )    /* ptoc_string returns "" on error */
    return TRUE;
  else {
    interface_error("Empty argument string: %s", stringDesc);
    ctop_int(status_reg,INTERFACE_ERROR);
    return FALSE;
  }
}


/*
 * 'countDesc' should be a brief description of the count being checked,
 * e.g., "Bind Variables".
 */

static bool isValidArg_IOCount(int io_count, char countDesc[],
			       int status_reg) {

  if ( (io_count >= 0) && (io_count <= MAX_NUM_IOVALUES) )
    return TRUE;
  else {
    interface_error("Invalid number of %s: %d\n(Valid range: %d - %d)",
		    countDesc, io_count, 0, MAX_NUM_IOVALUES);
    ctop_int(status_reg,INTERFACE_ERROR);
    return FALSE;
  }
}

/* ------------------------------------------------------------------------ */

/* Connecting to Oracle
 * --------------------
 *  Open a connection and initialize the interface's structures.
 */

void oracle_connect() {

  const int regUserName     = 2;   /* in */
  const int regPassword     = 3;   /* in */
  const int regReturnStatus = 4;   /* out */

  /* EXEC SQL BEGIN DECLARE SECTION; */ 

    /* VARCHAR  username[128]; */ 
struct { unsigned short len; unsigned char arr[128]; } username;

    /* VARCHAR  password[32]; */ 
struct { unsigned short len; unsigned char arr[32]; } password;

  /* EXEC SQL END DECLARE SECTION; */ 



  strcpy((char *) username.arr, ptoc_string(regUserName));
  username.len = strlen((char *) username.arr);
  
  strcpy((char *) password.arr, ptoc_string(regPassword));
  password.len = strlen((char *) password.arr);
  
  /* EXEC SQL WHENEVER SQLERROR GOTO connect_error; */ 

  /* EXEC SQL CONNECT :username IDENTIFIED BY :password; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 3;
  sqlstm.iters = (unsigned int  )200;
  sqlstm.offset = (unsigned int  )2186;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)&username;
  sqlstm.sqhstl[0] = (unsigned int  )130;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqhstv[1] = (unsigned char  *)&password;
  sqlstm.sqhstl[1] = (unsigned int  )34;
  sqlstm.sqindv[1] = (         short *)0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto connect_error;
}


  
  initialize_interface();
  
  ctop_int(regReturnStatus,SUCCESS);
  return;

 connect_error:
  oracle_error("Cannot connect to ORACLE as user %s\n", username.arr);
  ctop_int(regReturnStatus,ORACLE_ERROR);
}

/* ------------------------------------------------------------------------ */

/* Establishing a Cursor
 * ---------------------
 *  Allocates and initializes local and remote resources for cursor support.
 *  A successfully returned cursor handle is ready to receive input values.
 *  Because we provide the user control over the size of the buffers, we
 *  note the values of all such parameters at this time.
 */

void allocate_cursor() {

  const int regTemplateNumber = 2;   /* in */
  const int regSqlStmtString  = 3;   /* in */
  const int regNumBindvars    = 4;   /* in */
  const int regSqlStmtType    = 5;   /* out */
  const int regCursorHandle   = 6;   /* out */
  const int regReturnStatus   = 7;   /* out */

  TemplateNumber templateNumber;
  char *stmtString;
  int numBindVars;
  SqlStmtType stmtType;
  CursorHandle cursorHandle;
  int status_flag;


  /* Get and validate arguments */

  templateNumber = ptoc_int(regTemplateNumber);
  if ( ! isValidArg_TemplateNumber(templateNumber,regReturnStatus) )
    return;

  stmtString = ptoc_string(regSqlStmtString);
  if ( ! isValidArg_String(stmtString,"SQL Statement",regReturnStatus) )
    return;

  numBindVars = ptoc_int(regNumBindvars);
  if ( ! isValidArg_IOCount(numBindVars,"Bind Variables",regReturnStatus) )
    return;


  /* Locate a suitable cursor */

  cursorHandle = locate_cursor(templateNumber);
  if (cursorHandle == INVALID_CURSOR_HANDLE) {
    interface_error("Cannot find a suitable Cursor for template %d",
		    templateNumber);
    ctop_int(regReturnStatus,INTERFACE_ERROR);
    return;
  }

  /* Initialize the Cursor */

  if (cursors[cursorHandle].status == UNUSED_CURSOR_STATUS) {
    stmtType = statement_type(stmtString);
    switch ( stmtType ) {
    case SELECT_SQL_STMT:
      cursors[cursorHandle].sli.array_length =
	validateArrayDimension(ORA_OUTPUTARRAY_LENGTH);      
      break;
    case CURSOR_DAMAGING_SQL_STMT:
      interface_warning("Cursor-damaging SQL Statement detected in cursor "
			"processing!\n\n\t%s", stmtString);
      break;
    case OTHER_SQL_STMT:
      interface_warning("Statement not needing a cursor?\n\n\t%s", stmtString);
      break;
    default:
      break;
    }
    cursors[cursorHandle].stmt.type = stmtType;
    cursors[cursorHandle].stmt.string = stmtString;
    cursors[cursorHandle].stmt.template = templateNumber;

    status_flag = oracleCursor_prepare_and_declare(cursorHandle,stmtString);
    if ( IsFailureStatus(status_flag) ) {
      ctop_int(regReturnStatus,status_flag);
      return;
    }
    status_flag = init_input_specification(cursorHandle,numBindVars);
  }
  else    /* Initialize Cursor for Reuse */
    status_flag = resume_cursor(cursorHandle);

  ctop_int(regReturnStatus,status_flag);
  if ( IsSuccessStatus(status_flag) ) {
    cursors[cursorHandle].status = ACTIVE_CURSOR_STATUS;
    ctop_int(regSqlStmtType,cursors[cursorHandle].stmt.type);
    ctop_int(regCursorHandle,cursorHandle);
  }
  else
    free_cursor_resources(cursorHandle);
 return;
}

/* ------------------------------------------------------------------------ */

/* Entering Input Tuple Components
 * -------------------------------
 *  Set a single input value, according to bind variable name.
 */

void set_input_value() {

  const int regCursorHandle = 2;   /* in */
  const int regBindvarName  = 3;   /* in */
  const int regBindvarValue = 4;   /* in */
  const int regBindvarType  = 5;   /* in:  0 = string, 1 = integer */
  const int regReturnStatus = 6;   /* out */

  CursorHandle cursorHandle;
  int bufferWidth, inputStringLen;
  int ithBV, indicator;
  SQLDA *descriptor;
  char *bindvarName, *entry_loc, *inputString, numberString[12], nullString[5];


  cursorHandle = ptoc_int(regCursorHandle);
  if ( ! isValidArg_CursorHandle(cursorHandle,regReturnStatus) )
    return;

  descriptor = cursors[cursorHandle].bv.descriptor;
  if ( SQLDA_NumEntriesFound(descriptor) == 0 ) {
    interface_error("Attempting to set input value for statement without "
		    "bind variables (cursor %d)\n\n\t%s", cursorHandle,
		    cursors[cursorHandle].stmt.string);
    ctop_int(regReturnStatus,INTERFACE_ERROR);
    return;
  }

  bindvarName = ptoc_string(regBindvarName);
  if ( ! isValidArg_String(bindvarName,"Bind Variable Name",regReturnStatus) )
    return;

  for (ithBV = 0; ithBV < SQLDA_NumEntriesFound(descriptor); ithBV++)
    if ( strncmp(SQLDA_ItemNameBuffer(descriptor,ithBV),bindvarName,
		 SQLDA_ItemNameLength(descriptor,ithBV)) == 0 )
      break;

  if (ithBV == SQLDA_NumEntriesFound(descriptor)) {
    interface_error("Cursor %d:  Cannot find bind variable %s\n",
		    cursorHandle, bindvarName);
    ctop_int(regReturnStatus,INTERFACE_ERROR);
    return;
  }

  if (ptoc_int(regBindvarType) == 0) {   /* Value is given as a string */
    inputString = ptoc_string(regBindvarValue);
    capitalize_word(inputString, nullString, 5);
    if ( strcmp(nullString,"NULL") == 0 ) {
      inputStringLen = 4;
      indicator = -1;
    }
    else {
      inputStringLen = strlen(inputString);
      indicator = 0;
    }
  }
  else {  /* Value is given as an integer */
    inputStringLen = sprintf(numberString, "%d", ptoc_int(regBindvarValue));
    inputString = numberString;
    indicator = 0;
  }

  bufferWidth = SQLDA_ItemValueArrayWidth(descriptor,ithBV) - 1;
  if (cursors[cursorHandle].bv.array_length == 1) {
    if (inputStringLen > bufferWidth) {
      /*
       *  We can take the full value by reallocating.  If there is not
       *  enough memory to expand, the original buffer is left unchanged.
       */
      char *newBuffer =
	realloc(SQLDA_ItemValueArrayBase(descriptor,ithBV),(inputStringLen+1));
      if ( IsNonNULL(newBuffer) ) {
	SQLDA_ItemValueArrayBase(descriptor,ithBV) = newBuffer;
	SQLDA_ItemValueArrayWidth(descriptor,ithBV) = inputStringLen + 1;
	bufferWidth = inputStringLen;
      }
    }
    *SQLDA_IndValueArrayBase(descriptor,ithBV) = indicator;
    entry_loc = SQLDA_ItemValueArrayBase(descriptor,ithBV);
  }
  else {   /* We're stuck with this size ;-( */
    int iArrayEntry = cursors[cursorHandle].bv.cur_entry_index;

    SQLDA_IndValueArrayBase(descriptor,ithBV)[iArrayEntry] = indicator;
    entry_loc = SQLDA_ItemValueArrayBase(descriptor,ithBV) +
      iArrayEntry * (bufferWidth + 1);
  }
  strncpy(entry_loc, inputString, bufferWidth);
  entry_loc[bufferWidth] = '\0';
  ctop_int(regReturnStatus,SUCCESS);
  #ifdef VIEW_BVs
    printf("Set bindvar %s to value %s\n", bindvarName, entry_loc);
  #endif
}

/* ------------------------------------------------------------------------ */

/*
 * Execution for Statements with Cursors
 * -------------------------------------
 *  Assumes that all bind variables have been set and the statement is
 *  ready to execute.  Hence the input tuple index is incremented,
 *  readying the buffer for another tuple of input values (to support
 *  arrayed inputs) and denoting the number of valid tuples presently
 *  recorded in the buffers.  If this number is equal to the length of
 *  the input buffer for this cursor, then the statement is executed.
 *  Otherwise the data sits in the buffers awaiting either a flush
 *  operation or for the buffers to become full, at which time an OPEN
 *  is triggered, sending the data to Oracle.
 */

void execute_cursor() {

  const int regCursorHandle = 2;   /* in */
  const int regNumberSLIs   = 3;   /* in */
  const int regReturnStatus = 4;   /* out */

  CursorHandle cursorHandle;
  int numSLIs, status;


  cursorHandle = ptoc_int(regCursorHandle);
  if ( ! isValidArg_CursorHandle(cursorHandle,regReturnStatus) )
    return;

  numSLIs = ptoc_int(regNumberSLIs);
  if ( ! isValidArg_IOCount(numSLIs, "Select List Items", regReturnStatus) )
    return;

  cursors[cursorHandle].bv.cur_entry_index++;

  if ( cursors[cursorHandle].bv.cur_entry_index ==
       cursors[cursorHandle].bv.array_length ) {
    status = oracleCursor_execute(cursorHandle);
    if ( ! IsSuccessStatus(status) )
      cursors[cursorHandle].bv.cur_entry_index--;
  }
  else
    status = SUCCESS;

  if ( (cursors[cursorHandle].stmt.type != SELECT_SQL_STMT)
       || (! IsSuccessStatus(status)) ) {
    ctop_int(regReturnStatus, status);
    return;
  }

  /* Prepare the output stuff */

  if ( IsNULL(cursors[cursorHandle].sli.descriptor) )
    status = init_output_specification(cursorHandle, numSLIs);

  ctop_int(regReturnStatus, status);
}


/*
 * Execution for Statements without Cursors
 * ----------------------------------------
 *  Short-cut processing for SQL statements which either do not require
 *  buffered data, or will not be executed often.
 */

void execute_immediate() {

  const int regSqlStmtString = 2;   /* in */
  const int regReturnStatus  = 3;   /* out */

  /* EXEC SQL BEGIN DECLARE SECTION; */ 

    char *stmtString;
  /* EXEC SQL END DECLARE SECTION; */ 


  SqlStmtType stmtType;
/*  int count; */


  stmtString = ptoc_string(regSqlStmtString);
  if ( ! isValidArg_String(stmtString,"SQL Statement",regReturnStatus) )
    return;

  stmtType = statement_type(stmtString);
  if (stmtType == SELECT_SQL_STMT) {
    interface_error("SELECT stmts cannot be executed without a cursor!");
    ctop_int(regReturnStatus,INTERFACE_ERROR);
    return;
  }
  else if (stmtType == CURSOR_DAMAGING_SQL_STMT) {
/*    count = invalidate_active_cursors();
    if (count > 0) */
      interface_warning("Executing cursor-damaging SQL statement... ");
/*	      "Invalidating %d active cursors\n", count); */
  }

  /* EXEC SQL WHENEVER SQLERROR GOTO error_handler; */ 

  /* EXEC SQL EXECUTE IMMEDIATE :stmtString; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 3;
  sqlstm.stmt = "";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )2212;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (unsigned char  *)stmtString;
  sqlstm.sqhstl[0] = (unsigned int  )0;
  sqlstm.sqindv[0] = (         short *)0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto error_handler;
}



  ctop_int(regReturnStatus,SUCCESS);
  return;

 error_handler:
  oracle_error("EXECUTE IMMEDIATE failed for SQL statement\n\n\t%s",
	       stmtString);
  ctop_int(regReturnStatus,ORACLE_ERROR);
}

/* ------------------------------------------------------------------------ */

/*
 * Collecting Output Tuples
 * ------------------------
 *  For SELECT statements, readies the next output tuple.  If there are
 *  no more tuples left in the active set, then return an exception
 *  status.
 */

void fetch_from_cursor() {  

  const int regCursorHandle = 2;   /* in */
  const int regReturnStatus = 3;   /* out */

  CursorHandle cursorHandle;
  SLI_Spec *sliSpec;
  int status;


  cursorHandle = ptoc_int(regCursorHandle);
  if ( ! isValidArg_CursorHandle(cursorHandle,regReturnStatus) )
    return;

  if (cursors[cursorHandle].stmt.type != SELECT_SQL_STMT) {
    interface_error("Attempting to fetch from non-SELECT statement "
		    "(cursor %d)\n\n\t%s", cursorHandle,
		    cursors[cursorHandle].stmt.string);
    ctop_int(regReturnStatus,INTERFACE_ERROR);
    return;
  }

  sliSpec = &cursors[cursorHandle].sli;
  #ifdef VIEW_FETCH
    printf("\tRequesting row: %d    Rows received: %d\n",
	   sliSpec->cur_row_number + 1, sliSpec->total_rows_recvd);
  #endif
  if (sliSpec->cur_row_number < sliSpec->total_rows_recvd) {
    #ifdef VIEW_FETCH
      printf("\tUsing buffered data (index %d of %d)\n",
	     sliSpec->cur_row_index + 1, sliSpec->array_length - 1);
    #endif
    sliSpec->cur_row_number++;
    sliSpec->cur_row_index++;
    sliSpec->cur_col_index = 0;
    status = SUCCESS;
  }
  else if (sliSpec->end_of_active_set) {
    #ifdef VIEW_FETCH
    printf("\tNo more rows to retrieve.\n");
    #endif
    status = INTERFACE_EXCEPTION;
  }
  else {
    #ifdef VIEW_FETCH
      printf("\tRetreiving more data.\n");
    #endif
    status = oracleCursor_fetch(cursorHandle);
    if ( IsSuccessStatus(status) ) {
      #ifdef VIEW_FETCH
        printf("\tNumber of new rows received: %ld\n",
	       sqlca.sqlerrd[2] - sliSpec->total_rows_recvd);
      #endif
      sliSpec->cur_row_number++;
      sliSpec->cur_row_index = 0;
      sliSpec->cur_col_index = 0;
      sliSpec->total_rows_recvd = sqlca.sqlerrd[2];
    }
  }
  ctop_int(regReturnStatus,status);
}

/* ------------------------------------------------------------------------ */

/*
 * Get Single Output Value
 * -----------------------
 *  Set to the first column of the tuple in fetch_from_cursor(), return
 *  the current column value to Prolog and make the next column the new
 *  current.  If all columns in the tuple have already been returned,
 *  then return an exception status.
 */

void get_output_value() {

  const int regCursorHandle = 2;   /* in */
  const int regSLI_Value    = 3;   /* in */
  const int regReturnStatus = 4;   /* out */

  CursorHandle cursorHandle;
  SQLDA *sliDesc;
  int iCurColumn;        /* index (into descriptor) for current column */
  int iCurRow;           /* index (into output array) for current row */
  int status;


  cursorHandle = ptoc_int(regCursorHandle);
  if ( ! isValidArg_CursorHandle(cursorHandle,regReturnStatus) )
    return;
  else if ( cursors[cursorHandle].stmt.type != SELECT_SQL_STMT ) {
    interface_error("Attempting to retrieve output values from non-SELECT "
		    "statement (cursor %d)\n\n\t%s", cursorHandle,
		    cursors[cursorHandle].stmt.string);
    ctop_int(regReturnStatus,INTERFACE_ERROR);
    return;
  }
  else if ( cursors[cursorHandle].sli.total_rows_recvd == 0 ) {
    interface_error("Attempting to retrieve output values before they "
		    "are FETCHed!");
    ctop_int(regReturnStatus,INTERFACE_ERROR);
    return;
  }

  sliDesc = cursors[cursorHandle].sli.descriptor;
  iCurColumn = cursors[cursorHandle].sli.cur_col_index;

  if ( iCurColumn >= SQLDA_NumEntriesFound(sliDesc) ) {
    /*
     * We've completed another tuple: there are no more columns left to
     * return.  Signal this by returning an exception status.
     */
#ifdef VIEW_GET
    printf("\n");
#endif
    ctop_int(regReturnStatus,INTERFACE_EXCEPTION);
    return;
  }
  #ifdef VIEW_GET
    printf("\tCol %d: ", iCurColumn);
  #endif

  iCurRow = cursors[cursorHandle].sli.cur_row_index;
  if (SQLDA_IndValueArrayBase(sliDesc,iCurColumn)[iCurRow] < 0) {
    #ifdef VIEW_GET
      printf("NULL\n");
    #endif
    ctop_string( regSLI_Value, string_find("NULL",1) );
    status = SUCCESS;
  }
  else {
    short valueDataType;   /* coerced-to datatype of current column */
    ColumnSpec *colSpec;   /* cursor substruct contains table's column specs */
    char *pColumnValue;    /* ptr into output array to the column's value */
    int arrayWidth;

    valueDataType = SQLDA_ItemValueArrayType(sliDesc,iCurColumn);
    arrayWidth = SQLDA_ItemValueArrayWidth(sliDesc,iCurColumn);
    colSpec = &cursors[cursorHandle].sli.column_specs[iCurColumn];
    pColumnValue = SQLDA_ItemValueArrayBase(sliDesc,iCurColumn) +
                     iCurRow * arrayWidth;

    if (SQLDA_IndValueArrayBase(sliDesc,iCurColumn)[iCurRow] != 0)
      status = ORACLE_EXCEPTION;      /* value truncated */
    else
      status = SUCCESS;
    if (colSpec->datatype == CHAR_ODT)
      remove_blank_padding(pColumnValue,arrayWidth - 1);

    switch ( valueDataType ) {
    /*
     * Currently, we coerce everything to a string.
     */
    case STRING_ODT:
      ctop_string(regSLI_Value, string_find(pColumnValue,1));
      break;
    default:
      interface_error("Cursor %d: Retrieval of SLI %d\nColumn with unexpected"
		      " datatype: %s.\n", cursorHandle, iCurColumn,
		      oracle_datatype_as_string(valueDataType));
      status = INTERFACE_ERROR;
      break;
    }
    #ifdef VIEW_GET
      printf("type %d found: ->%s<-\n", valueDataType, pColumnValue);
    #endif
  }
  cursors[cursorHandle].sli.cur_col_index++;
  ctop_int(regReturnStatus, status);
}

/* ------------------------------------------------------------------------ */

/*
 * Releasing Hold on a Cursor
 * --------------------------
 *  To be called when statement processing with a cursor has completed
 *  but the cursor may be utilized again.  For efficiency sake, neither
 *  local nor database resources are freed at this time
 */

void deallocate_cursor() {

  const int regCursorHandle = 2;   /* in */
  const int regReturnStatus = 3;   /* out */

  CursorHandle cursorHandle;


  cursorHandle = ptoc_int(regCursorHandle);
  if ( ! isValidArg_CursorHandle(cursorHandle,regReturnStatus) )
    return;

  ctop_int(regReturnStatus,suspend_cursor(cursorHandle));
}

/* ------------------------------------------------------------------------ */

/*
 * Freeing a Cursor
 * ----------------
 *  To be called when statement processing with a cursor has completed
 *  and the cursor is no longer needed.  Resources are freed both
 *  locally and at the database, via a CLOSE.
 */

void close_cursor() {

  const int regCursorHandle = 2;   /* in */
  const int regReturnStatus = 3;   /* out */

  CursorHandle cursorHandle;


  cursorHandle = ptoc_int(regCursorHandle);
  if ( ! isValidArg_CursorHandle(cursorHandle,regReturnStatus) )
    return;

  ctop_int(regReturnStatus,free_cursor_resources(cursorHandle));
}

/* ------------------------------------------------------------------------ */

/*
 * Empty Cursor's Input Buffer(s)
 * ------------------------------
 *  To support input host arrays, any remaining data collected as input
 *  must be sent to the database.  Flushing also occurs automatically
 *  from execute_cursor() when the buffers are full.
 */

void flush_cursor() {

  const int regCursorHandle = 2;   /* in */
  const int regReturnStatus = 3;   /* out */

  CursorHandle cursorHandle;


  cursorHandle = ptoc_int(regCursorHandle);
  if ( ! isValidArg_CursorHandle(cursorHandle,regReturnStatus) )
    return;

  if (cursors[cursorHandle].stmt.type != INSERT_SQL_STMT) {
    interface_error("Attempting to flush non-INSERT statement (cursor %d)\n\n"
		    "\t%s", cursorHandle, cursors[cursorHandle].stmt.string);
    ctop_int(regReturnStatus,INTERFACE_ERROR);
    return;
  }

  ctop_int(regReturnStatus,oracleCursor_execute(cursorHandle));
}

/* ------------------------------------------------------------------------ */

/*
 * Reinitialize All Cursors
 * ------------------------
 *  Reclaims all local and database resources for cursors which are (were)
 *  opened but never closed.
 */

void reclaim_cursors() {

  const int regReturnStatus = 2;   /* out */

  ctop_int(regReturnStatus,reinitialize_cursors());
}

/* ------------------------------------------------------------------------ */

/* Disconnecting from Oracle
 * -------------------------
 *  Close the connection to Oracle.  Before doing so, clean up the
 *  interface by deallocating resources and closing opened database
 *  cursors.
 */

  void oracle_disconnect() {

  const int regReturnStatus = 2;   /* out */
  int status;

  status = reinitialize_cursors();
  /* EXEC SQL WHENEVER SQLERROR GOTO disconnect_error; */ 

  /* EXEC SQL COMMIT WORK RELEASE; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 8;
  sqlstm.arrsiz = 3;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )2230;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto disconnect_error;
}


  ctop_int(regReturnStatus,status);
  return;

 disconnect_error:
  oracle_error("Error during commit and disconnection from Oracle");
  ctop_int(regReturnStatus,ORACLE_ERROR);
}

/* ======================================================================== */

/*
 *                  A D D I T I O N A L   B U I L T I N S
 *                  =====================================
 */


void db_sqlcaid() 
{	
  ctop_string(2,(char *) string_find(sqlca.sqlcaid,1));
}

void db_sqlabc()
{
  ctop_int(2,sqlca.sqlabc);
}

void db_sqlcode()
{
  ctop_int(2,sqlca.sqlcode);
}

void db_sqlerrml()
{
  ctop_int(2,sqlca.sqlerrm.sqlerrml);
}

void db_sqlerrmc()
{
  ctop_string(2,(char *) string_find(sqlca.sqlerrm.sqlerrmc,1));
}

void db_sqlerrp()
{
  ctop_int(2,0);
}

void db_sqlerrd0()
{
  ctop_int(2,0);
}

void db_sqlerrd1()
{
  ctop_int(2,0);
}

void db_sqlerrd2()
{
  ctop_int(2,sqlca.sqlerrd[2]);
}

void db_sqlerrd3()
{
  ctop_int(2,0);
}

void db_sqlerrd4()
{
  ctop_int(2,sqlca.sqlerrd[4]);
}

void db_sqlerrd5()
{
  ctop_int(2,0);
}

void db_sqlwarn0()
{
  ctop_string(2,(char *) string_find(&sqlca.sqlwarn[0],1));
}

void db_sqlwarn1()
{
  ctop_string(2,(char *) string_find(&sqlca.sqlwarn[1],1));
}

void db_sqlwarn3()
{
  ctop_string(2,(char *) string_find(&sqlca.sqlwarn[3],1));
}

void db_sqlwarn4()
{
  ctop_string(2,(char *) string_find(&sqlca.sqlwarn[4],1));
}

void db_sqlwarn5()
{
  ctop_string(2,(char *) string_find(&sqlca.sqlwarn[5],1));
}

void db_sqlext()
{
  ctop_int(2,0);
}
