/* File:      sig.h
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


/* signals 1 and 8-15 are asynchronous. They are flaged by the variable  */
/* *asynint_ptr, which can be 8-15 and maybe OR'ed with KEYINT_MARK.	 */
/* At some interval (currently at the entry of "call", etc), a check is  */
/* made to see if there is an asynchronous interrupt occurred. If yes,	 */
/* the procedure "interrupt_proc" is invoked. The rest of signals (0 and */
/* 2-4) are synchronous. The procedure "interrupt_proc" is invoked       */
/* directly.								 */

#define MYSIG_UNDEF 0			/* undefined predicate */
#define MYSIG_KEYB 1			/* keyboard interrupt (^C) */
#define MYSIG_SPY 3			/* spy point */
#define MYSIG_TRACE 4			/* trace point */
#define MYSIG_CLAUSE 16			/* clause interrupt */

#define KEYINT_MARK 0x80		/* keyboard interrupt ^C */

#define MSGINT_MARK 0x20		/* software message interrupt */

extern int *asynint_ptr;	/* 0 - no interrupt (or being processed) */



/*
 *  These are no longer supported.
 */

/*** #define MYSIG_OFMEM 2 ***/		/* global/local stack overflow */
/*** #define MYSIG_OFTC 5 ***/		/* trail/CP stack overflow */
/*** #define MYSIG_KILLED 6 ***/	/* response to MYSIG_KILL */
/*** #define MYSIG_STAT 7 ***/		/* gather statistics */
/*** #define MYSIG_TERM 8 ***/		/* termination of child */
/*** #define MYSIG_CUT 9 ***/		/* send by child to parent */
/*** #define MYSIG_KILL 10 ***/		/* send by parent to child */
/*** #define MYSIG_DELAY 11 ***/	/* delayed goal awaking */
/*** #define MYSIG_QJOB 12 ***/		/* request for job; not used now */
/*** #define MYSIG_RJOB 13 ***/		/* response for job; not used now */
/*** #define MYSIG_GJOB 14 ***/		/* get a job */
/*** #define MYSIG_SJOB 15 ***/		/* send a job */

/*** #define DELAYINT_MARK 0x40 ***/	/* delayed goal awaken interrupt */
