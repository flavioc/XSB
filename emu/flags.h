/* File:      flags.h
** Author(s): Jiyang Xu, Kostis F. Sagonas, Ernie Johnson
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


extern Cell flags[];		/* System flags + user flags */

/* -------- system flags --------------------------------------------------*/

/*	used by interpreter ->  Y/N ; read only -> R/W/N (no access)	   */

#define PIL_TRACE 	 0	/* 0 = pil trace off, 1 = on		YW */
#define HITRACE		 1	/* 0 = hitrace off, 1 = on		YW */
#define STACK_REALLOC    2      /* 1 = allow realloc of stacks on Oflow YW */
#define TRACE_STA	 3	/* 1 = keep max stack size stats	YW */
#define DEBUG_ON	 4	/* 1 = debug on; 0 = off 		YW */
#define HIDE_STATE	 5	/* 0 = no hide, >0 = hide level 	YW */
#define TRACE		 6	/* 1 = trace on, 0 = trace off	    	YW */
#define INVOKE_NUM	 7	/* debugger, the ordinal invoke number 	NW */
#define SKIPPING	 8	/* debugger, 1 = skip, 0 = not	   	NW */
#define QUASI_SKIPPING	 9	/* debugger, 1 = quasi skip, 0 = not	NW */
#define CURRENT_INPUT	10	/* current input file descriptor	NW */
#define CURRENT_OUTPUT	11	/* current output file descriptor	NW */
#define CURRENT_MODULE	12	/* current module (0 = usermod)		YW */
#define MOD_LIST	13	/* the list of module (Psc) entries	YR */
#define RELOC_TABLE	14	/* relocation table			YR */

#define DCG_MODE        18      /* DGC mode: standard or xsb	        MK */

#define GARBAGE_COLLECT 20      /* type of garbage collecion employed:     */
                                /* 0 = none; 1 = sliding; 2 = copying.  NW */
#define CMD_LINE_GOAL  	21	/* The Prolog goal passd on cmd 
				   line with -e	       	       	           */
#define USER_HOME  	22	/* $HOME, if not null. Else INSTALL_DIR    */
#define INSTALL_DIR	23	/* determined dynamically in xmain.c   	   */

#define CLAUSE_INT	24	/* for clause interrupt			YW */

#define CONFIG_FILE	26	/* Where xsb_configuration.P lives	   */
/* loader uses CONFIG_NAME flag before xsb_configuration is loaded */
#define CONFIG_NAME	28	/* this looks like this: cpu-vendor-os	   */


/*
 *  Flags 32-48 are reserved for Interrupt Handler PSCs.
 */

/* --------------------------------------------------------
As best as I can tell, only the following exist/are used:

 MYSIG_UNDEF      32    // _$load_undef
 MYSIG_KEYB       33    // _$keyboard_int
 MYSIG_SPY        35    // _$calltonum

 MYSIG_CLAUSE     48    // _$clause_int
--------------------------------------------------------- */


/* This flag is used by the loader to tell itself whether it should look into
   user-supplied library search paths or not. If 0, the loader will look only
   in lib/syslib/cmplib. If 1, the loader will look in library_directory/1
   before checking the standard places. */
#define LIBS_LOADED	  50

#define PROFFLAG          52

#define LETTER_VARS	  55      /* For printing vars in the interpreter */
#define BOOT_MODULE       56      /* First file loaded; usually loader.P  */
#define CMD_LOOP_DRIVER   57      /* File that contains top-level command loop
				     driver  */

#define ORA_INPUTARRAY_LENGTH     58   /* max # simultaneous input tuples */
#define ORA_INPUTARRAY_WIDTH      59   /* max size of each input value    */
#define ORA_OUTPUTARRAY_LENGTH    60   /* max # simultaneous output tuples */
