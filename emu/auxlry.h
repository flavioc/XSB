/* File:      auxlry.h
** Author(s): Warren, Xu, Swift, Sagonas, Johnson
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1999
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

#ifndef __AUXLRY_H__
#define __AUXLRY_H__

#include "basicdefs.h"
#include "basictypes.h"

extern double cpu_time(void);
extern double real_time(void);
extern void get_date(int *year, int *month, int *day,
		    int *hour, int *minute, int *second);

#define ihash(val, size) ((word)(val) % (size))

#ifndef MULTI_THREAD
extern int asynint_val;
#endif

/*
 *  Mode in which XSB is run.
 */

typedef enum XSB_Execution_Mode {
  DEFAULT,           /* mode has not been set by user */
  INTERPRETER,       /* currently the mode to be used in default condition */
  DISASSEMBLE,       /* dissassemble object file file */
  C_CALLING_XSB,
  CUSTOM_BOOT_MODULE,     /* user specifies boot module on the command line */
  CUSTOM_CMD_LOOP_DRIVER  /* user specifies command loop driver 
			     on the command line */ 
} Exec_Mode;

extern Exec_Mode xsb_mode;

#define fileptr(xsb_filedes)  open_files[xsb_filedes].file_ptr

extern char *xsb_default_segfault_msg;
extern char *xsb_segfault_message;

/* This would yield a meaningful message in case of segfault */
#define SET_FILEPTR(stream, xsb_filedes) \
    if (xsb_filedes < 0 || xsb_filedes >= MAX_OPEN_FILES) \
	xsb_abort("Invalid file descriptor %d in I/O predicate",\
			xsb_filedes);\
    stream = fileptr(xsb_filedes); \
    if ((stream==NULL) && (xsb_filedes != 0)) \
	xsb_abort("Invalid file descriptor %d in I/O predicate", xsb_filedes);

extern void gdb_dummy(void);

#endif /* __AUXLRY_H__ */

