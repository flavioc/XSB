/* File:      auxlry.h
** Author(s): Warren, Xu, Swift, Sagonas, Johnson
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


#include "basictypes.h"

#define MOD %

#define IsNULL(ptr)      ( (ptr) == NULL )
#define IsNonNULL(ptr)   ( (ptr) != NULL )

typedef unsigned char byte;
typedef unsigned int counter;
typedef unsigned long word;
typedef byte *pb;
typedef word *pw;
typedef int (*PFI)();
typedef int *int_ptr;

#define ihash(val, size) (word)(val) % (size)

extern double cpu_time(void);
extern double real_time(void);

struct trace_str {		/* for tracing purpose below */
    unsigned long maxlstack_count, maxgstack_count, maxtrail_count, maxcpstack_count;
    unsigned long maxopenstack_count, maxlevel_num;
    double time_count;
};

extern struct trace_str tds;

extern byte call_intercept;	/* hitrace or trace_sta for efficiency */

#define local_global_exception(t_pcreg) \
 t_pcreg = exception_handler("! Local/Global Stack Overflow Exception\n")

#define float_unification_exception(t_pcreg) \
 t_pcreg = exception_handler("! Float Unification Exception\n")

#define unify_float_unification_exception \
  exception_handler("! Float Unification Exception\n")

#define complstack_exception(t_pcreg) \
 t_pcreg = exception_handler("! Completion Stack Overflow Exception\n")

#define trail_cp_exception(t_pcreg) \
 t_pcreg = exception_handler("! Trail/CP Stack Overflow Exception\n")


/*
 *  Mode in which XSB is run.
 */

typedef enum XSB_Execution_Mode {
  DEFAULT,           /* mode has not been set by user */
  INTERPRETER,       /* currently the mode to be used in default condition */
  DISASSEMBLE,       /* dissassemble .O file */
  C_CALLING_XSB,
  CUSTOM_BOOT_MODULE,     /* user specifies boot module on the command line */
  CUSTOM_CMD_LOOP_DRIVER  /* user specifies command loop driver 
			     on the command line */ 
} Exec_Mode;

extern Exec_Mode xsb_mode;

#define fileptr(xsb_filedes)  open_files[xsb_filedes]

extern char *xsb_default_segfault_msg;
extern char *xsb_segfault_message;

/* This would yield a meaningful message in case of segfault */
#define SET_FILEPTR(stream, xsb_filedes) \
    if (xsb_filedes < 0 || xsb_filedes >= MAX_OPEN_FILES) \
	xsb_abort("Invalid file descriptor %d in I/O predicate", xsb_filedes);\
    stream = fileptr(xsb_filedes); \
    if ((stream==NULL) && (xsb_filedes != 0)) \
	xsb_abort("Invalid file descriptor %d in I/O predicate", xsb_filedes);
