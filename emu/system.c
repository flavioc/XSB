/* File:      system.c
** Author(s): David S. Warren, Jiyang Xu, Kostis F. Sagonas
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

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "configs/config.h"
#include "basictypes.h"
#include "auxlry.h"
#include "cell.h"
#include "xsberror.h"
#include "cinterf.h"
#include "msyscall.h"
#include "io_builtins.h"
/* special.h must be included after sys/stat.h */
#include "configs/special.h"

#ifdef WIN_NT
#include <direct.h>
#include <io.h>
#else
#include <unistd.h>
#endif

extern FILE *popen(const char *cmd, const char *mode);
extern int pclose(FILE *stream);

int sys_syscall(int callno)
{
  int result;
  struct stat stat_buff;

  switch (callno) {
#if (!defined(WIN_NT))
    case SYS_getpid : result = getpid(); break; 
    case SYS_link  : result = link(ptoc_string(3), ptoc_string(4)); break;
#endif
    case SYS_unlink: result = unlink(ptoc_string(3)); break;
    case SYS_chdir : result = chdir(ptoc_string(3)); break;
    case SYS_access: result = access(ptoc_string(3), ptoc_int(4)); break;
    case SYS_stat  : result = stat(ptoc_string(3), &stat_buff); break;
    case SYS_rename: result = rename(ptoc_string(3), ptoc_string(4)); break;
    default: fprintf(stderr, "Unknown system call number %d\n", callno);
	     result = -1;
  }
  return result;
}


bool sys_system(int callno)
{
  FILE *sysout, *fptr;
  char *syscmd, *prolog_mode, *c_style_mode=NULL;
  int i, filedes;

  switch (callno) {
  case 1: /* dumb system call: output to system out */
    ctop_int(3, system(ptoc_string(2)));
    break;
  case 2: /* smart system call: output to a new stream */
    syscmd = ptoc_string(2);
    prolog_mode = ptoc_string(3);
    if (strcmp(prolog_mode, "read") == 0)
      c_style_mode = "r";
    else if (strcmp(prolog_mode, "write") == 0)
      c_style_mode = "w";
    else
      xsb_abort("OPEN_SHELL_STREAM: Invalid read/write mode: %s", prolog_mode);

    sysout = popen(syscmd, c_style_mode);

    /* if can't open pipe or not enough memory, fail */
    if (syscmd == NULL) return FALSE;

    i = xsb_intern_file(sysout, "SHELL");
    ctop_int(4, i);
    break;
  case 3: /* pclose*/
    /* take XSB stream that represents a pipe opened to a shell process and do
       pclose on it */
    filedes = ptoc_int(2);
    SET_FILEPTR(fptr, filedes);
    ctop_int(3, pclose(fptr));
    open_files[filedes] = NULL;
    break;
  default:
    xsb_abort("SYS_SYSTEM: wrong call number (an XSB bug)");
  }
  return TRUE;
}

