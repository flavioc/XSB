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


#include "configs/config.h"
#include "cell.h"
#include "cinterf.h"
#include "msyscall.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN_NT
/* added by Juliana 03/10/97 */
#include <direct.h>
#include <io.h>
#else
#include <unistd.h>
#endif

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

