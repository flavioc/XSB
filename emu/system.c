/* File:      system.c
** Author(s): David S. Warren, Jiyang Xu, Kostis F. Sagonas, kifer
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1999
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
#include <process.h>
#else
#include <unistd.h>	
#include <stddef.h>
#include <sys/wait.h>
#endif

#ifndef fileno				/* fileno may be a  macro */
extern int    fileno(FILE *f);	        /* this is defined in POSIX */
#endif

extern FILE *fdopen(int fildes, const char *type);

#ifdef WIN_NT
#define PIPE(filedes)	 _pipe(filedes, 5*MAXBUFSIZE, _O_TEXT)
#else
#define PIPE(filedes)	 pipe(filedes)
#endif

/* return codes from xsb_spawn */
#define  PIPE_TO_PROC_FAILED	-1
#define  PIPE_FROM_PROC_FAILED	-2
#define  SUB_PROC_FAILED	-3

#define SPAWN_PARAM_NO 50  /* max number of cmd line params in a subprocess */

extern char *p_charlist_to_c_string(prolog_term term, char *outstring, 
				    int outstring_size,
				    char *in_func, char *where);
int xsb_spawn (char *prog, char *arg[],
	       int pipe1[], int pipe2[],
	       bool to, bool from);

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
  switch (callno) {
  case 1: /* dumb system call: output to system out */
    ctop_int(3, system(ptoc_string(2)));
    return TRUE;
  case 2: { /* spawn new process, capture its stdin/out and redirect to XSB */
    /* +CallNo=2, +ProcAndArgsList, -StreamToProc, -StreamFromProc */
    static int pipe_to_proc[2], pipe_from_proc[2];
    int toproc_stream, fromproc_stream;
    int status;
    FILE *toprocess, *fromprocess;
    char *params[SPAWN_PARAM_NO+2]; /* one for progname--0th member,
				       one for NULL termination*/
    char arg_buf[MAXPATHLEN];
    prolog_term cmdlist_term, cmdlist_temp_term;
    prolog_term cmd_or_arg_term;
    bool toproc_needed=FALSE, fromproc_needed=FALSE;
    char *cmd_or_arg=NULL;
    int idx = 0;

    cmdlist_term = reg_term(2);
    if (!is_list(cmdlist_term))
      xsb_abort("SPAWN_PROCESS: Arg 1 must be a list [command, arg, ...]");

    /* the user can indicate that he doesn't want either of the streams by
       putting a nonvar in the corresponding argument position */
    if (is_var(reg_term(3)))
      toproc_needed = TRUE;
    if (is_var(reg_term(4)))
      fromproc_needed = TRUE;

    /* fill in the params[] array */
    if (is_nil(cmdlist_term))
      xsb_abort("SPAWN_PROCESS: Arg 1 must not be an empty list");

    cmdlist_temp_term = cmdlist_term;
    do {
      cmd_or_arg_term = p2p_car(cmdlist_temp_term);
      cmdlist_temp_term = p2p_cdr(cmdlist_temp_term);
      if (is_string(cmd_or_arg_term)) {
	cmd_or_arg = string_val(cmd_or_arg_term);
      } else if (is_list(cmd_or_arg_term)) {
	cmd_or_arg =
	  p_charlist_to_c_string(cmd_or_arg_term, arg_buf, sizeof(arg_buf),
				 "SPAWN_PROCESS", "command or argument");
      } else 
	xsb_abort("SPAWN_PROCESS: Non string list member in the Arg");

      params[idx++] = cmd_or_arg;
      if (idx > SPAWN_PARAM_NO)
	xsb_abort("SPAWN_PROCESS: Too many arguments passed to subprocess");

    } while (!is_nil(cmdlist_temp_term));

    params[idx] = NULL; /* null termination */
    

    /* params[0] is the progname */
    status = xsb_spawn(params[0], params, pipe_to_proc, pipe_from_proc,
		       toproc_needed, fromproc_needed);

    if (status < 0) {
      xsb_warn("SPAWN_PROCESS: Subprocess creation failed");
      return FALSE;
    }

    if (toproc_needed) {
      toprocess = fdopen(pipe_to_proc[1], "w");
      toproc_stream = xsb_intern_file(toprocess, "SPAWN_PROCESS");
      ctop_int(3, toproc_stream);
    }
    if (fromproc_needed) {
      fromprocess = fdopen(pipe_from_proc[0], "r");
      fromproc_stream = xsb_intern_file(fromprocess, "SPAWN_PROCESS");
      ctop_int(4, fromproc_stream);
    }

    return TRUE;
  }
  default:
    xsb_abort("SYS_SYSTEM: wrong call number (an XSB bug)");
  }
  return TRUE;
}


/* spawn a subprocess PROGNAME and pass it the arguments ARGV[]
   ARGV must be a NULL-terminated array of strings.
   Also pass it two arrays of strings: PIPE_TO_PROC[2] and PIPE_FROM_PROC[2].
   These are going to be the arrays of fds for the communication pipes 
*/
int xsb_spawn (char *progname, char *argv[],
	       int pipe_to_proc[], int pipe_from_proc[],
	       bool toproc_needed, bool fromproc_needed)
{
  pid_t pid;
  int stdin_saved, stdout_saved;

  if ( toproc_needed && PIPE(pipe_to_proc) < 0 ) {
    /* can't open pipe to process */
    return PIPE_TO_PROC_FAILED;
  }
  if ( fromproc_needed && PIPE(pipe_from_proc) < 0 ) {
    /* can't open pipe to process */
    return PIPE_FROM_PROC_FAILED;
  }

  /* The following is due to the awkwardness of windoze process creation.
     We commit this atrocity in order to be portable between Unix and Windows.
     1. Save stdio of the parent process.
     2. Redirect main process stdio to the pipes.
     3. Spawn subprocess. The subprocess inherits the redirected I/O
     4. Restore the original stdio for the parent process.
  */

  /* save I/O */
  stdin_saved = dup(fileno(stdin));
  stdout_saved = dup(fileno(stdout));

  if (toproc_needed) {
    /* close child stdin, bind it to the reading part of pipe_to_proc */
    if (dup2(pipe_to_proc[0], fileno(stdin)) < 0) {
      return PIPE_TO_PROC_FAILED;
    }
    close(pipe_to_proc[0]); /* close the original read end of pipe */
  }

  if (fromproc_needed) {
    /* close child stdout, bind it to the write part of pipe_from_proc */
    if (dup2(pipe_from_proc[1], fileno(stdout)) < 0) {
      return PIPE_TO_PROC_FAILED;
    }
    close(pipe_from_proc[1]); /* close the original write end of pipe */
  }

#ifdef WIN_NT
  pid = spawnvp(P_NOWAIT, progname, argv);
#else
  pid = fork();
#endif

  if (pid < 0) {
    /* failed */
    return pid;
  } else if (pid == 0) {
    /* child process */

#ifndef WIN_NT  /* Unix: must exec */
    execvp(progname, argv);
    /* if we ever get here, this means that invocation of the process has
       failed */
    exit(SUB_PROC_FAILED);
#endif
  }

  /* main process continues */

  /* duplicate saved copies of stdio fds back into main process stdio */
  if (dup2(stdin_saved, fileno(stdin)) < 0) {
    return PIPE_TO_PROC_FAILED;
  }
  if (dup2(stdout_saved, fileno(stdout)) < 0) {
    return PIPE_TO_PROC_FAILED;
  }

  return TRUE;
}

