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
#include <signal.h>
#include <string.h>
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
#include "system.h"
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

#ifndef WIN_NT
extern int kill(int pid, int sig);
#endif

#ifdef WIN_NT
#define PIPE(filedes)	 _pipe(filedes, 5*MAXBUFSIZE, _O_TEXT)
#else
#define PIPE(filedes)	 pipe(filedes)
#endif

#define FREE_PROC_TABLE_CELL(pid)   ((pid < 0) \
				     || ((process_status(pid) != RUNNING) \
					 && (process_status(pid) != STOPPED)))

/* return codes from xsb_spawn */
#define  PIPE_TO_PROC_FAILED	-1
#define  PIPE_FROM_PROC_FAILED	-2
#define  SUB_PROC_FAILED	-3

#define MAX_SUBPROC_PARAMS 30  /* max # of cmdline params in a subprocess */

#define MAX_SUBPROC_NUMBER 20  /* max number of subrocesses allowed       */


#define RUNNING	       1
#define STOPPED	       2
#define EXITED	       3
#define ABORTED	       4
#define INVALID	       5
#define UNKNOWN	       6

extern char *p_charlist_to_c_string(prolog_term term, char *outstring, 
				    int outstring_size,
				    char *in_func, char *where);

static int xsb_spawn (char *prog, char *arg[],
	       int pipe1[], int pipe2[], int pipe3[],
	       bool to, bool from, bool fromstderr);
static void concat_array(char *array[], char *result_str, int maxsize);
static int get_free_process_cell(void);
static void init_process_table(void);
static int process_status(pid_t pid);


static struct proc_table_t {
  int search_idx;	       /* index where to start search for free cells */
  struct proc_array_t {
    int pid;
    int to_stream;     	       /* XSB stream to process stdin    */
    int from_stream;           /* XSB stream from process stdout */
    int stderr_stream;	       /* XSB stream from process stderr */
    char cmdline[MAXPATHLEN];  /* the cmd line used to invoke the process */
  } process[MAX_SUBPROC_NUMBER];
} xsb_process_table;


int sys_syscall(int callno)
{
  int result=-1;
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
    default: xsb_abort("Unknown system call number, %d", callno);
  }
  return result;
}

bool sys_system(int callno)
{
  int pid;

  switch (callno) {
  case PLAIN_SYSTEM_CALL: /* dumb system call: no communication with XSB */
    ctop_int(3, system(ptoc_string(2)));
    return TRUE;
  case SPAWN_PROCESS: { /* spawn new process, reroute stdin/out/err to XSB */
    /* +CallNo=2, +ProcAndArgsList,
       -StreamToProc, -StreamFromProc, -StreamFromProcStderr,
       -Pid */
    static int pipe_to_proc[2], pipe_from_proc[2], pipe_from_stderr[2];
    int toproc_stream=-1, fromproc_stream=-1, fromproc_stderr_stream=-1;
    int pid_or_status;
    FILE *toprocess_fptr, *fromprocess_fptr, *fromproc_stderr_fptr;
    char *params[MAX_SUBPROC_PARAMS+2]; /* one for progname--0th member,
				       one for NULL termination*/
    char arg_buf[MAXPATHLEN];
    prolog_term cmdlist_term, cmdlist_temp_term;
    prolog_term cmd_or_arg_term;
    bool toproc_needed=FALSE, fromproc_needed=FALSE, fromstderr_needed=FALSE;
    char *cmd_or_arg=NULL;
    int idx = 0, tbl_pos;

    init_process_table();

    cmdlist_term = reg_term(2);
    if (!is_list(cmdlist_term))
      xsb_abort("SPAWN_PROCESS: Arg 1 must be a list [command, arg, ...]");

    /* the user can indicate that he doesn't want either of the streams by
       putting a nonvar in the corresponding argument position */
    if (is_var(reg_term(3)))
      toproc_needed = TRUE;
    if (is_var(reg_term(4)))
      fromproc_needed = TRUE;
    if (is_var(reg_term(5)))
      fromstderr_needed = TRUE;

    if (!is_var(reg_term(6)))
      xsb_abort("SPAWN_PROCESS: Arg 5 (process id) must be a variable");

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
      if (idx > MAX_SUBPROC_PARAMS)
	xsb_abort("SPAWN_PROCESS: Too many arguments passed to subprocess");

    } while (!is_nil(cmdlist_temp_term));

    params[idx] = NULL; /* null termination */
    
    /* -1 means: no space left */
    if ((tbl_pos = get_free_process_cell()) < 0) {
      xsb_warn("Can't create subprocess: XSB process table is full");
      return FALSE;
    }

    /* params[0] is the progname */
    pid_or_status = xsb_spawn(params[0], params,
			      pipe_to_proc, pipe_from_proc, pipe_from_stderr,
			      toproc_needed,fromproc_needed,fromstderr_needed);

    if (pid_or_status < 0) {
      xsb_warn("SPAWN_PROCESS: Subprocess creation failed");
      return FALSE;
    }

    if (toproc_needed) {
      toprocess_fptr = fdopen(pipe_to_proc[1], "w");
      toproc_stream = xsb_intern_file(toprocess_fptr, "SPAWN_PROCESS");
      ctop_int(3, toproc_stream);
    }
    if (fromproc_needed) {
      fromprocess_fptr = fdopen(pipe_from_proc[0], "r");
      fromproc_stream = xsb_intern_file(fromprocess_fptr, "SPAWN_PROCESS");
      ctop_int(4, fromproc_stream);
    }
    if (fromstderr_needed) {
      fromproc_stderr_fptr = fdopen(pipe_from_proc[0], "r");
      fromproc_stderr_stream
	= xsb_intern_file(fromproc_stderr_fptr, "SPAWN_PROCESS");
      ctop_int(5, fromproc_stderr_stream);
    }
    ctop_int(6, pid_or_status);

    xsb_process_table.process[tbl_pos].pid = pid_or_status;
    xsb_process_table.process[tbl_pos].to_stream = toproc_stream;
    xsb_process_table.process[tbl_pos].from_stream = fromproc_stream;
    xsb_process_table.process[tbl_pos].stderr_stream = fromproc_stderr_stream;
    concat_array(params,xsb_process_table.process[tbl_pos].cmdline,MAXPATHLEN);
    
    return TRUE;
  }

  case GET_PROCESS_TABLE: { /* sys_system(3, X). X is bound to the list
	       of the form [process(Pid,To,From,Stderr,Cmdline), ...] */
    int i;
    prolog_term table_term_tail, listHead;
    prolog_term table_term=reg_term(2);

    init_process_table();

    if (!is_var(table_term))
      xsb_abort("GET_PROCESS_TABLE: Arg 1 must be a variable");

    table_term_tail = table_term;
    for (i=0; i<MAX_SUBPROC_NUMBER; i++) {
      if (!FREE_PROC_TABLE_CELL(xsb_process_table.process[i].pid)) {
	c2p_list(table_term_tail); /* make it into a list */
	listHead = p2p_car(table_term_tail);

	c2p_functor("process", 5, listHead);
	c2p_int(xsb_process_table.process[i].pid, p2p_arg(listHead,1));
	c2p_int(xsb_process_table.process[i].to_stream, p2p_arg(listHead,2));
	c2p_int(xsb_process_table.process[i].from_stream, p2p_arg(listHead,3));
	c2p_int(xsb_process_table.process[i].stderr_stream,
		p2p_arg(listHead,4));
	c2p_string(xsb_process_table.process[i].cmdline, p2p_arg(listHead,5));

	table_term_tail = p2p_cdr(table_term_tail);
      }
    }
    c2p_nil(table_term_tail); /* bind tail to nil */
    return p2p_unify(table_term, reg_term(2));
  }

  case PROCESS_STATUS: {
    prolog_term pid_term=reg_term(2), status_term=reg_term(3);

    init_process_table();

    if (!is_int(pid_term))
      xsb_abort("PROCESS_STATUS: Arg 1 (process id) must be an integer");
    pid = int_val(pid_term);

    if (!is_var(status_term))
      xsb_abort("PROCESS_STATUS: Arg 2 (process staus) must be a variable");
    
    switch (process_status(pid)) {
    case RUNNING:
      c2p_string("running", status_term);
      break;
    case STOPPED:
      c2p_string("stopped", status_term);
      break;
    case EXITED:
      c2p_string("exited", status_term);
      break;
    case ABORTED:
      c2p_string("aborted", status_term);
      break;
    case INVALID:
      c2p_string("invalid", status_term);
      break;
    default:
      c2p_string("unknown", status_term);
    }
    return TRUE;
  }

  case PROCESS_CONTROL: {
    /* sys_system(PROCESS_CONTROL, +Pid, +Signal). Signal: wait, kill */
    char *signal;
    int status;
    prolog_term pid_term=reg_term(2), signal_term=reg_term(3);

    init_process_table();

    if (!is_int(pid_term))
      xsb_abort("PROCESS_CONTROL: Arg 1 (process id) must be an integer");
    pid = int_val(pid_term);

    if (!is_string(signal_term))
      xsb_abort("PROCESS_CONTROL: Arg 2 (process status) must be a variable");
    signal = string_val(signal_term);

    if (strcmp(signal, "kill")==0) {
#ifdef WIN_NT
      if (!TerminateProcess(pid, -1))   /* -1 is the return value */
#else
      if (kill(pid, SIGKILL) < 0)
#endif
	return FALSE;  /* some kind of a problem during kill */
      return TRUE;
    }
    if (strcmp(signal, "wait")==0) {
#ifdef WIN_NT
      if (_cwait(&status, pid, NULL) < 0)
#else
      if (waitpid(pid, &status, 0) < 0)
#endif
	return FALSE;
      return TRUE;
    }

    xsb_warn("PROCESS_CONTROL: Invalid signal, %s", signal);
    return FALSE;
  }

  default:
    xsb_abort("SYS_SYSTEM: wrong call number (an XSB bug)");
  } /* end case */
  return TRUE;
}


/* spawn a subprocess PROGNAME and pass it the arguments ARGV[]
   ARGV must be a NULL-terminated array of strings.
   Also pass it two arrays of strings: PIPE_TO_PROC[2] and PIPE_FROM_PROC[2].
   These are going to be the arrays of fds for the communication pipes 
*/
static int xsb_spawn (char *progname, char *argv[],
	       int pipe_to_proc[],int pipe_from_proc[],int pipe_from_stderr[],
	       bool toproc_needed,bool fromproc_needed,bool fromstderr_needed)
{
  pid_t pid;
  int stdin_saved, stdout_saved, stderr_saved;

  if ( toproc_needed && PIPE(pipe_to_proc) < 0 ) {
    /* can't open pipe to process */
    return PIPE_TO_PROC_FAILED;
  }
  if ( fromproc_needed && PIPE(pipe_from_proc) < 0 ) {
    /* can't open pipe to process */
    return PIPE_FROM_PROC_FAILED;
  }
  if ( fromstderr_needed && PIPE(pipe_from_stderr) < 0 ) {
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
  stderr_saved = dup(fileno(stderr));

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

  if (fromstderr_needed) {
    /* close child stdout, bind it to the write part of pipe_from_proc */
    if (dup2(pipe_from_stderr[1], fileno(stderr)) < 0) {
      return PIPE_TO_PROC_FAILED;
    }
    close(pipe_from_stderr[1]); /* close the original write end of pipe */
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
  if (dup2(stderr_saved, fileno(stderr)) < 0) {
    return PIPE_TO_PROC_FAILED;
  }

  return pid;
}

/* array is a NULL terminated array of strings. Concat it and return string */
static void concat_array(char *array[], char *result_str, int maxsize)
{
  int space_left = maxsize-1;
  char *current_pos=result_str;
  int idx=0;

  /* init result_str */
  *current_pos='\0';

  /* Die, he who neglects to NULL-terminate an array */
  while ((array[idx] != NULL) && (space_left > 0)) {
    int len = strlen(array[idx]);

    strncat(current_pos, array[idx], space_left);
    current_pos = current_pos + (len < space_left ? len : space_left);
    *current_pos='\0';
    space_left = space_left - len;
    /* insert space separator */
    strncat(current_pos, " ", space_left);
    current_pos++;
    *current_pos='\0';
    space_left--;
    idx++;
  }
}


static int get_free_process_cell(void) 
{
  int possible_free_cell = xsb_process_table.search_idx;
  int pid;

  do {
    pid = xsb_process_table.process[possible_free_cell].pid;
    if (FREE_PROC_TABLE_CELL(pid)) {
      /* free cell found */
      xsb_process_table.search_idx =
	(possible_free_cell + 1) % MAX_SUBPROC_NUMBER;
      return possible_free_cell;
    }
    possible_free_cell = (possible_free_cell + 1) % MAX_SUBPROC_NUMBER;
  } while (possible_free_cell != xsb_process_table.search_idx);

  /* no space */
  return -1;
}


static void init_process_table(void)
{
  static bool process_table_initted = FALSE;
  int i;

  if (!process_table_initted) {
    for (i=0; i<MAX_SUBPROC_NUMBER; i++) {
      xsb_process_table.process[i].pid = -1;
    }
    xsb_process_table.search_idx = 0;
    process_table_initted = TRUE;
  }
}


/* check process status */
int process_status(int pid)
{
#ifdef WIN_NT
  LPDWORD status;
  if (GetExitCodeProcess(pid, &status)) {
    if (status == STILL_ACTIVE)
      return RUNNING;
    else
      return EXITED;
  } else
    return INVALID;
#else
  int retcode;
  int status;
  /* don't wait for children that run or are stopped */
  retcode = waitpid(pid, &status, WNOHANG | WUNTRACED);

  if (retcode == 0)    	   return RUNNING; /* running	       */
  if (retcode < 0)         return INVALID; /* doesn't exist or isn't a child */
  if (WIFSTOPPED(status))  return STOPPED; /* stopped	       */
  if (WIFEXITED(status))   return EXITED;  /* exited normally  */
  if (WIFSIGNALED(status)) return ABORTED; /* aborted	       */

  return UNKNOWN; /*  unknown status */
#endif
}
