/* File:      io_builtins_xsb_i.h
** Author(s): davulcu, kifer
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


/* This file is separate from io_builtins.c because here we have the
   in-lined file_function (to speed up file_get/put). */


#include "file_modes_xsb.h"

static struct stat stat_buff;
extern char   *expand_filename(char *filename);


static FILE *stropen(char *str)
{
  int i;
  STRFILE *tmp;

  for (i=0; i<MAXIOSTRS; i++) {
    if (iostrs[i] == NULL) break;
  }
  if (i>=MAXIOSTRS) return FALSE;
  tmp = (STRFILE *)mem_alloc(sizeof(STRFILE));
  iostrs[i] = tmp;
  tmp->strcnt = strlen(str);
  tmp->strptr = str;
  tmp->strbase = str;
  return (FILE *)iostrdecode(i);
}

static void strclose(int i)
{
  i = iostrdecode(i);
  mem_dealloc((byte *)iostrs[i],sizeof(STRFILE));
  iostrs[i] = NULL;
}



/* file_flush, file_pos, file_truncate, file_seek */
inline static xsbBool file_function(void)
{
  static FILE *fptr;
  static int io_port, value, size, offset, length, mode;
  static STRFILE *sfptr;
  static XSB_StrDefine(VarBuf);
  static char *addr, *tmpstr;
  static prolog_term pterm;
  static Cell term;
  static char *strmode;

  switch (ptoc_int(1)) {
  case FILE_FLUSH: /* file_function(0,+IOport,-Ret,_,_) */
    /* ptoc_int(2) is XSB I/O port */
    SET_FILEPTR(fptr, ptoc_int(2));   
    value = fflush(fptr);
    ctop_int(3, (int) value);
    break;
  case FILE_SEEK: /* file_function(1,+IOport, +Offset, +Place, -Ret) */
    SET_FILEPTR(fptr, ptoc_int(2));
    value = fseek(fptr, (long) ptoc_int(3), ptoc_int(4));
    ctop_int(5, (int) value);
    break;
  case FILE_TRUNCATE: /* file_function(2,+IOport,+Length,-Ret,_) */
    size = ptoc_int(3);
#ifndef WIN_NT
    SET_FILEPTR(fptr, ptoc_int(2));
    fseek(fptr, (long) size, 0);
    value = ftruncate( fileno(fptr), (off_t) size);
    ctop_int(4, (int) value);
#else
    xsb_warn("FILE_TRUNCATE: operation not supported under Windows.");
#endif
    break;
  case FILE_POS: /* file_function(3, +IOport, -Pos) */
    io_port = ptoc_int(2); 
    term = ptoc_tag(3);
    if (io_port >= 0) {
      SET_FILEPTR(fptr, io_port);
      if (isnonvar(term))
	return ptoc_int(3) == ftell(fptr);
      else
	ctop_int(3, ftell(fptr));
    } else { /* reading from string */
      sfptr = strfileptr(io_port);
      offset = sfptr->strptr - sfptr->strbase;
      if (isnonvar(term))
	return ptoc_int(3) == offset;
      else
	ctop_int(3, offset);
    }
    break;
  case FILE_OPEN:
    /* file_function(4, +FileName, +Mode, -IOport)
       When read, mode = 0; when write, mode = 1, 
       when append, mode = 2, when opening a 
       string for read mode = 3 */

    tmpstr = ptoc_string(2);
    pterm = reg_term(3);
    if (is_int(pterm))
      mode = int_val(pterm);
    else if (is_string(pterm)) {
      switch ((string_val(pterm))[0]) {
      case 'r': mode = OREAD; break;
      case 'w': mode = OWRITE; break;
      case 'a': mode = OAPPEND; break;
      case 's':
	if ((string_val(pterm))[1] == 'r')
	  /* reading from string */
	  mode = OSTRINGR;
	else if ((string_val(pterm))[1] == 'w')
	  /* writing to string */
	  mode = OSTRINGW;
	else
	  mode = -1;
	break;
      default: mode = -1;
      }
    } else
      xsb_abort("[FILE_OPEN] File opening mode must be an atom or an integer");

    switch (mode) {
      /* "b" does nothing, but POSIX allows it */
    case OREAD:   strmode = "rb"; break; /* READ_MODE */
    case OWRITE:  strmode = "wb"; break; /* WRITE_MODE */
    case OAPPEND: strmode = "ab"; break; /* APPEND_MODE */
    case OSTRINGR:
      if ((fptr = stropen(tmpstr)))
	ctop_int(4, (Integer)fptr);
      else 
	ctop_int(4, -1000);
      return TRUE;
    case OSTRINGW:
      xsb_abort("[FILE_OPEN] Output to strings has not been implemented yet");
      ctop_int(4, -1000);
      return TRUE;
    default:
      xsb_warn("FILE_OPEN: Invalid open file mode");
      ctop_int(4, -1000);
      return TRUE;
    }
    
    /* we reach here only if the mode is OREAD,OWRITE,OAPPEND */
    addr = expand_filename(tmpstr);
    fptr = fopen(addr, strmode);

    if (fptr) {
      if (!stat(addr, &stat_buff) && !S_ISDIR(stat_buff.st_mode))
	/* file exists and isn't a dir */
	ctop_int(4, xsb_intern_file(fptr, "FILE_OPEN"));
      else {
	xsb_warn("FILE_OPEN: File %s is a directory, cannot open!", tmpstr);
	fclose(fptr);
	ctop_int(4, -1);
      }
    } else
      ctop_int(4, -1);
    
    break;

  case FILE_CLOSE: /* file_function(5, +IOport) */
    io_port = ptoc_int(2);
    if (io_port < 0) strclose(io_port);
    else {
      SET_FILEPTR(fptr, io_port);
      fclose(fptr);
      open_files[io_port] = NULL;
    }
    break;
  case FILE_GET:	/* file_function(6, +IOport, -IntVal) */
    io_port = ptoc_int(2);
    if ((io_port < 0) && (io_port >= -MAXIOSTRS)) {
      sfptr = strfileptr(io_port);
      ctop_int(3, strgetc(sfptr));
    } else {
      SET_FILEPTR(fptr, io_port);
      ctop_int(3, getc(fptr));
    }
    break;
  case FILE_PUT:   /* file_function(7, +IOport, +IntVal) */
    /* ptoc_int(2) is XSB I/O port */
    io_port = ptoc_int(2);
    SET_FILEPTR(fptr, io_port);
    /* ptoc_int(3) is char to write */
    value = ptoc_int(3);
    putc(value, fptr);
#ifdef WIN_NT
    if (io_port==2 && value=='\n') fflush(fptr); /* hack for Java interface */
#endif
    break;
  case FILE_GETBUF:
    /* file_function(8, +IOport, +ByteCount (int), -String, -BytesRead)
       Read ByteCount bytes from IOport into String starting 
       at position Offset. */
    size = ptoc_int(3);
    SET_FILEPTR(fptr, ptoc_int(2));
    XSB_StrSet(&VarBuf,"");
    XSB_StrEnsureSize(&VarBuf,size);
    value = fread(VarBuf.string, 1, size, fptr);
    VarBuf.length = value;
    XSB_StrNullTerminate(&VarBuf);
    ctop_string(4, string_find(VarBuf.string,1));
    ctop_int(5, value);
    break;
  case FILE_PUTBUF:
    /* file_function(9, +IOport, +ByteCount (int), +String, +Offset,
			-BytesWritten) */
    /* Write ByteCount bytes into IOport from String beginning with Offset in
       that string	      */
    pterm = reg_term(4);
    if (is_list(pterm))
      addr = 
	p_charlist_to_c_string(pterm,&VarBuf,"FILE_WRITE_LINE","input string");
    else if (is_string(pterm))
      addr = string_val(pterm);
    else
      xsb_abort("[FILE_PUTBUF] Output argument must be an atom or a character list");
    size = ptoc_int(3);
    offset = ptoc_int(5);
    length = strlen(addr);
    size = ( size < length - offset ? size : length - offset);
    SET_FILEPTR(fptr, ptoc_int(2));
    value = fwrite(addr+offset, 1, size, fptr);
    ctop_int(6, value);
    break;
  case FILE_READ_LINE: {
    /* Works like fgets(buf, size, stdin). Fails on reaching the end of file
    ** Invoke: file_function(FILE_READ_LINE, +File, -Str). Returns
    ** the string read.
    ** Prolog invocation: file_read_line(+File, -Str) */
    char buf[MAX_IO_BUFSIZE+1];
    int break_loop = FALSE;
    int eof=FALSE;

    SET_FILEPTR(fptr, ptoc_int(2));
    XSB_StrSet(&VarBuf,"");

    do {
      if (fgets(buf, MAX_IO_BUFSIZE, fptr) == NULL) {
	eof=TRUE;
	break;
      } else {
	XSB_StrAppend(&VarBuf,buf);
	break_loop = (buf[(strlen(buf)-1)] == '\n');
      }
    } while (!break_loop);
    
    ctop_string(3, string_find(VarBuf.string,1));
    
    /* this complex cond takes care of incomplete lines: lines that end with
       end of file and not with end-of-line. */
    if ((VarBuf.length>0) || (!eof))
      return TRUE;
    else
      return FALSE;
  }
  /* Like FILE_PUTBUF, but ByteCount=Line length. Also, takes atoms and lists
     of characters: file_function(11, +IOport, +String, +Offset) */
  case FILE_WRITE_LINE:
    pterm = reg_term(3);
    if (is_list(pterm))
      addr =
	p_charlist_to_c_string(pterm,&VarBuf,"FILE_WRITE_LINE","input string");
    else if (is_string(pterm))
      addr = string_val(pterm);
    else
      xsb_abort("[FILE_WRITE_LINE] Output arg must be an atom or a char list");
    offset = ptoc_int(4);
    size = strlen(addr)-offset;
    SET_FILEPTR(fptr, ptoc_int(2));
    fwrite(addr+offset, 1, size, fptr);
    break;

  case FILE_REOPEN: 
    /* file_function(FILE_REOPEN, +Filename,+Mode,+IOport,-ErrorCode) */
    tmpstr = ptoc_string(2);
    pterm = reg_term(3);
    if (is_int(pterm))
      mode = int_val(pterm);
    else if (is_string(pterm)) {
      switch ((string_val(pterm))[0]) {
      case 'r': mode = OREAD; break;
      case 'w': mode = OWRITE; break;
      case 'a': mode = OAPPEND; break;
      case 's':
	if ((string_val(pterm))[1] == 'r')
	  /* reading from string */
	  mode = OSTRINGR;
	else if ((string_val(pterm))[1] == 'w')
	  /* writing to string */
	  mode = OSTRINGW;
	else
	  mode = -1;
	break;
      default: mode = -1;
      }
    } else
      xsb_abort("[FILE_REOPEN] Open mode must be an atom or an integer");

    switch (mode) {
      /* "b" does nothing, but POSIX allows it */
    case OREAD:   strmode = "rb";  break; /* READ_MODE */
    case OWRITE:  strmode = "wb";  break; /* WRITE_MODE */
    case OAPPEND: strmode = "ab";  break; /* APPEND_MODE */
    case OSTRINGR:
      xsb_abort("[FILE_REOPEN] Reopening of strings hasn't been implemented");
      ctop_int(5, -1000);
      return TRUE;
    case OSTRINGW:
      xsb_abort("[FILE_REOPEN] Reopening of strings hasn't been implemented");
      ctop_int(5, -1000);
      return TRUE;
    default:
      xsb_warn("FILE_REOPEN: Invalid open file mode");
      ctop_int(5, -1000);
      return TRUE;
    }
    
    /* we reach here only if the mode is OREAD,OWRITE,OAPPEND */
    addr = expand_filename(tmpstr);
    SET_FILEPTR(fptr, ptoc_int(4));
    fflush(fptr);
    fptr = freopen(addr, strmode, fptr);

    if (fptr) {
      if (!stat(addr, &stat_buff) && !S_ISDIR(stat_buff.st_mode))
	/* file exists and isn't a dir */
	ctop_int(5, 0);
      else {
	xsb_warn("FILE_REOPEN: File %s is a directory, cannot open!", tmpstr);
	ctop_int(5, -2);
      }
    } else
      ctop_int(5, -3);

    break;

  case FILE_CLONE: {
    /* file_function(FILE_CLONE,SrcIOport,DestIOport,ErrorCode) */
    /* Note: when cloning (dup) streams, NT doesn't copy the buffering mode of
       the source file. So, if this will turn out to be a problem, a new
       builtin (interface to setvbuf) will have to be introduced. */
    FILE *src_fptr, *dest_fptr;
    int src_fd, dest_fd, dest_xsb_fileno, errcode=0;
    char *mode = NULL;
    prolog_term dest_fptr_term;

    SET_FILEPTR(src_fptr, ptoc_int(2));
    fflush(src_fptr);
    src_fd = fileno(src_fptr);

    dest_fptr_term = reg_term(3);
    if (isnonvar(dest_fptr_term)) {
      /* assume the user wants dup2-like functionality */
      SET_FILEPTR(dest_fptr, int_val(dest_fptr_term));
      dest_fd = fileno(dest_fptr);
      errcode = dup2(src_fd,dest_fd);
    } else {
      /* user wanted dup-like functionality */
      dest_fd = dup(src_fd);
      if (dest_fd >= 0) {
#ifdef WIN_NT
	/* NT doesn't have fcntl(). Brain damage? */
	mode = "r+";
#else /* Unix */ 
	int fd_flags;
	/* get the flags that open has set for this file descriptor */
	fd_flags = fcntl(dest_fd, F_GETFL) & (O_ACCMODE | O_APPEND); 
	switch (fd_flags) {
	case O_RDONLY:
	    mode = "rb";
	    break;

	case O_WRONLY:
	    mode = "wb";
	    break;

	case O_ACCMODE:
		/* Should not happen */
		/* Falls through */

	case O_RDWR:
	    mode = "rb+";
	    break;

	case O_RDONLY | O_APPEND:
	    mode = "rb";
	    break;

	case O_WRONLY | O_APPEND:
	    mode = "ab";
	    break;

	case O_ACCMODE | O_APPEND:
		/* Should not happen */
		/* Falls through */

	case O_RDWR | O_APPEND:
	    mode = "ab+";
	    break;

	default:
		mode = "rb+";
		break;
	}
#endif
	dest_fptr = fdopen(dest_fd, mode);
	if (dest_fptr) {
	  dest_xsb_fileno = xsb_intern_file(dest_fptr, "FILE_CLONE");
	  c2p_int(dest_xsb_fileno, dest_fptr_term);
	} else {
	  /* error */
	  errcode = -1;
	}
      } else
	/* error */
	errcode = -1;
    }
    ctop_int(4, errcode);

    break;
  }

  case PIPE_OPEN: { /* open_pipe(-ReadPipe, -WritePipe) */
    int pipe_fd[2];

    if (PIPE(pipe_fd) < 0) {
      ctop_int(2, PIPE_TO_PROC_FAILED);
      ctop_int(3, PIPE_FROM_PROC_FAILED);
      return TRUE;
    }
    ctop_int(2, pipe_fd[0]);
    ctop_int(3, pipe_fd[1]);
    break;
  }

  case FD2IOPORT: { /* fd2ioport(+Pipe, -IOport) */
    /* this can take any C file descriptor and make it into an XSB I/O port */
    int pipe_fd;
    char *mode=NULL;
#ifndef WIN_NT /* unix */
    int fd_flags;
#endif

    pipe_fd = ptoc_int(2); /* the C file descriptor */
#ifdef WIN_NT
    /* NT doesn't have fcntl(). Brain damage? */
    mode = "r+";
#else /* unix */
    fd_flags = fcntl(pipe_fd, F_GETFL); 
    if (fd_flags == O_RDONLY)
      mode = "rb";
    else if (fd_flags == O_WRONLY)
      mode = "wb";
    else {
      /* can't determine the mode of the C fd -- return "r+" */
      mode = "r+";
    }
#endif

    fptr = fdopen(pipe_fd, mode);

    /* xsb_intern_file will return -1, if fdopen fails */
    ctop_int(3, xsb_intern_file(fptr, "FD2IOPORT"));
    break;
  }
    
  case FILE_CLEARERR: { /* file_function(16, +IOport) */
    io_port = ptoc_int(2);
    if ((io_port < 0) && (io_port >= -MAXIOSTRS)) {
    }
    else {
      SET_FILEPTR(fptr, io_port);
      clearerr(fptr);
    }
    break;
  }

  case TMPFILE_OPEN: {
    /* file_function(17, -IOport)
       Opens a temp file in r/w mode and returns its IO port */
    if ((fptr = tmpfile()))
      ctop_int(2, xsb_intern_file(fptr, "TMPFILE_OPEN"));
    else
      ctop_int(2, -1);
    break;
  }
    
  case IS_VALID_IOPORT: {
    io_port = ptoc_int(2);
    if (io_port >= MAX_OPEN_FILES)
	return FALSE;
    if ((io_port < 0) && (io_port >= -MAXIOSTRS)) {
      /* port for reading from string */
      sfptr = strfileptr(io_port);
      if (sfptr == NULL)
	return FALSE;
      return TRUE;
    }
    if (io_port < -MAXIOSTRS)
      return FALSE;
    fptr = fileptr(io_port); \
    if ((fptr==NULL) && (io_port != 0))
	return FALSE;
    return TRUE;
  }

  default:
    xsb_abort("[FILE_FUNCTION]: Invalid file operation, %d\n", ptoc_int(1));
  }
  
  return TRUE;
}

