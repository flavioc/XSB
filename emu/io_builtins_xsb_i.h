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

#if (defined(CYGWIN))
#include <fcntl.h>
#endif

#ifdef WIN_NT
#include <io.h>
#endif

static struct stat stat_buff;
extern char   *expand_filename(char *filename);
extern int xsb_intern_fileptr(FILE *, char *, char *, char *);

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

/* TLS: these are ports, rather than file descriptors, therefore using
   the Prolog defines.  Should they be moved into a different .h file? 
*/

#define STDIN 0
#define STDOUT 1

/* file_flush, file_pos, file_truncate, file_seek */
inline static xsbBool file_function(CTXTdecl)
{
  static FILE *fptr;
  static int io_port, value, size, offset, length, mode;
  static STRFILE *sfptr;
  static XSB_StrDefine(VarBuf);
  static char *addr, *tmpstr;
  static prolog_term pterm;
  static Cell term;
  static char *strmode;
  static char *line_buff = NULL;
  static int line_buff_len = 0;
  int line_buff_disp, ioport;

  switch (ptoc_int(CTXTc 1)) {
  case FILE_FLUSH: /* file_function(0,+IOport,-Ret,_,_) */
    /* ptoc_int(CTXTc 2) is XSB I/O port */
    SET_FILEPTR(fptr, ptoc_int(CTXTc 2));   
    value = fflush(fptr);
    ctop_int(CTXTc 3, (int) value);
    break;
  case FILE_SEEK: /* file_function(1,+IOport, +Offset, +Place, -Ret) */
    io_port = ptoc_int(CTXTc 2);
    if (io_port < 0) {
      if (ptoc_int(CTXTc 4) != 0) 
	xsb_permission_error(CTXTc "file_seek","atom",ptoc_int(CTXTc 4),"file_seek",1); 
      sfptr = iostrs[iostrdecode(io_port)];
      value = ptoc_int(CTXTc 3);
      length = sfptr->strcnt + sfptr->strptr - sfptr->strbase ;
      if (value <= length) {
	if (sfptr->strcnt == -1) length++;
	sfptr->strptr = sfptr->strbase + value;
	sfptr->strcnt = length - value;
	ctop_int(CTXTc 5, 0);
	/*	printf("base %x cur %x cnt %d\n",
		sfptr->strbase,sfptr->strptr,sfptr->strcnt);*/
      }
      else ctop_int(CTXTc 5,-1);
    }
    else {
      SET_FILEPTR(fptr, io_port);
      value = fseek(fptr, (long) ptoc_int(CTXTc 3), ptoc_int(CTXTc 4));
      ctop_int(CTXTc 5, (int) value);
    }
    break;
  case FILE_TRUNCATE: /* file_function(2,+IOport,+Length,-Ret,_) */
    size = ptoc_int(CTXTc 3);
    SET_FILEPTR(fptr, ptoc_int(CTXTc 2));
#ifndef WIN_NT
    fseek(fptr, (long) size, 0);
    value = ftruncate( fileno(fptr), (off_t) size);
    ctop_int(CTXTc 4, (int) value);
#else
    //    xsb_warn("FILE_TRUNCATE: operation not supported under Windows.");
    ctop_int(CTXTc 4, (int) _chsize(fileno(fptr), size));
#endif
    break;
  case FILE_POS: /* file_function(3, +IOport, -Pos) */
    io_port = ptoc_int(CTXTc 2); 
    term = ptoc_tag(CTXTc 3);
    if (io_port >= 0) {
      SET_FILEPTR(fptr, io_port);
      if (isnonvar(term))
	return ptoc_int(CTXTc 3) == ftell(fptr);
      else
	ctop_int(CTXTc 3, ftell(fptr));
    } else { /* reading from string */
      sfptr = strfileptr(io_port);
      if (sfptr->strcnt == EOF) 
	offset = EOF;
      else 
	offset = sfptr->strptr - sfptr->strbase;
      if (isnonvar(term))
	return ptoc_int(CTXTc 3) == offset;
      else
	ctop_int(CTXTc 3, offset);
    }
    break;
  case XSB_FILE_OPEN: {
    /* file_function(4, +FileName, +Mode, -IOport) TLS: changing modes
     and differentiating binaries, so its best to not allow integer
     modes any more */

    int str_type = 0;
    char string_mode[3];

    tmpstr = ptoc_longstring(CTXTc 2);
    pterm = reg_term(CTXTc 3);

    if (isstring(pterm)) {
      strcpy(string_mode,string_val(pterm));

      switch ((string_mode)[0]) {
      case 'r': 
	mode = OREAD; 
	if ((string_mode)[1] == 'b')
	  str_type = BINARY_FILE_STREAM;
	else  str_type = TEXT_FILE_STREAM;
	break;
      case 'w': 
	mode = OWRITE; 
	if ((string_mode)[1] == 'b')
	  str_type = BINARY_FILE_STREAM;
	else  str_type = TEXT_FILE_STREAM;
	break;
      case 'a': 
	mode = OAPPEND; 
	if ((string_mode)[1] == 'b')
	  str_type = BINARY_FILE_STREAM;
	else  str_type = TEXT_FILE_STREAM;
	break;
      case 's':
	str_type = STRING_STREAM;
	if ((string_mode)[1] == 'r')
	  /* reading from string */
	  mode = OSTRINGR;
	else if ((string_mode)[1] == 'w')
	  /* writing to string */
	  mode = OSTRINGW;
	else
	  mode = -1;
	break;
      default: mode = -1;
      }
    } else
      xsb_abort("[FILE_OPEN] File opening mode must be an atom.");

    switch (mode) {

      /* In UNIX the 'b" does nothing, but in Windows it
	 differentiates a binary from a text file.  If I take the 'b'
	 out, this breaks the compiler. */

    case OREAD: strmode = "rb"; break; /* READ_MODE */
    case OWRITE:  strmode = "wb"; break; /* WRITE_MODE */
    case OAPPEND: strmode = "ab"; break; /* APPEND_MODE */
    case OSTRINGR:
      if ((fptr = stropen(tmpstr)))
	ctop_int(CTXTc 4, (Integer)fptr);
      else 
	ctop_int(CTXTc 4, -1000);
      return TRUE;
    case OSTRINGW:
      xsb_abort("[FILE_OPEN] Output to strings has not been implemented yet");
      ctop_int(CTXTc 4, -1000);
      return TRUE;
    default:
      xsb_warn("FILE_OPEN: Invalid open file mode");
      ctop_int(CTXTc 4, -1000);
      return TRUE;
    }
    
    /* we reach here only if the mode is OREAD,OWRITE,OAPPEND */
    addr = expand_filename(tmpstr);

    /*    printf("xsb_intern_file addr %s,string_mode %s\n",addr,string_mode); */


    if (!xsb_intern_file("FILE_OPEN",addr, &ioport,strmode)) {
      open_files[ioport].stream_type = str_type;
      ctop_int(CTXTc 4,ioport);
    }
    else ctop_int(CTXTc 4,-1);

    break;
  }
    /* TLS: handling the case in which we are closing a flag that
       we're currently seeing or telling.  Probably bad programming
       style to mix streams w. open/close, though. */
  case FILE_CLOSE: /* file_function(5, +Stream,FORCE/NOFORCE) */
    {
      int rtrn; 
      io_port = ptoc_int(CTXTc 2);
      if (io_port < 0) strclose(io_port);
      else {
	SET_FILEPTR(fptr, io_port);
	if ((rtrn = fclose(fptr))) {
	  if (ptoc_int(CTXTc 3) == NOFORCE_FILE_CLOSE)
	    {xsb_permission_error(CTXTc "fclose","file",rtrn,"file_close",1); }
	}
	open_files[io_port].file_ptr = NULL;
	open_files[io_port].file_name = NULL;
	open_files[io_port].io_mode = '\0';
	open_files[io_port].stream_type = 0;
	if (flags[CURRENT_INPUT] == io_port) 
	  { flags[CURRENT_INPUT] = STDIN;}
	if (flags[CURRENT_OUTPUT] == io_port) 
	  { flags[CURRENT_OUTPUT] = STDOUT;}
      }
    break;
    }
  case FILE_GET:	/* file_function(6, +IOport, -IntVal) */
    io_port = ptoc_int(CTXTc 2);
    if ((io_port < 0) && (io_port >= -MAXIOSTRS)) {
      sfptr = strfileptr(io_port);
      ctop_int(CTXTc 3, strgetc(sfptr));
    } else {
      SET_FILEPTR(fptr, io_port);
      ctop_int(CTXTc 3, getc(fptr));
    }
    break;
  case FILE_PUT:   /* file_function(7, +IOport, +IntVal) */
    /* ptoc_int(CTXTc 2) is XSB I/O port */
    io_port = ptoc_int(CTXTc 2);
    SET_FILEPTR(fptr, io_port);
    /* ptoc_int(CTXTc 3) is char to write */
    value = ptoc_int(CTXTc 3);
    putc(value, fptr);
#ifdef WIN_NT
    if (io_port==2 && value=='\n') fflush(fptr); /* hack for Java interface */
#endif
    break;
  case FILE_GETBUF:
    /* file_function(8, +IOport, +ByteCount (int), -String, -BytesRead)
       Read ByteCount bytes from IOport into String starting 
       at position Offset. */
    size = ptoc_int(CTXTc 3);
    SET_FILEPTR(fptr, ptoc_int(CTXTc 2));
    XSB_StrSet(&VarBuf,"");
    XSB_StrEnsureSize(&VarBuf,size);
    value = fread(VarBuf.string, 1, size, fptr);
    VarBuf.length = value;
    XSB_StrNullTerminate(&VarBuf);
    ctop_string(CTXTc 4, string_find(VarBuf.string,1));
    ctop_int(CTXTc 5, value);
    break;
  case FILE_PUTBUF:
    /* file_function(9, +IOport, +ByteCount (int), +String, +Offset,
			-BytesWritten) */
    /* Write ByteCount bytes into IOport from String beginning with Offset in
       that string	      */
    pterm = reg_term(CTXTc 4);
    if (islist(pterm))
      addr = 
	p_charlist_to_c_string(CTXTc pterm,&VarBuf,"FILE_WRITE_LINE","input string");
    else if (isstring(pterm))
      addr = string_val(pterm);
    else
      xsb_abort("[FILE_PUTBUF] Output argument must be an atom or a character list");
    size = ptoc_int(CTXTc 3);
    offset = ptoc_int(CTXTc 5);
    length = strlen(addr);
    size = ( size < length - offset ? size : length - offset);
    SET_FILEPTR(fptr, ptoc_int(CTXTc 2));
    value = fwrite(addr+offset, 1, size, fptr);
    ctop_int(CTXTc 6, value);
    break;
  case FILE_READ_LINE: {
    /* Works like fgets(buf, size, stdin). Fails on reaching the end of file
    ** Invoke: file_function(FILE_READ_LINE, +File, -Str). Returns
    ** the string read.
    ** Prolog invocation: file_read_line(+File, -Str) */
    char buf[MAX_IO_BUFSIZE+1];
    int break_loop = FALSE;
    int eof=FALSE;

    SET_FILEPTR(fptr, ptoc_int(CTXTc 2));
    XSB_StrSet(&VarBuf,"");

    do {
      if (fgets(buf, MAX_IO_BUFSIZE, fptr) == NULL && feof(fptr)) {
	eof=TRUE;
	break;
      } else {
	XSB_StrAppend(&VarBuf,buf);
	break_loop = (buf[(strlen(buf)-1)] == '\n');
      }
    } while (!break_loop);
    
    ctop_string(CTXTc 3, string_find(VarBuf.string,1));
    
    /* this complex cond takes care of incomplete lines: lines that end with
       end of file and not with end-of-line. */
    if ((VarBuf.length>0) || (!eof))
      return TRUE;
    else
      return FALSE;
  }
  case FILE_READ_LINE_LIST: {
    /* Works like FILE_READ_LINE but returns a list of codes
    ** Invoke: file_function(FILE_READ_LINE, +File, -List). Returns
    ** the list of codes read. Rewritten by DSW 5/18/04 to allow \0 in lines.
    ** Prolog invocation: file_read_line_list(+File, -Str) */
    char *atomname;
    char c;
    Cell new_list;
    CPtr top = NULL;
    int i;

    SET_FILEPTR(fptr, ptoc_int(CTXTc 2));

    line_buff_disp = 0;
    do {
      if (line_buff_disp >= line_buff_len) {
	line_buff_len = line_buff_disp+MAX_IO_BUFSIZE;
	line_buff = realloc(line_buff,line_buff_len);
      }
      *(line_buff+line_buff_disp) = c = getc(fptr);
      if (c == EOF) break;
      line_buff_disp++;
    } while (c != '\n');
    *(line_buff+line_buff_disp) = 0;
    
    check_glstack_overflow(3, pcreg, 2*sizeof(Cell)*line_buff_disp);
    atomname = line_buff;

    if (line_buff_disp == 0) new_list = makenil;
    else {
      new_list = makelist(hreg);
      for (i = 0; i < line_buff_disp; i++) {
	follow(hreg++) = makeint(*(unsigned char *)atomname);
	atomname++;
	top = hreg++;
	follow(top) = makelist(hreg);
      }
      follow(top) = makenil;
    }

    ctop_tag(CTXTc 3, new_list);
    
    /* this complex cond takes care of incomplete lines: lines that end with
       end of file and not with end-of-line. */
    //    if ((line_buff_disp>0) || (c != EOF))
    if (line_buff_disp>0)
      return TRUE;
    else
      return FALSE;
  }
  /* Like FILE_PUTBUF, but ByteCount=Line length. Also, takes atoms and lists
     of characters: file_function(11, +IOport, +String, +Offset) */
  case FILE_WRITE_LINE:
    pterm = reg_term(CTXTc 3);
    if (islist(pterm))
      addr =
	p_charlist_to_c_string(CTXTc pterm,&VarBuf,"FILE_WRITE_LINE","input string");
    else if (isstring(pterm))
      addr = string_val(pterm);
    else
      xsb_abort("[FILE_WRITE_LINE] Output arg must be an atom or a char list");
    offset = ptoc_int(CTXTc 4);
    size = strlen(addr)-offset;
    SET_FILEPTR(fptr, ptoc_int(CTXTc 2));
    fwrite(addr+offset, 1, size, fptr);
    break;

  case FILE_REOPEN: 
    /* file_function(FILE_REOPEN, +Filename,+Mode,+IOport,-ErrorCode) */
    tmpstr = ptoc_string(CTXTc 2);
    pterm = reg_term(CTXTc 3);
    if (isinteger(pterm)|isboxedinteger(pterm))
      mode = oint_val(pterm);
    else if (isstring(pterm)) {
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
      ctop_int(CTXTc 5, -1000);
      return TRUE;
    case OSTRINGW:
      xsb_abort("[FILE_REOPEN] Reopening of strings hasn't been implemented");
      ctop_int(CTXTc 5, -1000);
      return TRUE;
    default:
      xsb_warn("FILE_REOPEN: Invalid open file mode");
      ctop_int(CTXTc 5, -1000);
      return TRUE;
    }
    
    /* we reach here only if the mode is OREAD,OWRITE,OAPPEND */
    addr = expand_filename(tmpstr);
    SET_FILEPTR(fptr, ptoc_int(CTXTc 4));
    fflush(fptr);
    fptr = freopen(addr, string_val(pterm), fptr);

    if (fptr) {
      if (!stat(addr, &stat_buff) && !S_ISDIR(stat_buff.st_mode))
	/* file exists and isn't a dir */
	ctop_int(CTXTc 5, 0);
      else {
	xsb_warn("FILE_REOPEN: File %s is a directory, cannot open!", tmpstr);
	ctop_int(CTXTc 5, -2);
      }
    } else
      ctop_int(CTXTc 5, -3);

    break;

    /* TLS: I looked through this, and it seems to work with streams,
       but its possible that the file clone should move the file name
       and mode from the source to the destination when it copies or
       creates an io port? */

  case FILE_CLONE: {
    /* file_function(FILE_CLONE,SrcIOport,DestIOport,ErrorCode) */
    /* Note: when cloning (dup) streams, NT doesn't copy the buffering mode of
       the source file. So, if this will turn out to be a problem, a new
       builtin (interface to setvbuf) will have to be introduced. */
    FILE *src_fptr, *dest_fptr;
    int src_fd, dest_fd, dest_xsb_fileno, src_xsb_fileno, errcode=0;
    char *mode = NULL;
    prolog_term dest_fptr_term;

    src_xsb_fileno = ptoc_int(CTXTc 2);
    SET_FILEPTR(src_fptr, src_xsb_fileno);
    fflush(src_fptr);
    src_fd = fileno(src_fptr);

    dest_fptr_term = reg_term(CTXTc 3);
    if (isnonvar(dest_fptr_term)) {
      /* assume the user wants dup2-like functionality */
      SET_FILEPTR(dest_fptr, int_val(dest_fptr_term));
      dest_fd = fileno(dest_fptr);
      errcode = dup2(src_fd,dest_fd);
    } else {
      /* user wanted dup-like functionality */
      dest_fd = dup(src_fd);
      if (dest_fd >= 0) {
#if (defined (WIN_NT) && ! defined(CYGWIN))
	/* NT doesn't have fcntl(). Brain damage? But Cygwin does */
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
	  dest_xsb_fileno = 
	    xsb_intern_fileptr(dest_fptr,"FILE_CLONE",
			       open_files[src_xsb_fileno].file_name,
			       &open_files[src_xsb_fileno].io_mode);
	  c2p_int(CTXTc dest_xsb_fileno, dest_fptr_term);
	} else {
	  /* error */
	  errcode = -1;
	}
      } else
	/* error */
	errcode = -1;
    }
    ctop_int(CTXTc 4, errcode);

    break;
  }

  case PIPE_OPEN: { /* open_pipe(-ReadPipe, -WritePipe) */
    int pipe_fd[2];

    if (PIPE(pipe_fd) < 0) {
      ctop_int(CTXTc 2, PIPE_TO_PROC_FAILED);
      ctop_int(CTXTc 3, PIPE_FROM_PROC_FAILED);
      return TRUE;
    }
    ctop_int(CTXTc 2, pipe_fd[0]);
    ctop_int(CTXTc 3, pipe_fd[1]);
    break;
  }

  case FD2IOPORT: { /* fd2ioport(+Pipe, -IOport,+Mode) */
    /* this can take any C file descriptor and make it into an XSB I/O port.
        For backward compatability,mode may not be used -- where it is "u" */
    int pipe_fd, i;
    char *mode=NULL;
#ifndef WIN_NT /* unix */
    int fd_flags;
#endif
    pipe_fd = ptoc_int(CTXTc 2); /* the C file descriptor */
    pterm = reg_term(CTXTc 4);

    if (isstring(pterm)) {
      if ((string_val(pterm))[0] == 'u') {
	/* Need to try to find mode */
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
      } 
      else mode = string_val(pterm);
    }
    else xsb_abort("[FD2IOPORT] Opening mode must be an atom.");

    fptr = fdopen(pipe_fd, mode);

    /* xsb_intern_file will return -1, if fdopen fails */
    i = xsb_intern_fileptr(fptr, "FD2IOPORT","created from fd",mode);
    ctop_int(CTXTc 3, i);
    open_files[i].stream_type = PIPE_STREAM;
    break;
  }
    
  case FILE_CLEARERR: { /* file_function(16, +IOport) */
    io_port = ptoc_int(CTXTc 2);
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
      ctop_int(CTXTc 2, xsb_intern_fileptr(fptr, "TMPFILE_OPEN",
					 "TMPFILE","wb+"));
    else
      ctop_int(CTXTc 2, -1);
    break;
  }
    
  case STREAM_PROPERTY: {
    int stream;
    stream = ptoc_int(CTXTc 2);
    switch (ptoc_int(CTXTc 3)) {

      /* Type, Repos, eof_actions are all currently functions of class */
    case STREAM_EOF_ACTION:
    case STREAM_REPOSITIONABLE:
    case STREAM_TYPE: 
    case STREAM_CLASS: 
      ctop_int(CTXTc 4, open_files[stream].stream_type);
      break;
    
    case STREAM_FILE_NAME:  
      if (open_files[stream].stream_type < 3)
	ctop_string(CTXTc 4, open_files[stream].file_name);
      break;

    case STREAM_MODE: 
    case STREAM_INPUT: 
    case STREAM_OUTPUT: {

      mode = open_files[stream].io_mode; 
      if (mode == 'r' || mode == 's') {
	ctop_int(CTXTc 4,READ_MODE);
      } else if (mode == 'w' || mode == 'x') {
	ctop_int(CTXTc 4,WRITE_MODE);
      } else if (mode == 'a' || mode == 'b') {
	ctop_int(CTXTc 4,APPEND_MODE);
      }
      break;
    }
    }
    break;
  }

  case IS_VALID_STREAM: {
    int stream;
    char iomode;

    stream = ptoc_int(CTXTc 2);
    if (stream >= MAX_OPEN_FILES)
	return FALSE;
    if ((stream < 0) && (stream >= -MAXIOSTRS)) {
      /* port for reading from string */
      sfptr = strfileptr(stream);
      if (sfptr == NULL)
	return FALSE;
      else {
	ctop_int(CTXTc 3,READ_MODE);
	return TRUE;
      }
    }
    if (stream < -MAXIOSTRS)
      return FALSE;
    fptr = fileptr(stream); \
    if ((fptr==NULL) && (stream != 0))
	return FALSE;
    else {
	iomode = open_files[stream].io_mode; 
	if (iomode == 'r' || iomode == 's') {
	  ctop_int(CTXTc 3,READ_MODE);
	} else ctop_int(CTXTc 3,WRITE_MODE);
	return TRUE;
      }  
  }

  case PRINT_OPENFILES: { /* no args */
    int i; 
    for (i= 0 ; i < MAX_OPEN_FILES ; i++) {
      if (open_files[i].file_name == NULL) {
 	printf("i: %d File Ptr %p Mode %c Type %d \n",
 	        i,open_files[i].file_ptr,open_files[i].io_mode,
	        open_files[i].stream_type);
      } else {
	printf("i; %d File Ptr %p Name %s Mode %c Type %d\n",i,
	       open_files[i].file_ptr, open_files[i].file_name,open_files[i].io_mode,
	       open_files[i].stream_type);
      }
    }
    break;
  }

    /* TLS: range checking for streams done by is_valid_stream */
  case FILE_END_OF_FILE: {

    io_port = ptoc_int(CTXTc 2);
    if (io_port < 0) {
      sfptr = strfileptr(io_port);
      return  (sfptr->strcnt == EOF);
    }
    else {
      return (feof(open_files[ptoc_int(CTXTc 2)].file_ptr) != 0);
    }
  }

  case FILE_PEEK: {
    int bufchar;

    io_port = ptoc_int(CTXTc 2);
    if ((io_port < 0) && (io_port >= -MAXIOSTRS)) {
      sfptr = strfileptr(io_port);
      ctop_int(CTXTc 3, strgetc(sfptr));
    } else {
      SET_FILEPTR(fptr, io_port);
      bufchar = getc(fptr);
      ctop_int(CTXTc 3, bufchar);
      if (bufchar >= 0) 
	ungetc(bufchar, fptr);
    }
    break;
  }

  default:
    xsb_abort("[FILE_FUNCTION]: Invalid file operation, %d\n", ptoc_int(CTXTc 1));
  }
  
  return TRUE;
}

