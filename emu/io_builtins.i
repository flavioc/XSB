/*  -*-c-*-  Make sure this file comes up in the C mode of emacs */ 
/* File:      io_builtins.i
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


static struct stat stat_buff;
#ifndef fileno				/* fileno may be a  macro */
extern int    fileno(FILE *f);	        /* this is defined in POSIX */
#endif
extern Cell   ptoc_tag(int i);
extern char   *expand_filename(char *filename);
extern char *p_charlist_to_c_string (prolog_term, char *, int, char *, char *);


static FILE *stropen(char *str)
{
  int i;
  STRFILE *tmp;

  for (i=0; i<MAXIOSTRS; i++) {
    if (iostrs[i] == NULL) break;
  }
  if (i>=MAXIOSTRS) return 0;
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


/* use stat() to get file mod time, size, and other things */
/* file_stat(+FileName, +FuncNumber, -Result)	     	   */
bool file_stat(void)
{
  int retcode = stat(ptoc_string(1), &stat_buff);
  int functor_arg3 = is_functor(reg_term(3));

  switch (ptoc_int(2)) {
  case 0:
    /* This is DSW's hack to get 32 bit time values.
       The idea is to call this builtin as file_time(File,time(T1,T2))
       where T1 represents the most significant 8 bits and T2 represents
       the least significant 24.
       ***This probably breaks 64 bit systems, so David will look into it!
       */
    if (!retcode && functor_arg3) {
      /* file exists & arg3 is a term, return 2 words*/
      c2p_int(0xFFFFFF & stat_buff.st_mtime,p2p_arg(reg_term(3),2));
      c2p_int(stat_buff.st_mtime >> 24,p2p_arg(reg_term(3),1));
    } else if (!retcode) {
      /* file exists, arg3 non-functor:  issue an error */
      xsb_warn("Arg 2 in file_time must be a term: time(X,Y)");
      ctop_int(3, (0x7FFFFFF & stat_buff.st_mtime));
    } else if (functor_arg3) {
      /* no file, and arg3 is functor: return two 0's */
      c2p_int(0, p2p_arg(reg_term(3),2));
      c2p_int(0, p2p_arg(reg_term(3),1));
    } else {
      /* no file, no functor: return 0 */
      xsb_warn("Arg 2 in file_time must be a term: time(X,Y)");
      ctop_int(3, 0);
    }
    break;
  case 1: /* Take file size in 4-byte words */
    /*** NOTE: File_size can handle only files up to 128K.
	 We must use the same trick here as we did with file_time above */
    if (!retcode)
      /* file exists */
      ctop_int(3, (0x7FFFFFF & (stat_buff.st_size >> 2)));
    else /* no file */
      ctop_int(3, 0);
    break;
  }
  return TRUE;
}

/* file_flush, file_pos, file_truncate, file_seek */
inline static bool file_function(void)
{
  static int file_des, value, size, offset, length, mode;
  static STRFILE *sfptr;
  static char buf[MAX_IO_BUFSIZE+1];
  static char *addr, *tmpstr;
  static prolog_term pterm;
  static Cell term;

  switch (ptoc_int(1)) {
  case FILE_FLUSH: /* file_function(0,+filedes,-ret,-dontcare, -dontcare) */
    /* ptoc_int(2) is file descriptor */
    SET_FILEPTR(fptr, ptoc_int(2));   
    value = fflush(fptr);
    ctop_int(3, (int) value);
    break;
  case FILE_SEEK: /* file_function(1,+filedes, +offset, +place, -ret) */
    SET_FILEPTR(fptr, ptoc_int(2));
    value = fseek(fptr, (long) ptoc_int(3), ptoc_int(4));
    ctop_int(5, (int) value);
    break;
  case FILE_TRUNCATE: /* file_function(2,+filedes,+length,-ret,-dontcare) */
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
  case FILE_POS: /* file_function(3, +filedes, -pos) */
    file_des = ptoc_int(2);  /* expand for reading from strings?? */
    term = ptoc_tag(3);
    if (file_des >= 0) {
      SET_FILEPTR(fptr, file_des);
      if (isnonvar(term)) return ptoc_int(3) == ftell(fptr);
      else ctop_int(3, ftell(fptr));
    } else { /* reading from string */
      sfptr = strfileptr(file_des);
      offset = sfptr->strptr - sfptr->strbase;
      if (isnonvar(term))
	return ptoc_int(3) == offset;
      else ctop_int(3, offset);
    }
    break;
  case FILE_OPEN:		
    /* file_function(4, +FileName, +Mode, -FileDes)
       When read, mode = 0; when write, mode = 1, 
       when append, mode = 2, when opening a 
       string for read mode = 3 */
    tmpstr = ptoc_string(2);
    mode = ptoc_int(3);
    if (mode<3) {
      addr = expand_filename(tmpstr);
      switch (mode) {
	/* "b"'s needed for DOS. -smd */
      case 0: fptr = fopen(addr, "rb"); break; /* READ_MODE */
      case 1: fptr = fopen(addr, "wb"); break; /* WRITE_MODE */
      case 2: fptr = fopen(addr, "ab"); break; /* APPEND_MODE */
      }
      if (fptr) {
	if (!stat(addr, &stat_buff) && !S_ISDIR(stat_buff.st_mode))
	  /* file exists and isn't a dir */
	  ctop_int(4, xsb_intern_file(fptr, "FILE_OPEN"));
	else {
	  xsb_warn("File %s is a directory, cannot open!", tmpstr);
	  ctop_int(4, -1);
	}
      } else ctop_int(4, -1);
    } else if (mode==3) {  /* open string! */
      if ((fptr = stropen(tmpstr))) ctop_int(4, (Integer)fptr);
      else ctop_int(4, -1000);
    } else {
      xsb_warn("Unknown open file mode");
      ctop_int(4, -1000);
    }
    break;
  case FILE_CLOSE: /* file_function(5, +FileDes) */
    file_des = ptoc_int(2);
    if (file_des < 0) strclose(file_des);
    else {
      SET_FILEPTR(fptr, file_des);
      fclose(fptr);
      open_files[file_des] = NULL;
    }
    break;
  case FILE_GET:	/* file_function(6, +FileDes, -IntVal) */
    file_des = ptoc_int(2);
    if ((file_des < 0) && (file_des >= -MAXIOSTRS)) {
      sfptr = strfileptr(file_des);
      ctop_int(3, strgetc(sfptr));
    } else {
      SET_FILEPTR(fptr, file_des);
      ctop_int(3, getc(fptr));
    }
    break;
  case FILE_PUT:   /* file_function(7, +FileDes, +IntVal) */
    /* ptoc_int(2) is file descriptor */
    file_des = ptoc_int(2);
    SET_FILEPTR(fptr, file_des);
    /* ptoc_int(3) is char to write */
    value = ptoc_int(3);
    putc(value, fptr);
#ifdef WIN_NT
    if (file_des==2 && value=='\n') fflush(fptr); /* hack for Java interface */
#endif
    break;
  case FILE_GETBUF:
    /* file_function(8, +FileDes, +ByteCount (int), -String, -BytesRead)
       Read ByteCount bytes from FileDes into String starting 
       at position Offset. Doesn't intern string.	      */
    size = ptoc_int(3);
    if (size > MAX_IO_BUFSIZE) {
      size = MAX_IO_BUFSIZE;
      xsb_warn("FILE_GETBUF: Byte count(%d) exceeds MAX_IO_BUFSIZE(%d)",
	       size, MAX_IO_BUFSIZE);
    }

    SET_FILEPTR(fptr, ptoc_int(2));
    value = fread(buf, 1, size, fptr);
    *(buf+value) = '\0';
    ctop_string(4, buf);
    ctop_int(5, value);
    break;
  case FILE_PUTBUF:
    /* file_function(9, +FileDes, +ByteCount (int), +String, +Offset,
			-BytesWritten) */
    /* Write ByteCount bytes into FileDes from String beginning with Offset in
       that string	      */
    pterm = reg_term(4);
    if (is_list(pterm))
      addr = p_charlist_to_c_string(pterm, buf, sizeof(buf),
				    "FILE_WRITE_LINE", "input string");
    else if (is_string(pterm))
      addr = string_val(pterm);
    else
      xsb_abort("FILE_PUTBUF: Output argument must be an atom or a character list");
    size = ptoc_int(3);
    offset = ptoc_int(5);
    length = strlen(addr);
    size = ( size < length - offset ? size : length - offset);
    SET_FILEPTR(fptr, ptoc_int(2));
    value = fwrite(addr+offset, 1, size, fptr);
    ctop_int(6, value);
    break;
  case FILE_READ_LINE:
    /* Works like fgets(buf, size, stdin). Fails on reaching the end of file
    ** Invoke: file_function(FILE_READ_LINE, +File, -Str, -IsFullLine). Returns
    ** the string read and an indicator (IsFullLine = 1 or 0) of whether the
    ** string read is a full line. Doesn't intern string.
    ** Prolog invocation: file_read_line(10, +File, -Str, -IsFullLine) */
    SET_FILEPTR(fptr, ptoc_int(2));
    if (fgets(buf, MAX_IO_BUFSIZE, fptr) == NULL) {
      return FALSE;
    } else {
      ctop_string(3, buf);
      if (buf[(strlen(buf)-1)] == '\n')
	ctop_int(4, 1);
      else ctop_int(4, 0);
      return TRUE;
    }
    /* Like FILE_PUTBUF, but ByteCount=Line length. Also, takes atoms and lists
       of characters: file_function(11, +FileDes, +String, +Offset) */
  case FILE_WRITE_LINE:
    pterm = reg_term(3);
    if (is_list(pterm))
      addr = p_charlist_to_c_string(pterm, buf, sizeof(buf),
				    "FILE_WRITE_LINE", "input string");
    else if (is_string(pterm))
      addr = string_val(pterm);
    else
      xsb_abort("FILE_WRITE_LINE: Output argument must be an atom or a character list");
    offset = ptoc_int(4);
    size = strlen(addr)-offset;
    SET_FILEPTR(fptr, ptoc_int(2));
    fwrite(addr+offset, 1, size, fptr);
    break;

  case FILE_REOPEN: 
    /* file_function(FILE_REOPEN, +Filename,+Mode,+FileDes,-ErrorCode) */
    tmpstr = ptoc_string(2);
    mode = ptoc_int(3);
    if (mode<3) {
      addr = expand_filename(tmpstr);
      SET_FILEPTR(fptr, ptoc_int(4));
      fflush(fptr);
      switch (mode) {
	/* "b"'s needed for DOS. -smd */
      case 0: fptr = freopen(addr, "rb", fptr); break; /* READ_MODE */
      case 1: fptr = freopen(addr, "wb", fptr); break; /* WRITE_MODE */
      case 2: fptr = freopen(addr, "ab", fptr); break; /* APPEND_MODE */
      }
      if (fptr) {
	if (!stat(addr, &stat_buff) && !S_ISDIR(stat_buff.st_mode))
	  /* file exists and isn't a dir */
	  ctop_int(5, 0);
	else {
	  xsb_warn("File %s is a directory, cannot open!", tmpstr);
	  ctop_int(5, -2);
	}
      } else ctop_int(5, -1);
    } 
    /* Whoever knows how to str-reopen a file --- please fix this. mk
    else if (mode==3) {
      if ((fptr = stropen(tmpstr))) ctop_int(5, (Integer)fptr);
      else ctop_int(5, -1000);
    } 
    */
    else {
      xsb_warn("Unknown open file mode");
      ctop_int(5, -1000);
    }
    break;

  case FILE_CLONE: {
    /* file_function(FILE_CLONE,SrcFileDes,DestFileDes,ErrorCode) */
    FILE *src_fptr, *dest_fptr;
    int src_fd, dest_fd, dest_xsb_fileno, errcode, fd_flags;
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
      errcode = dest_fd;
      if (dest_fd >= 0) {
	/* get the flags that open has set for this file descriptor */
	fd_flags = fcntl(dest_fd, F_GETFL); 
	if (fd_flags == (O_APPEND | O_WRONLY))
	  mode = "ab";
	else if (fd_flags == O_RDONLY)
	  mode = "rb";
	else if (fd_flags == O_WRONLY)
	  mode = "wb";
	else {
	  /* can't determine the r/w/a mode of dest_fd 
	     This usually happens for stdin/out/err and their clones.
	     However, the mode r+ seems to work well for them. */
	  mode = "r+";
	}
	dest_fptr = fdopen(dest_fd, mode);
	dest_xsb_fileno = xsb_intern_file(dest_fptr, "FILE_CLONE");
	c2p_int(dest_xsb_fileno, dest_fptr_term);
      }
    }
    ctop_int(4, errcode);

    break;
  }
    
  default:
    xsb_abort("Invalid file function request %d\n", ptoc_int(1));
  }
  
  return TRUE;
}

