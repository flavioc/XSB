/* File:      io_defs_xsb.h
** Author(s): kifer
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

/* TLS: redefined FILE_OPEN to XSB_FILE_OPEN to avoid conflict with
   win32api/windef.h */

/* OP numbers for file_function */
#define FILE_FLUSH         0
#define FILE_SEEK          1
#define FILE_TRUNCATE      2
#define FILE_POS      	   3
#define XSB_FILE_OPEN      	   4
#define FILE_CLOSE     	   5
#define FILE_GET     	   6
#define FILE_PUT     	   7
#define FILE_GETBUF    	   8
#define FILE_PUTBUF    	   9
#define FILE_READ_LINE 	   10
#define FILE_WRITE_LINE	   11
#define FILE_REOPEN    	   12
#define FILE_CLONE     	   13
#define PIPE_OPEN     	   14
#define FD2IOPORT   	   15
#define FILE_CLEARERR      16
#define TMPFILE_OPEN       17
#define IS_VALID_STREAM    18
#define FILE_READ_LINE_LIST 19
#define STREAM_PROPERTY 20
#define PRINT_OPENFILES 21
#define FILE_END_OF_FILE 22
#define FILE_PEEK 23

/* This sequence is for stream properties */
#define STREAM_FILE_NAME              0
#define STREAM_MODE                      1
#define STREAM_INPUT                       2
#define STREAM_OUTPUT                   3
#define STREAM_POSITION                 4
#define STREAM_END_OF_STREAM    5
#define STREAM_REPOSITIONABLE    6

/* Need in here for stream_property/2 */
#define MAX_OPEN_FILES    55
#define MIN_USR_OPEN_FILE 7     /* Where user files start in the XSB
				   open files table */

/* OP numbers for formatted_io */
#define FMT_WRITE    	   1
#define FMT_WRITE_STRING   2
#define FMT_READ       	   3

#define READ_MODE 0
#define WRITE_MODE 1
#define APPEND_MODE 2

#define FORCE_FILE_CLOSE     0 
#define NOFORCE_FILE_CLOSE     1

/* This list may eventually include sockets, urls, etc.
   Starting with 1 to preserve 0 as undef.  */

#define TEXT_FILE_STREAM            1
#define BINARY_FILE_STREAM         2
#define STRING_STREAM                3
#define PIPE_STREAM                    4
#define CONSOLE_STREAM            5
