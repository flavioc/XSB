/* File:      io_builtins.h
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



/* OP numbers for file_function */
#define FILE_FLUSH         0
#define FILE_SEEK          1
#define FILE_TRUNCATE      2
#define FILE_POS      	   3
#define FILE_OPEN      	   4
#define FILE_CLOSE     	   5
#define FILE_GET     	   6
#define FILE_PUT     	   7
#define FILE_GETBUF    	   8
#define FILE_PUTBUF    	   9
#define FILE_READ_LINE 	   10

/* OP numbers for formatted_io */
#define FMT_WRITE    	   1
#define FMT_WRITE_STRING   2
#define FMT_READ       	   3


/* OP numbers for file_stat */
#define FILE_STAT_TIME	  0
#define FILE_STAT_SIZE	  1

