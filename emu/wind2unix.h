/* File:      wind2unix.h -- some definitions for Unix/Windows compatibility
** Author(s): kifer
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1998
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


/* This stuff isn't defined in NT */
#ifndef S_ISDIR
#define S_ISDIR(mode) ((mode & S_IFMT) == S_IFDIR)
#endif
#ifndef S_ISREG
#define S_ISREG(mode) ((mode & S_IFMT) == S_IFREG)
#endif

#ifndef R_OK
#define R_OK_XSB 4
#else
#define R_OK_XSB R_OK
#endif
#ifndef W_OK
#define W_OK_XSB 2
#else
#define W_OK_XSB W_OK
#endif
/* On NT this just tests for existence rather than execution */
#ifndef X_OK
#define X_OK_XSB 0
#else
#define X_OK_XSB X_OK
#endif

#ifdef WIN_NT
#define snprintf   _snprintf
#define fdopen     _fdopen
#define popen      _popen
#define pclose     _pclose
#define dup        _dup
#define putenv     _putenv
#define dup2       _dup2
#define fileno     _fileno
#define unlink     _unlink
#define strcasecmp _stricmp
#define access     _access
#endif

/* The separator used between pathnames in PATH environment */
#ifdef WIN_NT
#define PATH_SEPARATOR ';'
#else
#define PATH_SEPARATOR ':'
#endif
