/* File:      auxlry.h
** Author(s): kifer
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
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


#ifndef bool
#define bool  int
#endif

#ifndef FALSE
#define FALSE  0
#endif
#ifndef TRUE
#define TRUE  (!FALSE)
#endif


#ifdef WIN_NT
#define SLASH '\\'
#else
#define SLASH '/'
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN   1024  /* SYSV */
#endif

#ifndef MAXNAME
#define MAXNAME   255  /* SYSV */
#endif

#ifndef MAXBUFSIZE
#define MAXBUFSIZE   1024  /* used when a large string buffer is needed */
#endif

#define K   1024
