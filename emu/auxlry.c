/* File:      auxlry.c
** Author(s): Warren, Sagonas, Xu
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


#include "configs/config.h"

#include <stdio.h>

#if defined(WIN_NT)
#include <time.h>
#else
#include <sys/time.h>
#include <sys/resource.h>
#ifdef SOLARIS
/*--- Include the following to bypass header file inconcistencies ---*/
extern int getrusage();
extern int gettimeofday();
#endif
#endif

#if (defined(HP300) || defined(HP700))
#include <sys/syscall.h>
#define getrusage(T, USAGE)	syscall(SYS_getrusage, T, USAGE);
#endif

/*----------------------------------------------------------------------*/

double cpu_time(void)
{
  float time_sec;

#if defined(WIN_NT)

  time_sec = ((float) clock() / CLOCKS_PER_SEC);

#else
  struct rusage usage;

  getrusage(RUSAGE_SELF, &usage);
  time_sec = (float)usage.ru_utime.tv_sec +
	     (float)usage.ru_utime.tv_usec / 1000000.0;
#endif

  return time_sec;
}

/*----------------------------------------------------------------------*/

/** Note: this one is not used anywhere, but it would make a good builtin for
    some applications!!! --mk */
#ifdef HAVE_GETTIMEOFDAY
int get_date(void)
{
  struct tm *value;
  struct timeval tvs;

  gettimeofday(&tvs, 0);
  value = localtime(&(tvs.tv_sec));
  return ((value->tm_year)<<16) + ((value->tm_mon+1)<<8) + value->tm_mday;
}
#endif

/*----------------------------------------------------------------------*/

double real_time(void)
{
#if defined(WIN_NT)
  double value = ((float) clock() / CLOCKS_PER_SEC);
#else
  double value;
  struct timeval tvs;

  gettimeofday(&tvs, 0);
  value = tvs.tv_sec + 0.000001 * tvs.tv_usec;
#endif
  return value;
}

/*----------------------------------------------------------------------*/
