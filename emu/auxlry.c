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


#include "xsb_config.h"

#include <stdio.h>

/* take care of the time.h problems */
#include "xsb_time.h"

#ifndef WIN_NT
#include <sys/resource.h>

#ifdef SOLARIS
/*--- Include the following to bypass header file inconcistencies ---*/
extern int getrusage();
extern int gettimeofday();
#endif

#ifdef HP700
#include <sys/syscall.h>
extern int syscall();
#define getrusage(T, USAGE)	syscall(SYS_getrusage, T, USAGE);
#endif

#endif

#ifdef WIN_NT
#include "windows.h"
#endif

/*----------------------------------------------------------------------*/

double cpu_time(void)
{
  float time_sec;

#if defined(WIN_NT)
#if 0 /* code specific for win nt, 2000 & xp */
      /* needs testing --lfcastro */
  HANDLE thisproc;
  FILETIME creation, exit, kernel, user;
  long long lkernel, luser;

  thisproc = GetCurrentProcess();
  GetProcessTimes(thisproc,&creation,&exit,&kernel,&user);
  /* unfinished -- how to convert kernel+user (two 64-bit unsigned
     integers) into an appropriate float?              --lfcastro */
  /* the code below assumes sizeof(long long) == 8 */
  lkernel = (kernel.dwHighDateTime << 32) + kernel.dwLowDateTime;
  luser = (kernel.dwHighDateTime << 32) + kernel.dwLowDateTime;
  luser += lkernel;

  time_sec = luser / 10000.0;

#else /* this code is for Win98 */

  time_sec = ((float) clock() / CLOCKS_PER_SEC);
#endif

#else
  struct rusage usage;

  getrusage(RUSAGE_SELF, &usage);
  time_sec = (float)usage.ru_utime.tv_sec +
	     (float)usage.ru_utime.tv_usec / 1000000.0;
#endif

  return time_sec;
}

/*----------------------------------------------------------------------*/

int get_date(int *year, int *month, int *day,
	     int *hour, int *minute)
{
#ifdef WIN_NT
    SYSTEMTIME SystemTime;
    TIME_ZONE_INFORMATION tz;
    GetLocalTime(&SystemTime);
    *year = SystemTime.wYear;
    *month = SystemTime.wMonth;
    *day = SystemTime.wDay;
    *hour = SystemTime.wHour;
    *minute = SystemTime.wMinute;
    GetTimeZoneInformation(&tz);
    *hour = *hour + tz.Bias/60;
    *minute = *minute + tz.Bias % 60;
#else
#ifdef HAVE_GETTIMEOFDAY
    struct timeval tv;
    struct tm *tm;

    gettimeofday(&tv,NULL);
    tm = gmtime(&tv.tv_sec);
    *year = tm->tm_year;
    if (*year < 1900)
      *year += 1900;
    *month = tm->tm_mon + 1;
    *day = tm->tm_mday;
    *hour = tm->tm_hour;
    *minute = tm->tm_min;
#endif
#endif
    return;
}

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
