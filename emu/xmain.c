/* File:      xmain.c
** Author(s): Warren
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1993-1998
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
#include "basictypes.h"


#ifdef WIN_NT
#include <direct.h>
#include <io.h>
#else
#include <unistd.h>
#endif

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
/* special.h must be included after sys/stat.h */
#include "configs/special.h"

#include "emuloop.h"

#ifdef HAVE_SOCKET
#ifdef WIN_NT
#include <windows.h>
#include <winsock.h>
#endif
#endif

extern void xsb_executable_full_path(char *);
extern void set_xsbinfo_dir (void);
extern void set_install_dir(void);
extern void set_config_file(void);
extern void set_user_home(void);

int main(int argc, char *argv[])
{ 
#ifdef HAVE_SOCKET
#ifdef WIN_NT
  INT err;
  WSADATA wsaData;
  FILE *stream_err, *stream_out;
#ifdef SILENT_NT
  stream_err = freopen("errorlog", "w", stderr);
  stream_out = freopen("outlog", "w", stdout);
#endif
  err = WSAStartup(0x0101, &wsaData);
  if (err == SOCKET_ERROR) {
    fprintf (stdout, "WSAStartup Failed\n");
    return FALSE;
  }
#endif
#endif

  /* set the name of the executable to the real name */
  xsb_executable_full_path(argv[0]);

  /* set install_dir, xsb_config_file, and user_home */
  set_install_dir();
  set_config_file();
  set_user_home();

  xsb(0, argc, argv);  /* init xsb */

  /* do it after initialization, so that typing
     xsb -v or xsb -h won't create .xsb directory */
  set_xsbinfo_dir ();

  xsb(1, 0, 0);        /* normal execution */
  xsb(2, 0, 0);        /* when halts, exit */
  return 0;
}

