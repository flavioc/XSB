/*  -*-c-*-  Make sure this file comes up in the C mode of emacs */ 
/* File:      dynwin32.i
** Author(s): Luis Castro
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
** Copyright (C) ECRC, Germany, 1990
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


#include <windows.h>
#include <sys/types.h>
#include <sys/stat.h>
/* special.h must be included after sys/stat.h */
#include "configs/special.h"
#include <errno.h>
#include <stdio.h>

#include "auxlry.h"
#include "cell.h"
#include "memory.h"
#include "inst.h"
#include "psc.h"

#define BUFFEXTRA 1024

/*----------------------------------------------------------------------*/

bool dummy()
{
    fprintf(stderr, "++Error: trying to use an undefined foreign procedure\n");
    return FALSE;
}

/*----------------------------------------------------------------------*/

static byte *load_obj_dyn(char *pofilename, Psc cur_mod, char *ld_option)
{
  char	*name;
#ifdef XSB_DLL
  char tempname[128];
  int  tempsize;
#endif
  Pair	search_ptr;
  char	sofilename[128];
  int 	strl = strlen(pofilename);
  HMODULE handle;
  void	*funcep;
  bool	dummy();
  
  /* (1) create filename.so */
  
  strcpy(sofilename, pofilename);
  sofilename[strl-1] = 'd';
  sofilename[strl]   = 'l';
  sofilename[strl+1] = 'l';
  sofilename[strl+2] = '\0';
  
  /* (2) open the needed object */
  
  if (( handle = LoadLibrary(sofilename)) == 0 ) {
    fprintf(stderr, "Cannot load library %s\n",sofilename);
    return 0;
  }
  
  /* (3) find address of function and data objects
  **
  ** dyn_link_all(loc, cur_mod);
  */
  
  search_ptr = (Pair)get_ep(cur_mod);
  
  while (search_ptr) {
    name = get_name(search_ptr->psc_ptr);
#ifdef XSB_DLL
    tempname = "__";
    strcpy(tempname[2],name);
    tempsize=strlen(tempname);
    tempname[tempsize++] = '@';
    tempname[tempsize++] = '1';
    tempname[tempsize++] = '\0';
    name = tempname;
#endif
    if (get_type(search_ptr->psc_ptr) == T_FORN) {
      if ((funcep = (int (*)) GetProcAddress(handle, name)) == NULL) {
	fprintf(stderr, "Cannot find function %s\n", name);
	fprintf(stderr, "++Warning: cannot find foreign procedure %s\n", name);
	set_ep(search_ptr->psc_ptr, (byte *)(dummy));
      } else { 
	set_ep(search_ptr->psc_ptr, (byte *)(funcep));
      }
      
    }
    search_ptr = search_ptr->next;
  }
  return (byte *)4;
}

