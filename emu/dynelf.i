/* File:      dynload.c
** Author(s): Harald Schroepfer, Steve Dawson
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


#include <dlfcn.h>
#include <sys/file.h>
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
  Pair	search_ptr;
  char	sofilename[128];
  int 	strl = strlen(pofilename);
  void	*handle;
  void	*funcep;
  bool	dummy();
  
  /* (1) create filename.so */
  
  strcpy(sofilename, pofilename);
  sofilename[strl-1] = 's';
  sofilename[strl]   = 'o';
  sofilename[strl+1] = '\0';
  
  /* (2) open the needed object */
  
  if (( handle = dlopen(sofilename, RTLD_LAZY)) == 0 ) {
    fprintf(stderr, "%s\n", dlerror());
    return 0;
  }
  
  /* (3) find address of function and data objects
  **
  ** dyn_link_all(loc, cur_mod);
  */
  
  search_ptr = (Pair)get_ep(cur_mod);
  
  while (search_ptr) {
    name = get_name(search_ptr->psc_ptr);
    
    if (get_type(search_ptr->psc_ptr) == T_FORN) {
      if ((funcep = (int (*)) dlsym(handle, name)) == NULL) {
	fprintf(stderr, "%s\n", dlerror());
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
