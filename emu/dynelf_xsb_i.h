/* File:      dynelf_xsb_i.h
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
#include <stdlib.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "memory_xsb.h"
#include "inst_xsb.h"
#include "psc_xsb.h"
#include "error_xsb.h"
#include "io_builtins_xsb.h"

#define BUFFEXTRA 1024

/*----------------------------------------------------------------------*/

bool dummy()
{
    xsb_error("Trying to use an undefined foreign procedure");
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
  char  *ldp1,*ldp2;
  char  *ldstring,*libpath;
  char  ldtemp;
  int   slibpath;
  
  /* (1) create filename.so */
  
  strcpy(sofilename, pofilename);
  sofilename[strl-1] = 's';
  sofilename[strl]   = 'o';
  sofilename[strl+1] = '\0';
  
  /* (1.5) include necessary paths into LD_LIBRARY_PATH */
  
/*   libpath = getenv("LD_LIBRARY_PATH"); */
/*   slibpath = strlen(libpath); */
/*   ldstring = (char *) malloc(sizeof(char)*(strlen(ld_option)+slibpath+17)); */

/*   if (ldstring == NULL) */
/*     xsb_abort("Could not allocate memory for ld_options manipulation"); */

/*   *ldstring = '\0'; */
/*   ldstring = strcpy(ldstring,"LD_LIBRARY_PATH="); */
/*   ldstring = strcat(ldstring,libpath); */
  
/*   for (ldp1=ld_option; (*ldp1); ldp1++) { */
/*     if (*ldp1 == '-' && *(ldp1+1) == 'L') { */
/*       ldp1 += 2; */
/*       ldp2 = ldp1; */
/*       while (*ldp1 != ' ' && *ldp1 != '\0') */
/* 	ldp1++; */
/*       *ldp1 = '\0'; */
/*       ldtemp = *(ldp2-1); */
/*       *(ldp2-1) = ':'; */
/*       ldstring = strcat(ldstring,ldp2-1); */
/*       *ldp1 = ' '; */
/*       *(ldp2-1) = ldtemp; */
/*     } else if (*ldp1 == '\'') { */
/*       ldp1++; */
/*       while (*ldp1 != '\'') */
/* 	ldp1++; */
/*     } else if (*ldp1 == '\"') { */
/*       ldp1++; */
/*       while (*ldp1 != '\"') */
/* 	ldp1++; */
/*     } */
/*   } */

/*   if (putenv(ldstring) != 0) */
/*     xsb_warn("+++Error changing environment variable LD_LIBRARY_PATH"); */

  /*   free(ldstring); */
  /* the call above segfaults on linux when calling getenv afterwards */
  /* luis, 10.21.99 */

  
  /* (2) open the needed object */
  
  if (( handle = dlopen(sofilename, RTLD_LAZY)) == 0 ) {
    xsb_mesg("%s", dlerror());
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
      if ((funcep = (int *) dlsym(handle, name)) == NULL) {
	fprintf(stdwarn, "%s\n", dlerror());
	xsb_warn("Cannot find foreign procedure %s", name);
	set_ep(search_ptr->psc_ptr, (byte *)(dummy));
      } else { 
	set_ep(search_ptr->psc_ptr, (byte *)(funcep));
      }
      
    }
    search_ptr = search_ptr->next;
  }
  return (byte *)4;
}

