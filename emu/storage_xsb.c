/* File:      storage_xsb.c  -- support for the storage.P module
** Author(s): Michael Kifer
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 2001
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
#include "xsb_debug.h"

#include <stdio.h>
#include <stdlib.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "psc_xsb.h"
#include "cinterf.h"
#include "register.h"
#include "flags_xsb.h"
#include "trie_internals.h"
#include "macro_xsb.h"
#include "error_xsb.h"

#include "storage_xsb.h"
#include "tr_utils.h"

#include "conget_xsb_i.h"

STORAGE_HANDLE *get_storage_handle(Cell name);
STORAGE_HANDLE *increment_storage_snapshot(Cell name);
STORAGE_HANDLE *mark_storage_changed(Cell name);
STORAGE_HANDLE *destroy_storage_handle(Cell name);


STORAGE_HANDLE *storage_builtin(int builtin_number, Cell name)
{
  switch (builtin_number) {
  case GET_STORAGE_HANDLE:
    return get_storage_handle(name);
  case INCREMENT_STORAGE_SNAPSHOT:
    return increment_storage_snapshot(name);
  case MARK_STORAGE_CHANGED:
    return mark_storage_changed(name);
  case DESTROY_STORAGE_HANDLE:
    return destroy_storage_handle(name);
  default:
    xsb_abort("Unknown storage builtin");
    return NULL;
  }
}


STORAGE_HANDLE *get_storage_handle(Cell name)
{
  STORAGE_HANDLE *ptr;

  if (!isstring(name))
    xsb_abort("[GET_STORAGE_HANDLE] Non-string used as storage name");

  ptr = (STORAGE_HANDLE *) conget(name);
  if (ptr != NULL)
    return ptr;

  ptr = (STORAGE_HANDLE *)malloc(sizeof(STORAGE_HANDLE));
  if (ptr==NULL)
    xsb_exit("Out of Memory");

  conset(name,(Integer)ptr);

  ptr->name = name;
  ptr->handle= newtrie();
  ptr->snapshot_number=0;
  ptr->changed=FALSE;

  return ptr;
}


STORAGE_HANDLE *increment_storage_snapshot(Cell name)
{
  STORAGE_HANDLE *ptr = get_storage_handle(name);
  ptr->snapshot_number++;
  ptr->changed = FALSE;
  return ptr;
}

STORAGE_HANDLE *mark_storage_changed(Cell name)
{
  STORAGE_HANDLE *ptr = get_storage_handle(name);
  ptr->changed = TRUE;
  return ptr;
}



STORAGE_HANDLE *destroy_storage_handle(Cell name)
{
  STORAGE_HANDLE *ptr = get_storage_handle(name);
  if (ptr != NULL)
    free(ptr);
  conset(name,(Integer)NULL);
  return NULL;
}
