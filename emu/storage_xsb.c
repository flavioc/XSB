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
#include "trie_internals.h"
#include "macro_xsb.h"
#include "error_xsb.h"
#include "tr_utils.h"
#include "storage_xsb.h"
#include "hashtable_xsb.h"

/* this func would insert handle into hashtable, if it isn't there */
#define find_or_insert_storage_handle(name)  \
    	    (STORAGE_HANDLE *)search_bucket(name,&hash_table,hashtable_insert)
#define destroy_storage_handle(name) \
    	    search_bucket(name,&hash_table,hashtable_delete)
#define show_table_state()    show_table_state(&hash_table)

/* 127 is a prime that is close to 2^7 */
#define STORAGE_TBL_SIZE  127

static STORAGE_HANDLE        *increment_storage_snapshot(Cell name);
static STORAGE_HANDLE        *mark_storage_changed(Cell name);


static xsbHashTable hash_table =
  {STORAGE_TBL_SIZE,sizeof(STORAGE_HANDLE),FALSE,NULL};


static inline STORAGE_HANDLE *get_storage_handle(Cell name)
{
  STORAGE_HANDLE *handle_cell;

  handle_cell = find_or_insert_storage_handle(name);
  /* new buckets are filled out with 0's by the calloc in hashtable_xsb.c */
  if (handle_cell->handle==(Cell)0) {
    /* initialize new handle */
    handle_cell->handle= newtrie();
    /* Note: not necessary to initialize snapshot_number&changed: handle_cell
       was calloc()'ed 
       handle_cell->snapshot_number=0;
       handle_cell->changed=FALSE;
    */
  }
  return handle_cell;
}

STORAGE_HANDLE *storage_builtin(int builtin_number, Cell name)
{
  switch (builtin_number) {
  case GET_STORAGE_HANDLE:
    return get_storage_handle(name);
  case INCREMENT_STORAGE_SNAPSHOT:
    return increment_storage_snapshot(name);
  case MARK_STORAGE_CHANGED:
    return mark_storage_changed(name);
  case DESTROY_STORAGE_HANDLE: {
    destroy_storage_handle(name);
    return NULL;
  }
  case SHOW_TABLE_STATE: {
    show_table_state();
    return NULL;
  }
  default:
    xsb_abort("Unknown storage builtin");
    return NULL;
  }
}


static STORAGE_HANDLE *increment_storage_snapshot(Cell name)
{
  STORAGE_HANDLE *ptr = find_or_insert_storage_handle(name);
  ptr->snapshot_number++;
  ptr->changed = FALSE;
  return ptr;
}

static STORAGE_HANDLE *mark_storage_changed(Cell name)
{
  STORAGE_HANDLE *ptr = find_or_insert_storage_handle(name);
  ptr->changed = TRUE;
  return ptr;
}
