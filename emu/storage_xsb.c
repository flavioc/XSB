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


static inline STORAGE_HANDLE *get_storage_handle(Cell name);
static STORAGE_HANDLE        *increment_storage_snapshot(Cell name);
static STORAGE_HANDLE        *mark_storage_changed(Cell name);
static void                   destroy_storage_handle(Cell name);
static void                   show_table_state();
static void                   init_hash_table();


static STORAGE_HANDLE hash_table[STORAGE_HASH_SIZE];
static xsbBool table_initted = FALSE;


static inline STORAGE_HANDLE *get_storage_handle(Cell name)
{
  STORAGE_HANDLE *handle_cell;

  if (!isstring(name))
    xsb_abort("[GET_STORAGE_HANDLE] Non-string used as storage name");

  if (! table_initted) init_hash_table();

  handle_cell = &hash_table[storage_hash(name,STORAGE_HASH_SIZE)];
  while (TRUE) {
    if (handle_cell->name == name) return handle_cell;
    if (isfree_storage_handle(handle_cell)) {
      handle_cell->name = name;
      handle_cell->handle= newtrie();
      handle_cell->snapshot_number=0;
      handle_cell->changed=FALSE;
      return handle_cell;
    }
    if (handle_cell->next == NULL) {
      handle_cell->next = (STORAGE_HANDLE *)malloc(sizeof(STORAGE_HANDLE));
      if (handle_cell->next==NULL)
	xsb_exit("Out of Memory: Can't allocate storage handle");
      (handle_cell->next) -> name = (Cell)0; /* initiaize */
      (handle_cell->next) -> next = NULL;
      (handle_cell->next) -> prev = handle_cell;
    }
    handle_cell = handle_cell->next;
  }

  /* unreachable -- just to calm down the compiler */
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
  STORAGE_HANDLE *ptr = get_storage_handle(name);
  ptr->snapshot_number++;
  ptr->changed = FALSE;
  return ptr;
}

static STORAGE_HANDLE *mark_storage_changed(Cell name)
{
  STORAGE_HANDLE *ptr = get_storage_handle(name);
  ptr->changed = TRUE;
  return ptr;
}



static void destroy_storage_handle(Cell name)
{
  STORAGE_HANDLE *ptr = get_storage_handle(name);
  if (ptr->prev != NULL)
    (ptr->prev)->next = ptr->next;
  if (ptr->next != NULL)
    (ptr->next)->prev = ptr->prev;
  /* ptr->prev == NULL means this entry is in the main table, not overflow --
     can't free it then */
  if (ptr->prev != NULL)
    free(ptr);
  else {
    ptr->name = (Cell)0;
    ptr->next = NULL;
  }
}


static void init_hash_table()
{
  int i;
  table_initted = TRUE;
  for (i=0; i < STORAGE_HASH_SIZE; i++) {
    hash_table[i].name = (Cell)0;
    hash_table[i].next = NULL;
    hash_table[i].prev = NULL;
  }
}

static void show_table_state()
{
  STORAGE_HANDLE *handle_cell;
  int i;

  printf("\nCell Status\tOverflow Count\n\n");
  for (i=0; i < STORAGE_HASH_SIZE; i++) {
    handle_cell = &hash_table[i];
    if (isfree_storage_handle(handle_cell)) {
      printf("   ---\t\t   ---\n");
    } else {
      int overflow_count=0;

      printf("  taken\t\t");
      handle_cell = hash_table[i].next;
      while (handle_cell != NULL) {
	overflow_count++;
	handle_cell = handle_cell->next;
      }
      printf("   %d\n", overflow_count);
    }
  }
}
