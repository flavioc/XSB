/* File:      psc.c
** Author(s): Xu, Sagonas, Swift
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


#include <stdio.h>
#include <string.h>

#include "configs/config.h"
#include "debugs/debug.h"

#include "auxlry.h"
#include "cell.h"
#include "xsberror.h"
#include "psc.h"
#include "tries.h"
#include "hash.h"
#include "xmacro.h"
#include "loader.h"
#include "flags.h"
#include "inst.h"
#include "memory.h"


/* === String Table manipulation ======================================	*/

/*
 * Looks up a string in the String Table.  If it is not found and the
 * insert flag is set, then inserts the string into the table.
 * If the string exists in the table, returns a pointer to the string
 * part of the corresponding table entry.
 *                      
 * String Table entries have the form:
 *           +--------------------------+
 *           | Ptr_to_Next | String ... |
 *           +--------------------------+
 */

#define CHAR_PTR_SIZE  sizeof(char *)

char *string_find(char *str, int insert) {

  char **ptr, *str0;

  ptr = (char **)string_table.table + hash(str, 0, string_table.size);
  while (*ptr) {
    str0 = *ptr + CHAR_PTR_SIZE;
    if (strcmp(str, str0) == 0)
      return str0;
    ptr = (char **)(*ptr);
  }
  
  if (insert) {
    str0 = (char *)mem_alloc(CHAR_PTR_SIZE + strlen(str) + 1);
    *ptr = str0;
    *(char **)str0 = NULL;
    str0 = str0 + CHAR_PTR_SIZE;
    strcpy(str0, str);
    string_table_increment_and_check_for_overflow;
    return str0;
  }
  else
    return NULL;
}


/* === PSC and PSC-PAIR structure creation/initialization =============== */

/*
 *  Create a PSC record and initialize its fields.
 */
static Psc make_psc_rec(char *name, char arity) {
  Psc temp;
  int length;
  
  length = strlen(name);
  temp = (Psc)mem_alloc(sizeof(struct psc_rec));
  set_env(temp, 0);
  set_type(temp, 0);
  set_spy(temp, 0);
  set_arity(temp, arity);
  set_length(temp, length);
  set_ep(temp,0);
  set_name(temp, string_find(name, 1));
  return temp;
}


/*
 *  Create a PSC-PAIR record, set it to point to a PSC record, and place
 *  it at the head of a PSC-PAIR record chain.
 */
static Pair make_psc_pair(Psc psc_ptr, Pair *link_ptr) {

  Pair new_pair;
  
  new_pair = (Pair)mem_alloc(sizeof(struct psc_pair));
  pair_psc(new_pair) = psc_ptr;         /* set 1st to point to psc_rec */
  pair_next(new_pair) = *link_ptr;      /* set 2nd to old head */
  *link_ptr = new_pair;                 /* new symbol is in the head! */
  return new_pair;
}


/* === get_tip: get the TIP from a PSC record =========================	*/

tab_inf_ptr get_tip(Psc temp) 
{
    CPtr temp1 ;

    switch (get_type(temp)) {
      case T_DYNA:
      case T_PRED:
	temp1 = (CPtr)get_ep(temp);
	if (temp1 != 0) {
	  switch (*(pb)temp1) {
	    case tabletry:
	    case tabletrysingle:
	      return (tab_inf_ptr) (temp1[2]) ;
	    case test_heap:
	      if (*(pb)(temp1+2) == tabletry ||
		  *(pb)(temp1+2) == tabletrysingle)
		return (tab_inf_ptr) (temp1[4]) ;
	      else return NULL;
	      break;
	    case switchon3bound:
	    case switchonbound:
	    case switchonterm:
	      if (  *(pb) (temp1+3) == tabletry 
	        ||  *(pb) (temp1+3) == tabletrysingle) 
		return (tab_inf_ptr) (temp1[5]) ;
	      else return (tab_inf_ptr) NULL;
	    default:
	      return (tab_inf_ptr) NULL;
	  }
	}
	else return (tab_inf_ptr) NULL;
      default: 
	return (tab_inf_ptr) NULL;
    }
}


/* === is_globalmod: Is a global module ===============================	*/

static int is_globalmod(Psc mod_psc)
{
/* 
 * The modules considered global are the ones that have the value 1 in
 * their entry point field of the module's psc record.  The modules I
 * know that have this property are the modules "global" and "usermod".
 */
    if (mod_psc)
      return (Cell)get_ep(mod_psc) == 1;
    else
      return 1;
}


/* === search: search in a given chain ================================	*/

/*
 *  Returns a pointer to the PSC-PAIR structure which points to the
 *  PSC record of the desired symbol.
 */
static Pair search(int arity, char *name, Pair *search_ptr)
{
    Psc psc_ptr;

    while (*search_ptr) {
      psc_ptr = (*search_ptr)->psc_ptr;
      if (strcmp(name, get_name(psc_ptr)) == 0
	  && arity == get_arity(psc_ptr) )
	return (*search_ptr);
      else
	search_ptr  = &((*search_ptr)->next);
    }
    return NULL;
} /* search */


/* === insert0: search/insert to a given chain ========================	*/

static Pair insert0(char *name, char arity, Pair *search_ptr, int *is_new)
{
    Pair new_pair;

    new_pair = search(arity, name, search_ptr);
    if (!new_pair) {
      *is_new = 1;
      new_pair = make_psc_pair(make_psc_rec(name,arity), search_ptr);
    }
    else
      *is_new = 0;
    return new_pair;
} /* insert0 */


/* === insert: search/insert to a given module ========================	*/

Pair insert(char *name, char arity, Psc mod_psc, int *is_new)
{
    Pair *search_ptr, temp;

    if (is_globalmod(mod_psc)) {
      search_ptr = (Pair *)(symbol_table.table +
	           hash(name, arity, symbol_table.size));
      temp = insert0(name, arity, search_ptr, is_new);
      if (*is_new)
	symbol_table_increment_and_check_for_overflow;
      return temp;
    }
    else {
      search_ptr = (Pair *)&(get_ep(mod_psc));
      return insert0(name, arity, search_ptr, is_new);
    }
} /* insert */


/* === insert_module: search for/insert a given module ================	*/

Pair insert_module(int type, char *name)
{
    Pair new_pair;
    int is_new;

    new_pair = insert0(name, 0, (Pair *)&flags[MOD_LIST], &is_new);
    if (is_new) {
	get_type(new_pair->psc_ptr) = type;
	get_ep(new_pair->psc_ptr) = 0;
    } else {	/* set loading bit: T_MODU - loaded; 0 - unloaded */
	get_type(new_pair->psc_ptr) |= type;
    }
    return new_pair;
} /* insert_module */


/* === link_sym: link a symbol into a given module ==================== */

/*
 *  Given a PSC record 'psc' for a particular symbol, check to see if
 *  that symbol already exists in the module 'mod_psc'.
 *  Does NOT exist => insert it and return a ptr to its PSC-PAIR record.
 *  DOES exist => check if the found PSC record is the same as 'psc'.
 *    YES => return a ptr to its PSC-PAIR record.
 *     NO => replace the old PSC record with 'psc'; return a ptr to the
 *           PSC-PAIR record.
 */

Pair link_sym(Psc psc, Psc mod_psc)
{
    Pair *search_ptr, found_pair;
    char *name, message[120];
    int arity, global_flag;

    name = get_name(psc);
    arity = get_arity(psc);
    if ( (global_flag = is_globalmod(mod_psc)) )
      search_ptr = (Pair *)symbol_table.table +
	           hash(name, arity, symbol_table.size);
    else
      search_ptr = (Pair *)&get_ep(mod_psc);
    if ((found_pair = search(arity, name, search_ptr))) {
      if (pair_psc(found_pair) != psc) {
	/*
	 *  Invalidate the old name!! It is no longer accessible
	 *  through the global chain.
	 */
	if ( get_type(pair_psc(found_pair)) != T_ORDI ) {
	  sprintf(message,
		  "%s/%d (type %d) was defined in another module!",
		  name, arity, get_type(pair_psc(found_pair)));
	  xsb_warn(message);
	}
	pair_psc(found_pair) = psc;
      }
    }
    else {
      found_pair = make_psc_pair(psc, search_ptr);
      if (global_flag)
	symbol_table_increment_and_check_for_overflow;
    }
    return found_pair;
} /* link_sym */


/*
 * Get the PSC for ret/n.  If it already exists, just return it.  Or
 * create one and save it in ret_psc[n].
 */
Psc get_ret_psc(int n)
{
  Pair temp;
  int new_indicator;

  if (!ret_psc[n]) {
    temp = (Pair) insert("ret", n, global_mod, &new_indicator);
    ret_psc[n] = pair_psc(temp);
  }
  return ret_psc[n];
}


/*
 * Get the PSC for intern/1, a generic functor for storing in the roots
 * of interned tries.
 */
Psc get_intern_psc() {

  Pair intern_handle;
  int new_indicator;

  intern_handle = insert("intern", 1, global_mod, &new_indicator);
  return (pair_psc(intern_handle));
}
