/* File:      xsb_wildmatch.c
** Author(s): kifer
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1999
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
#include "debugs/debug.h"

#include <stdio.h>
#include <string.h>
#include <fnmatch.h>
#include <glob.h>

#include "auxlry.h"
#include "cell.h"
#include "xsberror.h"
#include "cinterf.h"

extern char *p_charlist_to_c_string(prolog_term term, char *outstring,
				    char *in_func, char *where);
extern void c_string_to_p_charlist(char *name, prolog_term list,
				   char *in_func, char *where);
static char wild_buffer[MAXBUFSIZE], input_string_buffer[MAXBUFSIZE];
static prolog_term wild_term, input_string_term;

/* XSB wildcard matcher entry point 
** Arg1: wildcard, Arg2: string to be matched, Arg3: IgnoreCase flag */
bool do_wildmatch__(void)
{
  int ignorecase=FALSE;
  int flags = 0; /* passed to wildcard matcher */
  char *wild_ptr=NULL, *input_string=NULL;

  wild_term = reg_term(1); /* Arg1: wildcard */
  input_string_term = reg_term(2); /* Arg2: string to find matches in */
  /* If arg 3 is bound to anything, then consider this as ignore case flag */
  if (! is_var(reg_term(3)))
    ignorecase = TRUE;

  flags = (ignorecase ? FNM_CASEFOLD : 0);

  /* check wildcard expression */
  if (is_string(wild_term))
    wild_ptr = string_val(wild_term);
  else if (is_list(wild_term))
    wild_ptr = p_charlist_to_c_string(wild_term, wild_buffer,
				      "WILDMATCH", "wildcard");
  else
    xsb_abort("WILDMATCH: Wildcard (Arg 1) must be an atom or a character list");

  /* check string to be matched */
  if (is_string(input_string_term))
    input_string = string_val(input_string_term);
  else if (is_list(input_string_term)) {
    input_string = p_charlist_to_c_string(input_string_term,
					  input_string_buffer,
					  "WILDMATCH", "input string");
  } else
    xsb_abort("WILDMATCH: Input string (Arg 2) must be an atom or a character list");

  if (0 == fnmatch(wild_ptr, input_string, flags))
    return TRUE;
  return FALSE;
}

/* XSB glob matcher: match files in current directory according to a wildcard.
** Arg1: wildcard, Arg2: Mark directories with `/' flag, Arg3: variable that
** gets the list of matched files.
** Arg4 tells if conversion into a charlist is required. */
bool do_glob_directory__(void)
{
  glob_t file_vector;
  prolog_term listOfMatches, listHead, listTail;
  int markdirs=FALSE; /* flag: whether to append '/' to directories */
  int flags = 0;      /* passed to glob matcher */
  char *wild_ptr=NULL;
  int conversion_required, return_code, i;

  wild_term = reg_term(1); /* Arg1: wildcard */
  /* If arg 3 is bound to anything, then consider this as ignore case flag */
  if (! is_var(reg_term(2)))
    markdirs = TRUE;

  flags = (markdirs ? GLOB_MARK : 0);

  conversion_required = ptoc_int(4);

  /* check wildcard expression */
  if (is_string(wild_term))
    wild_ptr = string_val(wild_term);
  else if (is_list(wild_term)) {
    wild_ptr = p_charlist_to_c_string(wild_term, wild_buffer,
				      "GLOB_DIRECTORY", "wildcard");
  }
  else
    xsb_abort("GLOB_DIRECTORY: Wildcard (Arg 1) must be an atom or a character list");

  file_vector.gl_offs = 0; /* put results right in the first element of
			     file_vector */
  return_code = glob(wild_ptr, flags, NULL, &file_vector);

  switch (return_code) {
  case GLOB_NOMATCH:
    globfree(&file_vector); /* glob allocates a long string, which must be
			       freed to avoid memory leak */
    return FALSE;
  case 0: break;
  default:
    globfree(&file_vector);
    xsb_abort("GLOB_DIRECTORY: Can't read directory or out of memory");
  }

  /* matched successfully: now retrieve results */
  listTail = listOfMatches = reg_term(3);
  if (! is_var(listTail))
    xsb_abort("GLOB_DIRECTORY: Argument 7 (list of matches) must be an unbound variable");

  for (i=0; i<file_vector.gl_pathc; i++) {
    c2p_list(listTail); /* make it into a list */
    listHead = p2p_car(listTail); /* get head of the list */

    if (conversion_required)
      c_string_to_p_charlist(file_vector.gl_pathv[i], listHead,
			     "GLOB_DIRECTORY", "arg 3");
    else
      c2p_string(file_vector.gl_pathv[i], listHead);

    listTail = p2p_cdr(listTail);
  }

  c2p_nil(listTail); /* bind tail to nil */
  globfree(&file_vector);
  return TRUE;
}
