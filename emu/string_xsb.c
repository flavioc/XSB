/* File:      string_xsb.c  -- string manipulation stuff
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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "configs/config.h"
#include "debugs/debug.h"
#include "auxlry.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "psc_xsb.h"
#include "hash_xsb.h"
#include "tries.h"
#include "choice.h"
#include "deref.h"
#include "memory_xsb.h"
#include "heap_xsb.h"
#include "register.h"
#include "flags_xsb.h"
#include "binding.h"
#include "builtin.h"
#include "cinterf.h"

#include "sp_unify_xsb_i.h"

extern char *p_charlist_to_c_string(prolog_term term, char *outstring, 
				    int outstring_size,
				    char *in_func, char *where);
extern void c_string_to_p_charlist(char *name, prolog_term list,
				   char *in_func, char *where);

extern Cell ptoc_tag(int regnum);

static Cell term, term2;
static char output_buffer[5*MAXBUFSIZE], input_buffer[4*MAXBUFSIZE];


/* R1: +Substring; R2: +String; R3: ?Pos
   Check if Arg1 is a substring of Arg2; unify pos of match with Arg3
*/
bool str_sub(void)
{
  static char *subptr, *stringptr, *matchptr;
  static int substr_pos;

  term = ptoc_tag(1);
  term2 = ptoc_tag(2);
  if (isstring(term) && isstring(term2)) { 
    subptr = string_val(term);
    stringptr = string_val(term2);
    matchptr = strstr(stringptr, subptr);
    substr_pos = matchptr-stringptr+1; /* relative pos of substring */
    if (matchptr == NULL)
      return FALSE;
    else {
      return int_unify(makeint(substr_pos), ptoc_tag(3));
    }
  } else return FALSE;
}


bool str_cat(void)
{
  static char *str1, *str2, *tmpstr;

  term = ptoc_tag(1);
  term2 = ptoc_tag(2);
  if (isstring(term) && isstring(term2)) {
    str1 = string_val(term);
    str2 = string_val(term2);
    
    tmpstr = (char *)malloc(strlen(str1) + strlen(str2) + 1);
    strcpy(tmpstr, str1);
    strcat(tmpstr, str2);
    str1 = string_find(tmpstr, 1);
    free(tmpstr);
    return atom_unify(makestring(str1), ptoc_tag(3));
  } else return FALSE;
}


/* XSB string substitution entry point
   In: 
       Arg1: string
       Arg2: beginning offset
       Arg3: ending offset. `-' : end of string, -1 : char before last, etc.
   Out:
       Arg4: new (output) string
   Always succeeds, unless error.
*/
bool substring(void)
{
  /* Prolog args are first assigned to these, so we could examine the types
     of these objects to determine if we got strings or atoms. */
  prolog_term input_term, output_term;
  prolog_term beg_offset_term, end_offset_term;
  char *input_string=NULL;    /* string where matches are to be found */
  int beg_offset=0, end_offset=0, input_len=0, substring_len=0;
  int conversion_required=FALSE;

  input_term = reg_term(1);  /* Arg1: string to find matches in */
  if (is_string(input_term)) /* check it */
    input_string = string_val(input_term);
  else if (is_list(input_term)) {
    input_string =
      p_charlist_to_c_string(input_term, input_buffer, sizeof(input_buffer),
			     "SUBSTRING", "input string");
    conversion_required = TRUE;
  } else
    xsb_abort("SUBSTRING: Arg 1 (the input string) must be an atom or a character list");

  input_len = strlen(input_string);

  /* arg 2: beginning offset */
  beg_offset_term = reg_term(2);
  if (! is_int(beg_offset_term))
    xsb_abort("SUBSTRING: Arg 2 (the beginning offset) must be an integer");
  beg_offset = int_val(beg_offset_term);
  if (beg_offset < 0)
    beg_offset = 0;
  else if (beg_offset > input_len)
    beg_offset = input_len;

  /* arg 3: ending offset */
  end_offset_term = reg_term(3);
  if (is_var(end_offset_term))
    end_offset = input_len;
  else if (! is_int(end_offset_term))
    xsb_abort("SUBSTRING: Arg 3 (the end offset) must be integer or _");
  else end_offset = int_val(end_offset_term);

  if (end_offset < 0)
    end_offset = input_len + end_offset;
  else if (end_offset > input_len)
    end_offset = input_len;
  else if (end_offset < beg_offset)
    end_offset = beg_offset;

  output_term = reg_term(4);
  if (! is_var(output_term))
    xsb_abort("SUBSTRING: Arg 4 (the output string) must be an unbound variable");

  /* do the actual replacement */
  substring_len = end_offset-beg_offset;
  strncpy(output_buffer, input_string+beg_offset, substring_len);
  *(output_buffer+substring_len) = '\0';
  
  /* get result out */
  if (conversion_required)
    c_string_to_p_charlist(output_buffer, output_term,
			   "SUBSTRING", "Arg 4");
  else
    c2p_string(output_buffer, output_term);
  
  return(TRUE);
}


/* XSB string substitution entry point: replace substrings specified in Arg2
   with strings in Arg3.
   In: 
       Arg1: string
       Arg2: substring specification, a list [s(B1,E1),s(B2,E2),...]
       Arg3: list of replacement string
   Out:
       Arg4: new (output) string
   Always succeeds, unless error.
*/
bool string_substitute(void)
{
  /* Prolog args are first assigned to these, so we could examine the types
     of these objects to determine if we got strings or atoms. */
  prolog_term input_term, output_term;
  prolog_term subst_reg_term, subst_spec_list_term, subst_spec_list_term1;
  prolog_term subst_str_term=(prolog_term)0,
    subst_str_list_term, subst_str_list_term1;
  char *input_string=NULL;    /* string where matches are to be found */
  char *subst_string=NULL;
  prolog_term beg_term, end_term;
  int beg_offset=0, end_offset=0, input_len;
  int last_pos = 0; /* last scanned pos in input string */
  /* the output buffer is made large enough to include the input string and the
     substitution string. */
  char subst_buf[MAXBUFSIZE];
  char *output_ptr;
  int conversion_required=FALSE; /* from C string to Prolog char list */

  input_term = reg_term(1);  /* Arg1: string to find matches in */
  if (is_string(input_term)) /* check it */
    input_string = string_val(input_term);
  else if (is_list(input_term)) {
    input_string =
      p_charlist_to_c_string(input_term, input_buffer, sizeof(input_buffer),
			     "STRING_SUBSTITUTE", "input string");
    conversion_required = TRUE;
  } else
    xsb_abort("STRING_SUBSTITUTE: Arg 1 (the input string) must be an atom or a character list");

  input_len = strlen(input_string);

  /* arg 2: substring specification */
  subst_spec_list_term = reg_term(2);
  if (!is_list(subst_spec_list_term) && !is_nil(subst_spec_list_term))
    xsb_abort("STRING_SUBSTITUTE: Arg 2 must be a list [s(B1,E1),s(B2,E2),...]");

  /* handle substitution string */
  subst_str_list_term = reg_term(3);
  if (! is_list(subst_str_list_term))
    xsb_abort("STRING_SUBSTITUTE: Arg 3 must be a list of strings");

  output_term = reg_term(4);
  if (! is_var(output_term))
    xsb_abort("STRING_SUBSTITUTE: Arg 4 (the output) must be an unbound variable");

  subst_spec_list_term1 = subst_spec_list_term;
  subst_str_list_term1 = subst_str_list_term;

  if (is_nil(subst_spec_list_term1)) {
    strncpy(output_buffer, input_string, sizeof(output_buffer));
    goto EXIT;
  }
  if (is_nil(subst_str_list_term1))
    xsb_abort("STRING_SUBSTITUTE: Arg 3 must not be an empty list");

  /* initialize output buf */
  output_ptr = output_buffer;

  do {
    subst_reg_term = p2p_car(subst_spec_list_term1);
    subst_spec_list_term1 = p2p_cdr(subst_spec_list_term1);

    if (!is_nil(subst_str_list_term1)) {
      subst_str_term = p2p_car(subst_str_list_term1);
      subst_str_list_term1 = p2p_cdr(subst_str_list_term1);

      if (is_string(subst_str_term)) {
	subst_string = string_val(subst_str_term);
      } else if (is_list(subst_str_term)) {
	subst_string =
	  p_charlist_to_c_string(subst_str_term, subst_buf, sizeof(subst_buf),
				 "STRING_SUBSTITUTE", "substitution string");
      } else 
	xsb_abort("STRING_SUBSTITUTE: Arg 3 must be a list of strings");
    }

    beg_term = p2p_arg(subst_reg_term,1);
    end_term = p2p_arg(subst_reg_term,2);

    if (!is_int(beg_term) || !is_int(end_term))
      xsb_abort("STRING_SUBSTITUTE: Non-integer in Arg 2");
    else{
      beg_offset = int_val(beg_term);
      end_offset = int_val(end_term);
    }
    /* -1 means end of string */
    if (end_offset < 0)
      end_offset = input_len;
    if ((end_offset < beg_offset) || (beg_offset < last_pos))
      xsb_abort("STRING_SUBSTITUTE: Substitution regions in Arg 2 not sorted");

    /* do the actual replacement */
    strncpy(output_ptr, input_string + last_pos, beg_offset - last_pos);
    output_ptr = output_ptr + beg_offset - last_pos;
    if (sizeof(output_buffer)
	> (output_ptr - output_buffer + strlen(subst_string)))
      strcpy(output_ptr, subst_string);
    else
      xsb_abort("STRING_SUBSTITUTE: Substitution result size %d > maximum %d",
		beg_offset + strlen(subst_string),
		sizeof(output_buffer));
    
    last_pos = end_offset;
    output_ptr = output_ptr + strlen(subst_string);

  } while (!is_nil(subst_spec_list_term1));

  if (sizeof(output_buffer) > (output_ptr-output_buffer+input_len-end_offset))
    strcat(output_ptr, input_string+end_offset);

 EXIT:
  /* get result out */
  if (conversion_required)
    c_string_to_p_charlist(output_buffer,output_term,"STRING_SUBSTITUTE","Arg 4");
  else
    c2p_string(output_buffer, output_term);
  
  return(TRUE);
}

