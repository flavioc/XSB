/* File:      xsb_re_match.c
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
#include <regex.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "cinterf.h"
#include "heap_xsb.h"

extern char *p_charlist_to_c_string(prolog_term term, VarString *outstring, 
				    char *in_func, char *where);
extern void c_string_to_p_charlist(char *name, prolog_term list,
				   char *in_func, char *where);

/* from hash.c */
extern unsigned long hash(char *objname, byte arity, unsigned long tbl_size);

#define REGEXP_TBL_SIZE 17   /* keep this many compiled regexp in cache */
#define NMATCH          31   /* The size of the array where we get the results
				of the matches. The first elt describes the
				global match. The other describe the number of
				parenthetical matches. */

#define GET_MATCH_PTR(i)  (input_string + match_array[i].rm_so)
#define GET_MATCH_SIZE(i) (match_array[i].rm_eo - match_array[i].rm_so)

/* cache table for compiled regular expressions */
struct regexp_tbl_entry {
  int     ignorecase;	     	     	/* whether case should be ignored */
  char    *original;	    	    	/* the original regexp */
  regex_t compiled;	    	    	/* the compiled regexp */
};

struct regexp_tbl_entry regexp_tbl[REGEXP_TBL_SIZE];

static bool xsb_re_match(char *regexp_ptr, char* match_str, int ignorecase,
			 regmatch_t **match_array, int *paren_number);
static void initialize_regexp_tbl(void);

static int first_call = TRUE; /* whether this is the first call to the regexp
				 matcher. Used to initialize the regexp tbl */

static vstrDEFINE(input_buffer);
static vstrDEFINE(subst_buf);
static vstrDEFINE(output_buffer);
static vstrDEFINE(regexp_buffer);


/* XSB regular expression matcher entry point
   In:
       Arg1: regexp
       Arg2: string
       Arg3: offset
       Arg4: ignorecase
   Out:
       Arg5: list of the form [match(bo0,eo0), match(bo1,eo1),...]
       	     where bo0,eo0 specifies the beginning and ending offsets of the
	     matched substring; bo1,eo1 specify the beginning and ending
	     offsets of the match corresponding to the first parenthesized
	     subexpression, etc.
*/
bool do_regmatch__(void)
{
  prolog_term listHead, listTail;
  /* Prolog args are first assigned to these, so we could examine the types
     of these objects to determine if we got strings or atoms. */
  prolog_term regexp_term, input_term, offset_term;
  prolog_term output_term = p2p_new();
  int i;
  char *regexp_ptr=NULL;      /* regular expression ptr	       	      */
  char *input_string=NULL;    /* string where matches are to be found */
  int ignorecase=FALSE;
  int return_code, paren_number, offset;
  regmatch_t *match_array;

  if (first_call)
    initialize_regexp_tbl();

  regexp_term = reg_term(1);  /* Arg1: regexp */
  if (is_string(regexp_term)) /* check it */
    regexp_ptr = string_val(regexp_term);
  else if (is_list(regexp_term))
    regexp_ptr = p_charlist_to_c_string(regexp_term, &regexp_buffer,
					"RE_MATCH", "regular expression");
  else
    xsb_abort("RE_MATCH: Arg 1 (the regular expression) must be an atom or a character list");

  input_term = reg_term(2);  /* Arg2: string to find matches in */
  if (is_string(input_term)) /* check it */
    input_string = string_val(input_term);
  else if (is_list(input_term)) {
    input_string = p_charlist_to_c_string(input_term, &input_buffer,
					  "RE_MATCH", "input string");
  } else
    xsb_abort("RE_MATCH: Arg 2 (the input string) must be an atom or a character list");
  
  offset_term = reg_term(3); /* arg3: offset within the string */
  if (! is_int(offset_term))
    xsb_abort("RE_MATCH: Arg 3 (the offset) must be an integer");
  offset = int_val(offset_term);
  if (offset < 0 || offset > strlen(input_string))
    xsb_abort("RE_MATCH: Arg 3 (=%d) must be between 0 and %d",
	      strlen(input_string));

  /* If arg 4 is bound to anything, then consider this as ignore case flag */
  if (! is_var(reg_term(4)))
    ignorecase = TRUE;


  /* paren_number gets the # of parenthetical subexpressions (not 1 minus!) */
  return_code = xsb_re_match(regexp_ptr, input_string+offset, ignorecase,
			     &match_array, &paren_number);

  if (! return_code) return FALSE;



  /* return result */
  listTail = output_term;
  for (i=0; i <= paren_number; i++) {
    c2p_list(listTail); /* make it into a list */
    listHead = p2p_car(listTail); /* get head of the list */

    /* bind i-th match to listHead as match(beg,end) */
    c2p_functor("match", 2, listHead);
    c2p_int(match_array[i].rm_so+offset, p2p_arg(listHead,1));
    c2p_int(match_array[i].rm_eo+offset, p2p_arg(listHead,2));

    listTail = p2p_cdr(listTail);
  }

  c2p_nil(listTail); /* bind tail to nil */
  return p2p_unify(output_term, reg_term(5));
}


/* XSB regular expression matcher entry point
   In:
       Arg1: regexp
       Arg2: string
       Arg3: offset
       Arg4: ignorecase
   Out:
       Arg5: list of the form [match(bo0,eo0), match(bo1,eo1),...]
       	     where bo*,eo* specify the beginning and ending offsets of the
	     matched substrings.
	     All matched substrings are returned. Parenthesized expressions are
	     ignored.
*/
bool do_bulkmatch__(void)
{
  prolog_term listHead, listTail;
  /* Prolog args are first assigned to these, so we could examine the types
     of these objects to determine if we got strings or atoms. */
  prolog_term regexp_term, input_term, offset_term;
  prolog_term output_term = p2p_new();
  char *regexp_ptr=NULL;      /* regular expression ptr	       	      */
  char *input_string=NULL;    /* string where matches are to be found */
  int ignorecase=FALSE;
  int return_code, paren_number, offset;
  regmatch_t *match_array;
  int last_pos=0, input_len;
  
  if (first_call)
    initialize_regexp_tbl();

  regexp_term = reg_term(1);  /* Arg1: regexp */
  if (is_string(regexp_term)) /* check it */
    regexp_ptr = string_val(regexp_term);
  else if (is_list(regexp_term))
    regexp_ptr = p_charlist_to_c_string(regexp_term, &regexp_buffer,
					"RE_MATCH", "regular expression");
  else
    xsb_abort("RE_MATCH: Arg 1 (the regular expression) must be an atom or a character list");

  input_term = reg_term(2);  /* Arg2: string to find matches in */
  if (is_string(input_term)) /* check it */
    input_string = string_val(input_term);
  else if (is_list(input_term)) {
    input_string = p_charlist_to_c_string(input_term, &input_buffer,
					  "RE_MATCH", "input string");
  } else
    xsb_abort("RE_MATCH: Arg 2 (the input string) must be an atom or a character list");

  input_len = strlen(input_string);
  
  offset_term = reg_term(3); /* arg3: offset within the string */
  if (! is_int(offset_term))
    xsb_abort("RE_MATCH: Arg 3 (the offset) must be an integer");
  offset = int_val(offset_term);
  if (offset < 0 || offset > input_len)
    xsb_abort("RE_MATCH: Arg 3 (=%d) must be between 0 and %d", input_len);

  /* If arg 4 is bound to anything, then consider this as ignore case flag */
  if (! is_var(reg_term(4)))
    ignorecase = TRUE;

  last_pos = offset;
  /* returned result */
  listTail = output_term;
  while (last_pos < input_len) {
    c2p_list(listTail); /* make it into a list */
    listHead = p2p_car(listTail); /* get head of the list */

    return_code = xsb_re_match(regexp_ptr, input_string+last_pos, ignorecase,
			       &match_array, &paren_number);
    /* exit on no match */
    if (! return_code) break;

    /* bind i-th match to listHead as match(beg,end) */
    c2p_functor("match", 2, listHead);
    c2p_int(match_array[0].rm_so+last_pos, p2p_arg(listHead,1));
    c2p_int(match_array[0].rm_eo+last_pos, p2p_arg(listHead,2));

    listTail = p2p_cdr(listTail);
    if (match_array[0].rm_eo > 0)
      last_pos = match_array[0].rm_eo+last_pos;
    else
      last_pos++;
  }

  c2p_nil(listTail); /* bind tail to nil */
  return p2p_unify(output_term, reg_term(5));
}


/* should be removed when XSB gets garbage collector */
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
bool do_regsubstitute__(void)
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
  int conversion_required=FALSE; /* from C string to Prolog char list */
  
  vstrSET(&output_buffer,"");

  input_term = reg_term(1);  /* Arg1: string to find matches in */
  if (is_string(input_term)) /* check it */
    input_string = string_val(input_term);
  else if (is_list(input_term)) {
    input_string = p_charlist_to_c_string(input_term, &input_buffer,
					  "RE_SUBSTITUTE", "input string");
    conversion_required = TRUE;
  } else
    xsb_abort("RE_SUBSTITUTE: Arg 1 (the input string) must be an atom or a character list");

  input_len = strlen(input_string);

  /* arg 2: substring specification */
  subst_spec_list_term = reg_term(2);
  if (!is_list(subst_spec_list_term) && !is_nil(subst_spec_list_term))
    xsb_abort("RE_SUBSTITUTE: Arg 2 must be a list [s(B1,E1),s(B2,E2),...]");

  /* handle substitution string */
  subst_str_list_term = reg_term(3);
  if (! is_list(subst_str_list_term))
    xsb_abort("RE_SUBSTITUTE: Arg 3 must be a list of strings");

  output_term = reg_term(4);
  if (! is_var(output_term))
    xsb_abort("RE_SUBSTITUTE: Arg 4 (the output) must be an unbound variable");

  subst_spec_list_term1 = subst_spec_list_term;
  subst_str_list_term1 = subst_str_list_term;

  if (is_nil(subst_spec_list_term1)) {
    vstrSET(&output_buffer, input_string);
    goto EXIT;
  }
  if (is_nil(subst_str_list_term1))
    xsb_abort("RE_SUBSTITUTE: Arg 3 must not be an empty list");

  do {
    subst_reg_term = p2p_car(subst_spec_list_term1);
    subst_spec_list_term1 = p2p_cdr(subst_spec_list_term1);

    if (!is_nil(subst_str_list_term1)) {
      subst_str_term = p2p_car(subst_str_list_term1);
      subst_str_list_term1 = p2p_cdr(subst_str_list_term1);

      if (is_string(subst_str_term)) {
	subst_string = string_val(subst_str_term);
      } else if (is_list(subst_str_term)) {
	subst_string = p_charlist_to_c_string(subst_str_term, &subst_buf,
					      "RE_SUBSTITUTE",
					      "substitution string");
      } else 
	xsb_abort("RE_SUBSTITUTE: Arg 3 must be a list of strings");
    }

    beg_term = p2p_arg(subst_reg_term,1);
    end_term = p2p_arg(subst_reg_term,2);

    if (!is_int(beg_term) || !is_int(end_term))
      xsb_abort("RE_SUBSTITUTE: Non-integer in Arg 2");
    else{
      beg_offset = int_val(beg_term);
      end_offset = int_val(end_term);
    }
    /* -1 means end of string */
    if (end_offset < 0)
      end_offset = input_len;
    if ((end_offset < beg_offset) || (beg_offset < last_pos))
      xsb_abort("RE_SUBSTITUTE: Substitution regions in Arg 2 not sorted");

    /* do the actual replacement */
    vstrAPPENDBLK(&output_buffer, input_string+last_pos, beg_offset-last_pos);
    vstrAPPEND(&output_buffer, subst_string);
    
    last_pos = end_offset;

  } while (!is_nil(subst_spec_list_term1));

  vstrAPPEND(&output_buffer, input_string+end_offset);

 EXIT:
  /* get result out */
  if (conversion_required)
    c_string_to_p_charlist(output_buffer.string, output_term,
			   "RE_SUBSTITUTE", "Arg 4");
  else
    /* DO NOT intern. When atom table garbage collection is in place, then
       replace the instruction with this:
       	   c2p_string(output_buffer, output_term);
       The reason for not interning is that in Web page
       manipulation it is often necessary to process the same string many
       times. This can cause atom table overflow. Not interning allws us to
       circumvent the problem.  */
    ctop_string(4, output_buffer.string);
  
  return(TRUE);
}


/* should be removed when XSB gets garbage collector */
/* XSB string substitution entry point
   In: 
       Arg1: string
       Arg2: beginning offset
       Arg3: ending offset. < 0 means end of string
   Out:
       Arg4: new (output) string
   Always succeeds, unless error.
*/
bool do_regsubstring__(void)
{
  /* Prolog args are first assigned to these, so we could examine the types
     of these objects to determine if we got strings or atoms. */
  prolog_term input_term, output_term;
  prolog_term beg_offset_term, end_offset_term;
  char *input_string=NULL;    /* string where matches are to be found */
  int beg_offset, end_offset, input_len, substring_len;
  int conversion_required=FALSE;
  
  vstrSET(&output_buffer,"");

  input_term = reg_term(1);  /* Arg1: string to find matches in */
  if (is_string(input_term)) /* check it */
    input_string = string_val(input_term);
  else if (is_list(input_term)) {
    input_string = p_charlist_to_c_string(input_term, &input_buffer,
					  "RE_SUBSTRING", "input string");
    conversion_required = TRUE;
  } else
    xsb_abort("RE_SUBSTRING: Arg 1 (the input string) must be an atom or a character list");

  input_len = strlen(input_string);

  /* arg 2: beginning offset */
  beg_offset_term = reg_term(2);
  if (! is_int(beg_offset_term))
    xsb_abort("RE_SUBSTRING: Arg 2 (the beginning offset) must be an integer");
  beg_offset = int_val(beg_offset_term);
  if (beg_offset < 0 || beg_offset > input_len)
    xsb_abort("RE_SUBSTRING: Arg 2 (=%d) must be between 0 and %d",  
	      beg_offset, input_len);

  /* arg 3: ending offset */
  end_offset_term = reg_term(3);
  if (! is_int(end_offset_term))
    xsb_abort("RE_SUBSTRING: Arg 3 (the ending offset) must be an integer");
  end_offset = int_val(end_offset_term);
  if (end_offset < 0)
    end_offset = input_len;
  else if (end_offset > input_len || end_offset < beg_offset)
    xsb_abort("RE_SUBSTRING: Arg 3 (=%d) must be < 0 or between %d and %d",
	      end_offset, beg_offset, input_len);

  output_term = reg_term(4);
  if (! is_var(output_term))
    xsb_abort("RE_SUBSTRING: Arg 4 (the output string) must be an unbound variable");

  /* do the actual replacement */
  substring_len = end_offset-beg_offset;
  vstrAPPENDBLK(&output_buffer, input_string+beg_offset, substring_len);
  vstrNULL_TERMINATE(&output_buffer);
  
  /* get result out */
  if (conversion_required)
    c_string_to_p_charlist(output_buffer.string, output_term,
			   "RE_SUBSTITUTE", "Arg 4");
  else
    /* DO NOT intern. When atom table garbage collection is in place, then
       replace the instruction with this:
       	   c2p_string(output_buffer, output_term);
       The reason for not interning is that in Web page
       manipulation it is often necessary to process the same string many
       times. This can cause atom table overflow. Not interning allws us to
       circumvent the problem.  */
    ctop_string(4, output_buffer.string);
  
  return(TRUE);
}


/* should be removed when XSB gets garbage collector */
/* converts charlist to string, but doesn't intern */
static vstrDEFINE(temp_buffer);
bool do_regcharlist_to_string__(void)
{

  prolog_term input_term = reg_term(1);

  p_charlist_to_c_string(input_term, &temp_buffer,
			 "RE_CHARLIST_TO_STRING", "input string");
  ctop_string(2, temp_buffer.string);
  return TRUE;
}


/* 
   Takes REGEXP, BUFFER (the string where matches are to be found), and a
   Boolean flag IGNORECASE.
   PREMATCH will be assigned a pointer to a static string where the prefix
   before the matched substring will be stored. This string is allocated in
   this function. 
   POSTMATCH is a pointer to the string suffix adjacent to the end of the
   matching string. This points somewhere within BUFFER.
   MATCH_ARRAY gets a pointer to an array of type regmatch_t, which contains
   info on the matched string as well as the matched parenthetical
   subexpressions. 

   Returns: TRUE if matched, false, if not.
 */
#define ERR_MSG_LEN 100
static bool xsb_re_match(char *regexp_ptr, char *match_str, int ignorecase,
			 regmatch_t **match_array, int *paren_number)
{
  static regmatch_t matches[NMATCH];   /* the array where matches are stored */
  regex_t *compiled_re;
  int flags = (ignorecase ? (REG_EXTENDED | REG_ICASE) : REG_EXTENDED);
  int idx, err_code;
  char err_msg[ERR_MSG_LEN];

  *match_array = matches;

  idx = hash(regexp_ptr, 1, REGEXP_TBL_SIZE);
  /* we employ a very simple heuristic: either regexp is in the cell pointed to
     by hash or we replace what's in that cell with the current regexp.
     Probability of collision is low and the cost of replacement is low as
     well. */
  compiled_re = &regexp_tbl[idx].compiled;
  if ((regexp_tbl[idx].original == NULL)
      || (0 != strcmp(regexp_ptr, regexp_tbl[idx].original))
      || (regexp_tbl[idx].ignorecase != ignorecase)
      ) {
    /* need to recompile regexp */
    regexp_tbl[idx].original = regexp_ptr;
    regexp_tbl[idx].ignorecase = ignorecase;
    if (0 == (err_code = regcomp(&regexp_tbl[idx].compiled, regexp_ptr, flags)))
      regexp_tbl[idx].original = regexp_ptr;
    else {
      regerror(err_code, compiled_re, err_msg, ERR_MSG_LEN);
      xsb_abort("RE_MATCH: %s", err_msg);
    }
  }

  *paren_number = compiled_re->re_nsub;
  err_code = regexec(&regexp_tbl[idx].compiled, match_str, NMATCH, matches, 0);
  /* no match is not an error */
  if (err_code == REG_NOMATCH) return FALSE;

  if (err_code != 0) {
    regerror(err_code, compiled_re, err_msg, ERR_MSG_LEN);
    xsb_abort("RE_MATCH: %s", err_msg);
  }

  return TRUE;
}


void initialize_regexp_tbl()
{
  int i;
  first_call = FALSE;
  for (i=0; i<NMATCH; i++) {
    regexp_tbl[i].original = NULL;
    regexp_tbl[i].ignorecase = FALSE;
  }
}

