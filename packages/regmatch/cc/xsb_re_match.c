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
#include "cell.h"
#include "xsberror.h"
#include "cinterf.h"
#include "heap.h"

extern char *p_charlist_to_c_string(prolog_term term, char *outstring,
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
#define GET_MATCH(i, where) \
    	    strncpy(where, \
    	    	    input_string+match_array[i].rm_so, \
    	    	    match_array[i].rm_eo-match_array[i].rm_so); \
    	    *(where+match_array[i].rm_eo-match_array[i].rm_so) = '\0'

/* cache table for compiled regular expressions */
struct regexp_tbl_entry {
  int     ignorecase;	     	     	/* whether case should be ignored */
  char    *original;	    	    	/* the original regexp */
  regex_t compiled;	    	    	/* the compiled regexp */
};

struct regexp_tbl_entry regexp_tbl[REGEXP_TBL_SIZE];

static bool xsb_re_match(char *regexp_ptr, char* match_str, int ignorecase,
			 char **prematch, char **postmatch,
			 regmatch_t **match_array, int *paren_number);

static int first_call = TRUE; /* whether this is the first call to the regexp
				 matcher. Used to initialize the regexp tbl */
static char temp_buf[MAXBUFSIZE];

/* XSB regular expression matcher entry point */
bool do_regmatch__(void)
{
  prolog_term listOfMatches, listHead, listTail;
  /* Prolog args are first assigned to these, so we could examine the types
     of these objects to determine if we got strings or atoms. */
  prolog_term regexp_term, input_string_term,
    match_term, prematch_term, postmatch_term;
  int i;
  static char regexp_buffer[MAXBUFSIZE], input_string_buffer[MAXBUFSIZE];
  char *regexp_ptr=NULL;      /* regular expression ptr	       	      */
  char *input_string=NULL;    /* string where matches are to be found */
  int conversion_required = FALSE; /* Tell Prolog level whether atom-to-list
				      conversion is required for the result.
				      Conversion is required if input string is
				      a character list. We then assume that the
				      user expects output in the same form. */
  char *prematch_ptr, *postmatch_ptr;
  int ignorecase=FALSE;
  int return_code, paren_number;
  regmatch_t *match_array;

  if (first_call) {
    /* initialize the regexp table */
    first_call = FALSE;
    for (i=0; i<NMATCH; i++) {
      regexp_tbl[i].original = NULL;
      regexp_tbl[i].ignorecase = FALSE;
    }
  }

  regexp_term = reg_term(1); /* Arg1: regexp */
  input_string_term = reg_term(2); /* Arg2: string to find matches in */
  /* If arg 3 is bound to anything, then consider this as ignore case flag */
  if (! is_var(reg_term(3)))
    ignorecase = TRUE;
  match_term = reg_term(4);
  if (! is_var(match_term))
    xsb_abort("REGMATCH: Argument 4 (the matched substring) must be an unbound variable");
  prematch_term = reg_term(5);
  if (! is_var(prematch_term))
    xsb_abort("REGMATCH: Argument 5 (prefix of the matched substring) must be an unbound variable");
  postmatch_term = reg_term(6);
  if (! is_var(postmatch_term))
    xsb_abort("REGMATCH: Argument 6 (suffix of the matched substring) must be an unbound variable");
  /* Arg5: List of parenthetical matches to return */
  listTail = listOfMatches = reg_term(7);
  if (! is_var(listTail))
    xsb_abort("REGMATCH: Argument 7 (list of parenthetical matches) must be an unbound variable");

  /* check regular expression */
  if (is_string(regexp_term))
    regexp_ptr = string_val(regexp_term);
  else if (is_list(regexp_term))
    regexp_ptr = p_charlist_to_c_string(regexp_term, regexp_buffer,
					"REGMATCH", "regular expression");
  else
    xsb_abort("REGMATCH: Regular expression (Arg 1) must be an atom or a character list");

  /* check string to be matched */
  if (is_string(input_string_term))
    input_string = string_val(input_string_term);
  else if (is_list(input_string_term)) {
    input_string = p_charlist_to_c_string(input_string_term,
					  input_string_buffer,
					  "REGMATCH", "input string");
    conversion_required = TRUE;
  } else
    xsb_abort("REGMATCH: Input string (Arg 2) must be an atom or a character list");

  /* paren_number gets the # of parenthetical subexpressions (not 1 minus!) */
  return_code = xsb_re_match(regexp_ptr, input_string, ignorecase,
			     &prematch_ptr, &postmatch_ptr, 
			     &match_array, &paren_number);

  if (! return_code) return FALSE;

  GET_MATCH(0, temp_buf);

  if (conversion_required) {
    c2p_list(match_term); /* make it into a list */
    c2p_list(prematch_term);
    c2p_list(postmatch_term);
    c_string_to_p_charlist(temp_buf, match_term, "REGMATCH", "arg 4");
    c_string_to_p_charlist(prematch_ptr, prematch_term,
			   "REGMATCH", "arg 5");
    c_string_to_p_charlist(postmatch_ptr, postmatch_term,
			   "REGMATCH", "arg 6");
  } else {
    c2p_string(temp_buf, match_term);
    c2p_string(prematch_ptr, prematch_term);
    c2p_string(postmatch_ptr, postmatch_term);
  }

  /* return result */
  for (i=1; i <= paren_number; i++) {
    c2p_list(listTail); /* make it into a list */
    listHead = p2p_car(listTail); /* get head of the list */

    GET_MATCH(i, temp_buf);

    /* set list head */
    if (conversion_required)
      c_string_to_p_charlist(temp_buf, listHead, "REGMATCH", "arg 7");
    else
      c2p_string(temp_buf, listHead);

    listTail = p2p_cdr(listTail);
  }

  c2p_nil(listTail); /* bind tail to nil */
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
			 char **prematch, char **postmatch, 
			 regmatch_t **match_array, int *paren_number)
{
  static regmatch_t matches[NMATCH];   /* the array where matches are stored */
  static char prematch_str[MAXBUFSIZE];
  regex_t *compiled_re;
  int flags = (ignorecase ? (REG_EXTENDED | REG_ICASE) : REG_EXTENDED);
  int idx, err_code;
  char err_msg[ERR_MSG_LEN];

  *prematch = prematch_str;
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
      xsb_abort("REGMATCH: %s", err_msg);
    }
  }

  *paren_number = compiled_re->re_nsub;
  err_code = regexec(&regexp_tbl[idx].compiled, match_str, NMATCH, matches, 0);
  /* no match is not an error */
  if (err_code == REG_NOMATCH) return FALSE;

  if (err_code != 0) {
    regerror(err_code, compiled_re, err_msg, ERR_MSG_LEN);
    xsb_abort("REGMATCH: %s", err_msg);
  }

  /* matches[0].rm_so contains offset to the beginning of the matched string */
  strncpy(prematch_str, match_str, matches[0].rm_so);

  /* matches[0].rm_eo contains offset to the end of the matched string */
  *postmatch = match_str+matches[0].rm_eo;

  return TRUE;
}
