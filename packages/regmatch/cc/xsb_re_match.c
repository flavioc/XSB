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
				    int outstring_size,
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
			 char **prematch, char **postmatch,
			 regmatch_t **match_array, int *paren_number);
static void initialize_regexp_tbl(void);
static void get_match(int i, char *buf, int bufsize, char *input_string, regmatch_t *match_array);

static int first_call = TRUE; /* whether this is the first call to the regexp
				 matcher. Used to initialize the regexp tbl */
/* XSB regular expression matcher entry point
   In:
       Arg1: regexp
       Arg2: string
       Arg3: ignorecase
   Out:
       Arg4: matched substring
       Arg5: prefix before the matched substring
       Arg6: suffix after the matched substring
       Arg7: list of matches against parenthesized expressions
*/
bool do_regmatch__(void)
{
  prolog_term listOfMatches, listHead, listTail;
  /* Prolog args are first assigned to these, so we could examine the types
     of these objects to determine if we got strings or atoms. */
  prolog_term regexp_term, input_string_term,
    match_term, prematch_term, postmatch_term;
  int i;
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
  char temp_buf[MAXBUFSIZE];
  char regexp_buffer[MAXBUFSIZE], input_string_buffer[4*MAXBUFSIZE];


  if (first_call)
    initialize_regexp_tbl();

  regexp_term = reg_term(1); /* Arg1: regexp */
  input_string_term = reg_term(2); /* Arg2: string to find matches in */
  /* If arg 3 is bound to anything, then consider this as ignore case flag */
  if (! is_var(reg_term(3)))
    ignorecase = TRUE;
  match_term = reg_term(4);
  if (! is_var(match_term))
    xsb_abort("RE_MATCH: Arg 4 (the matched substring) must be an unbound variable");
  prematch_term = reg_term(5);
  if (! is_var(prematch_term))
    xsb_abort("RE_MATCH: Arg 5 (prefix of the matched substring) must be an unbound variable");
  postmatch_term = reg_term(6);
  if (! is_var(postmatch_term))
    xsb_abort("RE_MATCH: Arg 6 (suffix of the matched substring) must be an unbound variable");
  /* Arg5: List of parenthetical matches to return */
  listTail = listOfMatches = reg_term(7);
  if (! is_var(listTail))
    xsb_abort("RE_MATCH: Arg 7 (list of parenthetical matches) must be an unbound variable");

  /* check regular expression */
  if (is_string(regexp_term))
    regexp_ptr = string_val(regexp_term);
  else if (is_list(regexp_term))
    regexp_ptr = p_charlist_to_c_string(regexp_term, regexp_buffer,
					sizeof(regexp_buffer),
					"RE_MATCH", "regular expression");
  else
    xsb_abort("RE_MATCH: Regular expression (Arg 1) must be an atom or a character list");

  /* check string to be matched */
  if (is_string(input_string_term))
    input_string = string_val(input_string_term);
  else if (is_list(input_string_term)) {
    input_string = p_charlist_to_c_string(input_string_term,
					  input_string_buffer,
					  sizeof(input_string_buffer),
					  "RE_MATCH", "input string");
    conversion_required = TRUE;
  } else
    xsb_abort("RE_MATCH: Input string (Arg 2) must be an atom or a character list");

  /* paren_number gets the # of parenthetical subexpressions (not 1 minus!) */
  return_code = xsb_re_match(regexp_ptr, input_string, ignorecase,
			     &prematch_ptr, &postmatch_ptr, 
			     &match_array, &paren_number);

  if (! return_code) return FALSE;

  get_match(0, temp_buf, sizeof(temp_buf), input_string, match_array);

  if (conversion_required) {
    c2p_list(match_term); /* make it into a list */
    c2p_list(prematch_term);
    c2p_list(postmatch_term);
    c_string_to_p_charlist(temp_buf, match_term, "RE_MATCH", "arg 4");
    c_string_to_p_charlist(prematch_ptr, prematch_term,
			   "RE_MATCH", "arg 5");
    c_string_to_p_charlist(postmatch_ptr, postmatch_term,
			   "RE_MATCH", "arg 6");
  } else {
    c2p_string(temp_buf, match_term);
    c2p_string(prematch_ptr, prematch_term);
    c2p_string(postmatch_ptr, postmatch_term);
  }

  /* return result */
  for (i=1; i <= paren_number; i++) {
    c2p_list(listTail); /* make it into a list */
    listHead = p2p_car(listTail); /* get head of the list */

    get_match(i, temp_buf, sizeof(temp_buf), input_string, match_array);

    /* set list head */
    if (conversion_required)
      c_string_to_p_charlist(temp_buf, listHead, "RE_MATCH", "arg 7");
    else
      c2p_string(temp_buf, listHead);

    listTail = p2p_cdr(listTail);
  }

  c2p_nil(listTail); /* bind tail to nil */
  return TRUE;
}


/* XSB regexp substitution entry point
   In: 
       Arg1: regexp
       Arg2: string
       Arg3: ignorecase
       Arg4: match spec---whether to replace match, prematch, postmatch, or
			   parenmatch 
       Arg5: substitution string
   Out:
       Arg6: new (output) string
   Returns FALSE if no match or the specified parenthetical expression is out
   of range.
*/
int do_regsubst__(void)
{
  /* Prolog args are first assigned to these, so we could examine the types
     of these objects to determine if we got strings or atoms. */
  prolog_term regexp_term, input_string_term, output_string_term,
    which_match_term, subst_string_term;
  char *regexp_ptr=NULL;      /* regular expression ptr	       	      */
  char *input_string=NULL;    /* string where matches are to be found */
  int conversion_required = FALSE; /* Tell Prolog level whether atom-to-list
				      conversion is required for the result.
				      Conversion is required if input string is
				      a character list. We then assume that the
				      user expects output in the same form. */
  char *prematch_ptr, *postmatch_ptr, *subst_string=NULL;
  char *which_match_str;
  char which_match=0;
  int ignorecase=FALSE;
  int return_code, paren_number;
  regmatch_t *match_array;
  /* output buffer should be large enough to include the input string and the
     substitution string <= sizeof(temp_buf) */
  char temp_buf[MAXBUFSIZE], output_buffer[5*MAXBUFSIZE];
  char regexp_buffer[MAXBUFSIZE], input_string_buffer[4*MAXBUFSIZE];

  
  if (first_call)
    initialize_regexp_tbl();

  regexp_term = reg_term(1); /* Arg1: regexp */
  input_string_term = reg_term(2); /* Arg2: string to find matches in */
  /* If arg 3 is bound to anything, then consider this as ignore case flag */
  if (! is_var(reg_term(3)))
    ignorecase = TRUE;

  /* handle substitution string */
  subst_string_term = reg_term(5);
  if (is_string(subst_string_term)) {
    subst_string = string_val(subst_string_term);
  } else if (is_list(subst_string_term)) {
    subst_string = p_charlist_to_c_string(subst_string_term, temp_buf,
					  sizeof(temp_buf),
					  "RE_SUBST", "substitution string");
  } else {
    xsb_abort("RE_SUBST: Arg 5 (the substitution string) must be an atom or a list of characters");
  }

  output_string_term = reg_term(6);
  if (! is_var(output_string_term))
    xsb_abort("RE_SUBST: Arg 6 (the output string) must be an unbound variable");

    /* check regular expression */
  if (is_string(regexp_term))
    regexp_ptr = string_val(regexp_term);
  else if (is_list(regexp_term))
    regexp_ptr = p_charlist_to_c_string(regexp_term, regexp_buffer,
					sizeof(regexp_buffer),
					"RE_SUBST", "regular expression");
  else
    xsb_abort("RE_SUBST: Regular expression (Arg 1) must be an atom or a character list");

  /* check string to be matched */
  if (is_string(input_string_term))
    input_string = string_val(input_string_term);
  else if (is_list(input_string_term)) {
    input_string = p_charlist_to_c_string(input_string_term,
					  input_string_buffer,
					  sizeof(input_string_buffer),
					  "RE_MATCH", "input string");
    conversion_required = TRUE;
  } else
    xsb_abort("RE_SUBST: Input string (Arg 2) must be an atom or a character list");

  /* paren_number gets the # of parenthetical subexpressions (not 1 minus!) */
  return_code = xsb_re_match(regexp_ptr, input_string, ignorecase,
			     &prematch_ptr, &postmatch_ptr, 
			     &match_array, &paren_number);

  if (! return_code) return FALSE;

  which_match_term = reg_term(4);
  if (is_string(which_match_term)) {
    which_match_str = string_val(which_match_term);
    which_match = (int) which_match_str[0];
  } else if (is_int(which_match_term)) {
    which_match = int_val(which_match_term);
    if (which_match < 0 || which_match > paren_number) {
      xsb_warn("RE_SUBST: Arg 4 (=%d): invalid parenthetical subexpressions number", which_match);
      return(FALSE);
    }
  } else
    xsb_abort("RE_SUBST: Arg 4 must be 0, `-', `+', or a number specifying a parenthetical match");

  /* do the actual replacement; use tmp_buf */
  switch (which_match) {
  case '+': /* replace postmatch */
    strncpy(output_buffer, input_string, postmatch_ptr-input_string);
    strcpy(output_buffer+(postmatch_ptr-input_string), subst_string);
    break;
  case '-': /* replace prematch */
    strcpy(output_buffer, subst_string);
    strcat(output_buffer, GET_MATCH_PTR(0));
  default:  /* replace the exact match or a parenthetical match */
    strncpy(output_buffer, input_string,
	    GET_MATCH_PTR(which_match) - input_string);
    strcpy(output_buffer + (GET_MATCH_PTR(which_match) - input_string),
	   subst_string);
    strcat(output_buffer,
	   GET_MATCH_PTR(which_match) + GET_MATCH_SIZE(which_match));
  }

  /* get result out */
  if (conversion_required) {
    c2p_list(output_string_term); /* make it into a list */
    c_string_to_p_charlist(output_buffer, output_string_term,
			   "RE_SUBST", "arg 5");
  } else
    c2p_string(output_buffer, output_string_term);
  
  return(TRUE);
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

  /* matches[0].rm_so contains offset to the beginning of the matched string */
  strncpy(prematch_str, match_str, matches[0].rm_so);

  /* matches[0].rm_eo contains offset to the end of the matched string */
  *postmatch = match_str+matches[0].rm_eo;

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

/* extract the I-th match from match array and put it into BUFFER */
static void get_match(int i, char *buff, int buff_size, char *input_string, regmatch_t *match_array) 
{
  int len = match_array[i].rm_eo - match_array[i].rm_so;

  if (len >= (buff_size-1)) {
    len = buff_size - 2;
    xsb_warn("Regular expression matches a substring that exceeds the maximum allowed");
  }

  strncpy(buff, input_string+match_array[i].rm_so, len);
  *(buff + len) = '\0';
}
