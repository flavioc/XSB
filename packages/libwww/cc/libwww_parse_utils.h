/* File:      libwww_parse_utils.h
** Author(s): kifer, Yang Yang
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 2000
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


/*
#define LIBWWW_DEBUG_VERBOSE
#define LIBWWW_DEBUG
#define LIBWWW_DEBUG_TERSE
*/
#ifdef LIBWWW_DEBUG_VERBOSE
#define LIBWWW_DEBUG
#endif
#ifdef LIBWWW_DEBUG
#define LIBWWW_DEBUG_TERSE
#endif


/* This error code is used in this package only -- not an HTTP error */
#define HT_DOC_SYNTAX	     	    -555

#define SELECTED_TAGS_TBL_SIZE	    29
#define SUPPRESSED_TAGS_TBL_SIZE    41
#define STRIPPED_TAGS_TBL_SIZE	    37

#define DEFAULT_TIMEOUT	      	  7000 /* 7 sec */


/* from HTTP.c */
#define FREE_TARGET(t)	(*(t->target->isa->_free))(t->target)

/* Must define this, since HTStream is just a name aliased to _HTStream */
struct _HTStream {
    const HTStreamClass *	isa;
};

#define STACK_TOP(htext)        htext->stack[htext->stackptr]
#define STACK_PREV(htext)       htext->stack[htext->stackptr-1]

#define suppressing(htext) \
     	     	     ( (htext->stackptr < 0 && htext->suppress_is_default) \
      	      	       ||(htext->stackptr >= 0 && STACK_TOP(htext).suppress))
#define parsing(htext)  (!suppressing(htext))


struct hash_table {
  int size;
  HKEY *table;
};
typedef struct hash_table HASH_TABLE;


/* used to pass the input info to request and get output info from request back
   to the Prolog side*/
struct request_context {
  int  request_id;
  int  suppress_is_default;
  void *userdata;
  /* input */
  HASH_TABLE selected_tags_tbl;
  HASH_TABLE suppressed_tags_tbl;
  HASH_TABLE stripped_tags_tbl;
  /* output */
  prolog_term parsed_result;
  prolog_term status_term;
};
typedef struct request_context REQUEST_CONTEXT;

#define REQUEST_ID(request) \
  ((REQUEST_CONTEXT *)HTRequest_context(request))->request_id


#ifdef LIBWWW_DEBUG_VERBOSE
PRIVATE void print_prolog_term(prolog_term term, char *message);
#endif


#define IS_SELECTED_TAG(element, request) \
    is_in_htable(element, \
    	    	 &(((REQUEST_CONTEXT *)HTRequest_context(request))->selected_tags_tbl))
#define IS_SUPPRESSED_TAG(element, request) \
    is_in_htable(element, \
    	    	 &(((REQUEST_CONTEXT *)HTRequest_context(request))->suppressed_tags_tbl))
#define IS_STRIPPED_TAG(element, request) \
    is_in_htable(element, \
    	    	 &(((REQUEST_CONTEXT *)HTRequest_context(request))->stripped_tags_tbl))

/* like strcpy, but also converts to lowercase */
PRIVATE void strcpy_lower(char *to, const char *from);


PRIVATE void set_request_context(HTRequest *request,
				 prolog_term prolog_req,
				 int req_id,
				 char *caller);
PRIVATE void free_request_context (REQUEST_CONTEXT *context);

PRIVATE void init_tag_table(prolog_term tag_list, HASH_TABLE *tag_tbl);
PRIVATE void init_htable(HASH_TABLE *htable, int size, char *caller);
PRIVATE int add_to_htable(HKEY item, HASH_TABLE *htable);
PRIVATE void free_htable(HASH_TABLE *htable);
PRIVATE int is_in_htable(const HKEY item, HASH_TABLE *htable);


PRIVATE int general_parse_abort_handler (HTRequest  *request,
					 HTResponse *response,
					 void 	    *param,
					 int 	    status);

