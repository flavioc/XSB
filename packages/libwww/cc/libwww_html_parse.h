/* File:      libwww_html_parse.h
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
#define LIBWWW_DEBUG
*/
#ifdef LIBWWW_DEBUG_VERBOSE
#define LIBWWW_DEBUG
#endif


#define MAX_TAG_OR_ATTR_SIZE  75   /* max size of an HTML tag or attribute */
#define DEFAULT_TIMEOUT	      5000 /* 5 sec */

struct hash_table {
  int size;
  /* hash table is one big chunk, not an array */
  int *table;
};
typedef struct hash_table HASH_TABLE;

#define SELECTED_TAGS_TBL_SIZE	    29     /* must be > MAX_SELECTED_TAGS */
#define IGNORED_TAGS_TBL_SIZE	    41      /* must be > MAX_EXCLUDED_TAGS */

/* from HTTP.c */
#define FREE_TARGET(t)	(*(t->target->isa->_free))(t->target)

/* Must define this, since HTStream is just a name aliased to _HTStream */
struct _HTStream {
    const HTStreamClass *	isa;
};


/* used to pass the input info to request and get output info from request back
   to the Prolog side*/
struct request_context {
  int request_id;
  /* input */
  int ignore_is_default;
  HASH_TABLE selected_tags_tbl;
  HASH_TABLE ignored_tags_tbl;
  /* output */
  prolog_term parsed_result;
  prolog_term status_term;
};
typedef struct request_context REQUEST_CONTEXT;

#define REQUEST_ID(request) \
  ((REQUEST_CONTEXT *)HTRequest_context(request))->request_id


#define MAX_HTML_NESTING  70
struct _HText {
  HTRequest *		  request;    	   /* not used */
  HTParentAnchor * 	  node_anchor; 	   /* not used */
  HTStream *		  target; 	   /* not used */
  SGML_dtd *		  dtd;
  int 	      	      	  status;    	    /* status of the HTTP request */
  int	 	     	  ignore_is_default; /* whether we begin parsing by
						ignoring tags */
  prolog_term	     	  parsed_term;      /* actual result of the parse */
  prolog_term	     	  parsed_term_tail; /* auxil variable */
  int   		  stackptr;
  struct stack_node {
    int	       	   element_number;    /* which element this is  */
    SGMLContent    element_type;      /* SGML_EMPTY, PCDATA_SPECIAL, normal */
    int	       	   ignore;    	      /* whether this element is in the ignored
					 region */
    prolog_term	   elt_term;	      /* here we build elements */
    prolog_term    content_list_tail; /* auxil var to help build elements */
  } 	    	    	  stack[MAX_HTML_NESTING]; /* keeps nested elements */
};

#define STACK_TOP(htext)        htext->stack[htext->stackptr]
#define STACK_PREV(htext)       htext->stack[htext->stackptr-1]

#define ignoring(htext)  (  (htext->stackptr < 0 && htext->ignore_is_default) \
      	      	      	  ||(htext->stackptr >= 0 && STACK_TOP(htext).ignore))
#define parsing(htext)  (! ignoring(htext))

/* special tag type that we use to wrap around text */
#define PCDATA_SPECIAL 	        -77

/* function declarations */

/* hash table maintenance */
PRIVATE int is_in_htable(int item, HASH_TABLE *htable);
PRIVATE int add_to_htable(int item, HASH_TABLE *htable);
PRIVATE void init_htable(HASH_TABLE *htable, int size);
PRIVATE void init_tag_table(prolog_term tag_list, HASH_TABLE *tag_tbl);
PRIVATE void free_htable(HASH_TABLE *htable);

PRIVATE void get_form_params(prolog_term form_params);
PRIVATE void set_request_context(HTRequest *request,
				 prolog_term prolog_req, int req_id);
PRIVATE void free_request_context (REQUEST_CONTEXT *context);

/* like strcpy, but also converts to lowercase */
void strcpy_lower(char *to, char *from);
PRIVATE inline HTTag *special_find_tag(HText *htext, int element_number);

PRIVATE HText *create_HText_obj( HTRequest         *request,
				 HTParentAnchor    *anchor,
				 HTStream          *output_stream);
PRIVATE BOOL delete_HText_obj(HText *me);

PRIVATE void setup_request_structure (prolog_term prolog_req, int req_id);
PRIVATE int find_matching_elt(HText *htext, int elt_number);

PRIVATE void push_element (HText        *htext,
			   int          element_number,
			   const BOOL   *present,
			   const char  **value);
PRIVATE void pop_element(HText *htext);
PRIVATE void push_ignore_element(HText *htext, int element_number);
PRIVATE void pop_ignore_element(HText *htext);
PRIVATE void collect_attributes ( prolog_term  elt_term,
				  HTTag        *tag,
				  const BOOL   *present,
				  const char  **value);


PRIVATE void addText (HText *htext, const char *textbuf, int len);
PRIVATE void beginElement(HText  	*htext, /* document */
			  int		element_number, /* internal tag # */
			  /* bitmap that tells which tag attrs are present */
			  const BOOL 	*present,
			  /* array of values (strings) for the attributes that
			     are specified by the "present" bitmap */ 
			  const char   **value);
PRIVATE void endElement(HText *htext, int element_number);
PRIVATE int parse_termination_handler(HTRequest  *request,
				      HTResponse *response,
				      /* param is set to be the HText object
					 associated with request */
				      void 	  *param,
				      int     	  status);

#define IS_SELECTED_TAG(element_number, htext) \
    is_in_htable(element_number, \
    	    	 &(((REQUEST_CONTEXT *)HTRequest_context(htext->request))->selected_tags_tbl))
#define IS_IGNORED_TAG(element_number, htext) \
    is_in_htable(element_number, \
    	    	 &(((REQUEST_CONTEXT *)HTRequest_context(htext->request))->ignored_tags_tbl))


#ifdef LIBWWW_DEBUG
PRIVATE void print_prolog_term(prolog_term term, char *message);
#endif
