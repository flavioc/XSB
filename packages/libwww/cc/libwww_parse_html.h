/* File:      libwww_parse_html.h
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


/* special tag type that we use to wrap around text */
#define PCDATA_SPECIAL 	  -77

#define MAX_HTML_NESTING  70
struct _HText {
  HTRequest *		  request;
  HTParentAnchor * 	  node_anchor; 	   /* not used */
  HTStream *		  target;
  SGML_dtd *		  dtd;
  int	 	     	  suppress_is_default; /* whether we begin parsing by
						  suppressing tags */
  prolog_term	     	  parsed_term;      /* actual result of the parse */
  prolog_term	     	  parsed_term_tail; /* auxil variable */
  int   		  stackptr;
  struct stack_node {
    int	       	   element_number;    /* which element this is  */
    SGMLContent    element_type;      /* SGML_EMPTY, PCDATA_SPECIAL, normal */
    int	       	   suppress;   	      /* whether this element is in the
					 suppressed region */
    prolog_term	   elt_term;	      /* here we build elements */
    prolog_term    content_list_tail; /* auxil var to help build elements */
  } 	    	    	  stack[MAX_HTML_NESTING]; /* keeps nested elements */
};
typedef HText  USERDATA;


/* function declarations */

PRIVATE inline HTTag *special_find_tag(USERDATA *htext, int element_number);

PRIVATE USERDATA *create_userData( HTRequest         *request,
				   HTParentAnchor    *anchor,
				   HTStream          *output_stream);
PRIVATE BOOL delete_userData(USERDATA *me);

PRIVATE void setup_html_request_structure (prolog_term prolog_req, int req_id);
PRIVATE int find_matching_elt(USERDATA *htext, int elt_number);

PRIVATE void html_push_element (USERDATA        *htext,
				int             element_number,
				const BOOL      *present,
				const char     **value);
PRIVATE void html_pop_element(USERDATA *htext);
PRIVATE void html_push_suppressed_element(USERDATA *htext, int element_number);
PRIVATE void html_pop_suppressed_element(USERDATA *htext);
PRIVATE void collect_html_attributes ( prolog_term  elt_term,
				  HTTag        *tag,
				  const BOOL   *present,
				  const char  **value);


PRIVATE void html_addText (USERDATA *htext, const char *textbuf, int len);
PRIVATE void html_beginElement(USERDATA  	*htext,
			       int	        element_number,
			       const BOOL       *present,
			       const char       **value);
PRIVATE void html_endElement(USERDATA *htext, int element_number);

/*-------------------*/
enum http_method {FORM_GET, FORM_POST};
typedef enum http_method HTTP_METHOD;

PRIVATE HTAssocList *get_form_params(prolog_term form_params);
PRIVATE HTTP_METHOD get_request_method(prolog_term method);
/*-------------------*/

/* hash table stuff */
typedef int HKEY;
#define HTABLE_CELL_INITIALIZER       -1
#define HASH(item) 	    	      item
#define SET_HASH_CELL(cell,item)      cell=item
#define HASH_CELL_EQUAL(cell,item)    cell==item
