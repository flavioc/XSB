/* File:      libwww_parse_xml.h
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


#define LIBWWW_DEBUG_VERBOSE
/*
#define LIBWWW_DEBUG_TERSE
#define LIBWWW_DEBUG
*/
#ifdef LIBWWW_DEBUG_VERBOSE
#define LIBWWW_DEBUG
#endif
#ifdef LIBWWW_DEBUG
#define LIBWWW_DEBUG_TERSE
#endif


#define MAX_XML_TAG_OR_ATTR_SIZE    100

#define DESTROY_HASH_TABLE hashTableDestroy 

#define MAX_XML_NESTING  170
struct XML_userData {
  XML_Parser 	          parser; 
  HTRequest *		  request;
  HTStream *		  target;
  SGML_dtd *		  dtd;
  int 	      	      	  status;    	    /* status of the HTTP request */
  int	 	     	  suppress_is_default; /* whether we begin parsing by
						  suppressing tags */
  prolog_term	     	  parsed_term;      /* actual result of the parse */
  prolog_term	     	  parsed_term_tail; /* auxil variable */
  int   		  stackptr;
  struct stack_node {
    XML_Char	   *tag;              /* which element this is  */
    int	       	   suppress;   	      /* whether this element is in the
					 suppressed region */
    prolog_term	   elt_term;	      /* here we build elements */
    prolog_term    content_list_tail; /* auxil var to help build elements */
  } 	    	    	  stack[MAX_XML_NESTING]; /* keeps nested elements */
};
typedef struct XML_userData XML_USERDATA;


/* function declarations */

PRIVATE XML_USERDATA *create_userData(XML_Parser parser,
				      HTRequest  *request,
				      HTStream   *target_stream);
PRIVATE void delete_userData(XML_USERDATA *me);

PRIVATE void setup_xml_request_structure (prolog_term prolog_req, int req_id);

PRIVATE void xml_push_element (XML_USERDATA    *userdata,
			       const XML_Char  *tag,
			       const XML_Char  **attrs);
PRIVATE void xml_pop_element(XML_USERDATA *userdata);
PRIVATE void xml_push_suppressed_element(XML_USERDATA   *userdata,
					 const XML_Char *tag);
PRIVATE void xml_pop_suppressed_element(XML_USERDATA *userdata);
PRIVATE void collect_xml_attributes (prolog_term     elt_term,
				     const XML_Char  **attrs);


PRIVATE void xml_addText (void 	         *userdata,
			  const XML_Char *textbuf,
			  int 	      	 len);
PRIVATE void xml_beginElement(void  	     *userdata,
			      const XML_Char *tag,
			      const XML_Char **attributes);
PRIVATE void xml_endElement(void *userdata, const XML_Char *tag);
PRIVATE int xml_parse_termination_handler(HTRequest    *request,
					  HTResponse   *response,
					  void 	       *param,
					  int          status);
PRIVATE void xml_libwww_abort_request(HTRequest *request, int status,
				      char *description, ...);

PRIVATE void HTXML_newInstance (HTStream *		me,
				HTRequest *		request,
				HTFormat 		target_format,
				HTStream *		target_stream,
				XML_Parser              xmlparser,
				void * 			context);

PRIVATE void xml_processingInstruction (void 	       *userData,
					const XML_Char *target,
					const XML_Char *data);
PRIVATE void xml_unparsedEntityDecl (void     	    *userData,
				     const XML_Char *entityName,
				     const XML_Char *base,
				     const XML_Char *systemId,
				     const XML_Char *publicId,
				     const XML_Char *notationName);
PRIVATE void xml_notationDecl (void 	      *userData,
			       const XML_Char *notationName,
			       const XML_Char *base,
			       const XML_Char *systemId,
			       const XML_Char *publicId);
PRIVATE int xml_externalEntityRef (XML_Parser 	  parser,
				   const XML_Char *openEntityNames,
				   const XML_Char *base,
				   const XML_Char *systemId,
				   const XML_Char *publicId);
PRIVATE int xml_unknownEncoding (void 	      	*encodingHandlerData,
				 const XML_Char *name,
				 XML_Encoding   *info);
/*
PRIVATE void xml_default (void * userData, const XML_Char * s, int len);
*/

PRIVATE int is_in_htable(const char *item, HASH_TABLE *htable);
PRIVATE void add_to_htable(char *tagname, HASH_TABLE *tag_tbl);
