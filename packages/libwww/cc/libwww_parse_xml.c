/* File:      libwww_parse_xml.c
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


#include "WWWLib.h"
#include "WWWHTTP.h"
#include "WWWInit.h"
#include "HTAABrow.h"
#include "WWWApp.h"
#include "WWWXML.h"
#include "HTUtils.h"
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "basictypes.h"
#include "basicdefs.h"
#include "auxlry.h"
#include "configs/xsb_config.h"
#include "configs/special.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "cinterf.h"
#include "varstring_xsb.h"


static int total_number_of_requests;


#include "libwww_parse_xml.h"
#include "libwww_parse_utils.h"
#include "libwww_parse_utils.c"


/* BOOL, PRIVATE, PUBLIC, etc., are defined in a Libwww header */

/* Calling sequence:
       libwww_parse_xml([req1,req2,...])

   Each req: f(URL, FORM-Params, SELECTED-Tags, PARSED-Result, ERROR-Code)
   SELECTED-Tags:
       	     _ (all tags),
	     f(chosen-tag-list,suppressed-tag-list,stripped-tag-list) means:
	     	    parse only inside
		    the tags on the chosen tag list. Stop parsing if a
		    suppressed tag is found. Resume if a chosen tag is found,
		    etc. 
		    Stripped tags are those that just get discarded.
	     f(_,suppressed-tag-list,...) means: parse all tags except those in
		    the suppressed tags list.
	     f(chosen-tag-list,_,...) means: parse only inside the chosen tags.
 */
BOOL  libwww_parse_xml(void)
{
  prolog_term request_term_list = reg_term(1), request_list_tail;
  int request_id=0;

  /* Create a new premptive client */
  HTProfile_newHTMLNoCacheClient ("XML Parser", "1.0");
  HTAlert_setInteractive(NO);
  HTHost_setEventTimeout(DEFAULT_TIMEOUT);

  /* use abort here, because this is a programmatic mistake */
  if (!is_list(request_term_list))
    xsb_abort("LIBWWW_PARSE_XML: Argument must be a list");

  request_list_tail = request_term_list;
  total_number_of_requests=0;
  while (is_list(request_list_tail) && !is_nil(request_list_tail)) {
    request_id++;
    setup_xml_request_structure(p2p_car(request_list_tail), request_id);
    request_list_tail = p2p_cdr(request_list_tail);
    total_number_of_requests = request_id;
  }

  /* start the event loop and begin to parse all requests in parallel */
  if (total_number_of_requests > 0) {
    int status;

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In libwww_parse_xml: starting event loop. Total requests=%d",
	     total_number_of_requests);
#endif

    status = HTEventList_newLoop();
    if (status != HT_OK)
      xsb_abort("LIBWWW_PARSE_XML: Couldn't launch HTTP request");

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In libwww_parse_xml: event loop ended");
#endif
  }
  
  /* don't delete the profile: it crashes libwww on the second try */
  return TRUE;
}


/* Sets up the libwww request structure for the request specified in
   PROLOG_REQ, including the request context, which contains the info about the
   return parameters.
   Note that we use xsb_abort here instead of abort handlers, because the error
   conditions handled here are programmatic mistakes rather than network
   conditions. */
PRIVATE void setup_xml_request_structure(prolog_term prolog_req, int request_id)
{
  int	      status;
  HTAnchor    *anchor = NULL;
  HTRequest   *request;
  char 	      *uri = NULL;

  /* Register our new XML Instance handler */
  HTXMLCallback_registerNew(HTXML_newInstance, NULL);

  /* get URL */
  uri=string_val(p2p_arg(prolog_req,1));
  if (uri == NULL)
    xsb_abort("LIBWWW_PARSE_XML: Arg 1(URI) is invalid");

  /* Create a new request and attach the context structure to it */
  request = HTRequest_new();
  set_request_context(request, prolog_req, request_id, "LIBWWW_PARSE_XML");

  uri = HTParse(uri, NULL, PARSE_ALL);
  /* Create a new Anchor */
  anchor = HTAnchor_findAddress(uri);
  /* Hook up anchor to our request */
  status = (YES==HTLoadAnchor(anchor, request));

  if (!status)
    /* use abort here, because it is a programmatic mistake */
    xsb_abort("LIBWWW_PARSE_XML: Invalid data in URI %d", uri);
}


/* ------------------------------------------------------------------------- */
/*			     HTXML STREAM HANDLERS			     */
/* ------------------------------------------------------------------------- */

PRIVATE void HTXML_setHandlers (XML_Parser me)
{
  XML_SetElementHandler(me, xml_beginElement, xml_endElement);
  XML_SetCharacterDataHandler(me, xml_addText);
  XML_SetProcessingInstructionHandler(me, xml_processingInstruction);
  XML_SetUnparsedEntityDeclHandler(me, xml_unparsedEntityDecl);
  XML_SetNotationDeclHandler(me, xml_notationDecl);
  XML_SetExternalEntityRefHandler(me, xml_externalEntityRef);
  XML_SetUnknownEncodingHandler(me, xml_unknownEncoding, NULL);

  /* This exists only in expat 1.1. This version doesn't prohibit expansion of
     internal entities. Commented until expat 1.1 is included in libwww
  XML_SetDefaultHandlerExpand(me, xml_default);
  */
}

PRIVATE void HTXML_newInstance (HTStream *		me,
				HTRequest *		request,
				HTFormat 		target_format,
				HTStream *		target_stream,
				XML_Parser              xmlparser,
				void * 			context)
{
  USERDATA *userdata = create_userData(xmlparser, request, target_stream);
  XML_SetUserData(xmlparser, (void *) userdata);
  if (me && xmlparser) HTXML_setHandlers(xmlparser);
}



/* This is the callback that captures start tag events */
PRIVATE void xml_beginElement(void  *userdata, /* where we build everything */
			      const XML_Char *tag, /* tag */
			      const XML_Char **attributes)
{
  USERDATA *userdata_obj = (USERDATA *) userdata;

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In xml_beginElement(%d): stackptr=%d tag=%s suppress=%d choose=%d",
	     REQUEST_ID(userdata_obj->request),
	     userdata_obj->stackptr, tag,
	     IS_SUPPRESSED_TAG((HKEY) tag, userdata_obj->request),
	     IS_SELECTED_TAG((HKEY) tag, userdata_obj->request)
	     );
#endif

  if (IS_STRIPPED_TAG((HKEY)tag, userdata_obj->request)) return;

  if ((suppressing(userdata_obj)
       && !IS_SELECTED_TAG((HKEY)tag, userdata_obj->request))
      || (parsing(userdata_obj)
	  && IS_SUPPRESSED_TAG((HKEY)tag, userdata_obj->request))) {
    xml_push_suppressed_element(userdata_obj, tag);
    return;
  }

  /* parsing or suppressing & found a selected tag */
  if ((parsing(userdata_obj)
       && !IS_SUPPRESSED_TAG((HKEY)tag, userdata_obj->request))
      || (suppressing(userdata_obj) 
	  && IS_SELECTED_TAG((HKEY)tag, userdata_obj->request))) {
    xml_push_element(userdata_obj,tag,attributes);
    return;
  }
}


/* The callback for the end-tag event */
PRIVATE void xml_endElement (void *userdata, const XML_Char *tag)
{
  USERDATA *userdata_obj = (USERDATA *) userdata;

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In xml_endElement(%d): stackptr=%d, tag=%s",
	     REQUEST_ID(userdata_obj->request),
	     userdata_obj->stackptr, tag);
#endif

  if (IS_STRIPPED_TAG((HKEY)tag, userdata_obj->request)) return;

  if (strcasecmp(STACK_TOP(userdata_obj).tag, tag) != 0)
    libwww_abort_request(userdata_obj->request,
			 HT_DOC_SYNTAX,
			 "LIBWWW_PARSE_XML: End tag %s/start tag %s mismatch",
			 tag, STACK_TOP(userdata_obj).tag);

  if (parsing(userdata_obj))
    xml_pop_element(userdata_obj);
  else
    xml_pop_suppressed_element(userdata_obj);

#ifdef LIBWWW_DEBUG_VERBOSE
  if (!STACK_TOP(userdata_obj).suppress)
    print_prolog_term(STACK_TOP(userdata_obj).elt_term, "elt_term");
#endif

  return;
}



/* The callback to capture text events */
PRIVATE void xml_addText (void	         *userdata,
			  const XML_Char *textbuf, int len)
{
  USERDATA *userdata_obj = (USERDATA *) userdata;
  static vstrDEFINE(pcdata_buf);
  int shift = 0;

#ifdef LIBWWW_DEBUG_VERBOSE
  xsb_dbgmsg("In xml_addText (%d):", REQUEST_ID(userdata_obj->request));
#endif

  if (IS_STRIPPED_TAG("pcdata", userdata_obj->request)) return;
  if (suppressing(userdata_obj)) return;

  /* strip useless newlines */
  if (strncmp(textbuf,"\n", len) == 0) return;

  xml_push_element(userdata_obj, "pcdata", NULL);

  /* copy textbuf (which isn't null-terminated) into a variable length str */
  vstrENSURE_SIZE(&pcdata_buf, len+1);
  strncpy(pcdata_buf.string, textbuf, len);
  pcdata_buf.length = len;
  vstrNULL_TERMINATE(&pcdata_buf);

  /* if string starts with a newline, skip the newline */
  if (strncmp(textbuf,"\n", strlen("\n")) == 0)
    shift = strlen("\n");

#ifdef LIBWWW_DEBUG_VERBOSE
  xsb_dbgmsg("pcdata=%s", pcdata_buf.string+shift);
#endif

  /* put the text string into the elt term and then pop it */
  c2p_string(pcdata_buf.string+shift,
	     p2p_arg(STACK_TOP(userdata_obj).elt_term,3));
  xml_pop_element(userdata_obj);
  return;
}


/* Collect tag's attributes and make them into a list of the form
   [attval(attr,val), ...]; bind it to Arg 2 of ELT_TERM */
PRIVATE void collect_xml_attributes (prolog_term     elt_term,
				     const XML_Char  **attrs)
{
  static vstrDEFINE(attrname);
  prolog_term
    prop_list = p2p_arg(elt_term,2),
    prop_list_tail = prop_list,
    prop_list_head;

  c2p_list(prop_list_tail);

  while (attrs && *attrs) {
    vstrENSURE_SIZE(&attrname, strlen((char *)*attrs));
    strcpy_lower(attrname.string, (char *)*attrs);
    
#ifdef LIBWWW_DEBUG_VERBOSE
    xsb_dbgmsg("attr=%s", attrname.string);
#endif
    prop_list_head = p2p_car(prop_list_tail);
    c2p_functor("attval",2,prop_list_head);
    c2p_string(attrname.string, p2p_arg(prop_list_head,1));
    /* get value */
    attrs++;
    /* if *attrs=NULL, then it is an error: expat will stop */
    if (*attrs)
      c2p_string((char *)*attrs, p2p_arg(prop_list_head, 2));
    
    prop_list_tail = p2p_cdr(prop_list_tail);
    c2p_list(prop_list_tail);
    attrs++;
  }
  
  /* Terminate the property list */
  c2p_nil(prop_list_tail);
  return;
}


/* push element onto HTEXT->stack */
PRIVATE void xml_push_element (USERDATA    *userdata,
			       const XML_Char  *tag,
			       const XML_Char  **attrs)
{
  static vstrDEFINE(lower_tagname);
  prolog_term location;

  /*   If tag is not valid */
  if (tag == NULL) return;

  if (userdata->stackptr < 0)
    location = userdata->parsed_term_tail;
  else 
    location = STACK_TOP(userdata).content_list_tail;

  userdata->stackptr++;

#ifdef LIBWWW_DEBUG_VERBOSE
    xsb_dbgmsg("In xml_push_element(%d): stackptr=%d tag=%s",
	       REQUEST_ID(userdata->request), userdata->stackptr, tag);
#endif

  if (userdata->stackptr > MAX_XML_NESTING)
    libwww_abort_request(userdata->request,
			 HT_DOC_SYNTAX,
			 "LIBWWW_PARSE_XML: Element nesting exceeds MAX(%d)",
			 MAX_XML_NESTING);

  /* wire the new elt into where it should be in the content list */
  STACK_TOP(userdata).elt_term = p2p_car(location);

  STACK_TOP(userdata).tag = (XML_Char *)tag; /* cast to discard const
						declaration */
  STACK_TOP(userdata).suppress = FALSE;

  /* lowercase the tag */
  vstrENSURE_SIZE(&lower_tagname, strlen(tag)+1);
  strcpy_lower(lower_tagname.string, tag);

  /* normal tags look like elt(tagname, attrlist, contentlist);
     pcdata tags are: elt(pcdata,[],text); */
  if (vstrSTRCMP(&lower_tagname, "pcdata")==0)
    c2p_functor("elt",3,STACK_TOP(userdata).elt_term);
  else /* normal elt */
    c2p_functor("elt",3,STACK_TOP(userdata).elt_term);

  c2p_string(lower_tagname.string, p2p_arg(STACK_TOP(userdata).elt_term, 1));
  collect_xml_attributes(STACK_TOP(userdata).elt_term, attrs);
  
#ifdef LIBWWW_DEBUG_VERBOSE
  xsb_dbgmsg("elt_name=%s", lower_tagname.string);
  print_prolog_term(STACK_TOP(userdata).elt_term, "elt_term");
#endif

  /* normal element */
  if (vstrSTRCMP(&lower_tagname, "pcdata")!=0) {
    STACK_TOP(userdata).content_list_tail =
      p2p_arg(STACK_TOP(userdata).elt_term,3);
    c2p_list(STACK_TOP(userdata).content_list_tail);
  }
}


/* when we are done with an elt, we must close its contents list and pop the
   stack */
PRIVATE void xml_pop_element(USERDATA *userdata)
{
#ifdef LIBWWW_DEBUG_VERBOSE
  xsb_dbgmsg("In xml_pop_element(%d): stackptr=%d, elt_name=%s",
	     REQUEST_ID(userdata->request),
	     userdata->stackptr,
	     STACK_TOP(userdata).tag);
#endif
  /* close the property list, for notmal elements */
  if (strcasecmp(STACK_TOP(userdata).tag, "pcdata")!=0) {
    c2p_nil(STACK_TOP(userdata).content_list_tail);
  }

  /* insert new list cell into the tail and change content_list_tail to point
     to the new tail */
  if (userdata->stackptr > 0) {
    STACK_PREV(userdata).content_list_tail =
      p2p_cdr(STACK_PREV(userdata).content_list_tail);
    c2p_list(STACK_PREV(userdata).content_list_tail);
  } else {
    userdata->parsed_term_tail = p2p_cdr(userdata->parsed_term_tail);
    c2p_list(userdata->parsed_term_tail);
  }

  userdata->stackptr--;

#ifdef LIBWWW_DEBUG_VERBOSE
  if (userdata->stackptr >= 0)
    print_prolog_term(STACK_TOP(userdata).content_list_tail,
		      "content_list_tail");
  else
    print_prolog_term(userdata->parsed_term_tail, "parsed_term_tail");
#endif

  return;
}


/* pushes tag, but keeps only the tag info; doesn't convert to prolog term */
PRIVATE void xml_push_suppressed_element(USERDATA   *userdata,
					 const XML_Char *tag)
{
  /* non-empty tag */
  userdata->stackptr++; /* advance ptr, but don't push tag */

  STACK_TOP(userdata).tag = (XML_Char *)tag; /* cast to discard const
						declaration */
  STACK_TOP(userdata).suppress = TRUE;

  /* passing content list tail through suppressed elements */
  if (userdata->stackptr == 0)
    STACK_TOP(userdata).content_list_tail = userdata->parsed_term_tail;
  else 
    STACK_TOP(userdata).content_list_tail =
      STACK_PREV(userdata).content_list_tail;

  return;
}


PRIVATE void xml_pop_suppressed_element(USERDATA *userdata)
{
  /* chain the list tails back through the sequence of suppressed tags */
  if (userdata->stackptr > 0) {
    STACK_PREV(userdata).content_list_tail = STACK_TOP(userdata).content_list_tail;
  } else {
    userdata->parsed_term_tail = STACK_TOP(userdata).content_list_tail;
  }

  userdata->stackptr--;

#ifdef LIBWWW_DEBUG_VERBOSE
  xsb_dbgmsg("In xml_pop_suppressed_element(%d): stackptr=%d",
	     REQUEST_ID(userdata->request), userdata->stackptr);
  if (userdata->stackptr >= 0)
    print_prolog_term(STACK_TOP(userdata).content_list_tail, "content_list_tail");
  else
    print_prolog_term(userdata->parsed_term_tail, "parsed_term_tail");
#endif

  return;
}


PRIVATE USERDATA *create_userData(XML_Parser parser,
				  HTRequest *request,
				  HTStream  *target_stream)
{
  USERDATA *me = NULL;
  if (parser) {
    if ((me = (USERDATA *) HT_CALLOC(1, sizeof(USERDATA))) == NULL)
      HT_OUTOFMEM("libwww_parse_xml");
    me->parser = parser;
    me->request = request;
    me->target = target_stream;
    me->suppress_is_default = 
      ((REQUEST_CONTEXT *)HTRequest_context(request))->suppress_is_default;
    me->parsed_term = p2p_new();
    c2p_list(me->parsed_term);
    me->parsed_term_tail = me->parsed_term;
    me->stackptr = -1;
  }

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In create_userData(%d):", REQUEST_ID(request));
#endif

  /* Hook up userdata to the request context */
  ((REQUEST_CONTEXT *)HTRequest_context(request))->userdata = (void *)me;

  return me;
}


PRIVATE void delete_userData(USERDATA *me)
{
  prolog_term 
    parsed_result =
    ((REQUEST_CONTEXT *)HTRequest_context(me->request))->parsed_result,
    status_term =
    ((REQUEST_CONTEXT *)HTRequest_context(me->request))->status_term;
#ifdef LIBWWW_DEBUG
  int request_id = REQUEST_ID(me->request);
  xsb_dbgmsg("In delete_userData(%d): stackptr=%d",
	     request_id, me->stackptr);
#endif

  /* if the status code says the doc was loaded fine, but stackptr is != -1,
     it means the doc is ill-formed */
  if (me->stackptr >= 0 && (int_val(status_term) == HT_LOADED)) {
    c2p_int(HT_DOC_SYNTAX,status_term);
    xsb_warn("LIBWWW_PARSE_XML: Ill-formed document (a syntax error or tags left open)");
  }

  /* terminate the parsed prolog terms list */
  c2p_nil(me->parsed_term_tail);

  /* pass the result to the outside world */
  if (is_var(me->parsed_term))
    p2p_unify(parsed_result, me->parsed_term);
  else
    xsb_warn("LIBWWW_PARSE_XML: Request %d: Arg 4 (Parse result) must be a variable",
	      REQUEST_ID(me->request));

  if (me->target) FREE_TARGET(me);
  HT_FREE(me);

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("Request %d: freed the USERDATA object", request_id);
#endif

  return;
}


/* Unused handlers, which might get used later in the development */

PRIVATE void xml_processingInstruction (void * userData,
					const XML_Char * target,
					const XML_Char * data)
{
  return;
}

/* 
** This is called for a declaration of an unparsed (NDATA)
** entity.  The base argument is whatever was set by XML_SetBase.
** The entityName, systemId and notationName arguments will never be null.
** The other arguments may be.
*/
PRIVATE void xml_unparsedEntityDecl (void * userData,
				     const XML_Char * entityName,
				     const XML_Char * base,
				     const XML_Char * systemId,
				     const XML_Char * publicId,
				     const XML_Char * notationName)
{
  return;
}

/* 
** This is called for a declaration of notation.
** The base argument is whatever was set by XML_SetBase.
** The notationName will never be null.  The other arguments can be.
*/
PRIVATE void xml_notationDecl (void * userData,
			       const XML_Char * notationName,
			       const XML_Char * base,
			       const XML_Char * systemId,
			       const XML_Char * publicId)
{
  return;
}

/* 
** This is called for a reference to an external parsed general entity.  The
** referenced entity is not automatically parsed.  The application can parse it
** immediately or later using XML_ExternalEntityParserCreate.  The parser
** argument is the parser parsing the entity containing the reference; it can
** be passed as the parser argument to XML_ExternalEntityParserCreate.  The
** systemId argument is the system identifier as specified in the entity
** declaration; it will not be null.  The base argument is the system
** identifier that should be used as the base for resolving systemId if
** systemId was relative; this is set by XML_SetBase; it may be null.  The
** publicId argument is the public identifier as specified in the entity
** declaration, or null if none was specified; the whitespace in the public
** identifier will have been normalized as required by the XML spec.  The
** openEntityNames argument is a space-separated list of the names of the
** entities that are open for the parse of this entity (including the name of
** the referenced entity); this can be passed as the openEntityNames argument
** to XML_ExternalEntityParserCreate; openEntityNames is valid only until the
** handler returns, so if the referenced entity is to be parsed later, it must
** be copied.  The handler should return 0 if processing should not continue
** because of a fatal error in the handling of the external entity.  In this
** case the calling parser will return an XML_ERROR_EXTERNAL_ENTITY_HANDLING
** error.  Note that unlike other handlers the first argument is the parser,
** not userData.  */
PRIVATE int xml_externalEntityRef (XML_Parser parser,
				   const XML_Char * openEntityNames,
				   const XML_Char * base,
				   const XML_Char * systemId,
				   const XML_Char * publicId)
{
  /* This external entity processor doesn't do anything. This is because we
     need to figure out how to start a new libwww request on the external
     entity URL while the parent request is waiting. */
  int status;
  XML_Parser extParser =
    XML_ExternalEntityParserCreate(parser, openEntityNames, 0);
  HTAnchor    *anchor = NULL;
  HTRequest *request = HTRequest_new();
  char *uri;
  USERDATA *userdata = XML_GetUserData(parser);
  void *context = HTRequest_context(userdata->request);

  uri = HTParse((char *)systemId, NULL, PARSE_ALL);
  anchor = HTAnchor_findAddress(uri);
  status = (YES==HTLoadAnchor(anchor, request));

  /* put the same context on this request */
  HTRequest_setContext(request, (void *) context);
  
  /* launch the new request; this should add it to the existing event loop */
  if (YES != HTLoadAnchor(anchor,request))
    XML_ParserFree(extParser);

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In xml_externalEntityRef(%d): uri=%s",
	     REQUEST_ID(request), uri);
#endif

  return TRUE;
}

/* 
** This is called for an encoding that is unknown to the parser.
** The encodingHandlerData argument is that which was passed as the
** second argument to XML_SetUnknownEncodingHandler.
** The name argument gives the name of the encoding as specified in
** the encoding declaration.
** If the callback can provide information about the encoding,
** it must fill in the XML_Encoding structure, and return 1.
** Otherwise it must return 0.
** If info does not describe a suitable encoding,
** then the parser will return an XML_UNKNOWN_ENCODING error.
*/
PRIVATE int xml_unknownEncoding (void 	        *encodingHandlerData,
				 const XML_Char *name,
				 XML_Encoding   *info)
{
  return 0;
}

/* Default is commented out so that it'll parse entities.
PRIVATE void xml_default (void * userData, const XML_Char * str, int len)
{
  vstrDEFINE(unparsed);

  vstrENSURE_SIZE(&unparsed, len+1);
  strncpy(unparsed.string, str, len);
  unparsed.length = len;
  vstrNULL_TERMINATE(&unparsed);
#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In xml_default: Request: %d: Unparsed: %s",
	     REQUEST_ID(((USERDATA *)userData)->request), unparsed.string);
#endif

  return;
}
*/


/* hash table stuff */


PRIVATE void init_tag_table(prolog_term tag_list, HASH_TABLE *tag_tbl)
{
  prolog_term tail, head;
  int i=0;
  HKEY tagname;
  /* Save tag numbers in the table */
  tail=tag_list;
  while (is_list(tail) && !is_nil(tail) && i < tag_tbl->size) {
    head= p2p_car(tail);
    tail=p2p_cdr(tail);
    tagname = string_val(head);
    add_to_htable(tagname, tag_tbl);
    i++;
  }
}


PRIVATE unsigned long myhash(HKEY s)
{
  unsigned long h = 0;
  while (*s)
    h = (h << 5) + h + (unsigned char)*s++;
  return h;
}


PRIVATE void free_htable(HASH_TABLE *htable)
{
  int i;
  for (i=0; i < htable->size; i++)
    if (htable->table[i] != NULL)
      free(htable->table[i]);
  free(htable->table);
}


