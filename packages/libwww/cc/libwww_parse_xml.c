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


#include "libwww_util.h"
#include "libwww_parse.h"
#include "libwww_parse_xml.h"


/* BOOL, PRIVATE, PUBLIC, etc., are defined in a Libwww header */

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

void HTXML_newInstance (HTStream *		me,
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
	     IS_SUPPRESSED_TAG((HKEY)(char *)tag, userdata_obj->request),
	     IS_SELECTED_TAG((HKEY)(char *)tag, userdata_obj->request)
	     );
#endif

  if (IS_STRIPPED_TAG((HKEY)(char *)tag, userdata_obj->request)) return;

  if ((suppressing(userdata_obj)
       && !IS_SELECTED_TAG((HKEY)(char *)tag, userdata_obj->request))
      || (parsing(userdata_obj)
	  && IS_SUPPRESSED_TAG((HKEY)(char *)tag, userdata_obj->request))) {
    xml_push_suppressed_element(userdata_obj, tag);
    return;
  }

  /* parsing or suppressing & found a selected tag */
  if ((parsing(userdata_obj)
       && !IS_SUPPRESSED_TAG((HKEY)(char *)tag, userdata_obj->request))
      || (suppressing(userdata_obj) 
	  && IS_SELECTED_TAG((HKEY)(char *)tag, userdata_obj->request))) {
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

  if (IS_STRIPPED_TAG((HKEY)(char *)tag, userdata_obj->request)) return;

  /* Expat does checking for tag mismatches, so we don't have to */
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
  REQUEST_CONTEXT *context =
    (REQUEST_CONTEXT *)HTRequest_context(userdata_obj->request);

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In xml_addText (%d):", REQUEST_ID(userdata_obj->request));
#endif

  if (IS_STRIPPED_TAG((HKEY)"pcdata", userdata_obj->request)) return;
  if (suppressing(userdata_obj)) return;

  /* strip useless newlines */
  if (strncmp(textbuf,"\n", len) == 0) return;

  if (!xml_push_element(userdata_obj, "pcdata", NULL))
    return;

  /* copy textbuf (which isn't null-terminated) into a variable length str */
  vstrENSURE_SIZE(&pcdata_buf, len+1);
  strncpy(pcdata_buf.string, textbuf, len);
  pcdata_buf.length = len;
  vstrNULL_TERMINATE(&pcdata_buf);

  /* if string starts with a newline, skip the newline */
  if (strncmp(textbuf,"\n", strlen("\n")) == 0)
    shift = strlen("\n");

#ifdef LIBWWW_DEBUG_VERBOSE
  xsb_dbgmsg("In addText: pcdata=%s", pcdata_buf.string+shift);
#endif

  /* put the text string into the elt term and then pop it */
  if (context->convert2list)
    c2p_chars(pcdata_buf.string+shift,
	      p2p_arg(STACK_TOP(userdata_obj).elt_term,3));
  else
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
PRIVATE int xml_push_element (USERDATA    *userdata,
			       const XML_Char  *tag,
			       const XML_Char  **attrs)
{
  static vstrDEFINE(lower_tagname);
  prolog_term location;

  /*   If tag is not valid */
  if (tag == NULL) return TRUE;

  if (userdata->stackptr < 0)
    location = userdata->parsed_term_tail;
  else 
    location = STACK_TOP(userdata).content_list_tail;

  userdata->stackptr++;

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In xml_push_element(%d): stackptr=%d tag=%s",
	     REQUEST_ID(userdata->request), userdata->stackptr, tag);
#endif

  CHECK_STACK_OVERFLOW(userdata);

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
  return TRUE;
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


USERDATA *create_userData(XML_Parser parser,
			  HTRequest *request,
			  HTStream  *target_stream)
{
  USERDATA *me = NULL;
  if (parser) {
    if ((me = (USERDATA *) HT_CALLOC(1, sizeof(USERDATA))) == NULL)
      HT_OUTOFMEM("libwww_parse_xml");
    me->delete_method = delete_userData;
    me->parser = parser;
    me->request = request;
    me->target = target_stream;
    me->suppress_is_default = 
      ((REQUEST_CONTEXT *)HTRequest_context(request))->suppress_is_default;
    me->parsed_term = p2p_new();
    c2p_list(me->parsed_term);
    me->parsed_term_tail = me->parsed_term;
    SETUP_STACK(me);
  }

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In create_userData(%d):", REQUEST_ID(request));
#endif

  /* Hook up userdata to the request context */
  ((REQUEST_CONTEXT *)HTRequest_context(request))->userdata = (void *)me;

  return me;
}


PRIVATE void delete_userData(void *userdata)
{
  prolog_term parsed_result, status_term;
  USERDATA *me = (USERDATA *)userdata;
#ifdef LIBWWW_DEBUG
  int request_id;
#endif

  if (me->request) {
    parsed_result =
      ((REQUEST_CONTEXT *)HTRequest_context(me->request))->request_result;
    status_term =
      ((REQUEST_CONTEXT *)HTRequest_context(me->request))->status_term;
  } else return;

#ifdef LIBWWW_DEBUG
  request_id = REQUEST_ID(me->request);
  xsb_dbgmsg("In delete_userData(%d): stackptr=%d", request_id, me->stackptr);
#endif

  /* if the status code says the doc was loaded fine, but stackptr is != -1,
     it means the doc is ill-formed */
  if (me->stackptr >= 0 && (me->status == HT_LOADED)) {
    c2p_int(WWW_DOC_SYNTAX,status_term);
  }

  /* terminate the parsed prolog terms list */
  c2p_nil(me->parsed_term_tail);

  /* pass the result to the outside world */
  if (is_var(me->parsed_term))
    p2p_unify(parsed_result, me->parsed_term);
  else
    xsb_abort("LIBWWW_REQUEST: Request %d: Arg 4 (Result) must be unbound variable",
	      REQUEST_ID(me->request));

  if (me->target) FREE_TARGET(me);
  if (me->stack) HT_FREE(me->stack);
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
PRIVATE int xml_externalEntityRef (XML_Parser     parser,
				   const XML_Char *openEntityNames,
				   const XML_Char *base,
				   const XML_Char *systemId,
				   const XML_Char *publicId)
{
  /* This external entity processor doesn't do anything. This is because we
     need to figure out how to start a new libwww request on the external
     entity URL while the parent request is waiting. */
  XML_Parser extParser =
    XML_ExternalEntityParserCreate(parser, openEntityNames, 0);
  HTAnchor  *anchor = NULL;
  HTRequest *request = HTRequest_new();
  char      *uri;
  USERDATA  *userdata = XML_GetUserData(parser);
  REQUEST_CONTEXT *context =
    (REQUEST_CONTEXT *)HTRequest_context(userdata->request);
  HTChunk   *chunk = NULL;
  char      *cwd = HTGetCurrentDirectoryURL();

  /* put the same context on this request */
  HTRequest_setContext(request, (void *)context);

  uri = HTParse((char *)systemId, cwd, PARSE_ALL);
  anchor = HTAnchor_findAddress(uri);

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In xml_externalEntityRef(%d): uri=%s", context->request_id, uri);
#endif

  HTRequest_setOutputFormat(request, WWW_SOURCE);
  /* make it a blocking request, so that we get the result right away */
  HTRequest_setPreemptive(request, YES);
  /*
    HTRequest_addConnection(request, "close", "");
  */
  /* Launch a new subrequest. Since this request is blocking, this will
     execute the request immediately. */
  context->is_subrequest = TRUE;
  chunk = HTLoadAnchorToChunk(anchor,request);
  /* if subrequest failed to terminate, then kill it to avoid blockage */
  if (context->is_subrequest) {
    HTRequest_kill(request);
    context->is_subrequest = FALSE;
  }
  if (chunk) {
    char *ext_entity_expansion = HTChunk_toCString(chunk);

    if (ext_entity_expansion) {
#ifdef LIBWWW_DEBUG
      xsb_dbgmsg("In xml_externalEntityRef: entity=%s", ext_entity_expansion);
#endif
      XML_Parse(extParser,ext_entity_expansion,strlen(ext_entity_expansion),1);
      HT_FREE(ext_entity_expansion);
    }
  } else {
    /* return WWW_EXTERNAL_ENTITY error code in the response */
    add_subrequest_error(request, WWW_EXTERNAL_ENTITY);
  }

  HT_FREE(uri);
  HT_FREE(cwd);
  XML_ParserFree(extParser);
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


