/* File:      libwww_parse_html.c
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


#include "libwww_parse_html.h"
#include "libwww_parse_utils.h"
#include "libwww_parse_utils.c"


/* BOOL, PRIVATE, PUBLIC, etc., are defined in a Libwww header */

/* Calling sequence:
       libwww_parse_html([req1,req2,...])

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
BOOL  libwww_parse_html(void)
{
  prolog_term request_term_list = reg_term(1), request_list_tail;
  int request_id=0;

  /* Create a new premptive client */
  HTProfile_newHTMLNoCacheClient ("HTML Parser", "1.0");
  HTAlert_setInteractive(NO);
  HTHost_setEventTimeout(DEFAULT_TIMEOUT);

  if (!is_list(request_term_list))
    xsb_abort("LIBWWW_PARSE_HTML: Argument must be a list");
  
  request_list_tail = request_term_list;
  while (is_list(request_list_tail) && !is_nil(request_list_tail)) {
    request_id++;
    setup_html_request_structure(p2p_car(request_list_tail), request_id);
    request_list_tail = p2p_cdr(request_list_tail);
    total_number_of_requests++;
  }

#ifdef LIBWWW_DEBUG
  fprintf(stderr,
	  "In libwww_parse_html: starting the event loop. Total request#=%d\n",
	  total_number_of_requests);
#endif

  /* start the event loop and begin to parse all requests in parallel */
  if (total_number_of_requests > 0) {
    int status;
    status = HTEventList_newLoop();
    if (status != HT_OK)
      xsb_abort("LIBWWW_PARSE_HTML: Couldn't launch HTTP request");
  }

#ifdef LIBWWW_DEBUG
  fprintf(stderr, "In libwww_parse_html: event loop ended\n");
#endif
  
  /* don't delete the profile: it crashes libwww on the second try */
  return TRUE;
}


/* Sets up the libwww request structure for the request specified in
   PROLOG_REQ, including the request context, which contains the info about the
   return parameters.
   Note that we use xsb_abort here instead of abort handlers, because the error
   conditions handled here are programmatic mistakes rather than network
   conditions. */
PRIVATE void setup_html_request_structure(prolog_term prolog_req, int request_id)
{
  int	      status;
  HTAnchor    *anchor = NULL;
  HTRequest   *request;
  HTAssocList *formdata;
  char 	      *uri = NULL;
  prolog_term form_params;
  int 	      is_form_request;
  HTTP_METHOD form_method;

  /* register callback for begin/end element events */
  HText_registerElementCallback(html_beginElement, html_endElement);
  /* register callback for text chunks */
  HText_registerTextCallback(html_addText);
  /* register callbacks to create and delete the HText objects. These are
     objects where we build parsed terms */
  HText_registerCDCallback(create_HText_obj, delete_HText_obj);

  /* get URL */
  uri=string_val(p2p_arg(prolog_req,1));
  /* use abort here, because it is a programmatic mistake */
  if (uri == NULL) xsb_abort("LIBWWW_PARSE_HTML: Arg 1 (URI) is invalid");

  HTNet_addAfter(general_parse_abort_handler,
		 NULL, "LIBWWW_PARSE_HTML", HT_ALL, HT_FILTER_LAST);

  /* Create a new request and attach the context structure to it */
  request = HTRequest_new();
  set_request_context(request, prolog_req, request_id, "LIBWWW_PARSE_HTML");

  form_params = p2p_arg(prolog_req,2);
  if (is_var(form_params))
    is_form_request = FALSE;
  else if (is_list(form_params)) {
    is_form_request = TRUE;
    form_method = get_request_method(p2p_car(form_params));
    formdata = get_form_params(p2p_car(p2p_cdr(form_params)));
  } else
    xsb_abort("LIBWWW_PARSE_HTML: Request %d: Arg 2 (Form params) must be a variable or a list", request_id);

  uri = HTParse(uri, NULL, PARSE_ALL);
  /* Create a new Anchor */
  anchor = HTAnchor_findAddress(uri);
  /* Hook up anchor to our request */
  if (is_form_request)
    if (form_method == FORM_GET)
      status = (YES == HTGetFormAnchor(formdata,anchor,request));
    else /* FORM_POST */
      status = (NULL != HTPostFormAnchor(formdata,anchor,request));
  else /* not a form request */
    status = (YES==HTLoadAnchor(anchor, request));

#ifdef LIBWWW_DEBUG_TERSE
  if (is_form_request)
    fprintf(stderr,
	    "Request %d: HTTP Method: %s\n",
	    request_id,
	    (form_method==FORM_GET ? "FORM,GET" : "FORM,POST"));
  else
    fprintf(stderr, "Request %d: HTTP Method: NON-FORM REQ\n", request_id);
#endif

  if (formdata) HTAssocList_delete(formdata);

  if (!status)
    /* use abort, because it is a programmatic mistake */
    xsb_abort("LIBWWW_PARSE_HTML: Invalid data in URI %d", uri);
}


/* This is the callback that captures start tag events */
PRIVATE void html_beginElement(HText  	*htext, /* where we build everything */
			       int	element_number, /* internal tag # */
			       /* bitmap: tells which tag attrs are present */
			       const BOOL *present,
			       /* array of values for the attributes
				  specified by the "present" bitmap */ 
			       const char **value)
{
#ifdef LIBWWW_DEBUG
  HTTag *tag = SGML_findTag(htext->dtd, element_number);
  fprintf(stderr,
	  "In html_beginElement(%d): stackptr=%d tag=%s suppress=%d choose=%d\n",
	  REQUEST_ID(htext->request),
	  htext->stackptr, HTTag_name(tag),
	  IS_SUPPRESSED_TAG(element_number, htext->request),
	  IS_SELECTED_TAG(element_number, htext->request)
	  );
#endif

  if (IS_STRIPPED_TAG(element_number, htext->request)) return;

  if ((suppressing(htext) && !IS_SELECTED_TAG(element_number, htext->request))
      || (parsing(htext) && IS_SUPPRESSED_TAG(element_number, htext->request))) {
    html_push_suppressed_element(htext, element_number);
    return;
  }

  /* parsing or suppressing & found a selected tag */
  if ((parsing(htext) && !IS_SUPPRESSED_TAG(element_number, htext->request))
      || (suppressing(htext) 
	  && IS_SELECTED_TAG(element_number, htext->request))) {
    html_push_element(htext,element_number,present,value);
    return;
  }
}


/* The callback for the end-tag event */
PRIVATE void html_endElement (HText *htext, int element_number)
{
  int i, match;

#ifdef LIBWWW_DEBUG
  fprintf(stderr,
	  "In html_endElement(%d): stackptr=%d\n",
	  REQUEST_ID(htext->request), htext->stackptr);
#endif

  if (IS_STRIPPED_TAG(element_number, htext->request)) return;

  match = find_matching_elt(htext, element_number);
  /* the closing tag is probably out of place */
  if (match < 0) return;

#ifdef LIBWWW_DEBUG_VERBOSE
  fprintf(stderr, "match=%d\n", match);
#endif

  for (i=htext->stackptr; i>=match; i--)
    if (parsing(htext))
      html_pop_element(htext);
    else
      html_pop_suppressed_element(htext);

#ifdef LIBWWW_DEBUG_VERBOSE
  if (!STACK_TOP(htext).suppress)
    print_prolog_term(STACK_TOP(htext).elt_term, "elt_term");
#endif

  return;
}



/* The callback to capture text events */
PRIVATE void html_addText (HText *htext, const char *textbuf, int len)
{
  static vstrDEFINE(pcdata_buf);
  int shift = 0;

  if (IS_STRIPPED_TAG(PCDATA_SPECIAL, htext->request)) return;
  if (suppressing(htext)) return;

  /* strip useless newlines */
  if (strncmp(textbuf,"\n", len) == 0) return;

  html_push_element(htext, PCDATA_SPECIAL, NULL, NULL);

  /* copy textbuf (which isn't null-terminated) into a variable length str */
  vstrENSURE_SIZE(&pcdata_buf, len+1);
  strncpy(pcdata_buf.string, textbuf, len);
  pcdata_buf.length = len;
  vstrNULL_TERMINATE(&pcdata_buf);

  /* if string starts with a newline, skip the newline */
  if (strncmp(textbuf,"\n", strlen("\n")) == 0)
    shift = strlen("\n");

  /* put the text string into the elt term and then pop it */
  c2p_string(pcdata_buf.string+shift, p2p_arg(STACK_TOP(htext).elt_term,3));
  html_pop_element(htext);
  return;
}


/* Collect tag's attributes and make them into a list of the form
   [attval(attr,val), ...]; bind it to Arg 2 of ELT_TERM */
PRIVATE void collect_html_attributes ( prolog_term  elt_term,
				       HTTag        *tag,
				       const BOOL   *present,
				       const char  **value)
{
  int tag_attributes_number = HTTag_attributes(tag);
  static vstrDEFINE(attrname);
  int cnt;
  prolog_term
    prop_list = p2p_arg(elt_term,2),
    prop_list_tail = prop_list,
    prop_list_head;

  c2p_list(prop_list_tail);

#ifdef LIBWWW_DEBUG_VERBOSE
  fprintf(stderr,
	  "In collect_html_attributes: tag_attributes_number=%d\n",
	  tag_attributes_number);
#endif

  for (cnt=0; cnt<tag_attributes_number; cnt++) {
    if (present[cnt]) {
      vstrENSURE_SIZE(&attrname, strlen(HTTag_attributeName(tag, cnt)));
      strcpy_lower(attrname.string, HTTag_attributeName(tag, cnt));
      
#ifdef LIBWWW_DEBUG_VERBOSE
      fprintf(stderr, "attr=%s, val=%s \n",
	      attrname.string, (char *)value[cnt]);
#endif
      prop_list_head = p2p_car(prop_list_tail);
      c2p_functor("attval",2,prop_list_head);
      c2p_string(attrname.string, p2p_arg(prop_list_head,1));
      /* some attrs, like "checked", are boolean and have no value; in this
	 case we leave the value arg uninstantiated */
      if ((char *)value[cnt])
	c2p_string((char *)value[cnt], p2p_arg(prop_list_head, 2));
    
      prop_list_tail = p2p_cdr(prop_list_tail);
      c2p_list(prop_list_tail);
    }
  }

  /* Terminate the property list */
  c2p_nil(prop_list_tail);
  return;
}


/* push element onto HTEXT->stack */
PRIVATE void html_push_element (HText       *htext,
				int         element_number,
				const BOOL  *present,
				const char **value)
{
  static vstrDEFINE(tagname);
  HTTag *tag = special_find_tag(htext, element_number);
  prolog_term location;

  /*   If tag is not valid for HTML */
  if (tag == NULL) return;

  if (htext->stackptr < 0)
    location = htext->parsed_term_tail;
  else 
    location = STACK_TOP(htext).content_list_tail;

  htext->stackptr++;

#ifdef LIBWWW_DEBUG_VERBOSE
    fprintf(stderr,
	    "In html_push_element(%d): stackptr=%d\n",
	    REQUEST_ID(htext->request), htext->stackptr);
#endif

  if (htext->stackptr > MAX_HTML_NESTING)
    html_libwww_abort_request(htext->request,
			      HT_DOC_SYNTAX,
			      "LIBWWW_PARSE_HTML: Element nesting exceeds MAX(%d)",
			      MAX_HTML_NESTING);

  /* wire the new elt into where it should be in the content list */
  STACK_TOP(htext).elt_term = p2p_car(location);

  STACK_TOP(htext).element_number = element_number;
  STACK_TOP(htext).suppress = FALSE;

  /* normal tags look like elt(tagname, attrlist, contentlist);
     pcdata tags are: elt(pcdata,[],text);
     empty tags look like elt(tagname, attrlist); */
  STACK_TOP(htext).element_type = HTTag_content(tag);
  switch (STACK_TOP(htext).element_type) {
  case SGML_EMPTY:
    c2p_functor("elt",2,STACK_TOP(htext).elt_term);
    break;
  case PCDATA_SPECIAL:
    c2p_functor("elt",3,STACK_TOP(htext).elt_term);
    break;
  default: /* normal elt */
    c2p_functor("elt",3,STACK_TOP(htext).elt_term);
  }

  vstrENSURE_SIZE(&tagname, strlen(HTTag_name(tag)));
  strcpy_lower(tagname.string, HTTag_name(tag));
  c2p_string(tagname.string, p2p_arg(STACK_TOP(htext).elt_term, 1));
  collect_html_attributes(STACK_TOP(htext).elt_term, tag, present, value);
#ifdef LIBWWW_DEBUG_VERBOSE
  fprintf(stderr, "elt_name=%s\n", HTTag_name(tag));
  print_prolog_term(STACK_TOP(htext).elt_term, "elt_term");
#endif

  switch (STACK_TOP(htext).element_type) {
  case SGML_EMPTY:
    html_pop_element(htext);
    break;
  case PCDATA_SPECIAL:
    /* nothing to do: we pop this after thext is inserted in html_addText */
    break;
  default: /* normal elt */
    STACK_TOP(htext).content_list_tail = p2p_arg(STACK_TOP(htext).elt_term,3);
    c2p_list(STACK_TOP(htext).content_list_tail);
  }
}


/* when we are done with an elt, we must close its contents list and pop the
   stack */
PRIVATE void html_pop_element(HText *htext)
{
#ifdef LIBWWW_DEBUG_VERBOSE
  fprintf(stderr,
	  "In html_pop_element(%d): stackptr=%d, elt_name=%s\n",
	  REQUEST_ID(htext->request),
	  htext->stackptr,
	  HTTag_name(special_find_tag(htext, STACK_TOP(htext).element_number)));
#endif
  /* close the property list, for notmal elements */
  switch (STACK_TOP(htext).element_type) {
  case SGML_EMPTY: /* this case can't occur */
    break;
  case PCDATA_SPECIAL:
    break;
  default: /* normal element */
    c2p_nil(STACK_TOP(htext).content_list_tail);
  }

  /* insert new list cell into the tail and change content_list_tail to point
     to the new tail */
  if (htext->stackptr > 0) {
    STACK_PREV(htext).content_list_tail =
      p2p_cdr(STACK_PREV(htext).content_list_tail);
    c2p_list(STACK_PREV(htext).content_list_tail);
  } else {
    htext->parsed_term_tail = p2p_cdr(htext->parsed_term_tail);
    c2p_list(htext->parsed_term_tail);
  }

  htext->stackptr--;

#ifdef LIBWWW_DEBUG_VERBOSE
  if (htext->stackptr >= 0)
    print_prolog_term(STACK_TOP(htext).content_list_tail, "content_list_tail");
  else
    print_prolog_term(htext->parsed_term_tail, "parsed_term_tail");
#endif

  return;
}


/* pushes tag, but keeps only the tag info; doesn't convert to prolog term */
PRIVATE void html_push_suppressed_element(HText *htext, int element_number)
{
  /* if empty tag, then just return */
  if (SGML_findTagContents(htext->dtd, element_number) == SGML_EMPTY)
      return;
  /* non-empty tag */
  htext->stackptr++; /* advance ptr, but don't push tag */

  STACK_TOP(htext).element_number = element_number;
  STACK_TOP(htext).suppress = TRUE;

  /* passing content list tail through suppressed elements */
  if (htext->stackptr == 0)
    STACK_TOP(htext).content_list_tail = htext->parsed_term_tail;
  else 
    STACK_TOP(htext).content_list_tail = STACK_PREV(htext).content_list_tail;

  return;
}


PRIVATE void html_pop_suppressed_element(HText *htext)
{
  /* chain the list tails back through the sequence of suppressed tags */
  if (htext->stackptr > 0) {
    STACK_PREV(htext).content_list_tail = STACK_TOP(htext).content_list_tail;
  } else {
    htext->parsed_term_tail = STACK_TOP(htext).content_list_tail;
  }

  htext->stackptr--;

#ifdef LIBWWW_DEBUG_VERBOSE
  fprintf(stderr,
	  "In html_pop_suppressed_element(%d): stackptr=%d\n",
	  REQUEST_ID(htext->request), htext->stackptr);
  if (htext->stackptr >= 0)
    print_prolog_term(STACK_TOP(htext).content_list_tail, "content_list_tail");
  else
    print_prolog_term(htext->parsed_term_tail, "parsed_term_tail");
#endif

  return;
}

/* search the stack to see if there is a matching element */
PRIVATE int find_matching_elt(HText *htext, int elt_number)
{
  int i;
  for (i=htext->stackptr; i>=0; i--) {
#ifdef LIBWWW_DEBUG_VERBOSE
    fprintf(stderr, "In find_matching_elt\n");
    fprintf(stderr,
	    "i=%d htext->stack[i].element_number=%d(%s) elt_number=%d(%s)\n",
	    i,
	    htext->stack[i].element_number, 
	    SGML_findTagName(htext->dtd, htext->stack[i].element_number),
	    elt_number,
	    SGML_findTagName(htext->dtd, elt_number));
#endif
    if (htext->stack[i].element_number == elt_number)
      return i;
  }
  return -1;
}


PRIVATE inline HTTag *special_find_tag(HText *htext, int element_number)
{
  static HTTag pcdata_tag = {"pcdata", NULL, 0, PCDATA_SPECIAL};
  if (element_number == PCDATA_SPECIAL)
    return &pcdata_tag;
  return SGML_findTag(htext->dtd, element_number);
}

/* This is a per-request termination handler */
PRIVATE int html_parse_termination_handler (HTRequest  *request,
					    HTResponse *response,
					    /* param= HText object associated
					       with request */
					    void 	  *param,
					    int 	  status)
{
  ((HText *) param)->status = status;

  if (total_number_of_requests > 0)
    total_number_of_requests--;
  /* if the last request has finished, stop the event loop 
     and unregister the callbacks */
  if (total_number_of_requests == 0) {
    HTEventList_stopLoop();
    HText_unregisterElementCallback();
    HText_unregisterTextCallback();
  }

  delete_HText_obj((HText *) param);

#ifdef LIBWWW_DEBUG
  fprintf(stderr,
	  "In html_parse_termination_handler: cleaning up after request %d\n",
	  REQUEST_ID(request));
#endif

  /* Clean Up */
  free_request_context((REQUEST_CONTEXT *) HTRequest_context(request));
  HTRequest_clear(request);

  return TRUE;
}


PRIVATE void html_libwww_abort_request(HTRequest *request, int status,
				       char *description, ...)
{
  va_list args;
  HText *htext =
    (HText *)
    ((REQUEST_CONTEXT *)HTRequest_context(request))->userdata;

  htext->status = status;

  va_start(args, description);
  va_end(args);
  fprintf(stderr, "In Request %d:\n", REQUEST_ID(request));
  xsb_warn(description,args);

  if (total_number_of_requests > 0)
    total_number_of_requests--;
  /* if the last request has finished, stop the event loop 
     and unregister the callbacks */
  if (total_number_of_requests == 0) {
    HTEventList_stopLoop();
#ifdef LIBWWW_DEBUG
    fprintf(stderr, "In html_libwww_abort_request: event loop halted\n");
#endif
    /* we probably need to unregister the handlers here */
  }

  delete_HText_obj(htext);

#ifdef LIBWWW_DEBUG
  fprintf(stderr,
	  "In html_libwww_abort_request: cleaning up after request %d\n",
	  REQUEST_ID(request));
#endif

  /* Clean Up */
  free_request_context((REQUEST_CONTEXT *) HTRequest_context(request));
  HTRequest_kill(request);

  return;
}



/* HText creation and deletion callbacks */
PRIVATE HText *create_HText_obj( HTRequest *             request,
				 HTParentAnchor *        anchor,
				 HTStream *              output_stream)
{
  HText *me = NULL;
  if (request) {
    if ((me = (HText *) HT_CALLOC(1, sizeof(HText))) == NULL)
      HT_OUTOFMEM("libwww_parse_html");
    me->request = request;
    me->node_anchor =  anchor;
    me->target = output_stream;
    me->dtd = HTML_dtd();
    me->suppress_is_default = 
      ((REQUEST_CONTEXT *)HTRequest_context(request))->suppress_is_default;
    me->parsed_term = p2p_new();
    c2p_list(me->parsed_term);
    me->parsed_term_tail = me->parsed_term;
    me->status = HT_ERROR;
    me->stackptr = -1;
  }

#ifdef LIBWWW_DEBUG
  fprintf(stderr, "In create_HText_obj(%d):\n", REQUEST_ID(request));
#endif

  HTNet_deleteAfter(general_parse_abort_handler);
  /* Hook up userdata to the request context */
  ((REQUEST_CONTEXT *)HTRequest_context(request))->userdata = (void *)me;
  HTRequest_addAfter(request,
		     html_parse_termination_handler,
		     NULL,
		     me, /* param to pass to the terminate filter */
		     HT_ALL,
		     HT_FILTER_LAST,
		     YES);
  return me;
}


PRIVATE BOOL delete_HText_obj(HText *me)
{
  int i;
  prolog_term 
    parsed_result =
    ((REQUEST_CONTEXT *)HTRequest_context(me->request))->parsed_result,
    status_term =
    ((REQUEST_CONTEXT *)HTRequest_context(me->request))->status_term;
#ifdef LIBWWW_DEBUG
  int request_id = REQUEST_ID(me->request);
  fprintf(stderr,
	  "In delete_HText_obj(%d): stackptr=%d\n", request_id, me->stackptr);
#endif

  /* close open tags on stack */
  for (i=me->stackptr; i>=0; i--)
    if (parsing(me))
      html_pop_element(me);
    else
      html_pop_suppressed_element(me);

  /* terminate the parsed prolog terms list */
  c2p_nil(me->parsed_term_tail);

  /* pass the result to the outside world */
  if (is_var(me->parsed_term))
    p2p_unify(parsed_result, me->parsed_term);
  else
    xsb_warn("LIBWWW_PARSE_HTML: Request %d: Arg 4 (Parse result) must be a variable",
	      REQUEST_ID(me->request));
  if (is_var(status_term))
    c2p_int(me->status,status_term);
  else
    xsb_warn("LIBWWW_PARSE_HTML: Request %d: Arg 5 (Status) must be a variable",
	      REQUEST_ID(me->request));

  if (me->target) FREE_TARGET(me);
  HT_FREE(me);

#ifdef LIBWWW_DEBUG
  fprintf(stderr, "Request %d: freed the HText obj\n", request_id);
#endif

  return HT_OK;
}

PRIVATE void free_htable(HASH_TABLE *htable)
{
  free(htable->table);
}



PRIVATE void init_tag_table(prolog_term tag_list, HASH_TABLE *tag_tbl)
{
  prolog_term tail, head;
  int i=0;
  char *tagname;
  HKEY tag_number;
  SGML_dtd *dtd = HTML_dtd();
  /* Save tag numbers in the table */
  tail=tag_list;
  while (is_list(tail) && !is_nil(tail) && i < tag_tbl->size) {
    head= p2p_car(tail);
    tail=p2p_cdr(tail);
    tagname = string_val(head);
    tag_number = (strcasecmp(tagname,"pcdata")==0?
		  PCDATA_SPECIAL : SGML_findElementNumber(dtd, tagname));
    add_to_htable(tag_number, tag_tbl);
    i++;
  }
}


PRIVATE HTAssocList *get_form_params(prolog_term form_params)
{
  HTAssocList *formfields=NULL;

  if (!is_list(form_params))
    xsb_abort("LIBWWW_PARSE_HTML: Arg 2 (Form params) must be a list or a variable");
  
  while (!is_nil(form_params)) {
    prolog_term head;
    char *string;

    head = p2p_car(form_params);
    if (is_string(head))
      string = p2c_string(head);
    else
      xsb_abort("LIBWWW_PARSE_HTML: Non-string member in form parameter list");

    form_params = p2p_cdr(form_params);
		
    /* create a list to hold the form arguments */
    if (!formfields) formfields = HTAssocList_new();

    /* parse the content and add it to the association list */
    HTParseFormInput(formfields, string);
  }

  return formfields;
}


/* FORM_POST or FORM_GET */
PRIVATE HTTP_METHOD get_request_method(prolog_term method)
{
  if (is_string(method)) {
    if (strcasecmp(string_val(method), "POST")==0)
      return FORM_POST;
    if (strcasecmp(string_val(method), "GET")==0)
      return FORM_GET;
  }
  else
    xsb_abort("LIBWWW_PARSE_HTML: Invalid HTTP request method");
  /* this is just to pacify the compiler */
  return FORM_GET;
}

