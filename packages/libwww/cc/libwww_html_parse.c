/* File:      libwww_html_parse.c
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
#include <regex.h>
#include <time.h>
#include "basictypes.h"
#include "basicdefs.h"
#include "auxlry.h"
#include "configs/xsb_config.h"
#include "configs/special.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "cinterf.h"
#include "heap_xsb.h"
#include "deref.h"
#include "varstring_xsb.h"

#include "libwww_html_parse.h"

int total_number_of_requests;


/* BOOL, PRIVATE, PUBLIC, etc., are defined in a header in libwww */
BOOL  libwww_html_parse(void)
{
  prolog_term request_term_list = reg_term(1), request_list_tail;
  int request_id=0;

  /* Create a new premptive client */
  HTProfile_newHTMLNoCacheClient ("Parser", "1.0");
  HTAlert_setInteractive(NO);
  HTHost_setEventTimeout(DEFAULT_TIMEOUT);

  if (!is_list(request_term_list))
    xsb_abort("LIBWWW_HTML_PARSE: Argument must be a list!");

  request_list_tail = request_term_list;
  while (is_list(request_list_tail) && !is_nil(request_list_tail)) {
    request_id++;
    setup_request_structure(p2p_car(request_list_tail), request_id);
    request_list_tail = p2p_cdr(request_list_tail);
  }
  total_number_of_requests=request_id;

  /* start the event loop and begin to parse all requests in parallel */
  HTEventList_newLoop();
  
  /*
    HTProfile_delete();
  */
  return TRUE;
}


/* Sets up the libwww request structure for the request specified in
   PROLOG_REQ, including the request context, which contains the info about the
   return parameters. */
PRIVATE void setup_request_structure(prolog_term prolog_req, int request_id)
{
  BOOL	      status;
  HTAnchor    *anchor = NULL;
  HTRequest   *request;
  char *uri = NULL;
  prolog_term form_params;

  /* I think these are essentially local callbacks, because they are
     immediately associated with a request when parsing begins.
     Subsequent registrations affect only subsequent calls. CHECK!!! */
  HText_registerElementCallback(beginElement, endElement);
  HText_registerTextCallback(addText);
  HText_registerCDCallback(create_HText_obj, delete_HText_obj);

  /* get URL */
  uri=string_val(p2p_arg(prolog_req,1));
  if (uri == NULL)
    xsb_abort("LIBWWW_HTML_PARSE: Arg 1 (URI) is invalid!");

  /* This is for forms -- not implemented */
  form_params = p2p_arg(prolog_req,2);
  get_form_params(form_params);
    
  /* Create a new request */
  request = HTRequest_new();
  set_request_context(request, prolog_req, request_id);

  uri = HTParse(uri, NULL, PARSE_ALL);
  /* Create a new Anchor */
  anchor = HTAnchor_findAddress(uri);
  /* Hook up anchor to our request */
  status = HTLoadAnchor(anchor, request);
}


/* This is the callback that captures start tag events */
PRIVATE void beginElement(HText  	*htext, /* where we build everything */
			  int		element_number, /* internal tag # */
			  /* bitmap: tells which tag attrs are present */
			  const BOOL 	*present,
			  /* array of values for the attributes
			     specified by the "present" bitmap */ 
			  const char   **value)
{
#ifdef LIBWWW_DEBUG
  HTTag *tag = SGML_findTag(htext->dtd, element_number);
  printf("In beginElement(%d): stackptr=%d tagname=%s ignore=%d choose=%d\n",
	 REQUEST_ID(htext->request),
	 htext->stackptr, HTTag_name(tag),
	 IS_IGNORED_TAG(element_number, htext),
	 IS_SELECTED_TAG(element_number, htext)
	 );
#endif

  if ((ignoring(htext) && !IS_SELECTED_TAG(element_number, htext))
      || (parsing(htext) && IS_IGNORED_TAG(element_number, htext))) {
    push_ignore_element(htext, element_number);
    return;
  }

  /* parsing or ignoring & found a selected tag */
  if ((parsing(htext) && !IS_IGNORED_TAG(element_number, htext))
      || (ignoring(htext) 
	  && IS_SELECTED_TAG(element_number, htext))) {
    push_element(htext,element_number,present,value);
    return;
  }
}


/* The callback for the end-tag event */
PRIVATE void endElement (HText *htext, int element_number)
{
  int i, match = find_matching_elt(htext, element_number);

#ifdef LIBWWW_DEBUG
  printf("In endElement(%d): stackptr=%d\n",
	 REQUEST_ID(htext->request), htext->stackptr);
#endif
  /* doesn't close an open element */
  if (match < 0) return;

#ifdef LIBWWW_DEBUG_VERBOSE
  printf("match=%d\n", match);
#endif

  for (i=htext->stackptr; i>=match; i--)
    if (parsing(htext))
      pop_element(htext);
    else
      pop_ignore_element(htext);

#ifdef LIBWWW_DEBUG
  if (!STACK_TOP(htext).ignore)
    print_prolog_term(STACK_TOP(htext).elt_term, "elt_term");
#endif

  return;
}



/* The callback to capture text events */
PRIVATE void addText (HText *htext, const char *textbuf, int len)
{
  static vstrDEFINE(pcdata_buf);
  int shift = 0;

  if (ignoring(htext)) return;
  
  /* ignore useless newlines */
  if (strncmp(textbuf,"\n", len) == 0) return;

  push_element(htext, PCDATA_SPECIAL, NULL, NULL);

  /* copy textbuf (which isn't null-terminated) into a variable length str */
  vstrENSURE_SIZE(&pcdata_buf, len+1);
  strncpy(pcdata_buf.string, textbuf, len);
  pcdata_buf.length = len;
  vstrNULL_TERMINATE(&pcdata_buf);

  /* if string starts with a newline, skip the newline */
  if (strncmp(textbuf,"\n", strlen("\n")) == 0)
    shift = strlen("\n");

  /* put the text string into the elt term */
  c2p_string(pcdata_buf.string+shift, p2p_arg(STACK_TOP(htext).elt_term,3));
  pop_element(htext);
  return;
}


/* Collect tag's attributes and make them into a list of the form
   [attval(attr,val), ...]; bind it to Arg 2 of ELT_TERM */
PRIVATE void collect_attributes ( prolog_term  elt_term,
				  HTTag        *tag,
				  const BOOL   *present,
				  const char  **value)
{
  int tag_attributes_number = HTTag_attributes(tag);
  char attrname[MAX_TAG_OR_ATTR_SIZE];
  int cnt;
  prolog_term
    prop_list = p2p_arg(elt_term,2),
    prop_list_tail = prop_list,
    prop_list_head;

  c2p_list(prop_list_tail);

#ifdef LIBWWW_DEBUG_VERBOSE
  printf("In collect_attributes: tag_attributes_number=%d\n",
	 tag_attributes_number);
#endif

  for (cnt=0; cnt<tag_attributes_number; cnt++) {
    if (present[cnt]) {
      strcpy_lower(attrname, HTTag_attributeName(tag, cnt));
      
#ifdef LIBWWW_DEBUG_VERBOSE
      printf("attr=%s, val=%s \n", attrname, (char *)value[cnt]);
#endif
      prop_list_head = p2p_car(prop_list_tail);
      c2p_functor("attval",2,prop_list_head);
      c2p_string(attrname, p2p_arg(prop_list_head,1));
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


PRIVATE void push_element (HText       *htext,
			   int         element_number,
			   const BOOL  *present,
			   const char **value)
{
  char tagname[MAX_TAG_OR_ATTR_SIZE];
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
    printf("In push_element(%d): stackptr=%d\n",
	   REQUEST_ID(htext->request), htext->stackptr);
#endif

  if (htext->stackptr > MAX_HTML_NESTING)
    xsb_abort("LIBWWW_HTML_PARSE: HTML page element nesting exceeds MAX(%d)",
	      MAX_HTML_NESTING);

  /* wire the new elt into where it should be in the content list */
  STACK_TOP(htext).elt_term = p2p_car(location);

  STACK_TOP(htext).element_number = element_number;
  STACK_TOP(htext).ignore = FALSE;

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

  strcpy_lower(tagname, HTTag_name(tag));
  c2p_string(tagname, p2p_arg(STACK_TOP(htext).elt_term, 1));
  collect_attributes(STACK_TOP(htext).elt_term, tag, present, value);
#ifdef LIBWWW_DEBUG_VERBOSE
  printf("elt_name=%s\n", tagname);
  print_prolog_term(STACK_TOP(htext).elt_term, "elt_term");
#endif

  switch (STACK_TOP(htext).element_type) {
  case SGML_EMPTY:
    pop_element(htext);
    break;
  case PCDATA_SPECIAL:
    /* nothing to do: we pop this after thext is inserted in addText */
    break;
  default: /* normal elt */
    STACK_TOP(htext).content_list_tail = p2p_arg(STACK_TOP(htext).elt_term,3);
    c2p_list(STACK_TOP(htext).content_list_tail);
  }
}


/* when we are done with an elt, we must close its contents list and pop the
   stack */
PRIVATE void pop_element(HText *htext)
{
#ifdef LIBWWW_DEBUG_VERBOSE
  printf("In pop_element(%d): stackptr=%d, elt_name=%s\n",
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
PRIVATE void push_ignore_element(HText *htext, int element_number)
{
  /* if empty tag, then just return */
  if (SGML_findTagContents(htext->dtd, element_number) == SGML_EMPTY)
      return;
  /* non-empty tag */
  htext->stackptr++; /* advance ptr, but don't push tag */

  STACK_TOP(htext).element_number = element_number;
  STACK_TOP(htext).ignore = TRUE;

  /* passing content list tail through ignored elements */
  if (htext->stackptr == 0)
    STACK_TOP(htext).content_list_tail = htext->parsed_term_tail;
  else 
    STACK_TOP(htext).content_list_tail = STACK_PREV(htext).content_list_tail;

  return;
}


PRIVATE void pop_ignore_element(HText *htext)
{
  /* chain the list tails back through the sequence of ignored tags */
  if (htext->stackptr > 0) {
    STACK_PREV(htext).content_list_tail = STACK_TOP(htext).content_list_tail;
  } else {
    htext->parsed_term_tail = STACK_TOP(htext).content_list_tail;
  }

  htext->stackptr--;

#ifdef LIBWWW_DEBUG_VERBOSE
  printf("In pop_ignore_element(%d): %d\n",
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
    printf("In find_matching_elt\n");
    printf("i=%d htext->stack[i].element_number=%d(%s) elt_number=%d(%s)\n",
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
PRIVATE int parse_termination_handler (HTRequest  *request,
				       HTResponse *response,
				       /* param= HText object associated
					  with request */
				       void 	  *param,
				       int 	  status)
{
  ((HText *) param)->status = status;

  total_number_of_requests--;
  /* if the last request has finished, stop the event loop 
     and unregister the callbacks */
  if (total_number_of_requests<=0) {
    HTEventList_stopLoop();
    HText_unregisterElementCallback();
    HText_unregisterTextCallback();
  }

  delete_HText_obj((HText *) param);

#ifdef LIBWWW_DEBUG
  printf("In parse_termination_handler: cleaning up after request %d\n",
	 REQUEST_ID(request));
#endif

  /* Clean Up */
  free_request_context((REQUEST_CONTEXT *) HTRequest_context(request));
  HTRequest_clear(request);

  return FALSE;
}



/* HText creation and deletion callbacks */
PRIVATE HText *create_HText_obj( HTRequest *             request,
				 HTParentAnchor *        anchor,
				 HTStream *              output_stream)
{
  HText *me = NULL;
  if (request) {
    if ((me = (HText *) HT_CALLOC(1, sizeof(HText))) == NULL)
      HT_OUTOFMEM("libwww_html_parse");
    me->request = request;
    me->node_anchor =  anchor;
    me->target = output_stream;
    me->dtd = HTML_dtd();
    me->ignore_is_default = 
      ((REQUEST_CONTEXT *)HTRequest_context(request))->ignore_is_default;
    me->parsed_term = p2p_new();
    c2p_list(me->parsed_term);
    me->parsed_term_tail = me->parsed_term;
    me->status = -1;
    me->stackptr = -1;
  }
#ifdef LIBWWW_DEBUG
  printf("In create_HText_obj(%d):\n", REQUEST_ID(request));
#endif
  HTRequest_addAfter(request,
		     parse_termination_handler,
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
  printf("In delete_HText_obj(%d): stackptr=%d\n", 
	 request_id, me->stackptr);
#endif

  /* close open tags on stack */
  for (i=me->stackptr; i>=0; i--)
    pop_element(me);
  /* terminate the parsed prolog terms list */
  c2p_nil(me->parsed_term_tail);

  /* pass the result to the outside world */
  p2p_unify(parsed_result, me->parsed_term);
  c2p_int(me->status,status_term);

  if (me->target) FREE_TARGET(me);
  HT_FREE(me);

#ifdef LIBWWW_DEBUG
  printf("Request %d: freed the HText obj\n", request_id);
#endif

  return HT_OK;
}


/* hash table stuff; deals with integers stored in a table; the integers are
   html element numbers */
PRIVATE int is_in_htable(int item, HASH_TABLE *htable)
{
  int idx = item % htable->size, i = idx;

  while ( htable->table[i] != -1 ) {
    if (htable->table[i] == item)
      return TRUE;
    i++;
    i = i % htable->size;
    if (i == idx) /* reached full circle */
      return FALSE;
  }
  return FALSE;
}


PRIVATE int add_to_htable(int item, HASH_TABLE *htable)
{
  int idx = item % htable->size;
  int i;

  i=idx;
  while ( htable->table[i] != -1 ) {
    i++;
    i = i % htable->size;
    if (i == idx) /* reached full circle */
      return FALSE;
  }

  /* found spot */
  htable->table[i] = item;
  return TRUE;
}


PRIVATE void init_htable(HASH_TABLE *htable, int size)
{
  int i;
  htable->size = size;
  if ((htable->table=(int *)calloc(size, sizeof(int))) == NULL )
    xsb_abort("LIBWWW_HTML_PARSE: Not enough memory!");
  for (i=0; i<size; i++)
    htable->table[i] = -1;
}

PRIVATE void free_htable(HASH_TABLE *htable)
{
  free(htable->table);
}


PRIVATE void init_tag_table(prolog_term tag_list, HASH_TABLE *tag_tbl)
{
  prolog_term tail, head;
  int i=0, tag_number;
  SGML_dtd *dtd = HTML_dtd();
  /* Save tag numbers in the table */
  tail=tag_list;
  while (is_list(tail) && !is_nil(tail) && i < tag_tbl->size) {
    head= p2p_car(tail);
    tail=p2p_cdr(tail);
    tag_number = SGML_findElementNumber(dtd, string_val(head));
    add_to_htable(tag_number, tag_tbl);
    i++;
  }
}


PRIVATE void set_request_context(HTRequest *request,
				 prolog_term prolog_req, int request_id)
{
  REQUEST_CONTEXT *context;
  prolog_term selection;

  if ( (context = 
	(REQUEST_CONTEXT *)calloc(1, sizeof(REQUEST_CONTEXT))) == NULL )
    xsb_abort("LIBWWW_HTML_PARSE: Not enough memory!");

  context->request_id = request_id;

  init_htable(&(context->selected_tags_tbl),SELECTED_TAGS_TBL_SIZE);
  init_htable(&(context->ignored_tags_tbl),IGNORED_TAGS_TBL_SIZE);

  context->parsed_result = p2p_arg(prolog_req,4);
  if(!is_var(context->parsed_result))
    xsb_abort("LIBWWW_HTML_PARSE: Arg 4 (Parse result) must be unbound variable!");

  context->status_term = p2p_arg(prolog_req,5);
  if(!is_var(context->status_term))
    xsb_abort("LIBWWW_HTML_PARSE: Arg 5 (Request status) must be unbound variable!");


  /* get tag selection: f(chosen-list,ignored-list) */
  selection = p2p_arg(prolog_req,3);
  if (is_var(selection)) {
    context->ignore_is_default=FALSE;
  } else if (is_functor(selection) && (p2c_arity(selection)==2)) {
    prolog_term
      select_term=p2p_arg(selection,1), ignore_term=p2p_arg(selection,2);

    if (is_var(select_term))
      context->ignore_is_default=FALSE;
    else if (is_list(select_term)) {
      context->ignore_is_default=TRUE;
      init_tag_table(select_term, &(context->selected_tags_tbl));
    } else
      xsb_abort("LIBWWW_HTML_PARSE: Arg 3 (selection) in f(SELECT,_): SELECT must be a var or a list!");
    if (is_list(ignore_term)) {
      init_tag_table(ignore_term, &(context->ignored_tags_tbl));
    } else if (!is_var(ignore_term))
      xsb_abort("LIBWWW_HTML_PARSE: Arg 3 (selection) in f(_,IGNORE): IGNORE must be a var or a list!");
  } else
    xsb_abort("LIBWWW_HTML_PARSE: Arg 3 (selection) must be a var or f(ARG1,ARG2)!");
  
  /* attach context to the request */
  HTRequest_setContext(request, (void *) context);

  return;
}


PRIVATE void free_request_context (REQUEST_CONTEXT *context)
{
  free_htable(&(context->selected_tags_tbl));
  free_htable(&(context->ignored_tags_tbl));
  free(context);
}


/* not implemented */
PRIVATE void get_form_params(prolog_term form_params)
{
  return;
}

    
/* Copy FROM to TO and lowercase on the way; assume TO is large enough */
void strcpy_lower(char *to, char *from)
{
  int i=0;
  if (from)
    while (from[i]) {
      to[i] = tolower(from[i]);
      i++;
    }
  to[i] = '\0';
}


#ifdef LIBWWW_DEBUG
PRIVATE void print_prolog_term(prolog_term term, char *message)
{ 
#ifdef LIBWWW_DEBUG_VERBOSE
  static vstrDEFINE(StrArgBuf);
  vstrSET(&StrArgBuf,"");
  deref(term);
  print_pterm(term, 1, &StrArgBuf); 
  printf("%s = %s\n", message, StrArgBuf.string);
#endif
} 
#endif
