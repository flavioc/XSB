/* File:      libwww_request.c
** Author(s): kifer
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
#include "libwww_request.h"
#include "deref.h"


/* Calling sequence:
       libwww_request([req1,req2,...])

   Each req: functor(URL, REQUEST_Params, PARSED-Result, ERROR-Code)
   functor: htmlparse, xmlparse, fetch, header.
       	    The first two are requests to parse HTML/XML. Fetch means retrieve
       	    a page without parsing; header means retrieve header only.
	    All except "header" could be form fillouts, which return a page or
	    a parsed page.
   REQUEST_Params: [param, param, ...]
           Param: timeout(secs), if_modified_since(date-in-GMT-format),
	       	  authentication(realm,username,passwd),
		  formdata('attr-val-pair-list'),
		  selection(chosen-taglist,suppressed-taglist,stripped-taglist)
	          means:
	     	    parse only inside the tags on the chosen tag list. Stop
	     	    parsing if a suppressed tag is found. Resume if a chosen
	     	    tag is found, etc. 
		    Stripped tags are those that just get discarded.
		    selection(_,suppressed-tag-list,...) means: parse all tags
		    except those in the suppressed tags list.
		    f(chosen-tag-list,_,...) means: parse only inside the
		    chosen tags. 
*/
void libwww_request()
{
  prolog_term request_term_list = reg_term(1), request_list_tail;
  int request_id=0;

  /* Create a new premptive client */
  HTProfile_newHTMLNoCacheClient("XSB Request", "1.0");

  /* We must enable alerts in order for authentication modules to call our own
     callback defined by HTAlert_add below. However, we delete all alerts other
     than those needed for authentication */ 
  HTAlert_setInteractive(YES);
  /* Note: we just register a function to send the credentials.
     We don't need to register authentication filters, because they are already
     registered by profile initialization */
  HTAlert_deleteOpcode(HT_A_PROGRESS); /* harmless, but useless */
  HTAlert_deleteOpcode(HT_A_MESSAGE);
  HTAlert_deleteOpcode(HT_A_CONFIRM);  /* the next 3 coredump, if allowed */
  HTAlert_deleteOpcode(HT_A_PROMPT);
  HTAlert_deleteOpcode(HT_A_USER_PW);
  /* register alert callbacks that supplies credentials */
  HTAlert_add(libwww_send_credentials,HT_A_USER_PW); /* usrname and password */
  HTAlert_add(libwww_send_credentials,HT_A_SECRET);  /* just the password    */

  HTPrint_setCallback(printer);
  HTTrace_setCallback(tracer);
#if 0
  HTSetTraceMessageMask("*");
#endif

  /* This catch-all filter is needed in order to catch termination of
     subrequests, like the ones issues to parse external entities or to handle
     the if-modified-since user directive. */
  HTNet_deleteAfter(request_termination_handler);
  HTNet_addAfter(request_termination_handler,
		 NULL,
		 NULL,
		 HT_ALL,
		 HT_FILTER_LAST);

  /* use abort here, because this is a programmatic mistake */
  if (!is_list(request_term_list))
    xsb_abort("LIBWWW_REQUEST: Argument must be a list");

  request_list_tail = request_term_list;
  total_number_of_requests=0;
  event_loop_runnung = FALSE;
  while (is_list(request_list_tail) && !is_nil(request_list_tail)) {
    request_id++;
    total_number_of_requests++;
    setup_request_structure(p2p_car(request_list_tail), request_id);
    request_list_tail = p2p_cdr(request_list_tail);
  }

  /* start the event loop and begin to parse all requests in parallel */
  if (total_number_of_requests > 0) {
#ifdef LIBWWW_DEBUG
    xsb_dbgmsg("In libwww_request: starting event loop. Total requests=%d",
	       total_number_of_requests);
#endif

    event_loop_runnung = TRUE;
    HTEventList_newLoop();

#ifdef LIBWWW_DEBUG
    xsb_dbgmsg("In libwww_request: event loop ended: total outstanding requests=%d", total_number_of_requests);
#endif
  }
  
  /* free all registered callbacks and global preferences, so that this won't
     interfere with other applications */
  HTProfile_delete();
  return;
}


/* Sets up the libwww request structure for the request specified in
   PROLOG_REQ, including the request context, which contains the info about the
   return parameters.
   Note that we use xsb_abort here instead of abort handlers, because the error
   conditions handled here are programmatic mistakes rather than network
   conditions. */
PRIVATE void setup_request_structure(prolog_term req_term, int request_id)
{
  int	      status;
  HTAnchor    *anchor = NULL;
  HTRequest   *request=NULL;
  HTAssocList *formdata=NULL;
  char 	      *uri = NULL;
  REQUEST_CONTEXT *context;

  /* Create a new request and attach the context structure to it */
  request=HTRequest_new();
  context=set_request_context(request,req_term,request_id);
  setup_callbacks(context->type);
  /* get URL */
  uri = extract_uri(req_term,request);
  /* get other params */
  get_request_params(req_term, request);
  /* don't keep connection open; otherwise, idle connection timer event kicks
     in and gives a segfault */
  HTRequest_addConnection(request, "close", "");

  formdata = (context->formdata ? get_form_params(context->formdata) : NULL);

  uri = HTParse(uri, NULL, PARSE_ALL);
  /* Create a new Anchor */
  anchor = HTAnchor_findAddress(uri);
  /* Hook up anchor to our request */
  switch (context->type) {
  case HEADER:
    /* header-only requests seem to require preemptive mode (otherwise timer
       interrupts and crashes them */
    HTRequest_setPreemptive(request, YES);
    status = (YES == HTHeadAnchor(anchor,request));
    break;
  case FETCH:
    {
      HTStream *target;
      HTRequest_setOutputFormat(request, WWW_SOURCE);
      /* redirect stream to chunk */
      target = HTStreamToChunk(request, &(context->result_chunk), 0);
      HTRequest_setOutputStream(request, target);
      /* then do the same as in the case of parsing */
    }
  case HTMLPARSE:
  case XMLPARSE:
    if (formdata) {
      if (context->method == GET)
	status = (YES == HTGetFormAnchor(formdata,anchor,request));
      else if (context->method == POST)
	status = (NULL != HTPostFormAnchor(formdata,anchor,request));
    } else {
      /* not a form request */
      status = (YES==HTLoadAnchor(anchor, request));
    }
    break;
  default:
    xsb_abort("LIBWWW_REQUEST: Invalid type of request");
  }

#ifdef LIBWWW_DEBUG_TERSE
  switch (context->type) {
  case HTMLPARSE:
    xsb_dbgmsg("Request %d: request type: htmlparse", request_id);
    break;
  case XMLPARSE:
    xsb_dbgmsg("Request %d: request type: xmlparse", request_id);
    break;
  case HEADER:
    xsb_dbgmsg("Request %d: request type: header", request_id);
    break;
  case FETCH:
    xsb_dbgmsg("Request %d: request type: fetch", request_id);
    break;
  default:
    xsb_dbgmsg("Request %d: request type: invalid", request_id);
  }
  if (formdata)
    xsb_dbgmsg("Request %d: HTTP Method: %s",
	       request_id, (context->method==GET ? "FORM,GET" : "FORM,POST"));
  else
    xsb_dbgmsg("Request %d: HTTP Method: NON-FORM REQ", request_id);
#endif

  if (formdata) HTAssocList_delete(formdata);

  if (!status) {
    release_libwww_request(request);
    /* use abort, because it is a programmatic mistake */
    xsb_abort("LIBWWW_REQUEST: Invalid data in URI %d", uri);
  }
  HT_FREE(uri);
  HTHost_setEventTimeout(context->timeout);
}


void libwww_abort_request(HTRequest *request, int status,
			  char *description, ...)
{
  REQUEST_CONTEXT *context = ((REQUEST_CONTEXT *)HTRequest_context(request));
  va_list args;

  if (description) {
    va_start(args, description);
    fprintf(stdwarn, "\n++ Warning: ");
    vfprintf(stdwarn, description, args);
    fprintf(stdwarn, "\n");
    va_end(args);
  }

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In libwww_abort_request: Killing: request %d, status=%d remaining requests: %d",
	     REQUEST_ID(request), status, total_number_of_requests);
#endif

  context->statusOverride = status;
  HTRequest_kill(request);
  return;
}


/* In XML parsing, we sometimes have to issue additional requests to go and
   fetch external entities. Here we handle termination of such subrequests.
   A subrequest is independent of its parent request, except that it inherits
   the parent's id and context. When a subrequest returns, it should NOT
   release the context. */
PRIVATE void handle_subrequest_termination(HTRequest *request, int status)
{
  REQUEST_CONTEXT *context = (REQUEST_CONTEXT *)HTRequest_context(request);
#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In handle_subrequest_termination: status=%d", status);
#endif

  HTRequest_clear(request);
  /* restore parent process' context */
  context->is_subrequest = FALSE;
  return;
}


/* note that we use xsb_abort here instead of abort handlers, because the error
   conditions handled here are programmatic mistakes rather than network
   conditions. */
PRIVATE REQUEST_CONTEXT *set_request_context(HTRequest *request,
					     prolog_term req_term,
					     int request_id)
{
  REQUEST_CONTEXT *context;

  if ((context=(REQUEST_CONTEXT *)calloc(1,sizeof(REQUEST_CONTEXT))) == NULL)
    xsb_abort("LIBWWW_REQUEST: Not enough memory");

  context->request_id = request_id;
  context->suppress_is_default = FALSE;
  context->convert2list = FALSE;
  context->statusOverride = 0;
  context->is_subrequest = FALSE;
  context->userdata = NULL;
  context->last_modtime = 0;
  context->timeout = DEFAULT_TIMEOUT;
  context->user_modtime = 0;
  context->formdata=0;
  context->auth_info.realm = NULL;
  context->auth_info.uid = NULL;
  context->auth_info.pw = NULL;
  context->method = GET;
  context->selected_tags_tbl.table = NULL;
  context->suppressed_tags_tbl.table = NULL;
  context->stripped_tags_tbl.table = NULL;

  context->type = get_request_type(req_term);
  context->result_chunk = NULL;

  init_htable(&(context->selected_tags_tbl),
	      SELECTED_TAGS_TBL_SIZE,context->type);
  init_htable(&(context->suppressed_tags_tbl),
	      SUPPRESSED_TAGS_TBL_SIZE,context->type);
  init_htable(&(context->stripped_tags_tbl),
	      STRIPPED_TAGS_TBL_SIZE,context->type);
  /* output */
  context->result_params = p2p_arg(req_term,3);
  if(!is_var(context->result_params))
    xsb_abort("LIBWWW_REQUEST: Arg 3 (Result parameters) must be unbound variable");
  c2p_list(context->result_params);

  context->request_result = p2p_arg(req_term,4);
  if(!is_var(context->request_result))
    xsb_abort("LIBWWW_REQUEST: Arg 4 (Result) must be unbound variable");

  context->status_term = p2p_arg(req_term,5);
  if(!is_var(context->status_term))
    xsb_abort("LIBWWW_REQUEST: Arg 5 (Status) must be unbound variable");

  /* attach context to the request */
  HTRequest_setContext(request, (void *) context);
  /* we handle only HT_LOADED, HT_ERROR, and HT_NO_DATA conditions.
     We let the others (redirection, authentication, proxy) to be handled by
     the standard Libwww filters. */

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("Request %d: context set", request_id);
#endif

  HTRequest_addAfter(request,
		     request_termination_handler,
		     NULL,
		     NULL,
		     HT_LOADED,
		     HT_FILTER_LAST,
		     NO); /* don't override global filters! */
  HTRequest_addAfter(request,
		     request_termination_handler,
		     NULL,
		     NULL,
		     HT_ERROR,
		     HT_FILTER_LAST,
		     NO); /* don't override global filters! */
  HTRequest_addAfter(request,
		     request_termination_handler,
		     NULL,
		     NULL,
		     HT_NO_DATA,
		     HT_FILTER_LAST,
		     NO); /* don't override global filters! */
  return context;
}


PRIVATE void free_request_context (REQUEST_CONTEXT *context)
{
  AUTHENTICATION *next_auth, *curr_auth;
  if (!context) return;
  free_htable(&(context->selected_tags_tbl));
  free_htable(&(context->suppressed_tags_tbl));
  free_htable(&(context->stripped_tags_tbl));
  /* Note: we don't need to free context->result_chunk, since HTChunk_toCString
     deleted the chunk object, and we freed the chunk data earlier. */
  /* release authentication info */
  next_auth = context->auth_info.next;
  while (next_auth) {
    curr_auth = next_auth;
    next_auth=next_auth->next;
    free(curr_auth);
  }
  free(context);
}


/* Copy FROM to TO and lowercase on the way; assume TO is large enough */
void strcpy_lower(char *to, const char *from)
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
void print_prolog_term(prolog_term term, char *message)
{ 
  static vstrDEFINE(StrArgBuf);
  vstrSET(&StrArgBuf,"");
  deref(term);
  print_pterm(term, 1, &StrArgBuf); 
  xsb_dbgmsg("%s = %s", message, StrArgBuf.string);
} 
#endif


/* these are for tracing */
PRIVATE int printer (const char * fmt, va_list pArgs)
{
    return (vfprintf(stdout, fmt, pArgs));
}

PRIVATE int tracer (const char * fmt, va_list pArgs)
{
    return (vfprintf(stderr, fmt, pArgs));
}


/* this procedure is supposed to get the credentials from the input parameters
   and pass them on through the REPLY argument.
   For this to happen, alerts must be enabled.

*/
BOOL libwww_send_credentials(HTRequest * request, HTAlertOpcode op,
			     int msgnum, const char * dfault, void * realm,
			     HTAlertPar * reply)
{
  AUTHENTICATION *authinfo =
    &(((REQUEST_CONTEXT *)HTRequest_context(request))->auth_info),
    *credentials;

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In libwww_send_credentials: Request=%d default: '%s' input: '%s' msgnum=%d",
	     REQUEST_ID(request), dfault, realm, msgnum);
#endif

  /* the following blocks authentication filters on retry.
     So, if this authentication failed, the retry will call the termination
     filter with the HT_NO_ACCESS or HT_NO_PROXY_ACCESS code, and this is what
     will be returned to the application. */
  HTRequest_addAfter(request, request_termination_handler,
		     NULL, NULL,
		     HT_NO_ACCESS,
		     HT_FILTER_LAST,
		     NO); /* don't override global filters! */
  HTRequest_addAfter(request, request_termination_handler,
		     NULL, NULL,
		     HT_NO_PROXY_ACCESS,
		     HT_FILTER_LAST,
		     NO); /* don't override global filters! */

  credentials = find_credentials(authinfo,realm);
  if (credentials) {
    /* have authentication info */
    HTAlert_setReplyMessage(reply, credentials->uid);
    HTAlert_setReplySecret(reply, credentials->pw);
    return TRUE;
  }
  /* if no credentials supplied, send some phony stuff */
  HTAlert_setReplyMessage(reply, "foo");
  HTAlert_setReplySecret(reply, "foo");
  return TRUE;
}


PRIVATE AUTHENTICATION *find_credentials(AUTHENTICATION *auth_info,char *realm)
{
  AUTHENTICATION *credentials = auth_info;

  while (credentials) {
    if ((credentials->realm == NULL)
	|| (strcmp(credentials->realm, realm) == 0))
      return credentials;
    credentials = credentials->next;
  }
  return NULL;
}


PRIVATE char *extract_uri(prolog_term req_term, HTRequest *request)
{
  static  vstrDEFINE(uristr);
  int 	  urilen;
  char    *uri;
  prolog_term uri_term;

  uri_term=p2p_arg(req_term,1);
  if (is_charlist(uri_term, &urilen)) {
    ((REQUEST_CONTEXT *)HTRequest_context(request))->convert2list=TRUE;
    p2c_chars(uri_term, &uristr);
    uri = uristr.string;
  } else if (is_string(uri_term))
    uri=string_val(uri_term);
  else {
    release_libwww_request(request);
    /* use abort here, because it is a programmatic mistake */
    xsb_abort("LIBWWW_REQUEST: Arg 1 (URI) is invalid");
  }
  return uri;
}


PRIVATE void release_libwww_request(HTRequest *request)
{
  free_request_context((REQUEST_CONTEXT *) HTRequest_context(request));
  HTRequest_clear(request);
}


/* function to extract the individual request parameters from the request_params */
PRIVATE void get_request_params(prolog_term req_term, HTRequest *request)
{
  prolog_term param, req_params=p2p_arg(req_term,2);
  char *paramfunctor;
  REQUEST_CONTEXT *context = (REQUEST_CONTEXT *)HTRequest_context(request);

  while(is_list(req_params) && !is_nil(req_params)) {
    param = p2p_car(req_params);
    paramfunctor = p2c_functor(param);

    switch (paramfunctor[0]) {
    case 't': case 'T': /* user-specified timeout */ 
      if (!is_int(p2p_arg(param, 1)))
	xsb_abort("LIBWWW_REQUEST: Timeout parameter must be an integer");
      context->timeout = p2c_int(p2p_arg(param, 1)) * 1000;
      if (context->timeout <= 0)
	context->timeout = DEFAULT_TIMEOUT;
      break;
    case 'i': case 'I': /* if-modified-since */
      if (!is_string(p2p_arg(param, 1)))
	xsb_abort("LIBWWW_REQUEST: If_modified_since parameter must be a string");
      context->user_modtime =
	HTParseTime(string_val(p2p_arg(param,1)), NULL, YES);
      break;
    case 'a': case 'A': {  /* authorization */
      prolog_term auth_head, auth_tail=p2p_arg(param,1);
      AUTHENTICATION *auth_info = &(context->auth_info);
      
      do {
	if (is_list(auth_tail)) {
	  auth_head=p2p_car(auth_tail);
	  auth_tail=p2p_cdr(auth_tail);
	} else auth_head = auth_tail;

	if (is_string(p2p_arg(auth_head, 1)))
	  auth_info->realm = p2c_string(p2p_arg(auth_head, 1));
	else auth_info->realm = NULL;
	if (is_string(p2p_arg(auth_head, 2)))
	  auth_info->uid = p2c_string(p2p_arg(auth_head, 2));
	else auth_info->uid = NULL;
	/* passwd is always required in auth info */
	if (is_string(p2p_arg(auth_head, 3)))
	  auth_info->pw = p2c_string(p2p_arg(auth_head, 3));
	else auth_info->pw = NULL;

	if (is_list(auth_tail) && !is_nil(auth_tail)) {
	  auth_info->next = (AUTHENTICATION *)calloc(1,sizeof(AUTHENTICATION));
	  auth_info = auth_info->next;
	} else break;

      } while (TRUE);
      auth_info->next=NULL;
      break;
    }
    case 'f': case 'F':  /* formdata: the name and value pairs to fill out a form */
      context->formdata = p2p_arg(param, 1);
      break;
    case 'm': case 'M': {  /* HTTP method: GET/POST/PUT */
      char *method = p2c_string(p2p_arg(param, 1));
      switch (method[1]) {
      case 'O': case 'o':
	context->method = POST;
	break;
      case 'E': case 'e':
	context->method = GET;
	break;
      case 'P': case 'p':
	context->method = PUT;
	break;
      }
      break;
    }
    case 's': case 'S':  /* selection of tags to parse */
      /* tag selection: selection(chosen-list,suppressed-list,strip-list) */
      if (p2c_arity(param)==3) {
	prolog_term
	  select_term=p2p_arg(param,1),
	  suppressed_term=p2p_arg(param,2),
	  strip_term=p2p_arg(param,3);
	
	if (is_var(select_term))
	  context->suppress_is_default=FALSE;
	else if (is_list(select_term)) {
	  context->suppress_is_default=TRUE;
	  init_tag_table(select_term, &(context->selected_tags_tbl));
	} else
	  xsb_abort("LIBWWW_REQUEST: In Arg 2, selection(CHOOSE,_,_): CHOOSE must be a var or a list");

	if (is_list(suppressed_term)) {
	  init_tag_table(suppressed_term, &(context->suppressed_tags_tbl));
	} else if (!is_var(suppressed_term))
	  xsb_abort("LIBWWW_REQUEST: In Arg 2, selection(_,SUPPRESS,_): SUPPRESS must be a var or a list");
	
	if (is_list(strip_term)) {
	  init_tag_table(strip_term, &(context->stripped_tags_tbl));
	} else if (!is_var(strip_term))
	  xsb_abort("LIBWWW_REQUEST: In Arg 2, selection(_,_,STRIP): STRIP must be a var or a list");
      } else {
	xsb_abort("LIBWWW_REQUEST: In Arg 2, wrong number of arguments in selection parameter");
      }
      break;
    default:  /* ignore unknown params */
      break;
    }
    req_params = p2p_cdr(req_params);
  } 
  return;
}



PRIVATE HTAssocList *get_form_params(prolog_term form_params)
{
  HTAssocList *formfields=NULL;

  if (!is_list(form_params))
    xsb_abort("LIBWWW_REQUEST: Form parameters must be a non-empty list");
  
  while (!is_nil(form_params)) {
    prolog_term head;
    char *string;

    head = p2p_car(form_params);
    if (is_string(head))
      string = p2c_string(head);
    else
      xsb_abort("LIBWWW_REQUEST: Non-string member in form parameter list");

    form_params = p2p_cdr(form_params);
		
    /* create a list to hold the form arguments */
    if (!formfields) formfields = HTAssocList_new();

    /* parse the content and add it to the association list */
    HTParseFormInput(formfields, string);
  }
  return formfields;
}


PRIVATE REQUEST_TYPE get_request_type(prolog_term req_term)
{
  char *functor;
  if (!is_functor(req_term)) {
    xsb_abort("LIBWWW_REQUEST: Request must have the form functor(URL,PARAMS,RESULT,STATUS)");
  }
  functor = p2c_functor(req_term);

  if (strncmp("fetch",functor,3)==0) return FETCH;
  if (strncmp("xmlparse",functor,3)==0) return XMLPARSE;
  if (strncmp("htmlparse",functor,3)==0) return HTMLPARSE;
  if (strncmp("header",functor,3)==0) return HEADER;
  xsb_abort("LIBWWW_REQUEST: Invalid type of request");
  return TRUE; /* just to pacify the compiler */
}


PRIVATE void init_htable(HASH_TABLE *htable, int size, REQUEST_TYPE type)
{
  int i;
  if ((type != XMLPARSE) && (type != HTMLPARSE)) {
    htable->table = NULL;
    return;
  }

  htable->type = type;
  htable->size = size;
  if ((htable->table=(HKEY *)calloc(size, sizeof(HKEY))) == NULL )
    /* use xsb_abort here, because it is not worth trying to recover
       from an out of memory error */
    xsb_abort("LIBWWW_REQUEST: Not enough memory");
  for (i=0; i<size; i++)
    if (type == HTMLPARSE)
      htable->table[i].intkey = -1;
    else /* XML */
      htable->table[i].strkey = NULL;
}


PRIVATE void free_htable(HASH_TABLE *htable)
{
  if (!htable || !htable->table) return;
  if (htable->type == HTMLPARSE) {
    free(htable->table);
  } else if (htable->type == XMLPARSE) {
    int i;
    if (!htable) return;
    for (i=0; i < htable->size; i++)
      if (htable->table[i].strkey != NULL)
	free(htable->table[i].strkey);
    free(htable->table);
  } else return;
}


PRIVATE unsigned long key2int(HKEY s, REQUEST_TYPE type)
{
  if (type == HTMLPARSE)
    return s.intkey;
  else if (type == XMLPARSE) {
    unsigned long h = 0;
    while (*(s.strkey))
      h = (h << 5) + h + (unsigned char)*(s.strkey)++;
    return h;
  }
  return 0; /* this is just to make the compiler happy */
}


#define FREE_CELL(hkey,type) \
     (type==HTMLPARSE ? (hkey.intkey==-1) : (hkey.strkey==NULL))

int add_to_htable(HKEY item, HASH_TABLE *htable)
{
  int idx, i;
  if (!htable || !htable->table) return FALSE;

  idx = (int) (key2int(item, htable->type) % htable->size);
  i = idx;
  while (!FREE_CELL(htable->table[i], htable->type)) {
    i++;
    i = i % htable->size;
    if (i == idx) /* reached full circle */
      return FALSE;
  }
  /* found spot */
  if (htable->type==HTMLPARSE)
    htable->table[i].intkey = item.intkey;
  else {
    htable->table[i].strkey = (char *)malloc(strlen(item.strkey)+1);
    strcpy_lower(htable->table[i].strkey, item.strkey);
  }
  return TRUE;
}


#define HASH_CELL_EQUAL(cell,item,type) \
    (type==HTMLPARSE ? (cell.intkey==item.intkey) \
		     : (cell.strkey && strcasecmp(cell.strkey,item.strkey)==0))

/* hash table stuff; deals with HKEY's stored in a table */
int is_in_htable(const HKEY item, HASH_TABLE *htable)
{
  int idx, i;
  if (!htable || !htable->table) return FALSE;

  idx = (int) (key2int(item,htable->type) % htable->size);
  i = idx;
  while (!FREE_CELL(htable->table[i], htable->type)) {
    if (HASH_CELL_EQUAL(htable->table[i], item, htable->type)) {
      return TRUE;
    }
    i++;
    i = i % htable->size;
    if (i == idx) /* reached full circle */
      return FALSE;
  }
  return FALSE;
}


/* This is a per-request termination handler */
PRIVATE int request_termination_handler (HTRequest   *request,
					 HTResponse  *response,
					 void 	     *param,
					 int 	     status)
{
  REQUEST_CONTEXT *context = ((REQUEST_CONTEXT *)HTRequest_context(request));
  void *userdata = context->userdata;

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("Request %d: In request_termination_handler",
	     REQUEST_ID(request));
#endif

  if (context->is_subrequest) {
    handle_subrequest_termination(request, status);
    return !HT_OK;
  }

  /* Redirection code is commented out. It is better handled by the standard
     Libwww redirection/proxy handling filters */
#if 0
  if (status == HT_TEMP_REDIRECT || status == HT_PERM_REDIRECT ||
	status == HT_FOUND || status == HT_SEE_OTHER) {
    HTAnchor *redirection = HTResponse_redirection(response);
    /* if loaded redirection successfully, then return: the request will be
       processed by the existing event loop; otherwise, drop down and terminate
       the request */ 
    if (YES==HTLoadAnchor(redirection,request))
      return !HT_OK;
  }
#endif

  if (total_number_of_requests > 0)
    total_number_of_requests--;
  /* when the last request is done, stop the event loop */
  if ((total_number_of_requests == 0) && event_loop_runnung) {
    HTEventList_stopLoop();
    event_loop_runnung = FALSE;
#ifdef LIBWWW_DEBUG
    xsb_dbgmsg("In request_termination_handler: event loop halted, status=%d",
	       status);
#endif
    /*
      HText_unregisterElementCallback();
      HText_unregisterTextCallback();
    */
  }

  status = (context->statusOverride ? context->statusOverride : status);
  if (is_var(context->status_term))
    c2p_int(status, context->status_term);
  else
    xsb_warn("LIBWWW_REQUEST: Request %d: Arg 5 (Status) must be a var",
	     REQUEST_ID(request));
  deref(context->status_term);

  extract_request_headers(request);
  /* terminate the result parameters list */
  c2p_nil(context->result_params);

  /* Clean Up */
  if (userdata)
    (((USERDATA *)userdata)->delete_method)(userdata);
  else if (context->type == FETCH) {
    char *result_as_string = HTChunk_toCString(context->result_chunk);
    if (result_as_string)
      c2p_string(result_as_string, context->request_result);
    /* Note: HTChunk_toCString frees the chunk, and here we free the chank
       data. Thus, the chunk is completely cleared out. */
    HT_FREE(result_as_string);
  }

#ifdef LIBWWW_DEBUG
  xsb_dbgmsg("In request_termination_handler: Cleanup: request %d, status=%d remaining requests: %d",
	     REQUEST_ID(request), status, total_number_of_requests);
#endif

  release_libwww_request(request);
  /* when a filter returns something other than HT_OK, no other after filters
     will be called */
  return !HT_OK;
}


PRIVATE void setup_callbacks(REQUEST_TYPE type)
{
  switch (type) {
  case HTMLPARSE:
    html_register_callbacks();
    break;
  case XMLPARSE:
    /* Register our new XML Instance handler */
    HTXMLCallback_registerNew(HTXML_newInstance, NULL);
    break;
  case FETCH:
    break;
  case HEADER:
    break;
  }
}


PRIVATE void init_tag_table(prolog_term tag_list, HASH_TABLE *tag_tbl)
{
  prolog_term tail, head;
  int i=0;
  char *tagname;
  HKEY taghandle;
  if ((tag_tbl->type != XMLPARSE) && (tag_tbl->type != HTMLPARSE))
    return;
  /* Save tag numbers in the table */
  tail=tag_list;
  while (is_list(tail) && !is_nil(tail) && i < tag_tbl->size) {
    head= p2p_car(tail);
    tail=p2p_cdr(tail);
    tagname = string_val(head);
    if (tag_tbl->type == XMLPARSE)
      taghandle = (HKEY)tagname;
    else 
      taghandle = (HKEY)(strcasecmp(tagname,"pcdata")==0?
			 PCDATA_SPECIAL
			 : SGML_findElementNumber(HTML_dtd(), tagname));
    add_to_htable(taghandle, tag_tbl);
    i++;
  }
}


/* Add term to the result parameter list. FUNCTOR is the name of the functor to
   use for this parameter; BODY is what should appear inside */
PRIVATE void add_result_param(prolog_term *result_param, 
                              char *functor, prolog_term body)
{
  prolog_term listHead = p2p_car(*result_param);
  
  c2p_functor(functor, 1, listHead);
  p2p_unify(body, p2p_arg(listHead, 1));
  
  *result_param = p2p_cdr(*result_param);
  c2p_list(*result_param);
}


/* extract request headers and add then to the result parameters kept in the
   request context */
PRIVATE void extract_request_headers(HTRequest *request)
{
  HTParentAnchor * anchor;
  HTAssocList * headers;
  prolog_term paramvalue_term;
  REQUEST_CONTEXT *context = (REQUEST_CONTEXT *)HTRequest_context(request);

  anchor = HTRequest_anchor(request);
  headers = HTAnchor_header(anchor);
  if (headers) {
    HTAssocList *cur = headers;
    HTAssoc *pres;
    char *paramname, *paramvalue;

    while ((pres = (HTAssoc *) HTAssocList_nextObject(cur))) {
      paramname = HTAssoc_name(pres);
      paramvalue = HTAssoc_value(pres);
      c2p_string(paramvalue, paramvalue_term);

      add_result_param(&(context->result_params), paramname, paramvalue_term);

      /* save the page last_modified time */
      if (HTStrCaseMatch("Last-Modified", paramname))
	context->last_modtime = HTParseTime(paramvalue,NULL,YES);
    }
  }
}

