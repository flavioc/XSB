/* File:      xsb_libwww.c
** Author(s): kifer, Hui Zhang
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
#include "cinterf.h"
#include "error_xsb.h"
#include "heap_xsb.h"
#include "deref.h"
#include "varstring_xsb.h"

#include "xsb_libwww_util.h"
#include "xsb_libwww.h"

int total_number_of_requests;

/* BOOL, PRIVATE, PUBLIC, etc., are defined in a Libwww header */

/* Calling sequence:
       libwww_request([req1,req2,...])

   Each req: f(URL, REQUEST-Params, LIBWWW-Result, ERROR-Code)
   REQUEST-Params:
       	 timeout: user-specified timeout in seconds
		 if_modified_since: user-specified last-modification time, fetch 
		     the page only if the actual last-modified is more recent
		 authorization: user-supplied user id and password 
		 head: the user only wants the meta-information, such as 
		     content, server, date, last_modified time, etc.
		 form_method: user-supplied form request method, GET or POST
		 form_list: user-supplied name and value pairs 
*/

int global;

/* these are for tracing */
PRIVATE int printer (const char * fmt, va_list pArgs)
{
    return (vfprintf(stdout, fmt, pArgs));
}

PRIVATE int tracer (const char * fmt, va_list pArgs)
{
    return (vfprintf(stderr, fmt, pArgs));
}


BOOL do_libwww_request___(void)
{
  prolog_term request_term_list = reg_term(1), request_list_tail;
  int request_id=0;

  /* Create a new premptive client */
  HTProfile_newHTMLNoCacheClient ("libwww-request", "1.0");

  HTAlert_setInteractive(YES);
  HTHost_setEventTimeout(DEFAULT_TIMEOUT);

  HTPrint_setCallback(printer);
  HTTrace_setCallback(tracer);
  HTSetTraceMessageMask("*");

  if (!is_list(request_term_list))
    xsb_abort("XSB_LIBWWW_REQUEST: Argument must be a list!");

  global = FALSE;
  request_list_tail = request_term_list;
  total_number_of_requests = 0;
  while (is_list(request_list_tail) && !is_nil(request_list_tail)) {
    request_id++;
	printf("in while loop\n");
    setup_libwww_request(p2p_car(request_list_tail), request_id);
    request_list_tail = p2p_cdr(request_list_tail);
    total_number_of_requests=request_id;
  }

  printf("in do_request, total_number_of_requests is %d\n", total_number_of_requests);
  
  /* start the event loop and begin to process all requests in parallel */
  if (total_number_of_requests > 0 && !global) {
    global=TRUE;
    puts("1111111111111111111");
    HTEventList_newLoop();
  }

  puts("oooooooooooo");
  
  //HTProfile_delete(); 
  return TRUE;
}

/* Sets up the libwww request structure for the request specified in
   PROLOG_REQUEST, including the request context, which contains the
   information about the request input and output parameters. */
PRIVATE void setup_libwww_request(prolog_term prolog_request,
								  int request_id)
{
  HTRequest   *request;
  char        *url = NULL;
  LIBWWW_REQUEST_CONTEXT *context;

  if ( (context = (LIBWWW_REQUEST_CONTEXT *)calloc(1, 
	    sizeof(LIBWWW_REQUEST_CONTEXT))) == NULL )
    xsb_abort("XSB_LIBWWW: Not enough memory!");

  printf("in setup_libwww for the %d th request\n", request_id);

  /* extract the URL for the request */
  url=string_val(p2p_arg(prolog_request,1));
  printf("url is %s \n", url);
  if (url == NULL)
    xsb_abort("XSB_LIBWWW: Arg 1 (URL) is invalid!");

  /* Create a new request */
  request = HTRequest_new();
  /* set the libwww_request_context for the request */
  set_libwww_request_context(request, prolog_request, context, request_id);

  printf("after set_libwww for the %d request, timeout=%d\n", request_id,
	 context->timeout);

  if (context->timeout) HTHost_setEventTimeout(context->timeout);
  HTRequest_setOutputFormat(request, WWW_SOURCE);             

  /* process the request accordingly */
  process_request(url, request, context);
  return;
}

PRIVATE void process_request(char *url,
						HTRequest *request,
                        LIBWWW_REQUEST_CONTEXT *context)
{
  /* if form_request_flag is set, form_request, otherwise fetch_url */
  if (context->form_request_flag == FALSE) 
    fetch_url(url,request,context);	   /* process fetch-url */
  else 
    form_request(url,request,context); /* process form-request */
}

PRIVATE void fetch_url(char *url, HTRequest *request,
                   LIBWWW_REQUEST_CONTEXT *context)
{
  char *cwd, *absolute_url;
  BOOL status = TRUE;  
  HTAnchor *anchor;
  printf("in fetch_url\n");

  /* load the meta-information first */

  //load_header(url, context);
  printf("after load_header\n");


  /* if modtime_flag is set, check the newness of last_modified time */ 
  if (context->modtime_flag) modtime_comparison(context);

  printf("load_page_flag is %d for the %d request \n", context->load_page_flag, context->request_id);
  
  /* only fetch the page if the load_page_flag is set */
  if(context->load_page_flag == TRUE) {
	/* if auth_flag is set, attach the libwww_auth_termination_handler */
    //    if(context->auth_flag == TRUE) {
      //HTRequest_addAfter(request, libwww_auth_termination_handler,NULL,
	             //NULL,HT_ALL,HT_FILTER_LAST,YES);
    //	}
	
    //else /* attach the libwww_termination handler to the request */

    puts("adding term filter");
 	  
		HTRequest_addAfter(request,
		     libwww_termination_handler,
		     NULL,
		     NULL, 
		     HT_ALL,
		     HT_FILTER_LAST,
		     YES);

    /* get current directory */
    cwd = HTGetCurrentDirectoryURL();
    HTRequest_addConnection(request, "close", "");
    absolute_url = HTParse(url, cwd, PARSE_ALL);
    anchor = HTAnchor_findAddress(absolute_url);

    printf("before load chunk\n");
  
    /*
    HTRequest_setPreemptive(request, YES);
    */

    /* Now start the load */
    puts("p688888888888888");
    if ((context->chunk = HTLoadAnchorToChunk(anchor, request)) == NULL) {
      status = FALSE; 
    } else {
      printf("after load chunk: context->chunk = %p\n", context->chunk);
      /* HTRequest_setContext(request, context); */
    }
  }
  else 
    c2p_nil(context->libwww_result);
 
  puts("p66666666666");
  HT_FREE(absolute_url);
  HT_FREE(cwd);
  puts("pppppppppppppppppp");
  return;
}


PRIVATE int libwww_termination_handler (HTRequest  *request,
     			                           HTResponse *response,
	     		                           void *param,
		    	                           int status)
{
  char *body=NULL;
  prolog_term status_term;
  LIBWWW_REQUEST_CONTEXT * context;
  /* get the libwww_request_context	of the current request */
  context = (LIBWWW_REQUEST_CONTEXT*)HTRequest_context(request);
  status_term = context->status_term;

  printf("in term_handler for %d request, status=%d\n",context->request_id,status);
  printf("context->chunk = %p\n", context->chunk);

  printf("the size of the chunk is %d\n", HTChunk_size(context->chunk));

  if (total_number_of_requests > 0)
    total_number_of_requests--;
  printf("total_number_of_remaining_requests is %d\n", total_number_of_requests);
  
  /* if the last request has finished, stop the event loop */
  if (total_number_of_requests==0) {
    if (global) {
      HTEventList_stopLoop();
      puts("fffffffffffffff");
    } else {
      global=TRUE;
      puts("aaaaaaaaaaa");
    }
  }

  printf("before processing chunk\n");
  /* if the chunk is not NULL, add the result to libwww_result */

  if (HTChunk_size(context->chunk)>0) {
    printf("start to process chunk\n");
    body = HTChunk_toCString(context->chunk);

    printf("the body is %s\n", body);
    printf("i am in chunk %s\n", body);

    if (body) {
      add_libwww_result(&(context->libwww_result), "libwww_content", body);
      HT_FREE(body);
    }
    HTChunk_delete(context->chunk);
  }

  printf("I am going to bind status after this\n");

  /* terminate the libwww_result */
  c2p_nil(context->libwww_result); 

  /* bind the status to status_term and delete the request object. */
  c2p_int(status,status_term);
  delete_request_obj(request);
  puts("pgggggggggggggg");

  return HT_OK;
}

PRIVATE int libwww_auth_termination_handler (HTRequest  *request,
     			                             HTResponse *response,
	     		                             void *param,
		    	                             int status)
{
  prolog_term status_term;
  LIBWWW_REQUEST_CONTEXT * context;
  /* get the libwww_request_context	of the current request */
  context = (LIBWWW_REQUEST_CONTEXT*)HTRequest_context(request);
  status_term = context->status_term;

  /*
  printf("in term_handler, status=%d\n",status);
  */

  if (total_number_of_requests > 0)
    total_number_of_requests--;
  printf("auth_term: total_number_of_requests is %d\n", total_number_of_requests);
  
  /* if the last request has finished, stop the event loop */
  if (total_number_of_requests==0) {
    HTEventList_stopLoop();
  }

    /* try the request with the supplied uid and passwd */
	{  
    Basic_generate(request, &(context->auth_info), status);
    context->auth_flag = FALSE;	    /* unset the auth_flag */
    HTLoad(request, NO); 	        /* reload the page     */
    return HT_ERROR;
	}

  return HT_OK;
}

PRIVATE void form_request(char *url, HTRequest *request,
			  LIBWWW_REQUEST_CONTEXT  *context)
{
  prolog_term list_tail;
  char *string;
  HTAssocList * formfields = NULL;
  HTAnchor * anchor = NULL;

  list_tail = context->form_list;
  if (!is_list(list_tail)) 
    xsb_abort("LIBWWW_FORM_REQUEST: form_list must be a list!");

  while (!is_nil(list_tail)) {
    string = p2c_string(p2p_car(list_tail));
    list_tail = p2p_cdr(list_tail);
		
    /* create an AsscoList to hold the form arguments */
    if (!formfields) formfields = HTAssocList_new();
    /* parse the content of form_list to the association list */
    HTParseFormInput(formfields, string);
  }  

  HTRequest_addAfter(request,libwww_termination_handler,
		         NULL,NULL,HT_ALL,HT_FILTER_LAST,YES);

  if (formfields) {
    anchor = HTAnchor_findAddress(url);

	/* if POST is the method, call HTPostFormAnchorToChunk() */
    if (strcasecmp(context->method, "POST")==0)
      context->chunk = HTPostFormAnchorToChunk(formfields, anchor, request);

	/* if GET is the method, call HTGetFormAnchorToChunk() */
    else if (strcasecmp(context->method, "GET")==0)
      context->chunk = HTGetFormAnchorToChunk(formfields, anchor, request);

	/* set the contxt to the request */
	HTRequest_setContext(request, context);

    if (formfields)
      HTAssocList_delete(formfields);
    HTEventList_loop(request);
  }
  return;
}


/* function to check the newerness of the page if modtime_flag is set */
PRIVATE void modtime_comparison(LIBWWW_REQUEST_CONTEXT *context)
{               
  char * mon;
  /* for the actual and user-specific last_modified time respectively */
  time_t lastmod_sec, user_sec;
  struct tm time1; 
  char seps[] = ", :";
  
  /* extract actual last_modified time into tm structure */
  strtok(context->last_modtime, seps);
  time1.tm_mday = atoi(strtok(NULL, seps));
  mon = strtok(NULL, seps);
  if (strcmp(mon, "Jan")==0) time1.tm_mon = 0;
  if (strcmp(mon, "Feb")==0) time1.tm_mon = 1;
  if (strcmp(mon, "Mar")==0) time1.tm_mon = 2;
  if (strcmp(mon, "Apr")==0) time1.tm_mon = 3;
  if (strcmp(mon, "May")==0) time1.tm_mon = 4;
  if (strcmp(mon, "Jun")==0) time1.tm_mon = 5;
  if (strcmp(mon, "Jul")==0) time1.tm_mon = 6;
  if (strcmp(mon, "Aug")==0) time1.tm_mon = 7;
  if (strcmp(mon, "Sep")==0) time1.tm_mon = 8;
  if (strcmp(mon, "Oct")==0) time1.tm_mon = 9;
  if (strcmp(mon, "Nov")==0) time1.tm_mon = 10;
  if (strcmp(mon, "Dec")==0) time1.tm_mon = 11;

  time1.tm_year = atoi(strtok(NULL, seps))-1900;
  time1.tm_hour = atoi(strtok(NULL, seps));
  time1.tm_min = atoi(strtok(NULL, seps));
  time1.tm_sec = atoi(strtok(NULL, seps));
		
  /* call the mktime() to calculate the time in seconds from 1/1/1970 */
  lastmod_sec = mktime(&time1);
  user_sec = mktime(&context->user_modtime);

  if (lastmod_sec != -1 && user_sec != -1) {
    if (lastmod_sec < user_sec) context->load_page_flag = FALSE;
  }
  else 
    xsb_abort("LIBWWW_FETCH_URL: invalid Last-Modified value\n");
}


PRIVATE BOOL delete_request_obj(HTRequest *request)
{
  /* cleanup the request context associated with the request */
  free_libwww_request_context((LIBWWW_REQUEST_CONTEXT *) HTRequest_context(request));
  HTRequest_clear(request);
  return HT_OK;
}  
  

/* function to add results back to libwww_result, functor is the name of
the result and body is the content  */
PRIVATE void add_libwww_result(prolog_term *result_list, 
                              char *functor, char *body)
{
  prolog_term listHead = p2p_car(*result_list);

  c2p_functor(functor, 1, listHead);
  c2p_string(body, p2p_arg(listHead, 1));

  *result_list = p2p_cdr(*result_list);
  c2p_list(*result_list);

  printf("at the end of add_libwww\n");

}

/* function for the HEAD operation to get the meta-information */
PRIVATE void load_header(char *url, LIBWWW_REQUEST_CONTEXT *context)
{
  char * name, * value;
  HTAssocList * headers = NULL;
  HTAssocList * cur = NULL;
  HTAssoc * pres;
  HTRequest * requ = NULL;
  HTParentAnchor * anchor = NULL;

  //HTProfile_newPreemptiveClient("HTTPHeadApplication", "1.0");

  //HTProfile_newHTMLNoCacheClient("HTTPHeadApplication", "1.0");

  requ = HTRequest_new();
  HTRequest_setOutputFormat(requ, WWW_SOURCE);
  
  HTRequest_setPreemptive(requ, YES);

  if(context->head_only) {
    /* If load the meta-info only, attach the libwww_termination_handler
    to the request */
    HTRequest_addAfter(requ, libwww_termination_handler,
		     NULL, NULL, HT_ALL, HT_FILTER_LAST, YES);
  }

  printf("in load_header\n");

  HTHeadAbsolute(url, requ); 
  anchor = HTRequest_anchor(requ);
  headers = HTAnchor_header(anchor);
  if (headers) {
    cur = headers;
    while ((pres = (HTAssoc *) HTAssocList_nextObject(cur))) {
      name = HTAssoc_name(pres);
      value = HTAssoc_value(pres);
      add_libwww_result(&(context->libwww_result), name, value);
     
      /* get the actual last_modified time for the page */				
      if (HTStrCaseMatch("Last-Modified", name)) context->last_modtime = value;
    }
  }
  HTRequest_delete(requ);
}

PRIVATE void set_libwww_request_context(HTRequest *request, 
					prolog_term prolog_request,
					LIBWWW_REQUEST_CONTEXT *context,
					int request_id)
{
  prolog_term request_params;

  context->request_id = request_id;

  /* setup the default values for the flags*/
  context->auth_flag=FALSE;
  context->form_request_flag=FALSE;
  context->head_only=FALSE;
  context->load_page_flag=TRUE;
  context->chunk = NULL;

  printf("in set_libwww before get_request\n");

  /* This is for request parameters */
  request_params = p2p_arg(prolog_request,2);

  /* call get_request_params() to extract the request parameters */
  get_request_params(request_params, context);

  printf("after get_request_params\n");

  context->libwww_result = p2p_arg(prolog_request,3);

  /* make libwww_result a list */
  c2p_list(context->libwww_result);

  if(!is_var(context->libwww_result))
    xsb_abort("XSB_LIBWWW: Arg 3 (Libwww result) must be unbound variable!");

  context->status_term = p2p_arg(prolog_request,4);
  if(!is_var(context->status_term))
    xsb_abort("XSB_LIBWWW: Arg 4 (Request status) must be unbound variable!");

  /* attach context to the request */
  HTRequest_setContext(request, (void *) context);
  return;
}


PRIVATE void free_libwww_request_context (LIBWWW_REQUEST_CONTEXT *context)
{
  free(context->chunk);
  free(context);
  return;
}


/* function to extract the individual request parameters from the request_params */
PRIVATE void get_request_params(prolog_term            params,
                                LIBWWW_REQUEST_CONTEXT *context )
{
  prolog_term param;
  char * str;
  while(!is_nil(params)) {
    param = p2p_car(params);
    str = p2c_functor(param);

	printf("in get_request\n");

    /* user-specified timeout */ 
    if (strcmp(str, "timeout") == 0){
      if (is_int(p2p_arg(param, 1)))
      context->timeout = p2c_int(p2p_arg(param, 1));
    }
    
	/* user-specified last-modification time */
    else if (strcmp(str, "if_modified_since") == 0)
    {  
      context->modtime_flag = TRUE;        
    
      /* extract the user-specified modification-time into struct tm */
      context->user_modtime.tm_mday = p2c_int(p2p_arg(param,1));
      context->user_modtime.tm_mon = p2c_int(p2p_arg(param,2))-1;
      context->user_modtime.tm_year = p2c_int(p2p_arg(param,3))-1990;
      context->user_modtime.tm_hour = p2c_int(p2p_arg(param,4));
      context->user_modtime.tm_min = p2c_int(p2p_arg(param,5));
      context->user_modtime.tm_sec = p2c_int(p2p_arg(param,6));
    }
  
	/* extract user-supplied user id and password into struct HTBasic */
    else if (strcmp(str, "authorization") == 0) 
    {
      context->auth_flag = TRUE;
      context->auth_info.uid = p2c_string(p2p_arg(param, 1));
      context->auth_info.pw = p2c_string(p2p_arg(param, 2));
    }
 
	/* whether the user wants the meta-information only */
    else if (strcmp(str, "head") == 0) 
    {
      if (strcmp(p2c_string(p2p_arg(param, 1)), "yes") == 0) {
        context->head_only = TRUE;
        context->load_page_flag = FALSE;
      }
    }
 
	/* the form-list name and value pairs supplied by the user */
    else if (strcmp(str, "form_list") == 0)	
    {	
      context->form_list = p2p_arg(param, 1);
	  context->form_request_flag = TRUE;
    }
		
	/* user-specified form method */
    else if (strcmp(str, "form_method") == 0) 
    {	
      context->method = p2c_string(p2p_arg(param, 1));
    }
	params = p2p_cdr(params);
  } 
  return;
}


/* function to do form request using GET method */
PRIVATE HTChunk * HTGetFormAnchorToChunk (HTAssocList *formdata,
                                  HTAnchor    *anchor,
                                  HTRequest   *request)
{
  if (formdata && anchor && request) {
    HTChunk  *chunk = NULL;
    HTStream *target = HTStreamToChunk(request, &chunk, 0);
    HTRequest_setOutputStream(request, target);
    if (HTGetFormAnchor(formdata, anchor, request))
      return chunk;
    else {
      HTChunk_delete(chunk);
      return NULL;
    }
  }
  return NULL;
}
    
