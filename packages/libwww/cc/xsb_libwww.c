
#include "xsb_libwww_debug.h"

#include <time.h>
#include <string.h> 
#include "WWWLib.h"
#include "WWWHTTP.h"
#include "WWWInit.h"
#include "HTAABrow.h"
#include "WWWApp.h"

#include "cinterf.h"
#include "basictypes.h"
#include "xsb_libwww_util.h"

#ifndef XSB_LIBWWW_SEPARATE_UTIL
#include "xsb_libwww_util.c"
#endif

HTBasic   *auth_info; /* structure for authentication information */
struct tm *time2;     /* structure for user-specified modified time */

prolog_term Request_term, Response_term, Response_member, head;

/* prolog_term Status_term; */

prolog_term Form_list;

char     *response_array[MAX], *method, *last_modified;
int       timeout_integer;      /* user-specified timeout */
HTChunk  *result = NULL;
int       code = -5;

void reset(void);
prolog_term get_request_member(prolog_term);
void add_response(prolog_term *, char ** );
void load_header(char *);

/* function to reset the global flag variables. */
void reset(void)
{
  time_flag = FALSE;
  auth_flag = FALSE;
  filter_flag = FALSE;
  status_flag = FALSE;
  head_flag = FALSE;
  load_flag = TRUE;
  k = FALSE;
  auth_info = NULL;
  result = NULL;
}

/* after filter for all retcodes other than HT_NO_ACCESS and 
   HT_NO_PROXY_ACCESS */
int InfoFilter(HTRequest *request, HTResponse *response,
	       void *param, int status) 
{ 
  /*  code = status; */

  /* if in the head operation, don't add status back */
  if(!head_flag || k == TRUE) {
    code = status;
    /*
      c2p_functor("status", 1, Status_term);
      c2p_int(code, p2p_arg(Status_term,1));
    */
  }
  if (request)
    HTRequest_delete(request);
}

/* after filter for HT_NO_ACCESS or HT_NO_PROXY_ACCESS retcodes */
int HTAuthFilter (HTRequest *request, HTResponse *response,
		  void *param, int status) 
{
  code = status;
  
  /* change filter_flag to true, so will not go to InfoFilter */
  filter_flag = TRUE;

  /* proceed only if there is authentication information from the request*/
  if (auth_flag) {
    Basic_generate(request, auth_info, status);
    auth_flag = FALSE;
    HTLoad(request, NO); 
    return HT_ERROR;
  }
  else {
    /*
      c2p_functor("status", 1, Status_term);
      c2p_int(code, p2p_arg(Status_term,1));
    */
    return HT_OK;
  }
}

/* function to handle form request termination */
int terminate_handler (HTRequest *request, HTResponse *response, 
		       void *param, int status) 
{   
  char * string;
  code = status;

  /*
    c2p_functor("status", 1, Status_term);
    c2p_int(code, p2p_arg(Status_term,1));
  */

  /* if OK, add form request result to Response_member */
  if (status == HT_LOADED) {
    if (result && (string = HTChunk_data(result))) {
      response_array[0] = "result";
      response_array[1] = string;
      response_array[2] = NULL;
      add_response(&Response_member, response_array);
      HTChunk_delete(result);
    }
  } 
  
  if (request)
    HTRequest_delete(request); 
  c2p_nil(Response_member);
  HTProfile_delete();
  HTEventList_stopLoop();
}

/* function to get the request information from the list */
prolog_term get_request_member(prolog_term V)
{ 
  char *str;

  str = p2c_functor(V);
  auth_info = malloc(sizeof(HTBasic));
  time2 = malloc(sizeof(struct tm));

  if (strcmp(str, "timeout") == 0){
    if (is_int(p2p_arg(V, 1)))
      timeout_integer = p2c_int(p2p_arg(V, 1));
  } else if (strcmp(str, "if_modified_since") == 0) {  
    time_flag = TRUE;        
    
    /* extract the if_modified_since infomation into struct tm */
    time2->tm_mday = p2c_int(p2p_arg(V,1));
    time2->tm_mon  = p2c_int(p2p_arg(V,2))-1;
    time2->tm_year = p2c_int(p2p_arg(V,3))-1900;
    time2->tm_hour = p2c_int(p2p_arg(V,4));
    time2->tm_min  = p2c_int(p2p_arg(V,5));
    time2->tm_sec  = p2c_int(p2p_arg(V,6));
  } else if (strcmp(str, "authorization") == 0) {
    auth_flag = TRUE;
    auth_info->uid = p2c_string(p2p_arg(V, 1));
    auth_info->pw  = p2c_string(p2p_arg(V, 2));
  } else if (strcmp(str, "head") == 0) {
    if (strcmp(p2c_string(p2p_arg(V, 1)), "yes") == 0) {
      k = TRUE;
      load_flag = FALSE;
    }
  } else if (strcmp(str, "form_list") == 0) {	
    Form_list = p2p_arg(V, 1);
    printf("i get the form list\n");
  } else if (strcmp(str, "form_method") == 0) {	
    method = p2c_string(p2p_arg(V, 1));
    printf("i get the method %s \n", method);
  }
  return TRUE;
}

/* function to add terms back to response_term list */
void add_response(prolog_term *list, char **array)
{
  int i, j;
  prolog_term listHead;
  i=0;
  j=0;
  listHead = p2p_car(*list);
  while (array[i++] != NULL);
  c2p_functor(array[0], i-2, listHead);
  while (array[++j] != NULL)
    c2p_string(array[j], p2p_arg(listHead, j));

  *list = p2p_cdr(*list);
  c2p_list(*list);
}


/* function for the HEAD operation */
void load_header(char *url)
{
  char *name, *value;
  HTAssocList *headers = NULL;
  HTAssocList *cur = NULL;
  HTAssoc *pres;
  HTRequest *requ = NULL;
  HTParentAnchor *anchor = NULL;
  HTProfile_newPreemptiveClient("HTTPHeadApplication", "1.0");
  requ = HTRequest_new();
  HTRequest_setOutputFormat(requ, WWW_SOURCE);
  head_flag = TRUE;

  HTHeadAbsolute(url, requ); 
  anchor = HTRequest_anchor(requ);
  headers = HTAnchor_header(anchor);
  if (headers) {
    cur = headers;
    while ((pres = (HTAssoc *) HTAssocList_nextObject(cur))) {
      name = HTAssoc_name(pres);
      value = HTAssoc_value(pres);
      response_array[0] = name;
      response_array[1] = value;
      response_array[2] = NULL;
      add_response(&Response_member, response_array);
     
      /* get the last_modified time */				
      if (HTStrCaseMatch("Last-Modified", name)) last_modified = value;
    }
  }
}

int do_libwww_fetch_url___(void) 
{	
  /* char *url, prolog_term Request, prolog_term Response, prolog_term
     Status */ 

  char *url, *cwd, *absolute_url;
  char *string;
  prolog_term Status_term;
  HTRequest *request = NULL;
  HTChunk   *chunk = NULL;
  url = ptoc_string(1);
  Request_term  = reg_term(2);
  Response_term = reg_term(3);
  Status_term   = reg_term(4);
  
  /* make Response_term a list */
  c2p_list(Response_term);
  Response_member = Response_term;

  if (!is_list(Request_term) && !is_nil(Request_term))
    xsb_abort("LIBWWW_FETCH_URL: Arg 2 (Request) must be a list!");
	  
  /* get the request members from the Request_term list */
  while (!is_nil(Request_term)){		
    head = p2p_car(Request_term);
    get_request_member(head);
    Request_term = p2p_cdr(Request_term);
  }

  HTProfile_newPreemptiveClient("fetch_url", "1.0");
  HTRequest_setOutputFormat(request, WWW_SOURCE);             
 
  HTNet_addAfter(HTAuthFilter, NULL, NULL, HT_NO_ACCESS, HT_FILTER_LAST);
  HTNet_addAfter(HTAuthFilter, NULL, NULL, HT_NO_PROXY_ACCESS, HT_FILTER_LAST);
  
  /* if HTAuthFilter called, won't call InfoFilter */
  if (filter_flag == FALSE)
    HTNet_addAfter(InfoFilter, NULL, NULL, HT_ALL, HT_FILTER_LAST);

  /* setup timeout if specified */
  if (timeout_integer) HTHost_setEventTimeout(timeout_integer);

  load_header(url);
  head_flag = FALSE; 

  /* check for the newness of last_modified time */ 
  if (time_flag) time_comparison(last_modified, time2);

  if (url) {  
    if(load_flag == TRUE) {
      request = HTRequest_new();
      cwd = HTGetCurrentDirectoryURL();
      HTRequest_setOutputFormat(request, WWW_SOURCE);             
      HTRequest_addConnection(request, "close", "");

      absolute_url = HTParse(url, cwd, PARSE_ALL);
      chunk = HTLoadToChunk(absolute_url, request);
      if (chunk) {
        string = HTChunk_toCString(chunk);
        if (string){
          response_array[0] = "content"; 
          response_array[1] = string;
          response_array[2] = NULL;
          add_response(&Response_member, response_array);
          HT_FREE(string);
	  HT_FREE(absolute_url);
          HT_FREE(cwd);
        }
      } 
      c2p_nil(Response_member); 
    } else c2p_nil(Response_member);
  } else 
    xsb_abort("LIBWWW_FETCH_URL: Bad URL, %s", url);

  c2p_functor("status", 1, Status_term);
  c2p_int(code, p2p_arg(Status_term,1));

  HTProfile_delete();
  reset();
  return TRUE;

}

int do_libwww_form_request___(void) 
{	/* form_request(+Url, +Request, -Response) */
  char *string;
  char *url = NULL;
  prolog_term Status_term;
  HTAssocList *formfields = NULL;
  HTAnchor    *anchor = NULL;
  HTRequest   *request = NULL;

  url = ptoc_string(1);
  Request_term  = reg_term(2);
  Response_term = reg_term(3);
  Status_term   = reg_term(4);

  c2p_list(Response_term);
  Response_member = Response_term;
			
  HTProfile_newNoCacheClient("form-request", "1.0");	
  HTNet_addAfter(terminate_handler, NULL, NULL, HT_ALL, HT_FILTER_LAST); 

  if (!is_list(Request_term))
    xsb_abort("LIBWWW_FORM_REQUEST: Arg 2 (the request) must be a list");

  /* get the request members from the Request_term list */
  while (!is_nil(Request_term)){		
    head = p2p_car(Request_term);
    get_request_member(head);
    Request_term = p2p_cdr(Request_term);
  }
	
  if (!is_list(Form_list))
    xsb_abort("LIBWWW_FORM_REQUEST: form parameters in Arg 2 be a list");

  while (!is_nil(Form_list)) {
    head = p2p_car(Form_list);
    string = p2c_string(head);
    Form_list = p2p_cdr(Form_list);
		
    /* create a list to hold the form arguments */
    if (!formfields) formfields = HTAssocList_new();
		
    /* parse the content and add it to the association list */
    HTParseFormInput(formfields, string);
  }

  if (timeout_integer) HTHost_setEventTimeout(timeout_integer); 

  if (url && formfields) {
    request = HTRequest_new(); 
    HTRequest_setOutputFormat(request, WWW_SOURCE);             
    anchor = HTAnchor_findAddress(url);
    if ((strcmp(method, "POST")==0) || (strcmp(method, "Post")==0)) 
      result = HTPostFormAnchorToChunk(formfields, anchor, request);
    else if ((strcmp(method, "GET")==0) || (strcmp(method, "Get")==0))
      result = HTGetFormAnchorToChunk(formfields, anchor, request);

    if (formfields) HTAssocList_delete(formfields);
    HTEventList_loop(request);
  } else
    xsb_abort("Bad parameters - please try again\n");

  c2p_functor("status", 1, Status_term);
  c2p_int(code, p2p_arg(Status_term,1));

  reset();
  return TRUE;
}
