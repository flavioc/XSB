#include "WWWLib.h"
#include "WWWHTTP.h"
#include "WWWInit.h"
#include "HTAABrow.h"

#include "cinterf.h"
#include "basictypes.h"

#include "xsb_libwww_util.c"

HTBasic * auth_info;
HTRequest * request = NULL;
HTResponse * response = NULL;
HTChunk * chunk = NULL;
HTChunk * result = NULL;
HTParentAnchor * anchor = NULL;

HTAssocList * headers = NULL;
HTAssocList *cur = NULL;
HTAssoc * pres;

prolog_term Request_term, Response_term;
prolog_term Form_list, element_list, head, tail;

char *url, *cwd, *absolute_url, *string, * method;
char * response_array[MAX];
prolog_term modified_time;
char * name, * value;
char * last_modified;
int timeout_integer;

/* after filter for all retcodes other than HT_NO_ACCESS and HT_NO_PROXY_ACCESS */
int HTInfoFilter (HTRequest * request, HTResponse * response, void * param,
				int status) 
{ 	
	int i;
	for (i=0; i<48; i++) {
		if (status_msg[i].code == status) {
			response_array[0] = "status";
			response_array[1] = status_msg[i].message;
			response_array[2] = status_msg[i].type;
			response_array[3] = NULL;
			add_response(&Response_term, response_array);
		}
	}
}

/* after filter for HT_NO_ACCESS or HT_NO_PROXY_ACCESS retcodes */
int HTAuthFilter (HTRequest * request, HTResponse * response, void * param,
				int status) {	
	int i;
	for (i=0; i<48; i++) {
		if (status_msg[i].code == status) {
			response_array[0] = "status";
			response_array[1] = status_msg[i].message;
			response_array[2] = status_msg[i].type;
			response_array[3] = NULL;
			add_response(&Response_term, response_array);
		}
	}

	if (HTResponse_challenge(response)) {
		Basic_generate(request, auth_info, status);
		HTLoad(request, NO);
		return HT_ERROR;
	}
	return HT_OK;
}

prolog_term get_request_member(prolog_term V)
{ 
	char * str;
	str = p2c_functor(V);

	auth_info = malloc(sizeof(HTBasic));
	
	printf("started with get_member\n");

	if (str == "timeout") 
		timeout_integer = ptoc_int(p2p_arg(V, 1));
	if (str == "if_modified_since")
		modified_time = p2p_arg(V, 1);
	if (str == "authorization") 
	{ 
		auth_info->uid = ptoc_string(p2p_arg(V, 1));
		auth_info->pw = ptoc_string(p2p_arg(V, 2));
	}
	if (str == "head") load = FALSE;
	if (str == "form_list")	Form_list = p2p_arg(V, 1);		
	if (str == "form_method") 
		method = ptoc_string(p2p_arg(V, 1));
	printf("finished with get_member\n");
	return TRUE;
}

add_response(prolog_term * listTail, char ** array)
{
	int i, j;
	prolog_term listHead;
	i=0;
	j=0;
	listHead = p2p_car(*listTail);
	while (array[i++] != NULL); 
	c2p_functor(array[0], i-2, listHead);
	while (array[++j] != NULL) 
		c2p_string(array[j], p2p_arg(listHead, j));
	*listTail = p2p_cdr(*listTail);
}

load_header(HTRequest * Request)
{
	HTHeadAbsolute(url, Request); 
	anchor = HTRequest_anchor(Request);
	headers = HTAnchor_header(anchor);

	if (headers) {
		cur = headers;
		while ((pres = (HTAssoc *) HTAssocList_nextObject(cur))) {
			name = HTAssoc_name(pres);
			value = HTAssoc_value(pres);
			response_array[0] = name;
			response_array[1] = value;
			response_array[2] = NULL;
			add_response(&Response_term, response_array);
				
			if (HTStrCaseMatch("Last-Modified", name)) 
			last_modified = value;
		}
	}
}

bool do_libwww_fetch_url___(void) 
{	/* char *url, prolog_term Request, Prolog_term Response */ 
	url = ptoc_string(1);
	Request_term = reg_term(2);
	Response_term = reg_term(3);

	if (!is_list(Request_term)) xsb_abort("Request_term not a list!");
	  
	printf("start...\n");
	/* get the request members from the Request_term list */
	while (!is_nil(Request_term)) 
	{		
		head = p2p_car(Request_term);
		get_request_member(head);
		Request_term = p2p_cdr(Request_term);
	}

	HTProfile_newPreemptiveClient("fetch_url", "1.0");
 
	HTNet_addAfter(HTInfoFilter, NULL, NULL, HT_ALL, HT_FILTER_LAST);
 	HTNet_addAfter(HTAuthFilter, NULL, NULL, HT_NO_ACCESS, HT_FILTER_LAST);
 	HTNet_addAfter(HTAuthFilter, NULL, NULL, HT_NO_PROXY_ACCESS, HT_FILTER_LAST);

	if (timeout_integer) HTHost_setEventTimeout(timeout_integer);

	/* make Response_term a list */
	c2p_list(Response_term);

	printf("before url...\n");
	if (url) {
		request = HTRequest_new();
		cwd = HTGetCurrentDirectoryURL();
		HTRequest_setOutputFormat(request, WWW_SOURCE);             
		HTRequest_addConnection(request, "close", "");

		absolute_url = HTParse(url, cwd, PARSE_ALL);
		chunk =	(HTChunk *)HTLoadToChunk(absolute_url, request);
 		HT_FREE(absolute_url);
		HT_FREE(cwd);

	} else 
		xsb_abort("Bad parameters - please try again\n");
		
	/* check for the newness of last_modified time */ 
	if (modified_time != NULL) 
		time_comparison(last_modified, modified_time);
	else 
		load = TRUE;

	if ((is_nil(Request_term)) || (load == TRUE)) {
		if (chunk) {
			string = HTChunk_toCString(chunk);
			response_array[0] = "content"; 
			response_array[1] = string;
			response_array[2] = NULL;
			add_response(&Response_term, response_array);
			HT_FREE(string);
		} 
	}	
	
	/* get the header information and add to the response_term list */
	load_header(request);

	HTRequest_delete(request);
	HTProfile_delete();

	return TRUE;
}



bool do_libwww_form_request___(void) 
{	/* form_request(+Url, +Request, -Response) */
	HTAssocList * Form_fields = NULL;
	HTAnchor * anchor = NULL;
	char *string, *value;
	url = ptoc_string(1);
	Request_term = reg_term(2);
	Response_term = reg_term(3);
			
	HTProfile_newNoCacheClient("form-request", "1.0");	
	HTNet_addAfter(HTInfoFilter, NULL, NULL, HT_ALL, HT_FILTER_LAST);
 	HTNet_addAfter(HTAuthFilter, NULL, NULL, HT_NO_ACCESS, HT_FILTER_LAST);
 	HTNet_addAfter(HTAuthFilter, NULL, NULL, HT_NO_PROXY_ACCESS, HT_FILTER_LAST);

	if (!is_list(Request_term)) xsb_abort("Request_term not a list!");

	/* get the request members from the Request_term list */
	while (!is_nil(Request_term)) 
	{		
		head = p2p_car(Request_term);
		get_request_member(head);
		Request_term = p2p_cdr(Request_term);
	}
	c2p_nil(Request_term);
	
	if (!is_list(Form_list)) xsb_abort("form_list must be a list!");

	while (!is_nil(Form_list)) {
		element_list = p2p_car(Form_list);
		if (!is_list(element_list)) xsb_abort("wrong form list pattern!");
		head = p2p_car(element_list);
		tail = p2p_cdr(element_list);
		c2p_string(name, head);
		c2p_string(value, tail);
		c2p_nil(element_list);
		Form_list = p2p_cdr(Form_list);
		strcat(name, value);
		
		/* create a list to hold the form arguments */
		 if (!Form_fields) Form_fields = HTAssocList_new();
		
		/* parse the content and add it to the association list */
		HTParseFormInput(Form_fields, name);
	}

	if (timeout_integer) HTHost_setEventTimeout(timeout_integer);

	if (url && Form_fields) {
		request = HTRequest_new(); 
		HTRequest_setOutputFormat(request, WWW_SOURCE);             
		/* get an anchor object for the url */
		anchor = HTAnchor_findAddress(url);
		if (method == "POST") {
			/* post the data and get the result in a chunk */
			result = HTPostFormAnchorToChunk(Form_fields, anchor, request);

			load_header(request);
	
			/* If result != NULL then we have the data */
			if (result) {
				/* convert the chunk to C string */
				string = HTChunk_toCString(result);
				response_array[0] = "content"; 
				response_array[1] = string;
				response_array[2] = NULL;
				add_response(&Response_term, response_array);
				HT_FREE(string);
			} 
		} else if (method == "GET") {
				/* get the data and get the result in a chunk */
				result = HTGetFormAnchorToChunk(Form_fields, anchor, request);
			
				load_header(request);

				/* If result != NULL then we have the data */
				if (result) {
					/* convert the chunk to C string */
					string = HTChunk_toCString(result);
					response_array[0] = "content"; 
					response_array[1] = string;
					response_array[2] = NULL;
					add_response(&Response_term, response_array);
					HT_FREE(string);
				}
		}
	} else
		xsb_abort("Bad parameters - please try again\n");
	
	HTRequest_delete(request);
	HTProfile_delete();
	return TRUE;

}
