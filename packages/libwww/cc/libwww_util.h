/* File:      libwww_util.h
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



#include "WWWLib.h"
#include "WWWHTTP.h"
#include "WWWInit.h"
#include "HTAABrow.h"
#include "WWWApp.h"
#include "WWWXML.h"
#include "HTUtils.h"
#include "HTTPReq.h"
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

#define XSB_LIBWWW_PACKAGE
#include "../prolog_includes/http_errors.h"


/* definitions and macros included in all files */


/*
#define LIBWWW_DEBUG
#define LIBWWW_DEBUG_VERBOSE
#define LIBWWW_DEBUG_TERSE
*/
#ifdef LIBWWW_DEBUG_VERBOSE
#define LIBWWW_DEBUG
#endif
#ifdef LIBWWW_DEBUG
#define LIBWWW_DEBUG_TERSE
#endif

/* special tag type that we use to wrap around text */
#define PCDATA_SPECIAL 	  -77


/* from HTTP.c */
#define FREE_TARGET(t)	(*(t->target->isa->_free))(t->target)

/* Must define this, since HTStream is just a name aliased to _HTStream */
struct _HTStream {
    const HTStreamClass *	isa;
};

enum request_type {FETCH, HTMLPARSE, XMLPARSE, HEADER};
typedef enum request_type REQUEST_TYPE;

union hkey {
  int intkey;
  char *strkey;
};
typedef union hkey HKEY;
struct hash_table {
  int 	       size;
  REQUEST_TYPE type;
  HKEY 	       *table;
};
typedef struct hash_table HASH_TABLE;

enum http_method {GET, POST, PUT};
typedef enum http_method HTTP_METHOD;

typedef struct auth AUTHENTICATION;
struct auth {
  char 	         *realm;
  char 	         *uid;   /* username */
  char 	         *pw;    /* password */
  AUTHENTICATION *next;  /* next authorization record (used for subrequests) */
};

/* used to pass the input info to request and get output info from request back
   to the Prolog side*/
struct request_context {
  int  request_id;
  int  suppress_is_default;
  int  convert2list;    /* if convert pcdata to Prolog lists on exit */
  int  is_subrequest;  /* In XML parsing, we might need to go to a different
			  URI to fetch an external reference. This spawns a new
			  blocking subrequest with the same context. */
  int  statusOverride; /* If set, this status code should replace the one
			  returned by libwww */
  time_t last_modtime; /* page modtime */
  /* data structure where we build parsed terms, etc. */
  void *userdata;
  /* input */
  REQUEST_TYPE type;	    /* request type: html/xml parsing, fetching page */
  int  timeout;
  time_t user_modtime;      /* oldest modtime the user can tolerate */
  prolog_term formdata;
  AUTHENTICATION auth_info; /* list of name/pw pairs */
  HTTP_METHOD  method;
  HASH_TABLE selected_tags_tbl;
  HASH_TABLE suppressed_tags_tbl;
  HASH_TABLE stripped_tags_tbl;
  /* output */
  prolog_term status_term;
  prolog_term result_params;  /* additional params returned in the result */
  prolog_term request_result; /* either the parse tree of a string containing
				 the HTML page */
  HTChunk     *result_chunk;  /* used only by the FETCH method. Here we get the
				 resulting page before converting it to
				 prolog_term */
};
typedef struct request_context REQUEST_CONTEXT;

typedef void DELETE_USERDATA(void *userdata);

#define REQUEST_ID(request) \
  ((REQUEST_CONTEXT *)HTRequest_context(request))->request_id

/* like strcpy, but also converts to lowercase */
void strcpy_lower(char *to, const char *from);


int add_to_htable(HKEY item, HASH_TABLE *htable);
int is_in_htable(const HKEY item, HASH_TABLE *htable);


void print_prolog_term(prolog_term term, char *message);

void html_register_callbacks();
void HTXML_newInstance (HTStream *		me,
			HTRequest *		request,
			HTFormat 		target_format,
			HTStream *		target_stream,
			XML_Parser              xmlparser,
			void * 			context);
void add_result_param(prolog_term *result_param, 
		      char *functor, int cnt, ...);
void add_subrequest_error(HTRequest *request, int status);
