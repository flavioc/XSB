#ifndef _UTIL_H
#define _UTIL_H

#define BASIC_AUTH "basic"
typedef struct HT_err {
	int code;
	char * message;
	char * type;
} HT_status;

#define MAX_STATUS_MSG_INDEX 48 
HT_status status_msg[] = {  
    { 100, "Continue",                                  "information" }, 
    { 101, "Switching Protocols",                       "information" }, 
    { 200, "OK",                                        "success" }, 
    { 201, "Created",                                   "success" }, 
    { 202, "Accepted",                                  "success" }, 
    { 203, "Non-authoritative Information",             "success" }, 
    { 204, "Document Updated",                          "success" }, 
    { 205, "Reset Content",                             "success" }, 
    { 206, "Partial Content",                           "success" }, 
    { 207, "Partial Update OK",                         "success" }, 
    { 300, "Multiple Choices",                          "redirection" }, 
    { 301, "Moved Permanently",                         "redirection" }, 
    { 302, "Found",                                     "redirection" }, 
    { 303, "See Other",                                 "redirection" }, 
    { 304, "Not Modified",                              "redirection" }, 
    { 305, "Use Proxy",                                 "redirection" }, 
    { 306, "Proxy Redirect",                            "redirection" }, 
    { 307, "Temporary Redirect",                        "redirection" }, 
    { -401, "Unauthorized",                             "client_error" },
    { -403, "Forbidden",                                "client_error" },
    { -404, "Not Found",                                "client_error" },
    { -405, "Method Not Allowed",                       "client_error" },
    { -406, "Not Acceptable",                           "client_error" },
    { -407, "Proxy Authentication Required",            "client_error" },
    { -409, "Conflict",                                 "client_error" },
    { -411, "Length Required",                          "client_error" },
    { -412, "Precondition Failed",                      "client_error" },
    { -413, "Request Entity Too Large",                 "client_error" },
    { -414, "Request-URI Too Large",                    "client_error" },
    { -415, "Unsupported Media Type",                   "client_error" },
    { -416, "Range Not Satisfiable",                    "client_error" },
    { -417, "Expectation Failed",                       "client_error" },
    { -418, "Reauthentication Required",                "client_error" },
    { -419, "Proxy Reauthentication Reuired",           "client_error" },
    { -500, "Internal Server Error",                    "server_error" },
    { -501, "Not Implemented",                          "server_error" },
    { -502, "Bad Gateway",                              "server_error" },
    { -503, "Service Unavailable",                      "server_error" },
    { -504, "Gateway Timeout",                          "server_error" },
    { -505, "HTTP Version not supported",               "server_error" },
    { -506, "Partial update Not Implemented",           "server_error" },
    { -900, "Weird, should never happen",               "internal" },
    { -901, "We are in a select",                       "internal" }, 
    { -902, "Negative value",                           "internal" }, 
    { -903, "If we want to pause a stream",             "internal" }, 
    { -904, "Recover pipe line",                        "internal" }, 
    { -905, "Connection timeout",                       "internal" }, 
    { -906, "Can't locate remote host",                 "internal" }
};

typedef struct _HTBasic {		  /* Basic challenge and credentials */
    char *	uid;
    char *	pw;
    BOOL	proxy;				     /* Proxy authentication */
} HTBasic;


HTBasic *HTBasic_new();
BOOL Basic_credentials (HTRequest *request, HTBasic *basic);
int Basic_generate (HTRequest *request, void *context, int mode);
void time_comparison(char *, prolog_term);
HTChunk *HTGetFormAnchorToChunk (HTAssocList *formdata,
                                  HTAnchor *anchor,
                                  HTRequest *request);

#endif 

#define MAX 7

int load = FALSE;
#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif
