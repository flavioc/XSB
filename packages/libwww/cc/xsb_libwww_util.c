#include "xsb_libwww_debug.h"

#ifdef XSB_LIBWWW_SEPARATE_UTIL

#include <string.h>
#include "WWWLib.h"
#include "WWWHTTP.h"
#include "WWWInit.h"
#include "HTAABrow.h"
#include "WWWApp.h"

#include "cinterf.h"
#include "basictypes.h"
#include "xsb_libwww_util.h"

#endif

HTBasic *HTBasic_new()
{
  HTBasic * me = NULL;
  if ((me= (HTBasic *) HT_CALLOC(1, sizeof(HTBasic))) == NULL)
    HT_OUTOFMEM("HTBasic_new");
  return me;
}

/*
**      Make basic authentication scheme credentials and register this
**      information in the request object as credentials. They will then
**      be included in the request header. An example is 
**
**              "Basic AkRDIhEF8sdEgs72F73bfaS=="
**
**      The function can both create normal and proxy credentials
**      Returns HT_OK or HT_ERROR
*/

BOOL Basic_credentials (HTRequest * request, HTBasic * basic)
{
  if (request && basic) {
    char * cleartext = NULL;
    char * cipher = NULL;
    int cl_len = strlen(basic->uid ? basic->uid : "") +
      strlen(basic->pw ? basic->pw : "") + 5;
    int ci_len = 4 * (((cl_len+2)/3) + 1);
    if ((cleartext = (char *) HT_CALLOC(1, cl_len)) == NULL)
      HT_OUTOFMEM("basic_credentials");
    *cleartext = '\0';
    if (basic->uid) strcpy(cleartext, basic->uid);
    strcat(cleartext, ":");
    if (basic->pw) strcat(cleartext, basic->pw);
    if ((cipher = (char *) HT_CALLOC(1, ci_len + 3)) == NULL)
      HT_OUTOFMEM("basic_credentials");
    HTUU_encode((unsigned char *) cleartext, strlen(cleartext), cipher);

    /* Create the credentials and assign them to the request object */
    {
      int cr_len = strlen("basic") + ci_len + 3;
      char * cookie = (char *) HT_MALLOC(cr_len+1);
      if (!cookie) HT_OUTOFMEM("basic_credentials");
      strcpy(cookie, "Basic ");
      strcat(cookie, cipher);
      HTTRACE(AUTH_TRACE, "Basic Cookie `%s\'\n" _ cookie);
      
      /* Check whether it is proxy or normal credentials */
      if (basic->proxy)
	HTRequest_addCredentials(request, "Proxy-Authorization", cookie);
      else
	HTRequest_addCredentials(request, "Authorization", cookie);
      
      HT_FREE(cookie);
    }
    HT_FREE(cleartext);
    HT_FREE(cipher);
    return HT_OK;
  }
  return HT_ERROR;
}

/*      HTBasic_generate
**      ----------------
**      This function generates "basic" credentials for the challenge
**      found in the authentication information base for this request.
**      The result is stored as an association list in the request 
**      object. This is a callback function for the AA handler.
*/
int Basic_generate (HTRequest *request, void *context, int mode)
{ 
  char * url;
  HTBasic * basic = (HTBasic *) context;
  BOOL proxy = (mode==HT_NO_PROXY_ACCESS) ? YES : NO;
  if (request) {
    const char * realm = HTRequest_realm(request);

    if (!basic) {
      basic = HTBasic_new();
      if (proxy) {
	url = HTRequest_proxy(request);
	basic->proxy = YES;
	HTAA_updateNode(proxy, BASIC_AUTH, realm, url, basic);
      } else {
	url = HTAnchor_address((HTAnchor*)HTRequest_anchor(request));
	HTAA_updateNode(proxy, BASIC_AUTH, realm, url, basic);
	HT_FREE(url);
      }
    }
    
    Basic_credentials(request, basic);
    
    /*
      url = HTAnchor_address((HTAnchor*)HTRequest_anchor(request));
      HTAA_deleteNode(proxy, BASIC_AUTH, realm, url); HT_FREE(url);
    */
    
    return HT_ERROR;
  }
  return HT_OK;
}
