

#include "xsb_libwww_debug.h"

#ifdef XSB_LIBWWW_SEPARATE_UTIL

#include <time.h>
#include <string.h>
#include "WWWLib.h"
#include "WWWHTTP.h"
#include "WWWInit.h"
#include "HTAABrow.h"
#include "WWWApp.h"

#include "cinterf.h"
#include "basictypes.h"
#include "error_xsb.h"
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
**      This function generates "basic" credentials for the challenge found in
**      the authentication information base for this request. The result is
**      stored as an association list in the request object.
**      This is a callback function for the AA handler.
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
	char * url = HTRequest_proxy(request);
	basic->proxy = YES;
	HTAA_updateNode(proxy, BASIC_AUTH, realm, url, basic);
      } else {
	char * url = HTAnchor_address((HTAnchor*)HTRequest_anchor(request));
	HTAA_updateNode(proxy, BASIC_AUTH, realm, url, basic);
	HT_FREE(url);
      }
    }

    Basic_credentials(request, basic);
    url = HTAnchor_address((HTAnchor*)HTRequest_anchor(request));
    HTAA_deleteNode(proxy, BASIC_AUTH, realm, url);
    HT_FREE(url);
    return HT_ERROR;
        
  }
  return HT_OK;
}

/* function for GET form */
HTChunk * HTGetFormAnchorToChunk (HTAssocList *formdata,
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

/* function to handle if the timestamp is newer or not */
void time_comparison(char *lm_time, struct tm *time_user) 
{               
  char * mon;
  time_t lm_sec, user_sec; 
  struct tm *time1;
  char seps[] = ", :";

  time1=(struct tm*)malloc(sizeof(struct tm));

  strtok(lm_time, seps);
  time1->tm_mday = atoi(strtok(NULL, seps));
  mon = strtok(NULL, seps);
  if (strcmp(mon, "Jan")==0) time1->tm_mon = 0;
  if (strcmp(mon, "Feb")==0) time1->tm_mon = 1;
  if (strcmp(mon, "Mar")==0) time1->tm_mon = 2;
  if (strcmp(mon, "Apr")==0) time1->tm_mon = 3;
  if (strcmp(mon, "May")==0) time1->tm_mon = 4;
  if (strcmp(mon, "Jun")==0) time1->tm_mon = 5;
  if (strcmp(mon, "Jul")==0) time1->tm_mon = 6;
  if (strcmp(mon, "Aug")==0) time1->tm_mon = 7;
  if (strcmp(mon, "Sep")==0) time1->tm_mon = 8;
  if (strcmp(mon, "Oct")==0) time1->tm_mon = 9;
  if (strcmp(mon, "Nov")==0) time1->tm_mon = 10;
  if (strcmp(mon, "Dec")==0) time1->tm_mon = 11;

  time1->tm_year = atoi(strtok(NULL, seps))-1900;
  time1->tm_hour = atoi(strtok(NULL, seps));
  time1->tm_min = atoi(strtok(NULL, seps));
  time1->tm_sec = atoi(strtok(NULL, seps));
		

  lm_sec = mktime(time1);
  /*  printf("the sec is %d\n", lm_sec); */
	
  user_sec = mktime(time_user);
  /*  printf("the sec is %d\n", user_sec); */

  if (lm_sec != -1 && user_sec != -1) {
    if (lm_sec < user_sec) load_flag = FALSE;
  }
  else 
    xsb_abort("LIBWWW_FETCH_URL: invalid Last-Modified value\n");
}
