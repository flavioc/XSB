#ifndef _UTIL_H
#define _UTIL_H

#define BASIC_AUTH "basic"

typedef struct _HTBasic {		  /* Basic challenge and credentials */
  char  *uid;
  char  *pw;
  BOOL   proxy;				  /* Proxy authentication */
} HTBasic;

HTBasic *HTBasic_new();
BOOL     Basic_credentials (HTRequest *request, HTBasic *basic);
int      Basic_generate (HTRequest *request, void *context, int mode);

#endif 

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif