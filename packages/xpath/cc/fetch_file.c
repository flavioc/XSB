/*****************************************************************************
 *                        fetch_file.c
 * This file contains functions which are used to download remote files. If 
 * the input is a url, then the functions defined below are used to parse the
 * url and download the remote file.
 *
 ****************************************************************************/

#include "xsb_config.h"
#include "socketcall.h"
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>


#define SA      struct sockaddr

#define MAXSTRLEN 256

int parse_url( const char * url, char * server, char *fname);
int get_file_www(char *server, char * fname, char **buf);


/**
 * Function invoked if the input is a url.
 * Parse the url to extract the protocol, server, file path, port
 * Input : url
 * Output : server, port, file path
 **/
int parse_url( const char * url, char * server, char *fname)
{
  int i,j;
  int len = strlen(url);
  char temp[MAXSTRLEN];
  
  int flag = 0, flag_file = 0;
  
  for(i = 0; i<MAXSTRLEN; i++)
    {
      *(server+i) = 0;
      *(fname+i) = 0;
      *(temp+i) = 0;
    }
  
  
  for( i=0;i<len;i++)
    {
      *(temp+i)=url[i];
      
      if( url[i] == ':')
	{
	  
	  flag = 1;
	  i++;
	  /*If the protocol is not file:// or http:// its an error*/
	  if(strcmp( temp , "http:") && strcmp( temp, "file:"))
	    return FALSE;
	  
	  if(!strcmp( temp, "http:")){
	    flag_file = 1;
	  }
	  else if(!strcmp( temp, "file:")){
	    flag_file = 2;
	  }
	  
	  if( url[i] == '/' && url[i+1] == '/')
	    {
	      *(temp+i) = url[i];
	      i = i+ 1;		
	      *(temp+i) = url[i];
	      i = i+1;
	      break;
	    }
	  else
	    return FALSE;
	}
    }
  
  if(!flag)
    return FALSE;
  
  if( flag_file == 2){
    strcpy( server, "file");
    strcpy( fname, url+i);
    return TRUE;
  }
  
  /*Extract the server*/
  for(j=0;i<len;i++,j++)
    {
      if(url[i] == '/')
	break;
      
      *(server+j) = url[i];
    }
  /*Extract the filename*/
  for(j=0;i<len;i++,j++)
    {
      *(fname+j) = url[i];
    }
  return TRUE;
}

/**
 * Download the file from the specified url. Invoked only if the source is a 
 * url
 * Input :  server, port, file name
 * Output : The downloaded file
 **/
int get_file_www(char *server, char *fname, char **source)
{
  int sockfd;
  struct sockaddr_in	servaddr;
  struct in_addr		**pptr;
  struct hostent		*hp;
  int port = 80, len, i = 0, maxlen = 0;
  char *tempstr = NULL;

	
  /* Special socket handling for windows */
#ifdef WIN_NT
  int rc;
	
  WSADATA wsadata;
	
  rc = WSAStartup(2, &wsadata);
  *source = (char*)malloc(MAXSTRLEN);	
  if(rc) {
    sprintf( *source, "WSAStartup FAILED: err=%d\n", (int) GetLastError());
    return FALSE;
  }
#endif
	
  if(*source == NULL)
    {
      *source = (char*)malloc(MAXSTRLEN);
    }
  len = strlen( server);
	
  for( i = 0; i < len && server[i] != ':'; i++);
	
  if (server[i] == ':')
    {
      server[i] = 0;
      port = atoi(server + i + 1);
    }

  /* Resolve the hostname */
  if ( (hp = gethostbyname(server)) == NULL)
    {
      sprintf(*source, "hostname error for %s", server);
      return FALSE;
    }
	
  pptr = (struct in_addr **) hp->h_addr_list;
	
  /*Open the sicket connection*/	
  for ( ; *pptr != NULL; pptr++) {
	  
    sockfd = socket(AF_INET, SOCK_STREAM, 0);
	  
	  
    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_port = htons((unsigned short)port); 
    memcpy(&servaddr.sin_addr, *pptr, sizeof(struct in_addr));
    if (connect(sockfd, (SA *) &servaddr, sizeof(servaddr)) == 0)
      break;		
#ifdef WIN_NT
    closesocket(sockfd);
#else
    close(sockfd);
#endif
    return FALSE;
  }
  if (*pptr == NULL)
    {
      return FALSE;
    }
	
  len = 0;
	

  /*Issue the http get filename command*/
	
  sprintf( *source, "GET %s\n\n", fname);
  len = strlen( *source);
  (*source)[ len] = '\0';
  send( sockfd, *source, strlen(*source), 0);
		    
  i=0;
  maxlen = 0;
  /*Download the file*/
  while( (len != 0)  && ( len != -1)){
    *source = (char*) realloc( *source, ((i+1) * MAXSTRLEN) + 1);
    tempstr = (*source) + maxlen;
    len = recv( sockfd, tempstr , MAXSTRLEN, 0);
	  
    if( (len != 0) && (len != -1) ){
      maxlen+=len;
      (*source)[maxlen]='\0';
    }
    i++;
  }
		
  /* Handle http errors */

  if( strstr( *source, "Error 400")!= NULL){
    strcpy( *source, "400 Bad Request");
    return FALSE;
  }
	
  if( strstr( *source, "Error 401")!= NULL){
    strcpy( *source, "401 Unauthorized");
    return FALSE;
  }
	
  if( strstr( *source, "Error 402")!= NULL){
    strcpy( *source, "402 Payment Required");
    return FALSE;
  }
	
  if( strstr( *source, "Error 403")!= NULL){
    strcpy( *source, "403 Forbidden");
    return FALSE;
  }
	
  if( strstr( *source, "Error 404")!= NULL){
	  
    strcpy( *source, "Error 404 File not found");
    return FALSE;
  }
	
  if( strstr( *source, "Error 405")!= NULL){
    strcpy( *source, "405 Method Not Allowed");
    return FALSE;
  }
	
  if( strstr( *source, "Error 406")!= NULL){
    strcpy( *source, "406 Not Acceptable");
    return FALSE;
  }

  if( strstr( *source, "Error 407")!= NULL){
    strcpy( *source, "407 Proxy Authentication Required");
    return FALSE;
  }
	
	
  if( strstr( *source, "Error 408")!= NULL){
    strcpy( *source, "408 Request Timeout");
    return FALSE;
  }


  if( strstr( *source, "Error 409")!= NULL){
    strcpy( *source, "409 Conflict");
    return FALSE;
  }

  if( strstr( *source, "Error 410")!= NULL){
    strcpy( *source, "410 Gone");
    return FALSE;
  }

  if( strstr( *source, "Error 411")!= NULL){
    strcpy( *source, "411 Length Required"); ;
    return FALSE;
  }
  if( strstr( *source, "Error 412")!= NULL){
    strcpy( *source, "412 Precondition Failed");
    return FALSE;
  }

  if( strstr( *source, "Error 413")!= NULL){
    strcpy( *source, "413 Request Entity Too Large");
    return FALSE;
  }

  if( strstr( *source, "Error 414")!= NULL){
    strcpy( *source, "414 Request-URI Too Longe") ;
    return FALSE;
  }

  if( strstr( *source, "Error 415")!= NULL){
    strcpy( *source, "415 Unsupported Media Type");
    return FALSE;
  }
	
  if( strstr( *source, "Error 416")!= NULL){
    strcpy( *source, "416 Requested Range Not Satisfiable");
    return FALSE;
  }

  if( strstr( *source, "Error 417")!= NULL){
    strcpy( *source,  "417 Expectation Failed");
    return FALSE;
  }

  if( strstr( *source, "Error 500")!= NULL){
    strcpy( *source, "500 Internal Server Error");
    return FALSE;
  }
	 
  if( strstr( *source, "Error 501")!= NULL){
    strcpy( *source, "501 Not Implemented");
    return FALSE;
  }
  if( strstr( *source, "Error 502")!= NULL){
    strcpy( *source, "502 Bad Gateway");
    return FALSE;
  }

  if( strstr( *source, "Error 503")!= NULL){
    strcpy( *source, "503 Service Unavailable") ;
    return FALSE;
  }

  if( strstr( *source, "Error 504")!= NULL){
    strcpy( *source, "504 Gateway Timeout");
    return FALSE;
  }

  if( strstr( *source, "Error 505")!= NULL){
    strcpy( *source, "505 HTTP Version Not Supported");
    return FALSE;
  }

#ifdef WIN_NT
  closesocket(sockfd);
#else
  close(sockfd);
#endif

  return TRUE;
}
