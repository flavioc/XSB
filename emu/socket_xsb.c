/* File:      socket_xsb.c
** Author(s): juliana, davulcu, kifer
** Contact:   xsb-contact@cs.sunysb.edu
**
** Copyright (C) The Research Foundation of SUNY, 1999
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

#include "configs/config.h"
#include "debugs/debug.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
/* special.h must be included after sys/stat.h */
#include "configs/special.h"


/* The socket material */

#ifdef WIN_NT
#include <windows.h>
#include <tchar.h>
#include <io.h>
#include <stdarg.h>
#include <winsock.h>
#include <wsipx.h>
#else /* UNIX */
#include <sys/socket.h>
#include <sys/uio.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h> 
#endif 

#include "auxlry.h"
#include "cell.h"
#include "error_xsb.h"
#include "cinterf.h"

#include "io_builtins_xsb.h"
#include "socket_xsb.h"


/* In WIN_NT, this gets redefined into _fdopen by configs/special.h */
extern FILE *fdopen(int fildes, const char *type);

int retcode; /* return code from socket operation */

SOCKADDR_IN socket_addr;
FILE *sockptr;
int rc, domain, portnum;
char ch;
SOCKET sock_handle, sock_handle_in;
int sockfd;
char *sock_msg, ci, last[1];

struct linger sock_linger_opt;

static char *get_host_IP(char *host_name_or_IP) {
  struct hostent *host_struct;
  struct in_addr *ptr;

  char **listptr;

  /* if host_name_or_IP is an IP addr, then just return; 
     else use gethostbyname */
  if (IS_IP_ADDR(host_name_or_IP))
    return(host_name_or_IP);
  host_struct = gethostbyname(host_name_or_IP);
  
  listptr = host_struct->h_addr_list;

  if ((ptr = (struct in_addr *) *listptr++) != NULL) {
	  xsb_mesg(" Int. address: %s", inet_ntoa(*ptr));
	  return(inet_ntoa(*ptr));
  }
  return NULL;
}


int readmsg(SOCKET sock_handle, char *buff, int maxbuff)
{
  int n, rc;
  char c;

  for (n=1; n < maxbuff; n++) {
    rc = recv(sock_handle, &c, 1, 0);
    if (rc == 1) {
	        
      if (c == '`') {
	break;
      } else if (c == EOF) {
	return (-2);
      } else {
	*buff++=c;
      }
    } else if (rc == 0) {
      if (n == 1) {
	return(0);    
      } else {
	break;         
      }
    } else  {
      return (-1); 
    }
  }
  *buff = 0;
  return (n);
}


/* in order to save builtin numbers, create a single socket function with
 * options socket_request(SockOperation,....)  */
bool xsb_socket_request(void)
{
  int i;

  switch (ptoc_int(1)) {
  case SOCKET_ROOT:
    /* socket_request(SOCKET_ROOT,+domain,-socket_fd,-Error,_,_,_) 
       Currently only AF_INET domain */
    domain = ptoc_int(2); 
    if (domain == 0) domain = AF_INET;
    else if (domain == 1){
      domain = AF_UNIX;
      xsb_abort("SOCKET_REQUEST: domain AF_INET is not implemented");
    }
    else  {
      xsb_abort("SOCKET_REQUEST: Invalid domain. Valid domains are: 0 - AF_INET, 1 - AF_UNIX");           
      return FALSE;
    }
    
    sock_handle = socket(domain, SOCK_STREAM, IPPROTO_TCP);
    if (BAD_SOCKET(sock_handle)) {
      xsb_warn("SOCKET_REQUEST: Cannot open stream socket");
      return FALSE;
    }

    ctop_int(3, (SOCKET) sock_handle);
    break;
  case SOCKET_BIND:
    /* socket_request(SOCKET_BIND,+domain,+sock_handle,+port,-Error,_,_) 
       Currently only supports AF_INET */
    sock_handle = (SOCKET) ptoc_int(3);
    portnum = ptoc_int(4);

    domain = ptoc_int(2);
    if (domain == 0) domain = AF_INET;
    else if (domain == 1){
      domain = AF_UNIX;
      xsb_abort("SOCKET_REQUEST: domain AF_INET is not implemented");
    }
    else  {
      xsb_abort("SOCKET_REQUEST: Invalid domain. Valid domains are: 0 - AF_INET, 1 - AF_UNIX");           
      return FALSE;
    }
    
    /* Bind server to the agreed upon port number.
    ** See commdef.h for the actual port number. */
    FillWithZeros(socket_addr);
    socket_addr.sin_port = htons(portnum);
    socket_addr.sin_family = AF_INET;
#ifndef WIN_NT
    socket_addr.sin_addr.s_addr = htonl(INADDR_ANY);
#endif
    
    retcode = bind(sock_handle, (PSOCKADDR) &socket_addr, sizeof(socket_addr));
    if (SOCKET_OP_FAILED(retcode)) {
      xsb_warn("SOCKET_BIND: Connection failed");
      return FALSE;
    }
    break;
  case SOCKET_LISTEN: 
    /* socket_request(SOCKET_LISTEN,+sock_handle,+length,-Error,_,_,_) */
    sock_handle = (SOCKET) ptoc_int(2);
    retcode = listen(sock_handle, ptoc_int(3));
    if (SOCKET_OP_FAILED(retcode)) {
      xsb_warn("SOCKET_LISTEN: Connection failed");
      return FALSE;
    }
    break;
  case SOCKET_ACCEPT: {
    /* socket_request(SOCKET_ACCEPT,+sock_handle,-sockfptr, -Error,_,_,_) 
       returns Prolog stream number: an index into xsb openfile table */
    sock_handle_in = (SOCKET) ptoc_int(2);
    
    sock_handle = accept(sock_handle_in, NULL, NULL);
    if (BAD_SOCKET(sock_handle)) {
      xsb_warn("SOCKET_ACCEPT: Connection failed");
      return FALSE;
    }
    
#ifdef WIN_NT
    sockfd = _open_osfhandle(sock_handle, 0);
    if (sockfd == -1) {
      xsb_warn("SOCKET_ACCEPT: open_osfhandle failed");
      return FALSE;
    }
#else
    sockfd = sock_handle;
#endif
    if ((sockptr = fdopen(sockfd, "r+")) == NULL) {
      sockptr = NULL;
      xsb_warn("SOCKET_ACCEPT: fdopen failed");
      return FALSE;
    }
    /* find empty slot in XSB openfile table and put sockptr there */
    i = xsb_intern_file(sockptr, "SOCKET_ACCEPT");
    ctop_int(3, i);
    break;
  }
  
  case SOCKET_CONNECT:
    /* socket_request(SOCKET_CONNECT,+domain,+sock_handle,+port,
       	       	       +hostname,-sockfptr,-Error)
       Returns Prolog stream number: index into XSB openfile table */
    sock_handle = (SOCKET) ptoc_int(3);
    portnum = ptoc_int(4);

    domain = ptoc_int(2);
    if (domain == 0) domain = AF_INET;
    else if (domain == 1){
      domain = AF_UNIX;
      xsb_abort("SOCKET_REQUEST: domain AF_INET is not implemented");
    }
    else  {
      xsb_abort("SOCKET_REQUEST: Invalid domain. Valid domains are: 0 - AF_INET, 1 - AF_UNIX");           
      return FALSE;
    }
    
    /*** prepare to connect ***/
    FillWithZeros(socket_addr);
    socket_addr.sin_port = htons(portnum);
    socket_addr.sin_family = AF_INET;
    socket_addr.sin_addr.s_addr = inet_addr(get_host_IP(ptoc_string(5)));
    
#ifdef WIN_NT
    retcode = connect(sock_handle, (PSOCKADDR) &socket_addr, sizeof(socket_addr));
#else
    /* Why retry in Unix and not NT?? */
    while(((retcode =
	    connect(sock_handle, (PSOCKADDR)&socket_addr, sizeof(socket_addr)))
	   == -1)
	  && (errno == EINTR) );
#endif
    if (SOCKET_OP_FAILED(retcode)) {
      xsb_warn("SOCKET_CONNECT: connect failed");
      closesocket(sock_handle);
      return FALSE;
    }
    
#ifdef WIN_NT
    sockfd = _open_osfhandle(sock_handle, 0);
    if (sockfd == -1) {
      xsb_warn("SOCKET_ACCEPT: open_osfhandle failed");
      return FALSE;
    }
#else
    sockfd = sock_handle;
#endif
    if ((sockptr = fdopen(sockfd, "r+")) == NULL) {
      xsb_warn("SOCKET_CONNECT: fdopen failed");
      sockptr = NULL;
      return FALSE;
    }
    /* find empty slot in XSB openfile table and put fptr there */
    i = xsb_intern_file(sockptr,"SOCKET_CONNECT");
    ctop_int(6, i);
    break;
  case SOCKET_CLOSE:	
    /* socket_request(SOCKET_CLOSE,+sock_handle,-Error,_,_,_,_) */
    closesocket((SOCKET) ptoc_int(2));
    break;
  case SOCKET_RECV:    
    /* socket_request(SOCKET_RECV,+Sockfd, -Msg, -Error,_,_,_) */
    sock_handle = (SOCKET) ptoc_int(2);
    sock_msg = calloc(1024, sizeof(char));
    rc = readmsg(sock_handle, sock_msg,1024);
    ctop_string(3, (char*) string_find((char*) sock_msg,1));
    free(sock_msg);
    break;
  case SOCKET_SEND:   
    /* socket_request(SOCKET_SEND,+Sockfd, -Msg, -Error,_,_,_) */
    sock_handle = (SOCKET) ptoc_int(2);
    sock_msg = calloc(1024, sizeof(char));
    strcpy((char*) sock_msg, (char*) ptoc_string(3));
    send(sock_handle, sock_msg, strlen(sock_msg), 0);
    send(sock_handle, "`", strlen("`"), 0);
    /*send(sock_handle, "\n", strlen("\n"),0),*/
    free(sock_msg);
    break;
  case SOCKET_SEND_ASCI: 
    /* socket_request(SOCKET_SEND_ASCI,+Sockfd,-Msg,-Error,_,_,_) */
    sock_handle = (SOCKET) ptoc_int(2);
    rc = ptoc_int(3);
    sock_msg = calloc(1024, sizeof(char));
    ci = (char) rc;
    sprintf(sock_msg,"%c",ci);
    /*printf("XSB2: str:%s.\n", sock_msg);*/
    send(sock_handle, sock_msg, strlen(sock_msg), 0);
    send(sock_handle, "`", strlen("`"), 0);
    /*send(sock_handle, "\n", strlen("\n"),0);*/
    free(sock_msg);
    break;
  case SOCKET_SEND_EOF:   
    /* SOCKET_SEND_EOF(SOCKET_SEND_EOF,+Sockfd,-Error,_,_,_,_) */
    sock_handle = (SOCKET) ptoc_int(2);
    last[0] = EOF;
    send(sock_handle, last, 1, 0);
    send(sock_handle, "`", strlen("`"),0);
    break;
  case SOCKET_GET0:
    /* socket_request(SOCKET_GET0,+Sockfd,-C,-Error,_,_,_) */
    sock_handle = (SOCKET) ptoc_int(2);
    rc = recv (sock_handle,&ch,1,0);
    if (rc == 1)
      ctop_int(3,(unsigned char)ch);
    else {
      ctop_int(3,-1);
      ctop_int(4,WSAGetLastError());
    }
    break;
  case SOCKET_PUT: {
    /* socket_request(SOCKET_PUT,+Sockfd,+C,-Error_,_,_) */
    static char tmpch[4];
    sock_handle = (SOCKET) ptoc_int(2);
    tmpch[0] = (char)ptoc_int(3);
    send(sock_handle, tmpch, 1, 0);
    break;
  }
  case SOCKET_SET_OPTION: {
    /* socket_request(SOCKET_SET_OPTION,+Sockfd,+OptionName,+Value,_,_,_) */

    char *option_name;
    /* Set the "linger" parameter to a small number of seconds */
    sock_handle = (SOCKET) ptoc_int(2);
    option_name = ptoc_string(3);

    if (0==strcmp(option_name,"linger")) {
      int  linger_time=ptoc_int(4);

      if (linger_time < 0) {
	sock_linger_opt.l_onoff = FALSE;
	sock_linger_opt.l_linger = 0;
      } else {
	sock_linger_opt.l_onoff = TRUE;
	sock_linger_opt.l_linger = linger_time;
      }

      if (setsockopt(sock_handle, SOL_SOCKET, SO_LINGER,
		     (void *) &sock_linger_opt, sizeof(sock_linger_opt))
	  < 0) {
	xsb_warn("SOCKET_SET_OPTION: Cannot set socket linger time");
	return FALSE;
      }
    } else {
      xsb_warn("SOCKET_SET_OPTION: Invalid option, `%s'", option_name);
      return FALSE;
    }
    
    return TRUE;
  }
  
  default:
    xsb_warn("SOCKET_REQUEST: Invalid socket request %d", (int) ptoc_int(1));
    return FALSE;
  }
  return TRUE;
}
