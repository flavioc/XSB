/*  -*-c-*-  Make sure this file comes up in the C mode of emacs */ 
/* File:      xsbsocket.i
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


/* The socket material */

#ifdef WIN_NT
#include <windows.h>
#include <tchar.h>
#include <io.h>
#endif

#ifndef WIN_NT
#include <sys/socket.h>
#include <sys/uio.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif /* WIN_NT */

#include "xsbsocket.h"

struct linger sock_linger_opt;

char *get_host_IP(char *host_name_or_IP) {
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
	  fprintf(stderr," Int. address: %s \n", inet_ntoa(*ptr));
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
inline static bool xsb_socket_request(void)
{
  int i;

  switch (ptoc_int(1)) {
  case SOCKET_ROOT: /* socket_request(0,+domain,-socket_fd) */
    /* jf: for now only support AF_INET */
    domain = ptoc_int(2); 
    if (domain == 0) domain = AF_INET;
    else if (domain == 1){
      /* domain = AF_UNIX; */
      domain = AF_INET;
      xsb_warn("SOCKET_REQUEST: default domain is AF_INET");
    }
    else  {
      xsb_warn("SOCKET_REQUEST: Invalid domain. Valid domains are: 0 - AF_INET, 1 - AF_UNIX");           
      return FALSE;
    }
    
    sock_handle = socket(domain, SOCK_STREAM, IPPROTO_TCP);
    if (BAD_SOCKET(sock_handle)) {
      xsb_warn("SOCKET_REQUEST: Cannot open stream socket");
      return FALSE;
    }

    /* Set the "linger" parameter to a small number of seconds */
    sock_linger_opt.l_onoff = TRUE;
    sock_linger_opt.l_linger = 5;
    if (setsockopt(sock_handle, SOL_SOCKET, SO_LINGER,
		   &sock_linger_opt, sizeof(sock_linger_opt))
	< 0) {
      xsb_warn("SOCKET_REQUEST: Cannot set socket linger option");
      return FALSE;
    };

    ctop_int(3, (SOCKET) sock_handle);
    break;
  case SOCKET_BIND: /* socket_request(1,+domain,+sock_handle,+port) */
    /* jf: for now only support AF_INET, ignore param */
    /* domain = ptoc_int(2); */
    sock_handle = (SOCKET) ptoc_int(3);
    portnum = ptoc_int(4);
    
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
  case SOCKET_LISTEN:  /* socket_request(2,+sock_handle,+length) */
    sock_handle = (SOCKET) ptoc_int(2);
    retcode = listen(sock_handle, ptoc_int(3));
    if (SOCKET_OP_FAILED(retcode)) {
      xsb_warn("SOCKET_LISTEN: Connection failed");
      return FALSE;
    }
    break;
  case SOCKET_ACCEPT: { /* socket_request(3,+sock_handle,-sockfptr) */
    /* returns Prolog stream number: an index into xsb openfile table */
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
    /* socket_request(4,+domain,+sock_handle,+port,+hostname,-sockfptr) 
     * jf: domain is ignored for now
     */
    /* Returns Prolog stream number: index into XSB openfile table */
    sock_handle = (SOCKET) ptoc_int(3);
    portnum = ptoc_int(4);
    
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
  case SOCKET_CLOSE:	/* socket_request(6,+sock_handle) */
    closesocket((SOCKET) ptoc_int(2));
    break;
  case SOCKET_RECV:
    sock_handle = (SOCKET) ptoc_int(2);
    sock_msg = calloc(1024, sizeof(char));
    rc = readmsg(sock_handle, sock_msg,1024);
    ctop_string(3, (char*) string_find((char*) sock_msg,1));
    free(sock_msg);
    break;
  case SOCKET_SEND:
    sock_handle = (SOCKET) ptoc_int(2);
    sock_msg = calloc(1024, sizeof(char));
    strcpy((char*) sock_msg, (char*) ptoc_string(3));
    send(sock_handle, sock_msg, strlen(sock_msg), 0);
    send(sock_handle, "`", strlen("`"), 0);
    /*send(sock_handle, "\n", strlen("\n"),0),*/
    free(sock_msg);
    break;
  case SOCKET_SEND_ASCI:
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
    sock_handle = (SOCKET) ptoc_int(2);
    last[0] = EOF;
    send(sock_handle, last, 1, 0);
    send(sock_handle, "`", strlen("`"),0);
    break;
  case SOCKET_GET0: /* socket_request(11,+Sockfd,-C,-Error,_,_) */
    sock_handle = (SOCKET) ptoc_int(2);
    /*JPS: rc = readmsg(socketfd, &ch, 1);*/
    rc = recv (sock_handle,&ch,1,0);
    if (rc == 1)
      ctop_int(3,(unsigned char)ch);
    else {
      ctop_int(3,-1);
      ctop_int(4,WSAGetLastError());
    }
    break;
  case SOCKET_PUT: { /* socket_request(12,+Sockfd,+C,_,_,_) */
    /* We should fail on error...*/
    static char tmpch[4];
    sock_handle = (SOCKET) ptoc_int(2);
    /* JPS: sprintf(tmpch,"%c",ptoc_int(3));*/
    tmpch[0] = (char)ptoc_int(3);
    send(sock_handle, tmpch, 1, 0);
    break;
  }
  
  default:
    xsb_warn("SOCKET_REQUEST: Invalid socket request %d", (int) ptoc_int(1));
    return FALSE;
  }
  return TRUE;
}
