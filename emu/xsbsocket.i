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
#endif

#ifndef WIN_NT
#include <sys/socket.h>
#include <sys/uio.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif /* WIN_NT */

#include "xsbsocket.h"

int readmsg(SOCKET sockfd, char *buff, int maxbuff)
{
  int n, rc;
  char c;

  for (n=1; n < maxbuff; n++) {
    rc = recv(sockfd, &c, 1, 0);
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
    
    sockfd = socket(domain, SOCK_STREAM, IPPROTO_TCP);
    if (BAD_SOCKET(sockfd)) {
      xsb_warn("SOCKET_REQUEST: Cannot open stream socket");
      return FALSE;
    }
    ctop_int(3, (SOCKET) sockfd);
    break;
  case SOCKET_BIND: /* socket_request(1,+domain,+sockfd,+port) */
    /* jf: for now only support AF_INET, ignore param */
    /* domain = ptoc_int(2); */
    sockfd = (SOCKET) ptoc_int(3);
    portnum = ptoc_int(4);
    
    /* Bind server to the agreed upon port number.
    ** See commdef.h for the actual port number. */
    FillWithZeros(socket_addr);
    socket_addr.sin_port = htons(portnum);
    socket_addr.sin_family = AF_INET;
#ifndef WIN_NT
    socket_addr.sin_addr.s_addr = htonl(INADDR_ANY);
#endif
    
    retcode = bind(sockfd, (PSOCKADDR) &socket_addr, sizeof(socket_addr));
    if (SOCKET_OP_FAILED(retcode)) {
      xsb_warn("SOCKET_BIND: Connection failed");
      return FALSE;
    }
    break;
  case SOCKET_LISTEN:  /* socket_request(2,+sockfd,+length) */
    sockfd = (SOCKET) ptoc_int(2);
    retcode = listen(sockfd, ptoc_int(3));
    if (SOCKET_OP_FAILED(retcode)) {
      xsb_warn("SOCKET_LISTEN: Connection failed");
      return FALSE;
    }
    break;
  case SOCKET_ACCEPT: { /* socket_request(3,+sockfd,-sockfptr) */
    /* returns Prolog stream number: an index into xsb openfile table */
    sockfd_in = (SOCKET) ptoc_int(2);
    
    sockfd = accept(sockfd_in, NULL, NULL);
    if (BAD_SOCKET(sockfd)) {
      xsb_warn("SOCKET_ACCEPT: Connection failed");
      return FALSE;
    }
    
    /* find empty slot in XSB openfile table and put sockptr there */
    i = xsb_intern_file(sockptr, "SOCKET_ACCEPT");
#ifdef WIN_NT
    sockptr = NULL;
#else
    if ((sockptr = fdopen(sockfd, "r+")) == NULL) {
      sockptr = NULL;
      xsb_warn("SOCKET_ACCEPT: fdopen failed");
      return FALSE;
    }
#endif
    ctop_int(3, i);
    break;
  }
  
  case SOCKET_CONNECT:
    /* socket_request(4,+domain,+sockfd,+port,+hostname,-sockfptr) 
     * jf: domain is ignored for now
     */
    /* Returns Prolog stream number: index into XSB openfile table */
    sockfd = (SOCKET) ptoc_int(3);
    portnum = ptoc_int(4);
    
    /*** prepare to connect ***/
    FillWithZeros(socket_addr);
    socket_addr.sin_port = htons(portnum);
    socket_addr.sin_family = AF_INET;
    socket_addr.sin_addr.s_addr = inet_addr(ptoc_string(5));
    
#ifdef WIN_NT
    retcode = connect(sockfd, (PSOCKADDR) &socket_addr, sizeof(socket_addr));
#else
    /* Why retry in Unix and not NT?? */
    while(((retcode =
	    connect(sockfd, (PSOCKADDR)&socket_addr, sizeof(socket_addr)))
	   == -1)
	  && (errno == EINTR) );
#endif
    if (SOCKET_OP_FAILED(retcode)) {
      xsb_warn("SOCKET_CONNECT: connect failed");
      closesocket(sockfd);
      return FALSE;
    }
    
    /* find empty slot in XSB openfile table and put fptr there */
    i = xsb_intern_file(sockptr,"SOCKET_CONNECT");
#ifdef WIN_NT
    sockptr = NULL;
#else
    if ((sockptr = fdopen(sockfd, "r+")) == NULL) {
      xsb_warn("SOCKET_CONNECT: fdopen failed");
      sockptr = NULL;
      return FALSE;
    }
#endif
    ctop_int(6, i);
    break;
  case SOCKET_CLOSE:	/* socket_request(6,+sockfd) */
    closesocket((SOCKET) ptoc_int(2));
    break;
  case SOCKET_RECV:
    sockfd = (SOCKET) ptoc_int(2);
    sock_msg = calloc(1024, sizeof(char));
    rc = readmsg(sockfd, sock_msg,1024);
    ctop_string(3, (char*) string_find((char*) sock_msg,1));
    free(sock_msg);
    break;
  case SOCKET_SEND:
    sockfd = (SOCKET) ptoc_int(2);
    sock_msg = calloc(1024, sizeof(char));
    strcpy((char*) sock_msg, (char*) ptoc_string(3));
    send(sockfd, sock_msg, strlen(sock_msg), 0);
    send(sockfd, "`", strlen("`"), 0);
    /*send(sockfd, "\n", strlen("\n"),0),*/
    free(sock_msg);
    break;
  case SOCKET_SEND_ASCI:
    sockfd = (SOCKET) ptoc_int(2);
    rc = ptoc_int(3);
    sock_msg = calloc(1024, sizeof(char));
    ci = (char) rc;
    sprintf(sock_msg,"%c",ci);
    /*printf("XSB2: str:%s.\n", sock_msg);*/
    send(sockfd, sock_msg, strlen(sock_msg), 0);
    send(sockfd, "`", strlen("`"), 0);
    /*send(sockfd, "\n", strlen("\n"),0);*/
    free(sock_msg);
    break;
  case SOCKET_SEND_EOF:
    sockfd = (SOCKET) ptoc_int(2);
    last[0] = EOF;
    send(sockfd, last, 1, 0);
    send(sockfd, "`", strlen("`"),0);
    break;
  case SOCKET_GET0: /* socket_request(11,+Sockfd,-C,-Error,_,_) */
    sockfd = (SOCKET) ptoc_int(2);
    /*JPS: rc = readmsg(socketfd, &ch, 1);*/
    rc = recv (sockfd,&ch,1,0);
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
    sockfd = (SOCKET) ptoc_int(2);
    /* JPS: sprintf(tmpch,"%c",ptoc_int(3));*/
    tmpch[0] = (char)ptoc_int(3);
    send(sockfd, tmpch, 1, 0);
    break;
  }
  
  default:
    xsb_warn("SOCKET_REQUEST: Invalid socket request %d", (int) ptoc_int(1));
    return FALSE;
  }
  return TRUE;
}
