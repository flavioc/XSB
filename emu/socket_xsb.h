/* File:      socket_xsb.h
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


#include "socket_defs_xsb.h"

#ifdef WIN_NT
#define BAD_SOCKET(sockfd)         sockfd==INVALID_SOCKET
#define SOCKET_OP_FAILED(sockfd)   sockfd==SOCKET_ERROR
#define IS_IP_ADDR(string)    	   inet_addr(string) != INADDR_NONE
#define XSB_SOCKET_ERRORCODE	   WSAGetLastError()
#else
#define SOCKET 	        int
#define SOCKADDR_IN 	struct sockaddr_in /* in windows, but not Unix */
#define PSOCKADDR       struct sockaddr *  /* in windows, but not Unix */
#define closesocket    	       	  close
#define XSB_SOCKET_ERRORCODE   	  errno
#define BAD_SOCKET(sockfd)        sockfd<0
#define SOCKET_OP_FAILED(sockfd)  sockfd<0
#define IS_IP_ADDR(string)    	  inet_addr(string) != -1
#endif

#ifdef WIN_NT
#define FillWithZeros(addr)    	  ZeroMemory(&addr, sizeof(addr));
#else
#define FillWithZeros(addr)    	  memset((char *)&addr, (int) 0, sizeof(addr));
#endif

#define MAXCONNECT 50	     /* max number of connections per socket_select */

/* the length of the XSB header that contains the info on the size of the
   subsequent message. Used by socket_send/socket_recv */
#define XSB_MSG_HEADER_LENGTH  sizeof(int)

/* These are used only by the readmsg function */
#define SOCK_READMSG_FAILED  -1      /* failed socket call     	      	    */
#define SOCK_READMSG_EOF     -2	     /* when EOF is reached    	       	    */
