/* File:      socket_xsb.c
** Author(s): juliana, davulcu, kifer, songmei yu
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

#include "xsb_config.h"
#include "xsb_debug.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>

/* wind2unix.h must be included after sys/stat.h */
#include "wind2unix.h"

/* The socket material */
#ifdef WIN_NT
#include <windows.h>
#include <winuser.h>
#include <winbase.h>
#include <process.h>
#include <tchar.h>
#include <io.h>
#include <stdarg.h>
#include <winsock.h>
#include <wsipx.h>
#else /* UNIX */
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h> 
#endif 

#include "auxlry.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "cinterf.h"
#include "basictypes.h"

#include "io_builtins_xsb.h"
#include "socket_xsb.h"
#include "timer_xsb.h"

/* return error code handling */
static xsbBool set_error_code(int ErrCode, int ErrCodeArgNumber,char *Where);

/* define a macro to do memory free and errro handling for timeout related 
   socket calls */
#define FREE_TIMEOUT_AND_SET_ECODE(ecode, arg_num, Where) \
		free_timeout_obj(pSock); \
                return set_error_code(ecode, arg_num, Where); 

/* In WIN_NT, this gets redefined into _fdopen by configs/special.h */
extern FILE *fdopen(int fildes, const char *type);

/* return code from socket operation without timeout control */
int retcode;

/* define the timeout structure for the socket calls that need
   timeout control */ 
struct xsb_timeout {
  long parent_thread;  /* This field is mandatory; must be first! */
  int  return_code;    /* return value from the socket call */
  void *sockdata;      /* used to pass socket specific data */
};

/* free xsbTimeout object */
static void free_timeout_obj(xsbTimeout * pSock);
static xsbTimeout *make_timeout_obj();

/* declare the utility functions for select calls */
static void init_connections();
static xsbBool list_sockfd(prolog_term list, fd_set *fdset, int *max_fd,
			   int **fds, int * size);
static void test_ready(prolog_term *avail_sockfds, fd_set *fdset,
		       int *fds, int size);
static void select_destroy(char *connection_name);
static int getsize (prolog_term list);
static int checkslot (void);

/* define the structure for select call */
static struct connection_t {
  char *connection_name;
  int maximum_fd;
  int empty_flag;
  int sizer;	/* size of read fds */
  int sizew;	/* size of write fds */
  int sizee;	/* size of exception fds */
  fd_set readset;	
  fd_set writeset;
  fd_set exceptionset;
  int *read_fds;	/* array of read fds */ 
  int *write_fds;	/* array of write fds */
  int *exception_fds; 	/* array of exception fds */
} connections[MAXCONNECT];

static char *get_host_IP(char *host_name_or_IP) {
  struct hostent *host_struct;
  struct in_addr *ptr;
  char **listptr;
 
  /* if host_name_or_IP is an IP addr, then just return; else use 
     gethostbyname */
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


/* Returns:
   normal:  SOCK_OK
   EOF:     SOCK_READMSG_EOF
   error:   SOCK_READMSG_FAILED
   Read message header, then read the message itself.
*/
static int readmsg(SOCKET sock_handle, char **msg_buff)	
{
  int actual_len;
  /* 4-char buf that keeps the length of the subsequent msg */
  char lenbuf[XSB_MSG_HEADER_LENGTH];
  unsigned int msglen, net_encoded_len;

  actual_len
    = (long)recvfrom(sock_handle,lenbuf,XSB_MSG_HEADER_LENGTH,0,NULL,0);

  if (SOCKET_OP_FAILED(actual_len)) return SOCK_READMSG_FAILED;
  if (actual_len == 0) {
    *msg_buff = NULL;
    return SOCK_READMSG_EOF;
  }

  memcpy((void *) &net_encoded_len, (void *) lenbuf, XSB_MSG_HEADER_LENGTH);
  msglen = ntohl(net_encoded_len);

  if ((*msg_buff = calloc(msglen+1, sizeof(char))) == NULL) {
    xsb_abort("SOCKET_RECV: Can't allocate memory for the message buffer");
  }

  actual_len = (long) recvfrom(sock_handle,*msg_buff,msglen,0,NULL,0);
  if (SOCKET_OP_FAILED(actual_len)) return SOCK_READMSG_FAILED;

  /* The following should never arise. Points to a bug: some kind of mismatch
     between the communicating machines. */
  if ((unsigned int)actual_len != msglen)
    xsb_warn("SOCKET_RECV: msg length %ld differs from the header value %ld",
	     msglen, actual_len);

  return SOCK_OK;
}

/* socket calls which need timeout control: 
   socket_connect();
   socket_recv();
   socket_send();
   socket_get0();
   socket_put();
   The formal parameter is pointer to xsbTimeout.
*/

static void socket_connect(xsbTimeout *pptr) {
    SOCKET sock_handle;
    int domain, portnum;
    SOCKADDR_IN socket_addr;
    
    domain = ptoc_int(2);
    sock_handle = (SOCKET) ptoc_int(3);
    portnum = ptoc_int(4);
   
    if (domain == 0) domain = AF_INET;
    else if (domain == 1) {
      domain = AF_UNIX;
      xsb_abort("SOCKET_REQUEST: domain AF_INET is not implemented");
    } else  {
      xsb_abort("SOCKET_REQUEST: Invalid domain. Valid domains are: 0(AF_INET), 1(AF_UNIX)");           
    }

    /*** prepare to connect ***/
    FillWithZeros(socket_addr);
    socket_addr.sin_port = htons((unsigned short)portnum);
    socket_addr.sin_family = AF_INET;
    socket_addr.sin_addr.s_addr =
      inet_addr((char*)get_host_IP(ptoc_string(5)));

    pptr->return_code =
      connect(sock_handle,(PSOCKADDR)&socket_addr,sizeof(socket_addr));

    NOTIFY_PARENT_THREAD(pptr);
}

static void socket_recv(xsbTimeout *pptr)	{
    SOCKET sock_handle;
    
    sock_handle = (SOCKET) ptoc_int(2);
    
    pptr->return_code = readmsg(sock_handle,(char**)(&pptr->sockdata)); 

    NOTIFY_PARENT_THREAD(pptr);
}

static void socket_send(xsbTimeout *pptr) {
    SOCKET sock_handle;
    char *send_msg_aux;
    unsigned int msg_body_len, network_encoded_len;

    sock_handle = (SOCKET) ptoc_int(2);
    send_msg_aux = (char*) ptoc_string(3);
    
    /* We use the first XSB_MSG_HEADER_LENGTH bytes for the message size.*/
    if ((pptr->sockdata
	 = calloc(strlen(send_msg_aux)+XSB_MSG_HEADER_LENGTH+1,sizeof(char)))
	== NULL) {
      xsb_abort("SOCKET_SEND: Can't allocate memory for the message buffer");
    }
    msg_body_len = strlen(send_msg_aux);
    network_encoded_len = (unsigned int) htonl((unsigned long int)
					       msg_body_len); 
    memcpy((void *) (pptr->sockdata), (void *)
	   &network_encoded_len,XSB_MSG_HEADER_LENGTH);
    strcpy((char*)(pptr->sockdata)+XSB_MSG_HEADER_LENGTH, send_msg_aux);

    pptr->return_code = sendto(sock_handle,
			       (char*)(pptr->sockdata),
			       msg_body_len+XSB_MSG_HEADER_LENGTH,
			       0, NULL, 0);
    NOTIFY_PARENT_THREAD(pptr);
}

static void socket_get0(xsbTimeout * pptr) {
    SOCKET sock_handle;
    pptr->sockdata = malloc(sizeof(char));
    
    sock_handle = (SOCKET) ptoc_int(2);
    pptr->return_code =
          recvfrom(sock_handle,(char*)(pptr->sockdata),1,0,NULL,0);

    NOTIFY_PARENT_THREAD(pptr);
}
  
static void socket_put(xsbTimeout *pptr) {
    SOCKET sock_handle;
    static char tmpch;	
    
    sock_handle = (SOCKET) ptoc_int(2);
    tmpch = (char)ptoc_int(3);

    pptr->return_code = sendto(sock_handle, &tmpch, 1, 0, NULL,0);

    NOTIFY_PARENT_THREAD(pptr);
}

/* in order to save builtin numbers, create a single socket function with
 * options socket_request(SockOperation,....)  */
xsbBool xsb_socket_request(void)
{
  static int ecode = 0;  /* error code for socket ops */
  static SOCKET sock_handle, sock_handle_in;
  static int domain, portnum;
  static SOCKADDR_IN socket_addr;
  static struct linger sock_linger_opt;

  xsbTimeout *pSock;

  switch (ptoc_int(1)) {
  case SOCKET_ROOT:
    /* socket_request(SOCKET_ROOT,+domain,-socket_fd,-Error,_,_,_) 
       Currently only AF_INET domain */
    domain = ptoc_int(2); 
    if (domain == 0) domain = AF_INET;
    else if (domain == 1){
      domain = AF_UNIX;
      xsb_abort("SOCKET_REQUEST: Domain AF_INET is not implemented");
    } else  {
      xsb_abort("SOCKET_REQUEST: Invalid domain. Valid domains are: 0 - AF_INET, 1 - AF_UNIX");           
      return FALSE;
    }
    
    sock_handle = socket(domain, SOCK_STREAM, IPPROTO_TCP);
	
    /* error handling */
    if (BAD_SOCKET(sock_handle)) {
      ecode = XSB_SOCKET_ERRORCODE;
      perror("SOCKET_REQUEST");
    } else
      ecode = SOCK_OK;
	
    ctop_int(3, (SOCKET) sock_handle);
	
    return set_error_code(ecode, 4, "SOCKET_REQUEST");

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
    } else  {
      xsb_abort("SOCKET_REQUEST: Invalid domain. Valid domains are: 0 - AF_INET, 1 - AF_UNIX");           
      return FALSE;
    }
    
    /* Bind server to the agreed upon port number.
    ** See commdef.h for the actual port number. */
    FillWithZeros(socket_addr);
    socket_addr.sin_port = htons((unsigned short)portnum);
    socket_addr.sin_family = AF_INET;
#ifndef WIN_NT
    socket_addr.sin_addr.s_addr = htonl(INADDR_ANY);
#endif
    
    retcode = bind(sock_handle, (PSOCKADDR) &socket_addr, sizeof(socket_addr));
	
    /* error handling */
    if (SOCKET_OP_FAILED(retcode)) {
      ecode = XSB_SOCKET_ERRORCODE;
      perror("SOCKET_BIND");
    } else
      ecode = SOCK_OK;

    return set_error_code(ecode, 5, "SOCKET_BIND");

  case SOCKET_LISTEN: 
    /* socket_request(SOCKET_LISTEN,+sock_handle,+length,-Error,_,_,_) */
    sock_handle = (SOCKET) ptoc_int(2);
    retcode = listen(sock_handle, ptoc_int(3));

    /* error handling */
    if (SOCKET_OP_FAILED(retcode)) {
      ecode = XSB_SOCKET_ERRORCODE;
      perror("SOCKET_LISTEN");
    } else
      ecode = SOCK_OK;

    return set_error_code(ecode, 4, "SOCKET_LISTEN");

  case SOCKET_ACCEPT:
    /* socket_request(SOCKET_ACCEPT,+sock_handle_in,
       	       	       	       	    -sock_handle_out, -Error,_,_,_)  */
    sock_handle_in = (SOCKET) ptoc_int(2);
    sock_handle = accept(sock_handle_in, NULL, NULL);
	
    /* error handling */ 
    if (BAD_SOCKET(sock_handle)) {
      ecode = XSB_SOCKET_ERRORCODE;
      perror("SOCKET_ACCEPT");
    } else
      ecode = SOCK_OK;

    ctop_int(3, (SOCKET) sock_handle);
	
    return set_error_code(ecode, 4, "SOCKET_ACCEPT");
  
  case SOCKET_CONNECT: {
    /* socket_request(SOCKET_CONNECT,+domain,+sock_handle,+port,
				     +hostname,-Error) */
    int timeout_flag; 

    pSock = make_timeout_obj();

    /* control time out */
    if (CHECK_TIMER_SET) {
      timeout_flag=make_timed_call(pSock, socket_connect);

      if (timeout_flag == TIMER_SETUP_ERR) {
        FREE_TIMEOUT_AND_SET_ECODE(TIMER_SETUP_ERR,6,"SOCKET_CONNECT");
      } else if(timeout_flag) {   /* timed out and return */ 
        FREE_TIMEOUT_AND_SET_ECODE(TIMEOUT_ERR, 6,"SOCKET_CONNECT");
      }
    } else { /* timer not set */
      socket_connect(pSock);
    }

    /* error handling */
    if (SOCKET_OP_FAILED(pSock->return_code)) {
      ecode = XSB_SOCKET_ERRORCODE;
      perror("SOCKET_CONNECT");
      closesocket(ptoc_int(3));
    } else
      ecode = SOCK_OK;
	
    FREE_TIMEOUT_AND_SET_ECODE(ecode, 6, "SOCKET_CONNECT");
}

  case SOCKET_CLOSE: 
    /* socket_request(SOCKET_CLOSE,+sock_handle,-Error,_,_,_,_) */
    
    sock_handle = (SOCKET)ptoc_int(2);
    
    /* error handling */
    retcode = closesocket(sock_handle);
    if (SOCKET_OP_FAILED(retcode)) {
      ecode = XSB_SOCKET_ERRORCODE;
      perror("SOCKET_CLOSE");
    } else
      ecode = SOCK_OK;
    
    return set_error_code(ecode, 3, "SOCKET_CLOSE");
    
  case SOCKET_RECV: {
    /* socket_request(SOCKET_RECV,+Sockfd, -Msg, -Error,_,_,_) */
    int timeout_flag; 

    pSock = make_timeout_obj();
    
    /* control time out */
    if (CHECK_TIMER_SET) {
      timeout_flag=make_timed_call(pSock,socket_recv);
      if(timeout_flag == TIMER_SETUP_ERR) {
	FREE_TIMEOUT_AND_SET_ECODE(TIMER_SETUP_ERR,4,"SOCKET_RECV");
      } else if(timeout_flag) {  /* timed out */
        FREE_TIMEOUT_AND_SET_ECODE(TIMEOUT_ERR,4,"SOCKET_RECV");
      }
    } else  /* timer not set */ 
      socket_recv(pSock);
    
    /* error handling */
    switch (pSock->return_code) {
    case SOCK_OK:
      ecode = SOCK_OK;
      break;
    case SOCK_READMSG_FAILED:
      ecode = XSB_SOCKET_ERRORCODE;
      perror("SOCKET_RECV");
      break;
    case SOCK_READMSG_EOF:
      ecode = SOCK_EOF;
      break;
    default:
      xsb_abort("SOCKET_RECV: XSB bug: invalid return code from readmsg");
    }
    
    if (pSock->sockdata != NULL) 
      ctop_string(3,(char*)string_find(pSock->sockdata,1));
    else  /* this happens at end of file */
      ctop_string(3,(char*)string_find("",1));

    FREE_TIMEOUT_AND_SET_ECODE(ecode,4,"SOCKET_RECV");  

  }
  
  case SOCKET_SEND: {
    /* socket_request(SOCKET_SEND,+Sockfd, +Msg, -Error,_,_,_) */
    int timeout_flag; 

    pSock = make_timeout_obj();
    
    /* control time out */
    if (CHECK_TIMER_SET) {
      timeout_flag=make_timed_call(pSock,socket_send);
      if(timeout_flag == TIMER_SETUP_ERR ) {
        FREE_TIMEOUT_AND_SET_ECODE(TIMER_SETUP_ERR, 4,"SOCKET_SEND");
      } else if(timeout_flag) {  /* timed out */
        FREE_TIMEOUT_AND_SET_ECODE(TIMEOUT_ERR, 4, "SOCKET_SEND"); 
      }
    } else  /* timer not set */
      socket_send(pSock);
    
    if (SOCKET_OP_FAILED(pSock->return_code)) {
      ecode = XSB_SOCKET_ERRORCODE;
      perror("SOCKET_SEND");
    } else
      ecode = SOCK_OK;
      
     FREE_TIMEOUT_AND_SET_ECODE(ecode, 4, "SOCKET_SEND"); 
  }

  case SOCKET_GET0: {
    /* socket_request(SOCKET_GET0,+Sockfd,-C,-Error,_,_,_) */
    int timeout_flag;

    pSock = make_timeout_obj();

    /* control time out */
    if (CHECK_TIMER_SET) {
      timeout_flag=make_timed_call(pSock,socket_get0);
      if (timeout_flag == TIMER_SETUP_ERR) {
        FREE_TIMEOUT_AND_SET_ECODE(TIMER_SETUP_ERR, 4,"SOCKET_GET0"); 
      } else if(timeout_flag) /* timed out */ {
        FREE_TIMEOUT_AND_SET_ECODE(TIMEOUT_ERR, 4, "SOCKET_GET0"); 
      }
    } else  /* timer not set */
      socket_get0(pSock);
    
    /*error handling */ 
    switch (pSock->return_code) {
    case 1:
      ctop_int(3,(unsigned char)(*(char*)(pSock->sockdata)));
      ecode = SOCK_OK;
      break;
    case 0:
      ecode = SOCK_EOF;
      break;
    default:
      ctop_int(3,-1);
      perror("SOCKET_GET0");
      ecode = XSB_SOCKET_ERRORCODE;
    }

     FREE_TIMEOUT_AND_SET_ECODE(ecode, 4, "SOCKET_GET0");
  }

  case SOCKET_PUT: {
    /* socket_request(SOCKET_PUT,+Sockfd,+C,-Error_,_,_) */
    int timeout_flag;

    pSock = make_timeout_obj();

    /* control time out */
    if (CHECK_TIMER_SET) {
      timeout_flag=make_timed_call(pSock,socket_put);

      if(timeout_flag == TIMER_SETUP_ERR)  {
        FREE_TIMEOUT_AND_SET_ECODE(TIMER_SETUP_ERR, 4,"SOCKET_PUT"); 
      } else if(timeout_flag) /* timed out */  {
        FREE_TIMEOUT_AND_SET_ECODE(TIMEOUT_ERR, 4, "SOCKET_PUT"); 
      }
    } else  /* timer not set */ {
      socket_put(pSock);
    }

    /* error handling */
    if (pSock->return_code == 1)
      ecode = SOCK_OK;
    else if (SOCKET_OP_FAILED(pSock->return_code)) {
      ecode = XSB_SOCKET_ERRORCODE;
      perror("SOCKET_PUT");
    }
    
     FREE_TIMEOUT_AND_SET_ECODE(ecode, 4, "SOCKET_PUT");
  }

  case SOCKET_SET_OPTION: {
    /* socket_request(SOCKET_SET_OPTION,+Sockfd,+OptionName,+Value,_,_,_) */
    
    char *option_name;
    sock_handle = (SOCKET) ptoc_int(2);
    option_name = ptoc_string(3);

    /* Set the "linger" parameter to a small number of seconds */
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
     }else {
      xsb_warn("SOCKET_SET_OPTION: Invalid option, `%s'", option_name);
      return FALSE;
    }
    
    return TRUE;
  }

  case SOCKET_SET_SELECT:  {  
    /*socket_request(SOCKET_SET_SELECT,+connection_name,
                                       +R_sockfd,+W_sockfd,+E_sockfd) */
    prolog_term R_sockfd, W_sockfd, E_sockfd;
    int connection_count;
    int rmax_fd=0, wmax_fd=0, emax_fd=0; 

    /* bind fds to input arguments */
    R_sockfd = reg_term(3);
    W_sockfd = reg_term(4);
    E_sockfd = reg_term(5);	

    /* initialize the array of connect_t structure for select call */	
    init_connections(); 

    /* check whether there is empty slot left for connection */	
    if ((connection_count=checkslot())<MAXCONNECT) {
      if (connections[connection_count].connection_name == NULL) {
	connections[connection_count].connection_name = ptoc_string(2);
 	connections[connection_count].empty_flag = 0;

	/* call the utility function seperately to take the fds in */
	list_sockfd(R_sockfd, &connections[connection_count].readset,
		    &rmax_fd, &connections[connection_count].read_fds,
			&connections[connection_count].sizer);
	list_sockfd(W_sockfd, &connections[connection_count].writeset,
		    &wmax_fd, &connections[connection_count].write_fds,
			&connections[connection_count].sizew);
	list_sockfd(E_sockfd, &connections[connection_count].exceptionset, 
		    &emax_fd,&connections[connection_count].exception_fds,
			&connections[connection_count].sizee);

	connections[connection_count].maximum_fd =
	  max(max(rmax_fd,wmax_fd), emax_fd);
      } else 
	/* if this one is reached, it is probably a bug */
	xsb_abort("SOCKET_SET_SELECT: All connections are busy!");
    } else
      xsb_abort("SOCKET_SET_SELECT: Max number of collections exceeded!");
	
    return TRUE;
  }

  case SOCKET_SELECT: {
    /* socket_request(SOCKET_SELECT,+connection_name, +timeout
                                    -avail_rsockfds,-avail_wsockfds,
				    -avail_esockfds,-ecode)
       Returns 3 prolog_terms for available socket fds */

    prolog_term Avail_rsockfds, Avail_wsockfds, Avail_esockfds;
    prolog_term Avail_rsockfds_tail, Avail_wsockfds_tail, Avail_esockfds_tail;

    int maxfd;
    int i;       /* steps for connection_count */
    char *connection_name;
    struct timeval *tv;
    prolog_term timeout_term;
    int timeout =0;
    int connectname_found = FALSE;
    int count=0;			
    connection_name = ptoc_string(2);

    /* specify the time out */
    timeout_term = reg_term(3);
    timeout = p2c_int(timeout_term);
    
    /* initialize tv */
    tv = (struct timeval *)malloc(sizeof(struct timeval));
    tv->tv_sec = timeout;
    tv->tv_usec = 0;

    /* initialize the prolog term */ 
    Avail_rsockfds = p2p_new();
    Avail_wsockfds = p2p_new();
    Avail_esockfds = p2p_new(); 

    /* bind to output arguments */
    Avail_rsockfds = reg_term(4);
    Avail_wsockfds = reg_term(5);
    Avail_esockfds = reg_term(6);

    Avail_rsockfds_tail = Avail_rsockfds;
    Avail_wsockfds_tail = Avail_wsockfds;
    Avail_esockfds_tail = Avail_esockfds;

    c2p_list(Avail_rsockfds_tail);
    c2p_list(Avail_wsockfds_tail);	
    c2p_list(Avail_esockfds_tail); 
    
    for (i=0; i < MAXCONNECT; i++) {
      /* find the matching connection_name to select */
      if(connections[i].empty_flag==0) {
	if (strcmp(connection_name, connections[i].connection_name) == 0) {
	      connectname_found = TRUE;
              count = i;
	      break;
	} 
      }
    }
    if( i >= MAXCONNECT )  /* if no matching connection_name */
      xsb_abort("SOCKET_SELECT: connection `%s' doesn't exist",
                connection_name); 
    
    /* compute maxfd for select call */
    maxfd = connections[count].maximum_fd + 1;

    /* test whether the socket fd is available */
    retcode = select(maxfd, &connections[count].readset, 
		     &connections[count].writeset,
		     &connections[count].exceptionset, tv);

    /* error handling */	
    if (retcode == 0)     /* timed out */
      ecode = TIMEOUT_ERR;
    else if (SOCKET_OP_FAILED(retcode)) {
      perror("SOCKET_SELECT");
      ecode = XSB_SOCKET_ERRORCODE;
    } else {      /* no error */
      ecode = SOCK_OK;

      /* call the utility function to return the available socket fds */
      test_ready(&Avail_rsockfds_tail, &connections[count].readset,
	       connections[count].read_fds,connections[count].sizer);

      test_ready(&Avail_wsockfds_tail, &connections[count].writeset,
	       connections[count].write_fds,connections[count].sizew);

      test_ready(&Avail_esockfds_tail,&connections[count].exceptionset,
		connections[count].exception_fds,connections[count].sizee);
    }

    free((struct timeval *)tv);
    return set_error_code(ecode, 7, "SOCKET_SELECT");
  }

  case SOCKET_SELECT_DESTROY:  { 
    /*socket_request(SOCKET_SELECT_DESTROY, +connection_name) */
    char *connection_name = NULL;
    connection_name = (char*)ptoc_string(2);
    select_destroy(connection_name);
    return TRUE;
  }

  default:
    xsb_warn("SOCKET_REQUEST: Invalid socket request %d", (int) ptoc_int(1));
    return FALSE;
  }

  xsb_bug("SOCKET_REQUEST case %d has no return clause",
	  ptoc_int(1));
}


static xsbBool set_error_code(int ErrCode, int ErrCodeArgNumber, char *Where)
{
  prolog_term ecode_value_term, ecode_arg_term = p2p_new();
  
  ecode_value_term = reg_term(ErrCodeArgNumber);
  if (!is_var(ecode_value_term) && !is_int(ecode_value_term))
    xsb_abort("%s: Arg %d (the error code) must be a variable or an integer!",
	      Where, ErrCodeArgNumber);

  c2p_int(ErrCode, ecode_arg_term);
  return p2p_unify(ecode_arg_term, ecode_value_term);
}

/* free timeout object */
static void free_timeout_obj(xsbTimeout *pSock)
{
  if(pSock->sockdata !=NULL) {
    free(pSock->sockdata);    /* free user specified data first */
  }
  free(pSock);                /* free the whole structure */
  return;
}

static xsbTimeout *make_timeout_obj()
{
  xsbTimeout *pSock=NEW_TIMEOUT_OBJECT;
  pSock->sockdata = NULL;
  return pSock;
}

/* initialize the array of the structure */
static void init_connections() 
{
  int i;
  static int initialized = FALSE;

  if (!initialized) {
    for (i=0; i<MAXCONNECT; i++) {
      connections[i].connection_name = NULL; 
      connections[i].maximum_fd=0;
      connections[i].empty_flag=1;
      /*clear all FD_SET */
      FD_ZERO(&connections[i].readset);
      FD_ZERO(&connections[i].writeset);
      FD_ZERO(&connections[i].exceptionset);

      connections[i].read_fds = 0;
      connections[i].write_fds = 0 ;
      connections[i].exception_fds = 0; 
      connections[i].sizer= 0;
      connections[i].sizew = 0 ;
      connections[i].sizee = 0 ;
   }
   initialized = TRUE;
 }
}


/* utility function to take the user specified fds in and prepare for
   select call */

static xsbBool list_sockfd(prolog_term list, fd_set *fdset, int *max_fd,
			   int **fds, int * size)
{
  int i=0;
  prolog_term local=list;
  prolog_term head;

  FD_ZERO(fdset); 
  *size = getsize(local);

  *fds = (int*)malloc(sizeof(int)*(*size));

  while (!is_nil(list)) {
    head = p2p_car(list);
    (*fds)[i++] = p2c_int(head);
    list = p2p_cdr(list);
  }

  for (i=0; i<(*size); i++) {
    /* turn on the bit in the fd_set */
    FD_SET((*fds)[i], fdset);
    *max_fd = max(*max_fd, (*fds)[i]);
  }

  return TRUE;
}

/* utility function to return the available socket descriptors after testing */
static void test_ready(prolog_term *avail_sockfds, fd_set *fdset,
		       int *fds, int size) 
{
  prolog_term head;
  int i=0;

  for (i=0;i<size;i++) {
    if (FD_ISSET(fds[i], fdset)) {
      head = p2p_car(*avail_sockfds);
      c2p_int(fds[i], head);
      *avail_sockfds = p2p_cdr(*avail_sockfds);
      c2p_list(*avail_sockfds);
    } 
  }
  c2p_nil(*avail_sockfds);
  return;
}

/* utility function to destroy a select call */
static void select_destroy(char *connection_name)
{
  int i;
  int connectname_found = FALSE;

  for (i=0; i < MAXCONNECT; i++) {
    if(connections[i].empty_flag==0) {
      /* find the matching connection_name to destroy */
      if (strcmp(connection_name, connections[i].connection_name) == 0) {
        connectname_found = TRUE;
	      
        /* destroy the corresponding structure */
        FD_ZERO(&connections[i].readset);
        FD_ZERO(&connections[i].writeset);
        FD_ZERO(&connections[i].exceptionset);

        connections[i].connection_name = NULL;
        connections[i].maximum_fd = 0;

	/* free the fds obtained by malloc() */
        free(connections[i].read_fds);
        free(connections[i].write_fds);
        free(connections[i].exception_fds); 
      
	connections[i].sizer = 0;
	connections[i].sizew = 0 ;
	connections[i].sizee = 0 ;

	connections[i].empty_flag = 1; /* set the destroyed slot to empty */ 
        break;
      }
    }
  }
  
  /* if no matching connection_name */
  if (!connectname_found)
    xsb_abort("SOCKET_SELECT_DESTROY: connection `%s' doesn't exist", 
	      connection_name);
  
}

/* utility function to check whether there is empty slot left to connect */
static int checkslot (void) {
	int i;
	for (i=0; i<MAXCONNECT;i++) {
		if (connections[i].empty_flag == 1) break;
	}
	return i;
}

/* get the size of the list input from prolog side */
static int getsize (prolog_term list)
{
  int size = 0;
  prolog_term head;

  while (!is_nil(list)) {
    head = p2p_car(list);
    if(!is_int(head)) 
      xsb_abort("Socket descriptors must be integers!");
    list = p2p_cdr(list);
    size++;
  }

  return size;
}
