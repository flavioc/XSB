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

#include "configs/config.h"
#include "debugs/debug.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>

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


/* In WIN_NT, this gets redefined into _fdopen by configs/special.h */
extern FILE *fdopen(int fildes, const char *type);

int retcode; /* return code from socket operation */

static bool set_error_code(int ErrCode, int ErrCodeArgNumber, char *Where);

static int connection_count = -1; /* global variable for socket select calls */


/*define the structure for testing file descriptors in select calls */
typedef struct fd_list {
  int fd;
  struct fd_list *nextptr;
} FD_LIST;

static void init_connections();
static bool list_sockfd(prolog_term list, fd_set *fdset, int *max_fd,
			FD_LIST *head_ptr);
static void test_ready(prolog_term *avail_sockfds, fd_set *fdset,
		       FD_LIST *fds_ptr);
static void select_destroy(char *connection_name);

/* define the structure for select call */
static struct connection_t {
  char *connection_name;
  int maximum_fd;
  fd_set *readset;
  fd_set *writeset;
  fd_set *exceptionset;
  FD_LIST *read_fds;
  FD_LIST *write_fds;
  FD_LIST *exception_fds; 
} connections[MAXCONNECT];


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


/* Returns:
   normal:  SOCK_OK
   EOF:     SOCK_READMSG_EOF
   error:   SOCK_READMSG_FAILED

   Read message header, then read the message itself.
*/
int readmsg(SOCKET sock_handle, char **msg_buff)	
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
    xsb_warn("%s: msg header length %ld differs from actual msg length %ld",
	     "SOCKET_RECV", msglen, actual_len);
  
  return SOCK_OK;
}



/* in order to save builtin numbers, create a single socket function with
 * options socket_request(SockOperation,....)  */
bool xsb_socket_request(void)
{
  static int ecode = 0;                     /* error code for socket ops */
  static SOCKET sock_handle, sock_handle_in;
  static int domain, portnum;
  static SOCKADDR_IN socket_addr;
  static struct linger sock_linger_opt;

  switch (ptoc_int(1)) {
  case SOCKET_ROOT:
    /* socket_request(SOCKET_ROOT,+domain,-socket_fd,-Error,_,_,_) 
       Currently only AF_INET domain */
    domain = ptoc_int(2); 
    if (domain == 0) domain = AF_INET;
    else if (domain == 1){
      domain = AF_UNIX;
      xsb_abort("SOCKET_REQUEST: domain AF_INET is not implemented");
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
    socket_addr.sin_port = htons(portnum);
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
    /* socket_request(SOCKET_ACCEPT,+sock_handle_in, -sock_handle_out, -Error,_,_,_)  */
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
  
  case SOCKET_CONNECT:
    /* socket_request(SOCKET_CONNECT,+domain,+sock_handle,+port,
       +hostname,-Error) */
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
    
    /*** prepare to connect ***/
    FillWithZeros(socket_addr);
    socket_addr.sin_port = htons(portnum);
    socket_addr.sin_family = AF_INET;
    socket_addr.sin_addr.s_addr = inet_addr(get_host_IP(ptoc_string(5)));
   
    /* control time out */
    if (CHECK_TIMER_SET) {
      SETALARM;
      if (!OP_TIMED_OUT ) { /* no timeout */
	SET_TIMER; 
	retcode =
	  connect(sock_handle,(PSOCKADDR) &socket_addr,sizeof(socket_addr));
	TURNOFFALARM;
      } else {  /* timeout */
	TURNOFFALARM;
	return set_error_code(TIMEOUT_ERR, 6, "SOCKET_CONNECT");
      }
    } else  /* timer not set */
      retcode =
	connect(sock_handle, (PSOCKADDR) &socket_addr, sizeof(socket_addr));
   
    /* error handling */
    if (SOCKET_OP_FAILED(retcode)) {
      ecode = XSB_SOCKET_ERRORCODE;
      perror("SOCKET_CONNECT");
      closesocket(sock_handle);
    } else
      ecode = SOCK_OK;
   
    return set_error_code(ecode, 6, "SOCKET_CONNECT");

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
    char *recv_msg=NULL;

    sock_handle = (SOCKET) ptoc_int(2);
	
    /* control time out */
    if (CHECK_TIMER_SET) {
      SETALARM;
      if (!OP_TIMED_OUT ) { /* no timeout */
	SET_TIMER; 
	retcode = readmsg(sock_handle, &recv_msg);
	TURNOFFALARM;
      } else {  /* timeout */
	TURNOFFALARM;
	if (recv_msg != NULL) free(recv_msg);
	return set_error_code(TIMEOUT_ERR, 4, "SOCKET_RECV");
      }
    } else   /* timer not set */
      retcode = readmsg(sock_handle, &recv_msg);

    switch (retcode) {
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

    if (recv_msg != NULL) {
      ctop_string(3,(char*)string_find(recv_msg,1));
      free(recv_msg);
    }
    else  /* this happens at end of file */
      ctop_string(3,(char*)string_find("",1));

    return set_error_code(ecode, 4, "SOCKET_RECV");
  }

  case SOCKET_SEND: {
    /* socket_request(SOCKET_SEND,+Sockfd, +Msg, -Error,_,_,_) */
    char *send_msg, *send_msg_aux;
    unsigned int msg_body_len, network_encoded_len;

    sock_handle = (SOCKET) ptoc_int(2);
    send_msg_aux = (char*) ptoc_string(3);
    /* We use the first XSB_MSG_HEADER_LENGTH bytes for the message size. */
    if ((send_msg
	 = calloc(strlen(send_msg_aux)+XSB_MSG_HEADER_LENGTH+1, sizeof(char)))
	== NULL) {
      xsb_abort("SOCKET_SEND: Can't allocate memory for the message buffer");
    }
    msg_body_len = strlen(send_msg_aux);
    network_encoded_len = 
      (unsigned int) htonl((unsigned long int) msg_body_len); 
    memcpy((void *) send_msg, (void *) &network_encoded_len,
	   XSB_MSG_HEADER_LENGTH);
    strcpy(send_msg+XSB_MSG_HEADER_LENGTH, send_msg_aux);

    /* control time out */
    if (CHECK_TIMER_SET) {
      SETALARM; 
      if (!OP_TIMED_OUT ) { /* no timeout */
	SET_TIMER; 
	retcode = sendto(sock_handle, send_msg,
			 msg_body_len+XSB_MSG_HEADER_LENGTH, 0, NULL, 0);
	TURNOFFALARM;
      } else {  /* timeout */
	TURNOFFALARM;
	if (send_msg != NULL) free(send_msg);
	return set_error_code(TIMEOUT_ERR, 4, "SOCKET_SEND");
      }
    } else  /* timer not set */
      retcode = sendto(sock_handle, send_msg,
		       msg_body_len+XSB_MSG_HEADER_LENGTH, 0, NULL, 0);

    if (send_msg != NULL) free(send_msg);

    if (SOCKET_OP_FAILED(retcode)) {
      ecode = XSB_SOCKET_ERRORCODE;
      perror("SOCKET_SEND");
    } else
      ecode = SOCK_OK;

    return set_error_code(ecode, 4, "SOCKET_SEND");
  }

  case SOCKET_GET0: {
    /* socket_request(SOCKET_GET0,+Sockfd,-C,-Error,_,_,_) */
    static char tmpch;

    sock_handle = (SOCKET) ptoc_int(2);

    /* control time out */
    if (CHECK_TIMER_SET) {
      SETALARM;
      if (!OP_TIMED_OUT ) { /* no timeout */
	SET_TIMER; 
	retcode = recvfrom(sock_handle,&tmpch,1,0,NULL,0);
	TURNOFFALARM;
      } else {  /* timeout */
	TURNOFFALARM;
	return set_error_code(TIMEOUT_ERR, 4, "SOCKET_GET0");
      }
    } else  /* timer not set */
      retcode = recvfrom(sock_handle,&tmpch,1,0,NULL,0);

    /*error handling */ 
    switch (retcode) {
    case 1:
      ctop_int(3,(unsigned char)tmpch);
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

    return set_error_code(ecode, 4, "SOCKET_GET0");
  }

  case SOCKET_PUT: {
    /* socket_request(SOCKET_PUT,+Sockfd,+C,-Error_,_,_) */
    static char tmpch;
    sock_handle = (SOCKET) ptoc_int(2);
    tmpch = (char)ptoc_int(3);

    /* control time out */
    if (CHECK_TIMER_SET) {
      SETALARM;
      if (!OP_TIMED_OUT ) { /* no timeout */
	SET_TIMER; 
	retcode = sendto(sock_handle, &tmpch, 1, 0, NULL, 0);
	TURNOFFALARM;
      } else {  /* timeout */
	TURNOFFALARM;
	return set_error_code(TIMEOUT_ERR, 4, "SOCKET_PUT");
      }
    } else  /* timer not set */
      retcode = sendto(sock_handle, &tmpch, 1, 0, NULL, 0);
	
    /* error handling */
    if (retcode == 1)
      ecode = SOCK_OK;
    else if (SOCKET_OP_FAILED(retcode)) {
      ecode = XSB_SOCKET_ERRORCODE;
      perror("SOCKET_PUT");
    }
	
    return set_error_code(ecode, 4, "SOCKET_PUT");
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


  case SOCKET_SET_SELECT:  {  
    /*socket_request(SOCKET_SET_SELECT,+connection_name,
                                       +R_sockfd,+W_sockfd,+E_sockfd) */
    prolog_term R_sockfd, W_sockfd, E_sockfd;
    int rmax_fd, wmax_fd, emax_fd;
    connection_count++;

    /*bind to input arguments */
    R_sockfd = reg_term(3);
    W_sockfd = reg_term(4);
    E_sockfd = reg_term(5);	
	
    init_connections(); 
	
    if (connection_count < MAXCONNECT) {
      if (connections[connection_count].connection_name == NULL) {
	connections[connection_count].connection_name = ptoc_string(2);

	/* call the utility function seperately to take the input in */
	list_sockfd(R_sockfd, connections[connection_count].readset,
		    &rmax_fd, connections[connection_count].read_fds);
	list_sockfd(W_sockfd, connections[connection_count].writeset,
		    &wmax_fd, connections[connection_count].write_fds);
	list_sockfd(E_sockfd, connections[connection_count].exceptionset, 
		    &emax_fd, connections[connection_count].exception_fds);

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

    prolog_term avail_rsockfds, avail_wsockfds, avail_esockfds;
    prolog_term avail_rmember, avail_wmember, avail_emember;
    int maxfd;
    int i, count=0;
    char *connection_name;
    struct timeval *tv;
    prolog_term timeout_term;
    int timeout =0;
    int connectname_found = FALSE;
			
    connection_name = ptoc_string(2);

    /* specify the time out */
    timeout_term = reg_term(3);
    timeout = p2c_int(timeout_term);
    /* initialize tv */
    tv = (struct timeval *)malloc(sizeof(struct timeval));
    tv->tv_sec = timeout;
    tv->tv_usec = 0;

    avail_rsockfds = reg_term(4);
    avail_wsockfds = reg_term(5);
    avail_esockfds = reg_term(6);

    c2p_list(avail_rsockfds);
    c2p_list(avail_wsockfds);	
    c2p_list(avail_esockfds);
    
    avail_rmember = avail_rsockfds;
    avail_wmember = avail_wsockfds;
    avail_emember = avail_esockfds;
    
    for (i=0; i < connection_count; i++) {
      /* find the matching connection_name to select */
      if (strcmp(connection_name, connections[i].connection_name) == 0) {
	connectname_found = TRUE;
	break;
      }
    }
    
    /* if no matching connection_name */
    if (!connectname_found)
      xsb_abort("SOCKET_SELECT: connection `%s' doesn't exist", 
		connection_name);
    else 
      /* the index for the connections array to select */
      count = i;
    
    /* compute maxfd for select call */
    maxfd = connections[count].maximum_fd + 1;
	
    /* test whether the socket fd is available */
    retcode = select(maxfd, connections[count].readset, 
		     connections[count].writeset,
		     connections[count].exceptionset, tv);

    /* error handling */	
    if (retcode == 0) 
      ecode = TIMEOUT_ERR;
    if (SOCKET_OP_FAILED(retcode)) {
      perror("SOCKET_SELECT");
      ecode = XSB_SOCKET_ERRORCODE;
    } else
      ecode = SOCK_OK;

    /* call the utility function to return the available socket fds */
    test_ready(&avail_rmember, connections[count].readset,
	       connections[count].read_fds);

    test_ready(&avail_wmember, connections[count].writeset,
	       connections[count].write_fds);

    test_ready(&avail_emember,connections[count].exceptionset,
	       connections[count].exception_fds);

    return set_error_code(ecode, 7, "SOCKET_SELECT");
  }

  case SOCKET_SELECT_DESTROY:  { 
    /*socket_request(SOCKET_SELECT_DESTROY, +connection_name) */
    char *connection_name = ptoc_string(2);
    select_destroy(connection_name);
    return TRUE;
  }

  default:
    xsb_warn("SOCKET_REQUEST: Invalid socket request %d", (int) ptoc_int(1));
    return FALSE;
  }
}


static bool set_error_code(int ErrCode, int ErrCodeArgNumber, char *Where)
{
  prolog_term ecode_value_term, ecode_arg_term = p2p_new();
  
  ecode_value_term = reg_term(ErrCodeArgNumber);
  if (!is_var(ecode_value_term) && !is_int(ecode_value_term))
    xsb_abort("%s: Arg %d (the error code) must be a variable or an integer!",
	      Where, ErrCodeArgNumber);

  c2p_int(ErrCode, ecode_arg_term);
  return p2p_unify(ecode_arg_term, ecode_value_term);
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
     /*clear all FD_SET */
     connections[i].readset = 0;
     connections[i].writeset = 0;
     connections[i].exceptionset = 0;
     connections[i].read_fds = 0;
     connections[i].write_fds = 0 ;
     connections[i].exception_fds = 0; 
   }
   initialized = TRUE;
 }
}


/* utility function to take the user input in and make operations on it */
static bool list_sockfd(prolog_term list, fd_set *fdset, int *max_fd,
			FD_LIST *head_ptr)
{
  prolog_term head;
  FD_LIST *fd_ptr;
  int current_fd;
  FD_LIST *last_ptr;
  last_ptr= NULL;
	
  if (is_nil(list)) head_ptr = NULL;
  else {
    fdset = (fd_set*)malloc(sizeof(fd_set));
    FD_ZERO(fdset);
  }

  while (!is_nil(list)) {
    fd_ptr = (FD_LIST*)malloc(sizeof(FD_LIST));
    if(head_ptr==0) head_ptr = fd_ptr;       
    else 		
      last_ptr->nextptr = fd_ptr;
			
    head = p2p_car(list);
    list = p2p_cdr(list);
    if (is_int(head)) {
      current_fd = p2c_int(head);
      fd_ptr->fd = current_fd;
      /* turn on the bit in the fd_set */
      FD_SET(current_fd, fdset);
      *max_fd = max(*max_fd, current_fd);
    } else
      xsb_abort("Socket descriptors must be integers!");
    last_ptr = fd_ptr;
  }
  return TRUE;
}

/* utility to return the available socket descriptors after testing */
static void test_ready(prolog_term *avail_sockfds, fd_set *fdset,
		       FD_LIST *fds_ptr) 
{
  prolog_term head;
  int ready_fds_exist = FALSE;

  /* initialize prolog_term */
  /*head = p2p_new();*/

  if(fds_ptr==NULL) {
    c2p_nil(*avail_sockfds);
  }
  while ( fds_ptr != 0) {
    if (FD_ISSET(fds_ptr->fd, fdset)) {
      head = p2p_car(*avail_sockfds);
      c2p_int(fds_ptr->fd, head);
      *avail_sockfds = p2p_cdr(*avail_sockfds);
      c2p_list(*avail_sockfds);
      ready_fds_exist = TRUE;
    } 
		
    fds_ptr=fds_ptr->nextptr;
  }
  if (!ready_fds_exist) c2p_nil(*avail_sockfds);
}

/* utility function to destroy a select call */
static void select_destroy(char *connection_name)
{
  int i, j;
  int connectname_found = FALSE;

  for (i=0; i < connection_count; i++) {
    /* find the matching connection_name to destroy */
    if (strcmp(connection_name, connections[i].connection_name) == 0) {
      connectname_found = TRUE;
      
      /* destroy the corresponding structure */
      connections[i].readset = 0;
      connections[i].writeset = 0;
      connections[i].exceptionset = 0;
      connections[i].connection_name = NULL;
      connections[i].maximum_fd = 0;
      connections[i].read_fds = 0;
      connections[i].write_fds = 0;
      connections[i].exception_fds = 0; 
      
      /* decrease the number of connections */
      connection_count--;
      break;
    }
  }
  
  /* if no matching connection_name */
  if (!connectname_found)
    xsb_abort("SOCKET_SELECT_DESTROY: connection `%s' doesn't exist", 
	      connection_name);
  
  /* after destroying, move connections one slot back */
  for (j=i; j < connection_count; j++) {
    strcpy(connections[j].connection_name,
	   connections[j+1].connection_name);
    connections[j].maximum_fd = connections[j+1].maximum_fd;
    connections[j].read_fds = connections[j+1].read_fds;
    connections[j].write_fds = connections[j+1].write_fds;
    connections[j].exception_fds = connections[j+1].exception_fds;
    connections[j].readset = connections[j+1].readset;
    connections[j].writeset = connections[j+1].writeset;
    connections[j].exceptionset = connections[j+1].exceptionset;
  }
}

