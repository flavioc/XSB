/******************************************************************************
 *                             socketcall.h
 *  Files to include for socket handling for windows and unix
 *
 *****************************************************************************/
#ifdef WIN32
#include <winsock2.h>
#include <windows.h>
#else 
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>
#endif

