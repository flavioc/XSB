/* File: driver_manager.h
** Author: Saikat Mukherjee
** Contact: saikat@cs.sunysb.edu

** Data structures, function declarations for the driver_manager
*/

#ifdef WIN_NT
#include <windows.h>
#endif

#ifdef WIN_NT
#define XSB_DLL
#endif


#define MAX_CONNECTIONS 25
#define MAX_QUERIES 25
#define MAX_DRIVERS 25

#define CONNECT 0 
#define DISCONNECT 1
#define QUERY 2
#define PREPARE 3
#define EXEC_PREPARE 4
#define CLOSE_STMT 5
#define ERROR_MESG 6

#define INT_TYPE 1
#define FLOAT_TYPE 2
#define STRING_TYPE 3
#define TERM_TYPE 4

#define SUCCESS 0
#define FAILURE 1

#define QUERY_BEGIN 0
#define QUERY_RETRIEVE 1

#define QUERY_SIZE 1000
#define ELEMENT_SIZE 200

// **** the database value data structures ****

union xsb_value
{
	int* i_val;
	double* f_val;
	char* str_val;
};

struct xsb_data
{
	int type;
	int length;
	union xsb_value* val;
};

// **** the handle data structures ****

struct xsb_connectionHandle
{
	char* handle; //key
	char* server;
	char* user;
	char* database;
	char* password;
	char* dsn;
	char* driver;
};

struct xsb_queryHandle
{
	struct xsb_connectionHandle* connHandle;
	char* handle; // key
	char* query;
	int state;
	int numParams;
};

// **** the driver data structures ****


struct driverFunction
{
	int functionType;
	union functionPtrs* functionName;	
};

struct driver
{
	char* driver;
	int numberFunctions;
	struct driverFunction** functions;
};

// **** the function ptrs and function declarations ****

union functionPtrs
{
	int (*connectDriver)(struct xsb_connectionHandle*);
	int (*disconnectDriver)(struct xsb_connectionHandle*);
	struct xsb_data** (*queryDriver)(struct xsb_queryHandle*);
	int (*prepareStmtDriver)(struct xsb_queryHandle*);
	struct xsb_data** (*executeStmtDriver)(struct xsb_data**, struct xsb_queryHandle*);
	int (*closeStmtDriver)(struct xsb_queryHandle*);
	char* (*errorMesgDriver)();
};

DllExport int call_conv registerXSBDriver(char* driver, int num);
DllExport int call_conv registerXSBFunction(char* dr, int type, union functionPtrs* func);



