/* File: odbc_driver.h
** Author: Saikat Mukherjee
** Contact: saikat@cs.sunysb.edu

** This is the header file for the ODBC driver.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sql.h>
#include <sqlext.h>

#include "cinterf.h"
#include "driver_manager_defs.h"

#define MAX_HANDLES 25
#define MAX_QUERIES 25


/****** data structures for metadata *****/

struct driverODBC_columnmeta
{
	SQLSMALLINT type;
	SQLUINTEGER length;
};

struct driverODBC_meta
{
	SQLSMALLINT numCols;
	struct driverODBC_columnmeta** types;
};

/******* data structures for handles ******/

struct driverODBC_queryInfo
{
	char* query;
	char* handle;
	SQLHSTMT hstmt;
	struct driverODBC_meta* resultmeta;
	struct driverODBC_meta* parammeta;
};

struct driverODBC_connectionInfo
{
	SQLHDBC hdbc;
	char* handle;
};

/********* function declarations *********/

int driverODBC_initialise();
int driverODBC_connect(struct xsb_connectionHandle* handle);
int driverODBC_disconnect(struct xsb_connectionHandle* handle);
struct xsb_data** driverODBC_query(struct xsb_queryHandle* handle);
struct xsb_data** driverODBC_getNextRow(struct driverODBC_queryInfo* query, int direct);
int driverODBC_prepareStatement(struct xsb_queryHandle* qHandle);
struct xsb_data** driverODBC_execPrepareStatement(struct xsb_data** param, struct xsb_queryHandle* handle);
int driverODBC_closeStatement(struct xsb_queryHandle* handle);
void driverODBC_error(SQLSMALLINT handleType, SQLHANDLE handle);
char* driverODBC_errorMesg();
int driverODBC_getXSBType(SQLSMALLINT dataType);
int driverODBC_register();


extern int registerXSBDriver(char* dr, int num);
extern int registerXSBFunction(char* dr, int type, union functionPtrs* func);


