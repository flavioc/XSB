/* File: mysql_driver.h
** Author: Saikat Mukherjee
** Contact: saikat@cs.sunysb.edu

** This is the header file for the driver for connecting to a MySQL database.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mysql.h"
#include "mysql_com.h"

#include "cinterf.h"
#include "driver_manager_defs.h"

#define MAX_HANDLES 25
#define MAX_QUERIES 25

/****** data structures for handles *****/

struct driverMySQL_connectionInfo
{
	MYSQL* mysql;
	char* handle;
};

struct driverMySQL_queryInfo
{
	char* query;
	char* handle;
	MYSQL_RES* resultSet;
	struct driverMySQL_connectionInfo* connection;
};

/*struct driverMySQL_preparedresultset
{
	MYSQL_STMT* statement;
	struct xsb_queryHandle* handle;
	int returnFields;
	struct xsb_data** metaInfo;
};*/


/****** function declarations *******/

int driverMySQL_initialise();
int driverMySQL_connect(struct xsb_connectionHandle* handle);
int driverMySQL_disconnect(struct xsb_connectionHandle* handle);
struct xsb_data** driverMySQL_query(struct xsb_queryHandle* handle);
struct xsb_data** driverMySQL_getNextRow(struct driverMySQL_queryInfo* query);
int driverMySQL_register();
int driverMySQL_getXSBType(MYSQL_FIELD* field);
void driverMySQL_error(MYSQL* mysql);

//int driverMySQL_prepareStatement(char*, struct xsb_queryHandle*);
//struct xsb_data** driverMySQL_execPrepareStmt(struct xsb_data**, struct xsb_queryHandle*);
//struct xsb_data** driverMySQL_prepNextRow(struct driverMySQL_preparedresultset*);

extern int registerXSBDriver(char* dr, int num);
extern int registerXSBFunction(char* dr, int type, union functionPtrs* func);


