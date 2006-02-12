/* File: mysql_driver.h
** Author: Saikat Mukherjee
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 2002-2006
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
*/

/*
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

DllExport int call_conv driverMySQL_initialise();
DllExport int call_conv driverMySQL_connect(struct xsb_connectionHandle* handle);
DllExport int call_conv driverMySQL_disconnect(struct xsb_connectionHandle* handle);
DllExport struct xsb_data** call_conv driverMySQL_query(struct xsb_queryHandle* handle);
DllExport int call_conv driverMySQL_register();
DllExport char* call_conv driverMySQL_errorMesg();

//int driverMySQL_prepareStatement(char*, struct xsb_queryHandle*);
//struct xsb_data** driverMySQL_execPrepareStmt(struct xsb_data**, struct xsb_queryHandle*);
//struct xsb_data** driverMySQL_prepNextRow(struct driverMySQL_preparedresultset*);

extern DllExport int call_conv registerXSBDriver(char* dr, int num);
extern DllExport int call_conv registerXSBFunction(char* dr, int type, union functionPtrs* func);


