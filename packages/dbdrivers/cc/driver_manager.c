/* File: driver_manager.c
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
** This file is the  middle layer for the interface to the drivers. 
** This gets called from db_interface.P and in turn calls functions in
** driver specific C files which are in the packages/dbdrivers/xxx/cc directory.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "xsb_config.h"
#include "cell_xsb.h"

#ifdef WIN_NT
#define XSB_DLL
#endif

#include "cinterf.h"
#include "io_builtins_xsb.h"
#include "driver_manager_defs.h"

static struct xsb_connectionHandle* isConnectionHandle(char* handle);
static struct xsb_queryHandle* isQueryHandle(char* handle);
static char* buildSQLQuery(prolog_term sqlQueryList);
static union functionPtrs* getDriverFunction(char* driver, int type);
static int bindReturnList(prolog_term returnList, struct xsb_data** result, struct xsb_queryHandle*);
static void freeQueryHandle(struct xsb_queryHandle* qHandle, int pos);
static void freeConnectionHandle(struct xsb_connectionHandle* cHandle, int pos);
static int closeQueryHandle(char* queryHandle);

struct xsb_connectionHandle* CHandles[MAX_CONNECTIONS];
struct xsb_queryHandle* QHandles[MAX_QUERIES];
struct driver* DBdrivers[MAX_DRIVERS];
int numDrivers, numCHandles, numQHandles;
char* errorMesg;
char* errorNumber;


DllExport int call_conv initialise(void)
{
  numDrivers = numCHandles = numQHandles = 0;
  errorMesg = NULL;
  errorNumber = NULL;
  return TRUE;
}


DllExport int call_conv openConnection(void)
{
  int (*connectDriver)(struct xsb_connectionHandle*);
  char* (*errorMesgDriver)();
  struct xsb_connectionHandle* cHandle;
  char *handle, *driver, *server, *database=NULL, *user, *password, *dsn;
  int val;

  handle = ptoc_string(1);
  driver = ptoc_string(2);
  server = ptoc_string(3);
  if (strlen(server) == 0)
    dsn = ptoc_string(4);
  else
    database = ptoc_string(4);
  user = ptoc_string(5);
  password = ptoc_string(6);

  if (isConnectionHandle(handle) != NULL) {
    errorMesg = "XSB_DBI ERROR: Connection handle already exists";
    errorNumber = "XSB_DBI_006";
    return FALSE;
  }

  if (getDriverFunction(driver, CONNECT) != NULL)
    connectDriver = getDriverFunction(driver, CONNECT)->connectDriver;
  else
    return FALSE;

  cHandle = (struct xsb_connectionHandle *)malloc(sizeof(struct xsb_connectionHandle));
  cHandle->handle = handle;
  cHandle->driver = driver;
  if (strlen(server) == 0) {
    cHandle->dsn = dsn;
    cHandle->server = NULL;
    cHandle->database = NULL;
  }
  else {
    cHandle->server = server;
    cHandle->database = database;
    cHandle->dsn = NULL;
  }
  cHandle->user = user;
  cHandle->password = password;

  CHandles[numCHandles++] = cHandle;
  if ((val = connectDriver(cHandle)) != SUCCESS) {
    if (getDriverFunction(cHandle->driver, ERROR_MESG) != NULL)
      errorMesgDriver = getDriverFunction(cHandle->driver, ERROR_MESG)->errorMesgDriver;
    else
      return FALSE;

    errorMesg = errorMesgDriver();
    errorNumber = "XSB_DBI_000";
    freeConnectionHandle(cHandle, numCHandles - 1);
    return FALSE;
  }

  return TRUE;
}  


DllExport int call_conv closeConnection(void)
{
  int (*disconnectDriver)(struct xsb_connectionHandle *);
  int (*closeStmtDriver)(struct xsb_queryHandle *);
  char* (*errorMesgDriver)();
  char* handle;
  int val, i, j;

  handle = ptoc_string(1);

  for (i = 0 ; i < numCHandles ; i++) {
    if (!strcmp(CHandles[i]->handle, handle)) {
      if (getDriverFunction(CHandles[i]->driver, DISCONNECT) != NULL)
	disconnectDriver = getDriverFunction(CHandles[i]->driver, DISCONNECT)->disconnectDriver;
      else 
	return FALSE;

      val = disconnectDriver(CHandles[i]);
      
      if (val == FAILURE) {
	errorMesgDriver = getDriverFunction(CHandles[i]->driver, ERROR_MESG)->errorMesgDriver;
	errorMesg = errorMesgDriver();
	return FALSE;
      }
      
      for (j = 0 ; j < numQHandles ; j++) {
	if (!strcmp(QHandles[j]->connHandle->handle, handle)) {
	  if (getDriverFunction(CHandles[i]->driver, ERROR_MESG) != NULL)
	    closeStmtDriver = getDriverFunction(CHandles[i]->driver, ERROR_MESG)->closeStmtDriver;
	  else
	    return FALSE;
	  
	  val = closeStmtDriver(QHandles[j]);
	  if (val == FAILURE) {
	    errorMesgDriver = getDriverFunction(CHandles[i]->driver, ERROR_MESG)->errorMesgDriver;
	    errorMesg = errorMesgDriver();
	    return FALSE;
	  }
	  freeQueryHandle(QHandles[j], j);
	  j--;
	}
      }
      freeConnectionHandle(CHandles[i], i);
      return TRUE;
    }
  }
  
  errorMesg = "XSB_DBI ERROR: Connection handle does not exist";
  errorNumber = "XSB_DBI_004";
  return FALSE;
}


DllExport int call_conv queryConnection(void)
{
  struct xsb_data** (*queryDriver)(struct xsb_queryHandle*);
  char* (*errorMesgDriver)();
  prolog_term returnList, sqlQueryList;
  struct xsb_connectionHandle* cHandle;
  struct xsb_queryHandle* qHandle;
  struct xsb_data** result;
  char *chandle, *qhandle, *sqlQuery;
  int val;

  chandle = ptoc_string(1);
  qhandle = ptoc_string(2);
  sqlQueryList = reg_term(3);
  returnList = reg_term(4);

  result = NULL;

  if ((qHandle = isQueryHandle(qhandle)) != NULL) {
    if (strcmp(qHandle->connHandle->handle, chandle)) {
      errorMesg = "XSB_DBI ERROR: Query handle already exists";
      errorNumber = "XSB_DBI_007";;
      return FALSE;
    }
    
    if (getDriverFunction(qHandle->connHandle->driver, QUERY) != NULL) {
      queryDriver =
	getDriverFunction(qHandle->connHandle->driver, QUERY)->queryDriver;
    }
    else {
      return FALSE;
    }
    
    sqlQuery = buildSQLQuery(sqlQueryList);
    if (strcmp(qHandle->query, sqlQuery)) {
      errorMesg =
	"XSB DBI ERROR: Same query handle used for different queries";
      errorNumber = "XSB_DBI_010";
      return FALSE;
    }

    result = queryDriver(qHandle);
    if (result == NULL && qHandle->state == QUERY_RETRIEVE) {
      closeQueryHandle(qhandle);
    }
  }
  else if ((cHandle = isConnectionHandle(chandle)) != NULL) {
    sqlQuery = buildSQLQuery(sqlQueryList);
    qHandle = (struct xsb_queryHandle *)malloc(sizeof(struct xsb_queryHandle));
    qHandle->handle = qhandle;
    qHandle->connHandle = cHandle;
    qHandle->query = sqlQuery;
    qHandle->state = QUERY_BEGIN;
    QHandles[numQHandles++] = qHandle;

    if (getDriverFunction(qHandle->connHandle->driver, QUERY) != NULL)
      queryDriver = getDriverFunction(qHandle->connHandle->driver, QUERY)->queryDriver;
    else
      return FALSE;

    result = queryDriver(qHandle);
  }
  else {
    errorMesg = "XSB_DBI ERROR: Connection handle does not exist";
    errorNumber = "XSB_DBI_004";
    return FALSE;		
  }

  if (result == NULL) {
    closeQueryHandle(qhandle);
  }
  
  val = bindReturnList(returnList, result, qHandle);
  if (val == TOO_MANY_RETURN_COLS || val == TOO_FEW_RETURN_COLS || val == INVALID_RETURN_LIST)
    return FALSE;


  
  if ((cHandle = isConnectionHandle(chandle)) != NULL) {
    if (getDriverFunction(cHandle->driver, ERROR_MESG) != NULL)
      errorMesgDriver =
	getDriverFunction(cHandle->driver, ERROR_MESG)->errorMesgDriver;
    else 
      return FALSE;
    
    errorMesg = errorMesgDriver();
    errorNumber = "XSB_DBI_000";
  }

  if (errorMesg == NULL && val == RESULT_NONEMPTY_OR_NOT_REQUESTED)
    return TRUE;
  else
    return FALSE;
}

DllExport int call_conv prepareStatement(void)
{
  int (*prepareStmtDriver)(struct xsb_queryHandle*);
  char* (*errorMesgDriver)();
  prolog_term sqlQueryList;
  char *chandle, *qhandle, *sqlQuery;
  struct xsb_queryHandle* qHandle;
  struct xsb_connectionHandle* cHandle;
  int val;
  
  chandle = ptoc_string(1);
  qhandle = ptoc_string(2);
  sqlQueryList = reg_term(3);
  
  if ((cHandle = isConnectionHandle(chandle)) == NULL) {
    errorMesg = "XSB_DBI ERROR: Connection handle does not exist";
    errorNumber = "XSB_DBI_004";
    return FALSE;
  }
  
  if ((qHandle = isQueryHandle(qhandle)) != NULL) {
    errorMesg = "XSB_DBI ERROR: Query handle already exists";
    errorNumber = "XSB_DBI_007";
    return FALSE;
  }

  sqlQuery = buildSQLQuery(sqlQueryList);

  qHandle = (struct xsb_queryHandle *)malloc(sizeof(struct xsb_queryHandle));
  qHandle->connHandle = cHandle;
  qHandle->query = sqlQuery;
  qHandle->handle = qhandle;
  qHandle->state = QUERY_BEGIN;
  
  if (getDriverFunction(cHandle->driver, PREPARE) != NULL)
    prepareStmtDriver = getDriverFunction(cHandle->driver, PREPARE)->prepareStmtDriver;
  else
    return FALSE;
  if ((val = prepareStmtDriver(qHandle)) != FAILURE) {
    qHandle->numParams = val;
    QHandles[numQHandles++] = qHandle;
  }
  else {
    if (getDriverFunction(cHandle->driver, ERROR_MESG) != NULL)
      errorMesgDriver = getDriverFunction(cHandle->driver, ERROR_MESG)->errorMesgDriver;
    else
      return FALSE;
    errorMesg = errorMesgDriver();
    errorNumber = "XSB_DBI_000";
    free(qHandle->query);
    free(qHandle);
    return FALSE;
  }

  return TRUE;
}

DllExport int call_conv executePreparedStatement(void)
{
  struct xsb_data** (*executeStmtDriver)(struct xsb_data**, struct xsb_queryHandle*);
  char* (*errorMesgDriver)();
  struct xsb_queryHandle* qHandle;
  struct xsb_connectionHandle* cHandle;
  struct xsb_data** bindValues;
  struct xsb_data** result;
  prolog_term bindList, returnList, element;
  char *queryHandle, *chandle;
  int i, val;

  queryHandle = ptoc_string(1);
  bindList = reg_term(2);
  returnList = reg_term(3);

  if ((qHandle = isQueryHandle(queryHandle)) == NULL) {
    errorMesg = "XSB_DBI ERROR: Query handle does not exist";
    errorNumber = "XSB_DBI_005";
    return FALSE;
  }
 
  if (qHandle->state == QUERY_BEGIN) {
    bindValues =
      (struct xsb_data **)malloc(qHandle->numParams * sizeof(struct xsb_data *));
    for (i = 0 ; i < qHandle->numParams ; i++) {
      bindValues[i] = (struct xsb_data *)malloc(sizeof(struct xsb_data));
      if (is_nil(bindList)) {
	errorMesg = "XSB_DBI ERROR: Not all paremeters supplied";
	errorNumber = "XSB_DBI_008";
	return FALSE;
      }
      element = p2p_car(bindList);
      if (is_string(element)) {
	bindValues[i]->type = STRING_TYPE;
	bindValues[i]->length = strlen(p2c_string(element));
	bindValues[i]->val = (union xsb_value *)malloc(sizeof(union xsb_value));
	bindValues[i]->val->str_val = p2c_string(element);
      }
      else if (is_int(element)) {
	bindValues[i]->type = INT_TYPE;
	bindValues[i]->val = (union xsb_value *)malloc(sizeof(union xsb_value));
	bindValues[i]->val->i_val = p2c_int(element);
      }
      else if (is_float(element)) {
	bindValues[i]->type = FLOAT_TYPE;
	bindValues[i]->val = (union xsb_value *)malloc(sizeof(union xsb_value));
	bindValues[i]->val->f_val = p2c_float(element);
      }
      else if (is_functor(element)) {
      }
      else if (is_var(element)) {
	errorMesg = "XSB_DBI ERROR: Unbound variable in parameter list";
	errorNumber = "XSB_DBI_009";
	return FALSE;
      }
      bindList = p2p_cdr(bindList);
    }
  }
	
  if (getDriverFunction(qHandle->connHandle->driver, EXEC_PREPARE) != NULL)
    executeStmtDriver =
      getDriverFunction(qHandle->connHandle->driver, EXEC_PREPARE)->executeStmtDriver;
  else
    return FALSE;
  
  result = executeStmtDriver(bindValues, qHandle);

  if (result == NULL && qHandle->state == QUERY_BEGIN) {
    if (getDriverFunction(qHandle->connHandle->driver, ERROR_MESG) != NULL)
      errorMesgDriver =
	getDriverFunction(qHandle->connHandle->driver, ERROR_MESG)->errorMesgDriver;
    else
      return FALSE;
    
    errorMesg = errorMesgDriver();
    return FALSE;
  }
  
  val = bindReturnList(returnList, result, qHandle);

  if (result == NULL) {
    qHandle->state = QUERY_BEGIN;
  }
  
  if (val == TOO_MANY_RETURN_COLS || val == TOO_FEW_RETURN_COLS || val == INVALID_RETURN_LIST)
    return FALSE;

  cHandle = qHandle->connHandle;
  chandle = cHandle->handle;
  if ((cHandle = isConnectionHandle(chandle)) != NULL) {
    if (getDriverFunction(cHandle->driver, ERROR_MESG) != NULL)
      errorMesgDriver =
	getDriverFunction(cHandle->driver, ERROR_MESG)->errorMesgDriver;
    else
      return FALSE;
	  
    errorMesg = errorMesgDriver();
    errorNumber = "XSB_DBI_000";
  }

  if (errorMesg == NULL && val == RESULT_NONEMPTY_OR_NOT_REQUESTED)
    return TRUE;
  else 
    return FALSE;
}


DllExport int call_conv closeStatement(void)
{
  char* queryHandle;
  
  queryHandle = ptoc_string(1);
  return closeQueryHandle(queryHandle);
}


DllExport int call_conv exception(void)
{
  prolog_term number;
  prolog_term message;
	
  number = reg_term(1);
  message = reg_term(2);
  if (is_var(message) && errorMesg != NULL && errorNumber != NULL) {
    c2p_string(errorMesg, message);
    c2p_string(errorNumber, number);
    errorMesg = NULL;
    errorNumber = NULL;
    return TRUE;
  }
	
  return FALSE;
}


static char* buildSQLQuery(prolog_term sqlQueryList)
{
  prolog_term element;
  char* sqlQuery;
  char* temp;
  int i, cnt;

  sqlQuery = (char *)malloc(QUERY_SIZE * sizeof(char));
  sqlQuery[0] = '\0';
  while (!is_nil(sqlQueryList)) {
    element = p2p_car(sqlQueryList);
    sqlQueryList = p2p_cdr(sqlQueryList);
    if (is_string(element)) {
      if (p2c_string(element)[0] == TERM_CHAR) {
	cnt = 0;
	temp = (char *)malloc(ELEMENT_SIZE * sizeof(char));
	temp[cnt++] = '\'';
	/* protect inner quotes in Prolog terms */
	for (i = 0 ; i < strlen(p2c_string(element)) ; i++) {
	  if (p2c_string(element)[i] == '\'') {
	    temp[cnt++] = '\\';
	    temp[cnt++] = p2c_string(element)[i];
	  }
	  else {
	    temp[cnt++] = p2c_string(element)[i];
	  }
	}
	temp[cnt++] = '\'';
	strcat(sqlQuery, temp);
	free(temp);
      }
      else {
	strcat(sqlQuery, p2c_string(element));
      }
    }
    else if (is_int(element)) {
      temp = (char *)malloc(ELEMENT_SIZE * sizeof(char));
      sprintf(temp, "%d", p2c_int(element));
      strcat(sqlQuery, temp);
      free(temp);
    }
    else if (is_float(element)) {
      temp = (char *)malloc(ELEMENT_SIZE * sizeof(char));
      sprintf(temp, "%f", p2c_float(element));			
      strcat(sqlQuery, temp);
      free(temp);
    }
    else if (is_var(element)) {
      errorMesg = "XSB_DBI ERROR: Unbound variable in parameter list";
    }
  }

  return sqlQuery;  
}


static int bindReturnList(prolog_term returnList, struct xsb_data** result, struct xsb_queryHandle* qHandle)
{
  prolog_term element;
  char* temp;
  char c;
  int i, j;
  int rFlag;
  
  if (is_nil(returnList) && result == NULL) {
    rFlag = RESULT_NONEMPTY_OR_NOT_REQUESTED;
  }

  if (!is_nil(returnList) && result == NULL && qHandle->state == QUERY_BEGIN) {
    errorMesg = "XSB_DBI_ERROR: Invalid return list in query";
    errorNumber = "XSB_DBI_012";
    rFlag = INVALID_RETURN_LIST;
  }  
  else if (!is_nil(returnList) && result == NULL) {
    while (!is_nil(returnList)) {
      element = p2p_car(returnList);
      c2p_nil(element);
      returnList = p2p_cdr(returnList);
    }
    rFlag = RESULT_EMPTY_BUT_REQUESTED;
  }

  i = 0;
  if (result != NULL) {
    while (!is_nil(returnList)) {
      if (qHandle->numResultCols <= i) {
	errorMesg = "XSB_DBI ERROR: Number of requested columns exceeds the number of columns in the query";
	errorNumber = "XSB_DBI_011";
	rFlag = TOO_MANY_RETURN_COLS;
	return rFlag;	
      }
      element = p2p_car(returnList);
      if (result == NULL) {
	c2p_nil(element);
      }
      else if (is_var(element) && result[i]->type == STRING_TYPE) {
	if (result[i]->val == NULL)
	  c2p_nil(element);
	else {
	  c = result[i]->val->str_val[0];
	  if (c == TERM_CHAR) {
	    temp = (char *)malloc(strlen(result[i]->val->str_val) * sizeof(char));
	    for (j = 1 ; j < strlen(result[i]->val->str_val) ; j++) {
	      temp[j-1] = result[i]->val->str_val[j];
	    }
	    temp[strlen(result[i]->val->str_val) - 1] = '\0';
	    c2p_functor("term", 1, element);
	    c2p_string(temp, p2p_arg(element, 1));
	    free(temp);
	  }
	  else {
	    c2p_string(result[i]->val->str_val, element);	    
	  }
	}
      }
      else if (is_var(element) && result[i]->type == INT_TYPE)
	c2p_int(result[i]->val->i_val, element);
      else if (is_var(element) && result[i]->type == FLOAT_TYPE)
	c2p_float(result[i]->val->f_val, element);
      returnList = p2p_cdr(returnList);
      i++;
    }
    rFlag = RESULT_NONEMPTY_OR_NOT_REQUESTED;
  }

  if (result != NULL && qHandle->numResultCols > i) {
    errorMesg = "XSB_DBI ERROR: Number of requested columns is less than the number of returned columns";
    errorNumber = "XSB_DBI_013";    
    rFlag = TOO_FEW_RETURN_COLS;
    return rFlag;
  }

  return rFlag;  
}


static void freeConnectionHandle(struct xsb_connectionHandle* cHandle, int pos)
{
  int j;
  
  /*free(cHandle->handle);
  free(cHandle->driver);
  if (cHandle->server == NULL)
    free(cHandle->dsn);
  else {
    free(cHandle->server);
    free(cHandle->database);
  }
  free(cHandle->user);
  free(cHandle->password);*/
  free(cHandle);
  for (j = pos + 1 ; j < numCHandles ; j++)
    CHandles[j-1] = CHandles[j];
  CHandles[numCHandles-1] = NULL;
  numCHandles--;
}


static int closeQueryHandle(char* queryHandle)
{
  int (*closeStmtDriver)(struct xsb_queryHandle*);
  char* (*errorMesgDriver)();
  char* driverName;  
  int i, result;
  
  for (i = 0 ; i < numQHandles ; i++) {
    if (!strcmp(QHandles[i]->handle, queryHandle)) {
      driverName = QHandles[i]->connHandle->driver;
      
      if (getDriverFunction(driverName, CLOSE_STMT) != NULL)
	closeStmtDriver = getDriverFunction(driverName, CLOSE_STMT)->closeStmtDriver;
      else
	return FALSE;
      
      result = closeStmtDriver(QHandles[i]);
      if (result == FAILURE) {
	if (getDriverFunction(driverName, ERROR_MESG) != NULL)
	  errorMesgDriver = getDriverFunction(driverName, ERROR_MESG)->errorMesgDriver;
	else
	  return FALSE;

	errorMesg = errorMesgDriver();
	return FALSE;
      }
      freeQueryHandle(QHandles[i], i);
    }
  }
  
  return TRUE;
}


static void freeQueryHandle(struct xsb_queryHandle* qHandle, int pos)
{
  int j;
  
  //free(qHandle->handle);
  free(qHandle->query);
  free(qHandle);
  for (j = pos + 1 ; j < numQHandles ; j++)
    QHandles[j-1] = QHandles[j];
  QHandles[numQHandles-1] = NULL;
  numQHandles--;  
}


static struct xsb_connectionHandle* isConnectionHandle(char* handle)
{
  int i;
  for (i = 0 ; i < numCHandles ; i++)
    if (!strcmp(CHandles[i]->handle, handle))
      return CHandles[i];
  return NULL;
}


static struct xsb_queryHandle* isQueryHandle(char* handle)
{
  int i;
  for (i = 0 ; i < numQHandles ; i++)
    if (!strcmp(QHandles[i]->handle, handle))
      return QHandles[i];
  return NULL;
}


DllExport int call_conv registerXSBDriver(char* drivername, int num)
{
  struct driver* dr;
  int i;

  for (i = 0 ; i < numDrivers ; i++) {
    if (!strcmp(DBdrivers[i]->driver, drivername)) {
      errorMesg = "XSB_DBI ERROR: driver already registered";
      errorNumber = "XSB_DBI_001";
      return -1;
    }
  }
  dr = (struct driver *)malloc(sizeof(struct driver));
  dr->driver = drivername;
  dr->numberFunctions = num;
  dr->functions =
    (struct driverFunction **)malloc(num * sizeof(struct driverFunction *));
  for (i = 0 ; i < num ; i++)
    dr->functions[i] = NULL;

  DBdrivers[numDrivers++] = dr;
  return 0;
}


DllExport int call_conv registerXSBFunction(char* drivername, int type, union functionPtrs* func)
{
  int i, j;

  for (i = 0 ; i < numDrivers ; i++) {
    if (!strcmp(DBdrivers[i]->driver, drivername)) {
      for (j = 0 ; j < DBdrivers[i]->numberFunctions ; j++) {
	if (DBdrivers[i]->functions[j] == NULL) {
	  DBdrivers[i]->functions[j] =
	    (struct driverFunction *)malloc(sizeof(struct driverFunction));
	  DBdrivers[i]->functions[j]->functionType = type;
	  DBdrivers[i]->functions[j]->functionName = func;
	  break;
	}
      }
    }
  }

  return 0;
}


static union functionPtrs* getDriverFunction(char* drivername, int type)
{
  int i, j;

  for (i = 0 ; i < numDrivers ; i++) {
    if (!strcmp(DBdrivers[i]->driver, drivername)) {
      for (j = 0 ; j < DBdrivers[i]->numberFunctions ; j++) {
	if (DBdrivers[i]->functions[j]->functionType == type) {
	  return DBdrivers[i]->functions[j]->functionName;
	}
      }
      errorMesg = "XSB_DBI ERROR: Function does not exist in this driver";
      errorNumber = "XSB_DBI_003";
      return NULL;
    }
  }
  errorMesg = "XSB_DBI ERROR: Driver does not exist";
  errorNumber = "XSB_DBI_002";
  return NULL;
}
