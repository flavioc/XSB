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
//static char* parseTerm(prolog_term element);
static union functionPtrs* getDriverFunction(char* driver, int type);
static int bindReturnList(prolog_term returnList, struct xsb_data** result, struct xsb_queryHandle*);

struct xsb_connectionHandle* CHandles[MAX_CONNECTIONS];
struct xsb_queryHandle* QHandles[MAX_QUERIES];
struct driver* DBdrivers[MAX_DRIVERS];
int numDrivers, numCHandles, numQHandles;
char* errorMesg;
char* errorNumber;

//DllExport void call_conv write_canonical_term(Cell prolog_term);
//extern char* wcan_string;
//extern int wcan_disp;


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
  cHandle->handle = (char *)malloc((strlen(handle) + 1) * sizeof(char));
  strcpy(cHandle->handle, handle);
  cHandle->driver = (char *)malloc((strlen(driver) + 1) * sizeof(char));
  strcpy(cHandle->driver, driver);
  if (strlen(server) == 0) {
    cHandle->dsn = (char *)malloc((strlen(dsn) + 1) * sizeof(char));
    strcpy(cHandle->dsn, dsn);
    cHandle->server = NULL;
    cHandle->database = NULL;
  }
  else {
    cHandle->server = (char *)malloc((strlen(server) + 1) * sizeof(char));
    strcpy(cHandle->server, server);
    cHandle->database = (char *)malloc((strlen(database) + 1) * sizeof(char));
    strcpy(cHandle->database, database);
    cHandle->dsn = NULL;
  }
  cHandle->user = (char *)malloc((strlen(user) + 1) * sizeof(char));
  strcpy(cHandle->user, user);
  cHandle->password = (char *)malloc((strlen(password) + 1) * sizeof(char));
  strcpy(cHandle->password, password);

  if ((val = connectDriver(cHandle)) == SUCCESS)
    CHandles[numCHandles++] = cHandle;
  else {
    if (getDriverFunction(cHandle->driver, ERROR_MESG) != NULL)
      errorMesgDriver = getDriverFunction(cHandle->driver, ERROR_MESG)->errorMesgDriver;
    else
      return FALSE;

    errorMesg = errorMesgDriver();
    errorNumber = "XSB_DBI_000";
    free(cHandle->handle);
    free(cHandle->driver);
    if (strlen(server) == 0)
      free(cHandle->dsn);
    else {
      free(cHandle->server);
      free(cHandle->database);
    }
    free(cHandle->user);
    free(cHandle->password);
    free(cHandle);
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
  int val, i, j, k;

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
	  free(QHandles[j]->handle);
	  free(QHandles[j]->query);
	  free(QHandles[j]);
	  for (k = j + 1 ; k < numQHandles ; k++)
	    QHandles[k-1] = QHandles[k];
	  QHandles[numQHandles-1] = NULL;
	  numQHandles--;
	  break;
	}
      }

      free(CHandles[i]->handle);
      free(CHandles[i]->driver);
      if (CHandles[i]->server == NULL)
	free(CHandles[i]->dsn);
      else {
	free(CHandles[i]->server);
	free(CHandles[i]->database);
      }
      free(CHandles[i]->user);
      free(CHandles[i]->password);
      free(CHandles[i]);
      for (j = i + 1 ; j < numCHandles ; j++)
	CHandles[j-1] = CHandles[j];
      CHandles[numCHandles-1] = NULL;
      numCHandles--;
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
  int i, j, val;

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
      for (i = 0 ; i < numQHandles ; i++) {
	if (!strcmp(QHandles[i]->handle, qhandle)) {
	  free(QHandles[i]->handle);
	  free(QHandles[i]->query);
	  free(QHandles[i]);
	  for (j = i + 1 ; j < numQHandles ; j++)
	    QHandles[j-1] = QHandles[j];
	  QHandles[numQHandles-1] = NULL;
	  numQHandles--;
	  break;
	}
      }
    }
  }
  else if ((cHandle = isConnectionHandle(chandle)) != NULL) {
    sqlQuery = buildSQLQuery(sqlQueryList);
    qHandle = (struct xsb_queryHandle *)malloc(sizeof(struct xsb_queryHandle));
    qHandle->handle = (char *)malloc((strlen(qhandle) + 1) * sizeof(char));
    strcpy(qHandle->handle, qhandle);
    qHandle->connHandle = cHandle;
    qHandle->query = (char *)malloc((strlen(sqlQuery) + 1) * sizeof(char));
    strcpy(qHandle->query, sqlQuery);
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

  val = bindReturnList(returnList, result, qHandle);
  if (val == TOO_MANY_RETURN_COLS) {
    return FALSE;
  }

  if ((cHandle = isConnectionHandle(chandle)) != NULL) {
    if (getDriverFunction(cHandle->driver, ERROR_MESG) != NULL)
      errorMesgDriver =
	getDriverFunction(cHandle->driver, ERROR_MESG)->errorMesgDriver;
    else {
      return FALSE;
    }
	  
    errorMesg = errorMesgDriver();
    errorNumber = "XSB_DBI_000";
  }

  if (errorMesg == NULL && val == RESULT_NONEMPTY_OR_NOT_REQUESTED)
    return TRUE;
  else {
    return FALSE;
  }
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
  qHandle->query = (char *)malloc((strlen(sqlQuery) + 1) * sizeof(char));
  strcpy(qHandle->query, sqlQuery);
  qHandle->handle = (char *)malloc((strlen(qhandle) + 1) * sizeof(char));
  strcpy(qHandle->handle, qhandle);
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
  double temp_float;
  int i, temp_int, val;

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
	bindValues[i]->val->str_val =
	  (char *)malloc((strlen(p2c_string(element)) + 1) * sizeof(char));
	strcpy(bindValues[i]->val->str_val, p2c_string(element));
      }
      else if (is_int(element)) {
	bindValues[i]->type = INT_TYPE;
	bindValues[i]->val = (union xsb_value *)malloc(sizeof(union xsb_value));
	bindValues[i]->val->i_val = (int *)malloc(sizeof(int));
	temp_int = p2c_int(element);
	bindValues[i]->val->i_val = &temp_int;
      }
      else if (is_float(element)) {
	bindValues[i]->type = FLOAT_TYPE;
	bindValues[i]->val = (union xsb_value *)malloc(sizeof(union xsb_value));
	bindValues[i]->val->f_val = (double *)malloc(sizeof(double));
	temp_float = p2c_float(element);
	bindValues[i]->val->f_val = &temp_float;
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
  else if (result == NULL) {
    qHandle->state = QUERY_BEGIN;
  }
  
  val = bindReturnList(returnList, result, qHandle);
  if (val == TOO_MANY_RETURN_COLS)
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
  int (*closeStmtDriver)(struct xsb_queryHandle*);
  char* (*errorMesgDriver)();
  char* queryHandle;
  char* driverName;
  int result;
  int i, j;
  
  queryHandle = ptoc_string(1);
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
      free(QHandles[i]->query);
      free(QHandles[i]->handle);
      free(QHandles[i]);
      for (j = i + 1 ; j < numQHandles ; j++)
	QHandles[j-1] = QHandles[j];
      numQHandles--;
    }
  }

  return TRUE;
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
  char* t1;
  int i, cnt;

  sqlQuery = (char *)malloc(QUERY_SIZE * sizeof(char));
  sqlQuery[0] = '\0';
  while (!is_nil(sqlQueryList)) {
    element = p2p_car(sqlQueryList);
    sqlQueryList = p2p_cdr(sqlQueryList);
    if (is_string(element)) {
      t1 = (char *)malloc(ELEMENT_SIZE * sizeof(char));
      sprintf(t1, "%s", p2c_string(element));
      if (t1[0] == TERM_CHAR) {
	cnt = 0;
	temp = (char *)malloc(ELEMENT_SIZE * sizeof(char));
	temp[cnt++] = '\'';
	/* protect inner quotes in Prolog terms */
	for (i = 0 ; i < strlen(t1) ; i++) {
	  if (t1[i] == '\'') {
	    temp[cnt++] = '\\';
	    temp[cnt++] = t1[i];
	  }
	  else {
	    temp[cnt++] = t1[i];
	  }
	}
	temp[cnt++] = '\'';
	strcat(sqlQuery, temp);
      }
      else {
	strcat(sqlQuery, t1);            
      }
    }
    else if (is_int(element)) {
      temp = (char *)malloc(ELEMENT_SIZE * sizeof(char));
      sprintf(temp, "%d", p2c_int(element));
      strcat(sqlQuery, temp);
    }
    else if (is_float(element)) {
      temp = (char *)malloc(ELEMENT_SIZE * sizeof(char));
      sprintf(temp, "%f", p2c_float(element));			
      strcat(sqlQuery, temp);
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
    rFlag = 2;
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
	  }
	  else {
	    c2p_string(result[i]->val->str_val, element);	    
	  }
	}
      }
      else if (is_var(element) && result[i]->type == INT_TYPE)
	c2p_int(*(result[i]->val->i_val), element);
      else if (is_var(element) && result[i]->type == FLOAT_TYPE)
	c2p_float(*(result[i]->val->f_val), element);
      returnList = p2p_cdr(returnList);
      i++;
    }
    rFlag = RESULT_NONEMPTY_OR_NOT_REQUESTED;
  }

  return rFlag;  
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
