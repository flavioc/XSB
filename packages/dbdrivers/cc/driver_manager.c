/* File: driver_manager.c
** Author: Saikat Mukherjee
** Contact: saikat@cs.sunysb.edu

** This file is the  middle layer for the interface to the drivers. 
** This gets called from lib/db_interface.P and in turn calls the
** driver specific files which are in the packages/dbdrivers/ directory.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN_NT
#define XSB_DLL
#endif

#include "cinterf.h"
#include "driver_manager_defs.h"

static struct xsb_connectionHandle* isConnectionHandle(char* handle);
static struct xsb_queryHandle* isQueryHandle(char* handle);
static char* buildSQLQuery(prolog_term sqlQueryList);
static union functionPtrs* getDriverFunction(char* driver, int type);

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
	char *handle, *driver, *server, *database, *user, *password, *dsn;
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

	if (isConnectionHandle(handle) != NULL)
	{
		errorMesg = "XSB_DBI ERROR: Connection handle already exists";
		errorNumber = "XSB_DBI_006";
		return FALSE;
	}

	connectDriver = getDriverFunction(driver, CONNECT)->connectDriver;

	cHandle = (struct xsb_connectionHandle *)malloc(sizeof(struct xsb_connectionHandle));
	cHandle->handle = (char *)malloc((strlen(handle) + 1) * sizeof(char));
	strcpy(cHandle->handle, handle);
	cHandle->driver = (char *)malloc((strlen(driver) + 1) * sizeof(char));
	strcpy(cHandle->driver, driver);
	if (strlen(server) == 0)
	{
		cHandle->dsn = (char *)malloc((strlen(dsn) + 1) * sizeof(char));
		strcpy(cHandle->dsn, dsn);
		cHandle->server = NULL;
	}
	else
	{
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
	else
	{
		errorMesgDriver = getDriverFunction(cHandle->driver, ERROR_MESG)->errorMesgDriver;
		errorMesg = errorMesgDriver();
		free(cHandle->handle);
		free(cHandle->driver);
		if (strlen(server) == 0)
			free(cHandle->dsn);
		else
		{
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
	int val, i, j;

	handle = ptoc_string(1);

	for (i = 0 ; i < numCHandles ; i++)
	{
		if (!strcmp(CHandles[i]->handle, handle))
		{
			disconnectDriver = getDriverFunction(CHandles[i]->driver, DISCONNECT)->disconnectDriver;
			val = disconnectDriver(CHandles[i]);

			if(val == FAILURE)
			{
				errorMesgDriver = getDriverFunction(CHandles[i]->driver, ERROR_MESG)->errorMesgDriver;
				errorMesg = errorMesgDriver();
				return FALSE;
			}

			for (j = 0 ; j < numQHandles ; j++)
			{
				if (!strcmp(QHandles[j]->connHandle->handle, handle))
				{
					closeStmtDriver = getDriverFunction(CHandles[i]->driver, ERROR_MESG)->closeStmtDriver;
					val = closeStmtDriver(QHandles[j]);
					if (val == FAILURE)
					{
						errorMesgDriver = getDriverFunction(CHandles[i]->driver, ERROR_MESG)->errorMesgDriver;
						errorMesg = errorMesgDriver();
						return FALSE;
					}
					free(QHandles[j]->handle);
					free(QHandles[j]->query);
					free(QHandles[j]);
				}
			}

			free(CHandles[i]->handle);
			free(CHandles[i]->driver);
			if (CHandles[i]->server == NULL)
				free(CHandles[i]->dsn);
			else
			{
				free(CHandles[i]->server);
				free(CHandles[i]->database);
			}
			free(CHandles[i]->user);
			free(CHandles[i]->password);
			free(CHandles[i]);
			for (j = i + 1 ; j < numCHandles ; j++)
				CHandles[j-1] = CHandles[j];
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
	prolog_term returnList, sqlQueryList, element;
	struct xsb_connectionHandle* cHandle;
	struct xsb_queryHandle* qHandle;
	struct xsb_data** result;
	char *chandle, *qhandle, *sqlQuery;
	int i, j;

	chandle = ptoc_string(1);
	qhandle = ptoc_string(2);
	sqlQueryList = reg_term(3);
	returnList = reg_term(4);

	result = NULL;

	if ((qHandle = isQueryHandle(qhandle)) != NULL)
	{
		if (strcmp(qHandle->connHandle->handle, chandle))
		{
			errorMesg = "XSB_DBI ERROR: Query handle already exists";
			errorNumber = "XSB_DBI_007";;
			return FALSE;
		}
		queryDriver = getDriverFunction(qHandle->connHandle->driver, QUERY)->queryDriver;
		result = queryDriver(qHandle);
		if (result == NULL && qHandle->state == QUERY_RETRIEVE)
		{
			for (i = 0 ; i < numQHandles ; i++)
			{
				if (!strcmp(QHandles[i]->handle, qhandle))
				{
					free(QHandles[i]->handle);
					free(QHandles[i]->query);
					free(QHandles[i]);
					for (j = i + 1 ; j < numQHandles ; j++)
					{
						QHandles[j-1] = QHandles[j];
					}
					numQHandles--;
					break;
				}
			}
		}
	}
	else if ((cHandle = isConnectionHandle(chandle)) != NULL)
	{
		sqlQuery = buildSQLQuery(sqlQueryList);
		qHandle = (struct xsb_queryHandle *)malloc(sizeof(struct xsb_queryHandle));
		qHandle->handle = (char *)malloc((strlen(qhandle) + 1) * sizeof(char));
		strcpy(qHandle->handle, qhandle);
		qHandle->connHandle = cHandle;
		qHandle->query = (char *)malloc((strlen(sqlQuery) + 1) * sizeof(char));
		strcpy(qHandle->query, sqlQuery);
		qHandle->state = QUERY_BEGIN;
		QHandles[numQHandles++] = qHandle;
		queryDriver = getDriverFunction(cHandle->driver, QUERY)->queryDriver;
		result = queryDriver(qHandle);
	}
	else
	{
		errorMesg = "XSB_DBI ERROR: Connection handle does not exist";
		errorNumber = "XSB_DBI_004";
		return FALSE;		
	}

	i = 0;
	while (!is_nil(returnList))
	{
		element = p2p_car(returnList);
		if (result == NULL)
			c2p_nil(element);
		else if (is_var(element) && result[i]->type == STRING_TYPE)
			c2p_string(result[i]->val->str_val, element);
		else if (is_var(element) && result[i]->type == INT_TYPE)
			c2p_int(*(result[i]->val->i_val), element);
		else if (is_var(element) && result[i]->type == FLOAT_TYPE)
			c2p_float(*(result[i]->val->f_val), element);
		returnList = p2p_cdr(returnList);
		i++;
	}

	if (result != NULL)
		return TRUE;
	if ((cHandle = isConnectionHandle(chandle)) != NULL)
	{
		errorMesgDriver = getDriverFunction(cHandle->driver, ERROR_MESG)->errorMesgDriver;
		errorMesg = errorMesgDriver();
	}
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
	
	if ((cHandle = isConnectionHandle(chandle)) == NULL)
	{
		errorMesg = "XSB_DBI ERROR: Connection handle does not exist";
		errorNumber = "XSB_DBI_004";
		return FALSE;
	}
	
	if ((qHandle = isQueryHandle(qhandle)) != NULL)
	{
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

	prepareStmtDriver = getDriverFunction(cHandle->driver, PREPARE)->prepareStmtDriver;
	if ((val = prepareStmtDriver(qHandle)) != -1)
	{
		qHandle->numParams = val;
		QHandles[numQHandles++] = qHandle;
	}
	else
	{
		errorMesgDriver = getDriverFunction(cHandle->driver, ERROR_MESG)->errorMesgDriver;
		errorMesg = errorMesgDriver();
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
	struct xsb_data** bindValues;
	struct xsb_data** result;
	prolog_term bindList, returnList, element;
	char *queryHandle;
	int temp_int;
	double temp_float;
	int i;

	queryHandle = ptoc_string(1);
	bindList = reg_term(2);
	returnList = reg_term(3);

	if ((qHandle = isQueryHandle(queryHandle)) == NULL)
	{
		errorMesg = "XSB_DBI ERROR: Query handle does not exist";
		errorNumber = "XSB_DBI_005";
		return FALSE;
	}

	if (qHandle->state == QUERY_BEGIN)
	{
		bindValues = (struct xsb_data **)malloc(qHandle->numParams * sizeof(struct xsb_data *));
		for (i = 0 ; i < qHandle->numParams ; i++)
		{
			bindValues[i] = (struct xsb_data *)malloc(sizeof(struct xsb_data));
			if (is_nil(bindList))
			{
				errorMesg = "XSB_DBI ERROR: Not all paremeters supplied";
				errorNumber = "XSB_DBI_008";
				return FALSE;
			}
			element = p2p_car(bindList);
			if (is_string(element))
			{
				bindValues[i]->type = STRING_TYPE;
				bindValues[i]->length = strlen(p2c_string(element));
				bindValues[i]->val = (union xsb_value *)malloc(sizeof(union xsb_value));
				bindValues[i]->val->str_val = (char *)malloc((strlen(p2c_string(element)) + 1) * sizeof(char));
				strcpy(bindValues[i]->val->str_val, p2c_string(element));
			}
			else if (is_int(element))
			{
				bindValues[i]->type = INT_TYPE;
				bindValues[i]->val = (union xsb_value *)malloc(sizeof(union xsb_value));
				bindValues[i]->val->i_val = (int *)malloc(sizeof(int));
				temp_int = p2c_int(element);
				bindValues[i]->val->i_val = &temp_int;
			}
			else if (is_float(element))
			{
				bindValues[i]->type = FLOAT_TYPE;
				bindValues[i]->val = (union xsb_value *)malloc(sizeof(union xsb_value));
				bindValues[i]->val->f_val = (double *)malloc(sizeof(double));
				temp_float = p2c_float(element);
				bindValues[i]->val->f_val = &temp_float;
			}
			else if (is_var(element))
			{
				errorMesg = "XSB_DBI ERROR: Unbound variable in parameter list";
				errorNumber = "XSB_DBI_009";
				return FALSE;
			}
			bindList = p2p_cdr(bindList);
		}
	}
	
	executeStmtDriver = getDriverFunction(qHandle->connHandle->driver, EXEC_PREPARE)->executeStmtDriver;
	if ((result = executeStmtDriver(bindValues, qHandle)) == NULL)
		qHandle->state = QUERY_BEGIN;	

	i = 0;
	while (!is_nil(returnList))
	{
		element = p2p_car(returnList);
		if (result == NULL)
			c2p_nil(element);
		else if (is_var(element) && result[i]->type == STRING_TYPE)
			c2p_string(result[i]->val->str_val, element);
		else if (is_var(element) && result[i]->type == INT_TYPE)
			c2p_int(*(result[i]->val->i_val), element);
		else if (is_var(element) && result[i]->type == FLOAT_TYPE)
			c2p_float(*(result[i]->val->f_val), element);
		returnList = p2p_cdr(returnList);
		i++;
	}
	
	if (result == NULL)
	{
		errorMesgDriver = getDriverFunction(qHandle->connHandle->driver, ERROR_MESG)->errorMesgDriver;
		errorMesg = errorMesgDriver();
		return FALSE;
	}
	
	return TRUE;
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
	for (i = 0 ; i < numQHandles ; i++)
	{
		if (!strcmp(QHandles[i]->handle, queryHandle))
		{
			driverName = QHandles[i]->connHandle->driver;
			closeStmtDriver = getDriverFunction(driverName, CLOSE_STMT)->closeStmtDriver;
			result = closeStmtDriver(QHandles[i]);
			if (result == FAILURE)
			{
				errorMesgDriver = getDriverFunction(driverName, ERROR_MESG)->errorMesgDriver;
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
	if (is_var(message) && errorMesg != NULL)
	{
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
	char* temp;
	char* sqlQuery;

	sqlQuery = (char *)malloc(100 * sizeof(char));
	sqlQuery[0] = '\0';
	while (!is_nil(sqlQueryList))
	{
		element = p2p_car(sqlQueryList);
		if (is_string(element))		
			strcat(sqlQuery, p2c_string(element));
		else if (is_int(element))
		{
			temp = (char *)malloc(100 * sizeof(char));
			sprintf(temp, "%d", p2c_int(element));
			strcat(sqlQuery, temp);
			free(temp);
		}
		else if (is_float(element))
		{
			temp = (char *)malloc(100 * sizeof(char));
			sprintf(temp, "%f", p2c_float(element));
			strcat(sqlQuery, temp);
			free(temp);
		}
		else if (is_var(element))
		{
			errorMesg = "XSB_DBI ERROR: Unbound variable in parameter list";
			errorNumber = "XSB_DBI_009";
			return NULL;
		}
		sqlQueryList = p2p_cdr(sqlQueryList);
	}

	return sqlQuery;
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

	for (i = 0 ; i < numDrivers ; i++)
	{
		if (!strcmp(DBdrivers[i]->driver, drivername))
		{
			errorMesg = "XSB_DBI ERROR: driver already registered";
			errorNumber = "XSB_DBI_001";
			return -1;
		}
	}
	dr = (struct driver *)malloc(sizeof(struct driver));
	dr->driver = drivername;
	dr->numberFunctions = num;
	dr->functions = (struct driverFunction **)malloc(num * sizeof(struct driverFunction *));
	for (i = 0 ; i < num ; i++)
		dr->functions[i] = NULL;

	DBdrivers[numDrivers++] = dr;
	return 0;
}


DllExport int call_conv registerXSBFunction(char* drivername, int type, union functionPtrs* func)
{
	int i, j;

	for (i = 0 ; i < numDrivers ; i++)
	{
		if (!strcmp(DBdrivers[i]->driver, drivername))
		{
			for (j = 0 ; j < DBdrivers[i]->numberFunctions ; j++)
			{
				if (DBdrivers[i]->functions[j] == NULL)
				{
					DBdrivers[i]->functions[j] = (struct driverFunction *)malloc(sizeof(struct driverFunction));
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

	for (i = 0 ; i < numDrivers ; i++)
	{
		if (!strcmp(DBdrivers[i]->driver, drivername))
		{
			for (j = 0 ; j < DBdrivers[i]->numberFunctions ; j++)
			{
				if (DBdrivers[i]->functions[j]->functionType == type)
					return DBdrivers[i]->functions[j]->functionName;
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


