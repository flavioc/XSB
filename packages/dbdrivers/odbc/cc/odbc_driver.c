/* File: odbc_driver.c
** Author: Saikat Mukherjee
** Contact: saikat@cs.sunysb.edu

** This is the ODBC driver for connecting to a database.
*/

#ifdef WIN_NT
#include <windows.h>
#endif

#ifdef WIN_NT
#define XSB_DLL
#endif

#include "odbc_driver_defs.h"

static int driverODBC_getXSBType(SQLSMALLINT dataType);
static void driverODBC_error(SQLSMALLINT handleType, SQLHANDLE handle);
static struct xsb_data** driverODBC_getNextRow(struct driverODBC_queryInfo* query, int direct);

struct driverODBC_connectionInfo* odbcHandles[MAX_HANDLES];
struct driverODBC_queryInfo* odbcQueries[MAX_QUERIES];
int numHandles, numQueries;
SQLCHAR* errorMesg;
SQLHENV henv;

DllExport int call_conv driverODBC_initialise()
{
	SQLRETURN val;

	numHandles = numQueries = 0;
	errorMesg = NULL;
	val = SQLAllocEnv(&henv);

	return TRUE;
}


DllExport int call_conv driverODBC_connect(struct xsb_connectionHandle* handle)
{
	struct driverODBC_connectionInfo* odbcHandle;
	SQLRETURN val;

	odbcHandle = (struct driverODBC_connectionInfo *)malloc(sizeof(struct driverODBC_connectionInfo));

	val = SQLAllocConnect(henv, &(odbcHandle->hdbc));
	if (val != SQL_SUCCESS)
	{
		driverODBC_error(SQL_HANDLE_ENV, henv);
		val = SQLFreeHandle(SQL_HANDLE_ENV, henv);
		if (val != SQL_SUCCESS)
			driverODBC_error(SQL_HANDLE_ENV, henv);
		free(odbcHandle);
		return FAILURE;
	}
	
	val = SQLConnect(odbcHandle->hdbc, handle->dsn, SQL_NTS, handle->user, SQL_NTS, handle->password, SQL_NTS);
	if (val != SQL_SUCCESS)
	{
		driverODBC_error(SQL_HANDLE_DBC, odbcHandle->hdbc);
		val = SQLFreeHandle(SQL_HANDLE_DBC, odbcHandle->hdbc);
		if (val != SQL_SUCCESS)
			driverODBC_error(SQL_HANDLE_DBC, odbcHandle->hdbc);
		val = SQLFreeHandle(SQL_HANDLE_ENV, henv);
		if (val != SQL_SUCCESS)
			driverODBC_error(SQL_HANDLE_ENV, henv);
		free(odbcHandle);
		return FAILURE;
	}
	
	odbcHandle->handle = (char *)malloc((strlen(handle->handle) + 1) * sizeof(char));
	strcpy(odbcHandle->handle, handle->handle);
	odbcHandles[numHandles++] = odbcHandle;
	
	return SUCCESS;
}


DllExport int call_conv driverODBC_disconnect(struct xsb_connectionHandle* handle)
{
	SQLRETURN val;
	int i, j;

	for (i = 0 ; i < numHandles ; i++)
	{
		if (!strcmp(odbcHandles[i]->handle, handle->handle))
		{
			val = SQLDisconnect(odbcHandles[i]->hdbc);
			if (val != SQL_SUCCESS)
			{
				driverODBC_error(SQL_HANDLE_DBC, odbcHandles[i]->hdbc);
				return FAILURE;
			}
	
			val = SQLFreeHandle(SQL_HANDLE_DBC, odbcHandles[i]->hdbc);
			if (val != SQL_SUCCESS)
			{
				driverODBC_error(SQL_HANDLE_DBC, odbcHandles[i]->hdbc);
				return FAILURE;
			}
			
			val = SQLFreeHandle(SQL_HANDLE_ENV, henv);
			if (val != SQL_SUCCESS)
			{
				driverODBC_error(SQL_HANDLE_ENV, henv);
				return FAILURE;
			}

			free(odbcHandles[i]->handle);			
			free(odbcHandles[i]);
			
			for (j = i + 1 ; j < numHandles ; j++)
				odbcHandles[j-1] = odbcHandles[j];
			numHandles--;
			break;
		}
	}
	
	return SUCCESS;
}


DllExport struct xsb_data** call_conv driverODBC_query(struct xsb_queryHandle* handle)
{
	struct driverODBC_queryInfo* query;
	SQLHDBC hdbc;
	SQLRETURN val;
	int i;

	query = NULL;
	hdbc = NULL;
	if (handle->state == QUERY_RETRIEVE)
	{
		for (i = 0 ; i < numQueries ; i++)
		{
			if (!strcmp(odbcQueries[i]->handle, handle->handle))
			{
				query = odbcQueries[i];
				break;
			}
		}
	}
	else if (handle->state == QUERY_BEGIN)
	{
		for (i = 0 ; i < numHandles ; i++)
		{
			if (!strcmp(odbcHandles[i]->handle, handle->connHandle->handle))
			{
				hdbc = odbcHandles[i]->hdbc;
				break;
			}
		}
	
		query = (struct driverODBC_queryInfo *)malloc(sizeof(struct driverODBC_queryInfo));
		query->handle = (char *)malloc((strlen(handle->handle) + 1) * sizeof(char));
		strcpy(query->handle, handle->handle);
		query->query = (char *)malloc((strlen(handle->query) + 1) * sizeof(char));
		strcpy(query->query, handle->query);

		val = SQLAllocStmt(hdbc, &(query->hstmt));
		if (val != SQL_SUCCESS)
		{
			driverODBC_error(SQL_HANDLE_DBC, hdbc);
			return NULL;
		}
		
		val = SQLExecDirect(query->hstmt, handle->query, SQL_NTS);
		if (val != SQL_SUCCESS)
		{
			driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
			return NULL;
		}

		query->resultmeta = (struct driverODBC_meta *)malloc(sizeof(struct driverODBC_meta));
		val = SQLNumResultCols(query->hstmt, (SQLSMALLINT *)(&(query->resultmeta->numCols)));
		if (val != SQL_SUCCESS)
		{
			driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
			return NULL;
		}
		query->resultmeta->types = (struct driverODBC_columnmeta **)malloc(query->resultmeta->numCols * sizeof(struct driverODBC_columnmeta *));
		for (i = 0 ; i < query->resultmeta->numCols ; i++)
		{
			query->resultmeta->types[i] = (struct driverODBC_columnmeta *)malloc(sizeof(struct driverODBC_columnmeta));
			val = SQLColAttribute(query->hstmt, i + 1, SQL_COLUMN_TYPE, NULL, 0, NULL, &(query->resultmeta->types[i]->type));
			if (val != SQL_SUCCESS)
			{
				driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
				return NULL;
			}
			val = SQLColAttribute(query->hstmt, i + 1, SQL_COLUMN_LENGTH, NULL, 0, NULL, &(query->resultmeta->types[i]->length));
			if (val != SQL_SUCCESS)
			{
				driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
				return NULL;
			}
		}
	
		handle->state = QUERY_RETRIEVE;
		odbcQueries[numQueries++] = query;
	}
	
	return driverODBC_getNextRow(query, 1);
}


static struct xsb_data** driverODBC_getNextRow(struct driverODBC_queryInfo* query, int direct)
{
	struct xsb_data** result;
	SQLRETURN val;
	int i, j;

	result = (struct xsb_data **)malloc(query->resultmeta->numCols * sizeof(struct xsb_data *));
	for (i = 0 ; i < query->resultmeta->numCols ; i++)
	{
		result[i] = (struct xsb_data *)malloc(sizeof(struct xsb_data));
		result[i]->val = (union xsb_value *)malloc(sizeof(union xsb_value));
		if (driverODBC_getXSBType(query->resultmeta->types[i]->type) == STRING_TYPE)
		{
			result[i]->type = STRING_TYPE;
			result[i]->length = query->resultmeta->types[i]->length + 1;
			result[i]->val->str_val = (char *)malloc(result[i]->length * sizeof(char));
			val = SQLBindCol(query->hstmt, i + 1, SQL_C_CHAR, (SQLPOINTER *)result[i]->val->str_val, (SQLINTEGER)result[i]->length, NULL);
			if (val != SQL_SUCCESS)
			{
				driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
				return NULL;
			}
		}
		else if (driverODBC_getXSBType(query->resultmeta->types[i]->type) == INT_TYPE)
		{
			result[i]->type = INT_TYPE;
			result[i]->val->i_val = (int *)malloc(sizeof(int));
			val = SQLBindCol(query->hstmt, i + 1, SQL_C_SLONG, (SQLPOINTER *)result[i]->val->i_val, 0, NULL);
			if (val != SQL_SUCCESS)
			{
				driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
				return NULL;
			}
		}
		else if (driverODBC_getXSBType(query->resultmeta->types[i]->type) == FLOAT_TYPE)
		{
			result[i]->type = FLOAT_TYPE;
			result[i]->val->f_val = (double *)malloc(sizeof(double));
			val = SQLBindCol(query->hstmt, i + 1, SQL_C_DOUBLE, (SQLPOINTER *)result[i]->val->f_val, 0, NULL);
			if (val != SQL_SUCCESS)
			{
				driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
				return NULL;
			}
		}
	}

	val = SQLFetch(query->hstmt);
	if (val != SQL_SUCCESS)
	{
		if (direct == 1)
		{
			for (i = 0 ; i < query->resultmeta->numCols ; i++)
				free(query->resultmeta->types[i]);
			free(query->resultmeta->types);
			free(query->resultmeta);
			val = SQLFreeHandle(SQL_HANDLE_STMT, query->hstmt);
			if (val != SQL_SUCCESS)
				driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
			for (i = 0 ; i < numQueries ; i++)
			{
				if (!strcmp(query->query, odbcQueries[i]->query))
				{
					for (j = i + 1 ; j < numQueries ; j++)
						odbcQueries[j-1] = odbcQueries[j];
					break;
				}
			}
			free(query->handle);
			free(query->query);
			free(query);
			numQueries--;
		}
		else
		{
			val = SQLFreeStmt(query->hstmt, SQL_CLOSE);
			if (val != SQL_SUCCESS)
				driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
		}
		return NULL;
	}
	if (val != SQL_SUCCESS && val != SQL_NO_DATA)
	{
		driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
		return NULL;
	}	
	
	return result;
}


DllExport int call_conv driverODBC_prepareStatement(struct xsb_queryHandle* qHandle)
{
	struct driverODBC_queryInfo* query;
	SQLRETURN val;
	SQLHDBC hdbc;
	int i;
	
	hdbc = NULL;

	query = (struct driverODBC_queryInfo *)malloc(sizeof(struct driverODBC_queryInfo));
	query->handle = (char *)malloc((strlen(qHandle->handle) + 1) * sizeof(char));
	strcpy(query->handle, qHandle->handle);
	query->query = (char *)malloc((strlen(qHandle->query) + 1) * sizeof(char));
	strcpy(query->query, qHandle->query);

	for (i = 0 ; i < numHandles ; i++)
	{
		if (!strcmp(qHandle->connHandle->handle, odbcHandles[i]->handle))
		{
			hdbc = odbcHandles[i]->hdbc;
			break;
		}
	}
	
	val = SQLAllocStmt(hdbc, &(query->hstmt));
	if (val != SQL_SUCCESS)
	{
		driverODBC_error(SQL_HANDLE_DBC, hdbc);
		return FAILURE;
	}

	val = SQLPrepare(query->hstmt, query->query, SQL_NTS);
	if (val != SQL_SUCCESS)
	{
		driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
		return FAILURE;
	}

	query->parammeta = (struct driverODBC_meta *)malloc(sizeof(struct driverODBC_meta));
	val = SQLNumParams(query->hstmt, &(query->parammeta->numCols));
	if (val != SQL_SUCCESS)
	{
		driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
		return FAILURE;
	}

	query->parammeta->types = (struct driverODBC_columnmeta **)malloc(query->parammeta->numCols * sizeof(struct driverODBC_columnmeta));
	for (i = 0 ; i < query->parammeta->numCols ; i++)
	{
		query->parammeta->types[i] = (struct driverODBC_columnmeta *)malloc(sizeof(struct driverODBC_columnmeta));
		val = SQLDescribeParam(query->hstmt, i + 1, &(query->parammeta->types[i]->type), &(query->parammeta->types[i]->length), NULL, NULL);
		if (val != SQL_SUCCESS)
		{
			driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
			return FAILURE;
		}
	}
	
	odbcQueries[numQueries++] = query;
	
	return query->parammeta->numCols;
}


DllExport struct xsb_data** call_conv driverODBC_execPrepareStatement(struct xsb_data** param, struct xsb_queryHandle* handle)
{
	struct driverODBC_queryInfo* query;
	SQLRETURN val;
	int i;

	val = 0;
	query = NULL;
	for (i = 0 ; i < numQueries ; i++)
	{
		if (!strcmp(odbcQueries[i]->handle, handle->handle))
		{
			query = odbcQueries[i];
			break;
		}
	}
	
	if (handle->state == QUERY_RETRIEVE)
		return driverODBC_getNextRow(query, 0);
	
	// do a check of the number of parameters supplied and number expected

	for (i = 0 ; i < query->parammeta->numCols ; i++)
	{
		if (param[i]->type == STRING_TYPE)
			val = SQLBindParameter(query->hstmt, i + 1, SQL_PARAM_INPUT, SQL_C_DEFAULT, SQL_CHAR, 0, 0, (SQLPOINTER)param[i]->val->str_val, strlen(param[i]->val->str_val) + 1, NULL);
		else if (param[i]->type == INT_TYPE)
			val = SQLBindParameter(query->hstmt, i + 1, SQL_PARAM_INPUT, SQL_C_DEFAULT, SQL_INTEGER, 0, 0, (SQLPOINTER)param[i]->val->i_val, 0, NULL);
		else if (param[i]->type == FLOAT_TYPE)	
			val = SQLBindParameter(query->hstmt, i + 1, SQL_PARAM_INPUT, SQL_C_DEFAULT, SQL_DOUBLE, 0, 0, (SQLPOINTER)param[i]->val->f_val, 0, NULL);
		if (val != SQL_SUCCESS)
		{
			driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
			return NULL;
		}
	}
	
	val = SQLExecute(query->hstmt);
	if (val != SQL_SUCCESS)
	{
		driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
		return NULL;
	}

	query->resultmeta = (struct driverODBC_meta *)malloc(sizeof(struct driverODBC_meta));
	val = SQLNumResultCols(query->hstmt, (SQLSMALLINT *)(&(query->resultmeta->numCols)));
	if (val != SQL_SUCCESS)
	{
		driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
		return NULL;
	}
	query->resultmeta->types = (struct driverODBC_columnmeta **)malloc(query->resultmeta->numCols * sizeof(struct driverODBC_columnmeta *));
	for (i = 0 ; i < query->resultmeta->numCols ; i++)
	{
		query->resultmeta->types[i] = (struct driverODBC_columnmeta *)malloc(sizeof(struct driverODBC_columnmeta));
		val = SQLColAttribute(query->hstmt, i + 1, SQL_COLUMN_TYPE, NULL, 0, NULL, &(query->resultmeta->types[i]->type));
		if (val != SQL_SUCCESS)
		{
			driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
			return NULL;
		}
		val = SQLColAttribute(query->hstmt, i + 1, SQL_COLUMN_LENGTH, NULL, 0, NULL, &(query->resultmeta->types[i]->length));
		if (val != SQL_SUCCESS)
		{
			driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
			return NULL;
		}
	}

	handle->state = QUERY_RETRIEVE;

	return driverODBC_getNextRow(query, 0);
}


DllExport int call_conv driverODBC_closeStatement(struct xsb_queryHandle* handle)
{
	struct driverODBC_queryInfo* query;
	SQLRETURN val;
	int i, j;

	for (i = 0 ; i < numQueries ; i++)
	{
		if (!strcmp(odbcQueries[i]->handle, handle->handle))
		{
			query = odbcQueries[i];
			for (j = 0 ; j < query->resultmeta->numCols ; j++)
				free(query->resultmeta->types[j]);
			free(query->resultmeta->types);
			free(query->resultmeta);
			val = SQLFreeHandle(SQL_HANDLE_STMT, query->hstmt);
			if (val != SQL_SUCCESS)
			{
				driverODBC_error(SQL_HANDLE_STMT, query->hstmt);
				return FAILURE;
			}
			for (j = i + 1 ; j < numQueries ; j++)
				odbcQueries[j-1] = odbcQueries[j];
			free(query->handle);
			free(query->query);
			free(query);
			numQueries--;
		}
	}

	return SUCCESS;
}


DllExport char* call_conv driverODBC_errorMesg()
{
	char* temp;
	if (errorMesg != NULL)
	{
		temp = (char *)malloc(SQL_MAX_MESSAGE_LENGTH * sizeof(char));
		strcpy(temp, errorMesg);
		free(errorMesg);
		errorMesg = NULL;
		return temp;
	}
	return NULL;
}


static int driverODBC_getXSBType(SQLSMALLINT dataType)
{
	int type;
	type = STRING_TYPE;
	switch (dataType)
	{
		case SQL_CHAR:
		case SQL_VARCHAR:
		case SQL_LONGVARCHAR:
				type = STRING_TYPE;
				break;

		case SQL_DECIMAL:
		case SQL_NUMERIC:
		case SQL_REAL:
		case SQL_FLOAT:
		case SQL_DOUBLE:
				type = FLOAT_TYPE;
				break;

		case SQL_SMALLINT:
		case SQL_INTEGER:
		case SQL_TINYINT:
		case SQL_BIGINT:
				type = INT_TYPE;
				break;
	}

	return type;
}


static void driverODBC_error(SQLSMALLINT handleType, SQLHANDLE handle)
{
	SQLCHAR* sqlState;
	
	errorMesg = (SQLCHAR *)malloc(SQL_MAX_MESSAGE_LENGTH * sizeof(SQLCHAR));
	sqlState = (SQLCHAR *)malloc(6 * sizeof(SQLCHAR));
	if (handleType == SQL_HANDLE_ENV)		
		SQLError(SQL_NULL_HENV, SQL_NULL_HDBC, handle, sqlState, NULL, errorMesg, SQL_MAX_MESSAGE_LENGTH - 1, NULL);
	else if (handleType == SQL_HANDLE_DBC)
		SQLError(SQL_NULL_HENV, SQL_NULL_HDBC, handle, sqlState, NULL, errorMesg, SQL_MAX_MESSAGE_LENGTH - 1, NULL);
	else if (handleType == SQL_HANDLE_STMT)
		SQLError(SQL_NULL_HENV, SQL_NULL_HDBC, handle, sqlState, NULL, errorMesg, SQL_MAX_MESSAGE_LENGTH - 1, NULL);
	free(sqlState);
}


DllExport int call_conv driverODBC_register(void)
{
	union functionPtrs* funcConnect;
	union functionPtrs* funcDisconnect;
	union functionPtrs* funcQuery;
	union functionPtrs* funcPrepare;
	union functionPtrs* funcExecPrepare;
	union functionPtrs* funcCloseStmt;
	union functionPtrs* funcErrorMesg;

	registerXSBDriver("odbc", 7);

	funcConnect = (union functionPtrs *)malloc(sizeof(union functionPtrs));
	funcConnect->connectDriver = driverODBC_connect;
	registerXSBFunction("odbc", CONNECT, funcConnect);

	funcDisconnect = (union functionPtrs *)malloc(sizeof(union functionPtrs));
	funcDisconnect->disconnectDriver = driverODBC_disconnect;
	registerXSBFunction("odbc", DISCONNECT, funcDisconnect);

	funcQuery = (union functionPtrs *)malloc(sizeof(union functionPtrs));
	funcQuery->queryDriver = driverODBC_query;
	registerXSBFunction("odbc", QUERY, funcQuery);

	funcPrepare = (union functionPtrs *)malloc(sizeof(union functionPtrs));
	funcPrepare->prepareStmtDriver = driverODBC_prepareStatement;
	registerXSBFunction("odbc", PREPARE, funcPrepare);
	
	funcExecPrepare = (union functionPtrs *)malloc(sizeof(union functionPtrs));
	funcExecPrepare->executeStmtDriver = driverODBC_execPrepareStatement;
	registerXSBFunction("odbc", EXEC_PREPARE, funcExecPrepare);

	funcCloseStmt = (union functionPtrs *)malloc(sizeof(union functionPtrs));
	funcCloseStmt->closeStmtDriver = driverODBC_closeStatement;
	registerXSBFunction("odbc", CLOSE_STMT, funcCloseStmt);

	funcErrorMesg = (union functionPtrs *)malloc(sizeof(union functionPtrs));
	funcErrorMesg->errorMesgDriver = driverODBC_errorMesg;
	registerXSBFunction("odbc", ERROR_MESG, funcErrorMesg);

	return TRUE;
}


