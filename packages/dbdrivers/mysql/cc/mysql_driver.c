/* File: mysql_driver.c
** Author: Saikat Mukherjee
** Contact: saikat@cs.sunysb.edu

** This is the driver for connecting to a MySQL database.
** This is invoked from the middle_layer module in emu.
*/


#include "mysql_driver_defs.h"

struct driverMySQL_connectionInfo* mysqlHandles[MAX_HANDLES];
struct driverMySQL_queryInfo* mysqlQueries[MAX_QUERIES];
int numHandles, numQueries;
char* errorMesg;
//struct driverMySQL_preparedresultset* prepQueries[MAX_PREP_QUERIES];


int driverMySQL_initialise()
{
	numHandles = 0;
	numQueries = 0;
	errorMesg = NULL;

	return TRUE;
}


int driverMySQL_connect(struct xsb_connectionHandle* handle)
{
	struct driverMySQL_connectionInfo* mysqlHandle;
	MYSQL* mysql;

	mysql = (MYSQL *)malloc(sizeof(MYSQL));
	if (!mysql_init(mysql))
	{
		driverMySQL_error(mysql);
		free(mysql);
		return FAILURE;
	}
	
	if (!mysql_real_connect(mysql, handle->server, handle->user, handle->password, handle->database, 0, NULL, 0))
	{
		driverMySQL_error(mysql);
		free(mysql);
		return FAILURE; 
	}
	
	mysqlHandle = (struct driverMySQL_connectionInfo *)malloc(sizeof(struct driverMySQL_connectionInfo));
	mysqlHandle->handle = (char *)malloc((strlen(handle->handle) + 1) * sizeof(char));
	strcpy(mysqlHandle->handle, handle->handle);
	mysqlHandle->mysql = mysql;
	mysqlHandles[numHandles++] = mysqlHandle;

	return SUCCESS;
}


int driverMySQL_disconnect(struct xsb_connectionHandle* handle)
{
	int i, j;

	for (i = 0 ; i < numHandles ; i++)
	{
		if (!strcmp(handle->handle, mysqlHandles[i]->handle))
		{
			mysql_close(mysqlHandles[i]->mysql);
			free(mysqlHandles[i]->mysql);
			for (j = i + 1 ; j < numHandles ; j++)
				mysqlHandles[j-1] = mysqlHandles[j];
			numHandles--;
			break;
		}
	}

	return SUCCESS;
}


struct xsb_data** driverMySQL_query(struct xsb_queryHandle* handle)
{
	struct driverMySQL_connectionInfo* connection;
	struct driverMySQL_queryInfo* query;
	MYSQL_RES* resultSet;
	int i;

	query = NULL;
	connection = NULL;
	if (handle->state == QUERY_RETRIEVE)
	{
		for (i = 0 ; i < numQueries ; i++)
		{
			if (!strcmp(mysqlQueries[i]->handle, handle->handle))
			{
				query = mysqlQueries[i];
				break;
			}
		}
	}
	else if (handle->state == QUERY_BEGIN)
	{
		for (i = 0 ; i < numHandles ; i++)
		{
			if (!strcmp(mysqlHandles[i]->handle, handle->connHandle->handle))
			{
				connection = mysqlHandles[i];
				break;
			}
		}
		query = (struct driverMySQL_queryInfo *)malloc(sizeof(struct driverMySQL_queryInfo));
		query->handle = (char *)malloc((strlen(handle->handle) + 1) * sizeof(char));
		strcpy(query->handle, handle->handle);
		query->query = (char *)malloc((strlen(handle->query) + 1) * sizeof(char));
		strcpy(query->query, handle->query);
		query->connection = connection;

		if (mysql_query(query->connection->mysql, query->query))
		{
			driverMySQL_error(query->connection->mysql);
			return NULL;	
		}
		else
		{
			if ((resultSet = mysql_use_result(query->connection->mysql)) == NULL)
			{
				driverMySQL_error(query->connection->mysql);
				return NULL;
			}
			query->resultSet = resultSet;
			mysqlQueries[numQueries++] = query;
			handle->state = QUERY_RETRIEVE;
		}
	}

	return driverMySQL_getNextRow(query);
}


struct xsb_data** driverMySQL_getNextRow(struct driverMySQL_queryInfo* query)
{
	struct xsb_data** result;
	MYSQL_ROW row;
	int numFields;
	int i, j;

	result = NULL;
	if ((row = mysql_fetch_row(query->resultSet)) == NULL)
	{
		if (mysql_errno(query->connection->mysql))
			driverMySQL_error(query->connection->mysql);
		else
		{
			for (i = 0 ; i < numQueries ; i++)
			{
				if (!strcmp(mysqlQueries[i]->handle, query->handle))
				{
					mysql_free_result(query->resultSet);
					free(query->query);
					free(query->handle);
					free(query);
					for (j = i + 1 ; j < numQueries ; j++)
						mysqlQueries[j-1] = mysqlQueries[j];
					numQueries--;
				}
			}
		}
		return NULL;
	}

	numFields = mysql_num_fields(query->resultSet);
	result = (struct xsb_data **)malloc(numFields * sizeof(struct xsb_data *));
	for (i = 0 ; i < numFields ; i++)
	{
		result[i] = (struct xsb_data *)malloc(sizeof(struct xsb_data));
		result[i]->val = (union xsb_value *)malloc(sizeof(union xsb_value));
		result[i]->type = driverMySQL_getXSBType(mysql_fetch_field_direct(query->resultSet, i));
		switch (result[i]->type)
		{
			case INT_TYPE:
					result[i]->val->i_val = (int *)malloc(sizeof(int));
					result[i]->val->i_val = (int *)row[i];
					break;

			case FLOAT_TYPE:
					result[i]->val->f_val = (double *)malloc(sizeof(double));
					result[i]->val->f_val = (double *)row[i];
					break;

			case STRING_TYPE:
					result[i]->val->str_val = (char *)malloc(strlen(row[i]) * sizeof(char));
					strcpy(result[i]->val->str_val, (char *)row[i]);
					break;
		}
	}

	return result;
}


/* **** PREPARED STATEMENT FUNCTIONALITY (from MySQL 4.1 version) *****


int driverMySQL_prepareStatement(char* SQLQuery, struct xsb_queryHandle* handle)
{
	struct driverMySQL_preparedresultset* rs;
	MYSQL* mysql;
	MYSQL_STMT* stmt;
	MYSQL_RES* res;
	MYSQL_FIELD* field;
	int i;
	
	mysql = mysqlHandles[handle->connHandle->index];
	if ((stmt = mysql_prepare(mysql, SQLQuery, strlen(SQLQuery))) == NULL)
	{
		printf("MySQL ERROR: %s\n", mysql_error(mysql));
		return -1;		
	}
	if ((res = mysql_prepare_result(stmt)) == NULL)
	{
		printf("MySQL ERROR: %s\n", mysql_error(mysql));
		return -1;
	}
	rs = (struct driverMySQL_preparedresultset *)malloc(sizeof(struct driverMySQL_preparedresultset));
	rs->statement = stmt;
	rs->handle = handle;
	rs->handle->parameters = mysql_param_count(stmt);
	rs->returnFields = mysql_num_fields(res);
	rs->metaInfo = (struct xsb_data **)malloc(rs->returnFields * sizeof(struct xsb_data *));
	for (i = 0 ; i < rs->returnFields ; i++)
	{
		rs->metaInfo[i] = (struct xsb_data *)malloc(sizeof(struct xsb_data));
		MYSQL_FIELD* field = mysql_fetch_field_direct(res, i);
		rs->metaInfo[i]->type = driverMySQL_type(field);
		rs->metaInfo[i]->length = field->length;
	}
	prepQueries[numPrepQueries++] = rs;
	
	return (numPrepQueries - 1);
}

struct xsb_data** driverMySQL_execPrepareStmt(struct xsb_data** bindValues, struct xsb_queryHandle* handle)
{
	struct driverMySQL_preparedresultset* rs;
	int i;

	rs = prepQueries[handle->index];
	if (handle->state == QUERY_RETRIEVE)
		return driverMySQL_prepNextRow(rs);		
	
	MYSQL_BIND** bind = (MYSQL_BIND **)malloc(handle->parameters * sizeof(MYSQL_BIND *));
	for (i = 0 ; i < handle->parameters ; i++)
	{
		bind[i] = (MYSQL_BIND *)malloc(sizeof(MYSQL_BIND));
		if (bindValues[i]->type == INT_TYPE)
		{	
		}
		else if (bindValues[i]->type == FLOAT_TYPE)
		{
		}
		else if (bindValues[i]->type == STRING_TYPE)
		{
			bind[i]->buffer_type = MYSQL_TYPE_VAR_STRING;
			bind[i]->buffer_length = sizeof(bindValues[i]->val->str_val);
			bind[i]->buffer = bindValues[i]->val->str_val;
		}
	}
	if (mysql_bind_param(query->statement, bind))
	{
		printf("MySQL ERROR: %s\n", mysql_error(mysql));
		return NULL;
	}
	if (mysql_execute(query->statement))
	{
		printf("MySQL ERROR: %s\n", mysql_error(mysql));
		return NULL;
	}
	handle->state = QUERY_RETRIEVE;

	return driverMySQL_prepNextRow(rs);
}

struct xsb_data** driverMySQL_prepNextRow(struct driverMySQL_preparedresultset* rs)
{
	struct xsb_data** result;
	MYSQL_BIND** bindResult;
	int i;

	result = (struct xsb_data **)malloc(rs->returnFields * sizeof(struct xsb_data *));
	bindResult = (MYSQL_BIND **)malloc(rs->returnFields * sizeof(MYSQL_BIND *));
	for (i = 0 ; i < rs->returnFields ; i++)
	{
		result[i] = (struct xsb_data *)malloc(sizeof(struct xsb_data));
		bindResult[i] = (MYSQL_BIND *)malloc(sizeof(MYSQL_BIND));
		result[i]->type = rs->metaInfo[i]->type;
		result[i]->length = rs->metaInfo[i]->length;
		result[i]->val = (union value *)malloc(sizeof(union value));
		switch (result[i]->type)
		{
			case INT_TYPE:
					break;

			case FLOAT_TYPE:
					break;
			
			case STRING_TYPE:
					result[i]->val->str_val = (char *)malloc(result[i]->length * sizeof(char));
					bindResult[i]->buffer_type = MYSQL_TYPE_VAR_STRING;
					bindResult[i]->buffer = result[i]->val->str_val;
					break;
		}
	}
	
	if (mysql_bind_result(rs->statement, bindResult))
	{
		printf("MySQL ERROR: %s\n", mysql_error(mysql));
		return NULL;
	}
	
	if (mysql_fetch(rs->statement) == MYSQL_NO_DATA)
		return NULL;
	
	return result;
}

***** END OF PREPARED STATEMENT FUNCTIONALITY ***** */


char* driverMySQL_errorMesg()
{
	char* temp;
	if (errorMesg != NULL)
	{
		temp = (char *)malloc((strlen(errorMesg) + 1) * sizeof(char));
		strcpy(temp, errorMesg);
		free(errorMesg);
		errorMesg = NULL;
	}
	return NULL;
}


void driverMySQL_error(MYSQL* mysql)
{
	errorMesg = mysql_error(mysql);
}


int driverMySQL_getXSBType(MYSQL_FIELD* field)
{
	int type;

	type = 0;
	switch (field->type)
	{
		case FIELD_TYPE_TINY:
		case FIELD_TYPE_SHORT:
		case FIELD_TYPE_LONG:
		case FIELD_TYPE_INT24:
		case FIELD_TYPE_LONGLONG:
				type = INT_TYPE;
				break;
		
		case FIELD_TYPE_DECIMAL:
		case FIELD_TYPE_FLOAT:
		case FIELD_TYPE_DOUBLE:
				type = FLOAT_TYPE;
				break;

		case FIELD_TYPE_STRING:
		case FIELD_TYPE_DATE:
		case FIELD_TYPE_TIMESTAMP:
		case FIELD_TYPE_TIME:
		case FIELD_TYPE_DATETIME:
		case FIELD_TYPE_YEAR:
		case FIELD_TYPE_NEWDATE:
		case FIELD_TYPE_ENUM:
		case FIELD_TYPE_SET:
		case FIELD_TYPE_TINY_BLOB:
		case FIELD_TYPE_MEDIUM_BLOB:
		case FIELD_TYPE_LONG_BLOB:
		case FIELD_TYPE_BLOB:
		case FIELD_TYPE_VAR_STRING:
		case FIELD_TYPE_NULL:
				type = STRING_TYPE;
				break;
	}

	return type;
}


int driverMySQL_register(void)
{
	union functionPtrs* funcConnect;
	union functionPtrs* funcDisconnect;
	union functionPtrs* funcQuery;
	union functionPtrs* funcErrorMesg;
	//union functionPtrs* funcPrepare;
	//union functionPtrs* funcExecPrepare;

	registerXSBDriver("mysql", 4);

	funcConnect = (union functionPtrs *)malloc(sizeof(union functionPtrs));
	funcConnect->connectDriver = driverMySQL_connect;
	registerXSBFunction("mysql", CONNECT, funcConnect);

	funcDisconnect = (union functionPtrs *)malloc(sizeof(union functionPtrs));
	funcDisconnect->disconnectDriver = driverMySQL_disconnect;
	registerXSBFunction("mysql", DISCONNECT, funcDisconnect);

	funcQuery = (union functionPtrs *)malloc(sizeof(union functionPtrs));
	funcQuery->queryDriver = driverMySQL_query;
	registerXSBFunction("mysql", QUERY, funcQuery);

	funcErrorMesg = (union functionPtrs *)malloc(sizeof(union functionPtrs));
	funcErrorMesg->errorMesgDriver = driverMySQL_errorMesg;
	registerXSBFunction("mysql", ERROR_MESG, funcErrorMesg);

	//funcPrepare = (union functionPtrs *)malloc(sizeof(union functionPtrs));
	//funcPrepare->prepareStmtDriver = driverMySQL_prepareStatement;
	//registerXSBFunction("mysql", PREPARE, funcPrepare);

	//funcExecPrepare = (union functionPtrs *)malloc(sizeof(union functionPtrs));
	//funcExecPrepare->executeStmtDriver = driverMySQL_execPrepareStmt;
	//registerXSBFunction("mysql", EXEC_PREPARE, funcExecPrepare);
	
	return TRUE;
}

