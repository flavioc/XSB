/* File:      odbc_xsb.c
** Author(s): Lily Dong
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
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
** $Id$
** 
*/

#include "configs/config.h"

#ifdef WIN_NT
#include <windows.h>
#include <SQL.H>
#include <SQLEXT.H>
#include <odbcinst.h>
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "cinterf.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "export.h"

#define MAXCURSORNUM                    20
#define MAXCOLS                         100
#define MAXNUMPRECISION                 15
#define MAXNUMSTRINGSIZE                (MAXNUMPRECISION + 5)
#define MAXCHARLEN                      100
#define MAXBINDVALLEN                   100
#define MAXI(a,b)                       ((a)>(b)?(a):(b))

static char     *str[4] = {"NULL","string","integer", "number"};
static int      serverConnected = 0;

HENV henv;
HDBC hdbc;
HSTMT hstmt;
UCHAR uid[128];

struct Cursor {
  int Status;           // status of the cursor
  int StmtNum;          // the number of the sql statement
  UCHAR *Sql;            // pointer to the sql statement
  HSTMT hstmt;           // the statement handler
  int VarListNum;       // distinct bind variable number
  UCHAR **VarList;       // pointer to array of pointers to the actual bind vars
  int *VarTypes;        // types of the distinct bind vars
  int VarCurNum;        // Current Bind Var Number in the distinct Bind var list 
  int BListNum;         // number of total bind vars in the sql statement 
  UCHAR **BList;        // pointer to array of pointers to the bind vars
  int *BTypes;          // and pointer to their types
  int BCurNum;          // Current Bind Var Number in the total Bind var list
  SWORD ColNum;          // number of columns selected
  SWORD *ColTypes;       // pointer to array of column types
  UDWORD *ColLen;        // pointer to array of column lengths
  UDWORD *OutLen;        // pointer to array of actual column lenghts
  UCHAR **Data;          // pointer to array of pointers to data
  SWORD ColCurNum;       // the cloumn number that's already fetched by xsb
};

// global cursor table
struct Cursor CursorTable[MAXCURSORNUM];

//-----------------------------------------------------------------------------
//  FUNCTION NAME:
//      PrintErrorMsg()
//  PARAMETERS:
//      int i - index into the global cursor table  
//  NOTES:
//      PrintErrorMsg() prints out the error message that associates
//      with the statement handler of cursor i.  if i is less than 0, 
//-----------------------------------------------------------------------------
int PrintErrorMsg(int i)
{
  UCHAR FAR *szsqlstate;
  SDWORD FAR *pfnativeerror;
  UCHAR FAR *szerrormsg;
  SWORD cberrormsgmax;
  SWORD FAR *pcberrormsg;
  RETCODE rc;

  szsqlstate=(UCHAR FAR *)malloc(sizeof(UCHAR FAR)*10);
  pfnativeerror=(SDWORD FAR *)malloc(sizeof(SDWORD FAR));
  szerrormsg=(UCHAR FAR *)malloc(sizeof(UCHAR FAR)*SQL_MAX_MESSAGE_LENGTH);
  pcberrormsg=(SWORD FAR *)malloc(sizeof(SWORD FAR));
  cberrormsgmax=SQL_MAX_MESSAGE_LENGTH-1;
  if (i >= 0)
    rc = SQLError(SQL_NULL_HENV, hdbc, CursorTable[i].hstmt, szsqlstate,
		  pfnativeerror, szerrormsg,cberrormsgmax,pcberrormsg);
  else
    rc = SQLError(SQL_NULL_HENV, hdbc, hstmt, szsqlstate,
		  pfnativeerror, szerrormsg,cberrormsgmax,pcberrormsg);
  if ((rc == SQL_SUCCESS) || (rc == SQL_SUCCESS_WITH_INFO)) { 
    printf("ODBC SYSCALL ERROR:\n");
    printf("    ODBC Error Code: %s\n", szsqlstate);
    printf("    ODBC Error Message: %s\n", szerrormsg);
  }
  free(szsqlstate);
  free(pfnativeerror);
  free(szerrormsg);
  free(pcberrormsg);
  return 1;
}

//-----------------------------------------------------------------------------
//  FUNCTION NAME:
//      SetCursorClose()
//  PARAMETER:
//      int i - index into the global cursor table    
//  NOTES:
//      free all the memory resource allocated for cursor i
//-----------------------------------------------------------------------------
void SetCursorClose(int i)
{
  int j;

  SQLFreeStmt(CursorTable[i].hstmt, SQL_CLOSE);    // free statement handler

  if (CursorTable[i].VarListNum) {                 // free bind variable list
    for (j = 0; j < CursorTable[i].VarListNum; j++) 
      free((void *)CursorTable[i].VarList[j]);
    free(CursorTable[i].BList);
    free(CursorTable[i].VarList);
    free(CursorTable[i].BTypes);
    free(CursorTable[i].VarTypes);
  }

  if (CursorTable[i].ColNum) {                  // free the resulting row set
    for (j = 0; j < CursorTable[i].ColNum; j++)
      free(CursorTable[i].Data[j]);
    free(CursorTable[i].ColTypes);
    free(CursorTable[i].ColLen);
    free(CursorTable[i].OutLen);                
    free(CursorTable[i].Data);
  }

  // free memory for the sql statement associated w/ this cursor
  if (CursorTable[i].Sql) free(CursorTable[i].Sql);  
  // initialize the variables.  set them to the right value
  CursorTable[i].Sql = 0;
  CursorTable[i].ColNum =
    CursorTable[i].Status =
    CursorTable[i].VarListNum = 0;
}

//-----------------------------------------------------------------------------
//  FUNCTION NAME:
//      ODBCConnect()
//  NOTES:
//      This function is called when a user wants to start a db session,
//      assuming that she doesn't have one open.  It initializes system
//      resources for the new session, including allocations of various things:
//      environment handler, connection handler, statement handlers and then
//      try to connect to the database indicated by the second parameter prolog
//      passes us using the third one as user id and fourth one as passward.
//      If any of these allocations or connection fails, function returns a
//      failure code 1.  Otherwise 0. 
//-----------------------------------------------------------------------------
void ODBCConnect()
{
  int i;
  UCHAR *server;
  UCHAR *pwd;
  RETCODE rc;

  // if we already have a session open, then ... 
  if (serverConnected) {
    xsb_error("A session is already open");
    ctop_int(5, 1);
    return;
  }

  // allocated environment handler
  rc = SQLAllocEnv(&henv);
  if (rc != SQL_SUCCESS && rc != SQL_SUCCESS_WITH_INFO) {
    xsb_error("Environment allocation failed");   
    ctop_int(5, 1);
    return;
  }

  // allocated connection handler
  rc = SQLAllocConnect(henv, &hdbc);
  if (rc != SQL_SUCCESS && rc != SQL_SUCCESS_WITH_INFO) {
    SQLFreeEnv(henv);
    xsb_error("Connection Resources Allocation Failed");
    ctop_int(5, 1);
    return;
  }

  // get server name, user id and passward
  server = (UCHAR *)ptoc_string(2);
  strcpy(uid, (UCHAR *)ptoc_string(3));
  pwd = (UCHAR *)ptoc_string(4);

  // connect to database
  rc = SQLConnect(hdbc, server, SQL_NTS, uid, SQL_NTS, pwd, SQL_NTS);
  if (rc != SQL_SUCCESS && rc != SQL_SUCCESS_WITH_INFO) {
    SQLFreeConnect(hdbc);
    SQLFreeEnv(henv);
    xsb_error("Connection to server %s failed", server);   
    ctop_int(5, 1);
    return;
  }

  if (!((rc=SQLAllocStmt(hdbc,&hstmt))==SQL_SUCCESS) ||
      (rc==SQL_SUCCESS_WITH_INFO)) {
    SQLDisconnect(hdbc);
    SQLFreeConnect(hdbc);
    SQLFreeEnv(henv);
    ctop_int(5, 1);
    return;
  }
    
  // initialize cursor table. it includes statement handler initialization
  memset(CursorTable, 0, sizeof(struct Cursor) * MAXCURSORNUM);
  for (i = 0; i < MAXCURSORNUM; i++) {
    if (!(((rc=SQLAllocStmt(hdbc,&(CursorTable[i].hstmt)))==SQL_SUCCESS) ||
	  (rc==SQL_SUCCESS_WITH_INFO))) {
      int j;
      for (j = 0; j < i; j++)
	SQLFreeStmt(CursorTable[j].hstmt,SQL_DROP);
      SQLDisconnect(hdbc);
      SQLFreeConnect(hdbc);
      SQLFreeStmt(hstmt, SQL_DROP);
      SQLFreeEnv(henv);
      ctop_int(5, 1);
      return;
    }
  }

  serverConnected = 1;
  ctop_int(5, 0);
  return;
}

//-----------------------------------------------------------------------------
//  FUNCTION NAME:
//      ODBCDisconnect()    
//  NOTES:
//      Disconnect us from the server and free all the system resources we
//      allocated for the session - statement handlers, connection handler,
//      environment handler and memory space. 
//-----------------------------------------------------------------------------
void ODBCDisconnect()
{
  int i;

  if (!serverConnected) return;
    
  for (i = 0; i < MAXCURSORNUM; i++) {
    if (CursorTable[i].Status) 
      SetCursorClose(i);
    SQLFreeStmt(CursorTable[i].hstmt,SQL_DROP);
  }

  SQLFreeStmt(hstmt, SQL_DROP);    
  SQLDisconnect(hdbc);
  SQLFreeConnect(hdbc);
  SQLFreeEnv(henv);
  serverConnected = 0;
}

//-----------------------------------------------------------------------------
//  FUNCTION NAME:
//      FindFreeCursor()
//  NOTES:
//      Find a free statement handler and return its index number into the
//      global cursor table.  It gives priority to a closed cursor with same
//      stantement number over ordinary closed cursors.  If there is no handler
//      left,  function returns -1. possible cursor status values are
//      0 - never been used - no resource associated w/ the cursor 
//      1 - used before but having been closed-the cursor has all the resource
//      2 - reusing a used cursor w/ the same statement number, no resource
//          needs to be allocated
//      3 - using a cursor that has no resource - it needs to be allocated
//---------------------------------------------------------------------------------------------
void FindFreeCursor()
{ 
  int i, j = -1, k = -1;
  int StmtNum = ptoc_int(2);

  // search 
  for (i = 0; i < MAXCURSORNUM; i++) {
    if (!CursorTable[i].Status) j = i;        // a cursor never being used
    else {
      if (CursorTable[i].Status == 1) {    // a closed cursor
	// and it had the same statement number as this one. so grab it
	if (CursorTable[i].StmtNum == StmtNum) {     
	  if (StmtNum < 2) {
	    SetCursorClose(i);
	    CursorTable[i].Status = 3;
	  } else
	    CursorTable[i].Status = 2;
	  ctop_int(3, i);
	  return;
	} else k = i;                      // otherwise just record it
      }
    }
  }

  // done w/ the search and we didn't find a reusable one
  if ((j < 0) && (k < 0))                        // no cursor left
    i = -1;
  else {
    // we give the cursor that has never been used the priority
    if ((i = j) < 0) SetCursorClose(i = k);                      
    CursorTable[i].StmtNum = StmtNum;
    CursorTable[i].Status = 3;
  }
  ctop_int(3, i);
  return;    
}

//-----------------------------------------------------------------------------
//  FUNCTION NAME:
//     SetBindVarNum() 
//  NOTES:
//      set the number of different bind variables and their total number of
//      occurrances in the sql statement to VarListNum and BListMum
//      respectively and allocate memory for furture use, i.e. for holding the
//      bind variables' types and array of pointers to their value.  note that
//      the memory to store their values is not allocated here since we don't
//      know their type: 
//  	no information on how much memory is needed.  if we're reusing an old
//  	statement handler we don't have to worry about these things.  all we
//  	need to do is to make sure that the statement is in deed the same
//  	statement w/ the same bind variable number. 
//-----------------------------------------------------------------------------
void SetBindVarNum()
{
  int i = ptoc_int(2);

  if (CursorTable[i].Status == 2) {
    if (CursorTable[i].VarListNum != ptoc_int(3))
      xsb_exit("In SetBindVarNum: CursorTable[i].VarListNum != ptoc_int(3)");
    if (CursorTable[i].BListNum != ptoc_int(4))
      xsb_exit("In SetBindVarNum: CursorTable[i].BListNum != ptoc_int(4)");
    return;
  }
    
  CursorTable[i].VarListNum = ptoc_int(3);
  CursorTable[i].VarList = malloc(sizeof(UCHAR *) * CursorTable[i].VarListNum);
  if (!CursorTable[i].VarList)
    xsb_exit("Not enough memory for CursorTable[i].VarList!");
  CursorTable[i].VarTypes = malloc(sizeof(int) * CursorTable[i].VarListNum);
  if (!CursorTable[i].VarTypes)
    xsb_exit("Not enough memory for CursorTable[i].VarTypes!");
  CursorTable[i].BListNum = ptoc_int(4);
  CursorTable[i].BList = malloc(sizeof(UCHAR *) * CursorTable[i].BListNum);
  if (!CursorTable[i].BList)
    xsb_exit("Not enough memory for CursorTable[i].BList!");
  CursorTable[i].BTypes = malloc(sizeof(int) * CursorTable[i].BListNum);
  if (!CursorTable[i].BTypes)
    xsb_exit("Not enough memory for CursorTable[i].BTypes!");
  CursorTable[i].BCurNum = 0;
}

//-----------------------------------------------------------------------------
//  FUNCTION NAME:
//      SetVar()   
//  NOTES:
//      set the bind variables' values. 
//    	  allocate memory if it is needed(status == 3)
//-----------------------------------------------------------------------------
void SetVar()
{
  int i = ptoc_int(2);
  int j = atoi(ptoc_string(3)+4);

  if (!((j > 0) && (j <= CursorTable[i].VarListNum)))
    xsb_exit("Abnormal argument in SetVar!");
    
  // if we're reusing an opened cursor w/ the statement number
  if (CursorTable[i].Status == 2) {
    if (CursorTable[i].VarTypes[j-1] != ptoc_int(5))
      xsb_exit("CursorTable VarTypes error!");
    switch (CursorTable[i].VarTypes[j-1]) {
    case 0:
      *((int *)CursorTable[i].VarList[j-1]) = ptoc_int(4);
      break;
    case 1:
      *((float *)CursorTable[i].VarList[j-1]) = (float)ptoc_float(4);
      break;
    case 2:
      strncpy(CursorTable[i].VarList[j-1], ptoc_string(4), MAXBINDVALLEN);
      (CursorTable[i].VarList[j-1])[MAXBINDVALLEN - 1] = 0;
      break;
    default:
      xsb_exit("Unknown bind variable type, %d", CursorTable[i].VarTypes[j-1]);
    }
    return;
  }
    
  // otherwise, memory needs to be allocated in this case
  switch (CursorTable[i].VarTypes[j-1] = ptoc_int(5)) {
  case 0:
    CursorTable[i].VarList[j-1] = (UCHAR *)malloc(sizeof(int));
    if (!CursorTable[i].VarList[j-1])
      xsb_exit("Not enough memory for an int in SetVar!");
    *((int *)CursorTable[i].VarList[j-1]) = ptoc_int(4);
    break;
  case 1:
    CursorTable[i].VarList[j-1] = (UCHAR *)malloc(sizeof(float));
    if (!CursorTable[i].VarList[j-1])
      xsb_exit("Not enough memory for a float in SetVar!");
    *((float *)CursorTable[i].VarList[j-1]) = (float)ptoc_float(4);
    break;
  case 2:
    CursorTable[i].VarList[j-1] = (UCHAR *)malloc(sizeof(char) * MAXBINDVALLEN);
    if (!CursorTable[i].VarList[j-1])
      xsb_exit("Not enough memory for MAXBINDVALLEN chars in SetVar!");
    strncpy(CursorTable[i].VarList[j-1], ptoc_string(4), MAXBINDVALLEN);
    CursorTable[i].VarList[j-1][MAXBINDVALLEN - 1] = 0;
    break;
  default:
    xsb_exit("Unknown bind variable type, %d", CursorTable[i].VarTypes[j-1]);
  }
}

//-----------------------------------------------------------------------------
//  FUNCTION NAME:
//      SetBind()
//  NOTES:
//      set the bind variables' values for each occurrance of the bind
//      variables in the sql statement.
//-----------------------------------------------------------------------------
void SetBind()
{
  int i = ptoc_int(2);
  int j = atoi(ptoc_string(3)+4);
  
  if (!((j > 0) && (j <= CursorTable[i].VarListNum)))
    xsb_exit("Abnormal argument in SetBind!");
    
  if (CursorTable[i].Status == 2) return;                 // did this already

  CursorTable[i].BList[CursorTable[i].BCurNum] = CursorTable[i].VarList[--j];
  CursorTable[i].BTypes[(CursorTable[i].BCurNum)++] = CursorTable[i].VarTypes[j];
}

int DescribeSelectList(int);

//-----------------------------------------------------------------------------
//  FUNCTION NAME:
//      Parse()
//  NOTES:
//      parse the sql statement and submit it to DBMS to execute.  if all these
//      succeed, then prepare for resulting row fetching.  this includes
//      determination of column number in the resulting rowset and the length
//      of each column and memory allocation which is used to store each row.
//      Note indices -3 and -2 are reserved for transaction control(rollback
//      and commit) and index -1 is for those sql statements that don't need
//      cursor, i.e. they can be executed directly. 
//-----------------------------------------------------------------------------
void Parse()
{
  int j;
  int i = ptoc_int(2);
  RETCODE rc;
  UWORD TablePrivilegeExists;

  if (!((i >= -3) && ( i < MAXCURSORNUM)))
    xsb_exit("Abnormal argument in Parse!");

  switch (i) {
  case (-3):                // index = -3; special case for rollback
    if (((rc=SQLTransact(henv,hdbc,SQL_ROLLBACK)) == SQL_SUCCESS) ||
	(rc == SQL_SUCCESS_WITH_INFO)) {
      for (i = 0; i < MAXCURSORNUM; i++) {
	if (CursorTable[i].Status) 
	  SetCursorClose(i);
      }
      ctop_int(4,0);
    } else
      ctop_int(4, PrintErrorMsg(-1));
    return;
  case (-2):               // index = -2; special case for commit
    if (((rc=SQLTransact(henv,hdbc,SQL_COMMIT)) == SQL_SUCCESS) ||
	(rc == SQL_SUCCESS_WITH_INFO)) { 
      for (i = 0; i < MAXCURSORNUM; i++) {
	if (CursorTable[i].Status) 
	  SetCursorClose(i);
      }
      ctop_int(4,0);
    } else
      ctop_int(4,PrintErrorMsg(-1));
    return;
  case (-1):          // index = -1; special case for odbc_sql; no return rows
    if (((rc=SQLExecDirect(hstmt,ptoc_string(3),SQL_NTS)) == SQL_SUCCESS) ||
	(rc == SQL_SUCCESS_WITH_INFO)) 
      ctop_int(4,0);
    else
      ctop_int(4,PrintErrorMsg(-1));
    return;
  default: ;
  }
  switch (CursorTable[i].StmtNum) {
  case (0):             // column information retrieval
    if (((rc=SQLColumns(CursorTable[i].hstmt,
			NULL, 0,
			NULL, 0,
			ptoc_string(3), SQL_NTS,
			NULL,0)) == SQL_SUCCESS) ||
	(rc == SQL_SUCCESS_WITH_INFO)) {
      ctop_int(4,DescribeSelectList(i));
    } else {
      ctop_int(4,PrintErrorMsg(i));
      SetCursorClose(i);
    }
    return; 
  case (-1):             // all the table names in this database
    if (((rc=SQLTables(CursorTable[i].hstmt,
		       NULL, 0,
		       NULL, 0,
		       NULL, 0,
		       NULL, 0)) == SQL_SUCCESS) ||
	(rc == SQL_SUCCESS_WITH_INFO)) 
      ctop_int(4,DescribeSelectList(i));
    else {
      ctop_int(4,PrintErrorMsg(i));
      SetCursorClose(i);
    }
    return; 
  case (-2):             // user accessable table names,
    // since some ODBC drivers don't implement the function SQLTablePrivileges
    // we check it first
    SQLGetFunctions(hdbc, SQL_API_SQLTABLEPRIVILEGES, &TablePrivilegeExists);
    if (!TablePrivilegeExists) {
      printf("Privilege concept does not exist in this DVMS: you probably can access any of the existing tables\n");
      ctop_int(4, 2);
      return;
    }
    if (((rc=SQLTablePrivileges(CursorTable[i].hstmt,
				NULL, 0,
				NULL, 0,
				NULL, 0)) == SQL_SUCCESS) ||
	(rc == SQL_SUCCESS_WITH_INFO)) 
      ctop_int(4,DescribeSelectList(i));
    else {
      ctop_int(4,PrintErrorMsg(i));
      SetCursorClose(i);
    }
    return; 
  default: ;
  }
  if (CursorTable[i].Status == 2) {
    rc = SQLCancel(CursorTable[i].hstmt);
    if ((rc != SQL_SUCCESS) && (rc != SQL_SUCCESS_WITH_INFO)) {
      ctop_int(4, PrintErrorMsg(i));
      SetCursorClose(i);
      return;
    }
  } else {
    CursorTable[i].Sql = (UCHAR *)strdup(ptoc_string(3));
    if (!CursorTable[i].Sql)
      xsb_exit("Not enough memory for strdup in Parse!");

    if (SQLPrepare(CursorTable[i].hstmt, CursorTable[i].Sql, SQL_NTS)
	!= SQL_SUCCESS) {
      ctop_int(4,PrintErrorMsg(i));
      SetCursorClose(i);
      return;
    }
    
    // set the bind variables
    for (j = 0; j < CursorTable[i].BListNum; j++) {
      if (CursorTable[i].BTypes[j] == 2)
	// we're sloppy here.  it's ok for us to use the default values
	rc = SQLSetParam(CursorTable[i].hstmt, (short)(j+1), SQL_C_CHAR, SQL_CHAR,
			 MAXCHARLEN, 0,(char *) CursorTable[i].BList[j], NULL);
      else if (CursorTable[i].BTypes[j] == 1)
	rc = SQLSetParam(CursorTable[i].hstmt, (short)(j+1), SQL_C_FLOAT, SQL_FLOAT,
			 0, 0, (float *)(CursorTable[i].BList[j]), NULL);
      else
	rc = SQLSetParam(CursorTable[i].hstmt, (short)(j+1), SQL_C_SLONG, SQL_INTEGER,
			 0, 0, (int *)(CursorTable[i].BList[j]), NULL);
      if (rc != SQL_SUCCESS) {
	ctop_int(4,PrintErrorMsg(i));
	SetCursorClose(i);
	return;
      }
    }
  }
  // submit it for execution
  if (SQLExecute(CursorTable[i].hstmt) != SQL_SUCCESS) {
    ctop_int(4,PrintErrorMsg(i));
    SetCursorClose(i);
    return;
  }
  ctop_int(4,DescribeSelectList(i));
  return;
}

//-----------------------------------------------------------------------------
//  FUNCTION NAME:
//      DisplayColSize()
//  PARAMETERS:
//      SWORD coltype - column type which is a single word.
//      UDWORD collen - column length which is returned by SQLDescribeCol
//      UCHAR *colname - pointer to column name string
//  RETURN VALUE:
//      column length - the size of memory that is needed to store the column
//      value for supported column types
//      0             - otherwise
//-----------------------------------------------------------------------------
UDWORD DisplayColSize(SWORD coltype, UDWORD collen, UCHAR *colname)
{

  switch (coltype) {
  case SQL_CHAR:
  case SQL_VARCHAR:
    return(MAXI(collen, strlen((char *) colname)));
  case SQL_SMALLINT:
    return(MAXI(6, strlen((char *)colname)));
  case SQL_INTEGER:
    return(MAXI(11, strlen((char *)colname)));
  case SQL_DECIMAL:
  case SQL_NUMERIC:
  case SQL_REAL:
  case SQL_FLOAT:
  case SQL_DOUBLE:
    return(MAXI(MAXNUMSTRINGSIZE, strlen((char *)colname)));
  case SQL_DATE:
  case SQL_TIME:
  case SQL_TIMESTAMP: return 32;
  default: ;
  }
  return 0;
}

//-----------------------------------------------------------------------------
//  FUNCTION NAME:
//      DescribeSelectList()
//  PARAMETERS:
//      int i - cursor number, the index into the global cursor table
//  RETURN VALUES:
//      0 - the result row has at least one column and
//      1 - something goes wrong, we can't retrieve column information, memory
//          allocation fails (if this happens program stops).
//      2 - no column is affected
//  NOTES:
//      memory is also allocated for future data storage
//-----------------------------------------------------------------------------
int DescribeSelectList(int i)
{
  int j;
  UCHAR colname[50];
  SWORD colnamelen;
  SWORD scale;
  SWORD nullable;
  UDWORD collen;

  CursorTable[i].ColCurNum = 0;
  SQLNumResultCols(CursorTable[i].hstmt, &(CursorTable[i].ColNum));
  if (!(CursorTable[i].ColNum)) return 2;   // no columns are affected

  // if we aren't reusing a closed statement hand, we need to get
  // resulting rowset info and allocate memory for it
  if (CursorTable[i].Status != 2) {
    CursorTable[i].ColTypes =
      (SWORD *)malloc(sizeof(SWORD) * CursorTable[i].ColNum);
    if (!CursorTable[i].ColTypes)
      xsb_exit("Not enough memory for ColTypes!");
    
    CursorTable[i].Data =
      (UCHAR **)malloc(sizeof(char *) * CursorTable[i].ColNum);
    if (!CursorTable[i].Data)
      xsb_exit("Not enough memory for Data!");

    CursorTable[i].OutLen =
      (UDWORD *)malloc(sizeof(UDWORD) * CursorTable[i].ColNum);
    if (!CursorTable[i].OutLen)
      xsb_exit("Not enough memory for OutLen!");

    CursorTable[i].ColLen =
      (UDWORD *)malloc(sizeof(UDWORD) * CursorTable[i].ColNum);
    if (!CursorTable[i].ColLen)
      xsb_exit("Not enough memory for ColLen!");
    
    for (j = 0; j < CursorTable[i].ColNum; j++) {
      SQLDescribeCol(CursorTable[i].hstmt, (short)(j+1), (UCHAR FAR*)colname,
		     sizeof(colname), &colnamelen,
		     &CursorTable[i].ColTypes[j],
		     &collen, &scale, &nullable);
      colnamelen = (colnamelen > 49) ? 49 : colnamelen; 
      colname[colnamelen] = '\0';
      if (!((CursorTable[i]).ColLen[j] =
	    DisplayColSize(CursorTable[i].ColTypes[j],collen,colname))) {
	CursorTable[i].ColNum = j; 
	// let SetCursorClose function correctly free all the memory allocated
	// for Data storage: CursorTable[i].Data[j]'s
	SetCursorClose(i);
	return(1);
      }
      CursorTable[i].Data[j] =
	(UCHAR *) malloc(((unsigned) CursorTable[i].ColLen[j]+1)*sizeof(UCHAR));
      if (!CursorTable[i].Data[j])
	xsb_exit("Not enough memory for Data[j]!");
    }
  }
  // bind them
  for (j = 0; j < CursorTable[i].ColNum; j++) 
    SQLBindCol(CursorTable[i].hstmt, (short)(j+1), SQL_C_CHAR, CursorTable[i].Data[j],
	       CursorTable[i].ColLen[j], (SDWORD FAR *)(&(CursorTable[i].OutLen[j])));
  return 0;
}

//-----------------------------------------------------------------------------
//  FUNCTION NAME:
//      FetchNextCol()
//  NOTES:
//      fetch next result rowset.  if we're retrieving user accessable table
//      names, we fetch until we get next user accessable table name
//-----------------------------------------------------------------------------
void FetchNextCol()
{
  int i = ptoc_int(2);
  RETCODE rc = SQLFetch(CursorTable[i].hstmt);

  // get user accessable table name
  if ((CursorTable[i].StmtNum == (-2))) {
    while (((rc == SQL_SUCCESS) || (rc == SQL_SUCCESS_WITH_INFO))
	   && (CursorTable[i].OutLen[1] != SQL_NULL_DATA)
	   && (strncmp(CursorTable[i].Data[1], uid, strlen(uid))
	       || (CursorTable[i].OutLen[1] != strlen(uid)))
	   && (strncmp(CursorTable[i].Data[4], uid, strlen(uid))
	       || (CursorTable[i].OutLen[4] != strlen(uid))))
      rc = SQLFetch(CursorTable[i].hstmt);
  }
  if ((rc == SQL_SUCCESS) || (rc == SQL_SUCCESS_WITH_INFO)) 
    ctop_int(3,0);
  else if (rc == SQL_NO_DATA_FOUND){
    CursorTable[i].Status = 1; // done w/fetching. set cursor status to unused 
    ctop_int(3,1);
  }
  else {
    SetCursorClose(i);         // error occured in fetching
    ctop_int(3,2);
  }
  return;
} 

//-----------------------------------------------------------------------------
//  FUNCTION NAME:
//      GetColumn() 
//  NOTES:
//      get the next column.  special care is taken if table information is
//      needed(statement number -2, -1 and 0) since we actually fetch more than
//      what we need. unfortunately it's inevitable.  we discard unwanted
//      columns.  for column info of a table, we need the fourth and fifth
//      columns(statement no 0). for table names in the database and user
//      accessable table names, we only need the third column(statement -1 and
//      -2). 
//-----------------------------------------------------------------------------
void GetColumn()
{
  int i = ptoc_int(2);
  int ColCurNum;
  UDWORD len;

  // if table information is retrieved, special care has to be paid
  // we set the ColCurNum to some appropriate value to get the columns we need
  // and discard unwanted 
  switch (CursorTable[i].StmtNum) {
  case (0):
    if (CursorTable[i].ColCurNum <= 3)
      CursorTable[i].ColCurNum = 3;
    else {
      if (CursorTable[i].ColCurNum > 4)
	CursorTable[i].ColCurNum = CursorTable[i].ColNum;
    }
    break;
  case (-1):
  case (-2):
    if (CursorTable[i].ColCurNum <= 2)
      CursorTable[i].ColCurNum = 2;
    else
      CursorTable[i].ColCurNum = CursorTable[i].ColNum;
    break;
  default: ;
  }
    
  if (CursorTable[i].ColCurNum == CursorTable[i].ColNum) {
    // no more columns in the result row
    CursorTable[i].ColCurNum = 0;
    ctop_int(4,1);   
    return;
  }

  // get the data
  ColCurNum = CursorTable[i].ColCurNum;
  if (CursorTable[i].OutLen[ColCurNum] == SQL_NULL_DATA) {
    // column value is NULL
    CursorTable[i].ColCurNum++;
    ctop_string(3,string_find(str[0],1));
    ctop_int(4,0);
    return;
  }

  // convert the column string to a C string 
  len = ((CursorTable[i].ColLen[ColCurNum] < CursorTable[i].OutLen[ColCurNum])?
	 CursorTable[i].ColLen[ColCurNum]:CursorTable[i].OutLen[ColCurNum]);
  *(CursorTable[i].Data[ColCurNum]+len) = '\0';

  // pass the result to Prolog if statement is 0, the column type of a table
  // is actually an integer, convert it to to corresponding string
  if ((!CursorTable[i].StmtNum) && (ColCurNum == 4)) {
    switch (atoi(CursorTable[i].Data[ColCurNum])) {
    case SQL_DATE:
    case SQL_TIME:
    case SQL_TIMESTAMP: 
    case SQL_CHAR:
    case SQL_VARCHAR:
      ctop_string(3, string_find(str[1],1));
      break;
    case SQL_SMALLINT:
    case SQL_INTEGER:
      ctop_string(3, string_find(str[2],1));
      break;
    case SQL_DECIMAL:
    case SQL_NUMERIC:
    case SQL_REAL:
    case SQL_FLOAT:
    case SQL_DOUBLE:
      ctop_string(3,string_find(str[3],1));
    }
    CursorTable[i].ColCurNum++;
    ctop_int(4,0);
    return;
  }
  // otherwise convert the string to either integer, float or string
  // according to the column type and pass it back to Prolog 
  switch (CursorTable[i].ColTypes[ColCurNum]) {
  case SQL_DATE:
  case SQL_TIME:
  case SQL_TIMESTAMP: 
  case SQL_CHAR:
  case SQL_VARCHAR:
    ctop_string(3, string_find(CursorTable[i].Data[ColCurNum],1)); 
    break;
  case SQL_SMALLINT:
  case SQL_INTEGER:
    ctop_int(3,atoi(CursorTable[i].Data[ColCurNum]));
    break;
  case SQL_DECIMAL:
  case SQL_NUMERIC:
  case SQL_REAL:
  case SQL_FLOAT:
  case SQL_DOUBLE:
    ctop_float(3,atof(CursorTable[i].Data[ColCurNum]));
  }
    
  CursorTable[i].ColCurNum++;
  ctop_int(4,0);
  return;
}


