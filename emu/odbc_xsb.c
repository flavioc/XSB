/* File:      odbc_xsb.c
** Author(s): Lily Dong, David S. Warren
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

#include "xsb_config.h"
#include "cell_xsb.h"

#ifdef WIN_NT
#include <windows.h>
#include <SQL.H>
#include <SQLEXT.H>
#include <string.h>
#else
#define FAR
#include "sql.h"
#include "sqlext.h"
#include "odbc_string.h"
#endif

#include <stdio.h>

#include <stdlib.h>
#include <assert.h>

#include "cinterf.h"
#include "deref.h"

#include "error_xsb.h"
#include "export.h"
#include "register.h"
#include "ptoc_tag_xsb_i.h"

#define MAXCURSORNUM                    25
#define MAXVARSTRLEN                    2000
#define MAXI(a,b)                       ((a)>(b)?(a):(b))

static Cell     nullStrAtom;
static int      serverConnected = 0;
/* static int      numberOfCursors = 0; */
static long      SQL_NTSval = SQL_NTS;

static HENV henv = NULL;
/*HDBC hdbc;*/
UCHAR uid[128];

struct Cursor {
  struct Cursor *NCursor; /* Next Cursor in cursor chain*/
  struct Cursor *PCursor; /* Prev Cursor in cursor chain*/
  HDBC hdbc;            /* connection handle for this cursor*/
  int Status;           /* status of the cursor*/
  UCHAR *Sql;           /* pointer to the sql statement*/
  HSTMT hstmt;          /* the statement handle*/
  int NumBindVars;      /* number of bind values*/
  UCHAR **BindList;     /* pointer to array of pointers to the bind values*/
  int *BindTypes;       /* types of the bind values*/
  SWORD NumCols;        /* number of columns selected*/
  SWORD *ColTypes;      /* pointer to array of column types*/
  UDWORD *ColLen;       /* pointer to array of max column lengths*/
  UDWORD *OutLen;       /* pointer to array of actual column lengths*/
  UCHAR **Data;         /* pointer to array of pointers to data*/
};

/* global cursor table*/
struct Cursor *FCursor;  /* root of curser chain*/
struct Cursor *LCursor;  /* tail of curser chain*/

/* Number of Cursors per Connection */
struct NumberofCursors{
  HDBC hdbc;
  int CursorCount;
  struct NumberofCursors *NCurNum;
};

struct NumberofCursors *FCurNum; /* First in the list of Number of Cursors */

SWORD ODBCToXSBType(SWORD odbcType)
{
  switch (odbcType) {
  case SQL_TINYINT:
  case SQL_SMALLINT:
  case SQL_INTEGER:
    return SQL_C_SLONG;
  case SQL_DECIMAL:
  case SQL_NUMERIC:
  case SQL_REAL:
  case SQL_FLOAT:
  case SQL_DOUBLE:
    return SQL_C_FLOAT;
  case SQL_DATE:
  case SQL_TIME:
  case SQL_TIMESTAMP: 
  case SQL_CHAR:
  case SQL_VARCHAR:
  default:
    return SQL_C_CHAR;
  }
}

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     PrintErrorMsg()*/
/*  PARAMETERS:*/
/*     struct Cursor *c - pointer to a cursor, or NULL*/
/*  NOTES:*/
/*     PrintErrorMsg() prints out the error message that associates*/
/*     with the statement handler of cursor i.*/
/*-----------------------------------------------------------------------------*/
int PrintErrorMsg(struct Cursor *cur)
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
  if (cur != NULL)
    rc = SQLError(SQL_NULL_HENV, cur->hdbc, cur->hstmt, szsqlstate,
		  pfnativeerror, szerrormsg,cberrormsgmax,pcberrormsg);
  else
    rc = SQLError(SQL_NULL_HENV, NULL, SQL_NULL_HSTMT, szsqlstate,
		  pfnativeerror, szerrormsg,cberrormsgmax,pcberrormsg);
  if ((rc == SQL_SUCCESS) || (rc == SQL_SUCCESS_WITH_INFO)) { 
    printf("ODBC SYSCALL ERROR (CODE %s): %s\n", szsqlstate, szerrormsg);
  }
  free(szsqlstate);
  free(pfnativeerror);
  free(szerrormsg);
  free(pcberrormsg);
  return 1;
}

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     SetCursorClose()*/
/*  PARAMETER:*/
/*     struct Cursor *cur - pointer to current cursor*/
/*  NOTES:*/
/*     free all the memory resource allocated for cursor cur*/
/*-----------------------------------------------------------------------------*/
void SetCursorClose(struct Cursor *cur)
{
  int j;

  SQLFreeStmt(cur->hstmt, SQL_CLOSE);    /* free statement handler*/

  if (cur->NumBindVars) {                 /* free bind variable list*/
    for (j = 0; j < cur->NumBindVars; j++) 
      if (cur->BindTypes[j] != 2) free((void *)cur->BindList[j]);
    free(cur->BindList);
    free(cur->BindTypes);
  }

  if (cur->NumCols) {                  /* free the resulting row set*/
    for (j = 0; j < cur->NumCols; j++)
      free(cur->Data[j]);
    free(cur->ColTypes);
    free(cur->ColLen);
    free(cur->OutLen);                
    free(cur->Data);
  }

  /* free memory for the sql statement associated w/ this cursor*/
  if (cur->Sql) free(cur->Sql);  
  /* initialize the variables.  set them to the right value*/
  cur->Sql = 0;
  cur->NumCols =
    cur->Status =
    cur->NumBindVars = 0;
}

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     ODBCConnect()*/
/*  PARAMETERS:*/
/*     R1: 1*/
/*     R2: Server*/
/*     R3: User name*/
/*     R4: Password*/
/*     R5: var, for returned Connection ID.*/
/*  NOTES:*/
/*     This function is called when a user wants to start a db session,*/
/*     assuming that she doesn't have one open.  It initializes system*/
/*     resources for the new session, including allocations of various things:*/
/*     environment handler, connection handler, statement handlers and then*/
/*     try to connect to the database indicated by the second parameter prolog*/
/*     passes us using the third one as user id and fourth one as passward.*/
/*     If any of these allocations or connection fails, function returns a*/
/*     failure code 1.  Otherwise 0. */
/*-----------------------------------------------------------------------------*/
void ODBCConnect()
{
  UCHAR *server;
  UCHAR *pwd;
  HDBC hdbc = NULL;
  RETCODE rc;

  /* if we don't yet have an environment, allocate one.*/
  if (!henv) {
    /* allocate environment handler*/
    rc = SQLAllocEnv(&henv);
    if (rc != SQL_SUCCESS && rc != SQL_SUCCESS_WITH_INFO) {
      xsb_error("Environment allocation failed");   
      ctop_int(5, 0);
      return;
    }
    /*    SQLSetEnvAttr(henv, SQL_ATTR_ODBC_VERSION, (SQLPOINTER)SQL_OV_ODBC2, 
		SQL_IS_UINTEGER);
    */

    LCursor = FCursor = NULL;
    FCurNum = NULL;
    nullStrAtom = makestring(string_find("NULL",1));
  }

  /* allocate connection handler*/
  rc = SQLAllocConnect(henv, &hdbc);
  if (rc != SQL_SUCCESS && rc != SQL_SUCCESS_WITH_INFO) {
    xsb_error("Connection Resources Allocation Failed");
    ctop_int(5, 0);
    return;
  }

  /* get server name, user id and password*/
  server = (UCHAR *)ptoc_string(2);
  strcpy(uid, (UCHAR *)ptoc_string(3));
  pwd = (UCHAR *)ptoc_string(4);

  /* connect to database*/
  rc = SQLConnect(hdbc, server, SQL_NTS, uid, SQL_NTS, pwd, SQL_NTS);
  if (rc != SQL_SUCCESS && rc != SQL_SUCCESS_WITH_INFO) {
    SQLFreeConnect(hdbc);
    xsb_error("Connection to server %s failed", server);   
    ctop_int(5, 0);
    return;
  }

  serverConnected = 1;
  ctop_int(5, (long)hdbc);
  return;
}

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     ODBCDisconnect()    */
/*  PARAMETERS:*/
/*     R1: 2*/
/*     R2: hdbc if closing a particular connection*/
/*         0 if closing entire ODBC session.*/
/*  NOTES:*/
/*     Disconnect us from the server and free all the system resources we*/
/*     allocated for the session - statement handlers, connection handler,*/
/*     environment handler and memory space. */
/*-----------------------------------------------------------------------------*/
void ODBCDisconnect()
{
  struct Cursor *cur = FCursor;
  struct Cursor *tcur;
  struct NumberofCursors *numi = FCurNum, *numj = FCurNum;
  HDBC hdbc = (HDBC)ptoc_int(2);

  if (!serverConnected) return;

  if (hdbc == NULL) {  /* close entire connection*/
    if (FCursor != NULL) 
      xsb_abort("Must close all connections before shutting down");
    SQLFreeEnv(henv);
    serverConnected = 0;
    return;
  }

  /* only free cursors associated with this connection (hdbc)*/
  while((numj != NULL) && (numj->hdbc != hdbc)){
    if(numj != FCurNum) numi=numi->NCurNum;
    numj=numj->NCurNum;
  }
  
  if(numj != NULL){
    if(numj == FCurNum) FCurNum=numj->NCurNum;
    else numi->NCurNum=numj->NCurNum;
    free(numj);
  }

  while (cur != NULL) {
    if (cur->hdbc == hdbc) {
      tcur = cur->NCursor;
      if (cur->Status != 0) SetCursorClose(cur);
      SQLFreeStmt(cur->hstmt,SQL_DROP);
      if (cur->PCursor) (cur->PCursor)->NCursor = cur->NCursor;
      else FCursor = cur->NCursor;
      if (cur->NCursor) (cur->NCursor)->PCursor = cur->PCursor;
      else LCursor = cur->PCursor;
      free(cur);
      /*      (num->CursorCount)-- */
       cur = tcur;
    }
    else cur = cur->NCursor;
  }

  SQLDisconnect(hdbc);
  SQLFreeConnect(hdbc);
  /*  SQLFreeEnv(henv);*/
  serverConnected = 0;
}

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     FindFreeCursor()*/
/*  PARAMETERS:*/
/*     R1: 7*/
/*     R2: Connection Handle*/
/*     R3: SQL Statement*/
/*     R4: var, in which Cursor addr is returned*/
/*  NOTES:*/
/*     Find a free statement handler and return its index number into the*/
/*     global cursor table.  It gives priority to a closed cursor with same*/
/*     stantement number over ordinary closed cursors.  If there is no handler*/
/*     left,  function returns NULL. possible cursor status values are*/
/*     0 - never been used - no resource associated w/ the cursor */
/*     1 - used before but having been closed-the cursor has all the resource*/
/*     2 - reusing a used cursor w/ the same statement number, no resource*/
/*         needs to be allocated*/
/*     3 - using a cursor that has no resource - it needs to be allocated*/
/*-----------------------------------------------------------------------------*/
void FindFreeCursor()
{ 
  struct Cursor *curi = FCursor, *curj = NULL, *curk = NULL;
  struct NumberofCursors *num = FCurNum;
  HDBC hdbc = (HDBC)ptoc_int(2);
  char *Sql_stmt = ptoc_longstring(3);
  RETCODE rc;

  /* search */
  while (curi != NULL) {
    if (curi->hdbc == hdbc) { /* only look at stmt handles for this connection */
      if (curi->Status == 0) curj = curi; /* cursor never been used*/
      else {
	if (curi->Status == 1) {    /* a closed cursor*/
	  /* same statement as this one, so grab and return it*/
	  if (!strcmp(curi->Sql,Sql_stmt)) {
	    if (curi != FCursor) {
	      (curi->PCursor)->NCursor = curi->NCursor;
	      if (curi == LCursor) LCursor = curi->PCursor;
	      else (curi->NCursor)->PCursor = curi->PCursor;
	      FCursor->PCursor = curi;
	      curi->PCursor = NULL;
	      curi->NCursor = FCursor;
	      FCursor = curi;
	    }
	    curi->Status = 2;
	    ctop_int(4, (long)curi);
	    /*printf("reuse cursor: %p\n",curi);*/
	    return;
	  } else {
	    curk = curi;                      /* otherwise just record it*/
	  }
	}
      }
    }
    curi = curi->NCursor;
  }

  /* done w/ the search; see what was found*/
  if (curj != NULL) {   /* give priority to an unused cursor*/
    curi = curj;
    /*printf("take unused cursor: %p\n",curi);*/
  }
  else {
    while((num != NULL) && (num->hdbc != hdbc)){
      num=num->NCurNum;
    }
    if(num == NULL){
      num = (struct NumberofCursors *)malloc(sizeof(struct NumberofCursors));
      num->hdbc = hdbc;
      num->NCurNum=FCurNum;
      FCurNum=num;
      num->CursorCount=0;
    }

    if (num->CursorCount < MAXCURSORNUM) { /* allocate a new cursor if allowed*/
    /* problem here: should have numberOfCursors for each connection */
    curi = (struct Cursor *)calloc(sizeof(struct Cursor),1);
    curi->PCursor = NULL;
    curi->NCursor = FCursor;
    if (FCursor == NULL) LCursor = curi;
    else FCursor->PCursor = curi;
    FCursor = curi;

    rc = SQLAllocStmt(hdbc,&(curi->hstmt));
    if (!((rc==SQL_SUCCESS) ||
	  (rc==SQL_SUCCESS_WITH_INFO))) {
      free(curi);
      /*      numberOfCursors--; */
      xsb_abort("while trying to allocate ODBC statement\n");
    }

    num->CursorCount++;

    /*printf("allocate a new cursor: %p\n",curi);*/
    }
    else if (curk == NULL) {  /* no cursor left*/
      ctop_int(4, 0);
      return;
    }
    else {                    /* steal a cursor*/
      curi = curk;
      SetCursorClose(curi);
      /*printf("steal a cursor: %p\n",curi);*/
    } 
  }

  /* move to front of list.*/
  if (curi != FCursor) {
    (curi->PCursor)->NCursor = curi->NCursor;
    if (curi == LCursor) LCursor = curi->PCursor;
    else (curi->NCursor)->PCursor = curi->PCursor;
    FCursor->PCursor = curi;
    curi->PCursor = NULL;
    curi->NCursor = FCursor;
    FCursor = curi;
  }

  curi->hdbc = hdbc;
  curi->Sql = (UCHAR *)strdup(Sql_stmt);
  if (!curi->Sql)
    xsb_exit("Not enough memory for SQL stmt in FindFreeCursor!");
  curi->Status = 3;
  ctop_int(4, (long)curi);
  return;    
}

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     SetBindVarNum() */
/*  PARAMETERS:*/
/*     R1: 3*/
/*     R2: Cursor Address*/
/*     R3: Number of bind values*/
/*  NOTES:*/
/*     set the number of bind variables.  Note that the memory to*/
/*     store their values is not allocated here since we don't know*/
/*     their type: no information on how much memory is needed.  If*/
/*     we're reusing an old statement handler we don't have to worry*/
/*     about these things.  All we need to do is to make sure that*/
/*     the statement is indeed the same statement w/ the same bind*/
/*     variable number.*/
/*-----------------------------------------------------------------------------*/
void SetBindVarNum()
{
  struct Cursor *cur = (struct Cursor *)ptoc_int(2);
  int NumBindVars = ptoc_int(3);

  if (cur->Status == 2) {
    if (cur->NumBindVars != NumBindVars)
      xsb_exit("Number of Bind values provided does not agree with query\n");
    return;
  }
    
  cur->NumBindVars = NumBindVars;
  cur->BindList = malloc(sizeof(UCHAR *) * NumBindVars);
  if (!cur->BindList)
    xsb_exit("Not enough memory for cur->BindList!");
  cur->BindTypes = malloc(sizeof(int) * NumBindVars);
  if (!cur->BindTypes)
    xsb_exit("Not enough memory for cur->BindTypes!");
}

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     SetBindVal()   */
/*  PARAMETERS:*/
/*     R1: 6*/
/*     R2: Cursor Addr*/
/*     R3: Bind variable index (starting from 0)*/
/*     R4: Value to give the bind variable*/
/*     R5: var, for returned status*/
/*  NOTES:*/
/*     set the bind variables' values. */
/*     allocate memory if it is needed(status == 3)*/
/*-----------------------------------------------------------------------------*/
void SetBindVal()
{
  RETCODE rc;
  struct Cursor *cur = (struct Cursor *)ptoc_int(2);
  int j = ptoc_int(3);
  Cell BindVal = ptoc_tag(4);

  if (!((j >= 0) && (j < cur->NumBindVars)))
    xsb_exit("Abnormal argument in SetBindVal!");
    
  /* if we're reusing an opened cursor w/ the statement number*/
  /* reallocate BindVar if type has changed (May not be such a good idea?)*/
  if (cur->Status == 2) {
    if (isinteger(BindVal)) {
      if (cur->BindTypes[j] != 0) {
	if (cur->BindTypes[j] != 2) free((void *)cur->BindList[j]);
	cur->BindList[j] = (UCHAR *)malloc(sizeof(int));
	cur->BindTypes[j] = 0;
	rc = SQLBindParameter(cur->hstmt, (short)(j+1), SQL_PARAM_INPUT, 
			      SQL_C_SLONG, SQL_INTEGER,
			      0, 0, (int *)(cur->BindList[j]), 0, NULL);
	if (rc != SQL_SUCCESS) {
	  ctop_int(5,PrintErrorMsg(cur));
	  SetCursorClose(cur);
	  return;
	}
      }
      *((int *)cur->BindList[j]) = int_val(BindVal);
    } else if (isfloat(BindVal)) {
      if (cur->BindTypes[j] != 1) { 
	/*printf("ODBC: Changing Type: flt to %d\n",cur->BindTypes[j]);*/
	if (cur->BindTypes[j] != 2) free((void *)cur->BindList[j]);
	cur->BindList[j] = (UCHAR *)malloc(sizeof(float));
	cur->BindTypes[j] = 1;
	rc = SQLBindParameter(cur->hstmt, (short)(j+1), SQL_PARAM_INPUT, 
			      SQL_C_FLOAT, SQL_FLOAT,
			      0, 0, (float *)(cur->BindList[j]), 0, NULL);
	if (rc != SQL_SUCCESS) {
	  ctop_int(5,PrintErrorMsg(cur));
	  SetCursorClose(cur);
	  return;
	}
      }
      *((float *)cur->BindList[j]) = (float)float_val(BindVal);
    } else if (isstring(BindVal)) {
      if (cur->BindTypes[j] != 2) { 
	/*printf("ODBC: Changing Type: str to %d\n",cur->BindTypes[j]);*/
	free((void *)cur->BindList[j]);
	cur->BindTypes[j] = 2;
	/* SQLBindParameter will be done anyway*/
      }
      cur->BindList[j] = string_val(BindVal);
    } else {
      xsb_exit("Unknown bind variable type, %d", cur->BindTypes[j]);
    }
    ctop_int(5,0);
    return;
  }
    
  /* otherwise, memory needs to be allocated in this case*/
  if (isinteger(BindVal)) {
    cur->BindTypes[j] = 0;
    cur->BindList[j] = (UCHAR *)malloc(sizeof(int));
    if (!cur->BindList[j])
      xsb_exit("Not enough memory for an int in SetBindVal!");
    *((int *)cur->BindList[j]) = int_val(BindVal);
  } else if (isfloat(BindVal)) {
    cur->BindTypes[j] = 1;
    cur->BindList[j] = (UCHAR *)malloc(sizeof(float));
    if (!cur->BindList[j])
      xsb_exit("Not enough memory for a float in SetBindVal!");
    *((float *)cur->BindList[j]) = (float)float_val(BindVal);
  } else if (isstring(BindVal)) {
    cur->BindTypes[j] = 2;
    cur->BindList[j] = string_val(BindVal);
  } else {
    xsb_exit("Unknown bind variable type, %d", cur->BindTypes[j]);
  }
  ctop_int(5,0);
  return;
}


/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     Parse()*/
/*  PARAMETERS:*/
/*     R1: 2*/
/*     R2: the SQL statement for the cursor*/
/*     R3: var, returned cursor address*/
/*  NOTES:*/
/*     parse the sql statement and submit it to DBMS to execute.  if all these*/
/*     succeed, then prepare for resulting row fetching.  this includes*/
/*     determination of column number in the resulting rowset and the length*/
/*     of each column and memory allocation which is used to store each row.*/
/*-----------------------------------------------------------------------------*/
void Parse()
{
  int j;
  struct Cursor *cur = (struct Cursor *)ptoc_int(2);
  RETCODE rc;

  if (cur->Status == 2) { /* reusing opened cursor*/
    rc = SQLCancel(cur->hstmt);
    if ((rc != SQL_SUCCESS) && (rc != SQL_SUCCESS_WITH_INFO)) {
      ctop_int(3, PrintErrorMsg(cur));
      SetCursorClose(cur);
      return;
    }
    /* reset just char select vars, since they store addr of chars*/
    for (j = 0; j < cur->NumBindVars; j++) {
      if (cur->BindTypes[j] == 2)
	rc = SQLBindParameter(cur->hstmt, (short)(j+1), SQL_PARAM_INPUT, SQL_C_CHAR, 
			      SQL_CHAR, 0, 0,(char *) cur->BindList[j], 0, &SQL_NTSval);
    }
  } else {
    if (SQLPrepare(cur->hstmt, cur->Sql, SQL_NTS) != SQL_SUCCESS) {
      ctop_int(3,PrintErrorMsg(cur));
      SetCursorClose(cur);
      return;
    }
    
    /* set the bind variables*/
    for (j = 0; j < cur->NumBindVars; j++) {
      if (cur->BindTypes[j] == 2)
	/* we're sloppy here.  it's ok for us to use the default values*/
	rc = SQLBindParameter(cur->hstmt, (short)(j+1), SQL_PARAM_INPUT, SQL_C_CHAR, 
			      SQL_CHAR, 0, 0,(char *)cur->BindList[j], 0, &SQL_NTSval);
      else if (cur->BindTypes[j] == 1) {
	rc = SQLBindParameter(cur->hstmt, (short)(j+1), SQL_PARAM_INPUT, SQL_C_FLOAT, SQL_FLOAT,
			 0, 0, (float *)cur->BindList[j], 0, NULL);
      } else
	rc = SQLBindParameter(cur->hstmt, (short)(j+1), SQL_PARAM_INPUT, SQL_C_SLONG, SQL_INTEGER,
			 0, 0, (int *)(cur->BindList[j]), 0, NULL);
      if (rc != SQL_SUCCESS) {
	ctop_int(3,PrintErrorMsg(cur));
	SetCursorClose(cur);
	return;
      }
    }
  }
  /* submit it for execution*/
  if (SQLExecute(cur->hstmt) != SQL_SUCCESS) {
    ctop_int(3,PrintErrorMsg(cur));
    SetCursorClose(cur);
    return;
  }
  ctop_int(3,0);
  return;
}

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     ODBCCommit()*/
/*  PARAMETERS:*/
/*     R1: 10*/
/*     R2: Connection Handle*/
/*     R3: var, returned status, 0 if successful*/
/*-----------------------------------------------------------------------------*/
void ODBCCommit()
{
  struct Cursor *cur = FCursor;
  HDBC hdbc = (HDBC)ptoc_int(2);
  RETCODE rc;

  if (((rc=SQLTransact(henv,hdbc,SQL_COMMIT)) == SQL_SUCCESS) ||
      (rc == SQL_SUCCESS_WITH_INFO)) { 
    /* close only those with right hdbc....*/
    while (cur != NULL) {
      if (cur->hdbc == hdbc && cur->Status != 0) SetCursorClose(cur);
      cur = cur->NCursor;
    }
    ctop_int(3,0);
  } else
    ctop_int(3,PrintErrorMsg(NULL));
  return;
}

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     ODBCRollback()*/
/*  PARAMETERS:*/
/*     R1: 11*/
/*     R2: Connection Handle*/
/*     R3: var, for returned status: 0 if successful*/
/*-----------------------------------------------------------------------------*/
void ODBCRollback()
{
  struct Cursor *cur = FCursor;
  HDBC hdbc = (HDBC)ptoc_int(2);
  RETCODE rc;

  if (((rc=SQLTransact(henv,hdbc,SQL_ROLLBACK)) == SQL_SUCCESS) ||
      (rc == SQL_SUCCESS_WITH_INFO)) {
    /* only close those with right hdbc*/
    while (cur != NULL) {
      if (cur->hdbc == hdbc && cur->Status != 0) SetCursorClose(cur);
      cur = cur->NCursor;
    }
    ctop_int(3,0);
  } else
    ctop_int(3, PrintErrorMsg(NULL));
  return;
}

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     ODBCColumns()*/
/*  PARAMETERS:*/
/*     R1: 12*/
/*     R2: Cursor Index*/
/*     R3: Table name*/
/*     R4: var for returned status: 0 if successful*/
/*-----------------------------------------------------------------------------*/
void ODBCColumns()
{
  struct Cursor *cur = (struct Cursor *)ptoc_int(2);
  char tmpstr[255];
  char *str1, *str2, *str3;
  RETCODE rc;

  strcpy(tmpstr,ptoc_string(3));
  str1 = strtok(tmpstr,".");
  str2 = str3 = NULL;
  if (str1) str2 = strtok(NULL,".");
  if (str2) str3 = strtok(NULL,".");
  if (!str3 && !str2) {str3 = str1; str1 = NULL;}
  else if (!str3) {str3 = str2; str2 = NULL;}
  /*  printf("str1 %s, str2 %s, str3 %s\n",str1,str2,str3);*/
  if (((rc=SQLColumns(cur->hstmt,
		      str1, SQL_NTS,
		      str2, SQL_NTS,
		      str3, SQL_NTS,
		      NULL,0)) == SQL_SUCCESS) ||
      (rc == SQL_SUCCESS_WITH_INFO)) {
    ctop_int(4,0);
  } else {
    ctop_int(4,PrintErrorMsg(cur));
    SetCursorClose(cur);
  }
  return; 
} 

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     ODBCTables()*/
/*  PARAMETERS:*/
/*     R1: 13*/
/*     R2: Cursor*/
/*     R3: var, returned status: 0 if successful*/
/*-----------------------------------------------------------------------------*/
void ODBCTables()
{
  struct Cursor *cur = (struct Cursor *)ptoc_int(2);
  RETCODE rc;

  if (((rc=SQLTables(cur->hstmt,
		     NULL, 0,
		     NULL, 0,
		     NULL, 0,
		     NULL, 0)) == SQL_SUCCESS) ||
      (rc == SQL_SUCCESS_WITH_INFO)) 
    ctop_int(3,0);
  else {
    ctop_int(3,PrintErrorMsg(cur));
    SetCursorClose(cur);
  }
  return; 
}

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     ODBCUserTables()*/
/*  PARAMETERS:*/
/*     R1: 14*/
/*     R2: Cursor*/
/*     R3: var, for returned status: 0 if successful*/
/*-----------------------------------------------------------------------------*/
void ODBCUserTables()
{
  struct Cursor *cur = (struct Cursor *)ptoc_int(2);
  UWORD TablePrivilegeExists;
  RETCODE rc;

  /* since some ODBC drivers don't implement the function SQLTablePrivileges*/
  /* we check it first*/
  SQLGetFunctions(cur->hdbc,SQL_API_SQLTABLEPRIVILEGES,&TablePrivilegeExists);
  if (!TablePrivilegeExists) {
    printf("Privilege concept does not exist in this DVMS: you probably can access any of the existing tables\n");
    ctop_int(3, 2);
    return;
  }
  if (((rc=SQLTablePrivileges(cur->hstmt,
			      NULL, 0,
			      NULL, 0,
			      NULL, 0)) == SQL_SUCCESS) ||
      (rc == SQL_SUCCESS_WITH_INFO)) 
    ctop_int(3,0);
  else {
    ctop_int(3,PrintErrorMsg(cur));
    SetCursorClose(cur);
  }
  return; 
}

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     DisplayColSize()*/
/*  PARAMETERS:*/
/*     SWORD coltype - column type which is a single word.*/
/*     UDWORD collen - column length which is returned by SQLDescribeCol*/
/*     UCHAR *colname - pointer to column name string*/
/*  RETURN VALUE:*/
/*     column length - the size of memory that is needed to store the column*/
/*     value for supported column types*/
/*     0             - otherwise*/
/*-----------------------------------------------------------------------------*/
UDWORD DisplayColSize(SWORD coltype, UDWORD collen, UCHAR *colname)
{
  switch (ODBCToXSBType(coltype)) {
  case SQL_C_SLONG: 
    return sizeof(long *);
  case SQL_C_CHAR: {
    UDWORD tmp = MAXI(collen+1, strlen((char *) colname));
    if (tmp < MAXVARSTRLEN) return tmp;
    else return MAXVARSTRLEN;
  }
  case SQL_C_FLOAT:
    return sizeof(float *);
  default: 
    printf("Illegal ODBC Type: %d\n",coltype);
    return 0;
  }
}


/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     DescribeSelect()*/
/*  PARAMETERS:*/
/*     R1: 15*/
/*     R2: Ptr to cursor record*/
/*     R3: var, for returned status:*/
/*         0 - the result row has at least one column and*/
/*         1 - something goes wrong, we can't retrieve column information, */
/*             memory allocation fails (if this happens program stops).*/
/*         2 - no column is affected*/
/*  NOTES:*/
/*     memory is also allocated for future data storage*/
/*-----------------------------------------------------------------------------*/
void ODBCDescribeSelect()
{
  int j;
  UCHAR colname[50];
  SWORD colnamelen;
  SWORD scale;
  SWORD nullable;
  UDWORD collen;
  struct Cursor *cur = (struct Cursor *)ptoc_int(2);

  cur->NumCols = 0;
  SQLNumResultCols(cur->hstmt, (SQLSMALLINT*)&(cur->NumCols));
  if (!(cur->NumCols)) {
    /* no columns are affected, set cursor status to unused */
    cur->Status = 1; 
    ctop_int(3,2);
    return;
  }
  /* if we aren't reusing a closed statement handle, we need to get*/
  /* resulting rowset info and allocate memory for it*/
  if (cur->Status != 2) {
    cur->ColTypes =
      (SWORD *)malloc(sizeof(SWORD) * cur->NumCols);
    if (!cur->ColTypes)
      xsb_exit("Not enough memory for ColTypes!");
    
    cur->Data =
      (UCHAR **)malloc(sizeof(char *) * cur->NumCols);
    if (!cur->Data)
      xsb_exit("Not enough memory for Data!");

    cur->OutLen =
      (UDWORD *)malloc(sizeof(UDWORD) * cur->NumCols);
    if (!cur->OutLen)
      xsb_exit("Not enough memory for OutLen!");

    cur->ColLen =
      (UDWORD *)malloc(sizeof(UDWORD) * cur->NumCols);
    if (!cur->ColLen)
      xsb_exit("Not enough memory for ColLen!");
    
    for (j = 0; j < cur->NumCols; j++) {
      SQLDescribeCol(cur->hstmt, (short)(j+1), (UCHAR FAR*)colname,
		     sizeof(colname), &colnamelen,
		     &(cur->ColTypes[j]),
		     &collen, &scale, &nullable);
      /* SQLServer returns this wierd type for a system table, treat it as varchar?*/
      if (cur->ColTypes[j] == -9) cur->ColTypes[j] = SQL_VARCHAR;
      colnamelen = (colnamelen > 49) ? 49 : colnamelen; 
      colname[colnamelen] = '\0';
      if (!(cur->ColLen[j] =
	    DisplayColSize(cur->ColTypes[j],collen,colname))) {
	/* let SetCursorClose function correctly free all the memory allocated*/
	/* for Data storage: cur->Data[j]'s*/
	cur->NumCols = j; /* set so close frees memory allocated thus far*/
	SetCursorClose(cur);
	/*	return(1);*/
	ctop_int(3,1);
	return;
      }
      cur->Data[j] =
	(UCHAR *) malloc(((unsigned) cur->ColLen[j]+1)*sizeof(UCHAR));
      if (!cur->Data[j])
	xsb_exit("Not enough memory for Data[j]!");
    }
  }
  /* bind them*/
  for (j = 0; j < cur->NumCols; j++) {
    SQLBindCol(cur->hstmt, (short)(j+1), 
	       ODBCToXSBType(cur->ColTypes[j]), cur->Data[j],
	       cur->ColLen[j], (SDWORD FAR *)(&(cur->OutLen[j])));
  }
  /*  return 0;*/
  ctop_int(3,0);
  return;
}

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     FetchNextRow()*/
/*  PARAMETERS:*/
/*     R1: 4*/
/*     R2: Cursor Address*/
/*     R3: var, for returned status*/
/*         0 => successful, a row is read and available*/
/*         1 => successful, no row avaliable*/
/*         2 => unsuccessful, an error occurred in fetching.*/
/*  NOTES:*/
/*     fetch next row of result rowset.*/
/*-----------------------------------------------------------------------------*/
void FetchNextRow()
{
  struct Cursor *cur = (struct Cursor *)ptoc_int(2);
  RETCODE rc;

  if (!serverConnected || cur->Status == 0) {
    ctop_int(3,2);
    return;
  }

  rc = SQLFetch(cur->hstmt);

  if ((rc == SQL_SUCCESS) || (rc == SQL_SUCCESS_WITH_INFO)) 
    ctop_int(3,0);
  else if (rc == SQL_NO_DATA_FOUND){
    cur->Status = 1; /* done w/fetching. set cursor status to unused */
    ctop_int(3,1);
  }
  else {
    SetCursorClose(cur);         /* error occured in fetching*/
    ctop_int(3,2);
  }
  return;
} 

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     ODBCConnectOption() */
/*  PARAMETERS:*/
/*     R1: 16*/
/*     R2: Connection Handle*/
/*     R3: 0 -> get; 1 -> set*/
/*     R4: Option indicator (int)*/
/*     R5: if R2=0 -> variable that attr val is returned in*/
/*         if R2=1 -> value used to set attr*/
/*     R6: Return Code*/
/*  NOTES:*/
/*     either gets value of a connection option, or sets it.*/
/*-----------------------------------------------------------------------------*/
void ODBCConnectOption()
{
  HDBC hdbc = (HDBC)ptoc_int(2);
  int set = ptoc_int(3);
  long value = 0;
  RETCODE rc;

  if (set) {
    rc = SQLSetConnectOption(hdbc,(UWORD)ptoc_int(4),(UDWORD)ptoc_int(5));
  } else {
    rc = SQLGetConnectOption(hdbc,(UWORD)ptoc_int(4),(PTR)&value);
    ctop_int(5,value);
  }
  if ((rc == SQL_SUCCESS) || (rc == SQL_SUCCESS_WITH_INFO)) 
    ctop_int(6,0);
  else ctop_int(6,PrintErrorMsg(NULL));
}

extern xsbBool unify(Cell, Cell);

/*-----------------------------------------------------------------------------*/
/*  FUNCTION NAME:*/
/*     GetColumn() */
/*  PARAMETERS:*/
/*     R1: 5*/
/*     R2: Cursor*/
/*     R3: Column Index*/
/*     R4: Column Value returned*/
/*     R5: Return Code*/
/*  NOTES:*/
/*     get the indicated column.*/
/*-----------------------------------------------------------------------------*/
int GetColumn()
{
  struct Cursor *cur = (struct Cursor *)ptoc_int(2);
  int ColCurNum = ptoc_int(3);
  Cell op = ptoc_tag(4);
  UDWORD len;

  if (ColCurNum < 0 || ColCurNum >= cur->NumCols) {
    /* no more columns in the result row*/
    ctop_int(5,1);   
    return TRUE;
  }

  ctop_int(5,0);

  /* get the data*/
  if (cur->OutLen[ColCurNum] == SQL_NULL_DATA) {
    /* column value is NULL*/
    return unify(op,nullStrAtom);
  }

  /* convert the string to either integer, float or string*/
  /* according to the column type and pass it back to XSB*/
  switch (ODBCToXSBType(cur->ColTypes[ColCurNum])) {
  case SQL_C_CHAR:
    /* convert the column string to a C string */
    len = ((cur->ColLen[ColCurNum] < cur->OutLen[ColCurNum])?
	   cur->ColLen[ColCurNum]:cur->OutLen[ColCurNum]);
    *(cur->Data[ColCurNum]+len) = '\0';

    /* compare strings here, so don't intern strings unnecessarily*/
    XSB_Deref(op);
    if (isref(op)) 
      return unify(op, makestring(string_find(cur->Data[ColCurNum],1))); 
    if (!isstring(op)) return FALSE;
    if (strcmp(string_val(op),cur->Data[ColCurNum])) return FALSE;
    return TRUE;
  case SQL_C_SLONG:
    return unify(op,makeint(*(long *)(cur->Data[ColCurNum])));
  case SQL_C_FLOAT:
    return unify(op,makefloat(*(float *)(cur->Data[ColCurNum])));
  }

  return FALSE;
}

