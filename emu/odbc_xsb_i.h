/* File:      odbc_xsb_i.h
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


#ifdef XSB_ODBC

case ODBC_EXEC_QUERY:
switch (ptoc_int(1)) {
 case ODBC_CONNECT:
   ODBCConnect();
   break;
 case ODBC_PARSE:
   Parse();
   break;
 case ODBC_SET_BIND_VAR_NUM:
   SetBindVarNum();
   break;
 case ODBC_FETCH_NEXT_ROW:
   FetchNextRow();
   break;
 case ODBC_GET_COLUMN:
   return GetColumn();
   break;
 case ODBC_SET_BIND_VAL:
   SetBindVal();
   break;
 case ODBC_FIND_FREE_CURSOR:
   FindFreeCursor();
   break;
 case ODBC_DISCONNECT:
   ODBCDisconnect();
   break;
 case ODBC_SET_CURSOR_CLOSE: {
   int i = ptoc_int(2);
   SetCursorClose(i);
   break;
 }
 case ODBC_COMMIT:
   ODBCCommit();
   break;
 case ODBC_ROLLBACK:
   ODBCRollback();
   break;
 case ODBC_COLUMNS:
   ODBCColumns();
   break;
 case ODBC_TABLES:
   ODBCTables();
   break;
 case ODBC_USER_TABLES:
   ODBCUserTables();
   break;
 case ODBC_DESCRIBE_SELECT:
   ODBCDescribeSelect();
   break;
 case ODBC_CONNECT_OPTION:
   ODBCConnectOption();
   break;
 case ODBC_DATA_SOURCES:
   ODBCDataSources();
   break;
 case ODBC_GET_INFO:
   ODBCGetInfo();
   break;
 default:
   xsb_error("Unknown or unimplemented ODBC request type");
   /* Put an error message here */
   break;
}
break;

#endif


