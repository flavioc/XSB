/* File:      xsb_odbc.h
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

extern void ODBCConnect();
extern void ODBCDisconnect();
extern void SetBindVarNum();
extern void FindFreeCursor();
extern void SetVar();
extern void SetBind();
extern void Parse();
extern void GetColumn();
extern void SetCursorFree(int);
extern void FetchNextCol();

#define ODBC_CONNECT             1
#define ODBC_PARSE               2
#define ODBC_SET_BIND_VAR_NUM    3
#define ODBC_FETCH_NEXT_COL      4
#define ODBC_GET_COLUMN          5
#define ODBC_SET_VAR             6
#define ODBC_FIND_FREE_CURSOR    7
#define ODBC_DISCONNECT          8
#define ODBC_SET_BIND            9

#endif

