/* File:      odbc_call.H
** Author(s): Lily Dong, David S. Warren
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1993-1998
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


:- export odbc_open/3, odbc_open/4,
	odbc_close/0, odbc_close/1,
	odbc_shutdown/0,
	odbc_sql/3, odbc_sql/4,
	odbc_query/2, odbc_query/3,
	odbc_import/2, odbc_import/3,
	odbc_transaction/1, odbc_transaction/2,
	odbc_connect_option/5,
	odbc_get_schema/2, odbc_get_schema/3,
	odbc_show_schema/1, odbc_show_schema/2,
	odbc_user_tables/1, odbc_user_tables/2,
	odbc_insert/2, odbc_insert/3,
	odbc_delete/2, odbc_delete/3,
	odbc_flag/3,
	odbc_attach/2, odbc_attach/3,
	odbc_create_table/2, odbc_create_table/3,
	odbc_create_index/3, odbc_create_index/4,
	odbc_delete_table/1, odbc_delete_table/2,
	odbc_delete_view/1, odbc_delete_view/2,
	odbc_delete_index/2, odbc_delete_index/3.

% import some necessary predicates from library.

:- import call0/1, odbc_exec_query/6 from machine.
:- import conset/2, conget/2, coninc/1 from gensym.
:- import length/2, append/3, member/2 from basics.
