
/* File: db_interface.H
** Author: Saikat Mukherjee
** Contact: saikat@cs.sunysb.edu

** This file declares the export/imports of the db_interface module
*/


:- import
	openConnection/6,
	queryConnection/4,
	closeConnection/1,
	prepareStatement/3,
	executePreparedStatement/3,
	closeStatement/1,
	exception/2
   from driver_manager.
			

:- export
	db_connect/6,
	db_connect/5,
	db_disconnect/1,
	db_sql/4,
	db_prepare/3,
	db_prepare_execute/3,
	db_statement_close/1.


