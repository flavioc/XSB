/* File: driver_manager.H
** Author: Saikat Mukherjee
** Contact: saikat@cs.sunysb.edu

** This is the header file for exporting 
** the C implementations of the functions defined in the driver_manager module
*/

:- export
 	initialise/0,
	openConnection/6, 
	queryConnection/4,
	closeConnection/1,
	prepareStatement/3,
	executePreparedStatement/3,
	closeStatement/1,
	exception/2.

