/* File:      flrprint.H
**
** Author(s): Guizhen Yang
**
** Contact:   xsb-contact@cs.sunysb.edu
**
** Copyright (C) The Research Foundation of SUNY, 2000
** 
** FLORA-2 is free software; you can redistribute it and/or modify it under the
** terms of the GNU Library General Public License as published by the Free
** Software Foundation; either version 2 of the License, or (at your option)
** any later version.
** 
** FLORA-2 is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
** FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
** more details.
** 
** You should have received a copy of the GNU Library General Public License
** along with FLORA-2; if not, write to the Free Software Foundation,
** Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
**
**
*/


:- import fmt_write/3 from file_io.

:- import file_nl/1 from xsb_writ.

:- export
	flora_stderr_nl/0,
	flora_stderr_string/1,
	flora_stderr_string/2,
	flora_stdwarn_nl/0,
	flora_stdwarn_string/1,
	flora_stdwarn_string/2,
	flora_stdmsg_nl/0,
	flora_stdmsg_string/1,
	flora_stdmsg_string/2,
	flora_stdmsg_line/1,
	flora_stdfdbk_nl/0,
	flora_stdfdbk_string/1,
	flora_stdfdbk_string/2,
	flora_stdfdbk_line/1,
	flora_error_heading/0,
	flora_error_indentline/0,
	flora_warning_heading/0,
	flora_warning_indentline/0,
	flora_message_heading/0,
	flora_message_tailing/0,
	flora_error_line/1,
	flora_error_line/2,
	flora_warning_line/1,
	flora_warning_line/2,
	flora_message_line/1,
	flora_message_line/2.
