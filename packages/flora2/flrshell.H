/* File:      flrshell.P  -- The Flora interactive shell
**
** Author(s): Bertram Ludaescher
**            Michael Kifer
**            Guizhen Yang
**
** Contact:   xsb-contact@cs.sunysb.edu
**
** Copyright (C) The Research Foundation of SUNY, 1999-2001
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



:- import
	abort/0,
	push_abort_cutpoint/0,
	pop_abort_cutpoint/0,
	call/1
   from standard.

:- import consult/1 from consult.

:- import member/2 from basics.

:- import findall/3 from setof.

:- import assert/1, retractall/1 from assert.

:- import fmt_write_string/3 from file_io.

:- import slash/1 from machine.

:- import cwd/1, cd/1 from shell.

:- import package_configuration/2 from packaging.

:- import conset/2 from gensym.

:- import pop_stdin/1 from push_io.

:- import
	add_xsb_hook/1,
	remove_xsb_hook/1,
	xsb_undefined_predicate_hook/1
   from xsb_hook.

:- import flora_undefined_predicate_hook/1 from flrundefhook.

:- import
	unstrap_flora/0
   from flora2.

:- import flora_shreset_operator/0 from flroperator.

:- import flora_shreset_arguments/0 from flrsignature.

%%:- import flora_shreset_prolog/0 from flrprolog.

:- import flora_shreset_firstorder/0 from flrnohilog.

:- import
	flora_load_library/1,
	flora_load_system_module/1
   from flrlibman.

:- import
	flora_stdmsg_nl/0,
	flora_stdmsg_line/1,
	flora_stdmsg_string/2,
	flora_stdfdbk_line/0,
	flora_stdfdbk_string/1,
	flora_error_line/1
   from flrprint.

:- import
	flMaxerr/1,
	flHalt/0,
	flP2H/2,
	flCompileInclude/1,
	flloadtrailer/1,
	flora_shell_command_line/3,
	flora_load_module_internal/1,
	flora_good_command/1,
	flora_end_of_input/1,
	flora_check_filename/1
   from flrutils.

:- import flora_trace/0,
	flora_notrace/0
   from flrdebugger.

:- dynamic flora_switch(_).

:- index(flora_switch/1,trie).
