/* File:      flrhilogtable.H
**
** Author(s): Chang Zhao
**
** Contact:   flora-users@lists.sourceforge.net
**
** Copyright (C) The Research Foundation of SUNY, 1999-2001
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
** $Id$
**
*/


:- dynamic flora_hilogtable(_,_).
:- dynamic flora_not_tabled_head(_,_).

:- dynamic flora_tabled_all_registry(_,_).
:- dynamic flora_tabled_registry(_,_,_).
:- dynamic flora_not_tabled_registry(_,_,_).

:- index(flora_hilogtable/2,trie).
:- index(flora_not_tabled_head/2,trie).

:- import
	is_prdirective/2,
	is_prtable/3,
	is_prrule/3,
	is_pratomlit/3,
	is_prtermlit/4,
	get_fingerprint/6,
    flora_write_struct/2
   from flrcoder.

:- import
	flora_write_atom/1,
	flora_write_quoted_atom/1
   from flrporting.

:- import
	flora_error_line/1
   from flrprint.

:- export
	flora_hilogtable/2,
	flora_not_tabled_head/2,
	flora_reset_hilogtable/0,
	flora_reset_table_registries/1,
	flora_define_hilogtable/2,
	flora_shdefine_hilogtable/3,
	flora_define_not_tabled_head/2,
	flora_check_tabled_registry/3,
	flora_enter_tabled_registry/3,
	flora_check_not_tabled_registry/3,
	flora_enter_not_tabled_registry/3,
	flora_table_info/1.
