/* File:      intern.P
** Author(s): Prasad Rao, Hasan Davulcu
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1998-2000
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


:- export storage_insert_fact_bt/3, storage_delete_fact_bt/3, 
	storage_insert_fact/3, storage_delete_fact/3,
	storage_delete_all/1,
	storage_find_fact/2,
	storage_reclaim_space/1,
	storage_reclaim_space/0,
	storage_insert_keypair_bt/4, storage_delete_keypair_bt/3,
	storage_insert_keypair/4, storage_delete_keypair/3,
	storage_find_keypair/3.

:- import trie_intern/5, trie_interned/4, trie_unintern_nr/2,
	unmark_uninterned_nr/2, new_trie/1,
	delete_trie/1,
	trie_reclaim_uninterned_nr/1
   from intern.

:- import assert/1, (dynamic)/1 from assert.

:- import conget/2, conset/2 from gensym.

:- import abort/1  from standard.
