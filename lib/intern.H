/* File:      intern.P
** Author(s): Prasad Rao
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


:- export new_trie/1, trie_intern/5, trie_intern/4, trie_interned/4,
	  trie_intern/3, trie_interned/3, trie_unintern/2, delete_trie/1,
	  trie_set_last_int/2,
	  trie_unintern_nr/2, trie_reclaim_uninterned_nr/1, 
	  unmark_uninterned_nr/2. 

:- import integer/1, abort/1 from standard.
:- import get_lastnode_cs_retskel/4 from tables.

%%%:- import buff_word/3, buff_set_word/3  from machine.
