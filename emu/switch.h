/* File:      switch.h
** Author(s): The XSB Group
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

extern NODEptr free_trie_nodes;
extern NODEptr free_trie_space;
extern NODEptr top_trie_space;
extern NODEptr tra_free_trie_nodes;
extern NODEptr tra_free_trie_space;
extern NODEptr tra_top_trie_space; 
extern NODEptr bak_free_trie_nodes;
extern NODEptr bak_free_trie_space;
extern NODEptr bak_top_trie_space;
extern char *trie_node_chunk_ptr;
extern char *tra_trie_node_chunk_ptr;
extern char *bak_trie_node_chunk_ptr;
extern struct HASHhdr HASHroot, *HASHrootptr, tra_HASHroot; 





#define switch_to_trie_assert {\
   bak_free_trie_nodes = free_trie_nodes;\
   bak_free_trie_space = free_trie_space;\
   bak_top_trie_space = top_trie_space;\
   bak_trie_node_chunk_ptr = trie_node_chunk_ptr;\
   free_trie_nodes = tra_free_trie_nodes;\
   free_trie_space = tra_free_trie_space;\
   top_trie_space = tra_top_trie_space;\
   trie_node_chunk_ptr = tra_trie_node_chunk_ptr;\
   HASHrootptr = &tra_HASHroot;\
			}
			 

#define switch_from_trie_assert {\
   tra_free_trie_nodes = free_trie_nodes;\
   tra_free_trie_space = free_trie_space;\
   tra_top_trie_space = top_trie_space;\
   tra_trie_node_chunk_ptr = trie_node_chunk_ptr;\
   free_trie_nodes = bak_free_trie_nodes;\
   free_trie_space = bak_free_trie_space;\
   top_trie_space = bak_top_trie_space;\
   trie_node_chunk_ptr = bak_trie_node_chunk_ptr;\
   HASHrootptr = &HASHroot;\
                        }
 
