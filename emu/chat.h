/* File:      chat.h
** Author(s): Bart Demoen, Kostis Sagonas
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) K.U. Leuven and the Research Foundation of SUNY, 1998
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

#define CHAT_CONS_AREA  0
#define CHAT_INCR_AREA  1

typedef struct init_chat_header
{ struct consumer_choice_point consumer_tcp; /* only the fixed part */

  short nrarguments; /* number of saved objects in consumer cp - short is enough */

  CPtr *malloc_start;  /* the return from malloc */
  CPtr *args_start ;   /* properly alligned */

  struct incr_chat_header *father_block_start;

  /* next two to link initial chat areas together */
  struct init_chat_header *next_header;
  struct init_chat_header *prev_header;

} init_chat_header ;

typedef struct incr_chat_header
{ int itr_len;  /* the number of CPtrs for the addresses and values */

  short imark_bit; /* only one bit needed */

  CPtr *imalloc_start, *itr_start ;

  struct incr_chat_header *ifather_block_start;

  int reference_counter;

} incr_chat_header ;

typedef init_chat_header *chat_init_pheader;
typedef incr_chat_header *chat_incr_pheader;

#define chat_set_nrargs(phead,i)         (phead)->nrarguments = i
#define chat_get_nrargs(phead)           (phead)->nrarguments

#define chat_set_chain(phead,p)          (phead)->next_header = p

#define chat_get_cons_start(phead)	 (phead)->consumer_tcp
#define chat_get_cons_length(phead)      (phead)->cons_len

#define chat_get_tr_start(phead)         (phead)->itr_start
#define chat_get_tr_length(phead)        (phead)->itr_len

#define chat_get_malloc_start(phead)     (phead)->malloc_start
#define chat_get_imalloc_start(phead)    (phead)->imalloc_start

#define chat_get_args_start(phead)       (phead)->args_start

#define chat_set_malloc_start(phead,i)   (phead)->malloc_start = i
#define chat_set_imalloc_start(phead,i)  (phead)->imalloc_start = i

#define chat_get_father(phead)		(phead)->father_block_start
#define chat_get_ifather(phead)		(phead)->ifather_block_start

#define chat_set_father(phead,p)	 chat_get_father(phead) = p
#define chat_set_ifather(phead,p)	 chat_get_ifather(phead) = p
#define chat_increment_reference(phead)  ((phead)->reference_counter)++

#define chat_area_imarked(phead)         ((phead)->imark_bit != 0)
#define chat_imark_area(phead)           (phead)->imark_bit = 1
#define chat_iunmark_area(phead)         (phead)->imark_bit = 0
#define chat_init_imark_bit(phead)       (phead)->imark_bit = 0

/*----------------------------------------------------------------------*/

typedef struct chat_root *CRPtr;

struct chat_root {
  chat_incr_pheader root_area;  /* always an incremental CHAT area */
  CRPtr next;
} ;

#define cr_root_area(CR)           (CR)->root_area
#define cr_next(CR)                (CR)->next

/*----------------------------------------------------------------------*/

extern void abolish_cr_space(void);
extern void reclaim_cr_space(TChoice);
extern void print_chat_statistics(void);
extern void reset_chat_statistics(void);
extern void chat_free_cons_chat_areas(SGFrame);
extern void chat_free_compl_susp_chat_areas(SGFrame);
extern CPtr chat_restore_consumer(chat_init_pheader);
extern CPtr chat_restore_compl_susp(chat_init_pheader,CPtr,CPtr);
extern void chat_update_compl_susp(chat_init_pheader);
extern void chat_restore_compl_susp_trail(chat_init_pheader);
extern chat_init_pheader save_a_consumer_copy(SGFrame,int);
extern chat_init_pheader save_a_chat_compl_susp(int,CPtr,SGFrame,CPtr,byte *);

extern void chat_set_chained(CPtr);
extern void chat_set_unchained(CPtr);
extern int chat_is_chained(CPtr);

#ifdef LOCAL_EVAL
extern chat_init_pheader save_a_consumer_for_generator(SGFrame);
#endif

extern chat_init_pheader chat_link_headers;

/* the following are for debugging purposes only and will be taken out */

#ifdef Chat_DEBUG
extern int chat_check_memory(chat_pheader);
#endif

/*------------------------- end of file chat.h --------------------------*/
