/* File:      findall.c
** Author(s): Bart Demoen
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
** Copyright (C) ECRC, Germany, 1990
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


#include <stdio.h>
#include <stdlib.h>

#include "configs/config.h"
#include "debugs/debug.h"

#include "auxlry.h"
#include "cell.h"    	     	 /* cell.h must be included before deref.h */
#include "deref.h"
#include "register.h"
#include "memory.h"
#include "psc.h"
#include "xsberror.h"
#include "heap.h"
#include "binding.h"
#include "subp.h"
#include "flags.h"
#include "loader.h"

/* copy term to heap included in this file, because of the
   functional similarities with findall - RFM 14/8/98 */
   
/* the next defines belong really elsewhere */

#define Areg(i) (reg + i)


/*
Findall copies its templates to a findall-heap.
This heap is allocated in chunks of FINDALL_CHUNCK_SIZE (Cell)entries.
Since more than one findall can be active at the same time, each findall
gets a number, determined by the global variables nextfree; this happens
in findall_init. The maximal number of active findalls is MAX_FINDALLS.

A solution list of findall is represented by its size and a pointer to the
beginning of the solution list and a pointer to the tail of this solution list.
The size is important for copying back to the heap, to ensure that there is
enough space, before we start copying.  The tail is a free variable.

One solution list or template can be in more than one chunk. Chuncks are
linked together by the first field in the chunk; this field is only needed
for the deallocation of the chunks, not for the copying itself.

Trailing of variables (to ensure proper sharing) is done on a special
purpose trail, which consists also of chuncks, linked together.

Everything is allocated dynamically, and freed asap.

findall_clean should be called at the start of every toplevel.
*/

#define FINDALL_CHUNCK_SIZE 4000 /* anything > MAX_ARITY+2 is good */
static int MAX_FINDALLS = 250 ;	/* make it larger if you want */

static int nextfree ; /* nextfree index in findall array */

/*
one invocation of findall is associated with one entry in the findall_solutions
array: we then call this entry active; the type of the entry is findall_solution_list
*/

typedef struct { CPtr	first_chunk ;	/* chunk in which solution list starts */
					/* the solution list starts at offset +1 */
		 CPtr   tail ;		/* tail of solution list - always a free var */
                 CPtr   current_chunk ;	/* most recent allocated chunk for this solution list */
                 CPtr   top_of_chunk ;	/* where next template can be copied to */
					/* points inside current_chunk */
		 int size ;		/* size is the size of the solution list - init = 1 */
					/* when entry not active, size = the next free entry */
					/* the last free entry, has size = -1, for overflow checking */
               } findall_solution_list ;

static findall_solution_list *findall_solutions, *current_findall ;

static CPtr gl_bot, gl_top ;
#define on_glstack(p) ((gl_bot <= p) && (p < gl_top))

/* get_more_chunk mallocs a new chunck and links it in in the current findall */

static int get_more_chunk()
{ CPtr newchunk ;

  if (!(newchunk = (CPtr)malloc(FINDALL_CHUNCK_SIZE * sizeof(Cell))))
    xsb_exit("get_more_chunk failed");

  *newchunk = 0 ;
  *(current_findall->current_chunk) = (Cell)newchunk ;
  current_findall->current_chunk = newchunk ;
  current_findall->top_of_chunk = newchunk + 1 ;

  return(1) ;

} /*get_more_chunk*/

/*
$$findall_init/2

	to be called with 2 free variables
	the first variable is bound to an index in the findall table
	the second remains free - it will be set to 666 by findall_get_solutions
	without trailing, so that later add's will not occur
*/

int findall_init()
{
  CPtr w ;
  findall_solution_list *p ;
  Cell arg1 ;
 
  if (findall_solutions == 0)
	{ int i ;
	  p = findall_solutions = (findall_solution_list *)
			malloc(MAX_FINDALLS*sizeof(findall_solution_list)) ;
	  if (findall_solutions == 0) xsb_exit("init of findall failed") ;
	  for (i = 0 ; i++ < MAX_FINDALLS ; p++)
		{ p->size = i ;
		  p->tail = 0 ;
		}
          (--p)->size = -1 ;
          nextfree = 0 ;
	}

  if (nextfree < 0) /* could realloc here - too lazy to do it */
	xsb_exit("findall: maximum number of active findalls reached");
  arg1 = (Cell)Areg(1) ; deref(arg1) ; *(CPtr)arg1 = makeint(nextfree) ;
	/* no checking - no trailing - just use findall_init correct :-) */
  p = findall_solutions + nextfree ;
  if (!(w = (CPtr)malloc(FINDALL_CHUNCK_SIZE * sizeof(Cell))))
	xsb_exit("no temp space for findall");

  *w = 0 ;
  p->first_chunk = p->current_chunk = w ;
  w++ ; bld_free(w) ; p->tail = w ; /* create an undef as init of tail */
  w++ ; p->top_of_chunk = w ;
  nextfree = p->size ;
  p->size = 1 ;
  return(1) ;
} /* findall_init */

/* findall_free is called to desactive an entry in the solution_list
   at the end of findall_get_solutions, and from findall_clean
*/

static void findall_free(int i)
{ CPtr to_free,p ;

  p = (findall_solutions + i)->current_chunk ;
  while (p != NULL)
	{ to_free = p ; p = (CPtr)(*p) ; free(to_free) ; }
  (findall_solutions + i)->tail = 0 ;
  (findall_solutions + i)->size = nextfree ;
  nextfree = i ;

} /*findall_free*/

/* findall_clean should be called after interrupts or jumps out of the
   interpreter - or just before jumping back into it
*/

void findall_clean()
{ findall_solution_list *p ;
  int i ;
	p = findall_solutions ;
	if (! p) return ;
	for (i = 0 ; i < MAX_FINDALLS ; i++)
		{ if (p->tail != 0)
			findall_free(i) ;
		  (findall_solutions + i)->size = i+1 ;
		}
	(findall_solutions + i - 1)->size = -1 ;
	nextfree = 0 ;

} /* findall_clean */

/* findall_copy_to_heap does not need overflow checking - heap space is ensured;
   variables in the findall solution list can be altered without problem, because
   they are not needed afterwards anymore, so no trailing
*/

static void findall_copy_to_heap(Cell from, CPtr to, CPtr *h)
{

copy_again : /* for tail recursion optimisation */

  switch ( cell_tag( from ) )
   {
	case INT :
	case FLOAT :
	case STRING :   *to = from ;
			return ;

	case REF :
	case REF1 :	deref(from) ;
			if (on_glstack((CPtr)(from))) *to = from ;
				else { *(CPtr)from = (Cell)to ; *to = (Cell)to ; }
			return ;

      case LIST :
              {	CPtr pfirstel ;

		*to = makelist(*h) ;
		to = (*h) ;
		(*h) += 2 ;
		pfirstel = clref_val(from) ;
		from = *pfirstel ; /* deref(from) ; */
		findall_copy_to_heap(from,to,h) ;
		from = *(pfirstel+1) ; deref(from) ; to++ ;
		goto copy_again ;
             }

      case CS :
              {	CPtr pfirstel ;
		int ar ;

		pfirstel = (CPtr)cs_val(from) ;

		*to = makecs((Cell)(*h)) ;
		to = *h ;
		*to = *pfirstel ; /* the functor */

		ar = get_arity((Psc)(*pfirstel)) ;
		*h += ar + 1 ;
		while ( --ar )
                   { from = *(++pfirstel) ; /* deref(from) ; */ to++ ;
                     findall_copy_to_heap(from,to,h) ;
                   }
		from = *(++pfirstel) ; /* deref(from) ; */ to++ ;
		goto copy_again ;
             }
   }

} /*findall_copy_to_heap*/


/* trailing variables during copying a template: a linked list of arrays is used */

#define F_TR_NUM 250 /* the number of trail entries in a chunck of the trail */

typedef struct f_tr_chunk
		{ struct f_tr_chunk *previous ;
		  CPtr tr[F_TR_NUM] ;
		} f_tr_chunk ;

static f_tr_chunk *cur_tr_chunk ;
static CPtr *cur_tr_top ;
static CPtr *cur_tr_limit ;

static void findall_untrail()
{ CPtr *p, *begin_trail ;
  f_tr_chunk *tr_chunk, *tmp ;

	if( !(tr_chunk = cur_tr_chunk) ) return ; /* protection */
	begin_trail = tr_chunk->tr ;

	for (p = cur_tr_top ; p-- > begin_trail ; ) bld_free((*p)) ;

	tmp = tr_chunk ; tr_chunk = tr_chunk->previous ; free(tmp) ;
	while (tr_chunk != 0)
	{ begin_trail = tr_chunk->tr ;
	  for (p = tr_chunk->tr + F_TR_NUM ; p-- > begin_trail ; )  bld_free((*p)) ;
	  tmp = tr_chunk ; tr_chunk = tr_chunk->previous ; free(tmp) ;
	}
} /* findall_untrail */

static int findall_trail(CPtr p)
{ f_tr_chunk *new_tr_chunk ;

	if (cur_tr_top == cur_tr_limit)
		{ if (!(new_tr_chunk = (f_tr_chunk *)malloc(sizeof(f_tr_chunk))))
			xsb_exit("findall_trail failed");
		  cur_tr_top = new_tr_chunk->tr ;
		  cur_tr_limit = new_tr_chunk->tr+F_TR_NUM ;
		  new_tr_chunk->previous = cur_tr_chunk ;
		  cur_tr_chunk = new_tr_chunk ;
		}
	*(cur_tr_top++) = p ;
	return(1) ;
} /* findall_trail */

static int init_findall_trail()
{
	if (!(cur_tr_chunk = (f_tr_chunk *)malloc(sizeof(f_tr_chunk))))
		xsb_exit("init_findall_trail failed");
	cur_tr_top = cur_tr_chunk->tr ;
	cur_tr_limit = cur_tr_chunk->tr+F_TR_NUM ;
	cur_tr_chunk->previous = 0 ;
	return(1) ;
} /* init_findall_trail */

/* findall_copy_template_to_chunk
		must do: overflow checking
			 trailing of variables
			 returns size of copied term
  if it "fails", returns a negative number
*/

static int findall_copy_template_to_chunk(Cell from, CPtr to, CPtr *h)
{ int size = 0 ;
  int s ;

copy_again : /* for tail recursion optimisation */

  switch ( cell_tag( from ) )
   {
	case INT :
	case FLOAT :
	case STRING :	*to = from ; return(size) ;

	case REF :
	case REF1 :	if (on_glstack((CPtr)(from))) 
				{ findall_trail((CPtr)from) ;
				  *(CPtr)from = (Cell)to ;
				  *to = (Cell)to ;
				}
			else *to = from ;
			return(size) ;

	case LIST :
              {	CPtr pfirstel ;

		if (*h > (current_findall->current_chunk + FINDALL_CHUNCK_SIZE - 3))
			{ if (! get_more_chunk()) return(-1) ;
			  *h = current_findall->top_of_chunk ;
			}
		*to = makelist(*h) ;
		to = (*h) ;
		(*h) += 2 ;
		pfirstel = clref_val(from) ;
		from = *pfirstel ; deref(from) ;
		s = findall_copy_template_to_chunk(from,to,h) ;
		if (s < 0) return(-1) ;
		size += s + 2 ;
		from = *(pfirstel+1) ; deref(from) ; to++ ;
		goto copy_again ;
             }

	case CS :
              {	CPtr pfirstel ;
		int ar ;

		pfirstel = (CPtr)cs_val(from) ;
		ar = get_arity((Psc)(*pfirstel)) ;
		if (*h > (current_findall->current_chunk + FINDALL_CHUNCK_SIZE - 1 - ar))
			{ if (! get_more_chunk()) return(-1) ;
			  *h = current_findall->top_of_chunk ;
			}


		*to = makecs((Cell)(*h)) ;
		to = *h ;
		*to = *pfirstel ; /* the functor */

		*h += ar + 1 ;
		size += ar + 1 ;
		while ( --ar )
                   { from = *(++pfirstel) ; deref(from) ; to++ ;
                     s = findall_copy_template_to_chunk(from,to,h) ;
		     if (s < 0) return(-1) ;
		     size += s ;
                   }
		from = *(++pfirstel) ; deref(from) ; to++ ;
		goto copy_again ;
             }
   }

return(-1) ; /* to keep compiler happy */

} /*findall_copy_template_to_chunk */

/*
$$findall_add/3
arg1 : any
arg2 : findall index - where to add it
arg3 : if not var, then return with fail immediately
at arg2, the term arg1 is added to the solution list
if findall_add/2 fails, the associated term remains unchanged
*/

int findall_add()
{
   Cell arg1, arg2, arg3 ;
   CPtr to, h ;
   int size ;

   arg3 = (Cell)Areg(3) ; deref(arg3) ;
   {	int t = cell_tag(arg3) ;
	if ((t != REF) && (t != REF1)) return(0) ;
   }

   arg1 = (Cell)Areg(1) ; deref(arg1) ;
   arg2 = (Cell)Areg(2) ; deref(arg2) ;

   current_findall = findall_solutions + int_val(arg2) ;
   if (current_findall->tail == 0)
		xsb_exit("internal error 1 in findall") ;

   to = current_findall->top_of_chunk ;
   if ((to+2) > (current_findall->current_chunk + FINDALL_CHUNCK_SIZE -1))
	{ if (! get_more_chunk()) return(0) ;
	  to = current_findall->top_of_chunk ;
	}


   h = to + 2 ;
   gl_bot = (CPtr)glstack.low ; gl_top = (CPtr)glstack.high ;

   if (init_findall_trail() &&
		(0 <= (size = findall_copy_template_to_chunk(arg1,to,&h))))
	{ findall_untrail() ;
	  current_findall->top_of_chunk = h ;
	  /* 2 because of ./2 of solution list */
	  current_findall->size += size + 2 ;
	  bld_free((to+1)) ;
	  *(CPtr)(*(current_findall->tail)) = makelist(to) ; /* link in new template now */
	  current_findall->tail = to+1 ; /* fill in new tail */
	  return(1) ;
	}

   findall_untrail() ;
   return(0) ;
} /* findall_add */

/*
$$findall_get_solutions/4
arg1 : out : solution list of findall
arg2 : out : tail of the list
arg3 : in : integer = findall index
arg4 : a variable which is now destructively set to 666
the list at arg3 is copied to the heap and then this copy is unified with arg1-arg2
*/

int findall_get_solutions()
{
   Cell arg1, arg2, arg3, arg4, from ;
   int cur_f ;
   findall_solution_list *p ;

   arg4 = (Cell)Areg(4) ; deref(arg4) ;
	{ int t = cell_tag(arg4) ;
	  if ((t == REF) || (t == REF1)) *(CPtr)arg4 = makeint(666) ;
	}

   arg3 = (Cell)Areg(3) ; deref(arg3) ; cur_f = int_val(arg3) ;

   p = findall_solutions + cur_f ;

   check_glstack_overflow(4, pcreg, p->size*sizeof(Cell)) ;

   arg1 = (Cell)Areg(1) ; deref(arg1) ; /* only after enough space is ensured */
   arg2 = (Cell)Areg(2) ; deref(arg2) ; /* only after enough space is ensured */

   gl_bot = (CPtr)glstack.low ; gl_top = (CPtr)glstack.high ;

   from = *(p->first_chunk+1) ; /* deref not necessary */
   findall_copy_to_heap(from,(CPtr)arg1,&hreg) ; /* this can't fail */
   *(CPtr)arg2 = *(p->tail) ; /* no checking, no trailing */
   findall_free(cur_f) ;
contcase:
   return(1) ;
} /* findall_get_solutions */

/* adapted from findall_copy_template_to_chunck */
/* returns the number of cells needed for the construction of term */
static long term_size(Cell term)
{
        static char message[60];
        long size = 0 ;
recur:
        switch(cell_tag(term))
        {       case FREE: case REF1:
                case INT: case STRING: case FLOAT:
                        return size ;
                case LIST:
                {       CPtr pfirstel ;

                        pfirstel = clref_val(term) ;
                        term = *pfirstel ; deref(term) ;
                        size += 2 + term_size(term) ;
                        term = *(pfirstel+1) ; deref(term) ;
                        goto recur;
                }
                case CS:
                {       int a ;
                        CPtr pfirstel ;

                        pfirstel = (CPtr)cs_val(term) ;
                        a = get_arity((Psc)(*pfirstel)) ;
                        size += a + 1 ;
                        while( --a )
                        {       term = *++pfirstel ; deref(term) ;
                                size += term_size( term ) ;
                        }
                        term = *++pfirstel ; deref(term) ;
                        goto recur;
                }
                default:
                        sprintf(message,
                        "Term type (tag = %ld) not handled by term_size.",
                                (Cell)cell_tag(term));
                        xsb_abort(message);
        }
        return 0 ;
}

/* copied from findall_copy_template_to_chunck */
/* recursively copies a term to a area of memory */
/* used by copy_term to build a variant in the heap */
static void do_copy_term(Cell from, CPtr to, CPtr *h)
{
copy_again : /* for tail recursion optimisation */

  switch ( cell_tag( from ) )
   {
        case INT :
        case FLOAT :
        case STRING :   *to = from ; return ;

        case REF :
        case REF1 :     if ((CPtr)from < hreg)
                                { findall_trail((CPtr)from) ;
                                  *(CPtr)from = (Cell)to ;
                                  *to = (Cell)to ;
                                }
                        else *to = from ;
                        return ;
        case LIST :
              { CPtr pfirstel ;

                *to = makelist(*h) ;
                to = (*h) ;
                (*h) += 2 ;
                pfirstel = clref_val(from) ;
                from = *pfirstel ; deref(from) ;
                do_copy_term(from,to,h) ;
                from = *(pfirstel+1) ; deref(from) ; to++ ;
                goto copy_again ;
             }
        case CS :
              { CPtr pfirstel ;
                int ar ;

                pfirstel = (CPtr)cs_val(from) ;
                ar = get_arity((Psc)(*pfirstel)) ;

                *to = makecs((Cell)(*h)) ;
                to = *h ;
                *to = *pfirstel ; /* the functor */

                *h += ar + 1 ;
                while ( --ar )
                   { from = *(++pfirstel) ; deref(from) ; to++ ;
                     do_copy_term(from,to,h) ;
                   }
                from = *(++pfirstel) ; deref(from) ; to++ ;
                goto copy_again ;
             }
   }
}

/* creates a new variant of a term in the heap
   arg1 - old term
   arg2 - new term ( must be given as an unbound variable, prolog
                     glue has to be provided for unification )
*/
int copy_term()
{
        long size ;
        Cell arg1, arg2, to ;
        CPtr hptr ;

        arg1 = (Cell)Areg(1) ; deref(arg1) ;

        if( isref(arg1) ) return 1;

        size = term_size(arg1) ;
        check_glstack_overflow( 2, pcreg, size*sizeof(Cell) ) ;

		/* again because stack might have been reallocated */
		arg1 = (Cell)Areg(1) ; deref(arg1) ;
        arg2 = (Cell)Areg(2) ; deref(arg2) ;

        if( !isref(arg2) ) xsb_abort( "incorrect use of copy_term0" ) ;
        hptr = hreg ;

        init_findall_trail() ;
        do_copy_term( arg1, &to, &hptr ) ;
        findall_untrail() ;

        hreg += size ;
        bind_copy((CPtr)arg2,to) ;

contcase:
        return 1 ;
}
