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


#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>
#include <stdlib.h>

#include "auxlry.h"
#include "cell_xsb.h"  	     /* cell_xsb.h must be included before deref.h */
#include "deref.h"
#include "register.h"
#include "memory_xsb.h"
#include "psc_xsb.h"
#include "error_xsb.h"
#include "heap_xsb.h"
#include "binding.h"
#include "subp.h"
#include "flags_xsb.h"
#include "loader_xsb.h"
#include "cinterf.h"
#include "findall.h"

findall_solution_list *findall_solutions = NULL;
findall_solution_list *current_findall;

#define MAX_FINDALLS  250
/* make MAX_FINDALLS larger if you want */

static int nextfree ; /* nextfree index in findall array */

CPtr gl_bot, gl_top ;

#include "ptoc_tag_xsb_i.h"


/* malloc a new chunck and link it in in the current findall */
int get_more_chunk()
{ CPtr newchunk ;

  if (!(newchunk = (CPtr)malloc(FINDALL_CHUNCK_SIZE * sizeof(Cell))))
    xsb_exit("get_more_chunk failed");

  *newchunk = 0 ;
  *(current_findall->current_chunk) = (Cell)newchunk ;
  current_findall->current_chunk = newchunk ;
  current_findall->top_of_chunk = newchunk + 1 ;

  return TRUE;

} /*get_more_chunk*/

/* $$findall_init/2

   to be called with 2 free variables
   the first variable is bound to an index in the findall table
   the second remains free - it will be set to 666 by findall_get_solutions
   without trailing, so that later add's will not occur
*/

int findall_init_c()
{
  CPtr w ;
  findall_solution_list *p ;
  int thisfree;
  
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
  thisfree = nextfree;
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
  return(thisfree) ;
} /* findall_init_c */

int findall_init()
{
  Cell arg1 ;
  int ichunk;
 
  arg1 = ptoc_tag(1); 
  ichunk = findall_init_c(); 
  *(CPtr)arg1 = makeint(ichunk) ;
  return TRUE;
} /* findall_init */

/* findall_free is called to desactive an entry in the solution_list
   at the end of findall_get_solutions, and from findall_clean
*/

void findall_free(int i)
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

/* findall_copy_to_heap does not need overflow checking - heap space is
   ensured; variables in the findall solution list can be altered without
   problem, because they are not needed afterwards anymore, so no trailing
*/

void findall_copy_to_heap(Cell from, CPtr to, CPtr *h)
{

copy_again : /* for tail recursion optimisation */

  switch ( cell_tag( from ) )
    {
    case XSB_INT :
    case XSB_FLOAT :
    case XSB_STRING :
      *to = from;
      return;
    
    case XSB_REF :
    case XSB_REF1 :
      XSB_Deref(from);
      if (! isref(from)) goto copy_again; /* it could be a XSB_LIST */
      if (on_glstack((CPtr)(from)))
	*to = from;
      else
	{
	  *(CPtr)from = (Cell)to;
	  *to = (Cell)to;
	}
      return;

  case XSB_LIST :
    {
      CPtr pfirstel;
      Cell q ;

      /* first test whether from - which is an L - is actually the left over
	 of a previously copied first list element
      */
      pfirstel = clref_val(from) ;
      if (on_glstack(pfirstel))
	{
	  /* pick up the old value and copy it */
	  *to = *pfirstel;
	  return;
	}

      q = *pfirstel;
      if (is_list(q))
	{
	  CPtr p;
	  
	  p = clref_val(q);
	  if (on_glstack(p))  /* meaning it is a shared list */
	    {
	      *to = q;
	      return;
	    }
	}



      /* this list cell has not been copied before */
      /* now be careful: if the first element of the list to be copied
           is an undef (a ref to an undef is not special !)
           we have to copy this undef now, before we do the general
           thing for lists
      */

      {
	Cell tr1;

	tr1 = *to = makelist(*h) ;
	to = (*h) ;
	(*h) += 2 ;
	if (q == (Cell)pfirstel) /* it is an UNDEF - special care needed */
	  {
	    /* it is an undef in the part we are copying from */
	    *to = (Cell)to ;
	    *pfirstel = makelist((CPtr)to);
	  }
	else
	  {
	    *pfirstel = makelist((CPtr)to);
	    findall_copy_to_heap(q,to,h);
	  }

	from = *(pfirstel+1) ; to++ ;
	goto copy_again ;
      }
    }

    case XSB_STRUCT :
      {
	CPtr pfirstel;
	Cell newpsc;
	int ar;
    
	pfirstel = (CPtr)cs_val(from);
	if ( cell_tag((*pfirstel)) == XSB_STRUCT )
	  {
	    /* this struct was copied before - it must be shared */
            *to = *pfirstel;
            return;
	  }

	/* first time we visit this struct */
      
	ar = get_arity((Psc)(*pfirstel)) ;
	
	newpsc = *to = makecs((Cell)(*h)) ;
	to = *h ;
	*to = *pfirstel ; /* the functor */
	*pfirstel = newpsc; /* no need for trailing */
	
	*h += ar + 1 ;
	while ( --ar )
	  {
	    from = *(++pfirstel) ; to++ ;
	    findall_copy_to_heap(from,to,h) ;
	  }
	from = *(++pfirstel) ; to++ ;
	goto copy_again ;
      }
  
  case XSB_ATTV: {
    CPtr var;
    
    /*
     * The following XSB_Deref() is necessary, because, in copying
     * f(X,X), after the first occurrence of X is copied, the VAR
     * part of X has been pointed to the new copy on the heap.  When
     * we see this X again, we should dereference it to find that X
     * is already copied, but this deref is not done (see the code
     * in `case XSB_STRUCT:' -- deref's are gone).
     */
    XSB_Deref(from);
    var = clref_val(from);  /* the VAR part of the attv  */
    if (on_glstack(var))    /* is a new attv in the `to area' */
      bld_attv(to, var);
    else {		  /* has not been copied before */
      from = cell(var + 1); /* from -> the ATTR part of the attv */
      XSB_Deref(from);
      *to = makeattv(*h);
      to = (*h);
      (*h) += 2;		  /* skip two cells */
      /*
       * Trail and bind the VAR part of the attv to the new attv
       * just created in the `to area', so that attributed variables
       * are shared in the `to area'.
       */
      bld_attv(var, to);
      cell(to) = (Cell) to;
      to++;
      goto copy_again;
    }
  } /* case XSB_ATTV */
  }
 
} /*findall_copy_to_heap*/


/* trailing variables during copying a template: a linked list of arrays is used */

#define F_TR_NUM 250 /* the number of trail entries in a chunck of the trail
			it must be a multiple of 2
		     */

typedef struct f_tr_chunk {
  struct f_tr_chunk *previous ;
  CPtr tr[F_TR_NUM] ;
} f_tr_chunk ;

static f_tr_chunk *cur_tr_chunk ;
static CPtr *cur_tr_top ;
static CPtr *cur_tr_limit ;

static void findall_untrail()
{
  CPtr *p, *begin_trail ;
  f_tr_chunk *tr_chunk, *tmp ;
  
  if( !(tr_chunk = cur_tr_chunk) ) return ; /* protection */
  begin_trail = tr_chunk->tr ;
  
  for (p = cur_tr_top ; p-- > begin_trail ; )
    {
      *((CPtr)(*p)) = (Cell)(*(p-1));
      p--;
    }
  
  tmp = tr_chunk ; tr_chunk = tr_chunk->previous ; free(tmp) ;
  while (tr_chunk != 0)
    {
      begin_trail = tr_chunk->tr ;
      for (p = tr_chunk->tr + F_TR_NUM; p-- > begin_trail ; )
	{
	  *((CPtr)(*p)) = (Cell)(*(p-1));
	  p--;
	}
      tmp = tr_chunk ; tr_chunk = tr_chunk->previous ; free(tmp) ;
    }
} /* findall_untrail */

/* if tr2 == 0, then we need to trail only the first two */

static int findall_trail(CPtr p, Cell val)
{ 
  f_tr_chunk *new_tr_chunk ;
  int trail_left = cur_tr_limit - cur_tr_top;
  
  if (trail_left == 0)
    {
      if (!(new_tr_chunk = (f_tr_chunk *)malloc(sizeof(f_tr_chunk))))
	xsb_exit("findall_trail failed");
      cur_tr_top = new_tr_chunk->tr ;
      cur_tr_limit = new_tr_chunk->tr+F_TR_NUM ;
      new_tr_chunk->previous = cur_tr_chunk ;
      cur_tr_chunk = new_tr_chunk ;
    }

  *(cur_tr_top++) = (CPtr)val;
  *(cur_tr_top++) = (CPtr)p;
  return TRUE;
} /* findall_trail */

static int init_findall_trail()
{
  if (!(cur_tr_chunk = (f_tr_chunk *)malloc(sizeof(f_tr_chunk))))
    xsb_exit("init_findall_trail failed");
  cur_tr_top = cur_tr_chunk->tr ;
  cur_tr_limit = cur_tr_chunk->tr+F_TR_NUM ;
  cur_tr_chunk->previous = 0 ;
  return TRUE;
} /* init_findall_trail */

/* findall_copy_template_to_chunk
   must do: overflow checking
   trailing of variables
   returns size of copied term
   if it "fails", returns a negative number
*/

static int findall_copy_template_to_chunk(Cell from, CPtr to, CPtr *h)
{
  int size = 0 ;
  int s ;

  copy_again : /* for tail recursion optimisation */

    switch ( cell_tag( from ) )
      {
      case XSB_INT :
      case XSB_FLOAT :
      case XSB_STRING :
	*to = from ;
	return(size) ;
    
      case XSB_REF :
      case XSB_REF1 :
	if (on_glstack((CPtr)(from)))
	  {
	    findall_trail((CPtr)from,from) ;
	    *(CPtr)from = (Cell)to ;
	    *to = (Cell)to ;
	  } else *to = from ;
	return(size) ;

      case XSB_LIST :
	{
	  CPtr pfirstel ;
	  Cell q ;

	  /* first test whether from - which is an L - is actually the left over
	     of a previously copied first list element
	  */
	  pfirstel = clref_val(from) ;
	  if (! on_glstack(pfirstel))
	    {
	      /* pick up the old value and copy it */
	      *to = *pfirstel;
	      return(size);
	    }
	  
	  q = *pfirstel;
	  if (is_list(q))
	    {
	      CPtr p;
	      
	      p = clref_val(q);
	      if (! on_glstack(p))
		{
		  *to = q;
		  return(size);
		}
	    }

	  if (*h > (current_findall->current_chunk + FINDALL_CHUNCK_SIZE - 3))
	    {
	      if (! get_more_chunk()) return(-1) ;
	      *h = current_findall->top_of_chunk ;
	    }

	  {
	    Cell tr1;

	    tr1 = *to = makelist(*h) ;
	    to = (*h) ;
	    (*h) += 2 ;
	    if (q == (Cell)pfirstel) /* it is an UNDEF - special care needed */
	      {
		/* it is an undef in the part we are copying from */
		findall_trail(pfirstel,(Cell)pfirstel);
		*to = (Cell)to ;
		*pfirstel = makelist((CPtr)to);
	      }
	    else
	      {
		findall_trail(pfirstel,q);
		*pfirstel = makelist((CPtr)to);
		XSB_Deref(q);
		s = findall_copy_template_to_chunk(q,to,h);
		if (s < 0) return(-1) ;
		size += s + 2 ;
	      }

	    from = *(pfirstel+1) ; XSB_Deref(from) ; to++ ;
	    goto copy_again ;
	  }
	}
	
      case XSB_STRUCT :
	{
	  CPtr pfirstel ;
	  Cell newpsc;
	  int ar ;
    
	  pfirstel = (CPtr)cs_val(from) ;
	  if ( cell_tag((*pfirstel)) == XSB_STRUCT )
	    {
	      /* this struct was copied before - it must be shared */
	      *to = *pfirstel;
	      return(size);
	    }

	  /* first time we visit this struct */

	  findall_trail(pfirstel,*pfirstel);

	  ar = get_arity((Psc)(*pfirstel)) ;
	  /* make sure there is enough space in the chunks */
	  if (*h > (current_findall->current_chunk + FINDALL_CHUNCK_SIZE - 1 - ar))
	    {
	      if (! get_more_chunk()) return(-1) ;
	      *h = current_findall->top_of_chunk ;
	    }

	  newpsc = *to = makecs((Cell)(*h)) ;
	  to = *h ;
	  *to = *pfirstel ; /* the functor */
	  *pfirstel = newpsc; /* was trailed already */

	  *h += ar + 1 ;
	  size += ar + 1 ;
	  while ( --ar )
	    {
	      from = *(++pfirstel) ; XSB_Deref(from) ; to++ ;
	      s = findall_copy_template_to_chunk(from,to,h) ;
	      if (s < 0) return(-1) ;
	      size += s ;
	    }
	  from = *(++pfirstel) ; XSB_Deref(from) ; to++ ;
	  goto copy_again ;
	}
  
  case XSB_ATTV: {
    CPtr var;
    
    var = clref_val(from);  /* the VAR part of the attv  */
    if (on_glstack(var)) {  /* has not been copied before */
      from = cell(var + 1); /* from -> the ATTR part of the attv */
      XSB_Deref(from);
      if (*h > (current_findall->current_chunk + FINDALL_CHUNCK_SIZE - 3)) {
	if (! get_more_chunk()) return(-1) ;
	*h = current_findall->top_of_chunk ;
      }
      *to = makeattv(*h);
      to = (*h);
      (*h) += 2;		  /* skip two cells */
      size += 2;
      /*
       * Trail and bind the VAR part of the attv to the new attv
       * just created in the `to area', so that attributed variables
       * are shared in the `to area'.
       */
      findall_trail(var,(Cell)var);
      bld_attv(var, to);
      cell(to) = (Cell) to;
      to++;
      goto copy_again;
    }
    else {		  /* is a new attv in the `to area' */
      bld_attv(to, var);
      return(size);
    }
  } /* case XSB_ATTV */
  } /* switch */
 
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
  
  arg3 = ptoc_tag(3);
  {
    int t = cell_tag(arg3) ;
    if ((t != XSB_REF) && (t != XSB_REF1)) return(0) ;
  }
  
  arg1 = ptoc_tag(1);
  arg2 = ptoc_tag(2);
  
  current_findall = findall_solutions + int_val(arg2) ;
  if (current_findall->tail == 0)
    xsb_exit("internal error 1 in findall") ;
  
  to = current_findall->top_of_chunk ;
  if ((to+2) > (current_findall->current_chunk + FINDALL_CHUNCK_SIZE -1)) {
    if (! get_more_chunk()) return(0) ;
    to = current_findall->top_of_chunk ;
  }
  
  h = to + 2 ;
  gl_bot = (CPtr)glstack.low ; gl_top = (CPtr)glstack.high ;
  
  if (init_findall_trail() &&
      (0 <= (size = findall_copy_template_to_chunk(arg1,to,&h)))) {
    findall_untrail() ;
    current_findall->top_of_chunk = h ;
    /* 2 because of ./2 of solution list */
    current_findall->size += size + 2 ;
    bld_free((to+1)) ;
    /* link in new template now */
    *(CPtr)(*(current_findall->tail)) = makelist(to);
    current_findall->tail = to+1 ; /* fill in new tail */
    return TRUE;
  }
  
  findall_untrail() ;
  return FALSE;
} /* findall_add */

/* $$findall_get_solutions/4
   arg1 : out : solution list of findall
   arg2 : out : tail of the list
   arg3 : in : integer = findall index
   arg4 : a variable which is now destructively set to 666
   
   the list at arg3 is copied to the heap and then this copy is unified with
   arg1-arg2 
*/

int findall_get_solutions()
{
  Cell arg1, arg2, arg3, arg4, from ;
  int cur_f ;
  findall_solution_list *p ;
  
  arg4 = ptoc_tag(4);
  {
    int t = cell_tag(arg4) ;
    if ((t == XSB_REF) || (t == XSB_REF1)) *(CPtr)arg4 = makeint(666) ;
  }
  
  arg3 = ptoc_tag(3); 
  cur_f = int_val(arg3) ;
  
  p = findall_solutions + cur_f ;
  
  check_glstack_overflow(4, pcreg, p->size*sizeof(Cell), return TRUE) ;
  
  arg1 = ptoc_tag(1);  /* only after enough space is ensured */
  arg2 = ptoc_tag(2);  /* only after enough space is ensured */
  
  gl_bot = (CPtr)glstack.low ; gl_top = (CPtr)glstack.high ;
  
  from = *(p->first_chunk+1) ; /* XSB_Deref not necessary */
  findall_copy_to_heap(from,(CPtr)arg1,&hreg) ; /* this can't fail */
  *(CPtr)arg2 = *(p->tail) ; /* no checking, no trailing */
  findall_free(cur_f) ;
  return TRUE;
} /* findall_get_solutions */

/* adapted from findall_copy_template_to_chunck */
/* returns the number of cells needed for the construction of term */
static long term_size(Cell term)
{
  long size = 0 ;
 recur:
  switch(cell_tag(term)) {
  case XSB_FREE:
  case XSB_REF1:
  case XSB_INT:
  case XSB_STRING:
  case XSB_FLOAT:
    return size ;
  case XSB_LIST: {
    CPtr pfirstel ;
    
    pfirstel = clref_val(term) ;
    term = *pfirstel ; XSB_Deref(term) ;
    size += 2 + term_size(term) ;
    term = *(pfirstel+1) ; XSB_Deref(term) ;
    goto recur;
  }
  case XSB_STRUCT: {
    int a ;
    CPtr pfirstel ;
    
    pfirstel = (CPtr)cs_val(term) ;
    a = get_arity((Psc)(*pfirstel)) ;
    size += a + 1 ;
    while( --a ) {
      term = *++pfirstel ; XSB_Deref(term) ;
      size += term_size( term ) ;
    }
    term = *++pfirstel ; XSB_Deref(term) ;
    goto recur;
  }
  case XSB_ATTV: {
    CPtr pfirstel;

    pfirstel = clref_val(term);
    if (pfirstel < hreg) {
      /*
       * This is the first occurrence of an attributed variable.  Its
       * first cell (the VAR part) will be changed to an XSB_ATTV cell which
       * points to hreg, and the cell of hreg will be set to a free
       * variable.  So the later occurrence of this attributed variable is
       * dereferenced and seen as an XSB_ATTV pointing to hreg, and we can
       * tell it has been counted before.
       */
      size += 2;
      findall_trail(pfirstel,(Cell)pfirstel);
      bld_attv(pfirstel, hreg); /* bind VAR part to a cell out of hreg */
      bld_free(hreg);
      term = cell(clref_val(term) + 1);
      goto recur;
    }
    else /* this XSB_ATTV has been counted before */
      return size;
  }
  }
  return FALSE;
}

/* rewritten */
/* recursively copies a term to a area of memory */
/* used by copy_term to build a variant in the heap */

static void do_copy_term(Cell from, CPtr to, CPtr *h)
{
copy_again : /* for tail recursion optimisation */

  switch ( cell_tag( from ) )
    {
    case XSB_INT :
    case XSB_FLOAT :
    case XSB_STRING :
      *to = from ;
      return ;
    
    case XSB_REF :
    case XSB_REF1 :
      if ((CPtr)from < hreg)  /* meaning: a not yet copied undef */
	{
	  findall_trail((CPtr)from,from) ;
	  *(CPtr)from = (Cell)to ;
	  *to = (Cell)to ;
	}
      else *to = from ;
      return ;

    case XSB_LIST :
      {
	/*
	 *  before copying:
	 *  
	 *  +----+        +----+----+
	 *  | x L|    (x) | a  | b  |    empty trail
	 *  +----+        +----+----+
	 *  
	 *  
	 *  after copying:
	 *  
	 *  
	 *  +----+        +----+----+
	 *  | x L|    (x) | x'L| b  |
	 *  +----+        +----+----+
	 *  
	 *  
	 *  trail:
	 *  
	 *  +----+----+
	 *  | a  | x  |
	 *  +----+----+
	 *  
	 *  
	 *  the copy is:
	 *  
	 *  +----+         +----+----+
	 *  | x'L|    (x') | a' | b' |
	 *  +----+         +----+----+
	 *  
	 *  careful if a is undef !
	 */

	CPtr pfirstel;
	Cell q ;

	/* first test whether from - which is an L - is actually the left over
	   of a previously copied first list element
	*/
	pfirstel = clref_val(from) ;
	if (pfirstel >= hreg)
	  {
	    /* pick up the old value and copy it */
	    *to = *pfirstel;
	    return;
	  }

	q = *pfirstel;
	if (is_list(q))
	  {
	    CPtr p;

	    p = clref_val(q);
	    if (p >= hreg)  /* meaning it is a shared list */
	      {
		*to = q;
		return;
	      }
	  }

	/* this list cell has not been copied before */
	/* now be careful: if the first element of the list to be copied
	   is an undef (a ref to an undef is not special !)
	   we have to copy this undef now, before we do the general
	   thing for lists
	*/

	{
	  Cell tr1;

	  tr1 = *to = makelist(*h) ;
	  to = (*h) ;
	  (*h) += 2 ;
	  if (q == (Cell)pfirstel) /* it is an UNDEF - special care needed */
	    {
	      /* it is an undef in the part we are copying from */
	      findall_trail(pfirstel,(Cell)pfirstel);
	      *to = (Cell)to ;
	      *pfirstel = makelist((CPtr)to);
	    }
	  else
	    {
	      findall_trail(pfirstel,q);
	      *pfirstel = makelist((CPtr)to);
	      XSB_Deref(q);
	      do_copy_term(q,to,h);
	    }

	  from = *(pfirstel+1) ; XSB_Deref(from) ; to++ ;
	  goto copy_again ;
	}
      }

    case XSB_STRUCT : {
      /*
       before copying:
       
           +--------+     +-----------------------------------+      +--------+
       (b) |a STRUCT| (a) | Functor | arg1 | arg2 | ... | argn|  (f) |a STRUCT|
           +--------+     +-----------------------------------+      +--------+
       
       trail stack empty
       
       after copying the first (at b)
       
       
           +--------+     +------------------------------------+     +--------+
       (b) |a STRUCT| (a) | d STRUCT | arg1 | arg2 | ... | argn| (f) |a STRUCT|
           +--------+     +------------------------------------+     +--------+
        	       
           +--------+      +-----------------------------------+ 
       (c) |d STRUCT|  (d) | Functor | arg1 | arg2 | ... | argn| 
           +--------+      +-----------------------------------+ 
       
              +-------------+
       trail: | Functor | a |
              +-------------+
        
       c and d are addresses of the copied things
       
       so when we come at the STRUCT pointer at f, we hit the |d STRUCT| cell
       at a, which means that it was copied before
        
       this relies on a Functor cell not having a STRUCT tag
        
       the situation for lists is more complicated
      */
      
	CPtr pfirstel ;
	Cell newpsc;
	int ar ;

	pfirstel = (CPtr)cs_val(from) ;
	if ( cell_tag((*pfirstel)) == XSB_STRUCT )
	  {
	    /* this struct was copied before - it must be shared */
	    *to = *pfirstel;
	    return;
	  }

	/* first time we visit this struct */

	findall_trail(pfirstel,*pfirstel);

	ar = get_arity((Psc)(*pfirstel)) ;
	
	newpsc = *to = makecs((Cell)(*h)) ;
	to = *h ;
	*to = *pfirstel ; /* the functor */
	*pfirstel = newpsc; /* was trailed already */
	
	*h += ar + 1 ;
	while ( --ar )
	  {
	    from = *(++pfirstel) ; XSB_Deref(from) ; to++ ;
	    do_copy_term(from,to,h) ;
	  }
	from = *(++pfirstel) ; XSB_Deref(from) ; to++ ;
	goto copy_again ;
      }

    case XSB_ATTV:
      {
	/*
	 *  before copying: (A means XSB_ATTV tag)
	 *  
	 *  +----+        +----+----+
	 *  | x A|    (x) | a  | b  |    empty trail
	 *  +----+        +----+----+
	 *  
	 *  because of deref, a is always an undef, meaning that actually
	 *  a == x
	 *  
	 *  
	 *  after copying:
	 *  
	 *  
	 *  +----+        +----+----+
	 *  | x A|    (x) | x'A| b  |
	 *  +----+        +----+----+
	 *  
	 *  
	 *  trail:
	 *  
	 *  +----+----+
	 *  | x  | x  |
	 *  +----+----+
	 *  
	 *  the copy is:
	 *  
	 *  +----+         +----+----+
	 *  | x'A|    (x') | a' | b' |
	 *  +----+         +----+----+
	 */

	CPtr var;
    
	var = clref_val(from);	/* the VAR part of the attv  */
	if (var < hreg) {	/* has not been copied before */
	  from = cell(var + 1);	/* from -> the ATTR part of the attv */
	  XSB_Deref(from);
	  *to = makeattv(*h);
	  to = (*h);
	  (*h) += 2;		/* skip two cells */
	  /*
	   * Trail and bind the VAR part of the attv to the new attv just
	   * created in the `to area', so that attributed variables are
	   * shared in the `to area'.
	   */
	  findall_trail(var,(Cell)var);
	  bld_attv(var, to);
	  cell(to) = (Cell) to;
	  to++;
	  goto copy_again;
	} else			/* is a new attv in the `to area' */
	  bld_attv(to, var);
      } /* case XSB_ATTV */
    } /* switch */
} /* do_copy_term */



/* creates a new variant of a term in the heap
   arg1 - old term
   arg2 - new term; copy of old term unifies with new term
*/

int copy_term()
{
  long size ;
  Cell arg1, arg2, to ;
  CPtr hptr ;

  arg1 = ptoc_tag(1);
  
  if( isref(arg1) ) return 1;

  init_findall_trail() ;
  size = term_size(arg1) ;
  findall_untrail() ;

  check_glstack_overflow( 2, pcreg, size*sizeof(Cell), return FALSE ) ;
  
  /* again because stack might have been reallocated */
  arg1 = ptoc_tag(1);
  arg2 = ptoc_tag(2);
  
  hptr = hreg ;
  
  gl_bot = (CPtr)glstack.low ; gl_top = (CPtr)glstack.high ;
  init_findall_trail() ;
  do_copy_term( arg1, &to, &hptr ) ;
  findall_untrail() ;
  
  {
    int size2 = hptr - hreg;
    /* fprintf(stderr,"copied size = %d\n",size2); */
    if (size2 > size)
      fprintf(stderr,"panic after copy_term\n");
  }

  hreg = hptr;

  return(unify(arg2, to));
} /* copy_term */
