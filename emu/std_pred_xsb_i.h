/* File:      std_pred_xsb_i.h
** Author(s): Kostis F. Sagonas
** Modified by Swift 
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


/*----------------------------------------*/
#include "xsb_config.h"
#include "builtin.h"
#include "sp_unify_xsb_i.h"
/*----------------------------------------*/

static xsbBool atom_to_list(CTXTdeclc int call_type);
static xsbBool number_to_list(CTXTdeclc int call_type);

/* TLS 10/01 changed functor so that it did not core dump on 
   functor(X,1,2) */
inline static xsbBool functor_builtin(CTXTdecl)
{
  /* r1: ?term; r2: ?functor; r3: ?arity (int)	*/
  int  new_indicator, arity, value, disp;
  Psc psc;
  char *name;
  Cell functor, term;
  Pair sym;

  term = ptoc_tag(CTXTc 1);
  if (isnonvar(term)) {
    if (isconstr(term) && !isboxedfloat(term) && !isboxedinteger(term)) {
      psc = get_str_psc(term);
      name = get_name(psc);
      arity = get_arity(psc);
      return (atom_unify(CTXTc makestring(name), ptoc_tag(CTXTc 2)) &&
	      int_unify(CTXTc makeint(arity), ptoc_tag(CTXTc 3)));
    } else if (islist(term))
      return (atom_unify(CTXTc makestring(list_dot), ptoc_tag(CTXTc 2)) &&
	      int_unify(CTXTc makeint(2), ptoc_tag(CTXTc 3)));
    else return (unify(CTXTc term, ptoc_tag(CTXTc 2)) &&
		 int_unify(CTXTc makeint(0), ptoc_tag(CTXTc 3)));
  } else {	/* term is a variable */
    functor = ptoc_tag(CTXTc 2);
    if (isstring(functor) || isinteger(functor) || isofloat(functor) ||
	isboxedinteger(functor) ||
	(isconstr(term) && get_arity(get_str_psc(term)) == 0)) {
      arity = ptoc_tag(CTXTc 3);
      /* tls: added !isnumber conjunct */
      if (arity_integer(arity) && !isnumber(functor)) {
	value = int_val(arity);
	if (value == 0) return unify(CTXTc functor, term);
	else {
	  if (value == 2 && isstring(functor) 
	      && string_val(functor) == list_dot) {
	    /* term is a variable and I bind it to a list. */
	    bind_list((CPtr)term, hreg);
	    new_heap_free(hreg);
	    new_heap_free(hreg);
	  } else { 
	    /* functor always creates a psc in the current module */
	    sym = (Pair)insert(string_val(functor), (char)value, 
			       (Psc)flags[CURRENT_MODULE],
			       &new_indicator);
	    sreg = hreg;
	    hreg += value+1;	/* need (arity+1) new cells */
	    bind_cs((CPtr)term, sreg);
	    new_heap_functor(sreg, sym->psc_ptr);
	    for (disp=0; disp<value; disp++) {
	      new_heap_free(sreg);
	    }
	  }
	  return TRUE;	/* always succeed! */
	}
	/* TLS rearranged order of the two elses below */
      } else {
	  if (isnumber(functor))
	    return (unify(CTXTc term, functor) && 
		    int_unify(CTXTc makeint(0), arity));
	  else {
	    if (isnonvar(arity)) {
	      if (isinteger(arity))
		err_handle(CTXTc RANGE, 3, "functor", 3,
		       "integer in the range 0..255", arity);
	      else 
		xsb_type_error(CTXTc "integer",arity,"functor",3,3); 

	    }
	  else err(INSTANTIATION, 3, "functor", 3);
	  }
      }
    }
      else {
      if (isnonvar(functor))
	xsb_type_error(CTXTc "atom",functor,"functor",3,2); 
      else err(INSTANTIATION, 2, "functor", 3);
      }
  }
  return TRUE;
}


inline static xsbBool arg_builtin(CTXTdecl)
{
  /* r1: +index (int); r2: +term; r3: ?arg (term) */
  Cell index;
  Cell term;
  int disp;

  index = ptoc_tag(CTXTc 1);
  if (isinteger(index)) {
    if ((disp = int_val(index)) > 0) {
      term = ptoc_tag(CTXTc 2);
      if (isnonvar(term)) {
	if (isconstr(term)) {
	  if (disp <= (int)get_arity(get_str_psc(term))) {
	    return unify(CTXTc (Cell)(clref_val(term)+disp),
			 ptoc_tag(CTXTc 3));
	  } 
	  else return FALSE;	/* fail */
	} else if (islist(term) && (disp==1 || disp==2)) {
	  return unify(CTXTc (Cell)(clref_val(term)+disp-1),
		       ptoc_tag(CTXTc 3));
	} else return FALSE;	/* fail */
      } else err(INSTANTIATION, 2, "arg", 3);
    } else return FALSE;	/* fail */
  } else {
    if (isnonvar(index)) xsb_type_error(CTXTc "integer",index,"arg",3,1); 
    else err(INSTANTIATION, 1, "arg", 3);
  }
  return TRUE;
}


inline static xsbBool univ_builtin(CTXTdecl)
{
  /* r1: ?term; r2: ?list	*/
  int i, arity;
  int  new_indicator;
  char *name;
  Cell list, new_list, term;
  CPtr head, top = 0;
  Pair sym;

  term = ptoc_tag(CTXTc 1);
  list = ptoc_tag(CTXTc 2);
  if (isnonvar(term)) {	/* Usage is deconstruction of terms */
    new_list = makelist(hreg);
    if (isatomic(term) || isboxedinteger(term)) { follow(hreg++) = term; top = hreg++; }
    else if (isconstr(term) && (arity = (get_arity(get_str_psc(term))))) {
      follow(hreg++) = makestring(get_name(get_str_psc(term)));
      top = hreg++;
      for (i = 1 ; i <= arity ; i++) {
	follow(top) = makelist(hreg); top = hreg++;
	follow(top) = (Cell)(clref_val(term)+i); top = hreg++;
      }
    }
    else { /* term is list */
      follow(hreg++) = makestring(list_dot);
      top = hreg++;
      follow(top) = makelist(hreg); top = hreg++;
      follow(top) = (Cell)(clref_val(term)); top = hreg++;
      follow(top) = makelist(hreg); top = hreg++;
      follow(top) = (Cell)(clref_val(term)+1); top = hreg++;
    }
    follow(top) = makenil;
    return unify(CTXTc list, new_list);
  } else { /* usage is construction; term is known to be a variable */
    if (islist(list)) {
      head = clref_val(list);
      XSB_Deref(cell(head));
      if (isatom(cell(head))) {
	if (isnil(cell(head+1))) {	/* atom construction */
	  bind_copy((CPtr)term, cell(head));
	  return TRUE;	/* succeed */
	} else {
	  xsbBool list_construction = FALSE;
	  name = string_val(cell(head));
	  if (!strcmp(name, ".")) { /* check for list construction */
	    list = cell(head+1); XSB_Deref(list);
	    if (islist(list)) {
	      list = cell(clref_val(list)+1); XSB_Deref(list);
	      if (islist(list)) {
		list = cell(clref_val(list)+1); XSB_Deref(list);
		if (isnil(list)) list_construction = TRUE;
	      }
	    }
	  }
	  if (list_construction) { /* no errors can occur */
	    bind_list((CPtr)term, hreg);
	    list = cell(head+1);
	    XSB_Deref(list);
	    bld_copy(hreg, cell(clref_val(list))); hreg++;
	    list = cell(clref_val(list)+1);
	    XSB_Deref(list);
	    bld_copy(hreg, cell(clref_val(list))); hreg++;
	  } else { /* compound term construction */
	    sreg = hreg;
	    bind_cs((CPtr)term, sreg); hreg = sreg; sreg++;
	    for (arity = 0, list = cell(head+1); ;
		 arity++, list = cell(clref_val(list)+1)) {
	      XSB_Deref(list); /* necessary */
	      if (!islist(list)) break; /* really ugly */
	      bld_copy(sreg, cell(clref_val(list))); sreg++;
	    }
	    if (isnil(list) && arity <= MAX_ARITY) {
	      /* '=..'/2 always creates a psc in the current * module */
	      sym = (Pair)insert(name, (char)arity,
				 (Psc)flags[CURRENT_MODULE],
				 &new_indicator);
	      new_heap_functor(hreg, sym->psc_ptr);
	      hreg = sreg+1;
	    } else {
	      hreg = hreg-1;	/* restore hreg */
	      if (arity > MAX_ARITY)
		xsb_abort("[In =..] Attempt to construct a functor with arity %d > %d",
			  arity, MAX_ARITY);
	      else xsb_type_error(CTXTc "list",list,"=..",2,2);  /* X =.. [foo|Y]. */
	      return FALSE;
	    }
	  }
	} return TRUE;
      }
      if ((isnumber(cell(head)) || isboxedinteger(cell(head))) && isnil(cell(head+1))) { /* list=[num] */
	bind_copy((CPtr)term, cell(head));	 /* term<-num  */
	return TRUE;	/* succeed */
      }
      else
	{
	  xsb_type_error(CTXTc "list",list,"=..",2,2);  /* X =.. X =.. [2,a,b]. */
	  return(FALSE);
	}
    }
    if (isnonvar(list))
	  xsb_type_error(CTXTc "list",list,"=..",2,2);  /* X =.. a */
    else err(INSTANTIATION, 2, "=..", 2);
  }
  return TRUE;
}


inline static xsbBool hilog_arg(CTXTdecl)
{
  /* r1: +index (int); r2: +term; r3: ?arg (term) */
  Cell index, term;
  int disp;

  index = ptoc_tag(CTXTc 1);
  if (isinteger(index)) {
    if ((disp = int_val(index)) > 0) {
      term = ptoc_tag(CTXTc 2);
      if (isnonvar(term)) {
	if (isconstr(term)) {
	  if (hilog_cs(term)) disp++;
	  if (disp <= (int)get_arity(get_str_psc(term))) {
	    return unify(CTXTc (Cell)(clref_val(term)+disp),
			 ptoc_tag(CTXTc 3));
	  } return FALSE;		/* fail */
	} else if (islist(term) && (disp==1 || disp==2)) {
	  return unify(CTXTc (Cell)(clref_val(term)+disp-1),
		       ptoc_tag(CTXTc 3));
	} else return FALSE;	/* fail */
      } else err(INSTANTIATION, 2, "hilog_arg", 3);
    } else return FALSE;	/* fail */
  } else {
    if (isnonvar(index))
      xsb_type_error(CTXTc "integer",index,"hilog_arg",3,1);
    else err(INSTANTIATION, 1, "hilog_arg", 3);
  }
  return TRUE;
}

#define INITIAL_NAMELEN 256

inline static xsbBool atom_to_list(CTXTdeclc int call_type)
{
  /* r1: ?term; r2: ?character list	*/
  int i, len;
  long c;
  char *atomname, *atomnamelast;
  static char *atomnameaddr = NULL;
  static int atomnamelen;
  char tmpstr[2], *tmpstr_interned;
  Cell heap_addr, term, term2;
  Cell list, new_list;
  CPtr top = 0;
  char *call_name = (call_type == ATOM_CODES ? "atom_codes" : "atom_chars");
  char *elt_type = (call_type == ATOM_CODES ? "ASCII code" : "character atom");

  SYS_MUTEX_LOCK(MUTEX_ATOM_BUF);

  term = ptoc_tag(CTXTc 1);
  list = ptoc_tag(CTXTc 2);
  if (!isnonvar(term)) {	/* use is: CODES/CHARS --> ATOM */
    if (atomnameaddr == NULL) {
      atomnameaddr = (char *)malloc(INITIAL_NAMELEN);
      atomnamelen = INITIAL_NAMELEN;
      /* printf("Allocated namebuf: %p, %d\n",atomnameaddr,atomnamelen);*/
    }
    atomname = atomnameaddr;
    atomnamelast = atomnameaddr + (atomnamelen - 1);
    term2 = list;	/* DON'T use heap for temp storage */
    do {
      XSB_Deref(term2);
      if (isnil(term2)) {
	*atomname++ = '\0';
	break;
      }
      if (islist(term2)) {
	heap_addr = cell(clref_val(term2)); XSB_Deref(heap_addr);
	if (((call_type==ATOM_CODES) && !isinteger(heap_addr))
	    || ((call_type==ATOM_CHARS) && !isstring(heap_addr))) {
	  if (isnonvar(heap_addr)) {
	    xsb_type_error(CTXTc elt_type,list,call_name,2,2); 
	  }
	  else err(INSTANTIATION, 2, call_name, 2);
  	  SYS_MUTEX_UNLOCK(MUTEX_ATOM_BUF);
	  return FALSE;	/* fail */
	}
	if (isinteger(heap_addr))
	  c = int_val(heap_addr);
	else /* ATOM CHARS */
	  c = *string_val(heap_addr);

	if (c < 0 || c > 255) {
	  err_handle(CTXTc RANGE, 2, call_name, 2, "ASCII code", heap_addr);
  	  SYS_MUTEX_UNLOCK(MUTEX_ATOM_BUF);
	  return FALSE;	/* fail */
	}
	if (atomname >= atomnamelast) {
	  atomnameaddr = (char *)realloc(atomnameaddr, (atomnamelen << 1));
	  atomname = atomnameaddr + (atomnamelen - 1);
	  atomnamelen = atomnamelen << 1;
	  atomnamelast = atomnameaddr + (atomnamelen - 1);
	  /*printf("Allocated namebuf: %p, %d\n",atomnameaddr,atomnamelen);*/
	}
	*atomname++ = (char)c;
	term2 = cell(clref_val(term2)+1);
      } else {
	if (isref(term2)) err(INSTANTIATION, 2, call_name, 2);
	else xsb_type_error(CTXTc "list",term2,call_name,2,2);  /* atom_chars(X,[1]) */
  	SYS_MUTEX_UNLOCK(MUTEX_ATOM_BUF);
	return FALSE;	/* fail */
      }
    } while (1);
    bind_string((CPtr)(term), (char *)string_find((char *)atomnameaddr, 1));
    SYS_MUTEX_UNLOCK(MUTEX_ATOM_BUF);
    return TRUE;
  } else {	/* use is: ATOM --> CODES/CHARS */
    if (isstring(term)) {
      atomname = string_val(term);
      len = strlen(atomname);
      if (len == 0) {
	if (!isnonvar(list)) {
	  bind_nil((CPtr)(list)); 
          SYS_MUTEX_UNLOCK(MUTEX_ATOM_BUF);
	  return TRUE;
	}
	else 
	{ SYS_MUTEX_UNLOCK(MUTEX_ATOM_BUF);
	  return isnil(list);
	}
      } else {
	/* check that there is enough space on the heap! */
	check_glstack_overflow(2, pcreg, 2*len*sizeof(Cell)) ;
	list = ptoc_tag(CTXTc 2);   /* in case it changed */

	new_list = makelist(hreg);
	for (i = 0; i < len; i++) {
 	  if (call_type==ATOM_CODES)
	    follow(hreg++) = makeint(*(unsigned char *)atomname);
	  else {
	    tmpstr[0]=*atomname;
	    tmpstr[1]='\0';
	    tmpstr_interned=string_find(tmpstr,1);
	    follow(hreg++) = makestring(tmpstr_interned);
	  }
	  atomname++;
	  top = hreg++;
	  follow(top) = makelist(hreg);
	}
	follow(top) = makenil;
	SYS_MUTEX_UNLOCK(MUTEX_ATOM_BUF);
	return unify(CTXTc list, new_list);
      } 
    } else xsb_type_error(CTXTc "atom",term,call_name,2,1);  /* atom_codes(1,F) */
  }
  SYS_MUTEX_UNLOCK(MUTEX_ATOM_BUF);
  return TRUE;
}

inline static xsbBool number_to_list(CTXTdeclc int call_type)
{
  int i, tmpval;
  long c;
  char tmpstr[2], *tmpstr_interned;
  char str[256];	
  int StringLoc = 0;
  Cell heap_addr, term, term2;
  Cell list, new_list;
  char hack_char;	
  CPtr top = 0;
  char *call_name =
    (call_type == NUMBER_CODES ?
     "number_codes" : (call_type == NUMBER_DIGITS?
		       "number_digits" : "number_chars"));
  char *elt_type =
    (call_type == NUMBER_CODES ?
     "integer" : (call_type == NUMBER_DIGITS? "digit" : "digit atom"));


  term = ptoc_tag(CTXTc 1);
  list = ptoc_tag(CTXTc 2);
  if (!isnonvar(term)) {	/* use is: CHARS/CODES --> NUMBER */
    term2 = list;
    do {
      XSB_Deref(term2);
      if (isnil(term2)) {
	str[StringLoc++] = '\0';
	break;
      }
      if (islist(term2)) {
	heap_addr = cell(clref_val(term2)); XSB_Deref(heap_addr);
	if (((call_type==NUMBER_CODES) && (!isinteger(heap_addr)))
	    || ((call_type==NUMBER_CHARS) && !isstring(heap_addr))
	    || ((call_type==NUMBER_DIGITS)
		&& !isstring(heap_addr)
		&& !isinteger(heap_addr))) {
	  if (isnonvar(heap_addr))
	    xsb_type_error(CTXTc elt_type,list,call_name,2,2); /* number_chars(X,[a]) */
	  else err(INSTANTIATION, 2, call_name, 2);
	  return FALSE;	/* fail */
	}
	if (call_type==NUMBER_CODES)
	  c = int_val(heap_addr);
	else if ((call_type==NUMBER_DIGITS) && (isinteger(heap_addr))) {
	  tmpval = int_val(heap_addr);
	  if ((tmpval < 0) || (tmpval > 9)) {
	    xsb_type_error(CTXTc elt_type,list,call_name,2,2); /* number_chars(X,[11]) */
	  }
	  c = (long) '0' + int_val(heap_addr);
	} else if (isstring(heap_addr))
	  c = *string_val(heap_addr);
	else {
	    xsb_type_error(CTXTc "integer, digit, or atom",list,call_name,2,2); /* number_chars(X,[a]) */
	}

	if (c < 0 || c > 255) {
	  err_handle(CTXTc RANGE, 2, call_name, 2, "ASCII code", heap_addr);
	  return FALSE;	/* fail */
	}
	if (StringLoc > 200) return FALSE;
	str[StringLoc++] = (char)c;
	term2 = cell(clref_val(term2)+1);
      } else {
	if (isref(term2))
	  err(INSTANTIATION, 2, call_name, 2);
	else
	  xsb_type_error(CTXTc "list",term2,call_name,2,2);
	return FALSE;	/* fail */
      }
    } while (1);

    if (sscanf(str, "%ld%c", &c, &hack_char) == 1) {
      bind_int((CPtr)(term), c);
    } else {
      Float float_temp;
      //TODO: Refactor the below few lines of code once the "Floats are always double?" 
      //situation is resolved.
#ifndef FAST_FLOATS
      if (sscanf(str, "%lf%c", &float_temp, &hack_char) == 1)
#else
      if (sscanf(str, "%f%c", &float_temp, &hack_char) == 1)
#endif
	{
	  bind_boxedfloat((CPtr)(term), float_temp);
	}
      else return FALSE;	/* fail */
    }
  } else {	/* use is: NUMBER --> CHARS/CODES/DIGITS */
    if (isinteger(term)) {
      sprintf(str, "%ld", (long)int_val(term));
    } else {
      if (isofloat(term)) {
	sprintf(str, "%e", ofloat_val(term));
      } else {
	if (isboxedinteger(term)) {
	  sprintf(str,"%ld",(long)boxedint_val(term));
	} else {
	  xsb_type_error(CTXTc "number",term,call_name,2,1);
	}
      }
    }
    new_list = makelist(hreg);
    for (i=0; str[i] != '\0'; i++) {
      if (call_type==NUMBER_CODES)
	follow(hreg++) = makeint((unsigned char)str[i]);
      else if (call_type==NUMBER_CHARS) {
	tmpstr[0] = str[i];
	tmpstr[1] = '\0';
	tmpstr_interned=string_find(tmpstr,1);
	follow(hreg++) = makestring(tmpstr_interned);
      } else { /* NUMBER_DIGITS */
	tmpval = str[i] - '0';
	if (0 <= tmpval && tmpval < 10)
	  follow(hreg++) = makeint((unsigned char)str[i] - '0');
	else {
	  tmpstr[0] = str[i];
	  tmpstr[1] = '\0';
	  tmpstr_interned=string_find(tmpstr,1);
	  follow(hreg++) = makestring(tmpstr_interned);
	}
      }
      top = hreg++;
      follow(top) = makelist(hreg);
    } follow(top) = makenil;
    return unify(CTXTc list, new_list);
  }
  return TRUE;
}

#ifdef MULTI_THREAD

typedef int (*compfptr)(CTXTdeclc const void *, const void *) ;

#define INSERT_SORT	8

#define QSORT_SWAP(a,b) { Cell t = *a ; *a = *b ; *b = t ; }

#define QSORT_LESS(A,B) ((*qsort_cmp)((CTXT),(A),(B)) < 0)

void qsort0(CTXTdeclc compfptr qsort_cmp, CPtr low, CPtr high )
{
/* low is address of lowest element on array */
/* high is address of rightmost element on array */

	if ( high - low >= INSERT_SORT )
	{
		Cell pivot ;
		CPtr l, r ;
		CPtr mid = low + ( high - low ) / 2 ;

		if ( QSORT_LESS(mid,low) )
			QSORT_SWAP( mid, low )
		if ( QSORT_LESS(high,mid) )
		{	QSORT_SWAP( high, mid )
			if ( QSORT_LESS(mid,low) )
				QSORT_SWAP( mid, low ) 
		}
		pivot = *mid ;

		l = low + 1 ;
		r = high - 1 ;
		do
		{	while( QSORT_LESS(l, &pivot) ) l++ ;
			while( QSORT_LESS(&pivot, r) ) r-- ;

			if( l < r )
			{	QSORT_SWAP( l, r )
				l++; r--;
			}
			else if( l == r )
			{	l++; r--;
			}
		} while( l <= r ) ;
		qsort0(CTXTc qsort_cmp, low, r) ;
		qsort0(CTXTc qsort_cmp, l, high) ;
	}
	else if( low < high )		/* insertion sort for small lists */
	{	CPtr p, min = low, r ;
		
		/* set a sentinel to speed up insert sort main loop */
		for( p = low + 1 ; p <= high ; p++ )
			if( QSORT_LESS( p, min ) )
				min = p ;
		if( low != min )
			QSORT_SWAP( low, min ) ;

		for( r = low + 2 ; r <= high ; r++ )
		{	Cell new_el = *r ;

			for( p = r ; QSORT_LESS( &new_el, p-1 ) ; p-- )
				*p = *(p-1) ;
			*p = new_el ;
		}
	}
}

void mt_qsort(th_context *th, CPtr v, int len, unsigned int sz, compfptr comp)
{
	qsort0( th, comp, v, v + len - 1 ) ;
}
#endif /* MULTI_THREAD */

inline static xsbBool sort(CTXTdecl)
{
  /* r1: +list of terms; r2: ?sorted list of terms */
  int i, len;
  Cell *cell_tbl;
  Cell heap_addr, term, term2;
  Cell list, new_list;
  CPtr top = 0;

  list = ptoc_tag(CTXTc 1);
  term2 = list; len = 0;
  do {
    XSB_Deref(term2);
    if (isnil(term2)) break;
    if (islist(term2)) {
      len++; term2 = cell(clref_val(term2)+1);
    } else {
      if (isref(term2)) err(INSTANTIATION, 1, "sort", 2);
      else xsb_type_error(CTXTc "list",list,"sort",2,1);
    }
  } while(1);
  check_glstack_overflow(3, pcreg, (2*len)*sizeof(Cell)) ;
  list = ptoc_tag(CTXTc 1); /* reset in case moved */
  if (len > 0) {
    term2 = list;
    cell_tbl = (Cell *)malloc((len * sizeof(Cell)));
    if (!cell_tbl)
      xsb_abort("Cannot allocate temporary memory for sort/2");
    for (i=0 ; i < len ; ++i) {
      XSB_Deref(term2);	/* Necessary for correctness.	*/
      heap_addr = cell(clref_val(term2)); XSB_Deref(heap_addr);
      cell_tbl[i] = heap_addr;
      term2 = cell(clref_val(term2)+1);
    }
#ifndef MULTI_THREAD
    qsort(cell_tbl, len, sizeof(Cell), compare);
#else
    mt_qsort(CTXTc cell_tbl, len, sizeof(Cell), compare);
#endif
    new_list = makelist(hreg);
    follow(hreg++) = cell_tbl[0]; top = hreg++;
    follow(top) = makelist(hreg);
    for (i=1 ; i < len ; i++) {
      if (compare(CTXTc (void*)cell_tbl[i], (void*)cell_tbl[i-1])) {
	follow(hreg++) = cell_tbl[i];
	top = hreg++;
	follow(top) = makelist(hreg);
      }
    } follow(top) = makenil;
    free(cell_tbl);
    term = ptoc_tag(CTXTc 2);
    return unify(CTXTc new_list, term);
  }
  term = ptoc_tag(CTXTc 2);
  return unify(CTXTc list, term);
}

inline static xsbBool keysort(CTXTdecl)
{
  /* r1: +list of terms of the form Key-Value;	*/
  /* r2: ?sorted list of terms			*/
  int i, len;
  Cell heap_addr, term, term2;
  Cell list, new_list;
  Cell *cell_tbl;
  CPtr top = 0;

  list = ptoc_tag(CTXTc 1);
  term2 = list; len = 0;
  do {
    XSB_Deref(term2);
    if (isnil(term2)) break;
    if (islist(term2)) {
      heap_addr = cell(clref_val(term2)); XSB_Deref(heap_addr);
      if (isconstr(heap_addr) && 
	  get_arity(get_str_psc(heap_addr)) == 2 &&
	  !strcmp(get_name(get_str_psc(heap_addr)), "-")) {
	len++; term2 = cell(clref_val(term2)+1);
      } else {
	xsb_type_error(CTXTc "pair of the form Key-Value", (Cell)NULL,"keysort",2,1);
      }
    } else {
      if (isref(term2)) err(INSTANTIATION, 1, "keysort", 2);
      else err_handle(CTXTc TYPE, 1, "keysort", 2, "list", list);
      return FALSE;	/* fail */
    }
  } while(1);
  check_glstack_overflow(3, pcreg, (2*len)*sizeof(Cell)) ;
  list = ptoc_tag(CTXTc 1);  /* reset in case moved */
  term = ptoc_tag(CTXTc 2);
  if (len > 0) {
    term2 = list;
    cell_tbl = (Cell *)malloc(len * sizeof(Cell));
    if (!cell_tbl)
      xsb_abort("Cannot allocate temporary memory for keysort/2");
    for (i=0 ; i < len ; ++i) {
      XSB_Deref(term2);	/* Necessary for correctness.	*/
      heap_addr = cell(clref_val(term2)); XSB_Deref(heap_addr);
      cell_tbl[i] = heap_addr;
      term2 = cell(clref_val(term2)+1);
    }
#ifndef MULTI_THREAD
    qsort(cell_tbl, len, sizeof(Cell), key_compare);
#else
    mt_qsort(CTXTc cell_tbl, len, sizeof(Cell), key_compare);
#endif
    new_list = makelist(hreg);
    for (i=0 ; i < len ; i++) {
      follow(hreg++) = cell_tbl[i];
      top = hreg++;
      follow(top) = makelist(hreg);
    } follow(top) = makenil;
    free(cell_tbl);
    return unify(CTXTc new_list, term);
  }
  return unify(CTXTc list, term);
}

#ifndef MULTI_THREAD
struct sort_par_spec par_spec;
#endif

int par_key_compare(CTXTdeclc const void * t1, const void * t2) {
  long ipar, cmp, ind1, ind2;
  Cell term1 = (Cell) t1 ;
  Cell term2 = (Cell) t2 ;

  XSB_Deref(term1);		/* term1 is not in register! */
  XSB_Deref(term2);		/* term2 is not in register! */
  if (par_spec.sort_num_pars > 0) {
    ipar = 0;
    while (ipar < par_spec.sort_num_pars) {
      ind1 = ind2 = par_spec.sort_par_ind[ipar];
      if (islist(term1)) ind1--;
      if (islist(term2)) ind2--;
      cmp = compare(CTXTc (void*)cell(clref_val(term1)+ind1),
		          (void*)cell(clref_val(term2)+ind2));
      if (cmp) {
	if (par_spec.sort_par_dir[ipar]) return cmp;
	else return -cmp;
      } else ipar++;
    }
    return 0;
  } else if (par_spec.sort_num_pars == 0) {
    return compare(CTXTc (void*)term1, (void*)term2);
  } else
    return -compare(CTXTc (void*)term1, (void*)term2);
}

inline static xsbBool parsort(CTXTdecl)
{
  /* r1: +list of terms;				*/
  /* r2: +list of sort indicators: asc(I) or desc(I)	*/
  /* r3: 1 if eliminate dupls, 0 if not			*/
  /* r4: ?sorted list of terms				*/
  int i, len;
  int max_ind = 0, elim_dupls;
  Cell heap_addr, term, term2, tmp_ind;
  Cell list, new_list;
  Cell *cell_tbl;
  CPtr top = 0;
  char ermsg[50];

  elim_dupls = ptoc_int(CTXTc 3);

  list = ptoc_tag(CTXTc 2);
  term2 = list; par_spec.sort_num_pars = 0;

  XSB_Deref(term2);
  if (isstring(term2) && !strcmp(string_val(term2),"asc")) par_spec.sort_num_pars = 0;
  else if (isstring(term2) && !strcmp(string_val(term2),"desc")) par_spec.sort_num_pars = -1;
  else
    while (TRUE) {
      if (isnil(term2)) break;
      if (islist(term2)) {
	heap_addr = cell(clref_val(term2)); XSB_Deref(heap_addr);
	if (isconstr(heap_addr) && 
	    get_arity(get_str_psc(heap_addr)) == 1 &&
	    !strcmp(get_name(get_str_psc(heap_addr)),"asc")) {
	  par_spec.sort_par_dir[par_spec.sort_num_pars] = 1;
	} else if (isconstr(heap_addr) && 
		   get_arity(get_str_psc(heap_addr)) == 1 &&
		   !strcmp(get_name(get_str_psc(heap_addr)),"desc")) {
	  par_spec.sort_par_dir[par_spec.sort_num_pars] = 0;
	} else xsb_type_error(CTXTc "asc/1 or desc/1 term",heap_addr,"parsort",4,2);
	tmp_ind = cell(clref_val(heap_addr)+1); XSB_Deref(tmp_ind);
	if (!isinteger(tmp_ind)) xsb_type_error(CTXTc "integer arg for asc/1 or desc/1",tmp_ind,"parsort",4,2);
	i = int_val(tmp_ind);
	/* TLS: Should be range below */
	if (i < 1 || i > 255) err_handle(CTXTc TYPE,2,"parsort",4,"arity-sized integer",tmp_ind);
	par_spec.sort_par_ind[par_spec.sort_num_pars] = i;
	if (i > max_ind) max_ind = i;
	par_spec.sort_num_pars++;
	term2 = cell(clref_val(term2)+1);
	XSB_Deref(term2);
      } else xsb_type_error(CTXTc "list",list,"parsort",4,2);
    }
      
  list = ptoc_tag(CTXTc 1);
  term2 = list; len = 0;
  do {
    XSB_Deref(term2);
    if (isnil(term2)) break;
    if (islist(term2)) {
      heap_addr = cell(clref_val(term2)); XSB_Deref(heap_addr);
      if (par_spec.sort_num_pars <= 0 || 
	  (isconstr(heap_addr) && (get_arity(get_str_psc(heap_addr)) >= max_ind)) ||
	  (islist(heap_addr) && max_ind <=2)) {
	len++; term2 = cell(clref_val(term2)+1);
      } else {
	sprintf(ermsg,"Term with arity at least %d", max_ind);
	err_handle(CTXTc TYPE, 1, "parsort", 4, ermsg, (Cell)heap_addr);
	return FALSE;	/* fail */
      }
    } else {
      if (isref(term2)) err(INSTANTIATION, 1, "parsort", 4);
      else err_handle(CTXTc TYPE, 1, "parsort", 4, "list", list);
      return FALSE;	/* fail */
    }
  } while(1);

  check_glstack_overflow(4, pcreg, (2*len)*sizeof(Cell)) ;
  list = ptoc_tag(CTXTc 1);  /* reset in case moved */
  term = ptoc_tag(CTXTc 4);
  if (len > 0) {
    term2 = list;
    cell_tbl = (Cell *)malloc(len * sizeof(Cell));
    if (!cell_tbl)
      xsb_abort("Cannot allocate temporary memory for parsort/4");
    for (i=0 ; i < len ; ++i) {
      XSB_Deref(term2);	/* Necessary for correctness.	*/
      heap_addr = cell(clref_val(term2)); XSB_Deref(heap_addr);
      cell_tbl[i] = heap_addr;
      term2 = cell(clref_val(term2)+1);
    }
#ifndef MULTI_THREAD
    qsort(cell_tbl, len, sizeof(Cell), par_key_compare);
#else
    mt_qsort(CTXTc cell_tbl, len, sizeof(Cell), par_key_compare);
#endif
    new_list = makelist(hreg);
    if (elim_dupls) {
      follow(hreg++) = cell_tbl[0]; top = hreg++;
      follow(top) = makelist(hreg);
      for (i=1 ; i < len ; i++) {
	if (compare(CTXTc (void*)cell_tbl[i], (void*)cell_tbl[i-1])) {
	  follow(hreg++) = cell_tbl[i];
	  top = hreg++;
	  follow(top) = makelist(hreg);
	}
      } 
    } else {
      for (i=0 ; i < len ; i++) {
	follow(hreg++) = cell_tbl[i];
	top = hreg++;
	follow(top) = makelist(hreg);
      } 
    }
    follow(top) = makenil;
    free(cell_tbl);
    return unify(CTXTc new_list, term);
  }
  return unify(CTXTc list, term);
}

