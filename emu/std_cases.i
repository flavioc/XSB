/*  -*-c-*-  Make sure this file comes up in the C mode of emacs */ 
/* File:      std_pred_cases.i
** Author(s): Kostis F. Sagonas
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



  case VAR:		/* r1: ?term */
    return isref(ptoc_tag(1));
    
  case NONVAR:	/* r1: ?term */
    return isnonvar(ptoc_tag(1));
    
  case ATOM:		/* r1: ?term */
    return isatom(ptoc_tag(1));
    
  case INTEGER:	/* r1: ?term */
    return isinteger(ptoc_tag(1));
    
  case REAL:		/* r1: ?term */
    return isfloat(ptoc_tag(1));
    
  case NUMBER:	/* r1: ?term */
    return isnumber(ptoc_tag(1));
    
  case ATOMIC:	/* r1: ?term */
    term = ptoc_tag(1);
    return isatomic(term);
    
  case COMPOUND:	/* r1: ?term */
    term = ptoc_tag(1);
    return ((isconstr(term) && get_arity(get_str_psc(term))) ||
	    (islist(term)));
    
  case CALLABLE:	/* r1: ?term */
    term = ptoc_tag(1);
    return (isconstr(term) || isstring(term) || islist(term));
    
  case IS_LIST:	/* r1: ?term */
    return is_proper_list(ptoc_tag(1));
    
  case FUNCTOR:	/* r1: ?term; r2: ?functor; r3: ?arity (int)	*/
    return functor_builtin();
    
  case ARG:	/* r1: +index (int); r2: +term; r3: ?arg (term) */
    return arg_builtin();
    
  case UNIV:	/* r1: ?term; r2: ?list	*/
    return univ_builtin();
    
  case HiLog_ARG:	/* r1: +index (int); r2: +term; r3: ?arg (term) */
    return hilog_arg();

  case HiLog_UNIV:	/* r1: ?term; r2: ?list	*/
    break;
    
  /* atom_chars should be redefined to return char-atoms rather than ASCII
     codes */ 
  case ATOM_CHARS:	/* r1: ?term; r2: ?character list	*/
  case ATOM_CODES:	/* r1: ?term; r2: ?character list	*/
    return atom_codes();
    
  /* number_chars should be redefined to return digit-atoms */
  case NUMBER_CHARS:	/* r1: ?term; r2: ?character list	*/
  case NUMBER_CODES:	/* r1: ?term; r2: ?character list	*/
    return number_chars();
    
  case PUT:	/* r1: +integer	*/
    term = ptoc_tag(1);
    if (isinteger(term)) {
      putc(int_val(term), fileptr(flags[CURRENT_OUTPUT]));
    } else {
      if (isnonvar(term)) err_handle(TYPE, 1, "put", 1, "integer", term);
      else err(INSTANTIATION, 1, "put", 1);
    }
    break;
    
  case TAB:	/* r1: +integer	*/
    term = ptoc_tag(1);
    if (isinteger(term)) {
      for (i=1; i<=int_val(term); i++)
	putc(32, fileptr(flags[CURRENT_OUTPUT]));	/* 32=' ' */
    } else {
      if (isnonvar(term)) err_handle(TYPE, 1, "tab", 1, "integer", term);
      else err(INSTANTIATION, 1, "tab", 1);
    }
    break;
    
  case SORT:		/* r1: +list of terms; r2: ?sorted list of terms */
  return sort();
    
  case KEYSORT:	/* r1: +list of terms of the form Key-Value;	*/
    /* r2: ?sorted list of terms			*/
   return keysort();
