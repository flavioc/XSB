/* File:      std_pred.i
** Author(s): Kostis F. Sagonas
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
	term = ptoc_tag(1);
	if (isnonvar(term)) {
	    if (isconstr(term)) {
		psc = get_str_psc(term);
		name = get_name(psc);
		arity = get_arity(psc);
		return (atom_unify(makestring(name), ptoc_tag(2)) &&
			int_unify(makeint(arity), ptoc_tag(3)));
	    } else if (islist(term))
			return (atom_unify(makestring(list_dot), ptoc_tag(2)) &&
				int_unify(makeint(2), ptoc_tag(3)));
		   else return (unify(term, ptoc_tag(2)) &&
				int_unify(makeint(0), ptoc_tag(3)));
	} else {	/* term is a variable */
	    functor = ptoc_tag(2);
	    if (isstring(functor) || isinteger(functor) || isfloat(functor) ||
		(isconstr(term) && get_arity(get_str_psc(term)) == 0)) {
		arity = ptoc_tag(3);
		if (arity_integer(arity)) {
		    value = int_val(arity);
		    if (value == 0) return unify(functor, term);
		    else {
			if (value == 2 && isstring(functor) 
				       && string_val(functor) == list_dot) {
			    /* term is a variable and I bind it to a list. */
			    bind_list((CPtr)term, hreg);
			    new_heap_free(hreg);
			    new_heap_free(hreg);
			}
	 		else { 
			/* functor always creates a psc in the current module */
			    sym = (Pair)insert(string_val(functor), value, 
					       (Psc)flags[CURRENT_MODULE],
					       &new_indicator);
			    sreg = hreg;
			    hreg += value+1;	/* need (arity+1) new cells */
			    bind_cs((CPtr)term, sreg);
			    new_heap_functor(sreg, sym->psc_ptr);
			    for (disp=0; disp<value; disp++) {
				new_heap_free(sreg);
			    }
			} return 1;	/* always succeed! */
		    }
		} else {
		    if (isnonvar(arity)) {
			if (isinteger(arity))
			     err_handle(RANGE, 3, "functor", 3,
					"integer in the range 0..255", arity);
			else err_handle(TYPE, 3, "functor", 3, "integer",arity);
		    } else {
			if (isnumber(functor))
			     return (unify(term, functor) && 
				     int_unify(makeint(0), arity));
			else err(INSTANTIATION, 3, "functor", 3);
		    }
		}
	    } else {
		if (isnonvar(functor))
		     err_handle(TYPE, 2, "functor", 3, "atom", functor);
		else err(INSTANTIATION, 2, "functor", 3);
	    }
	}
	break;

    case ARG:	/* r1: +index (int); r2: +term; r3: ?arg (term) */
	index = ptoc_tag(1);
	if (isinteger(index)) {
	    if ((disp = int_val(index)) > 0) {
		term = ptoc_tag(2);
		if (isnonvar(term)) {
		    if (isconstr(term)) {
			if (disp <= (int)get_arity(get_str_psc(term))) {
			    return unify((Cell)(clref_val(term)+disp),
					 ptoc_tag(3));
			} 
			else return 0;	/* fail */
		    } else if (islist(term) && (disp==1 || disp==2)) {
			      return unify((Cell)(clref_val(term)+disp-1),
					   ptoc_tag(3));
		           } else return 0;	/* fail */
		 } else err(INSTANTIATION, 2, "arg", 3);
	    } else return 0;	/* fail */
	} else {
	    if (isnonvar(index)) err_handle(TYPE, 1, "arg", 3, "integer",index);
	    else err(INSTANTIATION, 1, "arg", 3);
	}
	break;

    case UNIV:	/* r1: ?term; r2: ?list	*/
	term = ptoc_tag(1);
	list = ptoc_tag(2);
	if (isnonvar(term)) {	/* Usage is deconstruction of terms */
	    new_list = makelist(hreg);
	    if (isconstr(term) && (arity = (get_arity(get_str_psc(term))))) {
		follow(hreg++) = makestring(get_name(get_str_psc(term)));
		top = hreg++;
		for (i = 1 ; i <= arity ; i++) {
		    follow(top) = makelist(hreg); top = hreg++;
		    follow(top) = (Cell)(clref_val(term)+i); top = hreg++;
		}
	    }
	    else if (isatomic(term)) { follow(hreg++) = term; top = hreg++; }
	    else { /* term is list */
		follow(hreg++) = makestring(list_dot);
		top = hreg++;
		follow(top) = makelist(hreg); top = hreg++;
		follow(top) = (Cell)(clref_val(term)); top = hreg++;
		follow(top) = makelist(hreg); top = hreg++;
		follow(top) = (Cell)(clref_val(term)+1); top = hreg++;
	    }
	    follow(top) = makenil;
	    return unify(list, new_list);
	} else { /* usage is construction; term is known to be a variable */
	    if (islist(list)) {
	        head = clref_val(list);
		deref(cell(head));
	        if (isatom(cell(head))) {
		    if (isnil(cell(head+1))) {	/* atom construction */
			bind_copy((CPtr)term, cell(head));
			return TRUE;	/* succeed */
		    } else {
			bool list_construction = FALSE;
			name = string_val(cell(head));
			if (!strcmp(name, ".")) { /* check for list construction */
			    list = cell(head+1); deref(list);
			    if (islist(list)) {
				list = cell(clref_val(list)+1); deref(list);
				if (islist(list)) {
				    list = cell(clref_val(list)+1); deref(list);
				    if (isnil(list)) list_construction = TRUE;
				}
			    }
			}
			if (list_construction) { /* no errors can occur */
			    bind_list((CPtr)term, hreg);
			    list = cell(head+1);
			    bld_copy(hreg, cell(clref_val(list))); hreg++;
			    list = cell(clref_val(list)+1);
			    bld_copy(hreg, cell(clref_val(list))); hreg++;
			} else { /* compound term construction */
			    sreg = hreg;
			    bind_cs((CPtr)term, sreg); hreg = sreg; sreg++;
			    for (arity = 0, list = cell(head+1); ;
			         arity++, list = cell(clref_val(list)+1)) {
				deref(list); /* necessary */
				if (!islist(list)) break; /* really ugly */
				bld_copy(sreg, cell(clref_val(list))); sreg++;
			    }
			    if (isnil(list) && arity <= MAX_ARITY) {
				/* '=..'/2 always creates a psc
				 * in the current module */
				sym = (Pair)insert(name, arity,
						   (Psc)flags[CURRENT_MODULE],
						   &new_indicator);
				new_heap_functor(hreg, sym->psc_ptr);
				hreg = sreg+1;
			    } else {
				hreg = hreg-1;	/* restore hreg */
				if (arity > MAX_ARITY)
				    err_handle(IMPLEMENTATION, 2, "=..", 2,
					       "list of at most 255 elements",
					       arity);
				else err_handle(TYPE, 2, "=..", 2,
					        "[]-terminated list", list);
				return FALSE;
			    }
			}
		    } return TRUE;
	        }
	        if (isnumber(cell(head)) && isnil(cell(head+1))) { /* list=[num] */
		    bind_copy((CPtr)term, cell(head));	 /* term<-num  */
		    return TRUE;	/* succeed */
	        }
	        else err_handle(TYPE, 2, "=..", 2,
				"[]-terminated list whose first element is atomic",
				list);
	    }
	    if (isnonvar(list))
	        err_handle(TYPE, 2, "=..", 2,
			   "[]-terminated list whose first element is atomic",
			   list);
	    else err(INSTANTIATION, 2, "=..", 2);
	}
	break;

    case HiLog_ARG:	/* r1: +index (int); r2: +term; r3: ?arg (term) */
	index = ptoc_tag(1);
	if (isinteger(index)) {
	    if ((disp = int_val(index)) > 0) {
		term = ptoc_tag(2);
		if (isnonvar(term)) {
		    if (isconstr(term)) {
			if (hilog_cs(term)) disp++;
			if (disp <= (int)get_arity(get_str_psc(term))) {
			    return unify((Cell)(clref_val(term)+disp),
					 ptoc_tag(3));
			} return 0;		/* fail */
		    } else if (islist(term) && (disp==1 || disp==2)) {
			      return unify((Cell)(clref_val(term)+disp-1),
					   ptoc_tag(3));
		           } else return 0;	/* fail */
		 } else err(INSTANTIATION, 2, "hilog_arg", 3);
	    } else return 0;	/* fail */
	} else {
	    if (isnonvar(index))
		 err_handle(TYPE, 1, "hilog_arg", 3, "integer", index);
	    else err(INSTANTIATION, 1, "hilog_arg", 3);
	}
	break;

    case HiLog_UNIV:	/* r1: ?term; r2: ?list	*/
	break;

    case ATOM_CHARS:	/* r1: ?term; r2: ?character list	*/
	term = ptoc_tag(1);
	list = ptoc_tag(2);
	if (!isnonvar(term)) {	/* use is: CHARS --> ATOM */
	    name = (char *)hreg; term2 = list;	/* use heap for temp storage */
	    do {
		deref(term2);
		if (isnil(term2)) {
		    *name++ = '\0';
		    break;
		}
		if (islist(term2)) {
		    heap_addr = cell(clref_val(term2)); deref(heap_addr);
		    if (!isinteger(heap_addr)) {
			if (isnonvar(heap_addr))
			  err_handle(TYPE, 2, "atom_chars", 2, "integer", list);
			else err(INSTANTIATION, 2, "atom_chars", 2);
			return 0;	/* fail */
		    }
		    c = int_val(heap_addr);
		    if (c < 0 || c > 255) {
			err_handle(RANGE, 2, "atom_chars", 2,
				   "ASCII code", heap_addr);
		        return 0;	/* fail */
		    }
		    *name++ = c;
		    term2 = cell(clref_val(term2)+1);
		} else {
		    if (isref(term2)) err(INSTANTIATION, 2, "atom_chars", 2);
		    else err_handle(TYPE, 2, "atom_chars", 2, "list", term2);
		    return 0;	/* fail */
		}
	    } while (1);
	    bind_string((CPtr)(term), (char *)string_find((char *)hreg, 1));
	    return TRUE;
	} else {	/* use is: ATOM --> CHARS */
	    if (isstring(term)) {
		name = string_val(term);
		len = strlen(name);
		if (len == 0) {
		    if (!isnonvar(list)) {
			bind_nil((CPtr)(list)); return TRUE;
		    }
		    else return isnil(list);
		} else {
		    new_list = makelist(hreg);
		    for (i = 0; i < len; i++) {
			 follow(hreg++) = makeint(*(unsigned char *)name);
			 name++;
			 top = hreg++;
			 follow(top) = makelist(hreg);
		    } follow(top) = makenil;
		    return unify(list, new_list);
		} 
	    } else err_handle(TYPE, 1, "atom_chars", 2, "atom", term);
	}
	break;

    case NUMBER_CHARS:	/* r1: ?term; r2: ?character list	*/
	term = ptoc_tag(1);
	list = ptoc_tag(2);
	if (!isnonvar(term)) {	/* use is: CHARS --> NUMBER */
	    name = str; term2 = list;
	    do {
		deref(term2);
		if (isnil(term2)) {
		    *name++ = '\0';
		    break;
		}
		if (islist(term2)) {
		    heap_addr = cell(clref_val(term2)); deref(heap_addr);
		    if (!isinteger(heap_addr)) {
			if (isnonvar(heap_addr))
			  err_handle(TYPE, 2, "number_chars",2, "integer",list);
			else err(INSTANTIATION, 2, "number_chars", 2);
			return 0;	/* fail */
		    }
		    c = int_val(heap_addr);
		    if (c < 0 || c > 255) {
			err_handle(RANGE, 2, "number_chars", 2,
				   "ASCII code", heap_addr);
		        return 0;	/* fail */
		    }
		    *name++ = c;
		    term2 = cell(clref_val(term2)+1);
		} else {
		    if (isref(term2)) err(INSTANTIATION, 2, "number_chars", 2);
		    else err_handle(TYPE, 2, "number_chars", 2, "list", term2);
		    return 0;	/* fail */
		}
	    } while (1);
	    if (sscanf(str, "%ld%c", &c, &hack_char) == 1) {
		bind_int((CPtr)(term), c);
	    } else {
#ifdef BITS64
		if (sscanf(str, "%le%c", &float_temp, &hack_char) == 1) {
#else
		if (sscanf(str, "%e%c", &float_temp, &hack_char) == 1) {
#endif
		    bind_float((CPtr)(term), float_temp);
		} else return 0;	/* fail */
	    }
	} else {	/* use is: NUMBER --> CHARS */
	    if (isinteger(term)) {
		sprintf(str, "%ld", (long)int_val(term));
	    } else {
		if (isfloat(term)) {
		    sprintf(str, "%E", float_val(term));
		} else {
		    err_handle(TYPE, 1, "number_chars", 2, "number", term);
		    return 0;	/* fail */
		}
	    }
	    new_list = makelist(hreg);
	    for (i=0; str[i] != '\0'; i++) {
		follow(hreg++) = makeint((unsigned char)str[i]);
		top = hreg++;
		follow(top) = makelist(hreg);
	    } follow(top) = makenil;
	    return unify(list, new_list);
	}
	break;

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
	list = ptoc_tag(1);
	term = ptoc_tag(2);
	term2 = list; len = 0;
	do {
	    deref(term2);
	    if (isnil(term2)) break;
	    if (islist(term2)) {
		len++; term2 = cell(clref_val(term2)+1);
	    } else {
		if (isref(term2)) err(INSTANTIATION, 1, "sort", 2);
		else err_handle(TYPE, 1, "sort", 2, "list", list);
		return 0;	/* fail */
	    }
	} while(1);
	if (len > 0) {
	    term2 = list;
	    cell_tbl = (Cell *)malloc((len * sizeof(Cell)));
	    if (!cell_tbl)
		xsb_abort("Cannot allocate temporary memory for sort/2");
	    for (i=0 ; i < len ; ++i) {
		deref(term2);	/* Necessary for correctness.	*/
		heap_addr = cell(clref_val(term2)); deref(heap_addr);
		cell_tbl[i] = heap_addr;
		term2 = cell(clref_val(term2)+1);
	    }
	    qsort(cell_tbl, len, sizeof(Cell), compare);
	    new_list = makelist(hreg);
	    follow(hreg++) = cell_tbl[0]; top = hreg++;
	    follow(top) = makelist(hreg);
	    for (i=1 ; i < len ; i++) {
		if (compare(cell_tbl[i], cell_tbl[i-1])) {
		    follow(hreg++) = cell_tbl[i];
		    top = hreg++;
		    follow(top) = makelist(hreg);
		}
	    } follow(top) = makenil;
	    free(cell_tbl);
	    return unify(new_list, term);
	}
	return unify(list, term);

    case KEYSORT:	/* r1: +list of terms of the form Key-Value;	*/
			/* r2: ?sorted list of terms			*/
	list = ptoc_tag(1);
	term = ptoc_tag(2);
	term2 = list; len = 0;
	do {
	    deref(term2);
	    if (isnil(term2)) break;
	    if (islist(term2)) {
		heap_addr = cell(clref_val(term2)); deref(heap_addr);
		if (isconstr(heap_addr) && 
		    get_arity(get_str_psc(heap_addr)) == 2 &&
		    !strcmp(get_name(get_str_psc(heap_addr)), "-")) {
			len++; term2 = cell(clref_val(term2)+1);
		} else {
			err_handle(TYPE, 1, "keysort", 2,
				   "pair of the form Key-Value", (Cell)NULL);
			return 0;	/* fail */
		}
	    } else {
		if (isref(term2)) err(INSTANTIATION, 1, "keysort", 2);
		else err_handle(TYPE, 1, "keysort", 2, "list", list);
		return 0;	/* fail */
	    }
	} while(1);
	if (len > 0) {
	    term2 = list;
	    cell_tbl = (Cell *)malloc(len * sizeof(Cell));
	    if (!cell_tbl)
		xsb_abort("Cannot allocate temporary memory for keysort/2");
	    for (i=0 ; i < len ; ++i) {
		deref(term2);	/* Necessary for correctness.	*/
		heap_addr = cell(clref_val(term2)); deref(heap_addr);
		cell_tbl[i] = heap_addr;
		term2 = cell(clref_val(term2)+1);
	    }
	    qsort(cell_tbl, len, sizeof(Cell),  key_compare);
	    new_list = makelist(hreg);
	    for (i=0 ; i < len ; i++) {
		follow(hreg++) = cell_tbl[i];
		top = hreg++;
		follow(top) = makelist(hreg);
	    } follow(top) = makenil;
	    free(cell_tbl);
	    return unify(new_list, term);
	}
	return unify(list, term);
