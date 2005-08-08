/* File:      psc_xsb.h
** Author(s): Jiyang Xu, Terry Swift, Kostis Sagonas
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

#ifndef __PSC_XSB_H__
#define __PSC_XSB_H__

#ifndef SYMBOL_TABLE_DEFS

#define SYMBOL_TABLE_DEFS

/* The abstract module for the symbol table (PSC table) */

/*======================================================================*/
/* Type definitions: Psc						*/
/*======================================================================*/

struct psc_rec {
  byte env;			/* 0&0x3 - visible; 1&0x3 - local; 2&0x3 - unloaded;  */
  				/* 0xc0, 2 bits for spy */
				/* 0x20 - shared; 0x8 - tabled */
  byte entry_type;		/* see psc_defs.h */
  byte arity;
  byte length;
  char *nameptr;
  struct psc_rec *data;      /* psc of module, if pred; otw data */
  byte *ep;                     /* entry point (initted to next word) */
  word load_inst;               /* byte-code load_pred, or call_forn */
  struct psc_rec *this_psc;     /* BC arg: this psc or foreign entry point */
};

typedef struct psc_rec *Psc;

/* --- Pair    -------------------------------------------- */

struct psc_pair {
  Psc psc_ptr;
  struct psc_pair *next;
};

typedef struct psc_pair *Pair;

/* === env definition: (env) ======================================*/

/* Type definitions */
#include "psc_defs.h"

/*======================================================================*/
/* Interface macros (in the following "psc" is typed "Psc")		*/
/*======================================================================*/

#define  get_type(psc)		((psc)->entry_type)
#define  get_env(psc)		((psc)->env & 0x03)
#define  get_spy(psc)		((psc)->env & 0xc0)
#define  get_shared(psc)	((psc)->env & 0x20)
#define  get_tabled(psc)	((psc)->env & 0x08)
#define  get_arity(psc)		((psc)->arity)
#define  get_ep(psc)		((psc)->ep)
#define  get_data(psc)		((psc)->data)
#define  get_name(psc)		((psc)->nameptr)

#define  set_type(psc, type)	(psc)->entry_type = type
#define  set_env(psc, envir)	(psc)->env = get_spy(psc) | get_shared(psc) | get_tabled(psc) | envir
#define  set_spy(psc, spy)	(psc)->env = spy | get_shared(psc) | get_tabled(psc) | get_env(psc)
#define  set_shared(psc, shar)	(psc)->env = get_spy(psc) | shar | get_tabled(psc) | get_env(psc)
#define  set_tabled(psc, tab)	(psc)->env = get_spy(psc) | get_shared(psc) | tab | get_env(psc)
#define  set_arity(psc, ari)	((psc)->arity = ari)
#define  set_length(psc, len)	((psc)->length = len)
#define  set_ep(psc, val)	((psc)->ep = val)
#define  set_data(psc, val)     ((psc)->data = val)
#define  set_name(psc, name)	((psc)->nameptr = name)

#define set_forn(psc, val) {                   \
    cell_opcode(get_ep(psc)) = call_forn;      \
    *(((byte **)get_ep(psc))+1) = val;         \
}

#define  pair_psc(pair)		((pair)->psc_ptr)
#define  pair_next(pair)	((pair)->next)

/*======================================================================*/
/* Interface routines							*/
/*======================================================================*/

extern Pair link_sym(Psc, Psc);
extern Pair insert_module(int, char *);
extern Pair insert(char *, byte, Psc, int *);

extern char* string_find(char*, int);

/*======================================================================*/
/*  Special instance (0-arity interface functions)			*/
/*======================================================================*/

extern Psc global_mod;			/* PSC for "global" */
extern Psc true_psc;
extern Psc if_psc;
extern Psc list_psc;
extern Psc comma_psc;
extern Psc box_psc;
extern Psc tnot_psc;
extern Psc colon_psc;
extern Psc delay_psc;
extern char *nil_sym;
extern char *true_sym;
extern Pair list_str;
extern char *list_dot;

extern Psc ret_psc[];
extern Psc get_ret_psc(int);
inline static char *get_ret_string()	{ return (char *)ret_psc[0]; }

extern Psc get_intern_psc();

//extern struct Table_Info_Frame *get_tip(CTXTdeclc Psc);

extern void print_symbol_table();
extern Psc get_psc_from_ep(void *);

/*======================================================================*/
/*  HiLog related macros.						*/
/*======================================================================*/

#define hilog_psc(psc)	\
		(((!strcmp(get_name(psc),"apply")) && (get_arity(psc) > 1)))
#define hilog_cs(term) /* to be used when known that term is a XSB_STRUCT */ \
		((hilog_psc(get_str_psc(term))))
#define hilog_term(term) \
		((cell_tag(term) == XSB_STRUCT) && (hilog_psc(get_str_psc(term))))

/*----------------------------------------------------------------------*/

#endif

#endif /* __PSC_XSB_H__ */
