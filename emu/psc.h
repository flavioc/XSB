/* File:      psc.h
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



/* The abstract module for the symbol table (PSC table) */

/*======================================================================*/
/* Type definitions: Psc						*/
/*======================================================================*/

struct psc_rec {
  byte env;			/* 0 - visible; 1 - local; 2 - unloaded */
  byte entry_type;		/* see below */
  byte arity;
  byte length;
  char *nameptr;
  byte *ep;      /* entry point, various meaning */
};

typedef struct psc_rec *Psc;

/* --- Pair    -------------------------------------------- */

struct psc_pair {
  Psc psc_ptr;
  struct psc_pair *next;
};

typedef struct psc_pair *Pair;

/* === env definition: (env) ======================================*/

#define T_VISIBLE	0
#define T_HIDDEN	1
#define T_UNLOADED	2

/* === type definition: (entry_type) ===============================*/

#define T_ORDI  0	/* constant-type: no ep definition, may be set later */
#define T_DYNA	1	/* constant-type: dynamic, code in buffer */
#define T_PRED  2	/* constant-type: ep points to compiled code */

#define T_MODU  4	/* ep field is used to hold psc-list */
#define T_FILE  5	/* ep field could be file descriptor (not now) */

#define T_UDEF 12	/* unloaded T_PRED */
#define T_FORN 13	/* predicate in foreign language */

/*======================================================================*/
/* Interface macros (in the following "psc" is typed "Psc")		*/
/*======================================================================*/

#define  get_type(psc)		((psc)->entry_type)
#define  get_env(psc)		((psc)->env & 0x0f)
#define  get_spy(psc)		((psc)->env & 0xf0)
#define  get_arity(psc)		((psc)->arity)
#define  get_ep(psc)		((psc)->ep)
#define  get_name(psc)		((psc)->nameptr)

#define  set_type(psc, type)	(psc)->entry_type = type
#define  set_env(psc, envir)	(psc)->env = get_spy(psc) | envir
#define  set_spy(psc, spy)	(psc)->env = get_env(psc) | spy
#define  set_arity(psc, ari)	((psc)->arity = ari)
#define  set_length(psc, len)	((psc)->length = len)
#define  set_ep(psc, val)	((psc)->ep = val)
#define  set_name(psc, name)	((psc)->nameptr = name)

#define  pair_psc(pair)		((pair)->psc_ptr)
#define  pair_next(pair)	((pair)->next)

/*======================================================================*/
/* Interface routines							*/
/*======================================================================*/

extern Pair link_sym(Psc, Psc);
extern Pair insert_module(int, char *);
extern Pair insert(char *, char, Psc, int *);

extern char* string_find(char*, int);

/*======================================================================*/
/*  Special instance (0-arity interface functions)			*/
/*======================================================================*/

extern Psc global_mod;			/* PSC for "global" */
extern Psc list_psc;
extern Psc comma_psc;
extern Psc tnot_psc;
extern Psc delay_psc;
extern char *nil_sym;
extern Pair list_str;
extern char *list_dot;

extern Psc ret_psc[];
extern Psc get_ret_psc(int);

extern Psc get_intern_psc();

/*======================================================================*/
/*  HiLog related macros.						*/
/*======================================================================*/

#define hilog_psc(psc)	\
		(((!strcmp(get_name(psc),"apply")) && (get_arity(psc) > 1)))
#define hilog_cs(term) /* to be used when known that term is a CS */ \
		((hilog_psc(get_str_psc(term))))
#define hilog_term(term) \
		((cell_tag(term) == CS) && (hilog_psc(get_str_psc(term))))

/*----------------------------------------------------------------------*/
