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

/* PSC records are highly overloaded structures. See psc_defs.h for
   further documentation.

   env_byte: Two lowest-order bits of env byte indicate whether the
   symbol is visible by any module, local to a module, or unloaded.
   Bit 3 indicates whether the predicate is tabled for subsumption,
   bit 4 indicates whether the predicate is tabled for variance.
   (Bits 3 and 4 can both be on, indicating that the predicate is
   tabled, but whether it is variant or subsumptive has not yet been
   determined.)  Bit 5 indicates whether it has been determined that
   the predicate is thread-shared or thread-private.  Bit 6 indicates
   the predicate is shared among threads in the MT engine.  Thus, bit
   5 is meaningful only if bit 5 is also set.  

   Bits 7 and 8 are used for get_spy.

   data: If the psc record indicates a predicate data indicates its
   module; otherwise it contains data, as used in conpsc-style
   functions.  (how about foreign?)

   ep/load_inst: If the psc record indicates a (loaded prolog)
   predicate name, then the ep is its entry point; otherwise if a
   module, its ep is the beginning of the chain of psc pairs for
   predicates in the module.  

   If the psc record indicates a loaded foreign function ep points to
   the call_forn instruction, and load_inst is a pointer to the
   function itself.

   If the psc record indicates an unloaded predicate/foreign function,
   the ep points to the load_pred instruction, and this_psc is its
   opcode.  The action of calling this instruction will be to load the
   predicate, set the ep to the entry point of the byte code, and then
   branch to the byte code.

*/

struct psc_rec {
  byte env;			/* 0&0x3 - visible; 1&0x3 - local; 2&0x3 - unloaded;  */
  				/* 0xc0, 2 bits for spy */
				/* 0x20 - shared, 0x10 for determined; 0x8 - tabled */
  byte incr;                    /* 1 is incremental; 0 is non-incremental, 2: opaque, incremental evaluation */
  byte entry_type;		/* see psc_defs.h */
  byte arity; 
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
#define  get_env(psc)		((psc)->env & T_ENV)
#define  get_spy(psc)		((psc)->env & T_SPY)
#define  get_shared(psc)	((psc)->env & T_SHARED)
#define  get_private(psc)	((psc)->env & ~T_SHARED & T_SHARED_DET)
#define  get_tabled(psc)	((psc)->env & T_TABLED)
#define  get_incr(psc)          (((psc)->incr & 3) == 1)  /* incremental */
#define  get_opaque(psc)        (((psc)->incr & 3) == 2)  /* incremental */

// get_xxx_tabled will also succeed if tabling type is not yet known
// set_shared is also used to set_private
#define  get_subsumptive_tabled(psc)	((psc)->env & T_TABLED_SUB & ~T_TABLED_VAR)
#define  get_variant_tabled(psc)	((psc)->env & T_TABLED_VAR & ~T_TABLED_SUB)
#define  get_arity(psc)		((psc)->arity)
#define  get_ep(psc)		((psc)->ep)
#define  get_data(psc)		((psc)->data)
#define  get_name(psc)		((psc)->nameptr)

#define  set_type(psc, type)	(psc)->entry_type = type
#define  set_env(psc, envir)	(psc)->env = ((psc)->env & ~T_ENV) | envir
#define  set_spy(psc, spy)	(psc)->env = ((psc)->env & ~T_SPY) | spy
#define  set_shared(psc, shar)	(psc)->env = ((psc)->env & ~T_SHARED) | shar
#define  set_tabled(psc, tab)	(psc)->env = ((psc)->env & ~T_TABLED) | tab
#define  set_incr(psc,val)      ((psc)->incr = ((psc)->incr & 3) | val)  /* incremental */
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

extern char* string_find(const char*, int);

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
extern Psc ccall_mod_psc;
extern Psc c_callloop_psc;
extern Psc delay_psc;
extern Psc cond_psc;
extern Psc cut_psc;
extern Psc load_undef_psc;
extern char *nil_string;
extern char *true_string;
extern Pair list_pscPair;
extern char *list_dot_string;

extern int force_string_gc;

extern Psc ret_psc[];
extern Psc get_ret_psc(int);
inline static char *get_ret_string()	{ return (char *)ret_psc[0]; }

extern Psc get_intern_psc();

/* Can't use CTXTdeclc here because its included early in context.h */
#ifdef MULTI_THREAD
extern struct Table_Info_Frame *get_tip(struct th_context *, Psc);
#else
extern struct Table_Info_Frame *get_tip(Psc);
#endif

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
