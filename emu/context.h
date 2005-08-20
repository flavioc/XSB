/* File:      context.h
** Author(s): Warren, Swift, Xu, Sagonas
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

#ifndef __CONTEXT_H__

#define __CONTEXT_H__

#include "cell_def_xsb.h"
#include "basictypes.h"
#include "setjmp_xsb.h"
#include "flag_defs_xsb.h"

typedef struct ClRefHdr
{	unsigned long buflen ;
	struct ClRefHdr *prev ;
}	*ClRef, ClRefData, ClRefHdr ;

struct token_t {
  int type;
  char *value;
  int nextch;
};

struct funstktype {                                         
    char *fun;		/* functor name */
    int funop;	        /* index into opstk of first operand */
    int funtyp; 	/* 0 if functor, 1 if list, 2 if dotted-tail list */
};

struct opstktype {
    int typ;
    prolog_term op;
};

#define MAXVAR 1000
struct vartype {
  Cell varid;
  prolog_term varval;
};

struct sort_par_spec {
  long sort_num_pars;
  long sort_par_dir[10];
  long sort_par_ind[10];
};

struct random_seeds_t {
  short IX;
  short IY;
  short IZ;

  double TX;
  double TY;
  double TZ;
};

struct asrtBuff_t {
 char *Buff;
 int Buff_size;
 int *Loc;
 int BLim;
 int Size;
};

#define MAX_RETRACTED_CLAUSES   20

#ifdef MULTI_THREAD

#include <sys/types.h>
#include <stdio.h>

#include "debugs/debug_attv.h"

#include "basictypes.h"
#include "memory_xsb.h"
#include "varstring_xsb.h"
#include "dynamic_stack.h"
#include "psc_xsb.h"
#include "tries.h"
#include "choice.h"
#include "macro_xsb.h"
#include "token_defs_xsb.h"
#include "odbc_def_xsb.h"
#include "findall.h"


//BELOW INCLUDES ARE FOR SQL Interfaces
#ifdef CYGWIN
#define FAR
#include "sql.h"
#else
#ifdef WIN_NT
#include <windows.h>
#include <sql.h>
#endif
#endif
//end of SQL related includes.

#define MAX_REGS 257

struct th_context
{
/* System & user Flags */

int _call_intercept ;

/* The SLG-WAM data regions
   ------------------------ */

System_Stack	_pdl,           /* PDL                   */
		_glstack,	/* Global + Local Stacks */
		_tcpstack,	/* Trail + CP Stack      */
		_complstack;	/* Completion Stack  */

/* Argument Registers
   ------------------ */
Cell _reg[MAX_REGS];


/* Special Registers
   ----------------- */
CPtr _ereg;		/* last activation record       */
CPtr _breg;		/* last choice point            */
CPtr _hreg;		/* top of heap                  */
CPtr *_trreg;		/* top of trail stack           */
CPtr _hbreg;		/* heap back track point        */
CPtr _sreg;		/* current build or unify field */
byte *_cpreg;		/* return point register        */
byte *_pcreg;		/* program counter              */
CPtr _ebreg;		/* breg into environment stack	*/

CPtr _efreg;
CPtr _bfreg;
CPtr _hfreg;
CPtr *_trfreg;
CPtr _pdlreg;
CPtr _openreg;
xsbBool _neg_delay;
int _xwammode;
int _level_num;
CPtr _root_address;

CPtr _ptcpreg;
CPtr _delayreg;
CPtr _interrupt_reg;
Cell _interrupt_counter;

int _asynint_code;
int _asynint_val;

/* Compiled trie stuff - some of this may be able to be changed to
  local variables */

Cell *_reg_array;
CPtr _reg_arrayptr;
int  _reg_array_size;

#define MAX_TRIE_REGS 500
CPtr _var_regs[MAX_TRIE_REGS];
int  _num_vars_in_var_regs ;

int  _num_heap_term_vars;
CPtr *_var_addr;
int  _var_addr_arraysz;

Cell _VarEnumerator[NUM_TRIEVARS];
Cell _TrieVarBindings[NUM_TRIEVARS];

CPtr _VarEnumerator_trail[NUM_TRIEVARS];
CPtr *_VarEnumerator_trail_top;

int _addr_stack_pointer;
CPtr *_Addr_Stack;
int _addr_stack_size;
                                                                                
int  _term_stackptr;
Cell *_term_stack;
long _term_stacksize;

int _global_num_vars;


BTNptr  _NodePtr, 
	_Last_Nod_Sav;

int     _delay_it;

struct VariantContinuation *_variant_cont;
struct tstCCPStack_t *_tstCCPStack;
struct tstCPStack_t *_tstCPStack;
CPtr *_trail_base;
CPtr *_orig_trreg;            
CPtr _orig_hreg;
CPtr _orig_hbreg;
CPtr _orig_ebreg;

/* variables for findall buffers */
findall_solution_list *_findall_solutions;  /*= NULL;*/
findall_solution_list *_current_findall;
int 	_nextfree ; /* nextfree index in findall array */
CPtr 	_gl_bot, _gl_top ;
f_tr_chunk *_cur_tr_chunk ;
CPtr 	*_cur_tr_top ;
CPtr 	*_cur_tr_limit ;

VarString **_LSBuff; /* 30 buffers for making longstring in ctop_longstring */

/* Global thread-specific charstring buffers for local use within builtins */
VarString *_last_answer;  /* for c-calling-xsb interface */
VarString *_tsgLBuff1;
VarString *_tsgLBuff2;
VarString *_tsgSBuff1;
VarString *_tsgSBuff2;
/* read_canonical stacks */
int _opstk_size;
int _funstk_size;
struct funstktype *_funstk;
struct opstktype *_opstk;
struct vartype *_rc_vars;

/* Global variables for tokenizing */
struct token_t *_token;
int     _lastc; // = ' ';    /* previous character */
char*   _strbuff; // = NULL;  /* Pointer to token buffer; Will be allocated on first call to GetToken */
int     _strbuff_len; // = InitStrLen;  /* first allocation size, doubled on subsequent overflows */
double  _double_v;
long	_rad_int;

struct sort_par_spec _par_spec;		/* spec for par_sort */

jmp_buf _assertcmp_env;
jmp_buf _xsb_abort_fallback_environment;

ClRef _retracted_buffer[MAX_RETRACTED_CLAUSES+1];
ClRef *_OldestCl;
ClRef *_NewestCl;

struct random_seeds_t *_random_seeds;	/* struct containing seeds for random num gen */

struct asrtBuff_t *_asrtBuff;	/* assert code buffer */
int    _i_have_dyn_mutex;	/* This thread has dynamic mutex, for asserted code read */

/* Flag used in the locking of called tries */
int	trie_locked;

int	_AnsVarCtr;
CPtr	_ans_var_pos_reg;

struct Structure_Manager *_smBTN;
struct Structure_Manager *_smBTHT;

DynamicStack  _tstTermStack;
DynamicStack  _tstTermStackLog;
DynamicStack  _tstSymbolStack;
DynamicStack  _tstTrail;

/* Pointers to cursor information used by odbc_xsb.c */
/* context-local cursor table*/

struct Cursor *_FCursor;  /* root of curser chain*/
struct Cursor *_LCursor;  /* tail of curser chain*/
struct NumberofCursors *_FCurNum;

/* Private flags */

Cell _pflags[MAX_PRIVATE_FLAGS];

/* Thread Id (for fast access) */

int tid ;

/* stuff for deadlock detection in completion */
#ifdef SHARED_COMPL_TABLES
struct th_context * 	waiting_for_thread;
struct subgoal_frame *	waiting_for_subgoal;
int deadlock_brk_leader ;
int reset_thread ;
#endif

} ;

typedef struct th_context th_context ;

#define call_intercept		(th->_call_intercept)

#define pdl			(th->_pdl)
#define glstack			(th->_glstack)
#define tcpstack		(th->_tcpstack)
#define complstack		(th->_complstack)

#define reg			(th->_reg)

#define ereg			(th->_ereg)
#define breg			(th->_breg)
#define hreg			(th->_hreg)
#define trreg			(th->_trreg)
#define hbreg			(th->_hbreg)
#define sreg			(th->_sreg)
#define cpreg			(th->_cpreg)
#define pcreg			(th->_pcreg)
#define ebreg			(th->_ebreg)

#define efreg			(th->_efreg)
#define bfreg			(th->_bfreg)
#define hfreg			(th->_hfreg)
#define trfreg			(th->_trfreg)

#define pdlreg			(th->_pdlreg)
#define openreg			(th->_openreg)
#define neg_delay		(th->_neg_delay)
#define xwammode		(th->_xwammode)
#define level_num		(th->_level_num)
#define root_address		(th->_root_address)

#define ptcpreg			(th->_ptcpreg)
#define delayreg		(th->_delayreg)
#define interrupt_reg		(th->_interrupt_reg)
#define interrupt_counter	(th->_interrupt_counter)

#define asynint_code		(th->_asynint_code)
#define asynint_val		(th->_asynint_val)

#define reg_array		(th->_reg_array)
#define reg_arrayptr		(th->_reg_arrayptr)
#define reg_array_size		(th->_reg_array_size)

#define var_regs		(th->_var_regs)
#define num_vars_in_var_regs	(th->_num_vars_in_var_regs)

#define num_heap_term_vars	(th->_num_heap_term_vars)
#define var_addr		(th->_var_addr)
#define var_addr_arraysz	(th->_var_addr_arraysz)

#define VarEnumerator		(th->_VarEnumerator)
#define TrieVarBindings		(th->_TrieVarBindings)

#define VarEnumerator_trail	(th->_VarEnumerator_trail)
#define VarEnumerator_trail_top	(th->_VarEnumerator_trail_top)

#define addr_stack_pointer	(th->_addr_stack_pointer)
#define Addr_Stack		(th->_Addr_Stack)
#define addr_stack_size		(th->_addr_stack_size)
                                                                                
#define term_stackptr		(th->_term_stackptr)
#define term_stack		(th->_term_stack)
#define term_stacksize		(th->_term_stacksize)

#define global_num_vars		(th->_global_num_vars)

#define NodePtr			(th->_NodePtr)
#define Last_Nod_Sav		(th->_Last_Nod_Sav)

#define delay_it		(th->_delay_it)

#define variant_cont		(*(th->_variant_cont))
#define a_variant_cont		(th->_variant_cont)
#define tstCCPStack		(*(th->_tstCCPStack))
#define a_tstCCPStack		(th->_tstCCPStack)
#define tstCPStack		(*(th->_tstCPStack))
#define a_tstCPStack		(th->_tstCPStack)

#define trail_base		(th->_trail_base)
#define orig_trreg		(th->_orig_trreg)
#define orig_hreg		(th->_orig_hreg)
#define orig_hbreg		(th->_orig_hbreg)
#define orig_ebreg		(th->_orig_ebreg)

#define findall_solutions	(th->_findall_solutions)
#define current_findall		(th->_current_findall)
#define nextfree		(th->_nextfree)
#define gl_bot			(th->_gl_bot)
#define gl_top			(th->_gl_top)
#define cur_tr_chunk		(th->_cur_tr_chunk)
#define cur_tr_top		(th->_cur_tr_top)
#define cur_tr_limit		(th->_cur_tr_limit)

#define LSBuff			(th->_LSBuff)

#define last_answer           (th->_last_answer)
#define tsgLBuff1		(th->_tsgLBuff1)
#define tsgLBuff2		(th->_tsgLBuff2)
#define tsgSBuff1		(th->_tsgSBuff1)
#define tsgSBuff2		(th->_tsgSBuff2)

#define opstk_size		(th->_opstk_size)
#define funstk_size		(th->_funstk_size)
#define funstk			(th->_funstk)
#define opstk			(th->_opstk)
#define rc_vars			(th->_rc_vars)

#define token			(th->_token)
#define lastc			(th->_lastc)
#define strbuff			(th->_strbuff)
#define strbuff_len		(th->_strbuff_len)
#define double_v		(th->_double_v)
#define rad_int			(th->_rad_int)

#define par_spec		(th->_par_spec)

#define assertcmp_env		(th->_assertcmp_env)
#define xsb_abort_fallback_environment (th->_xsb_abort_fallback_environment)

#define retracted_buffer	(th->_retracted_buffer)
#define OldestCl		(th->_OldestCl)
#define NewestCl		(th->_NewestCl)

#define random_seeds		(th->_random_seeds)

#define asrtBuff              (th->_asrtBuff)
#define i_have_dyn_mutex      (th->_i_have_dyn_mutex)

#define AnsVarCtr		(th->_AnsVarCtr)
#define ans_var_pos_reg		(th->_ans_var_pos_reg)

#define smBTN			(th->_smBTN)
#define smBTHT			(th->_smBTHT)

#define  tstTermStack		(th->_tstTermStack)
#define  tstTermStackLog	(th->_tstTermStackLog)
#define  tstSymbolStack		(th->_tstSymbolStack)
#define  tstTrail		(th->_tstTrail)

#define FCursor (th->_FCursor)
#define LCursor (th->_LCursor)
#define FCurNum (th->_FCurNum)

#define  pflags			(th->_pflags)

#define CTXT			th
#define CTXTc			th ,

#define CTXTdecl		th_context *th
#define CTXTdeclc		th_context *th ,

#define CTXTdecltype		th_context *
#define CTXTdecltypec		th_context *,

#else

#define CTXT
#define CTXTc

#define CTXTdecl
#define CTXTdeclc

#define CTXTdecltype
#define CTXTdecltypec

#endif /* MULTI_THREAD */

#endif /* __CONTEXT_H__ */
