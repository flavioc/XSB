/* File:      builtin.c
** Author(s): Xu, Warren, Sagonas, Swift, Freire, Johnson
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


/* for XWAM, term_set_args, buff_assign_word not implemented; */


#include "configs/config.h"
#include "debugs/debug.h"

/* Private debugs */
#include "debugs/debug_delay.h"


#ifdef WIN_NT
#include <windows.h>
#include <tchar.h>
#endif

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
/* special.h must be included after sys/stat.h */
#include "configs/special.h"

#ifdef WIN_NT
#include <stdarg.h>
#include <winsock.h>
#include <wsipx.h>
#else
#include <unistd.h> 
#endif /* WIN_NT */

#include "auxlry.h"
#include "cell.h"
#include "xsberror.h"
#include "psc.h"
#include "hash.h"
#include "tries.h"
#include "choice.h"
#include "deref.h"
#include "memory.h"
#include "heap.h"
#include "register.h"
#include "flags.h"
#include "loader.h"
#include "load_seg.h"
#include "binding.h"
#include "xmacro.h"
#include "token.h"
#include "inst.h"
#include "builtin.h"
#include "subinst.h"
#include "sig.h"
#include "subp.h"
#include "tr_utils.h"
#include "trassert.h"
#include "dynload.h"
#include "cinterf.h"
#ifdef CHAT
#include "chat.h"
#endif
#include "residual.h"

#ifdef ORACLE
#include "oracle.h"
#endif

#ifdef XSB_ODBC
#include "xsb_odbc.h"
#endif

#include "io_builtins.h"

/*======================================================================*/

/* In WIN_NT, this gets redefined into _fdopen by configs/special.h */
extern FILE *fdopen(int fildes, const char *type);

extern tab_inf_ptr get_tip(Psc);
extern tab_inf_ptr first_tip;
extern tab_inf_ptr last_tip;

extern int  sys_syscall(int);
extern bool sys_system(int);
extern bool formatted_io(void), read_canonical(void);
extern bool file_stat(void);
extern bool private_builtin(void);

extern bool assert_code_to_buff(void), assert_buff_to_clref(void),
  gen_retract_all(void), compiled_to_dynamic(void), db_retract0(void),
  db_get_clause(void), db_build_prref(void), db_remove_prref(void),
  db_reclaim0(void);

extern char *dirname_canonic(char *);
extern char *expand_filename(char *filename);
extern char *tilde_expand_filename(char *filename);
extern bool is_absolute_filename(char *filename);
extern void parse_filename(char *filenam, char **dir, char **base, char **ext);

extern int  findall_init(void), findall_add(void), findall_get_solutions(void);
extern int  copy_term(void);

extern void force_answer_true(NODEptr);
extern void force_answer_false(NODEptr);

#if (defined(DEBUG) && defined(DEBUG_DELAY))
extern void print_delay_list(FILE *, CPtr);
extern void print_subgoal(FILE *, SGFrame);
#endif


/* ------- definitions of procedures used in "builtin_call" -----------	*/

static int  fast_ground(CPtr);
static void abolish_table_info(void);
static void write_quotedname(FILE *, char *);

#ifdef DEBUG
extern void printterm(Cell, byte, int);
#endif

#ifdef PROFILE
static void write_out_profile(void);
#endif

/* ------- variables also used in other parts of the system -----------	*/

Cell flags[64];			  /* System flags + user flags */
extern char *install_dir;    	  /* from self_orientation.c */
extern char *user_home;    	  /* from self_orientation.c */

/* ------- working variables for the procedure "builtin_call" ---------	*/

static FILE* fptr;			/* working variable */
static Float float_temp;		/* working variable */


/* ------- utility routines -------------------------------------------	*/

/*
 *  Returns the still-tagged value (a Cell) at the end of the deref chain
 *  leading from `regnum'.
 */
Cell ptoc_tag(int regnum)
{
  /* reg is global array in register.h */
  register Cell addr = cell(reg+regnum);

  deref(addr);
  return addr;
}


DllExport prolog_int call_conv ptoc_int(int regnum)
{
  /* reg is global array in register.h */
  register Cell addr = cell(reg+regnum);

  /* deref and then check the type */
  deref(addr);
  switch (cell_tag(addr)) {
  case FREE:
  case REF1: 
  case CS:
  case LIST:
  case FLOAT: xsb_abort("PTOC_INT: Integer argument expected");
  case STRING: return (prolog_int)string_val(addr);	/* dsw */
  case INT: return int_val(addr);
  default: xsb_abort("PTOC_INT: Argument of unknown type");
  }
  return FALSE;
}

DllExport prolog_float call_conv ptoc_float(int regnum)
{
  /* reg is global array in register.h */
  register Cell addr = cell(reg+regnum);

  /* deref and then check the type */
  deref( addr );
  switch (cell_tag(addr)) {
  case FREE:
  case REF1: 
  case CS:  
  case LIST:
  case INT:
  case STRING:
    xsb_abort("PTOC_FLOAT: Float argument expected");
  case FLOAT: return (prolog_float)float_val(addr);
  default:
    xsb_abort("PTOC_FLOAT: Argument of unknown type");
  }
  return 0.0;
}

DllExport char* call_conv ptoc_string(int regnum)
{
  /* reg is global array in register.h */
  register Cell addr = cell(reg+regnum);
  
  /* deref and then check the type */
  deref(addr);
  switch (cell_tag(addr)) {
  case FREE:
  case REF1: 
  case CS:  
  case LIST:
  case FLOAT:
    xsb_abort("PTOC_STRING: String (atom) argument expected");
  case INT: return (char *)int_val(addr);
  case STRING: return string_val(addr); 
  default:
    xsb_abort("PTOC_STRING: Argument of unknown type");
  }
  return "";
}


/*
 *  For decoding pointers to objects, currently PSC and PSC-PAIR records.
 */
#define ptoc_addr(regnum)	ptoc_int(regnum)


/*
 *  Deref's the variable of register `regnum', trails the binding,
 *  creates an INT Cell containing `value', and binds the variable to it.
 */
DllExport void call_conv ctop_int(int regnum, prolog_int value)
{
  register Cell addr = cell(reg+regnum);
  
  deref(addr);
  if (isref(addr)) {
    bind_int(vptr(addr), value);
  }
  else
    xsb_abort("CTOP_INT: Wrong type of argument %lx (Reg = %d)", addr, regnum);
}


DllExport void call_conv ctop_float(int regnum, prolog_float value) /* from float value form an int node */
{
  /* reg is global array in register.h */
  register Cell addr = cell(reg+regnum);

  deref(addr);
  if (isref(addr)) {
    bind_float(vptr(addr), value);
  }
  else xsb_abort("CTOP_FLOAT: Wrong type of argument: %lux", addr);
}

/* take a C string, form a string node */
DllExport void call_conv ctop_string(int regnum, char *value)
{
  /* reg is global array in register.h */
  register Cell addr = cell(reg+regnum);

  deref(addr);
  if (isref(addr)) {
    bind_string(vptr(addr), value);
  }
  else xsb_abort("CTOP_STRING: Wrong type of argument: %lux", addr);
}

void ctop_ref(int regnum, CPtr value)  /* from address form a reference node */
{
  register Cell addr = cell(reg+regnum);

  deref(addr);
  if (isref(addr)) {
    bind_ref(vptr(addr), value);
  }
  else xsb_abort("CTOP_REF: Wrong type of argument: %lux", addr);
}

void ctop_constr(int regnum, Pair psc_pair)
{				/* from psc_pair ptr form an constr node */
  register Cell addr = cell(reg+regnum);

  deref(addr);
  if (isref(addr)) {
    bind_cs(vptr(addr), psc_pair);
  }
  else xsb_abort("CTOP_CONSTR: Wrong type of argument: %lux", addr);
}

/*
 *  Bind the variable pointed to by the "regnum"th argument register to the
 *  term at address "term".  Make an entry in the trail for this binding.
 */
void ctop_tag(int regnum, Cell term)
{
  register Cell addr = cell(reg+regnum);

  deref(addr);
  if (isref(addr)) {
    bind_copy(vptr(addr), term);
  }
  else xsb_abort("CTOP_TAG: Wrong type of argument: %lux", addr);
}


/*
 *  For encoding pointers to objects, currently PSC and PSC-PAIR records.
 */
#define ctop_addr(regnum, val)    ctop_int(regnum, (prolog_int)val)

/* --------------------------------------------------------------------	*/

Cell  val_to_hash(Cell term)
{
  Cell value;

  switch(cell_tag(term)) {
    case INT:
    case FLOAT:  /* Yes, use int_val to avoid conversion problem */
      value = (Cell)int_val(term);
      break;
    case LIST:
      value = (Cell)(list_str);
      break;
    case CS:
      value = (Cell)get_str_psc(term);
      break;
    case STRING: /* The following test is a necessary nuisance caused  */
      /* by the strange (dynamic) compilation of []/0 in an */
      /* index position which should be fixed one fine day! */
      value = (Cell)(isnil(term) ? 0 : string_val(term));
      break;
    default: xsb_exit("Indexing on illegal argument");
      value = 0;
      break;
  }
  return value;
}

/* --------------------------------------------------------------------	*/

static int is_proper_list(Cell term)	/* for standard preds */
{
	register Cell addr;

	if (islist(term)) {
	     addr = cell(clref_val(term)+1);
	     deref(addr);
	     return is_proper_list(addr);
	}
	else return isnil(term);
}

/* --------------------------------------------------------------------	*/

Psc term_psc(Cell term)
{
  int value;
  Psc psc;
  Pair sym;

  if (isconstr(term))
    return get_str_psc(term);
  else {
    if (isstring(term)) {
      psc = (Psc)flags[CURRENT_MODULE];
      sym = insert(string_val(term), 0, psc, &value);
      return pair_psc(sym);
    }
    else return NULL;
  }
}

/* -------------------------------------------------------------------- */

void xsb_fprint_variable(FILE *fptr, CPtr var)
{
  if (var >= (CPtr)glstack.low && var <= top_of_heap)
    fprintf(fptr, "_h%ld", ((Cell)var-(Cell)glstack.low+1)/sizeof(CPtr));
  else {
    if (var >= top_of_localstk && var <= (CPtr)glstack.high)
      fprintf(fptr, "_l%ld", ((Cell)glstack.high-(Cell)var+1)/sizeof(CPtr));
    else fprintf(fptr, "_%p", var);   /* Should never happen */
  }
}

void xsb_sprint_variable(char *sptr, CPtr var)
{
  if (var >= (CPtr)glstack.low && var <= top_of_heap)
    sprintf(sptr, "_h%ld", ((Cell)var-(Cell)glstack.low+1)/sizeof(CPtr));
  else {
    if (var >= top_of_localstk && var <= (CPtr)glstack.high)
      sprintf(sptr, "_l%ld", ((Cell)glstack.high-(Cell)var+1)/sizeof(CPtr));
    else sprintf(sptr, "_%p", var);   /* Should never happen */
  }
}

/* --------------------------------------------------------------------	*/

STRFILE *iostrs[MAXIOSTRS] = {NULL,NULL,NULL,NULL,NULL};

/* --------------------------------------------------------------------	*/

Cell builtin_table[BUILTIN_TBL_SZ][2];

#define set_builtin_table(inst, instr) builtin_table[inst][0] = (Cell)(instr);

void init_builtin_table(void)
{
  int i;

  for (i = 0; i < BUILTIN_TBL_SZ; i++) builtin_table[i][1] = 0;
  
  set_builtin_table(PSC_NAME, "psc_name");
  set_builtin_table(PSC_ARITY, "psc_arity");
  set_builtin_table(PSC_TYPE, "psc_type");
  set_builtin_table(PSC_PROP, "psc_prop");
  set_builtin_table(PSC_SET_TYPE, "psc_set_type");
  set_builtin_table(PSC_SET_PROP, "psc_set_prop");
  set_builtin_table(FILE_FUNCTION, "file_function");
  set_builtin_table(TERM_PSC, "term_psc");
  set_builtin_table(TERM_TYPE, "term_type");
  set_builtin_table(TERM_COMPARE, "term_compare");
  set_builtin_table(TERM_NEW, "term_new");
  set_builtin_table(TERM_ARG, "term_arg");
  set_builtin_table(TERM_SET_ARG, "term_set_arg");
  set_builtin_table(STAT_FLAG, "stat_flag");
  set_builtin_table(STAT_SET_FLAG, "stat_set_flag");
  set_builtin_table(BUFF_ALLOC, "buff_alloc");
  set_builtin_table(BUFF_WORD, "buff_word");
  set_builtin_table(BUFF_SET_WORD, "buff_set_word");
  set_builtin_table(BUFF_BYTE, "buff_byte");
  set_builtin_table(BUFF_SET_BYTE, "buff_set_byte");
  set_builtin_table(CODE_CALL, "code_call");
  set_builtin_table(STR_LEN, "str_len");
  set_builtin_table(STR_CAT, "str_cat");
  set_builtin_table(STR_CMP, "str_cmp");
  set_builtin_table(STR_HSH, "str_hsh");
  set_builtin_table(STR_SUB, "str_sub");
  set_builtin_table(DIRNAME_CANONIC, "dirname_canonic");
  set_builtin_table(CALL0, "call0");
  set_builtin_table(STAT_STA, "stat_sta");
  set_builtin_table(STAT_CPUTIME, "stat_cputime");
  set_builtin_table(CODE_LOAD, "code_load");
  set_builtin_table(BUFF_SET_VAR, "buff_set_var");
  set_builtin_table(BUFF_DEALLOC, "buff_dealloc");
  set_builtin_table(BUFF_CELL, "buff_cell");
  set_builtin_table(BUFF_SET_CELL, "buff_set_cell");
  set_builtin_table(COPY_TERM,"copy_term");
  set_builtin_table(PSC_INSERT, "psc_insert");
  set_builtin_table(PSC_IMPORT, "psc_import");
  set_builtin_table(PSC_INSERTMOD, "psc_insertmod");
  set_builtin_table(LOAD_SEG, "load_seg");
  set_builtin_table(FILE_GETTOKEN, "file_gettoken");
  set_builtin_table(FILE_PUTTOKEN, "file_puttoken");
  set_builtin_table(TERM_HASH, "term_hash");
  set_builtin_table(UNLOAD_SEG, "unload_seg");
  set_builtin_table(LOAD_OBJ, "load_obj");
  set_builtin_table(GETENV, "getenv");
  set_builtin_table(SYS_SYSCALL, "sys_syscall");
  set_builtin_table(SYS_SYSTEM, "sys_system");
  set_builtin_table(SYS_GETHOST, "sys_gethost");
  set_builtin_table(SYS_ERRNO, "sys_errno");
  set_builtin_table(FILE_STAT, "file_stat");
  set_builtin_table(FILE_WRITEQUOTED, "file_writequoted");
  set_builtin_table(FAST_GROUND, "fast_ground");

  set_builtin_table(INTERN_STRING, "intern_string");
  set_builtin_table(EXPAND_FILENAME, "expand_filename");
  set_builtin_table(TILDE_EXPAND_FILENAME, "tilde_expand_filename");
  set_builtin_table(IS_ABSOLUTE_FILENAME, "is_absolute_filename");
  set_builtin_table(PARSE_FILENAME, "parse_filename");

  set_builtin_table(PSC_ENV, "psc_env");
  set_builtin_table(PSC_SPY, "psc_spy");
  set_builtin_table(PSC_TABLED, "psc_tabled");

  set_builtin_table(IS_INCOMPLETE, "is_incomplete");
  set_builtin_table(GET_OSP_BREG, "get_osp_breg");
  set_builtin_table(CUT_IF_LEADER, "cut_if_leader");
  set_builtin_table(GET_PTCP, "get_ptcp");
  set_builtin_table(GET_SUBGOAL_PTR, "get_subgoal_ptr");
  set_builtin_table(DEREFERENCE_THE_BUCKET, "dereference_the_bucket");
  set_builtin_table(PAIR_PSC, "pair_psc");
  set_builtin_table(PAIR_NEXT, "pair_next");
  set_builtin_table(NEXT_BUCKET, "next_bucket");
  set_builtin_table(SLG_NOT, "slg_not");
  set_builtin_table(IS_XWAMMODE, "is_xwammode");
  set_builtin_table(CLOSE_OPEN_TABLES, "close_open_tables");

  set_builtin_table(ABOLISH_TABLE_INFO, "abolish_table_info");
  set_builtin_table(ZERO_OUT_PROFILE, "zero_out_profile");
  set_builtin_table(WRITE_OUT_PROFILE, "write_out_profile");
  set_builtin_table(ASSERT_CODE_TO_BUFF, "assert_code_to_buff");
  set_builtin_table(ASSERT_BUFF_TO_CLREF, "assert_buff_to_clref");
  set_builtin_table(FORMATTED_IO, "formatted_io");
  set_builtin_table(SLASH_BUILTIN, "slash");
  set_builtin_table(FILE_READ_CANONICAL, "file_read_canonical");
  set_builtin_table(GEN_RETRACT_ALL, "gen_retract_all");
  set_builtin_table(COMPILED_TO_DYNAMIC, "compiled_to_dynamic");
  set_builtin_table(DB_RETRACT0, "db_retract0");
  set_builtin_table(DB_GET_CLAUSE, "db_get_clause");
  set_builtin_table(DB_BUILD_PRREF, "db_build_prref");
  set_builtin_table(DB_REMOVE_PRREF, "db_remove_prref");
  set_builtin_table(DB_RECLAIM0, "db_reclaim0");

  set_builtin_table(TABLE_STATUS, "table_status");
  set_builtin_table(GET_DELAY_LISTS, "get_delay_lists");

  set_builtin_table(ABOLISH_TABLE_PREDICATE, "abolish_table_predicate");
  set_builtin_table(TRIE_ASSERT, "trie_assert");
  set_builtin_table(TRIE_RETRACT, "trie_retract");
  set_builtin_table(TRIE_RETRACT_SAFE, "trie_retract_safe");
  set_builtin_table(TRIE_DELETE_TERM, "trie_delete_term");
  set_builtin_table(TRIE_GET_RETURN, "trie_get_return");
  set_builtin_table(TRIE_GET_CALL, "trie_get_call");
  set_builtin_table(GET_LASTNODE_CS_RETSKEL, "get_lastnode_cs_retskel");
  set_builtin_table(CONSTRUCT_RET_FOR_CALL, "construct_ret_for_call");
  set_builtin_table(BREG_RETSKEL,"breg_retskel");

  set_builtin_table(TRIMCORE, "trimcore");

  set_builtin_table(PRIVATE_BUILTIN, "private_builtin");

  set_builtin_table(VAR, "var");
  set_builtin_table(NONVAR, "nonvar");
  set_builtin_table(ATOM, "atom");
  set_builtin_table(INTEGER, "integer");
  set_builtin_table(REAL, "real");
  set_builtin_table(NUMBER, "number");
  set_builtin_table(ATOMIC, "atomic");
  set_builtin_table(COMPOUND, "compound");
  set_builtin_table(CALLABLE, "callable");
  set_builtin_table(IS_LIST, "is_list");

  set_builtin_table(FUNCTOR, "functor");
  set_builtin_table(ARG, "arg");
  set_builtin_table(UNIV, "univ");
  set_builtin_table(HiLog_ARG, "hilog_arg");
  set_builtin_table(HiLog_UNIV, "hilog_univ");
  set_builtin_table(ATOM_CODES, "atom_codes");
  set_builtin_table(ATOM_CHARS, "atom_chars");
  set_builtin_table(NUMBER_CHARS, "number_chars");
  set_builtin_table(NUMBER_CODES, "number_codes");
  set_builtin_table(IS_CHARLIST, "is_charlist");

  set_builtin_table(PUT, "put");
  set_builtin_table(TAB, "tab");
  set_builtin_table(SORT, "sort");
  set_builtin_table(KEYSORT, "keysort");

  set_builtin_table(ORACLE_QUERY, "oracle_query");
  set_builtin_table(ODBC_EXEC_QUERY, "odbc_exec_query");

  set_builtin_table(PRINT_CHAT, "print_chat");
  set_builtin_table(PRINT_LS, "print_ls");
  set_builtin_table(PRINT_TR, "print_tr");
  set_builtin_table(PRINT_HEAP, "print_heap");
  set_builtin_table(PRINT_CP, "print_cp");
  set_builtin_table(PRINT_REGS, "print_regs");
  set_builtin_table(PRINT_ALL_STACKS, "print_all_stacks");
  set_builtin_table(MARK_HEAP, "mark_heap");
  set_builtin_table(GC_HEAP, "gc_heap");
  set_builtin_table(FINDALL_INIT, "$$findall_init");
  set_builtin_table(FINDALL_ADD, "$$findall_add");
  set_builtin_table(FINDALL_GET_SOLS, "$$findall_get_solutions");

#ifdef HAVE_SOCKET
  set_builtin_table(SOCKET_REQUEST, "socket_request");
#endif

  set_builtin_table(JAVA_INTERRUPT, "setupJavaInterrupt");
  set_builtin_table(FORCE_TRUTH_VALUE, "force_truth_value");
}

/*----------------------------------------------------------------------*/

/* inlined definition of file_function */
#include "io_builtins.i"

#ifdef HAVE_SOCKET
#include "xsbsocket.i"
#endif /* HAVE_SOCKET */

/* inlined functions for prolog standard builtins */
#include "std_pred.i"

/* --- built in predicates --------------------------------------------	*/

int builtin_call(byte number)
{
  CPtr var;
  char *addr, *tmpstr;
  int  value, i, disp, arity, tmpval;
  Cell term;
  
  DL dl;				/* for residual program */
  DE de;				/* for residual program */
  NODEptr as_leaf;			/* for residual program */
  Cell delay_lists;			/* for residual program */
  CPtr dls_head, dls_tail = NULL;	/* for residual program */
  tab_inf_ptr tip;
  Psc  psc;
  Pair sym;
  CPtr subgoal_ptr, t_ptcp;
  register CPtr xtemp1, xtemp2;
#ifdef FOREIGN
  static int (*proc_ptr)(void);		/* working variable */
#endif
  
  switch (number) {
  case PSC_NAME:		/* R1: +PSC; R2: -String */
    psc = (Psc)ptoc_addr(1);
    ctop_string(2, get_name(psc));
    break;
  case PSC_ARITY:		/* R1: +PSC; R2: -int */
    psc = (Psc)ptoc_addr(1);
    ctop_int(2, (Integer)get_arity(psc));
    break;
  case PSC_TYPE:		/* R1: +PSC; R2: -int */
				/* type: see psc.h, `entry_type' field defs */
    psc = (Psc)ptoc_addr(1);
    ctop_int(2, (Integer)get_type(psc));
    break;
  case PSC_SET_TYPE:	/* R1: +PSC; R2: +type (int): see psc.h */
    psc = (Psc)ptoc_addr(1);
    set_type(psc, ptoc_int(2));
    break;
  case PSC_PROP:		/* R1: +PSC; R2: -term */
				/* prop: as a buffer pointer */
    psc = (Psc)ptoc_addr(1);
    if (get_type(psc)==T_ALIA) ctop_tag(2, (Cell)get_ep(psc));
    else ctop_int(2, (Integer)get_ep(psc));
    break;
  case PSC_SET_PROP:	        /* R1: +PSC; R2: +int */
    psc = (Psc)ptoc_addr(1);
    if (get_type(psc)==T_ALIA) set_ep(psc, (pb)ptoc_tag(2));
    else set_ep(psc, (pb)ptoc_int(2));
    break;
  case FILE_FUNCTION:  /* file_open/close/put/get/truncate/seek/pos */
    return file_function();

  case TERM_PSC:		/* R1: +term; R2: -PSC */
    /* Assumes that `term' is a CS-tagged Cell. */
    ctop_addr(2, get_str_psc(ptoc_tag(1)));
    break;
  case TERM_TYPE:		/* R1: +term; R2: tag (-int)		*/
				/* <0 - var, 1 - cs, 2 - int, 3 - list>	*/
    term = ptoc_tag(1);
    if (!isnonvar(term)) ctop_int(2, 0);
    else ctop_int(2, cell_tag(term));
    break;
  case TERM_COMPARE:	/* R1, R2: +term; R3: res (-int) */
    ctop_int(3, compare(ptoc_tag(1), ptoc_tag(2)));
    break;
  case TERM_NEW:		/* R1: +PSC, R2: -term */
    psc = (Psc)ptoc_addr(1);
    sreg = hreg;
    hreg += get_arity(psc) + 1;
    ctop_constr(2, (Pair)sreg);
    new_heap_functor(sreg, psc);
    for (disp=0; disp < (int)get_arity(psc); sreg++,disp++) {
      bld_free(sreg);
    }
    break;
  case TERM_ARG:	/* R1: +term; R2: index (+int); R3: arg (-term) */
    disp = ptoc_int(2);
    term = ptoc_tag(1);
    ctop_tag(3, cell(clref_val(term)+disp));
    break;
  case TERM_SET_ARG:	/* R1: +term; R2: index (+int) */
			/* R3: newarg (+term); R4: +perm(not used) */
    /* used in file_read.P, array.P, array1.P */
    disp = ptoc_int(2);
    term = ptoc_tag(1);
    if (!ptoc_int(4)) { pushtrail(clref_val(term)+disp,cell(reg+3));}
    bld_copy(clref_val(term)+disp, cell(reg+3));
    break;
  case STAT_FLAG:	/* R1: flagname(+int); R2: value(-int) */
    ctop_int(2, flags[ptoc_int(1)]);
    break;
  case STAT_SET_FLAG:	/* R1: flagname(+int); R2: value(+int); */
    flags[ptoc_int(1)] = ptoc_int(2);
    call_intercept = flags[DEBUG_ON]|flags[TRACE_STA]|flags[HITRACE]
      |flags[CLAUSE_INT];
    break;
  case BUFF_ALLOC:	/* R1: size (+integer); R2: -buffer; */
	           /* the length of the buffer is also stored at position 0 */
    value = ((ptoc_int(1)+7)>>3)<<3;
    value *= ZOOM_FACTOR ;
    addr = (char *)mem_alloc(value);
    value /= ZOOM_FACTOR ;
    *(Integer *)addr = value;	/* store buffer size at buf[0] */
    ctop_int(2, (Integer)addr);	/* use "integer" type now! */
    break;
  case BUFF_DEALLOC:	/* R1: +buffer; R2: +oldsize; R3: +newsize; */
    addr = ptoc_string(1);
    disp = ((ptoc_int(2)+7)>>3)<<3;
    disp *= ZOOM_FACTOR ;
    value = ((ptoc_int(3)+7)>>3)<<3;	/* alignment */
    value *= ZOOM_FACTOR ;
    if (value > disp) {
      xsb_warn("BUFF_DEALLOC: New Buffer Size (%d) Cannot exceed the old one (%d)!!",
	       value, disp);
      break;
    }
    mem_dealloc((byte *)(addr+value), disp-value);
    break;
  case BUFF_WORD:       /* R1: +buffer; r2: displacement(+integer); */
			/* R3: value (-integer) */
    addr = ptoc_string(1);
    disp = ptoc_int(2);
    disp *= ZOOM_FACTOR ;
    ctop_int(3, *(Integer *)(addr+disp));
    break;
  case BUFF_SET_WORD:	/* R1: +buffer; r2: displacement(+integer); */
			/* R3: value (+integer) */
    addr = ptoc_string(1);
    disp = ptoc_int(2);
    disp *= ZOOM_FACTOR ;
    *(CPtr)(addr+disp) = ptoc_int(3);
    break;
  case BUFF_BYTE:	/* R1: +buffer; r2: displacement(+integer); */
			/* R3: value (-integer) */
    addr = ptoc_string(1);
    disp = ptoc_int(2);
    ctop_int(3, (Integer)(*(byte *)(addr+disp)));
    break;
  case BUFF_SET_BYTE:	/* R1: +buffer; R2: displacement(+integer); */
			/* R3: value (+integer) */
    addr = ptoc_string(1);
    disp = ptoc_int(2);
    *(pb)(addr+disp) = ptoc_int(3);
    break;
  case BUFF_CELL:	/* R1: +buffer; R2: displacement(+integer); */
                        /* R3: -Cell at that location */
    addr = ptoc_string(1);
    disp = ptoc_int(2);
    disp *= ZOOM_FACTOR ;
    ctop_tag(3, (Cell)(addr+disp));
    break;
  case BUFF_SET_CELL:	/* R1: +buffer; R2: displacement(+integer);*/
			/* R3: type (+integer); R4: +term */
    /* When disp<0, set the type of the buff itself */
    /* The last function is not implemented */
    addr = ptoc_string(1);
    disp = ptoc_int(2);
    disp *= ZOOM_FACTOR ;
    value = ptoc_int(3);
    switch (value) {
    case REF: case REF1:
      bld_ref(vptr(addr+disp), (CPtr)ptoc_int(4)); break;
    case INT:
      tmpval = ptoc_int(4);
      bld_int(vptr(addr+disp), tmpval); break;
    case FLOAT:
      bld_float(vptr(addr+disp), ptoc_float(4)); break;
    case CS: 
      bld_cs(vptr(addr+disp), (Pair)ptoc_int(4)); break;
    case STRING:
      bld_string(vptr(addr+disp), (char *)ptoc_int(4)); break;
    case LIST:
      bld_list(vptr(addr+disp), (CPtr)ptoc_int(4)); break;
    default:
      xsb_warn("BUFF_SET_CELL: Type %d is not implemented", value);
    }
    break;
  case BUFF_SET_VAR:
    /* This procedure is used to make an external variable pointing to the
       buffer. The linkage inside the buffer will not be trailed so remains
       after backtracking. */
    /* R1: +buffer; R2: +disp; */
    /* R3: +buffer length; R4: External var */
    addr = ptoc_string(1);
    disp = ptoc_int(2);
    disp *= ZOOM_FACTOR;
    term = ptoc_tag(4);
    bld_free(vptr(addr+disp));
    if ((Cell)term < (Cell)addr || 
	(Cell)term > (Cell)addr+ptoc_int(3)) { /* var not in buffer, trail */
      bind_ref(vptr(term), (CPtr)(addr+disp));
    } else {		/* already in buffer */
      bld_ref(vptr(term), (CPtr)(addr+disp));	
    }
    break;
    
  case COPY_TERM: /* R1: +term to copy; R2: -variant */
    copy_term();
    break;
    
  case CALL0:			/* R1: +Term, the call to be made */
    /* Note: this procedure does not save cpreg, hence is more like */
    /* an "execute" instruction, and must be used as the last goal!!!*/
    term = ptoc_tag(1);
    if (isconstr(term)) {
      psc = get_str_psc(term);
      addr = (char *)(clref_val(term));
      for (disp = 1; disp <= (int)get_arity(psc); ++disp) {
	bld_copy(reg+disp, cell((CPtr)(addr)+disp));
      }
    } else if (isstring(term)) {
      sym = insert(string_val(term),0,(Psc)flags[CURRENT_MODULE],&value);
      psc = pair_psc(sym);
    } else {
      if (isnonvar(term))
	err_handle(TYPE, 1, "call", 1, "callable term", term);
      else err(INSTANTIATION, 1, "call", 1);
      pcreg = (pb)&fail_inst;
      return FALSE;
    }
    switch (get_type(psc)) {
    case T_PRED:
    case T_FUNC:
    case T_DYNA:
      pcreg = get_ep(psc);
      break;
    case T_FORN:
#ifdef FOREIGN
      proc_ptr = (PFI) get_ep(psc);
      /* A foreign function must return an int!
	 If it returns non-0 then proceed; 0 - fail */
      if (proc_ptr())
	pcreg = cpreg;      	 /* proceed */
      else
	pcreg = (pb)&fail_inst;	 /* fail    */
#else
      xsb_exit("Foreign call in configuration that does not support it !");
#endif
      break;
    case T_UDEF:
    case T_UFUN:
    default:
      psc = synint_proc(psc, MYSIG_UNDEF, NULL);
      if (psc) pcreg = get_ep(psc);
      break;
    }
    if (call_intercept) intercept(psc);
    break;
    
  case CODE_CALL:		/* R1: +Code (addr), the code address */
				/* R2: +Term, the call to be made */
				/* R3: +Type, code type (same as psc->type)  */
				/* may need to resume interrupt testing here */
    /* Note: this procedure does not save cpreg, hence is more like */
    /* an "execute" instruction, and must be used as the last goal!!!*/
    term = ptoc_tag(2);
    value = ptoc_int(3);  /* Cannot be delayed! R3 may be reused */
    pcreg = (byte *)ptoc_int(1);
    if (isconstr(term)) {
      psc = get_str_psc(term);
      addr = (char *)(clref_val(term));
      for (disp = 1; disp <= (int)get_arity(psc); ++disp) {
	bld_copy(reg+disp, cell((CPtr)(addr)+disp));
      }
      bld_int(reg+get_arity(psc)+1, value);
    } else psc = NULL; 
    if (value == T_FORN) {
#ifdef FOREIGN
      proc_ptr = (PFI) get_ep(psc);
      proc_ptr();
      pcreg = cpreg;		/* always "proceed" */
#else
      xsb_exit("Foreign call in configuration that does not support it !");
#endif
    }
    break;
  case STR_LEN:		/* R1: +String; R2: -Length */
    addr = ptoc_string(1);
    ctop_int(2, strlen(addr));
    break;
  case STR_CAT:		/* R1: +Str1; R2: +Str2: R3: -Str3 */
    addr = (char *)hreg;	/* use global stack as temp space */
    strcpy(addr, ptoc_string(1));
    strcat(addr, ptoc_string(2));
    ctop_string(3, string_find(addr, 1));
    break;
  case STR_CMP:		/* R1: +Str1; R2: +Str2: R3: -Res*/
    ctop_int(3, strcmp(ptoc_string(1), ptoc_string(2)));
    break;
  case STR_HSH:		/* R1: +String; R2: +Arity;
			   R3: +HashSize: R4: -Value */
    value = hash(ptoc_string(1), ptoc_int(2), ptoc_int(3));
    ctop_int(4, value);
    break;
  case STR_SUB:   /* R1: +Substring; R2: +String; R3: -Pos */
    { 
      char *subptr = ptoc_string(1);
      char *stringptr = ptoc_string(2);
      char *matchptr = strstr(stringptr, subptr);
      int substr_pos = matchptr-stringptr+1; /* relative pos of substring */
      if (matchptr == NULL)
	return FALSE;
      else {
	ctop_int(3, substr_pos);
	return TRUE;
      }
    }
  case INTERN_STRING: /* R1: +String1; R2: -String2 ; Intern string */
    ctop_string(2, string_find(ptoc_string(1), 1));
    break;
  case STAT_STA:		/* R1: +Amount */
    value = ptoc_int(1);
    print_statistics(value);
    break;
  case STAT_CPUTIME:	/* R1: -cputime, in miliseconds */	
    value = (int)(cpu_time() * 1000);
    ctop_int(1, value);
    break;
  case CODE_LOAD:		/* R1: +FileName, bytecode file to be loaded */
				/* R2: -int, addr of 1st instruction;	     */
				/*	0 indicates an error                 */
				/* R3 = 1 if exports to be exported, 0 otw   */
    ctop_int(2, (Integer)loader(ptoc_string(1), ptoc_int(3)));
    break;

  case PSC_INSERT:	/* R1: +String, symbol name
			   R2: +Arity
			   R3: -PSC, the new PSC
			   R4: +String, module to be inserted */
    /* inserts or finds a symbol in a given module.	*/
    /* When the given module is 0 (null string),	*/
    /* current module is used.			*/
    addr = ptoc_string(4);
    if (addr)
      psc = pair_psc(insert_module(0, addr));
    else
      psc = (Psc)flags[CURRENT_MODULE];
    sym = insert(ptoc_string(1), ptoc_int(2), psc, &value);
    ctop_addr(3, pair_psc(sym));
    break;

  case PSC_IMPORT:      /* R1: +String, functor name to be imported
			   R2: +Arity
			   R3: +String, Module name where functor lives  */
    /*
     * Creates a PSC record for a predicate and its module (if they
     * don't already exist) and links the predicate into usermod.
     */
    psc = pair_psc(insert_module(0, ptoc_string(3)));
    sym = insert(ptoc_string(1), ptoc_int(2), psc, &value);
    if (value)       /* if predicate is new */
      set_ep(pair_psc(sym), (byte *)(psc));
    env_type_set(pair_psc(sym), T_IMPORTED, T_ORDI, value);
    link_sym(pair_psc(sym), (Psc)flags[CURRENT_MODULE]);
    break;
  case FILE_GETTOKEN:     /* R1: +File, R2: +PrevCh, R3: -Type; */
                                /* R4: -Value, R5: -NextCh */
    tmpval = ptoc_int(1);
    if ((tmpval < 0) && (tmpval >= -MAXIOSTRS))
      token = GetToken(NULL,strfileptr(tmpval), ptoc_int(2));
    else {
      SET_FILEPTR(fptr, tmpval);
      token = GetToken(fptr, NULL, ptoc_int(2));
    }
    if (token->type == TK_ERROR) {
      pcreg = (pb)&fail_inst;
    }
    else {
      ctop_int(3, token->type);
      ctop_int(5, token->nextch);
      switch (token->type)
	{
	case TK_PUNC	   : ctop_int(4, *(token->value)); break;
	case TK_VARFUNC    : ctop_string(4, token->value); break;
	case TK_VAR	   : ctop_string(4, token->value); break;
	case TK_FUNC	   : ctop_string(4, token->value); break;
	case TK_INT	   : ctop_int(4, *(long *)(token->value)); break;
	case TK_ATOM	   : ctop_string(4, token->value); break;
	case TK_EOC	   : ctop_int(4, 0); break;
	case TK_VVAR	   : ctop_string(4, token->value); break;
	case TK_VVARFUNC   : ctop_string(4, token->value); break;
	case TK_REAL	   : 
	  float_temp = *(double *)(token->value);
	  ctop_float(4, float_temp); break;
	case TK_EOF	   : ctop_int(4, 0); break;
	case TK_STR	   : ctop_string(4, token->value); break;
	case TK_LIST	   : ctop_string(4, token->value); break;
	case TK_HPUNC	   : ctop_int(4, *(token->value)); break;
	case TK_INTFUNC    : ctop_int(4, *(long *)(token->value)); break;
	case TK_REALFUNC   : 
	  float_temp =  *(double *)(token->value);
	  ctop_float(4, float_temp); break;
	}   
    }
    break;
  case FILE_PUTTOKEN:	/* R1: +File, R2: +Type, R3: +Value; */
    tmpval = ptoc_int(1);
    SET_FILEPTR(fptr,tmpval);
    switch (ptoc_int(2)) {
    case FREE   : var = (CPtr)ptoc_tag(3);
      xsb_fprint_variable(fptr, var);
      break;
    case INT    : fprintf(fptr, "%ld", (long)ptoc_int(3)); break;
    case STRING : fprintf(fptr, "%s", ptoc_string(3)); break;
    case FLOAT  : fprintf(fptr, "%2.4f", ptoc_float(3)); break;
    case TK_INT_0  : {
      int tmp = (int) ptoc_int(3);
      fix_bb4((byte *)&tmp);
      fwrite(&tmp, 4, 1, fptr); break;
    }
    case TK_FLOAT_0: {
      float ftmp = (float)ptoc_float(3);
      fix_bb4((byte *)&ftmp);
      fwrite(&ftmp, 4, 1, fptr); break;
    }
    case TK_PREOP  : print_op(fptr, ptoc_string(3), 1); break;
    case TK_INOP   : print_op(fptr, ptoc_string(3), 2); break;
    case TK_POSTOP : print_op(fptr, ptoc_string(3), 3); break;
    case TK_QATOM  : print_qatom(fptr, ptoc_string(3)); break;
    case TK_QSTR   : fprintf(fptr, "\"%s\"", ptoc_string(3)); break;
    default : xsb_abort("FILE_PUTTOKEN: Unknown token type");
    }
    break;
  case PSC_INSERTMOD:   /* R1: +String, Module name */
                        /* R2: +Def (4 - is a definition; 0 -not) */
                        /* R3: -PSC of the Module entry */
    sym = insert_module(ptoc_int(2), ptoc_string(1));
    ctop_addr(3, pair_psc(sym));
    break;
  case LOAD_SEG:		/* R1: +segment number */
				/* R2: +text bytes */
				/* R3: +index bytes */
				/* R4: +File name */
				/* R5: -Initial address */	  
    tmpval = ptoc_int(4);
    value = (Integer)load_seg(ptoc_int(1), ptoc_int(2),
			      ptoc_int(3), fileptr(tmpval));
    ctop_int(5, value);
    break;
  case TERM_HASH:		/* R1: +Term	*/
				/* R2: +Size (of hash table) */
				/* R3: -HashVal */
    ctop_int(3, ihash(val_to_hash(ptoc_tag(1)),ptoc_int(2)));
    break;
  case UNLOAD_SEG:	/* R1: -Code buffer */
    unload_seg((pseg)ptoc_int(1));
    break;
  case LOAD_OBJ:		/* R1: +FileName, R2: +Module (Psc) */
	    			/* R3: +ld option, R4: -InitAddr */
#ifdef FOREIGN
    ctop_int(4, (Integer)load_obj(ptoc_string(1),(Psc)ptoc_addr(2),
				  ptoc_string(3)));
#else
    xsb_abort("Loading foreign object files is not implemented for this configuration");
#endif
    break;
  case EXPAND_FILENAME:	       /* R1: +FileName, R2: -ExpandedFileName */
    ctop_string(2, string_find(expand_filename(ptoc_string(1)), 1));
    break;

  case TILDE_EXPAND_FILENAME:  /* R1: +FileN, R2: -TildeExpanded FN */
    ctop_string(2, string_find(tilde_expand_filename(ptoc_string(1)), 1));
    break;
  case IS_ABSOLUTE_FILENAME: /* R1: +FN. Ret 1 if name is absolute, 0 else */
    return is_absolute_filename(ptoc_string(1));

  case PARSE_FILENAME: {    /* R1: +FN, R2: -Dir, R3: -Basename, R4: -Ext */
    char *dir, *basename, *extension;
    parse_filename(ptoc_string(1), &dir, &basename, &extension);
    ctop_string(2, string_find(dir, 1));
    ctop_string(3, string_find(basename, 1));
    ctop_string(4, string_find(extension, 1));
    break;
  }
  case GETENV:  {	/* R1: +environment variable */
			/* R2: -value of that environment variable */
    char *env = getenv(ptoc_string(1));
    if (env == NULL)
      /* otherwise, string_find dumps core */
      return FALSE;
    else
      ctop_string(2, string_find(env,1));
    break;
  }
  case SYS_SYSCALL:	/* R1: +int (call #, see <syscall.h> */
				/* R2: -int, returned value */
	    			/* R3, ...: Arguments */
    ctop_int(2, sys_syscall(ptoc_int(1)));
    break;
  case SYS_SYSTEM:	/* R1: call mubler, R2: +String (of command);
			   R3: -Int (res), or mode: read/write;
			   R4: undefined or Stream used for output/input
			   from/to the shell command. */
    return sys_system(ptoc_int(1));
  case SYS_GETHOST: {
    /* +R1: a string indicating the host name  */
    /* +R2: a buffer (of length 16) for returned structure */
#ifdef HAVE_GETHOSTBYNAME
    static struct hostent *hostptr;
    hostptr = gethostbyname(ptoc_string(1));
    memmove(ptoc_string(2), hostptr->h_addr, hostptr->h_length);
#else
    xsb_abort("SYS_GETHOST: Operation not available for this configuration");
#endif
    break;
  }
  case SYS_ERRNO:			/* R1: -Int (errno) */
    ctop_int(1, errno);
    break;
  case FILE_STAT: /* file_stat(+FileName, +StatFunction, -Result) 
		     Used to obtain file mod time, size, etc., using stat() */
    return file_stat();
  case FILE_WRITEQUOTED:
    tmpval = ptoc_int(1);
    SET_FILEPTR(fptr, tmpval);
    write_quotedname(fptr ,ptoc_string(2));
    break;
  case FAST_GROUND:
    return fast_ground((CPtr)ptoc_tag(1));

  case PSC_ENV:	       /* reg 1: +PSC; reg 2: -int */
    /* env: 0 = exported, 1 = local, 2 = imported */
    psc = (Psc)ptoc_addr(1);
    ctop_int(2, (Integer)get_env(psc));
    break;
  case PSC_SPY:		/* reg 1: +PSC; reg 2: -int */
				/* env: 0 = non-spied else spied */
    psc = (Psc)ptoc_addr(1);
    ctop_int(2, (Integer)get_spy(psc));
    break;
  case PSC_TABLED:	/* reg 1: +PSC; reg 2: -int */
    psc = (Psc)ptoc_addr(1);
    ctop_int(2, (Integer)get_tip(psc));
    break; 
/*----------------------------------------------------------------------*/

#include "bineg.i"

/*----------------------------------------------------------------------*/
    /*
     * Used together with CUT_IF_LEADER in tables.P for negation.
     * Revised to pass offsets into the respective stacks to Prolog
     *  instead of absolute addresses--needed for stack shifting.
     */
  case GET_OSP_BREG:
    ctop_int(1, (Integer)complstack.high - (Integer)openreg);
    ctop_int(2, (Integer)tcpstack.high - (Integer)breg);
    break;

  case GET_SUBGOAL_PTR: 	/* reg1: +term; reg2: -subgoal_ptr */
    term = ptoc_tag(1);
    if ((psc = term_psc(term)) == NULL) {
      err_handle(TYPE, 1, "get_subgoal_ptr", 2, "callable term", term);
      return FALSE;	/* fail */
    }
    tip = get_tip(psc);
    if (tip == NULL) {
      xsb_abort("Predicate %s/%d is not tabled", get_name(psc), get_arity(psc));
    }
    subgoal_ptr = get_subgoal_ptr(term, tip);
    ctop_int(2, (Integer)subgoal_ptr);
    break;

    /*
     * Revised to accept offsets into the respective stacks from Prolog
     *  instead of absolute addresses--needed for stack shifting.
     */
  case CUT_IF_LEADER:
#ifdef EXISTENTIAL_NEGATION
    { CPtr compl_stack_ptr, next_breg, next_openreg;

    term = ptoc_tag(1);
    psc = term_psc(term);
    tip = get_tip(psc);
    subgoal_ptr = get_subgoal_ptr(term, tip);
    compl_stack_ptr = subg_compl_stack_ptr(subgoal_ptr);
    if (prev_compl_frame(compl_stack_ptr) >= COMPLSTACKBOTTOM ||
	is_leader(compl_stack_ptr)) {
      next_openreg = (CPtr) ((Integer)complstack.high - ptoc_int(2));
      next_breg = (CPtr)(tcpstack.high - ptoc_int(3));
      /*	      printf("nb %x no %x\n",next_breg, next_openreg);*/
      breg = next_breg;
      
      /* BUG? the following may be using xtemp1 unitialised -- kostis */
      cut_restore_trail_condition_registers(breg);
      
      openreg = next_openreg;
      remove_open_tables_loop(openreg);
    }
    }
#else
    xsb_abort("Existential negation is not supported");
#endif
    break;
  case DEREFERENCE_THE_BUCKET:
    /*
     * Given an index into the symbol table, return the first Pair
     * in that bucket's chain.
     */
    ctop_int(2, (Integer)(symbol_table.table[ptoc_int(1)]));
    break;
  case PAIR_PSC:
    ctop_addr(2, pair_psc((Pair)ptoc_addr(1)));
    break;
  case PAIR_NEXT:
    ctop_addr(2, pair_next((Pair)ptoc_addr(1)));
    break;
  case NEXT_BUCKET:      /* R1: +Index of Symbol Table Bucket. */
    /* R2: -Next Index (0 if end of Hash Table) */
    value = ptoc_int(1);
    if ( (value >= (symbol_table.size - 1)) || (value < 0) )
      ctop_int(2, 0);
    else 
      ctop_int(2, (value + 1));
    break;

  case IS_XWAMMODE:     /* R1: -int flag for xwammode */  
    if (xwammode) ctop_int(1,1);
    else ctop_int(1,0);
    break;
  case CLOSE_OPEN_TABLES:	/* No registers needed */
    remove_open_tables_reset_freezes();
    break;

  case ABOLISH_TABLE_INFO:
    abolish_table_info();
    break;
#ifdef PROFILE
  case ZERO_OUT_PROFILE:
    for (i = 0 ; i <= BUILTIN_TBL_SZ ; i++) {
      inst_table[i][5] = 0;
      builtin_table[i][1] = 0;
      subinst_table[i][1] = 0;
    }
    break;
  case WRITE_OUT_PROFILE:
    write_out_profile();
    break;
#endif
  case ASSERT_CODE_TO_BUFF:
    assert_code_to_buff();
    break;
  case ASSERT_BUFF_TO_CLREF:
    assert_buff_to_clref();
    break;
  case DIRNAME_CANONIC: /* R1: +Dirname, R2: -Canonicized Dirname:
			   If file is a directory, add trailing slash and
			   rectify filename (delete multiple slashes, '..' and
			   '.'. */
    ctop_string(2, string_find(dirname_canonic(ptoc_string(1)), 1));
    break;
  case SLASH_BUILTIN: {  /* R1: -Slash. Tells what kind of slash the OS uses */
    static char slash_string[2];
    slash_string[0] = SLASH;
    slash_string[1] = '\0';
    ctop_string(1, string_find(slash_string, 1));
    break;
  }
  case FORMATTED_IO:
    return formatted_io();
  case FILE_READ_CANONICAL:
    return read_canonical();
  case GEN_RETRACT_ALL:
    return gen_retract_all();
  case COMPILED_TO_DYNAMIC:
    compiled_to_dynamic();
    break;
  case DB_RETRACT0:
    db_retract0();
    break;
  case DB_GET_CLAUSE:
    db_get_clause();
    break;
  case DB_BUILD_PRREF:
    db_build_prref();
    break;
  case DB_REMOVE_PRREF:
    db_remove_prref();
    break;
  case DB_RECLAIM0:
    db_reclaim0();
    break;
    
/*----------------------------------------------------------------------*/

#include "std_cases.i"
    
#ifdef ORACLE
#include "oracle.i"
#endif
    
#ifdef XSB_ODBC
#include "xsb_odbc.i"
#endif
    
/*----------------------------------------------------------------------*/
    
  case TABLE_STATUS:  /* reg1: +term; reg2: -status (int) */
    term = ptoc_tag(1);
    if ((psc = term_psc(term)) == NULL) {
      err_handle(TYPE, 1, "table_status", 2, "callable term", term);
      return FALSE;	/* fail */
    }
    tip = get_tip(psc);
    if (tip == NULL) {
      value = 0; /* undef */
    } else {
      subgoal_ptr = get_subgoal_ptr(term, tip);
      if (subgoal_ptr == NULL) {
	value = 1; /* no_call_yet */
      } else {
	value = (is_completed(subgoal_ptr)) ?  2 : 3;
      }
    }
    ctop_int(2, value);
    break;
  case ABOLISH_TABLE_PREDICATE:
    term = ptoc_tag(1);
    if ((psc = term_psc(term)) == NULL) {
      err_handle(TYPE, 1, "abolish_table_pred", 1,
		 "predicate (specification)", term);
      return FALSE;	/* fail */
    }
    tip = get_tip(psc);
    if (tip == NULL) {
      xsb_abort("Cannot abolish tables of untabled predicate %s/%d",
		get_name(psc), get_arity(psc));
    } else {
      NODEptr CallRoot = (NODEptr)ti_call_trie_root(tip);

      ti_call_trie_root(tip) = NULL;
      delete_predicate_table(CallRoot);
    }
    break;
  case TRIE_ASSERT:
    if (trie_assert())
      return TRUE;
    else
      xsb_exit("Failure of trie_assert/1");
  case TRIE_RETRACT:
    if (trie_retract())
      return TRUE;
    else
      xsb_exit("Failure of trie_retract/1");
  case TRIE_RETRACT_SAFE:
    return trie_retract_safe();
  case TRIE_DELETE_TERM:
    if (ptoc_int(3) == 0)
      delete_branch((NODEptr)ptoc_int(1),(NODEptr *)ptoc_int(2)); 
    else
      delete_return((NODEptr)ptoc_int(1),(SGFrame)ptoc_int(2)); 
    break;
  case TRIE_GET_RETURN:
    pcreg = trie_get_returns_for_call();
    break;
  case TRIE_GET_CALL: /* r1: +call_term */
    pcreg = trie_get_calls();
    break;
  case GET_LASTNODE_CS_RETSKEL:
    get_lastnode_cs_retskel();
    break;
  case CONSTRUCT_RET_FOR_CALL:
    construct_ret_for_call();
    break;
  case BREG_RETSKEL:
    breg_retskel();
    break;

  case TRIMCORE:
    /*
     * In each case, check whether the initial size of the data area is
     * large enough to contain the currently used portion of the data area.
     */
    if (tcpstack.size != tcpstack.init_size)
      if ( (Integer)((tcpstack.high - (byte *)top_of_cpstack) +
		     ((byte *)top_of_trail - tcpstack.low))
	   < tcpstack.init_size * K - OVERFLOW_MARGIN )
	tcpstack_realloc(tcpstack.init_size);

    if (complstack.size != complstack.init_size)
      if ( (Integer)(complstack.high - (byte *)openreg)
	   < complstack.init_size * K - OVERFLOW_MARGIN )
	complstack_realloc(complstack.init_size);

	if (glstack.size != glstack.init_size)
	  if ( (Integer)((glstack.high - (byte *)top_of_localstk) +
			 ((byte *)hreg - glstack.low))
	       < glstack.init_size * K - OVERFLOW_MARGIN )
	glstack_realloc(glstack.init_size,0);

    break;

  case NEWTRIE:
    newtrie();
    break;
  case TRIE_INTERN:
    trie_intern();
    break;
  case TRIE_INTERNED:
    return(trie_interned());
  case TRIE_DISPOSE:
    trie_dispose();
    break;
  case TRIE_DISPOSE_NR:
    safe_delete_branch((NODEptr)ptoc_int(1));
    break;
  case TRIE_UNDISPOSE:
    undelete_branch((NODEptr) ptoc_int(2));
    break;
  case BOTTOM_UP_UNIFY:
    return ( bottom_up_unify() );
    break;
  case DELETE_TRIE:
    if (strcmp(ptoc_string(2),"intern") == 0){
      tmpval = ptoc_int(1);
      delete_interned_trie(tmpval);
    }
    else {
      xsb_abort("DELETE_TRIE: Invalid use of this operation");
    }
    break;

  case PRINT_CHAT: print_chat(1) ; return TRUE ;
  case PRINT_LS: print_ls(1) ; return TRUE ;
  case PRINT_TR: print_tr(1) ; return TRUE ;
  case PRINT_HEAP: print_heap(0,2000,1) ; return TRUE ;
  case PRINT_CP: print_cp(1) ; return TRUE ;
  case PRINT_REGS: print_regs(10,1) ; return TRUE ;
  case PRINT_ALL_STACKS: print_all_stacks() ; return TRUE ;
  case EXP_HEAP: glstack_realloc(glstack.size + 1,0) ; return TRUE ;
  case MARK_HEAP: mark_heap(ptoc_int(1),&tmpval) ; return TRUE ;
  case GC_HEAP: return(gc_heap(0)) ;
    
    /* This is the builtin where people should put their private, experimental
       builtin code. SEE THE EXAMPLE IN private_builtin.c to UNDERSTAND HOW TO
       DO IT. Note: even though this is a single builtin, YOU CAN SIMULATE ANY
       NUMBER OF BUILTINS WITH IT.  */
  case PRIVATE_BUILTIN: return private_builtin();

  case IS_CHARLIST: {
    prolog_term size_var;
    int size;
    bool retcode;
    size_var = reg_term(2);
    if (! is_var(size_var)) {
      xsb_abort("IS_CHARLIST: Arg 2 must be a variable");
    }
    retcode = is_charlist(reg_term(1), &size);
    c2p_int(size,size_var);
    return retcode;
  }

  case FINDALL_INIT: return(findall_init()) ;
  case FINDALL_ADD: return(findall_add()) ;
  case FINDALL_GET_SOLS: return(findall_get_solutions()) ;

#ifdef HAVE_SOCKET
  case SOCKET_REQUEST:
    return xsb_socket_request();
#endif /* HAVE_SOCKET */	    

#ifdef WIN_NT
  case JAVA_INTERRUPT: 
    return( startInterruptThread( (SOCKET)ptoc_int(1) ) );
#endif

  case FORCE_TRUTH_VALUE: /* +R1: AnsLeafPtr; +R2: TruthValue */
    as_leaf = (NODEptr) ptoc_addr(1);
    tmpstr = ptoc_string(2);
    if (!strcmp(tmpstr, "true"))
      force_answer_true(as_leaf);
    else if (!strcmp(tmpstr, "false"))
      force_answer_false(as_leaf);
    else xsb_abort("FORCE_TRUTH_VALUE: Argument 2 has unknown truth value");
  break;

  default:
    xsb_exit("Builtin #%d is not implemented", number);
    break;
  }
  return TRUE;
}

/*------------------------- Auxiliary functions -----------------------------*/

static void abolish_table_info(void)
{
  tab_inf_ptr temp;

  temp = first_tip;
  while (temp != 0) {
    ti_call_trie_root(temp) = 0;
    temp = (tab_inf_ptr) ti_next_tip(temp);
  }
  reset_freeze_registers;
  openreg = COMPLSTACKBOTTOM;
  abolish_trie();
  abolish_wfs_space(); 
}

/* --------------------------------------------------------------------	*/

#ifdef PROFILE
static void write_out_profile(void)
{ 
  int i, isum, ssum, tot;
  float rat1, rat2;

  isum = ssum = tot = 0;
  for (i = 0; i < BUILTIN_TBL_SZ; i++) {
    if (inst_table[i][0] != 0) isum = isum + inst_table[i][5];
  }
  for (i = 0; i < BUILTIN_TBL_SZ; i++) {
    if (subinst_table[i][0] != 0) ssum = ssum + subinst_table[i][1];
  }
  tot = isum + ssum;
  if (tot!=0) {
    rat1 = isum / tot;
    rat2 = ssum / tot;
    fprintf(stdout,"total: %d inst: %d pct %f subinst: %d pct %f\n",
	    tot,isum,rat1,ssum,rat2);
    for (i = 0; i < BUILTIN_TBL_SZ; i++) {
      if (inst_table[i][0] != 0)
        fprintf(stdout,"-- %s %x %d %.3f\n",(char *) inst_table[i][0],i,
	        inst_table[i][5],(((float)inst_table[i][5])/(float)tot));
    }
    fprintf(stdout,"_______________subinsts_______________\n");
    for (i = 0; i < BUILTIN_TBL_SZ; i++) {
      if (subinst_table[i][0] != 0) {
	ssum = subinst_table[i][1];
	rat1 = ssum/tot;
	fprintf(stdout,"-- %s %x %d %g \n",(char *) subinst_table[i][0],i,
		subinst_table[i][1],rat1);
      }
    }
    fprintf(stdout,"_______________builtins_______________\n");
    for (i = 0; i < BUILTIN_TBL_SZ; i++)
      if (builtin_table[i][1] > 0 && builtin_table[i][0] != 0)
	fprintf(stdout,"%s %d %d \n",
		(char *) builtin_table[i][0],i,builtin_table[i][1]);
  }
  else 
    fprintf(stdout,"Instruction profiling not turned On\n");
}
#endif

/*----------------------- write_quotedname/2 ---------------------------*/

static bool no_quotes_needed(char *string)
{
  int nextchar;
  int ctr, flag;

  ctr = 0;
  nextchar = (int) string[0];
  flag = 0;
  if (nextchar >= 97 && nextchar <= 122) {    /* 0'a=97, 0'z=122  */
    while (nextchar != '\0' && !flag) {
      if (nextchar < 48 
	  || (nextchar > 57 && nextchar < 65)
	  || ((nextchar > 90 && nextchar < 97) && nextchar != 95)
	  || (nextchar > 122))
	flag = 1;
      ctr++;
      nextchar = (int) string[ctr];
    }
    if (!flag) return FALSE;
  }

  if (string[1] == '\0') {
    if ((int) string[0] == 33 /*--- || (int) string[0] == 59 ---*/)
      return FALSE;
    if ((int) string[0] == 46) return TRUE;
  }

  nextchar = (int) string[0];
  ctr = 0; 
  while (nextchar != '\0' && !flag) {
    switch(nextchar) {
    case 35: case 36: case 38: case 42: case 43: case 45: case 46:
    case 47: case 58: case 60: case 61: case 62: case 63: case 64: 
    case 92: case 94: case 96: case 126:
      nextchar++;
      break;
    default: 
      flag = 1;
    }
    ctr++;
    nextchar = (int) string[ctr];
  }
  return flag;
}

static void double_quotes(char *string, char *new_string)
{
  int ctr = 0, nctr = 0;

  while (string[ctr] != '\0') {
    if (string[ctr] == 39) {
      new_string[nctr] = 39;
      nctr++;
    }
    new_string[nctr] = string[ctr];
    nctr++; ctr++;
  }
  new_string[nctr] = '\0';
}

static void write_quotedname(FILE *file, char *string)
{
  char* new_string;

  new_string  = malloc(2*(strlen(string))+1);

  if (*string == '\0') 
    fprintf(file,"''");
  else {
    if (!no_quotes_needed(string)) {
      fprintf(file,"%s",string);
    }
    else {
      double_quotes(string,new_string);
      fprintf(file,"\'%s\'",new_string);
    }
  }
 
  free(new_string);
}

/*----------------------------------------------------------------------*/

static int fast_ground(CPtr temp)
{
  int j, flag = 1;

  cptr_deref(temp);
  switch(cell_tag(temp)) {
  case FREE: case REF1:
    return FALSE;
  case STRING: case INT: case FLOAT:
    return TRUE;
  case LIST:
    flag = flag * fast_ground(clref_val(temp));
    return flag * fast_ground(clref_val(temp)+1);
  case CS:
    for (j=1; j <= (int)get_arity(get_str_psc(temp)) ; j++) {
      flag = flag * fast_ground(clref_val(temp)+j);
    }
    return flag;
  default:
    xsb_abort("FAST_GROUND: Term with unknown tag (%d)",
	      (int)cell_tag(temp));
    return -1;	/* so that g++ does not complain */
  }
}

/*----------------------------------------------------------------------*/
