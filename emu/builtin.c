/* File:      builtin.c
** Author(s): Xu, Warren, Sagonas, Swift, Freire, Johnson
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1999
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

#include "configs/xsb_config.h"
#include "debugs/xsb_debug.h"
/* Private debugs */
#include "debugs/debug_delay.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>

#ifdef WIN_NT
#include <windows.h>
#include <direct.h>
#include <io.h>
#include <process.h>
#include <stdarg.h>
#include <winsock.h>
#include <wsipx.h>
#include <tchar.h>
#else /* Unix */
#include <unistd.h> 
#include <sys/socket.h>
#include <sys/uio.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#include <fcntl.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "psc_xsb.h"
#include "hash_xsb.h"
#include "tries.h"
#include "choice.h"
#include "deref.h"
#include "memory_xsb.h"
#include "heap_xsb.h"
#include "register.h"
#include "flags_xsb.h"
#include "loader_xsb.h"
#include "binding.h"
#include "macro_xsb.h"
#include "token_xsb.h"
#include "builtin.h"
#include "sig_xsb.h"
#include "subp.h"
#include "tr_utils.h"
#include "trassert.h"
#include "dynload.h"
#include "cinterf.h"
#ifdef CHAT
#include "chat.h"
#endif
#include "residual.h"
#include "tables.h"
#include "trie_internals.h"
#include "table_status_defs.h"

#ifdef ORACLE
#include "oracle_xsb.h"
#endif

#ifdef XSB_ODBC
#include "odbc_xsb.h"
#endif

#ifdef PROFILE
#include "inst_xsb.h"
#include "subinst.h"
#endif

#include "io_builtins_xsb.h"

/* special.h must be included after sys/stat.h */
#include "configs/special.h"
#include "system_xsb.h"
#include "random_xsb.h"

/*======================================================================*/

extern TIFptr first_tip;

extern int  sys_syscall(int);
extern bool sys_system(int);
extern bool formatted_io(void), read_canonical(void);
extern bool file_stat(void);
extern bool private_builtin(void);

extern void xsb_segfault_quitter(int err);

#ifdef WIN_NT
extern boolean startInterruptThread(SOCKET intSocket);
#endif

extern bool assert_code_to_buff(void), assert_buff_to_clref(void);
extern bool gen_retract_all(void), db_retract0(void), db_get_clause(void);
extern bool db_build_prref(void), db_remove_prref(void), db_reclaim0(void);

extern char *dirname_canonic(char *);
extern bool almost_search_module(char *);
extern char *expand_filename(char *filename);
extern char *existing_file_extension(char *);
extern char *tilde_expand_filename(char *filename);
extern bool is_absolute_filename(char *filename);
extern void parse_filename(char *filenam, char **dir, char **base, char **ext);

extern bool xsb_socket_request(void);

extern int  findall_init(void), findall_add(void), findall_get_solutions(void);
extern int  copy_term(void);

extern bool substring(void);
extern bool string_substitute(void);
extern bool str_cat(void);
extern bool str_sub(void);

extern void force_answer_true(BTNptr);
extern void force_answer_false(BTNptr);

extern int set_scope_marker();
extern int unwind_stack();
extern int clean_up_block();

#if (defined(DEBUG) && defined(DEBUG_DELAY))
extern void print_delay_list(FILE *, CPtr);
extern void print_subgoal(FILE *, SGFrame);
extern void printterm(Cell, byte, int);	/* used in bineg_xsb_i.h */
#endif

/* ------- variables also used in other parts of the system -----------	*/

Cell flags[64];			  /* System flags + user flags */

/* ------- utility routines -------------------------------------------	*/


#include "ptoc_tag_xsb_i.h"


DllExport prolog_int call_conv ptoc_int(int regnum)
{
  /* reg is global array in register.h */
  register Cell addr = cell(reg+regnum);

  /* deref and then check the type */
  deref(addr);
  switch (cell_tag(addr)) {
  case FREE:
  case REF1: 
  case ATTV:
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
  case ATTV:
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
  case ATTV:
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
 *  For decoding object pointers, like PSC, PSC-PAIR and Subgoal frames.
 */
#define ptoc_addr(regnum)	(void *)ptoc_int(regnum)
#define is_encoded_addr(term)	isinteger(term)
#define decode_addr(term)	(void *)int_val(term)


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
  else
    xsb_abort("CTOP_STRING: Wrong type of argument: %lux", addr);
}

inline static void ctop_constr(int regnum, Pair psc_pair)
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
inline static void ctop_tag(int regnum, Cell term)
{
  register Cell addr = cell(reg+regnum);

  deref(addr);
  if (isref(addr)) {
    bind_copy(vptr(addr), term);
  }
  else
    xsb_abort("CTOP_TAG: Wrong type of argument: %lux", addr);
}


/*
 *  For encoding object pointer, like PSC, PSC-PAIR and Subgoal frames.
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

/* -------------------------------------------------------------------- */

static int ground(CPtr temp)
{
  cptr_deref(temp);
  switch(cell_tag(temp)) {
  case FREE: 
  case REF1: 
  case ATTV:
    return FALSE;
  case STRING: 
  case INT: 
  case FLOAT:
    return TRUE;
  case LIST:
    {
      int flag;
      flag = ground(clref_val(temp));
      flag = flag & ground(clref_val(temp)+1);
      return flag;
    }
  case CS:
    {
      int j, arity, flag=1;
      arity = (int) get_arity(get_str_psc(temp));
      for (j=1; j <= arity ; j++) 
	flag = flag & ground(clref_val(temp)+j);
      return flag;
    }
  default:
    xsb_abort("In ground/1: Term with unknown tag (%d)",
	      (int)cell_tag(temp));
    return -1;	/* so that g++ does not complain */
  }
}

/* --------------------------------------------------------------------	*/

inline static int is_proper_list(Cell term)	/* for standard preds */
{
  register Cell addr;

  addr = term;
  deref(addr);
  while (islist(addr)) {
    addr = cell(clref_val(addr)+1);
    deref(addr);
  }
  return isnil(addr);
}

/* --------------------------------------------------------------------	*/

static CPtr mini_trail[MAX_ARITY];
static CPtr *mini_trail_top;

#define mini_undo_bindings		        \
    while (mini_trail_top >= mini_trail) {	\
	untrail(*mini_trail_top);		\
	mini_trail_top--;			\
    }	

#define mini_bind_variable(addr)                \
   follow(addr) = makenil;			\
   *(++mini_trail_top) = (CPtr)addr;

static int is_most_general_term(Cell term)
{
  deref(term);
  switch (cell_tag(term)) {
  case STRING:
    return TRUE;
  case CS:
    {
      Psc psc;
      CPtr taddr;
      int i, arity;
      register Cell addr;

      mini_trail_top = (CPtr *)(& mini_trail[0]) - 1;
      psc = get_str_psc(term);
      taddr = clref_val(term);
      arity = (int) get_arity(psc);

      for (i = 1; i <= arity ; ++i) {
	addr = cell(taddr+i);
	deref(addr);
	if (isnonvar(addr)) {
	  mini_undo_bindings;
	  return FALSE;
	} else {
	  mini_bind_variable(addr);
	}
      }
      mini_undo_bindings;
      return TRUE;
    }
  case LIST:
    {
      register Cell addr;

      mini_trail_top = (CPtr *) (& mini_trail[0]) -1;
      while (islist(term)) {
	addr = cell(clref_val(term));
	deref(addr);
	if (isnonvar(addr)) {
	  mini_undo_bindings;
	  return FALSE;
	} else {
	  mini_bind_variable(addr);
	  term = cell(clref_val(term)+1);
	  deref(term);
	}
      }
      mini_undo_bindings;
      return isnil(term);
    }
  default:
    return FALSE;
  }
}

/* --------------------------------------------------------------------	*/

#include "term_psc_xsb_i.h"

/* -------------------------------------------------------------------- */

inline static void xsb_fprint_variable(FILE *fptr, CPtr var)
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
  set_builtin_table(PSC_SET_SPY, "psc_set_spy");
  set_builtin_table(PSC_EP, "psc_ep");
  set_builtin_table(PSC_SET_EP, "psc_set_ep");

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
  set_builtin_table(SUBSTRING, "substring");
  set_builtin_table(STR_CAT, "str_cat");
  set_builtin_table(STR_CMP, "str_cmp");
  set_builtin_table(STRING_SUBSTITUTE, "string_substitute");

  set_builtin_table(CALL0, "call0");
  set_builtin_table(STAT_STA, "stat_sta");
  set_builtin_table(STAT_CPUTIME, "stat_cputime");
  set_builtin_table(CODE_LOAD, "code_load");
  set_builtin_table(BUFF_SET_VAR, "buff_set_var");
  set_builtin_table(BUFF_DEALLOC, "buff_dealloc");
  set_builtin_table(BUFF_CELL, "buff_cell");
  set_builtin_table(BUFF_SET_CELL, "buff_set_cell");
  set_builtin_table(COPY_TERM,"copy_term");

  set_builtin_table(STR_SUB, "str_sub");
  set_builtin_table(DIRNAME_CANONIC, "dirname_canonic");

  set_builtin_table(PSC_INSERT, "psc_insert");
  set_builtin_table(PSC_IMPORT, "psc_import");
  set_builtin_table(PSC_INSERTMOD, "psc_insertmod");

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
  set_builtin_table(GROUND, "ground");

  set_builtin_table(INTERN_STRING, "intern_string");
  set_builtin_table(EXPAND_FILENAME, "expand_filename");
  set_builtin_table(TILDE_EXPAND_FILENAME, "tilde_expand_filename");
  set_builtin_table(IS_ABSOLUTE_FILENAME, "is_absolute_filename");
  set_builtin_table(PARSE_FILENAME, "parse_filename");
  set_builtin_table(ALMOST_SEARCH_MODULE, "almost_search_module");
  set_builtin_table(EXISTING_FILE_EXTENSION, "existing_file_extension");

  set_builtin_table(PSC_ENV, "psc_env");
  set_builtin_table(PSC_SPY, "psc_spy");
  set_builtin_table(PSC_TABLED, "psc_tabled");

  set_builtin_table(IS_INCOMPLETE, "is_incomplete");

  set_builtin_table(GET_PTCP, "get_ptcp");
  set_builtin_table(GET_SUBGOAL_PTR, "get_subgoal_ptr");
  set_builtin_table(DEREFERENCE_THE_BUCKET, "dereference_the_bucket");
  set_builtin_table(PAIR_PSC, "pair_psc");
  set_builtin_table(PAIR_NEXT, "pair_next");
  set_builtin_table(NEXT_BUCKET, "next_bucket");

  set_builtin_table(SLG_NOT, "slg_not");
  set_builtin_table(IS_XWAMMODE, "is_xwammode");
  set_builtin_table(CLOSE_OPEN_TABLES, "close_open_tables");

  set_builtin_table(FILE_FUNCTION, "file_function");
  set_builtin_table(SLASH_BUILTIN, "slash");

  set_builtin_table(ABOLISH_TABLE_INFO, "abolish_table_info");
  set_builtin_table(ZERO_OUT_PROFILE, "zero_out_profile");
  set_builtin_table(WRITE_OUT_PROFILE, "write_out_profile");
  set_builtin_table(ASSERT_CODE_TO_BUFF, "assert_code_to_buff");
  set_builtin_table(ASSERT_BUFF_TO_CLREF, "assert_buff_to_clref");

  set_builtin_table(FILE_READ_CANONICAL, "file_read_canonical");
  set_builtin_table(GEN_RETRACT_ALL, "gen_retract_all");

  set_builtin_table(DB_RETRACT0, "db_retract0");
  set_builtin_table(DB_GET_CLAUSE, "db_get_clause");
  set_builtin_table(DB_BUILD_PRREF, "db_build_prref");
  set_builtin_table(DB_REMOVE_PRREF, "db_remove_prref");
  set_builtin_table(DB_RECLAIM0, "db_reclaim0");

  set_builtin_table(FORMATTED_IO, "formatted_io");
  set_builtin_table(TABLE_STATUS, "table_status");
  set_builtin_table(GET_DELAY_LISTS, "get_delay_lists");

  set_builtin_table(ABOLISH_TABLE_PREDICATE, "abolish_table_predicate");
  set_builtin_table(TRIE_ASSERT, "trie_assert");
  set_builtin_table(TRIE_RETRACT, "trie_retract");
  set_builtin_table(TRIE_RETRACT_SAFE, "trie_retract_safe");
  set_builtin_table(TRIE_DELETE_TERM, "trie_delete_term");
  set_builtin_table(TRIE_GET_RETURN, "trie_get_return");

  /* Note: TRIE_GET_CALL previously used for get_calls/1, before get_call/3
     was made a builtin itself. */
  set_builtin_table(TRIE_UNIFY_CALL, "get_calls");
  set_builtin_table(GET_LASTNODE_CS_RETSKEL, "get_lastnode_cs_retskel");
  set_builtin_table(TRIE_GET_CALL, "get_call");
  set_builtin_table(BREG_RETSKEL,"breg_retskel");

  set_builtin_table(TRIMCORE, "trimcore");
  set_builtin_table(NEWTRIE, "newtrie");
  set_builtin_table(TRIE_INTERN, "trie_intern");
  set_builtin_table(TRIE_INTERNED, "trie_interned");
  set_builtin_table(TRIE_DISPOSE, "trie_dispose");
  set_builtin_table(BOTTOM_UP_UNIFY, "bottom_up_unify");
  set_builtin_table(DELETE_TRIE, "delete_trie");
  set_builtin_table(TRIE_DISPOSE_NR, "trie_dispose_nr");
  set_builtin_table(TRIE_UNDISPOSE, "trie_undispose");

  set_builtin_table(SET_TABLED_EVAL, "set_tabled_eval_method");
  set_builtin_table(PUT_ATTRIBUTES, "put_attributes");
  set_builtin_table(GET_ATTRIBUTES, "get_attributes");
  set_builtin_table(DELETE_ATTRIBUTES, "delete_attributes");
  set_builtin_table(ATTV_UNIFY, "attv_unify");
  set_builtin_table(PRIVATE_BUILTIN, "private_builtin");
  set_builtin_table(SEGFAULT_HANDLER, "segfault_handler");

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
  set_builtin_table(IS_MOST_GENERAL_TERM, "is_most_general_term");
  set_builtin_table(HiLog_ARG, "hilog_arg");
  set_builtin_table(HiLog_UNIV, "hilog_univ");
  set_builtin_table(ATOM_CODES, "atom_codes");
  set_builtin_table(ATOM_CHARS, "atom_chars");
  set_builtin_table(NUMBER_CHARS, "number_chars");
  set_builtin_table(NUMBER_CODES, "number_codes");
  set_builtin_table(IS_CHARLIST, "is_charlist");
  set_builtin_table(NUMBER_DIGITS, "number_digits");

  set_builtin_table(PUT, "put");
  set_builtin_table(TAB, "tab");
  set_builtin_table(SORT, "sort");
  set_builtin_table(KEYSORT, "keysort");

  set_builtin_table(ORACLE_QUERY, "oracle_query");
  set_builtin_table(ODBC_EXEC_QUERY, "odbc_exec_query");
  set_builtin_table(SET_SCOPE_MARKER, "set_scope_marker");
  set_builtin_table(UNWIND_STACK, "unwind_stack");
  set_builtin_table(CLEAN_UP_BLOCK, "clean_up_block");

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

inline static void abolish_table_info(void)
{
  TIFptr pTIF;

  pTIF = first_tip;
  while (IsNonNULL(pTIF)) {
    TIF_CallTrie(pTIF) = NULL;
    pTIF = TIF_NextTIF(pTIF);
  }
  reset_freeze_registers;
  openreg = COMPLSTACKBOTTOM;
  release_all_tabling_resources();
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

/* inlined definition of file_function */
#include "io_builtins_xsb_i.h"

/* inlined functions for prolog standard builtins */
#include "std_pred_xsb_i.h"

/* --- built in predicates --------------------------------------------	*/

int builtin_call(byte number)
{
  switch (number) {
  case PSC_NAME: {	/* R1: +PSC; R2: -String */
    Psc psc = ptoc_addr(1);
    ctop_string(2, get_name(psc));
    break;
  }
  case PSC_ARITY: {	/* R1: +PSC; R2: -int */
    Psc psc = ptoc_addr(1);
    ctop_int(2, (Integer)get_arity(psc));
    break;
  }
  case PSC_TYPE: {	/* R1: +PSC; R2: -int */
			/* type: see psc_xsb.h, `entry_type' field defs */
    Psc psc = ptoc_addr(1);
    ctop_int(2, (Integer)get_type(psc));
    break;
  }
  case PSC_SET_TYPE: {	/* R1: +PSC; R2: +type (int): see psc_xsb.h */
    Psc psc = ptoc_addr(1);
    set_type(psc, ptoc_int(2));
    break;
  }
  case PSC_PROP: {	/* R1: +PSC; R2: -term */
			/* prop: as a buffer pointer */
    Psc psc = ptoc_addr(1);
    ctop_int(2, (Integer)get_data(psc));
    break;
  }
  case PSC_SET_PROP: {	       /* R1: +PSC; R2: +int */
    Psc psc = ptoc_addr(1);
    set_data(psc, (Psc)ptoc_int(2));
    break;
  }
  case PSC_EP: {	/* R1: +PSC; R2: -term */
			/* prop: as a buffer pointer */
    Psc psc = ptoc_addr(1);
    ctop_int(2, (Integer)get_ep(psc));
    break;
  }
  case PSC_SET_EP: {	       /* R1: +PSC; R2: +int */
    Psc psc = ptoc_addr(1);
    pb ep = (pb)ptoc_int(2);
    set_ep(psc, (ep==NULL?((byte *)(&(psc->load_inst))):ep));
    break;
  }

  case PSC_SET_SPY: { 	       /* R1: +PSC; R2: +int */
    Psc psc = ptoc_addr(1);
    set_spy(psc, ptoc_int(2));
    break;
  }

  case FILE_FUNCTION:  /* file_open/close/put/get/truncate/seek/pos */
    return file_function();

  case TERM_PSC:		/* R1: +term; R2: -PSC */
    /* Assumes that `term' is a CS-tagged Cell. */
    ctop_addr(2, get_str_psc(ptoc_tag(1)));
    break;
  case TERM_TYPE: {	/* R1: +term; R2: tag (-int)			  */
			/* <0 - var, 1 - cs, 2 - int, 3 - list, 7 - ATTV> */
    Cell term = ptoc_tag(1);
    if (isref(term)) ctop_int(2, 0);
    else ctop_int(2, cell_tag(term));
    break;
  }
  case TERM_COMPARE:	/* R1, R2: +term; R3: res (-int) */
    ctop_int(3, compare(ptoc_tag(1), ptoc_tag(2)));
    break;
  case TERM_NEW: {		/* R1: +PSC, R2: -term */
    int disp;
    Psc psc = ptoc_addr(1);
    sreg = hreg;
    hreg += get_arity(psc) + 1;
    ctop_constr(2, (Pair)sreg);
    new_heap_functor(sreg, psc);
    for (disp=0; disp < (int)get_arity(psc); sreg++,disp++) {
      bld_free(sreg);
    }
    break;
  }
  case TERM_ARG: {	/* R1: +term; R2: index (+int); R3: arg (-term) */
    int  disp = ptoc_int(2);
    Cell term = ptoc_tag(1);
    ctop_tag(3, cell(clref_val(term)+disp));
    break;
  }
  case TERM_SET_ARG: {	/* R1: +term; R2: index (+int) */
			/* R3: newarg (+term); R4: +perm(not used) */
    /* used in file_read.P, array.P, array1.P */
    int  disp = ptoc_int(2);
    Cell term = ptoc_tag(1);
    if (!ptoc_int(4)) { pushtrail(clref_val(term)+disp,cell(reg+3));}
    bld_copy(clref_val(term)+disp, cell(reg+3));
    break;
  }
  case STAT_FLAG:	/* R1: flagname(+int); R2: value(-int) */
    ctop_int(2, flags[ptoc_int(1)]);
    break;
  case STAT_SET_FLAG:	/* R1: flagname(+int); R2: value(+int); */
    flags[ptoc_int(1)] = ptoc_int(2);
    call_intercept = (byte) (flags[DEBUG_ON]|flags[TRACE_STA]|flags[HITRACE]
      |flags[CLAUSE_INT]);
    break;
  case BUFF_ALLOC: {	/* R1: size (+integer); R2: -buffer; */
	           /* the length of the buffer is also stored at position 0 */
    char *addr;
    int  value = ((ptoc_int(1)+7)>>3)<<3;
    value *= ZOOM_FACTOR ;
    addr = (char *)mem_alloc(value);
    value /= ZOOM_FACTOR ;
    *(Integer *)addr = value;	/* store buffer size at buf[0] */
    ctop_int(2, (Integer)addr);	/* use "integer" type now! */
    break;
  }
  case BUFF_DEALLOC: {	/* R1: +buffer; R2: +oldsize; R3: +newsize; */
    int  value;
    char *addr = ptoc_string(1);
    int  disp = ((ptoc_int(2)+7)>>3)<<3;
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
  }
  case BUFF_WORD: {     /* R1: +buffer; r2: displacement(+integer); */
			/* R3: value (-integer) */
    char *addr = ptoc_string(1);
    int  disp = ptoc_int(2);
    disp *= ZOOM_FACTOR ;
    ctop_int(3, *(Integer *)(addr+disp));
    break;
  }
  case BUFF_SET_WORD: {	/* R1: +buffer; r2: displacement(+integer); */
			/* R3: value (+integer) */
    char *addr = ptoc_string(1);
    int  disp = ptoc_int(2);
    disp *= ZOOM_FACTOR ;
    *(CPtr)(addr+disp) = ptoc_int(3);
    break;
  }
  case BUFF_BYTE: {	/* R1: +buffer; r2: displacement(+integer); */
			/* R3: value (-integer) */
    char *addr = ptoc_string(1);
    int  disp = ptoc_int(2);
    ctop_int(3, (Integer)(*(byte *)(addr+disp)));
    break;
  }
  case BUFF_SET_BYTE: {	/* R1: +buffer; R2: displacement(+integer); */
			/* R3: value (+integer) */
    char *addr = ptoc_string(1);
    int  disp = ptoc_int(2);
    *(pb)(addr+disp) = ptoc_int(3);
    break;
  }
  case BUFF_CELL: {	/* R1: +buffer; R2: displacement(+integer); */
                        /* R3: -Cell at that location */
    char *addr = ptoc_string(1);
    int  disp = ptoc_int(2);
    disp *= ZOOM_FACTOR ;
    ctop_tag(3, (Cell)(addr+disp));
    break;
  }
  case BUFF_SET_CELL: {	/* R1: +buffer; R2: displacement(+integer);*/
			/* R3: type (+integer); R4: +term */
    /* When disp<0, set the type of the buff itself */
    /* The last function is not implemented */
    int  value;
    char *addr = ptoc_string(1);
    int  disp = ptoc_int(2);
    disp *= ZOOM_FACTOR ;
    value = ptoc_int(3);
    switch (value) {
    case REF: case REF1:
      bld_ref(vptr(addr+disp), (CPtr)ptoc_int(4)); break;
    case INT: {
      int tmpval = ptoc_int(4);
      bld_int(vptr(addr+disp), tmpval); break;
    }
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
  }
  case BUFF_SET_VAR: {
    int  disp;
    Cell term;
    char *addr;
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
  }
  case COPY_TERM: /* R1: +term to copy; R2: -variant */
    copy_term();
    break;
    
  case CALL0: {			/* R1: +Term, the call to be made */
    /* Note: this procedure does not save cpreg, hence is more like */
    /* an "execute" instruction, and must be used as the last goal!!!*/
    Psc  psc;
    Cell term = ptoc_tag(1);

    if (isconstr(term)) {
      int  disp;
      char *addr;
      psc = get_str_psc(term);
      addr = (char *)(clref_val(term));
      for (disp = 1; disp <= (int)get_arity(psc); ++disp) {
	bld_copy(reg+disp, cell((CPtr)(addr)+disp));
      }
    } else if (isstring(term)) {
      int  value;
      Pair sym = insert(string_val(term),0,(Psc)flags[CURRENT_MODULE],&value);
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
    case T_DYNA:
    case T_UDEF:
    default:
    case T_FORN:
      pcreg = get_ep(psc);
      break;
/****    case T_FORN:
#ifdef FOREIGN
      proc_ptr = (PFI) get_ep(psc);
      if (proc_ptr())
	pcreg = cpreg;
      else
	pcreg = (pb)&fail_inst;	 
#else
      xsb_exit("Foreign call in configuration that does not support it !");
#endif
      break;
    case T_UDEF:
    default:
      psc = synint_proc(psc, MYSIG_UNDEF, NULL);
      if (psc) pcreg = get_ep(psc);
      break;
*****/
    }
    if (call_intercept) intercept(psc);
    break;
  }
    
  case CODE_CALL: {		/* R1: +Code (addr), the code address */
				/* R2: +Term, the call to be made */
				/* R3: +Type, code type (same as psc->type)  */
				/* may need to resume interrupt testing here */
    /* Note: this procedure does not save cpreg, hence is more like */
    /* an "execute" instruction, and must be used as the last goal!!!*/
    Psc  psc;
    Cell term = ptoc_tag(2);
    int  value = ptoc_int(3);  /* Cannot be delayed! R3 may be reused */

    pcreg = (byte *)ptoc_int(1);
    if (isconstr(term)) {
      int  disp;
      char *addr;
      psc = get_str_psc(term);
      addr = (char *)(clref_val(term));
      for (disp = 1; disp <= (int)get_arity(psc); ++disp) {
	bld_copy(reg+disp, cell((CPtr)(addr)+disp));
      }
      bld_int(reg+get_arity(psc)+1, value);
    } else psc = NULL; 
    break;
  }
  case SUBSTRING: /* R1: +String; R2,R3: +begin/end offset; R4: -OutSubstr */
    return substring(); 
  case STRING_SUBSTITUTE: /* R1: +Str, R2: [s(a1,b1),s(a2,b2),...], 
			     R3: [str1,str2,...], R4: -OutStr */
    return string_substitute();
  case STR_LEN:	{	/* R1: +String; R2: -Length */
    Cell term = ptoc_tag(1);
    if (isstring(term)) {
      char *addr = string_val(term);
      return int_unify(makeint(strlen(addr)), ptoc_tag(2));
    } else return FALSE;
  }
  case STR_CAT:		/* R1: +Str1; R2: +Str2: R3: -Str3 */
    return str_cat();
  case STR_CMP:		/* R1: +Str1; R2: +Str2: R3: -Res */
    ctop_int(3, strcmp(ptoc_string(1), ptoc_string(2)));
    break;
  case STR_SUB:   /* R1: +Substring; R2: +String; R3: -Pos */
    return str_sub();
  case INTERN_STRING: /* R1: +String1; R2: -String2 ; Intern string */
    ctop_string(2, string_find(ptoc_string(1), 1));
    break;
  case STAT_STA: {		/* R1: +Amount */
    int value = ptoc_int(1);
    print_statistics(value);
    break;
  }
  case STAT_CPUTIME: {	/* R1: -cputime, in miliseconds */	
    int value = (int)(cpu_time() * 1000);
    ctop_int(1, value);
    break;
  }
  case CODE_LOAD:		/* R1: +FileName, bytecode file to be loaded */
				/* R2: -int, addr of 1st instruction;	     */
				/*	0 indicates an error                 */
				/* R3 = 1 if exports to be exported, 0 otw   */
    ctop_int(2, (Integer)loader(ptoc_string(1), ptoc_int(3)));
    break;

  case PSC_INSERT: {	/* R1: +String, symbol name
			   R2: +Arity
			   R3: -PSC, the new PSC
			   R4: +String, module to be inserted */
    /* inserts or finds a symbol in a given module.	*/
    /* When the given module is 0 (null string), current module is used. */
    Psc  psc;
    Pair sym;
    int  value;
    char *addr = ptoc_string(4);
    if (addr)
      psc = pair_psc(insert_module(0, addr));
    else
      psc = (Psc)flags[CURRENT_MODULE];
    sym = insert(ptoc_string(1), (char)ptoc_int(2), psc, &value);
    ctop_addr(3, pair_psc(sym));
    break;
  }

  case PSC_IMPORT: {    /* R1: +String, functor name to be imported
			   R2: +Arity
			   R3: +String, Module name where functor lives  */
    /*
     * Creates a PSC record for a predicate and its module (if they
     * don't already exist) and links the predicate into usermod.
     */
    int  value;
    Psc  psc = pair_psc(insert_module(0, ptoc_string(3)));
    Pair sym = insert(ptoc_string(1), (char)ptoc_int(2), psc, &value);
    if (value)       /* if predicate is new */
      set_data(pair_psc(sym), (psc));
    env_type_set(pair_psc(sym), T_IMPORTED, T_ORDI, (bool)value);
    link_sym(pair_psc(sym), (Psc)flags[CURRENT_MODULE]);
    break;
  }

  case FILE_GETTOKEN: {    /* R1: +File, R2: +PrevCh, R3: -Type; */
                                /* R4: -Value, R5: -NextCh */
    int tmpval = ptoc_int(1);
    if ((tmpval < 0) && (tmpval >= -MAXIOSTRS))
      token = GetToken(NULL,strfileptr(tmpval), ptoc_int(2));
    else {
      FILE* fptr;
      SET_FILEPTR(fptr, tmpval);
      token = GetToken(fptr, NULL, ptoc_int(2));
    }
    if (token->type == TK_ERROR) {
      pcreg = (pb)&fail_inst;
    }
    else {
      ctop_int(3, token->type);
      ctop_int(5, token->nextch);
      switch (token->type) {
        case TK_ATOM : case TK_FUNC : case TK_STR : case TK_LIST :
        case TK_VAR : case TK_VVAR : case TK_VARFUNC : case TK_VVARFUNC :
	  ctop_string(4, token->value);
	  break;
        case TK_INT : case TK_INTFUNC :
	  ctop_int(4, *(long *)(token->value));
	  break;
        case TK_REAL : case TK_REALFUNC : 
	  {Float float_temp =  (float)(*(double *)(token->value));
	  ctop_float(4, float_temp);
	  }
	  break;
        case TK_PUNC : case TK_HPUNC :
	  ctop_int(4, *(token->value)); break;
        case TK_EOC : case TK_EOF :
	  ctop_int(4, 0); break;
      }
    }
    break;
  }
  case FILE_PUTTOKEN: {	/* R1: +File, R2: +Type, R3: +Value; */
    FILE* fptr;
    int tmpval = ptoc_int(1);
    SET_FILEPTR(fptr,tmpval);
    switch (ptoc_int(2)) {
    case FREE   : {
      CPtr var = (CPtr)ptoc_tag(3);
      xsb_fprint_variable(fptr, var);
      break;
    }
    case ATTV   : {
      CPtr var = (CPtr)dec_addr(ptoc_tag(3));
      xsb_fprint_variable(fptr, var);
      break;
    }
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
  }
  case PSC_INSERTMOD: { /* R1: +String, Module name */
                        /* R2: +Def (4 - is a definition; 0 -not) */
                        /* R3: -PSC of the Module entry */
    Pair sym = insert_module(ptoc_int(2), ptoc_string(1));
    ctop_addr(3, pair_psc(sym));
    break;
  }
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

  case WH_RANDOM:		/* R1: +Type of operation */
    switch (ptoc_int(1)) {
    case RET_RANDOM:		/* return a random float in [0.0, 1.0) */
      if (ret_random() == -1)
	return FALSE;
      break;
    case GET_RAND:		/* getrand */
      if (getrand() == -1)
	return FALSE;
      break;
    case SET_RAND:		/* setrand */
      setrand();
      break;
    }
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
  case ALMOST_SEARCH_MODULE: /* R1: +FileName, R2: -Dir, R3: -Mod,
				R4: -Ext, R5: -BaseName */
    return almost_search_module(ptoc_string(1));
  case EXISTING_FILE_EXTENSION: { /* R1: +FileN, R2: ?Ext */
    char *extension = existing_file_extension(ptoc_string(1));
    if (extension == NULL) return FALSE;
    else {
      extension = string_find(extension,1);
      return atom_unify(makestring(extension), ptoc_tag(2));
    }
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
  case FILE_WRITEQUOTED: {
    FILE* fptr;
    int   tmpval = ptoc_int(1);
    SET_FILEPTR(fptr, tmpval);
    write_quotedname(fptr ,ptoc_string(2));
    break;
  }
  case GROUND:
    return ground((CPtr)ptoc_tag(1));

  case PSC_ENV:	{       /* reg 1: +PSC; reg 2: -int */
    /* env: 0 = exported, 1 = local, 2 = imported */
    Psc psc = ptoc_addr(1);
    ctop_int(2, (Integer)get_env(psc));
    break;
  }
  case PSC_SPY:	{	/* reg 1: +PSC; reg 2: -int */
				/* env: 0 = non-spied else spied */
    Psc psc = ptoc_addr(1);
    ctop_int(2, (Integer)get_spy(psc));
    break;
  }
 case PSC_TABLED: {	/* reg 1: +PSC; reg 2: -int */
    Psc psc = ptoc_addr(1);
    ctop_int(2, (Integer)get_tip(psc));
    break;
  }
/*----------------------------------------------------------------------*/

#include "bineg_xsb_i.h"

/*----------------------------------------------------------------------*/
  case GET_SUBGOAL_PTR: {	/* reg1: +term; reg2: -subgoal_ptr */
    Psc  psc;
    TIFptr tip;
    Cell term = ptoc_tag(1);

    if ((psc = term_psc(term)) == NULL) {
      err_handle(TYPE, 1, "get_subgoal_ptr", 2, "callable term", term);
      return FALSE;	/* fail */
    }
    tip = get_tip(psc);
    if (tip == NULL) {
      xsb_abort("Predicate %s/%d is not tabled", get_name(psc),get_arity(psc));
    }
    ctop_addr(2, get_subgoal_ptr(term, tip));
    break;
  }

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
  case NEXT_BUCKET: {     /* R1: +Index of Symbol Table Bucket. */
    /* R2: -Next Index (0 if end of Hash Table) */
    int value = ptoc_int(1);
    if ( ((unsigned int)value >= (symbol_table.size - 1)) || (value < 0) )
      ctop_int(2, 0);
    else 
      ctop_int(2, (value + 1));
    break;
  }

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

#include "std_cases_xsb_i.h"
    
#ifdef ORACLE
#include "oracle_xsb_i.h"
#endif
    
#ifdef XSB_ODBC
#include "odbc_xsb_i.h"
#endif
    
/*----------------------------------------------------------------------*/
    
  case TABLE_STATUS: {
    /*
     * Given a tabled goal, report on the following attributes:
     * 1) Predicate Type: Variant, Subsumptive, or Untabled
     * 2) Goal Type: Producer, Properly Subsumed Consumer, Has No
     *      Call Table Entry, or Undefined
     * 3) Answer Set Status: Complete, Incomplete, or Undefined.
     *
     * Valid combinations reported by this routine:
     * When the predicate is an untabled functor, then only one sequence
     *   is generated:  Untabled,Undefined,Undefined
     * Otherwise the following combinations are possible:
     *
     * GoalType    AnsSetStatus   Meaning
     * --------    ------------   -------
     * producer    complete       call exists; it is a completed producer.
     *             incomplete     call exists; it is an incomplete producer.
     *
     * subsumed    complete       call exists; it's properly subsumed by a
     *                              completed producer.
     *             incomplete     call exists; it's properly subsumed by an
     *                              incomplete producer.
     *
     * no_entry    undefined      is a completely new call, not subsumed by
     *                              any other -> if this were to be called
     *                              right now, it would be a producing call.
     *             complete       there is no entry for this call, but if it
     *                              were to be called right now, it would
     *                              consume from a completed producer.
     *                              (The call is properly subsumed.)
     *             incomplete     same as previous, except the subsuming
     *                              producer is incomplete.
     *
     * Notice that not only can these combinations describe the
     * characteristics of a found goal, but they are also equipped to
     * predict how an unfound goal would have been treated had it really
     * been called.
     */
    const int regGoalHandle   = 1;   /* in:  either a term or a SF ptr */
    const int regPredType     = 2;   /* out: status (as INT) */
    const int regGoalType     = 3;   /* out: status (as INT) */
    const int regAnsSetStatus = 4;   /* out: status (as INT) */

    int pred_type, goal_type, answer_set_status;
    SGFrame goalSF, subsumerSF;
    Cell goalTerm;

    goalTerm = ptoc_tag(regGoalHandle);
    if ( is_encoded_addr(goalTerm) ) {
      goalSF = decode_addr(goalTerm);
      if ( IsNULL(goalSF) ) {
	err_handle(TYPE, regGoalHandle, "table_status", 4,
		   "Valid subgoal frame pointer", goalTerm);
	return FALSE;	/* fail */
      }
      subsumerSF = subg_producer(goalSF);
      pred_type = TIF_EvalMethod(subg_tif_ptr(subsumerSF));
    }
    else {
      Psc psc;
      TIFptr tif;

      psc = term_psc(goalTerm);
      if ( IsNULL(psc) ) {
	err_handle(TYPE, regGoalHandle, "table_status", 4,
		   "callable term", goalTerm);
	return FALSE;	/* fail */
      }
      tif = get_tip(psc);
      if ( IsNULL(tif) ) {
	ctop_int(regPredType, UNTABLED_PREDICATE);
	ctop_int(regGoalType, UNDEFINED_CALL);
	ctop_int(regAnsSetStatus, UNDEFINED_ANSWER_SET);
	return TRUE;
      }
      pred_type = TIF_EvalMethod(tif);
      if ( IsVariantPredicate(tif) )
	goalSF = subsumerSF = get_subgoal_ptr(goalTerm, tif);
      else {
	BTNptr leaf;
	TriePathType path_type;

	leaf =
	  subsumptive_trie_lookup(get_arity(psc), clref_val(goalTerm) + 1,
				  TIF_CallTrie(tif), &path_type);
	if ( path_type == NO_PATH )
	  goalSF = subsumerSF = NULL;
	else if ( path_type == VARIANT_PATH ) {
	  goalSF = CallTrieLeaf_GetSF(leaf);
	  subsumerSF = subg_producer(goalSF);
	}
	else {
	  goalSF = NULL;
	  subsumerSF = subg_producer(CallTrieLeaf_GetSF(leaf));
	}
      }
    }
    /*
     * Now both goalSF and subsumerSF should be set for all cases.
     * Determine status values based on these pointers.
     */
    if ( IsNonNULL(goalSF) ) {
      if ( goalSF == subsumerSF )
	goal_type = PRODUCER_CALL;
      else
	goal_type = SUBSUMED_CALL;
    }
    else
      goal_type = NO_CALL_ENTRY;

    if ( IsNonNULL(subsumerSF) ) {
      if ( is_completed(subsumerSF) )
	answer_set_status = COMPLETED_ANSWER_SET;
      else
	answer_set_status = INCOMPLETE_ANSWER_SET;
    }
    else
      answer_set_status = UNDEFINED_ANSWER_SET;

    ctop_int(regPredType, pred_type);
    ctop_int(regGoalType, goal_type);
    ctop_int(regAnsSetStatus, answer_set_status);
    return TRUE;
  }

  case ABOLISH_TABLE_PREDICATE: {
    const int regTerm = 1;   /* in: term with tabled pred as primary functor */
    Cell   term;
    Psc    psc;
    TIFptr tif;

    term = ptoc_tag(regTerm);
    psc = term_psc(term);
    if ( IsNULL(psc) ) {
      err_handle(TYPE, 1, "abolish_table_pred", 1,
		 "predicate (specification)", term);
      return FALSE;	/* fail */
    }
    tif = get_tip(psc);
    if ( IsNULL(tif) ) {
      xsb_abort("Table error: untabled predicate in argument 1 of %s/%d",
		get_name(psc), get_arity(psc));
    }
    delete_predicate_table(tif);
    return TRUE;
  }
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
      delete_branch((BTNptr)ptoc_int(1),(BTNptr *)ptoc_int(2)); 
    else
      delete_return((BTNptr)ptoc_int(1),(SGFrame)ptoc_int(2)); 
    break;
  case TRIE_GET_RETURN:
    pcreg = trie_get_returns_for_call();
    break;
  case TRIE_UNIFY_CALL: /* r1: +call_term */
    pcreg = trie_get_calls();
    break;
  case GET_LASTNODE_CS_RETSKEL: {
    const int regCallTerm  = 1;   /* in: call of a subsumptive predicate */
    const int regTrieLeaf  = 2;   /* out: a unifying trie term handle */
    const int regLeafChild = 3;   /* out: usually to get subgoal frame */
    const int regRetTerm   = 4;   /* out: term in ret/N form:
				     Call Trie -> answer template
				     Other Trie -> variable vector */
    ctop_int(regTrieLeaf, (Integer)Last_Nod_Sav);
    ctop_int(regLeafChild, (Integer)BTN_Child(Last_Nod_Sav));
    ctop_tag(regRetTerm, get_lastnode_cs_retskel(ptoc_tag(regCallTerm)));
    return TRUE;
  }
  case TRIE_GET_CALL: {
    const int regCallTerm = 1;   /* in:  tabled call to look for */
    const int regSF       = 2;   /* out: corresponding subgoal frame */
    const int regRetTerm  = 3;   /* out: answer template in ret/N form */

    Cell ret;
    SGFrame sf;

    sf = get_call(ptoc_tag(regCallTerm), &ret);
    if ( IsNonNULL(sf) ) {
      ctop_int(regSF, (Integer)sf);
      ctop_tag(regRetTerm, ret);
      return TRUE;
    }
    else
      return FALSE;
  }
  case BREG_RETSKEL:
    breg_retskel();
    break;

  case TRIMCORE:
    /*
     * In each case, check whether the initial size of the data area is
     * large enough to contain the currently used portion of the data area.
     */
    if (tcpstack.size != tcpstack.init_size)
      if ( (unsigned int)((tcpstack.high - (byte *)top_of_cpstack) +
		     ((byte *)top_of_trail - tcpstack.low))
	   < tcpstack.init_size * K - OVERFLOW_MARGIN )
	tcpstack_realloc(tcpstack.init_size);

    if (complstack.size != complstack.init_size)
      if ( (unsigned int)(complstack.high - (byte *)openreg)
	   < complstack.init_size * K - OVERFLOW_MARGIN )
	complstack_realloc(complstack.init_size);

	if (glstack.size != glstack.init_size)
	  if ( (unsigned int)((glstack.high - (byte *)top_of_localstk) +
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
    safe_delete_branch((BTNptr)ptoc_int(1));
    break;
  case TRIE_UNDISPOSE:
    undelete_branch((BTNptr) ptoc_int(2));
    break;
  case BOTTOM_UP_UNIFY:
    return ( bottom_up_unify() );
    break;
  case DELETE_TRIE:
    if (strcmp(ptoc_string(2),"intern") == 0){
      int tmpval = ptoc_int(1);
      delete_interned_trie(tmpval);
    }
    else {
      xsb_abort("DELETE_TRIE: Invalid use of this operation");
    }
    break;

  case SET_TABLED_EVAL: {    /* R1: +Term */
    Psc psc;
    TIFptr tif;

    psc = get_str_psc(ptoc_tag(1));
    tif = get_tip(psc);
    if ( IsNonNULL(tif) ) {
      if ( IsNULL(TIF_CallTrie(tif)) ) {
	TIF_EvalMethod(tif) = (TabledEvalMethod)ptoc_int(2);
	return TRUE;
      }
      else {
	xsb_warn("Cannot change tabling method for tabled predicate %s/%d\n"
		  "\t   Calls to %s/%d have already been issued\n",
		  get_name(psc), get_arity(psc),
		  get_name(psc), get_arity(psc));
	return FALSE;
      }
    }
    else {
      xsb_warn("Predicate %s/%d is not tabled", get_name(psc), get_arity(psc));
      return FALSE;
    }
  }

  case PRINT_CHAT: print_chat(1) ; return TRUE ;
  case PRINT_LS: print_ls(1) ; return TRUE ;
  case PRINT_TR: print_tr(1) ; return TRUE ;
  case PRINT_HEAP: print_heap(0,2000,1) ; return TRUE ;
  case PRINT_CP: print_cp(1) ; return TRUE ;
  case PRINT_REGS: print_regs(10,1) ; return TRUE ;
  case PRINT_ALL_STACKS: print_all_stacks() ; return TRUE ;
  case EXP_HEAP: glstack_realloc(glstack.size + 1,0) ; return TRUE ;
  case MARK_HEAP: {
    int tmpval;
    mark_heap(ptoc_int(1),&tmpval);
    return TRUE;
  }
  case GC_HEAP: return(gc_heap(0)) ;
    
    /* This is the builtin where people should put their private, experimental
       builtin code. SEE THE EXAMPLE IN private_builtin.c to UNDERSTAND HOW TO
       DO IT. Note: even though this is a single builtin, YOU CAN SIMULATE ANY
       NUMBER OF BUILTINS WITH IT.  */
  case PRIVATE_BUILTIN: return private_builtin();

  case SEGFAULT_HANDLER: { /* Set the desired segfault handler:
			      +Arg1:  none  - don't catch segfaults;
				      warn  - warn and exit;
				      catch - try to recover */
    char *type = ptoc_string(1);
    switch (*type) {
    case 'w': /* warn: Warn and wuit */
      xsb_default_segfault_handler = xsb_segfault_quitter;
      break;
    case 'n': /* none: Don't handle segfaults */
      xsb_default_segfault_handler = SIG_DFL;
      break;
    case 'c': /* catch: Try to recover from all segfaults */
      xsb_default_segfault_handler = xsb_segfault_catcher;
      break;
    default:
      xsb_warn("Request for unsupported type of segfault handling, %s", type);
      return TRUE;
    }
#ifdef SIGBUS
    signal(SIGBUS, xsb_default_segfault_handler);
#endif
    signal(SIGSEGV, xsb_default_segfault_handler);
    return TRUE;
  }

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

  case FORCE_TRUTH_VALUE: { /* +R1: AnsLeafPtr; +R2: TruthValue */
    BTNptr as_leaf = ptoc_addr(1);
    char *tmpstr = ptoc_string(2);
    if (!strcmp(tmpstr, "true"))
      force_answer_true(as_leaf);
    else if (!strcmp(tmpstr, "false"))
      force_answer_false(as_leaf);
    else xsb_abort("FORCE_TRUTH_VALUE: Argument 2 has unknown truth value");
    break;
  }

  case PUT_ATTRIBUTES: { /* R1: -Var; R2: +Atts */
    Cell attv = ptoc_tag(1);
    Cell atts = ptoc_tag(2);
    if (isref(attv)) {		/* attv is a free var */
      if (!isnil(atts)) {
	bind_attv((CPtr)attv, hreg);
	bld_free(hreg); hreg++;
	bld_copy(hreg, atts); hreg++;
      }
    }
    else if (isattv(attv)) {	/* attv is already an attv */
      if (isnil(atts)) {	/* change it back to normal var */
	bind_ref((CPtr)dec_addr(attv), hreg);
	bld_free(hreg); hreg++;
      }
      else {			/* update the atts (another copy) */
	bind_attv((CPtr)dec_addr(attv), hreg);
	bld_free(hreg); hreg++;
	bld_copy(hreg, atts); hreg++;
      }
    }
    else xsb_abort("PUT_ATTRIBUTES: Argument 1 is nonvar");
    break;
  }

  case GET_ATTRIBUTES: { /* R1: +Var; R2: -Vector; R3: -OldMask */
    Cell attv = ptoc_tag(1);
    if (isref(attv)) {		/* a free var */
      /* ctop_tag(2, makenil); */ /* keep it as a free var */
      ctop_tag(3, makeint(0));
    }
    else if (isattv(attv)) {
      CPtr vector;
      vector = (CPtr)dec_addr(attv) + 1;
      ctop_tag(2, cell(vector));
      ctop_tag(3, cell(clref_val(cell(vector)) + 1));
    }
    else xsb_abort("GET_ATTRIBUTES: Argument 1 is not an attributed variable");
    break;
  }

  case DELETE_ATTRIBUTES: { /* R1: -Var */
    Cell attv = ptoc_tag(1);
    if (isattv(attv)) {
      bind_ref((CPtr)dec_addr(attv), hreg);
      bld_free(hreg); hreg++;
    }
    break;

  }

  /*
   * attv_unify/1 is an internal builtin for binding an attv to a value
   * (it could be another attv or a nonvar term).  The users can call
   * this builtin in verify_attributes/2 to bind an attributed var
   * without triggering attv interrupt.
   */
  case ATTV_UNIFY: { /* R1: +Var; R2: +Value */
    Cell attv = ptoc_tag(1);
    bind_copy((CPtr)dec_addr(attv), ptoc_tag(2));
    break;
  }

  case SET_SCOPE_MARKER: {
    if (set_scope_marker()) return TRUE; else return FALSE;
    break;
  }
  case UNWIND_STACK: {
    if (unwind_stack()) return TRUE; else return FALSE;
    break;
  }
  case CLEAN_UP_BLOCK: {
    if (clean_up_block()) return TRUE; else return FALSE;
    break;
  }

  default:
    xsb_exit("Builtin #%d is not implemented", number);
    break;
  }
  return TRUE;
}

/*------------------------- end of builtin.c -----------------------------*/
