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
#include "debugs/debug_kostis.h"


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
#if defined(HAVE_SOCKET) || defined(HAVE_GETHOSTBYNAME)
#include <sys/socket.h>
#include <sys/uio.h>
#include <unistd.h> 
#include <netdb.h>
#include <netinet/in.h>
/* JF: NEW */
#include <arpa/inet.h>
#endif /* HAVE_SOCKET */
#endif /* WIN_NT */

#ifdef HAVE_SOCKET
/* socket macros */
#define SOCKET_ROOT        0
#define SOCKET_BIND        1
#define SOCKET_LISTEN      2
#define SOCKET_ACCEPT      3
#define SOCKET_CONNECT     4
#define SOCKET_FLUSH       5
#define SOCKET_CLOSE       6
#define SOCKET_RECV	   7
#define SOCKET_SEND	   8
#define SOCKET_SEND_EOF	   9
#define SOCKET_SEND_ASCI   10
#define SOCKET_GET0        11
#define SOCKET_PUT         12

#endif /* HAVE_SOCKET */

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

/*======================================================================*/

/* For tip hacking */
#define CALL_NEXT_TIP 0
#define CALL_HASH_ADDR 1
#define CALL_HASH_SIZE 2
#define RET_HASH_SIZE 3
#define FIRST_TIP 4

/*======================================================================*/

#define DELETED_SET 1

#ifdef HAVE_SOCKET
#ifdef WIN_NT
extern FILE *_fdopen(int handle, const char *mode);
#else
extern FILE *fdopen(int fildes, const char *type);
#endif
#endif

extern tab_inf_ptr get_tip(Psc);
extern tab_inf_ptr first_tip;
extern tab_inf_ptr last_tip;

extern int  sys_syscall(int);
extern int  buff_copyterm(Cell, pb, long, long, long);
extern bool fmt_read(void), fmt_write(void), fmt_write_string(void),
  read_canonical(void), file_read_line(void);
extern bool assert_code_to_buff(void), assert_buff_to_clref(void),
  gen_retract_all(void), compiled_to_dynamic(void), db_retract0(void),
  db_get_clause(void), db_build_prref(void), db_remove_prref(void);

extern char *dirname_canonic(char *);
extern char *expand_filename(char *filename);
extern char *tilde_expand_filename(char *filename);
extern bool is_absolute_filename(char *filename);
extern void parse_filename(char *filenam, char **dir, char **base, char **ext);

extern int  findall_init(void), findall_add(void), findall_get_solutions(void);
extern int  copy_term(void);

#if (defined(DEBUG) && defined(DEBUG_DELAY))
extern void print_delay_list(FILE *, CPtr);
extern void print_subgoal(FILE *, SGFrame);
#endif


/* ------- definitions of procedures used in "builtin_call" -----------	*/

static int  fast_ground(CPtr);
static void abolish_table_info(void);
static void get_subgoal_ptr(Cell, int, CPtr);
static void write_quotedname(FILE *, char *);

#ifdef DEBUG
static void print_predicate_table(char *, int, tab_inf_ptr);
extern void printterm(Cell, byte, int);
#endif

#ifdef PROFILE
static void write_out_profile(void);
#endif

/* ------- variables also used in other parts of the system -----------	*/

Cell flags[64];			  /* System flags + user flags */
FILE *open_files[MAX_OPEN_FILES]; /* open file table */
extern char *install_dir;    	  /* from self_orientation.c */
extern char *user_home;    	  /* from self_orientation.c */

/* ------- working variables for the procedure "builtin_call" ---------	*/

static FILE* fptr;			/* working variable */
static Float float_temp;		/* working variable */
static STRFILE *sfptr;

static struct stat stat_buff;

/* ------- utility for sockets ---------------------------------------- */

#ifdef HAVE_SOCKET
#ifdef WIN_NT
int readmsg(SOCKET sockfd, char *buff, int maxbuff)
{
  int n, rc;
  char c;

  for (n=1; n < maxbuff; n++) {
    rc = recv(sockfd, &c, 1, 0);
    if (rc == 1) {
	        
      if (c == '`') {
	break;
      } else if (c == EOF) {
	return (-2);
      } else {
	*buff++=c;
      }
    } else if (rc == 0) {
      if (n == 1) {
	return(0);    
      } else {
	break;         
      }
    } else  {
      return (-1); 
    }
  }
  *buff = 0;
  return (n);
}
#endif
#endif


/* ------- utility routines -------------------------------------------	*/

/*
 *  Returns the still-tagged value (a Cell) at the end of the deref chain
 *  leading from `regnum'.
 */
Cell ptoc_tag(int regnum)
{
  register Cell addr = cell(reg+regnum);

  deref(addr);
  return addr;
}


DllExport prolog_int call_conv ptoc_int(int regnum)
{
  register Cell addr = cell(reg+regnum);

  /* deref and then check the type */
  deref(addr);
  switch (cell_tag(addr)) {
  case FREE:
  case REF1: 
  case CS:
  case LIST:
  case FLOAT: fprintf(stderr, "Wrong arg in ptoc_int\n");
    return 0;
  case STRING: return (prolog_int)string_val(addr);	/* dsw */
  case INT: return int_val(addr);
  default: fprintf(stderr, "Argument with unknown tag in ptoc_int\n");
    return 0;
  }
}

DllExport prolog_float call_conv ptoc_float(int regnum)
{
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
    fprintf(stderr, "Wrong arg in ptoc_float\n"); return 0.0;
  case FLOAT: return (prolog_float)float_val(addr);
  default: fprintf(stderr, "Argument with unknown tag in ptoc_float\n");
    return 0.0;
  }
}

DllExport char* call_conv ptoc_string(int regnum)
{
  register Cell addr = cell(reg+regnum);
  
  /* deref and then check the type */
  deref(addr);
  switch (cell_tag(addr)) {
  case FREE:
  case REF1: 
  case CS:  
  case LIST:
  case FLOAT:
    xsb_abort("ptoc_string; Nonstring used where string is required");
    return ""; 
  case INT: return (char *)int_val(addr);
  case STRING: return string_val(addr); 
  default: fprintf(stderr, "Argument with unknown tag in ptoc_string\n");
    return "";
  }
}


/*
 *  For decoding pointers to objects, currently PSC and PSC-PAIR records.
 */
#define ptoc_addr(regnum)	ptoc_int(regnum)


/*
 *  Deref's the variable of register `regnum', trails the binding,
 *  creates an INT Cell containing `value', and binds the variable to it.
 */
DllExport void call_conv ctop_int(int regnum, prolog_int value)	/* from int value form an int node */
{
  register Cell addr = cell(reg+regnum);
  
  deref(addr);
  if (isref(addr)) {
    bind_int(vptr(addr), value);
  }
  else
    fprintf(stderr, "Wrong arg in ctop_int %lx (Reg = %d)\n", addr, regnum);
}


DllExport void call_conv ctop_float(int regnum, prolog_float value) /* from float value form an int node */
{
  register Cell addr = cell(reg+regnum);

  deref(addr);
  if (isref(addr)) {
    bind_float(vptr(addr), value);
  }
  else fprintf(stderr, "Wrong arg in ctop_float %lux\n", addr);
}

DllExport void call_conv ctop_string(int regnum, char *value) /* from string form a string node */
{
  register Cell addr = cell(reg+regnum);

  deref(addr);
  if (isref(addr)) {
    bind_string(vptr(addr), value);
  }
  else fprintf(stderr, "Wrong arg in ctop_string %lux\n", addr);
}

void ctop_ref(int regnum, CPtr value)  /* from address form a reference node */
{
  register Cell addr = cell(reg+regnum);

  deref(addr);
  if (isref(addr)) {
    bind_ref(vptr(addr), value);
  }
  else fprintf(stderr, "Wrong arg in ctop_ref %lux\n", addr);
}

void ctop_constr(int regnum, Pair psc_pair)
{				/* from psc_pair ptr form an constr node */
  register Cell addr = cell(reg+regnum);

  deref(addr);
  if (isref(addr)) {
    bind_cs(vptr(addr), psc_pair);
  }
  else fprintf(stderr, "Wrong arg in ctop_constr %lux\n", addr);
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
  else fprintf(stderr, "Wrong arg in ctop_tag %lux\n", addr);
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

static void fprint_variable(FILE *fptr, CPtr var)
{
  if (var >= (CPtr)glstack.low && var <= top_of_heap)
    fprintf(fptr, "_h%ld", ((Cell)var-(Cell)glstack.low+1)/sizeof(CPtr));
  else {
    if (var >= top_of_localstk && var <= (CPtr)glstack.high)
      fprintf(fptr, "_l%ld", ((Cell)glstack.high-(Cell)var+1)/sizeof(CPtr));
    else fprintf(fptr, "_%p", var);   /* Should never happen */
  }
}

/* --------------------------------------------------------------------	*/

STRFILE *iostrs[MAXIOSTRS] = {NULL,NULL,NULL,NULL,NULL};

static FILE *stropen(char *str)
{
  int i;
  STRFILE *tmp;

  for (i=0; i<MAXIOSTRS; i++) {
    if (iostrs[i] == NULL) break;
  }
  if (i>=MAXIOSTRS) return 0;
  tmp  = (STRFILE *)mem_alloc(sizeof(STRFILE));
  iostrs[i] = tmp;
  tmp->strcnt = strlen(str);
  tmp->strptr = str;
  tmp->strbase = str;
  return (FILE *)iostrdecode(i);
}

static void strclose(int i)
{
  i = iostrdecode(i);
  mem_dealloc((byte *)iostrs[i],sizeof(STRFILE));
  iostrs[i] = NULL;
}

/* --- built in predicates --------------------------------------------	*/

int builtin_call(byte number)
{
  CPtr var, reg_base;
  char message[80];
  char *addr, *tmpstr;
  int value, i, disp, arity, tmpval;
  long c; int new_indicator, len;	/* for standard preds */
  
  Cell heap_addr, term, term2, index;	/* used in standard preds */
  Cell functor, list, new_list;		/* for standard preds */
  CPtr head, top = 0;			/* for standard preds */
  char str[256];
  char *name;				/* for standard preds */
  char hack_char;			/* for standard preds */
  Cell *cell_tbl;			/* for standard preds */
  
  DL dl;				/* for residual program */
  DE de;				/* for residual program */
  NODEptr as_leaf;			/* for residual program */
  Cell delay_lists;			/* for residual program */
  CPtr dls_head, dls_tail = NULL;	/* for residual program */
  tab_inf_ptr tip;
  Psc psc;
  struct psc_pair *sym;
  CPtr subgoal_ptr, t_ptcp;
  register CPtr xtemp1, xtemp2;
#ifdef FOREIGN
  static int (*proc_ptr)(void);		/* working variable */
#endif
#ifdef HAVE_SOCKET
  FILE *sockptr;
  struct hostent *hostptr;
  int rc, domain, portnum;
  char ch;
#ifdef WIN_NT
  char Endtxt=3;
  SOCKET sockfd, sockfd_in;
  int  err, in;
  SOCKADDR_IN localAddr;
  SOCKADDR_IN remoteAddr;
  char *sock_msg, ci, last[1];
#else
  int  sockfd, sockfd_in;
  struct sockaddr_in socket_addr;
#endif
#endif /* HAVE_SOCKET */
  
  switch (number) {
  case PSC_NAME:		/* reg 1: +PSC; reg 2: -String */
    psc = (Psc)ptoc_addr(1);
    ctop_string(2, get_name(psc));
    break;
  case PSC_ARITY:		/* reg 1: +PSC; reg 2: -int */
    psc = (Psc)ptoc_addr(1);
    ctop_int(2, (Integer)get_arity(psc));
    break;
  case PSC_TYPE:		/* reg 1: +PSC; reg 2: -int */
				/* type: see psc.h, `entry_type' field defs */
    psc = (Psc)ptoc_addr(1);
    ctop_int(2, (Integer)get_type(psc));
    break;
  case PSC_SET_TYPE:	/* reg 1: +PSC; reg 2: +type (int): see psc.h */
    psc = (Psc)ptoc_addr(1);
    set_type(psc, ptoc_int(2));
    break;
  case PSC_PROP:		/* reg 1: +PSC reg 2: -term */
				/* prop: as a buffer pointer */
    psc = (Psc)ptoc_addr(1);
    if (get_type(psc)==T_ALIA) ctop_tag(2, (Cell)get_ep(psc));
    else ctop_int(2, (Integer)get_ep(psc));
    break;
  case PSC_SET_PROP:	        /* reg 1: +PSC; reg 2: int */
    psc = (Psc)ptoc_addr(1);
    if (get_type(psc)==T_ALIA) set_ep(psc, (pb)ptoc_tag(2));
    else set_ep(psc, (pb)ptoc_int(2));
    break;
  case FILE_OPEN:		/* r1: file name (+string);   */
				/* r2: mode (+int); r3: -file */
				/* When read, mode=0; when write, mode = 1, 
				   when append, mode = 2, when opening a 
				   string for read mode = 3 */
    tmpstr = ptoc_string(1);
    tmpval = ptoc_int(2);
    if (tmpval<3) {
      addr = expand_filename(tmpstr);
      switch (tmpval) {
	/* "b"'s needed for DOS. -smd */
      case 0: fptr = fopen(addr, "rb"); break; /* READ_MODE */
      case 1: fptr = fopen(addr, "wb"); break; /* WRITE_MODE */
      case 2: fptr = fopen(addr, "ab"); break; /* APPEND_MODE */
      }
      if (fptr) {
	if (!stat(addr, &stat_buff) && !S_ISDIR(stat_buff.st_mode)) {
	  /* file exists and isn't a dir */
	  for (i=3; i < MAX_OPEN_FILES && open_files[i] != NULL; i++) ;
	  if (i == MAX_OPEN_FILES) xsb_abort("Too many open files");
	  else {
	    open_files[i] = fptr;
	    ctop_int(3, i);
	  }
	} else {
	  sprintf(message,
		  "File %s is a directory, cannot open!", tmpstr);
	  xsb_abort(message);
	}
      } else ctop_int(3, -1);
    } else if (tmpval==3) {  /* open string! */
      if ((fptr = stropen(tmpstr))) ctop_int(3, (Integer)fptr);
      else ctop_int(3, -1000);
    } else {
      fprintf(stderr,"Unknown open file mode\n");
      ctop_int(3, -1000);
    }
    /* fprintf(stderr,"returning fptr: %d\n",ptoc_int(3)); */
    break;
  case FILE_CLOSE:	/* r1: +file */
    tmpval = ptoc_int(1);
    if (tmpval < 0) strclose(tmpval);
    else {
      fclose(fileptr(tmpval));
      open_files[tmpval] = NULL;
    }
    break;
  case FILE_GET:		/* r1: +file; r2: char (-int) */
    tmpval = ptoc_int(1);
    if ((tmpval < 0) && (tmpval >= -MAXIOSTRS)) {
      sfptr = strfileptr(tmpval);
      ctop_int(2, strgetc(sfptr));
    }
    else ctop_int(2, getc(fileptr(tmpval)));
    break;
  case FILE_PUT:  /* r1: +file; r2: char (+int) */
    tmpval = ptoc_int(1); i = ptoc_int(2); fptr = fileptr(tmpval);
    putc(i, fptr);
#ifdef WIN_NT
    if (tmpval==2 && i==10) fflush(fptr); /* hack for Java interface */
#endif
    break;
  case TERM_PSC:		/* r1: +term; r2: -PSC */
    /* Assumes that `term' is a CS-tagged Cell. */
    ctop_addr(2, get_str_psc(ptoc_tag(1)));
    break;
  case TERM_TYPE:		/* r1: +term; r2: tag (-int)		*/
				/* <0 - var, 1 - cs, 2 - int, 3 - list>	*/
    term = ptoc_tag(1);
    if (!isnonvar(term)) ctop_int(2, 0);
    else ctop_int(2, cell_tag(term));
    break;
  case TERM_COMPARE:	/* r1, r2: +term; rs: res (-int) */
    ctop_int(3, compare(ptoc_tag(1), ptoc_tag(2)));
    break;
  case TERM_NEW:		/* r1: +PSC, r2: -term */
    psc = (Psc)ptoc_addr(1);
    sreg = hreg;
    hreg += get_arity(psc) + 1;
    ctop_constr(2, (Pair)sreg);
    new_heap_functor(sreg, psc);
    for (disp=0; disp < (int)get_arity(psc); sreg++,disp++) {
      bld_free(sreg);
    }
    break;
  case TERM_ARG:		/* r1: +term; r2: index (+int); */
				/* r3: arg (-term) */
    disp = ptoc_int(2);
    term = ptoc_tag(1);
    ctop_tag(3, cell(clref_val(term)+disp));
    break;
  case TERM_SET_ARG:	/* r1: +term; r2: index (+int) */
				/* r3: newarg (+term); r4: +perm(not used) */
    /* used in file_read.P, array.P, array1.P */
    disp = ptoc_int(2);
    term = ptoc_tag(1);
    if (!ptoc_int(4)) { pushtrail(clref_val(term)+disp,cell(reg+3));}
    bld_copy0(clref_val(term)+disp, cell(reg+3));
    break;
  case STAT_FLAG:		/* R1: flagname(+int); R2: value(-int) */
    ctop_int(2, flags[ptoc_int(1)]);
    break;
  case STAT_SET_FLAG:	/* R1: flagname(+int); R2: value(+int); */
				/* R3: +Perm (ignored) */
    flags[ptoc_int(1)] = ptoc_int(2);
    call_intercept = flags[DEBUG_ON]|flags[TRACE_STA]|flags[HITRACE]
      |flags[CLAUSE_INT];
    break;
  case BUFF_ALLOC:	/* r1: size (+integer); r2: -buffer; */
	           /* the length of the buffer is also stored at position 0 */
    value = ((ptoc_int(1)+7)>>3)<<3;
    value *= ZOOM_FACTOR ;
    addr = (char *)mem_alloc(value);
    value /= ZOOM_FACTOR ;
    *(Integer *)addr = value;	/* store buffer size at buf[0] */
    ctop_int(2, (Integer)addr);	/* use "integer" type now! */
    break;
  case BUFF_DEALLOC:	/* r1: +buffer; r2: +oldsize; r3: +newsize; */
    addr = ptoc_string(1);
    disp = ((ptoc_int(2)+7)>>3)<<3;
    disp *= ZOOM_FACTOR ;
    value = ((ptoc_int(3)+7)>>3)<<3;	/* alignment */
    value *= ZOOM_FACTOR ;
    if (value > disp) {
      fprintf(stderr, "New Buffer Size Cannot exceed the old one!!\n");
      break;
    }
    mem_dealloc((byte *)(addr+value), disp-value);
    break;
  case BUFF_WORD:		/* R1: +buffer; r2: displacement(+integer); */
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
  case BUFF_BYTE:		/* R1: +buffer; r2: displacement(+integer);*/
				/* R3: value (-integer) */
    addr = ptoc_string(1);
    disp = ptoc_int(2);
    ctop_int(3, (Integer)(*(byte *)(addr+disp)));
    break;
  case BUFF_SET_BYTE:	/* R1: +buffer; r2: displacement(+integer);*/
				/* r3: value (+integer) */
    addr = ptoc_string(1);
    disp = ptoc_int(2);
    *(pb)(addr+disp) = ptoc_int(3);
    break;
  case BUFF_CELL:	/* R1: +buffer; r2: displacement(+integer);*/
    /* r3: -Cell at that location */
    addr = ptoc_string(1);
    disp = ptoc_int(2);
    disp *= ZOOM_FACTOR ;
    ctop_tag(3, (Cell)(addr+disp));
    break;
  case BUFF_SET_CELL:	/* R1: +buffer; r2: displacement(+integer);*/
				/* r3: type (+integer); r4: +term */
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
      fprintf(stderr,
	      "Type %d is not implemented by buff_set_cell\n",
	      value);
    }
    break;
  case BUFF_SET_VAR:
    /* This procedure is used in copyterm and make an external 
       variable pointing to the buffer. The linkage inside the
       buffer will not be trailed so remains after backtracking. */
    /* R1: +buffer; R2: +disp; */
    /* R3: +buffer length; R4: External var */
    addr = ptoc_string(1);
    disp = ptoc_int(2);
    disp *= ZOOM_FACTOR ;
    term = ptoc_tag(4);
    bld_free(vptr(addr+disp));
    if ((Cell)term < (Cell)addr || 
	(Cell)term > (Cell)addr+ptoc_int(3)) {
				/* var not in buffer, trail */
      bind_ref(vptr(term), (CPtr)(addr+disp));
    } else {		/* already in buffer */
      bld_ref(vptr(term), (CPtr)(addr+disp));	
    }
    break;
    
  case COPY_TERM: /* R1: +term to copy; R2: -variant */
    copy_term() ;
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
      proc_ptr();
      pcreg = cpreg;  /* always "proceed" -- unless somebody aborts/exits */
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
  case STR_INSERT:	/* R1: +Substring; R2: -OutString */
    ctop_string(2, string_find(ptoc_string(1), 1));
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

  case PSC_IMPORT:      /* R1: +String, Predicate name to be imported
			   R2: +Arity
			   R3: +String, Module name where pred lives  */
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
  case FILE_GETBUF:	/* R1: +file; R2: +byte count (int) */
				/* R3: +buff (int); R4: +offset */
    addr = ptoc_string(3);
    disp = ptoc_int(4);
    tmpval = ptoc_int(1);
    fread(addr+disp, 1, ptoc_int(2), fileptr(tmpval));
    break;
  case FILE_PUTBUF:	/* r1: +file; r2: +byte count (int) */
				/* r3: +buff (int); r4: +offset */
    addr = ptoc_string(3);
    disp = ptoc_int(4);
    tmpval = ptoc_int(1);
    fwrite(addr+disp, 1, ptoc_int(2), fileptr(tmpval));
    break;
  case FILE_GETTOKEN:     /* R1: +File, R2: +PrevCh, R3: -Type; */
                                /* R4: -Value, R5: -NextCh */
    tmpval = ptoc_int(1);
    if ((tmpval < 0) && (tmpval >= -MAXIOSTRS))
      token = GetToken(NULL,strfileptr(tmpval), ptoc_int(2));
    else token = GetToken(fileptr(tmpval), NULL, ptoc_int(2));
    if (token->type == TK_ERROR) {
      pcreg = (pb)&fail_inst;
    }
    else {
      ctop_int(3, token->type);
      ctop_int(5, token->nextch);
      switch (token->type)     /* Modified for HiLog. (KFS) */
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
    fptr = fileptr(tmpval);
    switch (ptoc_int(2)) {
    case FREE   : var = (CPtr)ptoc_tag(3);
      fprint_variable(fptr, var);
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
    default : xsb_abort("Unknown token type in file_puttoken");
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
    xsb_abort("Loading foreign object files is not available for this machine");
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
      return 0;
    else
      ctop_string(2, string_find(env,1));
    break;
  }
  case SYS_SYSCALL:	/* R1: +int (call #, see <syscall.h> */
				/* R2: -int, returned value */
	    			/* R3, ...: Arguments */
    ctop_int(2, sys_syscall(ptoc_int(1)));
    break;
  case SYS_SYSTEM:	/* R1: +String (of command), R2: -Int (res) */
    ctop_int(2, system(ptoc_string(1)));
    break;
  case SYS_GETHOST:
    /* +R1: a string indicating the host name  */
    /* +R2: a buffer (of length 16) for returned structure */
#ifdef HAVE_GETHOSTBYNAME
    hostptr = gethostbyname(ptoc_string(1));
    bcopy(hostptr->h_addr, ptoc_string(2), hostptr->h_length);
#else
    xsb_abort("sys_gethost is not available for this configuration");
#endif
    break;
  case SYS_ERRNO:			/* R1: -Int (errno) */
    ctop_int(1, errno);
    break;
  case FILE_STAT: {	    	/* R1: +FileName (it should exist!!), */
				/* R2: +int 0->ModTime, 1->FileSize*/
				/* R3: -int (ModTime or FileSize)*/
    int retcode = stat(ptoc_string(1), &stat_buff);
    int functor_arg3 = is_functor(reg_term(3));

    switch (ptoc_int(2)) {
    case 0:
      /* This is DSW's hack to get 32 bit time values.
	 The idea is to call this builtin as file_time(File,time(T1,T2))
	 where T1 represents the most significant 8 bits and T2 represents
	 the least significant 24.
	 ***This probably breaks 64 bit systems, so David will look into it!
      */
      if (!retcode && functor_arg3) {
	/* file exists & arg3 is a term, return 2 words*/
	c2p_int(0xFFFFFF & stat_buff.st_mtime,p2p_arg(reg_term(3),2));
	c2p_int(stat_buff.st_mtime >> 24,p2p_arg(reg_term(3),1));
      } else if (!retcode) {
	/* file exists, arg3 non-functor:  issue an error */
	xsb_warn("Arg 2 in file_time must be a term: time(X,Y)");
	xsb_warn("Passing a variable in Arg 2 is unsafe - significant digits might be lost");
	ctop_int(3, (0x7FFFFFF & stat_buff.st_mtime));
      } else if (functor_arg3) {
	/* no file, and arg3 is functor: return two 0's */
	c2p_int(0, p2p_arg(reg_term(3),2));
	c2p_int(0, p2p_arg(reg_term(3),1));
      } else {
	/* no file, no functor: return 0 */
	xsb_warn("Arg 2 in file_time must be a term: time(X,Y)");
	xsb_warn("Passing a variable in Arg 2 is unsafe - significant digits might be lost");
	ctop_int(3, 0);
      }
      break;
    case 1: /* Take file size in 4-byte words */
      /*** NOTE: File_size can handle only files up to 128K.
	   We must use the same trick here as we did with file_time above */
      if (!retcode)
	/* file exists */
	ctop_int(3, (0x7FFFFFF & (stat_buff.st_size >> 2)));
      else /* no file */
	ctop_int(3, 0);
      break;
    }
    break;
  }
  case FILE_WRITEQUOTED:
    tmpval = ptoc_int(1);
    write_quotedname(fileptr(tmpval),ptoc_string(2));
    break;
  case FAST_GROUND:
    return fast_ground((CPtr)ptoc_tag(1));

  case FILE_POS:
    /* r1: +file */
    /* r2: -int  */
    tmpval = ptoc_int(1);  /* expand for reading from strings?? */
    term = ptoc_tag(2);
    if (tmpval >= 0) {
      if (isnonvar(term)) return ptoc_int(2) == ftell(fileptr(tmpval));
      else ctop_int(2, ftell(fileptr(tmpval)));
    } else { /* reading from string */
      sfptr = strfileptr(tmpval);
      disp = sfptr->strptr - sfptr->strbase;
      if (isnonvar(term)) return ptoc_int(2) == disp;
      else ctop_int(2, disp);
    }
    break;
    
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
  case TIP_PROP: /*reg1: +TIP; reg2: +field; reg3: +get/set; reg4: ?val*/
    tip = (tab_inf_ptr) ptoc_addr(1);
    disp = ptoc_int(3);
    switch (ptoc_int(2)) {
    case CALL_NEXT_TIP:
      if (disp == 1) ti_next_tip(tip) = (CPtr) ptoc_int(4);
      else if (disp == 0) ctop_int(4, (Integer)ti_next_tip(tip));
      break;
    case CALL_HASH_ADDR:
      if (disp == 1) ti_call_trie_root(tip) = (CPtr) ptoc_int(4);
      else if (disp == 0) ctop_int(4, (Integer)ti_call_trie_root(tip));
      break;
    case CALL_HASH_SIZE: case RET_HASH_SIZE:
      fprintf(stderr, "! CHS and RHS are meaningless for tries\n");
      break;
    case FIRST_TIP:
      if (disp == 1) { /* add new tip at END of list of tips */
	tip = (tab_inf_ptr) ptoc_int(4);
	if (first_tip == 0) first_tip = tip;
	else ti_next_tip(last_tip) = (CPtr) tip;
	last_tip = tip;
      }
      else if (disp == 0) ctop_int(4, (Integer)first_tip);
      break;
    }
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
      return 0;	/* fail */
    }
    arity = get_arity(psc);
    tip = get_tip(psc);
    if (tip == NULL) {
      sprintf(message, "Predicate %s/%d is not tabled",
	      get_name(psc), arity);
      xsb_abort(message);
      return 0;
    }
    subgoal_ptr = ti_call_trie_root(tip);
    get_subgoal_ptr(term, arity, (CPtr)&subgoal_ptr);
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
    arity = get_arity(psc);
    tip = get_tip(psc);
    subgoal_ptr = ti_call_trie_root(tip);
    get_subgoal_ptr(term, arity, (CPtr)&subgoal_ptr);
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
    xsb_abort("existential negation is not supported...");
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
  case PRINT_PREDICATE_TABLE:		/* reg 1: +PSC;*/
#ifdef DEBUG
    psc = (Psc)ptoc_addr(1);
    if ((Integer) get_tip(psc) == 0) 
      printf("%s/%d is not tabled\n", get_name(psc), get_arity(psc));
    else {
      print_predicate_table(get_name(psc), get_arity(psc), get_tip(psc));
    }
#else
    xsb_abort("print_predicate_table is only available in debug mode");
#endif
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
  case FMT_WRITE:
    return fmt_write();
  case SLASH_BUILTIN: {  /* R1: -Slash. Tells what kind of slash the OS uses */
    static char slash_string[2];
    slash_string[0] = SLASH;
    slash_string[1] = '\0';
    ctop_string(1, string_find(slash_string, 1));
    break;
  }
  case FMT_WRITE_STRING:
    return fmt_write_string();
  case FILE_READ_LINE:
    return file_read_line();
  case FMT_READ:
    return fmt_read();
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

/*----------------------------------------------------------------------*/

#include "std_pred.i"

#ifdef ORACLE
#include "oracle.i"
#endif

#ifdef XSB_ODBC
#include "xsb_odbc.i"
#endif

/*----------------------------------------------------------------------*/

  case TRIE_NODE_ELEMENT:
    trie_node_element();
    break;
  case PROLOG_NEWNODE:
    prolog_newnode();
    break;
  case TABLE_STATUS:  /* reg1: +term; reg2: -status (int) */
    term = ptoc_tag(1);
    if ((psc = term_psc(term)) == NULL) {
      err_handle(TYPE, 1, "table_status", 2, "callable term", term);
      return 0;	/* fail */
    }
    tip = get_tip(psc);
    if (tip == NULL) {
      value = 0; /* undef */
    } else {
      arity = get_arity(psc);
      subgoal_ptr = ti_call_trie_root(tip);
      get_subgoal_ptr(term, arity, (CPtr)&subgoal_ptr);
      if (subgoal_ptr == NULL) {
	value = 1; /* no_call_yet */
      } else {
	value = (is_completed(subgoal_ptr)) ?  2 : 3;
      }
    }
    ctop_int(2, value);
    break;
  case DELETE_PREDICATE_TABLE:
    delete_predicate_table();
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
      delete_branch((NODEptr)ptoc_int(1),(CPtr)ptoc_int(2)); 
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
  case GET_EMU_DEPENDENT_CONST:	/* r1: +name; r2: -int */
    tmpstr = ptoc_string(1);
    if (!strcmp(tmpstr, "escape")) ctop_int(2, 0);
    else {
      xsb_abort("Unknown first arg in get_emu_dependent_const/2");
      return FALSE;
    }
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
  case BOTTOM_UP_UNIFY:
    bottom_up_unify();
    break;
  case DELETE_TRIE:
    if (strcmp(ptoc_string(2),"intern") == 0){
      switch_to_trie_assert;
      tmpval = ptoc_int(1);
      /*
       * We can only delete a valid NODEptr, so that only those sets
       * that were used before can be put into the free set list.
       */
      if ((Set_ArrayPtr[tmpval] != NULL) &&
	  (!((int) Set_ArrayPtr[tmpval] & 0x3))) {
	delete_trie(Set_ArrayPtr[tmpval]);
	switch_from_trie_assert;
        /*
	 * Save the value of first_free_set into Set_ArrayPtr[tmpval].
	 * Some simple encoding is needed, because in trie_interned/4 we
	 * have to know this set is already deleted.
	 */
	Set_ArrayPtr[tmpval] = (NODEptr) (first_free_set << 2 | DELETED_SET);
	first_free_set = tmpval;
      }
    }
    else{
      xsb_abort("Unknown Usage in intern:delete_trie/2\n");
    }
    break;
	    
  case PRINT_LS: print_ls(0) ; return TRUE ;
  case PRINT_TR: print_tr(0) ; return TRUE ;
  case PRINT_HEAP: print_heap(0,2000,0) ; return TRUE ;
  case PRINT_CP: print_cp(0) ; return TRUE ;
  case PRINT_REGS: print_regs(10,0) ; return TRUE ;
  case PRINT_ALL_STACKS: print_all_stacks() ; return TRUE ;
  case EXP_HEAP: glstack_realloc(glstack.size + 1,0) ; return TRUE ;
  case MARK_HEAP: mark_heap(ptoc_int(1),0,0) ; return TRUE ;
  case GC_HEAP: return(gc_heap(0)) ;

  case FINDALL_INIT: return(findall_init()) ;
  case FINDALL_ADD: return(findall_add()) ;
  case FINDALL_GET_SOLS: return(findall_get_solutions()) ;

#ifdef HAVE_SOCKET
#ifdef WIN_NT
    /* in order to save builtin numbers, create a single
     * socket function with options 
     * socket_request(SockOperation,....)
     */
  case SOCKET_REQUEST: {
    switch (ptoc_int(1)) {
    case SOCKET_ROOT: /* socket_request(0,+domain,-socket_fd) */
      /* jf: for now only support AF_INET */
      domain = ptoc_int(2); 
      if (domain == 0) domain = AF_INET;
      else if (domain == 1){
	/* domain = AF_UNIX; */
	domain = AF_INET;
	fprintf(stderr, "default domain is AF_INET.\n");
      }
      else  {
	fprintf(stderr, "Invalid domain value. 0 - AF_INET, \
1 - AF_UNIX.\n");           
	return FALSE;
      }

      sockfd = socket(domain, SOCK_STREAM, 0);
      if (sockfd == INVALID_SOCKET)
	{
	  fprintf(stderr, "Cannot open stream socket.\n");
	  return FALSE;
	}

		  
      ctop_int(3, (int) sockfd);
      break;
    case SOCKET_BIND: /* socket_request(1,+domain,+sockfd,+port) */
      /* jf: for now only support AF_INET, ignore param */
      /* domain = ptoc_int(2); */
      sockfd = (SOCKET) ptoc_int(3);
      portnum = ptoc_int(4);

      //
      // Bind our server to the agreed upon port number.  See
      // commdef.h for the actual port number.
      //
      ZeroMemory (&localAddr, sizeof (localAddr));
      localAddr.sin_port = htons (portnum);
      localAddr.sin_family = AF_INET;

	      
      /*socket_addr.sin_addr.s_addr = htonl(INADDR_ANY);*/
      err = bind (sockfd, (PSOCKADDR) & localAddr, sizeof (localAddr));
      if (err == SOCKET_ERROR)
	{
	  fprintf (stdout, "Socket Bind Failed\n");
	  return FALSE;
	}
	      
      break;
    case SOCKET_LISTEN:  /* socket_request(2,+sockfd,+length) */
      sockfd = (SOCKET) ptoc_int(2);
      err = listen (sockfd, ptoc_int(3));
      if (err == SOCKET_ERROR)
	{
	  fprintf (stdout, "Socket Listen Failed\n");
	  return FALSE;
	}

      break;
    case SOCKET_ACCEPT: { /* socket_request(3,+sockfd,+sockfptr) */
      sockfd_in = (SOCKET) ptoc_int(2);

      sockfd = accept (sockfd_in, NULL, NULL);
      if (sockfd == INVALID_SOCKET)
	{
	  fprintf (stdout, "Accept Failed\n");
	  return FALSE;
	}
	
      ctop_int(3, (int) sockfd);
	      
      break;
    }
    case SOCKET_CONNECT: {
      /* socket_request(4,+domain,+sockfd,+port,+hostname,-sockfptr) 
       * jf: domain is ignored for now
       */
      int con;

      sockfd = (SOCKET) ptoc_int(3);
      portnum = ptoc_int(4);

      ZeroMemory (&remoteAddr, sizeof (remoteAddr));

      remoteAddr.sin_family = AF_INET;
      remoteAddr.sin_port = htons(portnum);
      remoteAddr.sin_addr.s_addr = inet_addr(ptoc_string(5));

      err = connect (sockfd, (PSOCKADDR) & remoteAddr, sizeof (remoteAddr));
      if (err == SOCKET_ERROR)
	{
	  printf ("DoEcho: connect failed: %ld\n", GetLastError ());
	  closesocket (sockfd);
	  return FALSE;
	}
	      
      ctop_int(6, 0);
	      
      break;
    }
    case SOCKET_FLUSH: 	/* socket_request(5,+sockfd) */
      tmpval = ptoc_int(2);
      fptr = fileptr(tmpval);   
      fflush(fptr);
      break;
    case SOCKET_CLOSE:	/* socket_request(6,+sockfd) */
      closesocket((SOCKET)ptoc_int(2));
      break;
    case SOCKET_RECV:
      sockfd = (SOCKET) ptoc_int(2);
      sock_msg = calloc(1024, sizeof(char));
      rc = readmsg(sockfd, sock_msg,1024);
      ctop_string(3, (char*) string_find((char*) sock_msg,1));
      free(sock_msg);
      break;
    case SOCKET_SEND:
      sockfd = (SOCKET) ptoc_int(2);
      sock_msg = calloc(1024, sizeof(char));
      strcpy((char*) sock_msg, (char*) ptoc_string(3));
      send(sockfd, sock_msg, strlen(sock_msg), 0);
      send(sockfd, "`", strlen("`"), 0);
      /*send(sockfd, "\n", strlen("\n"),0),*/
      free(sock_msg);
      break;
    case SOCKET_SEND_ASCI:
      sockfd = (SOCKET) ptoc_int(2);
      rc = ptoc_int(3);
      sock_msg = calloc(1024, sizeof(char));
      ci = (char) rc;
      sprintf(sock_msg,"%c",ci);
      /*printf("XSB2: str:%s.\n", sock_msg);*/
      send(sockfd, sock_msg, strlen(sock_msg), 0);
      send(sockfd, "`", strlen("`"), 0);
      /*send(sockfd, "\n", strlen("\n"),0);*/
      free(sock_msg);
      break;
    case SOCKET_SEND_EOF:
      sockfd = (SOCKET) ptoc_int(2);
      last[0] = EOF;
      send(sockfd, last, 1, 0);
      send(sockfd, "`", strlen("`"),0);
      break;
    case SOCKET_GET0: /* socket_request(11,+Sockfd,-C,-Error,_,_) */
      sockfd = (SOCKET) ptoc_int(2);
      /*JPS: rc = readmsg(socketfd, &ch, 1);*/
      rc = recv (sockfd,&ch,1,0);
      if (rc == 1)
	ctop_int(3,(unsigned char)ch);
      else { ctop_int(3,-1); ctop_int(4,WSAGetLastError());}
      break;
    case SOCKET_PUT: /* socket_request(12,+Sockfd,+C,_,_,_) */
      { /* We should fail on error...*/
	static char tmpch[4];
	sockfd = (SOCKET) ptoc_int(2);
	/* JPS: sprintf(tmpch,"%c",ptoc_int(3));*/
	tmpch[0] = (char)ptoc_int(3);
	send(sockfd, tmpch, 1, 0);
      }
      break;

    default:
      fprintf(stderr, "Invalid socket request %d\n",ptoc_int(1));
      return FALSE;
    }
    break;
  }
#else
  /* in order to save builtin numbers, create a single
   * socket function with options 
   * socket_request(SockOperation,....)
   */
  case SOCKET_REQUEST: {
    switch (ptoc_int(1)) {
    case SOCKET_ROOT: /* socket_request(0,+domain,-socket_fd) */
      /* jf: for now only support AF_INET */
      domain = ptoc_int(2); 
      if (domain == 0) domain = AF_INET;
      else if (domain == 1){
	/* domain = AF_UNIX; */
	domain = AF_INET;
	fprintf(stderr, "default domain is AF_INET.\n");
      }
      else  {
	fprintf(stderr, "Invalid domain value. 0 - AF_INET, \
1 - AF_UNIX.\n");           
	return FALSE;
      }
	      
      if ((sockfd = socket(domain, SOCK_STREAM, IPPROTO_TCP)) < 0) {
	fprintf(stderr, "Cannot open stream socket.\n");
	return FALSE;
      }
      ctop_int(3, (Integer) sockfd);
      break;
    case SOCKET_BIND: /* socket_request(1,+domain,+sockfd,+port) */
      /* jf: for now only support AF_INET, ignore param */
      /* domain = ptoc_int(2); */
      sockfd = ptoc_int(3);
      portnum = ptoc_int(4);

      memset((char *)&socket_addr,(int) 0, sizeof(socket_addr));
	      
      socket_addr.sin_family = AF_INET;
      socket_addr.sin_port = htons(portnum); 
      socket_addr.sin_addr.s_addr = htonl(INADDR_ANY);
      /*
	for (i = 0; i < sizeof(socket_addr.sin_zero); i++)
	socket_addr.sin_zero[i] = 0;
      */
      if (bind(sockfd, (struct sockaddr *) &socket_addr, sizeof(socket_addr)) < 0){ 
	fprintf(stderr, "Cannot bind address.\n");
	return FALSE;
      }
      break;
    case SOCKET_LISTEN:  /* socket_request(2,+sockfd,+length) */
      sockfd = ptoc_int(2);
      if (listen(sockfd, ptoc_int(3)) < 0) {
	fprintf(stdout, "Cannot listen.\n");
	return FALSE;
      } 
      break;
    case SOCKET_ACCEPT: { /* socket_request(3,+sockfd,+sockfptr) */
      sockfd_in = ptoc_int(2);

      sockfd = accept(sockfd_in, NULL, NULL);
      sockptr = fdopen(sockfd, "r+");
      if (sockptr == NULL) {
	fprintf(stdout, "Cannot accept.\n");
	return FALSE;
      }

      for (i=3; i < MAX_OPEN_FILES && open_files[i] != NULL; i++) ;
      if (i == MAX_OPEN_FILES) {
	fprintf(stderr,"Can't accept: too many open files\n");
	return FALSE;
      }
      else {
	open_files[i] = sockptr;
	ctop_int(3, i);
      }
      break;
    }

    case SOCKET_CONNECT: {
      /* socket_request(4,+domain,+sockfd,+port,+hostname,-sockfptr) 
       * jf: domain is ignored for now
       */
      int con;

      sockfd = ptoc_int(3);
      portnum = ptoc_int(4);

      /*** prepare to connect ***/
      memset((char *)&socket_addr,(int) 0, sizeof(socket_addr));
      socket_addr.sin_family = AF_INET;
      /*
	for (i = 0; i < sizeof(dest.sin_zero); i++)
	dest.sin_zero[i] = 0;
      */
      socket_addr.sin_port = htons(portnum);
      socket_addr.sin_addr.s_addr = inet_addr(ptoc_string(5));


      while( ((con =
	       connect(sockfd, (struct sockaddr *)&socket_addr, 
		       sizeof(socket_addr))) == -1)
	     && (errno == EINTR) );
      if (con==-1) {
	close(sockfd);
	fprintf(stderr, "Cannot connect.\n");
      }

      sockptr = fdopen(sockfd, "r+");
      if (sockptr == NULL) {
	fprintf(stderr, "Cannot connect - fdopen failed.\n");
	return FALSE;
      }

      for (i=3; i < MAX_OPEN_FILES && open_files[i] != NULL; i++) ;
      if (i == MAX_OPEN_FILES) {
	fprintf(stderr, "Cannot connect - too many open files.\n");
	return FALSE;
      }
      else {
	open_files[i] = sockptr;
	ctop_int(6, i);
      }
      break;
    }
    case SOCKET_FLUSH: 	/* socket_request(5,+sockfd) */
      tmpval = ptoc_int(2);
      fptr = fileptr(tmpval);   
      fflush(fptr);
      break;
    case SOCKET_CLOSE:	/* socket_request(6,+sockfd) */
      close(ptoc_int(2));
      break;

    case SOCKET_GET0: { /* socket_request(11,+Sockfd,-C,-Error,_,_) */
      sockfd = ptoc_int(2);
      /*JPS: rc = readmsg(socketfd, &ch, 1);*/
      rc = recv (sockfd,&ch,1,0);
      if (rc == 1)
	ctop_int(3,(unsigned char)ch);
      else { ctop_int(3,-1); ctop_int(4,1);}  /* error msg constant 1 */
    }
    break;

    case SOCKET_PUT: /* socket_request(12,+Sockfd,+C,_,_,_) */
      { /* We should fail on error...*/
	char tmpch[4];
	sockfd = ptoc_int(2);
	/* JPS: sprintf(tmpch,"%c",ptoc_int(3));*/
	tmpch[0] = (char)ptoc_int(3);
	send(sockfd, tmpch, 1, 0);
      }
      break;

    default:
      fprintf(stderr, "Invalid socket request %d\n", (int) ptoc_int(1));
      return FALSE;
    }
    break;
  }
#endif
#endif /* HAVE_SOCKET */	    

#ifdef WIN_NT
  case JAVA_INTERRUPT: 
    return( startInterruptThread( (SOCKET)ptoc_int(1) ) );
#endif

  default:
    sprintf(message, "Builtin #%d is not implemented.\n", number);
    xsb_exit(message);
    break;
  }
  return 1;
}




/*------------------------- Auxiliary functions -----------------------------*/

#ifdef DEBUG
static void print_predicate_table(char *name, int arity, tab_inf_ptr tip)
{
  fprintf(stderr,"Printing predicate tables has not yet been implemented\n");
}
#endif /* DEBUG */

/* --------------------------------------------------------------------	*/

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

static void get_subgoal_ptr(Cell term, int arity, CPtr call_trie_root)
{
  int flag;

  /*  cptr_deref(term); --- Probably not needed --- Kostis. ---*/
  term = (Cell) cs_val(term);
  variant_call_search_rdonly(arity, (CPtr)term,
			     (CPtr *)call_trie_root, &flag, pcreg);
  if (flag) {
    *call_trie_root = (Cell) *((CPtr)*call_trie_root);
  }
  else {
    *call_trie_root = (Cell) 0;
  }
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
    if (!flag) return 0;
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
    return 0;
  case STRING: case INT: case FLOAT:
    return 1;
  case LIST:
    flag = flag * fast_ground(clref_val(temp));
    return flag * fast_ground(clref_val(temp)+1);
  case CS:
    for (j=1; j <= (int)get_arity(get_str_psc(temp)) ; j++) {
      flag = flag * fast_ground(clref_val(temp)+j);
    }
    return flag;
  default:
    fprintf(stderr, "Term with unknown tag (%d) in fast_ground\n",
	    (int)cell_tag(temp));
    abort();
    return -1;	/* so that g++ does not complain */
  }
}

/*----------------------------------------------------------------------*/
