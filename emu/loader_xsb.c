/* File:      loader_xsb.c
** Author(s): David S. Warren, Jiyang Xu, Terrance Swift, Kostis Sagonas
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


/************************************************************************/
/*
	This file contains routines for loading a byte code
	file into the emulator's permanent work space (pspace).
*/
/************************************************************************/

#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "auxlry.h"
#include "psc_xsb.h"
#include "psc_defs.h"
#include "loader_xsb.h"
#include "extensions_xsb.h"
#include "cell_xsb.h"
#include "heap_xsb.h"
#include "flags_xsb.h"
#include "tries.h"
#include "macro_xsb.h"
#include "error_xsb.h"
#include "io_builtins_xsb.h"
#include "inst_xsb.h"
#include "memory_xsb.h"
#include "register.h"
#include "varstring_xsb.h"

#ifdef FOREIGN
#include "dynload.h"
#endif

#include "debug_xsb.h"
 
/* === stuff used from elsewhere ======================================	*/

extern TIFptr get_tip(Psc);

/* === macros =========================================================	*/

#define st_ptrpsc(i_addr)  (cell(i_addr) = *reloc_table[cell(i_addr)])

#define st_pscname(i_addr) (cell(i_addr) = \
	(Cell) get_name((Psc)(*reloc_table[cell(i_addr)])))

#define gentry(opcode, arg1, arg2, ep) {	\
        (cell_opcode(ep)) = (opcode);      	\
        (cell_operand1(ep)) = 0;              	\
        (cell_operand2(ep)) = 0;              	\
        (cell_operand3(ep)) = (arg1);         	\
	(ep)++;				 	\
        cell(ep) = (Cell) (arg2);        	\
        (ep)++; }

#define gentabletry(opcode, arg1, arg2, arg3, ep)     { \
        gentry(opcode, arg1, arg2, ep);  		\
        cell(ep) = (Cell) (arg3);          		\
        (ep)++; }

#define reloc_addr(offset, base) ((CPtr)((offset)<0 ? \
       		(pb)&fail_inst : ((pb)(base))+(long)(offset)*ZOOM_FACTOR))


/* In the following, y is the number of bytes we want to read from fd   */
#define get_obj_data(x,y)	(fread((char *)(x), 1, (y), fd))

#define get_obj_byte(x)		(get_obj_data((x),1))
#define get_obj_word(x)		(get_obj_data((x),OBJ_WORD_SIZE))
#define get_obj_string(x,len)	(get_obj_data((x),(len)))

#define get_obj_word_bb(x)    {get_obj_word(x) ; fix_bb(x) ; }
#define get_obj_word_bbsig(x) {get_obj_word(x) ; fix_bb4(x) ; \
			       *(Cell *)(x) = makeint(*(int *)(x));}
#define get_obj_word_bbsig_notag(x) {get_obj_word(x) ; fix_bb4(x) ; \
			       *(Integer *)(x) = *(int *)(x);}
#define get_obj_word_bbflt(x) {get_obj_word(x) ; fix_bb4(x) ; \
			       *(Cell *)(x) = makefloat(*(float*)x);}

/* === local declarations =============================================	*/

struct hrec {
  long l;
  CPtr link;
} ;

/*----------------------------------------------------------------------*/

/* Number of entries in one "segment" of the index relocation table.
   The table isn't actually segmented, but it is allocated in
   chunks of this size. */
#define NUM_INDEX_BLKS 256

/* === variables also used in other parts of the system =============== */

Psc global_mod;	/* points to "global", whose ep is globallist */

struct tif_list  tif_list = {NULL, NULL};

/* === working variables ==============================================	*/

static pw   *reloc_table = NULL;
static pseg last_text = NULL;	/* permanent var, chain of text seg */
static pseg current_seg;	/* starting address -- used for relocation */
static CPtr *index_reloc;         	/* index relocation table */
static int  num_index_reloc;     	/* number of chunks in index_reloc */
static struct hrec *indextab;
static TIFptr tab_info_ptr;
static CPtr hptr;
static pindex *index_block_chain;	/* index block chain */

/* === return an appropriate hash table size ==========================	*/

inline static int hsize(int numentry)
{
  int i, j, temp;

  if (numentry > 16) temp = numentry; 
  else temp = 2 * numentry + 1;
  j = temp / 2 + 1;
  for (i = 2; i <= j; i++) {
    if ((i != temp) && ((temp % i) == 0)) {temp++; j = temp/2+1;}
  }
  return temp;
}

/* == unload a segment ================================================	*/

void unload_seg(pseg s)
{
  pindex i1, i2 ;
  pseg prev, next ;

  /* free the index blocks */
  i1 = seg_index(s) ;
  while (i1) {
    i2 = i_next(i1) ;
    mem_dealloc((pb)i1, i_size(i1));
    i1 = i2;
  }
  /* delete segment from segment dllist and dealloc it */
  next = seg_next(s) ;
  prev = seg_prev(s) ;
  if (next) seg_prev(next) = prev ;
  if (prev) seg_next(prev) = next ;
  if (last_text==s) last_text = prev ;
  mem_dealloc((pb)seg_hdr(s), seg_size(s));
}

/*----------------------------------------------------------------------*/

/* use heap top as temp place of hash link and entries; */
/* heap top pointer is not alterred so nothing affects later heap use */

inline static void inserth(CPtr label, struct hrec *bucket) 
{ 
  CPtr temp;

  bucket->l++;
  temp = (CPtr)&(bucket->link);
  if (bucket->l > 1) {
    temp = (CPtr)*temp;
    while ((CPtr)*temp != temp) 
      /*temp = (CPtr)*(++temp); */
      temp = (CPtr)*(temp+1);
  }
  *temp = (Cell)hptr;
  cell(hptr) = (Cell) label; hptr++;
  cell(hptr) = (Cell) hptr; hptr++;
}

/*----------------------------------------------------------------------*/

static int get_index_tab(FILE *fd, int clause_no)
{
  long hashval, size, j;
  long count = 0;
  byte  type ;
  CPtr label;
  Integer ival;
  Cell val;

  size = hsize(clause_no);

  indextab = (struct hrec *)malloc(size*sizeof(struct hrec)); 

  for (j = 0; j < size; j++) {
    indextab[j].l = 0;
    indextab[j].link = (CPtr)&(indextab[j].link);
  }
  for (j = 0; j < clause_no; j++) {
    get_obj_byte(&type);
    switch (type) {
    case 'i': get_obj_word_bbsig_notag(&ival);
      val = (Cell) ival; 
      count += 9;
      break;
    case 'l': val = (Cell)(list_str); 
      count += 5;
      break;
    case 'n': val = 0;
      count += 5;
      break;
    case 'c': get_obj_word_bb(&ival);
      count += 9;
      val = (Cell)ival ;
      st_pscname(&val);
      break;
    case 's': get_obj_word_bb(&ival);
      count += 9;
      val = (Cell)ival ;
      st_ptrpsc(&val);
      break; 
    }

    get_obj_word_bbsig_notag(&label);
    label = reloc_addr((Integer)label, seg_text(current_seg));
    hashval = ihash(val, size);
    inserth(label, &indextab[hashval]);
  }
  return count;
}

/*----------------------------------------------------------------------*/

inline static pindex new_index_seg(int no_cells)
{
  pindex new_i = (pindex)mem_alloc(SIZE_IDX_HDR + sizeof(Cell) * no_cells ) ;
 
  /* initialize fields of new index segment header */
  i_next(new_i) = 0 ;
  i_size(new_i) = SIZE_IDX_HDR + sizeof(Cell) * no_cells ;
  
  /* append at tail of block chain */
  *index_block_chain = new_i ;
  index_block_chain = &i_next(new_i) ;

  return new_i ;
}

/*----------------------------------------------------------------------*/

static void gen_index(xsbBool tabled, int clause_no, CPtr sob_arg_p, byte arity)
{
  pindex new_i;
  CPtr   ep1, ep2, temp;
  int    j, size; 
 
  size = hsize(clause_no);
  new_i = new_index_seg(size);

  ep1 = i_block(new_i) ;
  cell(sob_arg_p) = (Cell)ep1 ;
  for (j = 0; j < size; j++) {
    if (indextab[j].l == 0) 
      cell(ep1) = (Cell) &fail_inst;
    else if (indextab[j].l == 1) {
      if (!tabled) {
	cell(ep1) = *(indextab[j].link);
      } else {  /* create tabletrysingle */
	cell(ep1) = cell(indextab[j].link);
	new_i = new_index_seg(3);
	ep2 = i_block(new_i);
	cell(ep1) = (Cell) ep2;
	temp = indextab[j].link;
	gentabletry(tabletrysingle, arity, *temp++, tab_info_ptr, ep2);
      }
    } else {
      /* otherwise create try/retry/trust instruction */
      new_i = new_index_seg(2*indextab[j].l+tabled);
      ep2 = i_block(new_i) ;
      cell(ep1) = (Cell) ep2 ;
      temp = (indextab[j].link) ;
      if (!tabled) {	/* generate "try" */
	gentry(try, arity, *temp, ep2);
      } else {
	gentabletry(tabletry, arity, *temp, tab_info_ptr, ep2);
      }

      for (temp++; *temp != (Cell)temp; temp++) {
	temp = (CPtr) cell(temp);		/* generate "retry" */
	gentry((tabled?tableretry:retry), arity, *temp, ep2);
      }
      /* change last "retry" to "trust" */
      cell_opcode(ep2-2) = tabled ? tabletrust : trust;
    }
    ep1++;
  }
}

/************************************************************************
*                                                                       *
*  load_text() loads the byte code intruction from a byte code file to	*
*  the byte code program space.  References to indexes to the pcs table	*
*  are resolved with the use of the macro st_index.  New index relies   *
*  on the symbol table array which is assigned values by load_sms.	*
*  The routine assumes the current length 8/18/84 of byte code		*
*  intructions when reading from the byte code file.			*
*                                                                       *
************************************************************************/

static int load_text(FILE *fd, int seg_num, int text_bytes, int *current_tab)
{
  CPtr inst_addr, end_addr;
  int  current_opcode, oprand;
  Cell tab_config_hold;	/* working pointer */
  
  *current_tab = -1;
  inst_addr = seg_text(current_seg);
  end_addr  = (CPtr)((pb)inst_addr + text_bytes * ZOOM_FACTOR);
  while (inst_addr<end_addr && get_obj_word(inst_addr) ) {
    current_opcode = cell_opcode(inst_addr);
    inst_addr ++;
    for (oprand=1; oprand<=4; oprand++) {
      switch (inst_table[current_opcode][oprand]) {
      case A:
      case V:
      case R:
      case P:
      case PP:
      case PPP:
      case PPR:
      case RRR:
	break;
      case S:
	get_obj_word_bb(inst_addr);
	st_ptrpsc(inst_addr);
	inst_addr ++;
	break;
      case C:
	get_obj_word_bb(inst_addr);
	st_pscname(inst_addr);
	inst_addr ++;
	break;
      case L:
	get_obj_word_bbsig_notag(inst_addr);
	*(CPtr *)inst_addr = reloc_addr((Integer)cell(inst_addr),
					seg_text(current_seg));
	inst_addr ++;
	break;
      case G:
	get_obj_word_bb(inst_addr);
	st_pscname(inst_addr);
	inst_addr ++;
	break;
      case N:
	get_obj_word_bbsig_notag(inst_addr);
	inst_addr ++;
	break;
      case B:
	get_obj_word_bbsig(inst_addr);
	inst_addr ++;
	break;
      case F:
	get_obj_word_bbflt(inst_addr);
	inst_addr ++;
	break;
      case I:
	get_obj_word_bb(inst_addr);
	if (oprand==2) {	/* second operand of switchonbound */
	  if (cell(inst_addr) >= (unsigned long)(NUM_INDEX_BLKS*num_index_reloc)) {
	    num_index_reloc = (cell(inst_addr)/NUM_INDEX_BLKS)+1;
	    index_reloc = (CPtr *)realloc(index_reloc,NUM_INDEX_BLKS*
					  num_index_reloc*sizeof(CPtr));
	    if (!index_reloc) {
	      xsb_error("Couldn't allocate index relocation space");
	      return FALSE;
	    }
	  }
	  index_reloc[cell(inst_addr)] = (CPtr)inst_addr;
	}
	else 		/* third operand of switchonbound */
	  cell(inst_addr) = hsize(cell(inst_addr));
	inst_addr ++;
	break;
      case X:
	break;
      case T:	  
	*current_tab = 1;	/* flag for load index */
	if (current_opcode == tabletry || current_opcode == tabletrysingle)
	  New_TIF(tab_info_ptr,NULL);
	get_obj_word(&tab_config_hold);          /* space holder */
	cell(inst_addr) = (Cell) tab_info_ptr;
	inst_addr ++;
	break;
      default:
	break;
      }  /* switch */
    } /* for */
  }
  if (inst_addr != end_addr) {
    xsb_dbgmsg((LOG_DEBUG, "inst_addr %p, end_addr %p", inst_addr, end_addr));
    return FALSE;
  }
  else return TRUE;
}  /* end of load_text */

/*----------------------------------------------------------------------*/

static void load_index(FILE *fd, int index_bytes, int table_num)
{
  Integer index_bno, clause_no, t_len;
  byte    index_inst, arity;
  int     temp_space, count = 0;
  CPtr    sob_arg_p, temp_ptr;

  while (count < index_bytes) {
    get_obj_byte(&index_inst);
    get_obj_byte(&arity);
    get_obj_word_bb(&index_bno);
    sob_arg_p = index_reloc[index_bno];
    get_obj_word_bb(&clause_no);
    
    temp_space = clause_no * 2;
    if (top_of_localstk - hreg >= temp_space + 512)
      temp_ptr = hptr = hreg;
    else temp_ptr = hptr = (CPtr)malloc(temp_space*sizeof(CPtr));
    t_len = get_index_tab(fd, clause_no);
    
    gen_index((xsbBool)(table_num > 0), clause_no, sob_arg_p, arity);
    free(indextab);
    if (temp_ptr != hreg) free(temp_ptr);
    count += 10 + t_len;
  }
}

/*== the load_seg function =============================================*/

static pseg load_seg(FILE *fd, int seg_num, int text_bytes, int index_bytes)
{
   int current_tab;

   current_seg = (pseg) mem_alloc(ZOOM_FACTOR*text_bytes+SIZE_SEG_HDR);

   /* Allocate first chunk of index_reloc */
   index_reloc = (CPtr *)malloc(NUM_INDEX_BLKS*sizeof(CPtr));
   if (!index_reloc) {
     xsb_error("Couldn't allocate index relocation space");
     return NULL;
   }
   num_index_reloc = 1;

   /* alloc space, include 16 bytes header */
   current_seg++;
   seg_next(current_seg)  = 0;
   seg_prev(current_seg)  = last_text;
   seg_index(current_seg) = 0;
   seg_size(current_seg)  = text_bytes*ZOOM_FACTOR + SIZE_SEG_HDR;
   /* fd = file; */
   if (!load_text(fd, seg_num, text_bytes, &current_tab)) {
     mem_dealloc((pb)seg_hdr(current_seg), text_bytes+SIZE_SEG_HDR);
     return NULL;
   }
   index_block_chain = &seg_index(current_seg);
   load_index(fd, index_bytes, current_tab);
   free(index_reloc);
   
   /* set text-index segment chain */
   if (last_text) seg_next(last_text) = current_seg;
   last_text = current_seg;
   return current_seg;
}

/************************************************************************/
/*  Routines to check environment consistency.				*/
/************************************************************************/

#define T_NEW 3
#define E_HIDDEN -1
#define E_NOUSE -2

static int env_check[4][5] = {
/*		   T_EXPORT   T_LOCAL   T_IMPORTED   T_IMEX   T_GLOBAL	*/
/*======================================================================*/
/* T_VISIBLE  */ { T_VISIBLE, T_HIDDEN, T_VISIBLE,  E_NOUSE, T_VISIBLE  },
/* T_HIDDEN   */ { T_HIDDEN,  T_HIDDEN,	E_HIDDEN,   E_NOUSE, T_VISIBLE  },
/* T_UNLOADED */ { T_VISIBLE, E_HIDDEN, T_UNLOADED, E_NOUSE, T_UNLOADED },
/* T_NEW      */ { T_VISIBLE, T_HIDDEN, T_UNLOADED, E_NOUSE, T_VISIBLE  }
};

void env_type_set(Psc psc, byte t_env, byte t_type, xsbBool is_new)
{
  int env;
  byte type;

  if (is_new) {
    set_env(psc, env_check[T_NEW][t_env]);
    set_type(psc, t_type);
  } else {
    env = env_check[get_env(psc)][t_env];
    if (env < 0) {
      xsb_error("Environment conflict in the use of %s/%d !", 
		get_name(psc), get_arity(psc));
      /* In the following I am not sure whether setting the environment */
      /* in the presense of an environment conflict error is the right  */
      /* thing to do!  But an "imported_from" vs "local" (non-exported) */
      /* symbol conflict must definitely be resolved in favour of the   */
      /* "local" declaration.						*/
      if (env == E_HIDDEN) {
	if (t_env == T_IMPORTED) 
	  /* Here the psc record of the symbol has already been created */
	  /* by another module that imported (mistakenly) this symbol.  */
	  set_env(psc, T_LOCAL);	
	else /* We are trying to load a module
		that imports sth not exported. */
	  xsb_error("module imports something that is not exported");
      }
    }
    else set_env(psc, env);
    type = get_type(psc);
    if (t_type && type && t_type != type) {
      if (t_type==T_UDEF && (type==T_PRED || type==T_DYNA || type==T_FORN)) ;
      else if (t_type==T_FORN && type==T_UDEF) set_type(psc, T_FORN);
      else xsb_error("incompatible types in the use of %s (%x with %x)",
		     get_name(psc), type, t_type);
    } else set_type(psc, type | t_type);  
  }
}

/*----------------------------------------------------------------------*/

unsigned int read_magic(FILE *fd)
{
  unsigned int num;

  if (get_obj_word(&num) < 4) return 0;
  fix_bb4((byte *)&num);
  return num;
}

/*----------------------------------------------------------------------*/

inline static void get_obj_atom(FILE *fd, VarString *atom)
{
  byte x;
  unsigned int len;
  
  get_obj_data((&x),1);
  /* ``x'' gets the length of the string or > SHORT_LDOPTIONLEN.
     The latter means we have a long atom.
     In this case, the length is stored in 4 bytes & we use get_obj_word_bb */
  if (x > SHORT_LDOPTIONLEN) { /* handle unusual case specially */
    get_obj_word_bb(&len);
    /* xsb_dbgmsg(("get_obj_len = %d... Case is not handled yet!\n",len)); */
  } else
    len = x;

  XSB_StrEnsureSize(atom,len+1);
  get_obj_string(atom->string, len);
  atom->length = len;
  XSB_StrNullTerminate(atom);
}

/*----------------------------------------------------------------------*/

static xsbBool load_one_sym(FILE *fd, Psc cur_mod, int count, int exp)
{
  static XSB_StrDefine(str);
  int  is_new;
  byte t_arity, t_type, t_env;
  Pair temp_pair;
  Psc  mod;

  get_obj_byte(&t_env);
  /* this simple check can avoid worse situations in case of compiler bugs */
  if (t_env > T_GLOBAL) 
    xsb_abort("[LOADER] The loaded object file %s%s is corrupted",
	      cur_mod->nameptr, XSB_OBJ_EXTENSION_STRING);

  get_obj_byte(&t_type);
  get_obj_byte(&t_arity);
  get_obj_atom(fd, &str);
  if (t_type == T_MODU)
    temp_pair = insert_module(0, str.string);
  else {
    if (t_env == T_IMPORTED) {
      byte t_modlen;
      char modname[MAXNAME+1];
      get_obj_byte(&t_modlen);
      get_obj_string(modname, t_modlen);
      modname[t_modlen] = '\0';
      temp_pair = insert_module(0, modname);
      mod = temp_pair->psc_ptr;
    } else if (t_env == T_GLOBAL) mod = global_mod;
    else mod = cur_mod;
    temp_pair = insert(str.string, t_arity, mod, &is_new);
    if (is_new && t_env==T_IMPORTED)
      set_data(temp_pair->psc_ptr, mod);
    /* set psc_data to the psc record of the module name */
    env_type_set(temp_pair->psc_ptr, t_env, t_type, (xsbBool)is_new);
    /* dsw added following, maybe wrongly */
    if (exp && t_env == T_EXPORTED) {
      /* xsb_dbgmsg(("exporting: %s from: %s",name,cur_mod->nameptr)); */
      if (is_new) set_data(temp_pair->psc_ptr, mod);
      link_sym(temp_pair->psc_ptr, (Psc)flags[CURRENT_MODULE]);
    }
  }
  if (!temp_pair) return FALSE;
  
  /*	if (count >= REL_TAB_SIZE) {
	xsb_dbgmsg(("Reloc_table overflow"));
	return FALSE;
	}  */
  
  reloc_table[count] = (pw)temp_pair;
  return TRUE;
}  /* load_one_sym */

/************************************************************************
*                                                                       *
* Load_syms is a function which loads a symbol table given in a byte    *
* code file into an appropriate format in the pcs table.  As part of    *
* its function it resolves entry points for byte code intructions (call *
* to relloc_addr), and maintains a tableau so that instructions with    *
* indices into the psc table may have those indices resolved before     *
* loading them in the intruction array (byte code program space).  The  *
* intructions are loaded by a separate function.                        *
*                                                                       *
************************************************************************/

static xsbBool load_syms(FILE *fd, int psc_count, int count, Psc cur_mod, int exp)
{
  int i;
  
  reloc_table = (pw *) calloc((psc_count), sizeof(pw));
  /* xsb_dbgmsg(("reloc_table %x,psc_count %d",reloc_table,psc_count)); */

  for (i = count; i < psc_count; i++) {
    if (!load_one_sym(fd, cur_mod, i, exp)) return FALSE;
  }
  return TRUE;
}

/************************************************************************/

static byte *loader1(FILE *fd, int exp)
{
  char name[FOREIGN_NAMELEN], arity;
  byte name_len;
  int  is_new, seg_count;
  unsigned long psc_count;
  Integer text_bytes, index_bytes;
  pseg seg_first_inst, first_inst;
  Psc cur_mod;
  Pair ptr;
  TIFptr tip;
 
  seg_count = 0; first_inst = 0;
  get_obj_byte(&name_len);

  if (name_len >= FOREIGN_NAMELEN)
    xsb_abort("[LOADER] Foreign module name is too long");

  get_obj_string(name, name_len);
  name[(int)name_len] = 0;
  if (name_len==0) cur_mod = global_mod;
  else {
    ptr = insert_module(T_MODU, name);
    cur_mod = ptr->psc_ptr;
  }
  get_obj_word_bb(&psc_count);
  if (!load_syms(fd, (int)psc_count, 0, cur_mod, exp)) 
    return FALSE;
  /*	xsb_dbgmsg(("symbol table of module %s loaded", name));	*/
  do {
    /*		xsb_dbgmsg(("Seg count: %d",seg_count)); */
    if (read_magic(fd) != 0x11121306) break;
    seg_count++;
    /*		xsb_dbgmsg(("Seg count: %d",seg_count)); */
    /* get the header of the segment */
    get_obj_byte(&arity);
    get_obj_byte(&name_len);

    if (name_len >= FOREIGN_NAMELEN)
      xsb_abort("[LOADER] Module name is too long");

    get_obj_string(name, name_len);
    name[(int)name_len] = 0;
    get_obj_word_bb(&text_bytes);
    /*		xsb_dbgmsg(("Text Bytes %x %d",text_bytes,text_bytes));*/
    get_obj_word_bb(&index_bytes);
    /* load the text-index segment */
    seg_first_inst = load_seg(fd,seg_count,text_bytes,index_bytes);
    if (!seg_first_inst) return FALSE;
    if (seg_count == 1) first_inst = seg_first_inst;
    /* 1st inst of file */
    /* set the entry point of the predicate */
    ptr = insert(name, arity, cur_mod, &is_new);
    switch (get_type(ptr->psc_ptr)) {
    case T_ORDI:
    case T_UDEF:
      if (strcmp(name, "_$main")!=0) {
	set_type(ptr->psc_ptr, T_PRED);
	set_ep(ptr->psc_ptr, (pb)seg_first_inst);
      }
      if ((tip = get_tip(ptr->psc_ptr)) != NULL) {
	TIF_PSC(tip) = (ptr->psc_ptr);
      }
      break;
    case T_PRED:
      if (strcmp(name, "_$main")!=0) {
	unload_seg((pseg)get_ep(ptr->psc_ptr));
	set_ep(ptr->psc_ptr, (pb)seg_first_inst);
      }
      if ((tip = get_tip(ptr->psc_ptr)) != NULL) {
	TIF_PSC(tip) = (ptr->psc_ptr);
      }
      break;
    case T_DYNA:
      unload_seg(seg_first_inst);
      xsb_abort("[LOADER] Trying to compile a dynamic predicate, %s/%d",
		name, arity);
      return NULL;
    default:
      unload_seg(seg_first_inst);
      xsb_abort("[LOADER] The predicate %s/%d cannot be loaded", name, arity);
      return NULL;
    }
  } while (1==1);
  /*
    xsb_dbgmsg(("The first instruction of module %s is %x",
    get_name(cur_mod), first_inst));
  */
  return (pb)first_inst;
} /* loader1 */

/************************************************************************/
/*  Routines for the foreign language interface.			*/
/************************************************************************/

#ifdef FOREIGN
static byte *loader_foreign(char *filename, FILE *fd, int exp)
{
  byte name_len, *instr;
  char name[FOREIGN_NAMELEN];
  static XSB_StrDefine(ldoption);
  unsigned long psc_count;
  Psc  cur_mod;
  Pair ptr;

  get_obj_byte(&name_len);
  if (name_len >= FOREIGN_NAMELEN) {
    xsb_error("[LOADER] Foreign module name is too long");
    return FALSE;
  }
  get_obj_string(name, name_len);
  name[name_len] = 0;
  get_obj_atom(fd, &ldoption);
  ptr = insert_module(T_MODU, name);
  cur_mod = ptr->psc_ptr;
  get_obj_word_bb(&psc_count);
  if (!load_syms(fd, (int)psc_count, 0, cur_mod, exp)) return FALSE;
  instr = load_obj(filename, cur_mod, ldoption.string);
  return instr;
} /* end of loader_foreign */
#endif

/************************************************************************/
/*									*/
/* Loads the file into permanent space.					*/
/* Data segment first (mixed psc entries and name strings), then text	*/
/* segment.  Under normal circumstances it returns the address of first	*/
/* instruction; if errors occur, it returns NULL.			*/
/*									*/
/************************************************************************/

static int warned_old_obj = 0;	/* warned the user about old object files ? */

byte *loader(char *file, int exp)
{
  FILE *fd;	      /* file descriptor */
  unsigned int magic_num;
  byte *first_inst = NULL;

  fd = fopen(file, "rb"); /* "b" needed for DOS. -smd */
  if (!fd) return NULL;
  if (flags[HITRACE]) xsb_mesg("\n     ...... loading file %s", file);
  magic_num = read_magic(fd);

  if (magic_num == 0x11121304 || magic_num == 0x11121305) {
    if (!warned_old_obj) {
      xsb_warn("File \"%s\"\n"
	       "\t   has old byte code format, which is likely to cause\n"
	       "\t   unpredictable behavior.\n"
	       "\t   Please recompile the file with XSB version > 2.01.",
	       file);
      warned_old_obj = 1;
    }
  }

  if (magic_num == 0x11121307 || magic_num == 0x11121305)
    first_inst = loader1(fd,exp);
  else if (magic_num == 0x11121308) {
#ifdef FOREIGN
    first_inst = loader_foreign(file, fd, exp);
#else
    xsb_abort("Loading a foreign file: %s", file);
#endif
  }
  else {
    xsb_abort("File: %s does not have proper byte code format...\n%s",
	      file, "\t Please recompile it");
    first_inst = NULL;
  }

  fclose(fd);
  if (reloc_table) {
    free(reloc_table);
  }
  reloc_table = 0;
  return first_inst;
} /* loader */
