/* File:      loader.c
** Author(s): David S. Warren, Jiyang Xu 
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
	The file loader.c contains routines for loading a byte code
	file into the emulator's permanent work space (pspace).
	It uses routines in file load_seg.c to access pspace.
*/
/************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "configs/config.h"
#include "debugs/debug.h"

#include "auxlry.h"
#include "psc.h"
#include "loader.h"
#include "cell.h"
#include "heap.h"
#include "flags.h"
#include "load_seg.h"
#include "tries.h"
#include "xmacro.h"
#include "xsberror.h"
#include "io_builtins.h"

#ifdef FOREIGN
#include "dynload.h"
#endif
 
/*--------------------------------------------------------------------*/

extern TIFptr get_tip(Psc);

/*--------------------------------------------------------------------*/

Psc global_mod;	/* points to "global", whose ep is globallist */

pw *reloc_table = 0;

#ifndef CHAT
static int warned_old_obj = 0;	/* warned the user about old .O files ? */
#endif

static FILE *fd;

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

void env_type_set(Psc psc, byte t_env, byte t_type, bool is_new)
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

/************************************************************************/

static inline Cell read_magic(void)
{
  Cell num;

  if ((int)get_obj_word(&num) < 4) return 0;
  fix_bb((byte *)&num);
  return num;
}

/************************************************************************/

static bool load_one_sym(FILE *fd, Psc cur_mod, int count, int exp)
{
  char name[256], modname[256];
  int  is_new;
  byte t_arity, t_type, t_env, t_len, t_modlen;
  Pair temp_pair;
  Psc  mod;

  get_obj_byte(&t_env);
#ifdef DEBUG
  /* this simple check can avoid worse situations in case of compiler bugs */
  if (t_env > T_GLOBAL) xsb_exit("Object file corrupted");
#endif
  get_obj_byte(&t_type);
  get_obj_byte(&t_arity);
  get_obj_byte(&t_len);
  get_obj_string(name, t_len);
  name[t_len] = 0;
  if (t_type==T_MODU) temp_pair = insert_module(0, name);
  else {
    if (t_env == T_IMPORTED) {
      get_obj_byte(&t_modlen);
      get_obj_string(modname, t_modlen);
      modname[t_modlen] = 0;
      temp_pair = insert_module(0, modname);
      mod = temp_pair->psc_ptr;
    } else if (t_env == T_GLOBAL) mod = global_mod;
    else mod = cur_mod;
    temp_pair = insert(name, t_arity, mod, &is_new);
    if (is_new && t_env==T_IMPORTED)
      set_ep(temp_pair->psc_ptr, (byte *)(mod));
    /* set ep to the psc record of the module name */
    env_type_set(temp_pair->psc_ptr, t_env, t_type, is_new);
    /* dsw added following */
    if (exp && t_env == T_EXPORTED) {
      /* xsb_dbgmsg("exporting: %s from: %s",name,cur_mod->nameptr);*/
      if (is_new) set_ep(temp_pair->psc_ptr, (byte*)(mod));
      link_sym(temp_pair->psc_ptr, (Psc)flags[CURRENT_MODULE]);
    }
  }
  if (!temp_pair) return 0;
  
  /*	if (count >= REL_TAB_SIZE) {
	xsb_dbgmsg("Reloc_table overflow");
	return 0;
	}  */
  
  reloc_table[count] = (pw)temp_pair;
  return 1;
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

static bool load_syms(FILE *fd, int psc_count, int count, Psc cur_mod, int exp)
{
  int i;
  
  reloc_table = (pw *) calloc((psc_count), sizeof(pw));
  /* xsb_dbgmsg("reloc_table %x,psc_count %d",reloc_table,psc_count); */

  for (i = count; i < psc_count; i++) {
    if (!load_one_sym(fd, cur_mod, i, exp)) return 0;
  }
  return (1);
}

/************************************************************************/

static byte *loader1(FILE *fd, int exp)
{
  char name_len, name[64], arity;
  int  is_new, seg_count ;
  unsigned long psc_count;
  Integer text_bytes, index_bytes;
  pseg seg_first_inst, first_inst;
  struct psc_rec *cur_mod;
  Pair ptr;
  TIFptr tip;
 
  seg_count = 0; first_inst = 0;
  get_obj_byte(&name_len);
  if (name_len < 64)
    get_obj_string(name, name_len);
  else xsb_exit("module name %s too long");
  name[(int)name_len] = 0;
  if (name_len==0) cur_mod = global_mod;
  else {
    ptr = insert_module(T_MODU, name);
    cur_mod = ptr->psc_ptr;
  }
  get_obj_word_bb(&psc_count);
  if (!load_syms(fd, (int)psc_count, 0, cur_mod, exp)) 
    return 0;
  /*	xsb_dbgmsg("symbol table of module %s loaded", name);	*/
  do {
    /*		xsb_dbgmsg("Seg count: %d",seg_count); */
    if (read_magic() != 0x11121306) break;
    seg_count++;
    /*		xsb_dbgmsg("Seg count: %d",seg_count); */
    /* get the header of the segment */
    get_obj_byte(&arity);
    get_obj_byte(&name_len);
    if (name_len < 64)
      get_obj_string(name, name_len);
    else xsb_exit("name %s too long");
    name[(int)name_len] = 0;
    get_obj_word_bb(&text_bytes);
    /*		xsb_dbgmsg("Text Bytes %x %d",text_bytes,text_bytes);*/
    get_obj_word_bb(&index_bytes);
    /* load the text-index segment */
    seg_first_inst = load_seg(seg_count,text_bytes,index_bytes,fd);
    if (!seg_first_inst) return 0;
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
    default:
      xsb_error("the predicate %s/%d cannot be loaded", name, arity);
      unload_seg(seg_first_inst);
      return NULL;
    }
  } while (1==1);
  /*
    xsb_dbgmsg("The first instruction of module %s is %x",
    get_name(cur_mod), first_inst);
  */
  return (pb)first_inst;
} /* loader1 */

/************************************************************************/
/*  Routines for the foreign language interface.			*/
/************************************************************************/

#ifdef FOREIGN
static byte *loader_foreign(char *filename, FILE *fd, int exp)
{
  unsigned char name_len, ldoption_len; 
  char name[64], ldoption[256];
  unsigned long psc_count;
  Psc  cur_mod;
  Pair ptr;

  get_obj_byte(&name_len);
  get_obj_string(name, name_len);
  name[name_len] = 0;
  get_obj_byte(&ldoption_len);
  get_obj_string(ldoption, ldoption_len);
  if (ldoption_len >= 255) {
    xsb_error("ldoption is too long for foreign module %s", name);
    return 0;
  }
  ldoption[ldoption_len] = 0;
  ptr = insert_module(T_MODU, name);
  cur_mod = ptr->psc_ptr;
  get_obj_word_bb(&psc_count);
  if (!load_syms(fd, (int)psc_count, 0, cur_mod, exp)) return 0;
  return load_obj(filename, cur_mod, ldoption);
} /* end of loader_foreign */
#endif

/************************************************************************/
/*									*/
/* Loads the file into permanent space.					*/
/* Data segment first (mixed psc entries and name strings), then text	*/
/* segment.  Under normal circumstances it returns the address of first	*/
/* instruction; if errors occur, it returns 0.				*/
/*									*/
/************************************************************************/

byte *loader(char *file, int exp)
{
  Cell magic_num;
  byte *first_inst = NULL;
  char message[240];  /* Allow multiple lines of error reporting.    */

  fd = fopen(file, "rb"); /* "b" needed for DOS. -smd */
  if (!fd) return 0;
  if (flags[HITRACE]) xsb_mesg("\n     ...... loading file %s", file);
  magic_num = read_magic();

  if (magic_num == 0x11121304) {
#ifdef CHAT			/* CHAT does not work with old .O files */
    sprintf(message,
	    "File: %s has an old byte code format that is not\n%s",
	    file,
	    "\t supported by CHAT.  The XSB program file must be recompiled.");
    xsb_abort(message);
#else
    if (!warned_old_obj) {
      sprintf(message,
	      "File  \"%s\"\n"
	      "\t   has an old byte code format that will NOT be supported\n"
	      "\t   in the future.  You are recommended to recompile\n"
	      "\t   all the .O files generated by XSB version 1.x.x.", file);
      xsb_warn(message);
      warned_old_obj = 1;
    }
#endif
  }

  if (magic_num == 0x11121304 || magic_num == 0x11121305)
    first_inst = loader1(fd,exp);
  else if (magic_num == 0x11121308) {
#ifdef FOREIGN
    first_inst = loader_foreign(file, fd, exp);
#else
    sprintf(message, "Trying to load in foreign file: %s\n", file);
    xsb_abort(message);
#endif
  }
  else {
    sprintf(message,
	    "File: %s does not have proper byte code format...\n%s",
	    file, "\t Please recompile it");
    xsb_abort(message);
    first_inst = NULL;
  }

  fclose(fd);
  if (reloc_table) {
    free(reloc_table);
  }
  reloc_table = 0;
  return first_inst;
} /* loader */
