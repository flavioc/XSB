/* File:      load_seg.c
** Author(s): David S. Warren, Jiyang Xu, Terrance Swift,
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


#include <stdio.h>
#include <stdlib.h>

#include "configs/config.h"
#include "debugs/debug.h"

#include "auxlry.h"
#include "cell.h"
#include "inst.h"
#include "memory.h"
#include "register.h"
#include "psc.h"
#include "loader.h"
#include "tries.h"
#include "xmacro.h"

/************************************************************************
*                                                                       *
*  Load_text loads the byte code intruction from a byte code file to	*
*  the byte code program space.  References to indexes to the pcs table	*
*  are resolved with the use of the macro st_index.  New index relies   *
*  on the symbol table array which is assigned values by load_sms.	*
*  The routine assumes the current length 8/18/84 of byte code		*
*  intructions when reading from the byte code file.			*
*                                                                       *
************************************************************************/

/* Number of entries in one "segment" of the index relocation table.
   The table isn't actually segmented, but it is allocated in
   chunks of this size. */
#define NUM_INDEX_BLKS 256


/* === cross session variables ========================================	*/

static TIFptr tab_info_ptr;

TIFptr first_tip = 0, last_tip = 0;
static pseg last_text = 0;	/* permanent var, chain of text seg */
extern pw   *reloc_table; /* passed in parameter, but valid only once */

/* === local declarations =============================================	*/

struct hrec {
	long l;
	CPtr link;
} ;

/* === working variables valid only in one call to load_seg ===========	*/

static pseg current_seg;	/* starting address -- used for relocation */
static CPtr *index_reloc;         	/* index relocation table */
static int num_index_reloc;     	/* number of chunks in index_reloc */
static struct hrec *indextab;
static CPtr hptr;
static FILE *fd;			/* file descriptor */
static pindex *index_block_chain;	/* index block chain */

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


/* === return an appropriate hash table size ==========================	*/

static int hsize(int numentry)
{
    int i, j, temp;

/*  Weidong set the start point to be numentry+1; I change it as follows 
    change tls for faster indexing
    if (numentry > 64) temp = numentry / 2 else ; */
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

static void inserth(CPtr label, struct hrec *bucket) 
{ 
  CPtr temp;

  bucket->l++;
  temp = (CPtr)&(bucket->link);
  if (bucket->l > 1) {
       temp = (CPtr)*temp;
       while ((CPtr)*temp != temp) 
          temp = (CPtr)*(++temp);
  }
  *temp = (Cell)hptr;
  cell(hptr) = (Cell) label; hptr ++;
  cell(hptr) = (Cell) hptr; hptr ++;
}

/*----------------------------------------------------------------------*/

static int get_index_tab(int clause_no)
{
  long hashval, size, j;
  long count = 0;
  byte  type ;
  CPtr label;
  Integer ival;
  Cell val;

  hptr = hreg;
  size = hsize(clause_no);

  indextab = (struct hrec *)malloc(size*sizeof(struct hrec)); 

  for (j = 0; j < size; j++) {
      indextab[j].l = 0;
      indextab[j].link = (CPtr)&(indextab[j].link);
  }
  for (j = 0; j< clause_no; j++) {
         get_obj_byte(&type);
         switch (type) {
	    case 'i': get_obj_word_bb(&ival);
		      val = (Cell) ival ;
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
         get_obj_word_bbsig(&label);
         label = reloc_addr((Integer)label, seg_text(current_seg));
         hashval = ihash(val, size);
         inserth(label, &indextab[hashval]);
  }
  return count;
}

/*----------------------------------------------------------------------*/

static pindex new_index_seg( int no_cells )
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

static void gen_index(bool tabled, int clause_no, CPtr sob_arg_p, char arity)
{
  pindex new_i ;
  CPtr ep1, ep2;
  int j, size;
  CPtr temp;
 
  size = hsize(clause_no);
  new_i = new_index_seg(size);

  ep1 = i_block(new_i) ;
  cell(sob_arg_p) = (Cell)ep1 ;
  for (j = 0; j < size; j++) {
        if (indextab[j].l == 0) cell(ep1) = (Cell) &fail_inst;
        else if (indextab[j].l == 1) {
 		if (!tabled) {
		    cell(ep1) = *(indextab[j].link);
		} else {  /* create tabletrysingle */
           	    cell(ep1) = cell(indextab[j].link);
           	    new_i = new_index_seg(3);
           	    ep2 = i_block(new_i);
           	    cell(ep1) = (Cell) ep2;
           	    temp = indextab[j].link;
           	    gentabletry(tabletrysingle, arity,
				*temp++, tab_info_ptr, ep2);
        	}
      } else {
          /* otherwise create try/retry/trust instruction */
          new_i = new_index_seg(2*indextab[j].l+tabled);
          ep2 = i_block(new_i) ;
          cell(ep1) = (Cell) ep2 ;
          temp = (indextab[j].link) ;
	  if(!tabled)		/* generate "try" */
             gentry(try, arity, *temp, ep2) 
	  else
	     gentabletry(tabletry, arity, *temp, tab_info_ptr, ep2)

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

/*----------------------------------------------------------------------*/

static int load_text(int seg_num, int text_bytes, int *current_tab)
{
   CPtr inst_addr, end_addr;
   int current_opcode, oprand;
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
	   get_obj_word_bbsig(inst_addr);
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
	     if (cell(inst_addr) >= NUM_INDEX_BLKS*num_index_reloc) {
	       num_index_reloc = (cell(inst_addr)/NUM_INDEX_BLKS)+1;
	       index_reloc = (CPtr *)realloc(index_reloc,NUM_INDEX_BLKS*
					   num_index_reloc*sizeof(CPtr));
	       if (!index_reloc) {
		 fprintf(stderr, "ERROR: couldn't allocate index relocation space.\n");
		 return 0;
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
	   if (current_opcode == tabletry || current_opcode == tabletrysingle) {
	     New_TIF(tab_info_ptr,NULL);
	     if (first_tip == 0) first_tip = tab_info_ptr;
	     else TIF_NextTIF(last_tip) = tab_info_ptr;
	     last_tip = tab_info_ptr;
	   }
           get_obj_word(&tab_config_hold);          /* space holder */
           cell(inst_addr) = (Cell) tab_info_ptr;
	   inst_addr ++;
	   if (current_opcode == tabletry || 
	              current_opcode == tabletrysingle) {  /* tabconfig */
	 /* Consume the next 12 bytes (used to hold the CHS and RHS) */
	     get_obj_word(&tab_config_hold);
	     get_obj_word(&tab_config_hold);
	     get_obj_word(&tab_config_hold);
	   }
	   break;
	 default:
	   break;
	 }  /* switch */
       } /* for */
   }
   if (inst_addr != end_addr) {
     fprintf(stderr, "inst_addr %p, end_addr %p\n", inst_addr, end_addr);
     return 0;
   }
   else return 1;
}  /* end of load_text */

/*----------------------------------------------------------------------*/

static void load_index(int index_bytes, int table_num)
{
  Integer index_bno, clause_no, t_len;
  char index_inst,arity;
  int  count = 0;
  CPtr   sob_arg_p, temp_ptr;
  int temp_space ;

  while ( count < index_bytes ) {
      get_obj_byte(&index_inst);
      get_obj_byte(&arity);
      get_obj_word_bb(&index_bno);
      sob_arg_p = index_reloc[index_bno];
      get_obj_word_bb(&clause_no);

      temp_space = clause_no * 2 ;
      if( top_of_localstk - hreg >= temp_space + 512)
           temp_ptr = hptr = hreg;
      else temp_ptr = hptr = (CPtr)malloc(temp_space*sizeof(CPtr));
      t_len = get_index_tab(clause_no);

      gen_index(table_num > 0, clause_no, sob_arg_p, arity);
      free(indextab);
      if( temp_ptr != hreg ) free( temp_ptr ) ;
      count += 10 + t_len;
  }
}

/*== the entry procedure =============================================*/
/* Some debugging stuff in here.  I'll take it out when I'm sure 
   that I'm done with the changes */

pseg load_seg(int seg_num, int text_bytes, int index_bytes, FILE *file)
{
   int current_tab;

   current_seg = (pseg) mem_alloc(ZOOM_FACTOR*text_bytes+SIZE_SEG_HDR);

/* Allocate first chunk of index_reloc */
   index_reloc = (CPtr *)malloc(NUM_INDEX_BLKS*sizeof(CPtr));
   if (!index_reloc) {
     fprintf(stderr, "ERROR: couldn't allocate index relocation space.\n");
     return NULL;
   }
   num_index_reloc = 1;

/* SCAFFOLD */
   if ((long) current_seg % sizeof(Cell) != 0)
     fprintf(stderr, "non-aligned seg_first_inst!\n");
		/* alloc space, include 16 bytes header */
   current_seg++ ;
   seg_next(current_seg)  = 0 ;
   seg_prev(current_seg)  = last_text ;
   seg_index(current_seg) = 0 ;
   seg_size(current_seg)  = text_bytes*ZOOM_FACTOR + SIZE_SEG_HDR ;
   fd = file;
   if (!load_text(seg_num, text_bytes, &current_tab)) {
     mem_dealloc((pb)seg_hdr(current_seg), text_bytes+SIZE_SEG_HDR);
     return NULL;
   }
   index_block_chain = &seg_index(current_seg) ;
   load_index(index_bytes, current_tab) ;
   free(index_reloc) ;

/* set text-index segment chain */
   if (last_text) seg_next(last_text) = current_seg;
   last_text = current_seg;
   return current_seg;
}

/*----------------------------------------------------------------------*/
