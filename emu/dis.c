/* File:      dis.c
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


#include <stdio.h>
#include <string.h>

#include "configs/xsb_config.h"
#include "debugs/xsb_debug.h"

#include "auxlry.h"
#include "psc_xsb.h"
#include "hash_xsb.h"
#include "loader_xsb.h"
#include "cell_xsb.h"
#include "inst_xsb.h"
#include "builtin.h"
#include "memory_xsb.h"
#include "flags_xsb.h"
#include "tries.h"
#include "macro_xsb.h"

/* --------------- The following are working variables ----------------	*/

extern Cell builtin_table[BUILTIN_TBL_SZ][2];
extern TIFptr get_tip(Psc);

/*static FILE *filedes ;*/
#define filedes stdout

/* Include these so that the Gnu C Compiler does not complain */
static void dis_data(void);
static void dis_text(void);
static void dis_data_sub(Pair *);

void dis(bool distext)
{  
/*   filedes = fopen("stdout","w"); */
   dis_data();
   if (distext) dis_text();
/*   fflush(filedes);
   fclose(filedes); */
}

static void dis_data(void)
{
	int i;
	Pair *temp_ptr;
	Psc psc_ptr;
	char *modname;

	temp_ptr = (Pair *)(&flags[MOD_LIST]);
	while(*temp_ptr) {
	   psc_ptr = (*temp_ptr)->psc_ptr;
	   modname = get_name(psc_ptr);
	   if (get_type(psc_ptr))	/* 00000100 */
	        fprintf(filedes, "/* module %s : LOADED. */\n\n", modname);
	   else fprintf(filedes, "/* module %s : UNLOADED. */\n\n", modname);
	   if (strcmp(modname,"global")==0)
	   	for(i=0; i < (int)symbol_table.size; i++) {
		  if ( symbol_table.table[i] ) {
		    fprintf(filedes, "... ... BUCKET NO. %d\n", i);
		    dis_data_sub((Pair *)(symbol_table.table + i));
		  }
		}
	   else if (strcmp(modname,"usermod")==0) 
		fprintf(filedes, "\t/* Same as module global */\n");
	   else dis_data_sub((Pair *)&get_data(psc_ptr));
	   fprintf(filedes, "\n");
	   temp_ptr = &((*temp_ptr)->next);
	}
}

static void dis_data_sub(Pair *chain_ptr)
{
   Psc temp;

   while (*chain_ptr) {
	temp = (*chain_ptr)->psc_ptr;
	fprintf(filedes, "%p: ", temp);
	fflush(filedes);
	fprintf(filedes, "%s", get_name(temp));
	fprintf(filedes, "/%d,\t", get_arity(temp));
	switch(get_type(temp)) {
	    case T_PRED: fprintf(filedes, "PRED,\t"); break;
	    case T_DYNA: fprintf(filedes, "DYNA,\t"); break;
	    case T_ORDI: fprintf(filedes, "ORDI,\t"); break;
	    case T_FILE: fprintf(filedes, "FILE,\t"); break;
	    case T_MODU: fprintf(filedes, "MODU,\t"); break;
	    case T_FORN: fprintf(filedes, "FORN,\t"); break;
	    case T_UDEF: fprintf(filedes, "UDEF,\t"); break;
	    default:	 fprintf(filedes, "????"); break;
	}
	switch(get_env(temp)) {
	    case T_VISIBLE:  fprintf(filedes, "VISIBLE, "); break;
	    case T_HIDDEN:   fprintf(filedes, "HIDDEN, "); break;
	    case T_UNLOADED: fprintf(filedes, "UNLOADED, "); break;
	    default:	     fprintf(filedes, "(error env), "); break;
	}
	if (get_type(temp) == T_PRED) {
	  if (get_tip(temp) == NULL) fprintf(filedes, "UNTABLED, "); 
	  else fprintf(filedes, "TABLED, ");
	}
	fprintf(filedes, "%p\n", get_ep(temp));  /* dsw ???? */
	chain_ptr = &((*chain_ptr)->next);
   } /* while */
}

CPtr print_inst(FILE *fd, CPtr inst_ptr)
{
    Cell instr ;
    CPtr lpcreg ;
    int i,a;
    Psc psc;

    lpcreg = (CPtr) inst_ptr;
    fprintf(fd,"%p\t", lpcreg);
    instr = cell(lpcreg++) ;
/* We want the instruction string printed out below.  
 * Someday we should ANSI-fy it. 
 */
    fprintf(fd, (char *)inst_table[cell_opcode(&instr)][0]);
    a = 1 ; /* current operand */
    for (i=1; i<=4; i++) {
	switch (inst_table[cell_opcode(&instr)][i]) {
	 case A:
	   if (cell_opcode(&instr) == (byte) builtin) {
	     a++;
	     fprintf(fd, "\t%d\t(", cell_operand3(&instr));
	     fprintf(fd, (char *)builtin_table[cell_operand3(&instr)][0]);
	     fprintf(fd, ")");
	   } else fprintf(fd, "\t%d", cell_operandn(&instr,a++));
	   break;
	 case V:
	   fprintf(fd, "\tv%d", cell_operandn(&instr,a++));
	   break;
	 case R:
	   fprintf(fd, "\tr%d", cell_operandn(&instr,a++));
	   break;
	 case T:
	   fprintf(fd, "\t%lx", cell(lpcreg++));
	   break;
	 case P:
	   a++;
	   break;
	 case S:
	   if (cell_opcode(&instr) == (byte) call ||
	       cell_opcode(&instr) == (byte) execute) {
	     fprintf(fd, "\t0x%lx", *lpcreg);
	     psc = (Psc) cell(lpcreg++);
	     fprintf(fd,"\t(%s/%d)", get_name(psc), get_arity(psc));
	   }
	   else
	     fprintf(fd, "\t0x%lx", cell(lpcreg++));
	   break;
	 case C:
	 case L:
	 case G:
	   fprintf(fd, "\t0x%lx", cell(lpcreg++));
	   break;
	 case I:
	 case N:
	   fprintf(fd, "\t%ld", cell(lpcreg++));
	   break;
	 case F:
	   fprintf(fd, "\t0x%lx", cell(lpcreg++));
	   break;
	 case PP:
	   a += 2;
	   break;
	 case PPP:
	   break;
	 case PPR:
	   fprintf(fd, "\tr%d", cell_operand3(&instr));
	   break;
	 case RRR:
	   fprintf(fd, "\tr%d", cell_operand1(&instr));
	   fprintf(fd, "\tr%d", cell_operand2(&instr));
	   fprintf(fd, "\tr%d", cell_operand3(&instr));
	   break;
	 case X:
	   break;
	 default:
	   break;
	}  /* switch */
	/*if (cell_opcode(&instr) == noop) lpcreg += 2 * *(lpcreg-1); */
	if (cell_opcode(&instr) == noop) lpcreg += cell_operand3(&instr)/2; /* ?!@% */
    } /* for */
    fprintf(fd, "\n");
    fflush(fd);
    return lpcreg;
} /* print_inst */


static void dis_text(void)
{
   pseg   this_seg;
   pindex index_seg ;
   CPtr   endaddr, inst_addr2 ;

   fprintf(filedes, "\n/*text below\t\t*/\n\n");
   this_seg = (pseg) inst_begin;
   while (this_seg) {		/* repeat for all text segment */
      fprintf(filedes, "\nNew segment below \n\n");
      endaddr = (CPtr) ((pb) seg_hdr(this_seg) + seg_size(this_seg)) ;
      inst_addr2 = seg_text(this_seg);
      while (inst_addr2<endaddr) inst_addr2 = print_inst(filedes, inst_addr2);
      index_seg = seg_index(this_seg);
      while (index_seg) {
	inst_addr2 = i_block(index_seg);
	endaddr = (CPtr)((pb)index_seg + i_size(index_seg));
	if (cell_opcode(i_block(index_seg)) == try ||
            cell_opcode(i_block(index_seg)) == tabletry ||
	    cell_opcode(i_block(index_seg)) == tabletrysingle) {	
	                                           /* is try/retry/trust */
	  while (inst_addr2<endaddr) 
	    inst_addr2 = print_inst(filedes, inst_addr2);
	} else {					/* is hash table */
	  fprintf(filedes, "hash table.... \n");
	  while (inst_addr2<endaddr) {
	    fprintf(filedes, "%p:    %lx\n", inst_addr2, cell(inst_addr2));
	    inst_addr2 ++;
	  }
	  fprintf(filedes, "    end.... \n");
	}
	index_seg = i_next(index_seg);
      }
      this_seg = seg_next(this_seg);
   }  
}
