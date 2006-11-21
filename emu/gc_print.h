/* File:      gc_print.h
** Author(s): Luis Castro, Bart Demoen, Kostis Sagonas
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


inline static char *code_to_string(byte *pc)
{
  return((char *)(inst_table[*pc][0])) ;
} /* code_to_string */

static void print_cell(FILE *where, CPtr cell_ptr, int fromwhere)
{
  Integer index = 0 ;
  Cell cell_val ;
  CPtr p ;
  int  whereto, tag ;
  char *s = 0 ;

  cell_val = cell(cell_ptr);

  if (cell_val == 0) { fprintf(where,"null,0).\n") ; return; }

  if (fromwhere == FROM_CP) heap_top++ ; /* because the hreg in a CP
					    can be equal to heap_top
					    and pointer_from_cell tests
					    for strict inequality */
  p = pointer_from_cell(cell_val,&tag,&whereto) ;
  if (fromwhere == FROM_CP) heap_top-- ;
  switch (whereto)
    { case TO_HEAP : index = p - heap_bot ; s = "ref_heap" ; break ;
    case TO_NOWHERE : index = (Integer)p ; s = "ref_nowhere" ; break ;
    case TO_LS : index = ls_bot - p ; s = "ref_ls" ; break ;
    case TO_CP : index = cp_bot - p ; s = "ref_cp" ; break ;
    case TO_TR : index = p - tr_bot ; s = "ref_tr" ; break ;
    case TO_COMPL : index = p - compl_bot ; s = "ref_compl" ; break ;
      /*case TO_ATTV_ARRAY : index = p ; s = "ref_attv_array" ; break ;*/
    }
  switch (tag)
    {
    case XSB_REF:
    case XSB_REF1:
      if (p == NULL) fprintf(where,"null,0).\n") ;
      else
        if (p == cell_ptr) fprintf(where,"undef,_).\n") ;
        else
	  { switch (whereto)
	    {
	    case TO_HEAP :
	    case TO_LS :
	    case TO_TR :
	    case TO_CP :
	      fprintf(where,"%s,%ld).\n",s,(long)index) ;
	      break ;
	    case TO_COMPL :
	      fprintf(where,"%s,%ld).\n",s,(long)index) ;
	      break ;
	    case TO_NOWHERE:
	      if (points_into_heap(p))
		{
		  index = (p-heap_bot) ;
		  fprintf(where,"between_h_ls,'/'(%ld,%p),_) .\n",(long)index,p) ;
		}
	      else
		if ((Integer)cell_val < 10000)
		  fprintf(where,"strange,%ld).\n",(long)cell_val) ;
		else
		  if (fromwhere == FROM_HEAP)
		    fprintf(where,"funct,'/'('%s',%d)).\n",
			    get_name((Psc)(cell_val)),
			    get_arity((Psc)(cell_val))) ;
		  else
		    if ((fromwhere == FROM_LS) || (fromwhere == FROM_CP))
		      {
			char *s ;
		        if ((tr_bot < (CPtr)cell_val) &&
			    ((CPtr)cell_val < cp_bot))
			  fprintf(where,"between_trail_cp,%ld).\n",
				  (long)cell_val) ;
			else
			  {
			    s = code_to_string((byte *)cell_val) ;
			    if (s == NULL)
			      fprintf(where,"dont_know,%ld).\n",cell_val) ;
			    else fprintf(where,"code,'-'(%ld,%s)).\n",cell_val,s) ;
			  }
		      }
		    else fprintf(where,"strange_ref,%ld).\n",cell_val) ;
	      break ;
	    }
	  }
      break ;
      
    case XSB_STRUCT :
      if (whereto == TO_NOWHERE)
	fprintf(where,"'-'(cs,%s),%lx).\n",s,(long)index) ;
      else
	fprintf(where,"'-'(cs,%s),%ld).\n",s,(long)index) ;
      break ;
      
    case XSB_LIST :
      fprintf(where,"'-'(list,%s),%ld).\n",s,(long)index) ;
      break ;
      
    case XSB_INT :
      fprintf(where,"int  ,%ld).\n",(long)int_val(cell_val)) ;
      break ;
      
    case XSB_FLOAT :
      fprintf(where,"float,%.5g).\n",float_val((Integer)cell_val)) ;
      break ;
      
    case XSB_STRING :
      fprintf(where,"atom ,'%s').\n",string_val(cell_val)) ;
      break ;
      
    case XSB_ATTV :
      fprintf(where,"attrv_%s,%ld).\n",s,(long)index) ;
      break ;

    default :
      fprintf(where,"strange,%ld).\n",cell_val) ;
      break ;
    }
} /* print_cell */

void print_heap(CTXTdeclc int start, int end, int add)
{
  CPtr startp, endp ;
  char buf[100] ;
  FILE *where ;

  SYS_MUTEX_LOCK(MUTEX_STACKS);
  sprintf(buf,"HEAP%d",printnum) ;
  printnum += add ;
  where = fopen(buf,"w") ;
  if (! where)
    { xsb_dbgmsg((LOG_GC,"could not open HEAP%d",printnum));
      return;
    }
  stack_boundaries ;

  if (start < 0) start = 0 ;
  startp = heap_bot + start ;
  endp = heap_bot + end ;
  if (endp > heap_top) endp = heap_top ;

  while ( startp < endp )
  { fprintf(where,"heap('%p',%6d,%s,",startp,start,pr_h_marked(startp)) ;
    print_cell(where,startp,FROM_HEAP) ;
    startp++ ; start++ ;
  }

  fclose(where) ;
  SYS_MUTEX_UNLOCK(MUTEX_STACKS);
} /* print_heap */

void print_ls(CTXTdeclc int add)
{
  CPtr startp, endp ;
  char buf[100] ;
  int start ;
  FILE *where ;

  SYS_MUTEX_LOCK(MUTEX_STACKS);
  sprintf(buf,"LS%d",printnum) ;
  printnum += add ;
  where = fopen(buf,"w") ;
  if (! where)
    { xsb_dbgmsg((LOG_GC,"could not open LS%d", printnum));
      return;
    }
  stack_boundaries ;

  start = 1 ;
  startp = ls_bot - 1 ;
  endp = ls_top ;

  while ( startp >= endp )
  { fprintf(where,"ls(%6d,%s,",start,pr_ls_marked(startp)) ;
    print_cell(where,startp,FROM_LS) ;
    startp-- ; start++ ;
  }

  fclose(where) ;
  SYS_MUTEX_UNLOCK(MUTEX_STACKS);
} /* print_ls */

void print_cp(CTXTdeclc int add)
{
  CPtr startp, endp ;
  char buf[100] ;
  int  start ;
  FILE *where ;

  SYS_MUTEX_LOCK(MUTEX_STACKS);

  sprintf(buf,"CP%d",printnum) ;
  printnum += add ;
  where = fopen(buf,"w") ;
  if (! where)
    { xsb_dbgmsg((LOG_GC, "could not open CP%d", printnum));
      return;
    }
  stack_boundaries ;

  start = 0 ;
  startp = cp_bot ;
  endp = cp_top ;

  while ( startp >= endp )
  { fprintf(where,"cp('%p',%6d,%s,",startp,start,pr_cp_marked(startp)) ;
    print_cell(where,startp,FROM_CP) ;
    fflush(where);
    startp-- ; start++ ;
  }

  fclose(where) ;
  SYS_MUTEX_UNLOCK(MUTEX_STACKS);
} /* print_cp */

void print_tr(CTXTdeclc int add)
{
  CPtr startp, endp ;
  int  start ;
  FILE *where ;
  char buf[100] ;

  SYS_MUTEX_LOCK(MUTEX_STACKS);

  sprintf(buf,"TRAIL%d",printnum) ;
  printnum += add ;
  where = fopen(buf,"w") ;
  if (! where)
    { xsb_dbgmsg((LOG_GC, "could not open TRAIL%d",printnum));
      return;
    }
  stack_boundaries ;

  startp = tr_bot ;
  endp = tr_top ;
#ifdef PRE_IMAGE_TRAIL
  start = tr_top - tr_bot ;
#else
  start = 0 ;
#endif

  while ( startp <= endp )
  {
#ifdef PRE_IMAGE_TRAIL
    if ((*endp) & PRE_IMAGE_MARK) {
      Cell tagged_tr_cell = *endp ;
      cell(endp) = tagged_tr_cell - PRE_IMAGE_MARK ; /* untag tr cell */
      fprintf(where,"trail(%6d,%s,  tagged,",start,pr_tr_marked(endp)) ;
      print_cell(where,endp,FROM_TR) ;
      cell(endp) = tagged_tr_cell ; /* restore trail cell */
      endp-- ; start-- ;
      fprintf(where,"trail(%6d,%s,pre_imag,",start,pr_tr_marked(endp)) ;
      print_cell(where,endp,FROM_TR) ;
      endp-- ; start-- ;
    } else {
      fprintf(where,"trail(%6d,%s,untagged,",start,pr_tr_marked(endp)) ;
      print_cell(where,endp,FROM_TR) ;
      endp-- ; start-- ;
    }
#else
    fprintf(where,"trail(%6d,%s,",start,pr_tr_marked(startp)) ;
    print_cell(where,startp,FROM_TR) ;
    startp++ ; start++ ;
#endif
  }

  fclose(where) ;
  SYS_MUTEX_UNLOCK(MUTEX_STACKS);
} /* print_tr */

void print_regs(CTXTdeclc int a, int add)
{
  CPtr startp, endp ;                                                     
  int  start ;                                                             
  FILE *where ;
  char buf[100] ;                                                         

  sprintf(buf,"REGS%d",printnum) ;                                       
  printnum += add ;
  where = fopen(buf,"w") ;
  if (! where)
    { xsb_dbgmsg((LOG_GC, "could not open REGS%d",printnum));
      return;
    }
  stack_boundaries ;      

  startp = reg+1 ;                                                       
  endp = reg+a ;                                                         
  start = 1 ;                                                             

  while (startp <= endp)                                              
    { 
      fprintf(where,"areg(%6d,",start) ;
      print_cell(where,startp,FROM_AREG) ;                              
      startp++ ; start++ ;                   
    }

  fprintf(where,"wam_reg(trreg,%ld).\n",(long)((CPtr)trreg-tr_bot)) ;
  fprintf(where,"wam_reg(breg,%ld).\n",(long)(cp_bot-breg)) ;
  fprintf(where,"wam_reg(hreg,%ld).\n",(long)(hreg-heap_bot)) ;
  fprintf(where,"wam_reg(ereg,%ld).\n",(long)(ls_bot-ereg)) ;

  fprintf(where,"wam_reg(trfreg,%ld).\n",(long)((CPtr)trfreg-tr_bot)) ;
  fprintf(where,"wam_reg(bfreg,%ld).\n",(long)(cp_bot-bfreg)) ;
  fprintf(where,"wam_reg(hfreg,%ld).\n",(long)(hfreg-heap_bot)) ;
  fprintf(where,"wam_reg(efreg,%ld).\n",(long)(ls_bot-efreg)) ;

  fprintf(where,"wam_reg(ptcpreg,%ld).\n",(Cell)ptcpreg) ;

  fprintf(where,"wam_reg(ebreg,%ld).\n",(long)(ls_bot-ebreg)) ;
  fprintf(where,"wam_reg(hbreg,%ld).\n",(long)(hbreg-heap_bot)) ;

  fprintf(where,"wam_reg(cpreg,%ld).\n",(Cell)cpreg) ;
  fprintf(where,"wam_reg(pcreg,%ld).\n",(Cell)pcreg) ;

  if (delayreg)
    {
      fprintf(where,"delayreg(");
      print_cell(where,(CPtr)(&delayreg),FROM_AREG);
    }
  else fprintf(where,"wam_reg(delayreg,%ld).\n",(Cell)delayreg);

  fclose(where) ;
} /* print_regs */


void print_all_stacks(CTXTdeclc int arity)
{
    printnum++ ;
    print_regs(CTXTc arity,0) ;
    print_heap(CTXTc 0,200000,0) ;
    print_ls(CTXTc 0) ;
    print_tr(CTXTc 0) ;
    print_cp(CTXTc 0) ;
} /* print_all_stacks */

