/* File:      tables.h
** Author(s): Johnson, Swift, Sagonas, Rao, Freire
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


#ifndef TABLE_DEFS


#define TABLE_DEFS


/*===========================================================================*/

/*
 *                    Tabling Function Prototypes
 *                    ===========================
 */

void	table_call_search(TabledCallInfo *, CallLookupResults *);
BTNptr	table_answer_search(SGFrame, int, CPtr, bool *);
void	table_consume_answer(BTNptr, int, int, CPtr, TIFptr);
ALNptr	table_retrieve_answers(SGFrame, SGFrame, CPtr);
void	table_complete_entry(SGFrame);

void	release_all_tabling_resources();

/*===========================================================================*/


#endif
