/* File:      struct_manager.c
** Author(s): Ernie Johnson
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


/* config.h must be the first #include.  Please don't move it. */
#include "configs/config.h"
#include "debugs/debug.h"

#include <stdio.h>
#include <stdlib.h>

#include "auxlry.h"
#include "struct_manager.h"
#include "cell.h"
#include "xsberror.h"


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void smPrint(Structure_Manager smRecord, char *string) {

  fprintf(stddbg,
	  "  Structure Manager for %s (%s)\n"
	  "\tBlock:   0x%p\t\tFree List:  0x%p\n"
	  "\tNextStr: 0x%p\t\tAlloc List: 0x%p\n"
	  "\tLastStr: 0x%p\n"
	  "\tStructs per block:  %d\tStruct size: %d bytes\n",
	  SM_StructName(smRecord),	string,
	  SM_CurBlock(smRecord),	SM_FreeList(smRecord),
	  SM_NextStruct(smRecord),	SM_AllocList(smRecord),
	  SM_LastStruct(smRecord),
	  SM_StructsPerBlock(smRecord),	SM_StructSize(smRecord));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void smAllocateBlock(Structure_Manager *pSM) {

  void *pNewBlock;

#ifdef DEBUG_STRUCT_ALLOC
  smPrint(*pSM,"before block allocation");
#endif
  pNewBlock = malloc(SM_NewBlockSize(*pSM));
  if ( IsNULL(pNewBlock) )
    xsb_abort("Ran out of memory in allocation of %s block\n",
	      SM_StructName(*pSM));
  SMBlk_NextBlock(pNewBlock) = SM_CurBlock(*pSM);
  SM_CurBlock(*pSM) = pNewBlock;
  SM_NextStruct(*pSM) = SMBlk_FirstStruct(pNewBlock);
  SM_LastStruct(*pSM) = SMBlk_LastStruct(pNewBlock,
					 SM_StructSize(*pSM),
					 SM_StructsPerBlock(*pSM));
#ifdef DEBUG_STRUCT_ALLOC
  smPrint(*pSM,"after block allocation");
#endif
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Return all blocks held by the Structure Manager to the system.
 */
void smFreeBlocks(Structure_Manager *pSM) {

  void *pCurBlock, *pNextBlock;

  pCurBlock = SM_CurBlock(*pSM);
  while ( IsNonNULL(pCurBlock) ) {
    pNextBlock = SMBlk_NextBlock(pCurBlock);
    free(pCurBlock);
    pCurBlock = pNextBlock;
  }
  SM_CurBlock(*pSM) = SM_NextStruct(*pSM) = SM_LastStruct(*pSM) = NULL;
  SM_AllocList(*pSM) = SM_FreeList(*pSM) = NULL;
}
