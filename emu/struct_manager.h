/* File:      struct_manager.h
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


#ifndef STRUCTURE_MANAGER

#define STRUCTURE_MANAGER


/*===========================================================================*/

/*
 *           Generic Block-Oriented Data Structure Management
 *           ================================================
 *
 * The following structure and macros provide a uniform method for
 * managing pools of data structures allocated from the system by block
 * (rather than individually).  By following a few guidelines, the
 * implementation of any such structure with this desired allocation
 * property is simplified.
 *
 * Interface:
 * ---------
 * - SM_InitDecl(StructType,StructsPerBlock,StructNameString)
 * - SM_AllocateStruct(SM,pNewStruct)
 * - SM_DeallocateStruct(SM,pStruct)
 * - SM_DeallocateStructList(SM,pHead,pTail)
 * - SM_CurrentCapacity(SM,NumBlocks,NumStructs)
 * - SM_CountFreeStructs(SM,NumFree)
 * - SM_RawUsage(SM,TotalBlockUsageInBytes)
 * - SM_ReleaseResources(SM)
 *
 * Management Organization:
 * -----------------------
 * Blocks of structures (records) are allocated from the system, one at a
 * time, and maintained on a linked list.  The head of this list is the
 * most recently allocated block and the one from which individual records
 * are parceled out.  Deallocated records -- ones which were
 * previously-allocated but then returned to the Manager -- are maintained
 * in a list for reallocation.  Priority is given to these previously-used
 * structures during the allocation process.  The first word of a block,
 * as well as that in a deallocated record, is used as a link for chaining
 * these objects in their respective lists.  This has implications both
 * for the layout of a block and managed records.  Ramafications for the
 * latter extend only to how one wishes to return records to the Manager
 * for reallocation: one-at-a-time or list-at-a-time (see below).  The
 * allocation of a block occurs on a needs basis.  Its overall size is
 * determined by the number and size of the records it is to hold.  For
 * convenience, a field for chaining allocated structures is also
 * provided.  Note, however, that the responsibility of maintaining this
 * list rests solely with the user (read client).  Four additional macros
 * are provided for aiding in its maintenance, but it is up to the user to
 * apply them, and in a reasonable way.  Not all records will require this
 * feature, and so this field and these macros can be safely ignored.
 *
 * Use:
 * ---
 * To use, observe the following guidelines:
 *
 * 1) Declare a Structure_Manager variable for your data structure AND
 *    initialize it statically using the macro SM_InitDecl().  Provide
 *    the macro with the type of your record, the number you wish it to
 *    allocate at once, and a string which contains the name of the
 *    structure.  For example:
 *
 *         Structure_Manager mySM = SM_InitDecl(int,5,"C integer");
 *
 * 2) To obtain a new structure from your Manager, make a call to
 *    SM_AllocateStruct():
 *
 *         SM_AllocateStruct(mySM,pNewStruct)
 *
 *    If chaining of allocated structures is desired, then one can use
 *    macros SM_AddToAllocList_SL/DL to add the returned record to the
 *    head of this list.  One must define field-access macro(s) to the
 *    link field(s) of this record type for use by these list-creating
 *    macros.
 *
 * 3) If mass deallocation of a list of records is desired, then make
 *    the first field in your record the link field for supporting the
 *    list.  You may then use macro SM_DeallocateStructList():
 *
 *         SM_DeallocateStructList(mySM,pListHead,pListTail)
 *
 *    Otherwise, you'll have to deallocate them one at a time using
 *    SM_DeallocateStruct().  If these structs are maintained on the
 *    allocation chain of the Structure Manager, then they should be
 *    removed BEFORE the deallocation.  One may find the macros
 *    SM_RemoveFromAllocList_SL/DL helpful for this...
 *
 * 4) Get info about the state of the Manager using routines
 *    SM_CurrentCapacity(), SM_CountFreeStructs(), and SM_RawUsage().
 *
 * 5) Releasing the resources of the Structure Manager returns the blocks
 *    to the system and resets some of its fields.
 *
 * 6) With possibly the exception of the allocated-record-chain field,
 *    avoid direct manipulatiion of the Structure Manager!  Use the
 *    provided interface routines.
 */

typedef struct Structure_Manager {
  struct {		   /* Current block descriptor: */
    void *pBlock;          /* - current block; head of block chain */
    void *pNextStruct;     /* - next available struct in current block */
    void *pLastStruct;     /* - last struct in current block */
  } cur_block;
  struct {		   /* Structure characteristics: */
    size_t size;	   /* - size of the structure in bytes */
    counter num;	   /* - number of records per block */
    char *name;		   /* - short description of the struct type */
  } struct_desc;
  struct {		   /* Lists of structures: */
    void *alloc;	   /* - convenience hook, not directly maintained */
    void *dealloc;	   /* - deallocated structs, poised for reuse */
  } struct_lists;
} Structure_Manager;


/* Macro Short-Hands  (mainly for "internal" use)
   ----------------- */
#define SM_CurBlock(SM)		((SM).cur_block.pBlock)
#define SM_NextStruct(SM)	((SM).cur_block.pNextStruct)
#define SM_LastStruct(SM)	((SM).cur_block.pLastStruct)
#define SM_StructSize(SM)	((SM).struct_desc.size)
#define SM_StructsPerBlock(SM)	((SM).struct_desc.num)
#define SM_StructName(SM)	((SM).struct_desc.name)
#define SM_AllocList(SM)	((SM).struct_lists.alloc)
#define SM_FreeList(SM)		((SM).struct_lists.dealloc)

#define SM_NewBlockSize(SM)	/* in bytes */			\
   ( sizeof(void *) + SM_StructSize(SM) * SM_StructsPerBlock(SM) )

#define SM_CurBlockIsDepleted(SM)	 \
   ( IsNULL(SM_CurBlock(SM)) || (SM_NextStruct(SM) > SM_LastStruct(SM)) )

#define SM_NumStructsLeftInBlock(SM)					 \
   ( ! SM_CurBlockIsDepleted(SM)					 \
     ? ( ((char *)SM_LastStruct(SM) - (char *)SM_NextStruct(SM))	 \
	 / SM_StructSize(SM) + 1 )					 \
     : 0 )


#define SM_AllocateFree(SM,pNewStruct)				\
   pNewStruct = SM_FreeList(SM);				\
   SM_FreeList(SM) = SMFL_NextFreeStruct(SM_FreeList(SM))

#define SM_AllocateFromBlock(SM,pNewStruct)			\
   pNewStruct = SM_NextStruct(SM);				\
   SM_NextStruct(SM) = SMBlk_NextStruct(SM_NextStruct(SM),SM_StructSize(SM))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

extern void smPrint(Structure_Manager, char *);
extern void smAllocateBlock(Structure_Manager *);
extern void smFreeBlocks(Structure_Manager *);
extern xsbBool smIsValidStructRef(Structure_Manager, void *);
extern xsbBool smIsAllocatedStruct(Structure_Manager, void *);
extern xsbBool smIsAllocatedStructRef(Structure_Manager, void *);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Manipulating the Free List
   -------------------------- */
#define SMFL_NextFreeStruct(pFreeStruct)	( *(void **)(pFreeStruct) )


/* Manipulating Allocation Block
   ----------------------------- */
#define SMBlk_NextBlock(pBlock)		( *(void **)(pBlock) )

#define SMBlk_FirstStruct(pBlock)	( (char *)pBlock + sizeof(void *) )

#define SMBlk_LastStruct(pBlock,StructSize,StructsPerBlock)	\
   ( SMBlk_FirstStruct(pBlock) + StructSize * (StructsPerBlock - 1) )

#define SMBlk_NextStruct(pStruct,StructSize)   ( (char *)pStruct + StructSize )

/*-------------------------------------------------------------------------*/

/* Primary Interface Routines
   ========================== */

/*
 *  For initialization at declaration time.
 */
#define SM_InitDecl(StructType,StructsPerBlock,NameString) {	\
   {NULL, NULL, NULL},						\
   {sizeof(StructType), StructsPerBlock, NameString},		\
   {NULL, NULL}							\
 }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Allocate a structure from the Structure Manager.
 */
#define SM_AllocateStruct(SM,pStruct) {		\
						\
   SYS_MUTEX_LOCK( MUTEX_SM ); 			\
   if ( IsNonNULL(SM_FreeList(SM)) ) {		\
     SM_AllocateFree(SM,pStruct);		\
   }						\
   else {					\
     if ( SM_CurBlockIsDepleted(SM) )		\
       smAllocateBlock(&SM);			\
     SM_AllocateFromBlock(SM,pStruct);		\
   }						\
   SYS_MUTEX_UNLOCK( MUTEX_SM ); 		\
 }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  To deallocate a chain of structures -- of the same type -- at once,
 *  they must ALREADY be linked using the first word in the structure,
 *  since this conforms with the manner in which records on the free
 *  chain are maintained.  Head and Tail are relative to the ordering
 *  imposed by this first-word linkage.  Otherwise, each structure in
 *  the chain must be deallocated individually.
 */
#define SM_DeallocateStructList(SM,pHead,pTail) {	\
   SYS_MUTEX_LOCK( MUTEX_SM ); 				\
   SMFL_NextFreeStruct(pTail) = SM_FreeList(SM);	\
   SM_FreeList(SM) = pHead;				\
   SYS_MUTEX_UNLOCK( MUTEX_SM ); 			\
 }

#define SM_DeallocateStruct(SM,pStruct)		\
   SM_DeallocateStructList(SM,pStruct,pStruct)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Query the manager for the number of allocated blocks and the total
 *  number of records they hold.
 */
#define SM_CurrentCapacity(SM,NumBlocks,NumStructs) {	\
							\
   void *pBlock;					\
							\
   NumBlocks = 0;					\
   for ( pBlock = SM_CurBlock(SM);  IsNonNULL(pBlock);	\
         pBlock = SMBlk_NextBlock(pBlock) )		\
     NumBlocks++;					\
   NumStructs = NumBlocks * SM_StructsPerBlock(SM);	\
 }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Query the manager for the number of unallocated records it
 *  currently has.
 */
#define SM_CountFreeStructs(SM,NumFreeStructs) {		\
								\
   void *pStruct;						\
								\
   if ( IsNonNULL(SM_CurBlock(SM)) ) {				\
     NumFreeStructs = SM_NumStructsLeftInBlock(SM);		\
     for ( pStruct = SM_FreeList(SM);  IsNonNULL(pStruct);	\
	   pStruct = SMFL_NextFreeStruct(pStruct) )		\
       NumFreeStructs++;					\
   }								\
   else								\
     NumFreeStructs = 0;					\
 }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Query the manager for the total number of bytes it obtained from
 *  the system.
 */
#define SM_RawUsage(SM,UsageInBytes) {			\
   		 					\
   void *pBlock;					\
							\
   UsageInBytes = 0;					\
   for ( pBlock = SM_CurBlock(SM);  IsNonNULL(pBlock);	\
         pBlock = SMBlk_NextBlock(pBlock) )		\
     UsageInBytes = UsageInBytes + SM_NewBlockSize(SM);	\
 }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Return all memory blocks to the system.
 */
#define SM_ReleaseResources(SM)		smFreeBlocks(&SM)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Place a newly allocated record on the "in use" list, maintained as
 *  either a singly- or doubly-linked (SL/DL) list.
 */

#define SM_AddToAllocList_SL(SM,pRecord,LinkFieldMacro) {	\
   LinkFieldMacro(pRecord) = SM_AllocList(SM);			\
   SM_AllocList(SM) = pRecord;					\
 }

#define SM_AddToAllocList_DL(SM,pRecord,PrevFieldMacro,NextFieldMacro) { \
   PrevFieldMacro(pRecord) = NULL;					 \
   NextFieldMacro(pRecord) = SM_AllocList(SM);				 \
   SM_AllocList(SM) = pRecord;						 \
   if ( IsNonNULL(NextFieldMacro(pRecord)) )				 \
     PrevFieldMacro(NextFieldMacro(pRecord)) = pRecord;			 \
 }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 *  Prepare a record for deallocation by removing it from the "in use" list.
 */

#define SM_RemoveFromAllocList_SL(SM,pRecord,LinkFieldMacro,RecordType) { \
									  \
   RecordType pCur, pPrev;						  \
									  \
   for ( pPrev = NULL, pCur = SM_AllocList(SM);				  \
	 IsNonNULL(pCur);						  \
	 pPrev = pCur, pCur = LinkFieldMacro(pCur) )			  \
     if ( pCur == pRecord )						  \
       break;								  \
   if ( IsNonNULL(pCur) ) {						  \
     if ( IsNonNULL(pPrev) )						  \
       LinkFieldMacro(pPrev) = LinkFieldMacro(pCur);			  \
     else								  \
       SM_AllocList(SM) = LinkFieldMacro(pCur);				  \
     LinkFieldMacro(pCur) = NULL;					  \
   }									  \
   else									  \
     xsb_warn("Record not present in given Structure Manager: %s",	  \
	      SM_StructName(SM));					  \
 }

#define SM_RemoveFromAllocList_DL(SM,pRecord,PrevFieldMacro,NextFieldMacro) { \
   if ( IsNonNULL(PrevFieldMacro(pRecord)) )				      \
     NextFieldMacro(PrevFieldMacro(pRecord)) = NextFieldMacro(pRecord);	      \
   else {								      \
     if ( SM_AllocList(SM) == pRecord )					      \
       SM_AllocList(SM) = NextFieldMacro(pRecord);			      \
     else								      \
       xsb_abort("Record not present in given Structure Manager: %s",	      \
		 SM_StructName(SM));					      \
   }									      \
   if ( IsNonNULL(NextFieldMacro(pRecord)) )				      \
     PrevFieldMacro(NextFieldMacro(pRecord)) = PrevFieldMacro(pRecord);	      \
   NextFieldMacro(pRecord) = PrevFieldMacro(pRecord) = NULL;		      \
 }

/*===========================================================================*/

#endif
