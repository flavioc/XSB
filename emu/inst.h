/* File:      inst.h
** Author(s): Warren, Swift, Xu, Sagonas, Freire, Johnson, Rao
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


extern void init_inst_table(void);

/************************************************************************/
/*	The following are operand types of instructions.		*/
/************************************************************************/

#define A 1	/* a byte of integer (for arity, size, builtin-num, etc) */
#define V 2	/* variable offset */
#define R 3	/* register number */
#define S 4	/* structure symbol */
#define C 5	/* constant symbol */
#define L 6	/* label (address) */
#define G 7	/* string */
/*#define N 8	 number (integer and float) */
#define N 8	/* number (integer - rfm) */
#define I 9	/* 2nd & 3rd arguments of switchonbound */
#define P 10	/* pad */
#define X 11	/* not present */
#define PP 12	/* double pad */
#define PPP 13	/* triple pad */
#define PPR 14  /* = PP + R; for switchonterm and switchonbound */
#define T 15    /* tabletry */
#define RRR 16  /* = R + R + R; for switchon3bound */
#define F 17    /* floating point number */

/************************************************************************/
/*	Macros to fetch the instructions/operands.			*/
/************************************************************************/

#define cell_opcode(C)		(*(pb)(C))

#define cell_opregaddr1(C)	(rreg+((pb)(cell))[1])
#define cell_opregaddr2(C)	(rreg+((pb)(cell))[2])
#define cell_opregaddr3(C)	(rreg+((pb)(cell))[3])
#define cell_opregaddrn(C,N)	(rreg+((pb)(cell))[N])

#define cell_opreg1(C)		(cell(opregaddr1(C)))
#define cell_opreg2(C)		(cell(opregaddr2(C)))
#define cell_opreg3(C)		(cell(opregaddr3(C)))
#define cell_opregn(C)		(cell(opregaddrn(C,N)))

#define cell_operand1(C)	(((pb)(C))[1])
#define cell_operand2(C)	(((pb)(C))[2])
#define cell_operand3(C)	(((pb)(C))[3])
#define cell_operandn(C,N)	(((pb)(C))[N])

/* Unused right now, but may come handy in future. */

/*
#define get_code_cell(pc)			(*(pc)++)
*/

/* bit fields */

/*
#define get_last__8bits(cell)		(((Cell)(cell))&0xff)
#define get_last_16bits(cell)		(((Cell)(cell))&0xffff)
#define get_last_32bits(cell)		(((Cell)(cell))&0xffffffff)

#define get_first__8bits(cell)		(((Cell)(cell))>>(sizeof(Cell)*8-8))
#define get_first_16bits(cell)		(((Cell)(cell))>>(sizeof(Cell)*8-16))
#define get_first_32bits(cell)		(((Cell)(cell))>>(sizeof(Cell)*8-32))

#define get__8bit_field(cell, pos)	\
		(((Cell)(cell))>>((sizeof(Cell)-pos-1)*8)&0xff)
#define get_16bit_field(cell, pos)	\
		(((Cell)(cell))>>((sizeof(Cell)-pos-1)*16)&0xffff)
#define get_32bit_field(cell, pos)	\
		(((Cell)(cell))>>((sizeof(Cell)-pos-1)*32)&0xffffffff)
*/

/* opcode & operand fields */

/*
#ifdef BITS64
#define opquarter0(cell)		(get_first_16bits(cell))
#define opquarter1(cell)		(get_16bit_field(cell,1))
#define opquarter2(cell)		(get_16bit_field(cell,2))
#define opquarter3(cell)		(get_last_16bits(cell))
#define opquartern(cell,N)		(get_16bit_field(cell,N))
#else
#define opquarter0(cell)		(get_first__8bits(cell))
#define opquarter1(cell)		(get__8bit_field(cell,1))
#define opquarter2(cell)		(get__8bit_field(cell,2))
#define opquarter3(cell)		(get_last__8bits(cell))
#define opquartern(cell,N)		(get__8bit_field(cell,N))
#endif
#define opcell(cell)			((Cell)(cell))
#define opint(cell)			((Integer)(cell))

#define opcode(cell)			((short)opquarter0(cell))

#define opregaddr1(cell)		(rreg+opquarter1(cell))
#define opregaddr2(cell)		(rreg+opquarter2(cell))
#define opregaddr3(cell)		(rreg+opquarter3(cell))
#define opregaddrn(cell,N)		(rreg+opquartern(cell,N))

#define opreg1(cell)			(cell(opregaddr1(cell)))
#define opreg2(cell)			(cell(opregaddr2(cell)))
#define opreg3(cell)			(cell(opregaddr3(cell)))
#define opregn(cell)			(cell(opregaddrn(cell,N)))

#define operand1(cell)			((short)opquarter1(cell))
#define operand2(cell)			((short)opquarter2(cell))
#define operand3(cell)			((short)opquarter3(cell))
#define operandn(cell,N)		((short)opquartern(cell,N))
*/


/*
#define opvaraddr(cell)			(ereg+(-(Cell)(cell)))
*/
/* 64 bit could benefit from change in compiler/loader for the V operands */

/*
#define opvaraddr1(cell)	(ereg+(-(unsigned int)(opquarter1(cell))))
#define opvaraddr2(cell)	(ereg+(-(unsigned int)(opquarter2(cell))))
#define opvaraddr3(cell)	(ereg+(-(unsigned int)(opquarter3(cell))))
#define opvaraddrn(cell,N)	(ereg+(-(unsigned int)(opquartern(cell,n))))

#define opvar1(cell)			(cell(opvaraddr1(cell)))
#define opvar2(cell)			(cell(opvaraddr2(cell)))
#define opvar3(cell)			(cell(opvaraddr3(cell)))
#define opvarn(cell,N)			(cell(opvaraddrn(cell,N)))
*/


#define BUILTIN_TBL_SZ 256

#ifdef PROFILE
extern Cell inst_table[BUILTIN_TBL_SZ][6];
extern Cell builtin_table[BUILTIN_TBL_SZ][2];
#else
extern Cell inst_table[BUILTIN_TBL_SZ][5];
extern Cell builtin_table[BUILTIN_TBL_SZ][2];
#endif

/************************************************************************/
/*	The following is the set of all instructions.			*/
/************************************************************************/

/* Basic term instructions */

#define getpvar         0x00
#define getpval         0x01
#define getstrv         0x02
#define gettval         0x03
#define getcon          0x04
#define getnil          0x05
#define getstr          0x06
#define getlist         0x07
#define unipvar         0x08
#define unipval         0x09
#define unitvar         0x0a
#define unitval         0x0b
#define unicon          0x0c
#define uninil          0x0d
#define getnumcon	0x0e
#define putnumcon	0x0f
#define putpvar         0x10
#define putpval         0x11
#define puttvar         0x12
#define putstrv         0x13
#define putcon          0x14
#define putnil          0x15
#define putstr          0x16
#define putlist         0x17
#define bldpvar         0x18
#define bldpval         0x19
#define bldtvar         0x1a
#define bldtval         0x1b
#define bldcon          0x1c
#define bldnil          0x1d
#define uninumcon	0x1e
#define bldnumcon	0x1f

#define getlist_tvar_tvar	0x48

/*----- Instructions for tries as code (Do NOT change the numbers) -----*/

#define trie_no_cp_str		0x60
#define trie_trust_str		0x61
#define trie_try_str		0x62
#define trie_retry_str		0x63

#define trie_no_cp_list		0x64
#define trie_trust_list		0x65
#define trie_try_list		0x66
#define trie_retry_list		0x67

#define trie_no_cp_var		0x68
#define trie_trust_var		0x69
#define trie_try_var		0x6a
#define trie_retry_var		0x6b

#define trie_no_cp_val		0x6c
#define trie_trust_val		0x6d
#define trie_try_val		0x6e
#define trie_retry_val		0x6f

#define trie_no_cp_numcon	0x70
#define trie_trust_numcon	0x71
#define trie_try_numcon		0x72
#define trie_retry_numcon	0x73

#define trie_no_cp_numcon_succ	0x74
#define trie_trust_numcon_succ	0x75
#define trie_try_numcon_succ	0x76
#define trie_retry_numcon_succ	0x77

#define trie_proceed		0x78
#define hash_opcode 		0x79
#define hash_handle 		0x7a
#define trie_assert_inst	0x7c

/* jf: reclaim deleted returns at completion */
#define trie_no_cp_fail         0x90
#define trie_trust_fail         0x91
#define trie_try_fail           0x92
#define trie_retry_fail         0x93

/*----------------------------------------------------------------------*/

#define getfloat	0x80
#define putfloat	0x81
#define unifloat	0x82
#define bldfloat	0x83

/* Non-determinism instructions */

#define trymeelse       0xa0
#define retrymeelse     0xa1
#define trustmeelsefail 0xa2
#define try             0xa3
#define retry           0xa4
#define trust           0xa5
#define getpbreg        0xa6
#define gettbreg	0xa7
#define putpbreg	0xa8
#define puttbreg	0xa9
#define jumptbreg	0xaa

#define getVn	        0xab      /* for tabled predicates */
#define test_heap       0xac      /* for heap overflow testing */

/* Indexing instructions */

#define switchonterm    0xb0
#define switchonbound	0xb3
#define switchon3bound	0xb4

/* Instructions to compile body ors	*/

#define trymeorelse		0xb7
#define retrymeorelse		0xb8
#define trustmeorelsefail	0xb9

#define dyntrustmeelsefail	0xba	/* Dynamic trust instruction */

/* Tabling instructions */

#define tableretry		0xbd
#define tabletry		0xbe

#define tabletrust		0xc1
#define tabletrysingle		0xc5

#define answer_return		0xc7

#define check_complete		0xc9
#define resume_compl_suspension 0xca

#define new_answer_dealloc	0xce

#define term_comp		0xd0

/* Numeric instructions */

#define movreg          0xd1
#define negate		0xd2
#define and 		0xd3
#define or 		0xd4
#define lshiftl		0xd5
#define lshiftr		0xd6
#define addreg          0xd7
#define subreg          0xd8
#define mulreg          0xd9
#define divreg          0xda
#define idivreg		0xdb
#define int_test_z	0xdc
#define int_test_nz	0xdd

/* Unsafe term instructions */

#define putdval         0xe0
#define putuval         0xe1

/* Procedure instructions */

#define allocate_gc     0xe7
#define call            0xe8
#define allocate        0xe9
#define deallocate      0xea
#define proceed         0xeb
#define execute         0xec
#define calld           0xef

/* Branching instructions */

#define jump            0xf0
#define jumpz           0xf1
#define jumpnz          0xf2
#define jumplt          0xf3
#define jumple          0xf4
#define jumpgt          0xf5
#define jumpge          0xf6

/* Miscellaneous instructions */

#define cases		0xf7
#define fail            0xf8
#define noop            0xf9
#define halt            0xfa
#define builtin         0xfb
#define unifunc		0xfc
#define userfunc	0xfd
#define endfile         0xff
   /* virtual instruction, used for disassembler to link different segs */


/************************************************************************/
/*	The following is the set of all primitive predicates.		*/
/************************************************************************/

#define PSC_NAME	 1
#define PSC_ARITY	 2
#define PSC_TYPE	 3
#define PSC_PROP	 4
#define PSC_SET_TYPE	 5
#define PSC_SET_PROP	 6
#define FILE_OPEN	 7
#define FILE_CLOSE	 8
#define FILE_GET	 9
#define FILE_PUT	10
#define TERM_PSC	11
#define TERM_TYPE	12
#define TERM_COMPARE	13
#define TERM_NEW	14
#define TERM_ARG	15
#define TERM_SET_ARG	16
#define STAT_FLAG	17
#define STAT_SET_FLAG	18
#define BUFF_ALLOC	19
#define BUFF_WORD	20
#define BUFF_SET_WORD	21
#define BUFF_BYTE	22
#define BUFF_SET_BYTE	23
#define CODE_CALL	24
#define STR_LEN		25
#define STR_CAT		27
#define STR_CMP		28
#define STR_HSH		29
#define STR_INSERT	30
#define CALL0		31

/* some other builtins that might need hard implementation */

#define STAT_STA	32
#define STAT_CPUTIME	33
#define CODE_LOAD	34
#define BUFF_SET_VAR	35
#define BUFF_DEALLOC	36
#define BUFF_CELL	37
#define BUFF_SET_CELL	38
#define COPY_TERM	39

/* check for substring */
#define STR_SUB	        40
#define DIRNAME_CANONIC 41

/* for efficiency reasons, the following predicates are also implemented */

#define PSC_INSERT	42
#define PSC_IMPORT	43
#define FILE_GETBUF	44
#define FILE_PUTBUF	45
#define PSC_INSERTMOD	46
#define LOAD_SEG	47
#define FILE_GETTOKEN	48
#define FILE_PUTTOKEN	49
#define TERM_HASH	50
#define UNLOAD_SEG	51
#define LOAD_OBJ	52

#define GETENV			54
#define SYS_SYSCALL		55
#define SYS_SYSTEM		56
#define SYS_GETHOST		57
#define SYS_ERRNO		58
#define FILE_STAT		60
#define FILE_WRITEQUOTED	61
#define FAST_GROUND		62
#define FILE_POS                64
#define INTERN_STRING           65
#define EXPAND_FILENAME 	66
#define TILDE_EXPAND_FILENAME   67
#define IS_ABSOLUTE_FILENAME    68
#define PARSE_FILENAME        	69

#define PSC_ENV		100
#define PSC_SPY		101
#define PSC_TABLED	102
#define TIP_PROP	103
#define IS_INCOMPLETE   104
#define GET_OSP_BREG    105
#define CUT_IF_LEADER   106
#define GET_PTCP	107
#define GET_SUBGOAL_PTR	108

#define DEREFERENCE_THE_BUCKET	109
#define PAIR_PSC		110
#define PAIR_NEXT		111
#define NEXT_BUCKET		112

#define SLG_NOT			114

#define IS_XWAMMODE             117
#define CLOSE_OPEN_TABLES       118

#define PRINT_PREDICATE_TABLE   120

#define FILE_READ_LINE          123
#define SLASH_BUILTIN           124
#define FMT_WRITE_STRING        125

#define ABOLISH_TABLE_INFO      126
#define ZERO_OUT_PROFILE        127
#define WRITE_OUT_PROFILE       128
#define ASSERT_CODE_TO_BUFF	129
#define ASSERT_BUFF_TO_CLREF	130
#define FMT_WRITE		131
#define FMT_READ		132
#define FILE_READ_CANONICAL	133
#define GEN_RETRACT_ALL		134
#define COMPILED_TO_DYNAMIC	135
#define DB_RETRACT0		136
#define DB_GET_CLAUSE		137
#define DB_BUILD_PRREF		138
#define DB_REMOVE_PRREF		139

#define TRIE_NODE_ELEMENT	140
#define PROLOG_NEWNODE		141

#define TABLE_STATUS            143
#define GET_DELAY_LISTS		144
#define DELETE_PREDICATE_TABLE	145

#define TRIE_ASSERT		147
#define TRIE_RETRACT		148
#define TRIE_DELETE_TERM	149
#define TRIE_GET_RETURN		150
#define TRIE_GET_CALL		151
#define GET_LASTNODE_CS_RETSKEL 152
#define CONSTRUCT_RET_FOR_CALL  153

#define BREG_RETSKEL		154
#define TRIE_RETRACT_SAFE	155
#define GET_EMU_DEPENDENT_CONST	156
#define TRIMCORE		158

#define NEWTRIE                 159
#define TRIE_INTERN             160
#define TRIE_INTERNED           161
#define TRIE_DISPOSE            162
#define DELETE_TRIE             164
#define BOTTOM_UP_UNIFY         163

#define VAR			190
#define NONVAR			191
#define ATOM			192
#define INTEGER			193
#define REAL			194
#define NUMBER			195
#define ATOMIC			196
#define COMPOUND		197
#define CALLABLE		198
#define IS_LIST			199

#define FUNCTOR			200
#define ARG			201
#define UNIV			202
#define MY_HiLog_FUNCTOR	203
#define HiLog_ARG		204
#define HiLog_UNIV		205
#define MY_COPY_TERM		206
#define MY_NAME			207
#define ATOM_CHARS		208
#define NUMBER_CHARS		209

#define PUT			210
#define TAB			211

#define SORT			220
#define KEYSORT			221

#define ORACLE_QUERY		230
#define CASP_QUERY		231
#define ODBC_QUERY		239

/* added by Bart Demoen & Kostis Sagonas for debugging and convenience */
#define PRINT_LS                240
#define PRINT_TR                241
#define PRINT_HEAP              242
#define PRINT_CP                243
#define PRINT_REGS              244
#define PRINT_ALL_STACKS        245
#define EXP_HEAP                246
#define MARK_HEAP               247
#define GC_HEAP                 248

#define FINDALL_INIT		249
#define FINDALL_ADD		250
#define FINDALL_GET_SOLS	251

#define SOCKET_REQUEST          252

#define JAVA_INTERRUPT          253
