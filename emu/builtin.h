/* File:      builtin.h
** Author(s): Warren, Xu, Swift, Sagonas, Freire, Rao
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1999
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

#define BUILTIN_TBL_SZ 256

/************************************************************************/
/*	The following is the set of all primitive predicates.		*/
/************************************************************************/

#define PSC_NAME	 1
#define PSC_ARITY	 2
#define PSC_TYPE	 3
#define PSC_PROP	 4
#define PSC_SET_TYPE	 5
#define PSC_SET_PROP	 6

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

#define CALL0		31
/* some other builtins that might need hard implementation */
#define STAT_STA	32
#define STAT_CPUTIME	33
#define CODE_LOAD	34
#define BUFF_SET_VAR	35
#define BUFF_DEALLOC	36
#define BUFF_CELL	37
#define BUFF_SET_CELL	38
#define COPY_TERM0	39
/* check for substring */
#define STR_SUB	        40
#define DIRNAME_CANONIC 41
/* for efficiency reasons, the following predicates are also implemented */
#define PSC_INSERT	42
#define PSC_IMPORT	43

#define PSC_INSERTMOD	46

#define FILE_GETTOKEN	48
#define FILE_PUTTOKEN	49
#define TERM_HASH	50
#define UNLOAD_SEG	51
#define LOAD_OBJ	52

#define GETENV			 54
#define SYS_SYSCALL		 55
#define SYS_SYSTEM		 56
#define SYS_GETHOST		 57
#define SYS_ERRNO		 58

#define FILE_STAT		 60
#define FILE_WRITEQUOTED	 61
#define FAST_GROUND		 62

#define INTERN_STRING            65
#define EXPAND_FILENAME 	 66
#define TILDE_EXPAND_FILENAME    67
#define IS_ABSOLUTE_FILENAME     68
#define PARSE_FILENAME        	 69

#define PSC_ENV		        100
#define PSC_SPY		        101
#define PSC_TABLED	        102

#define IS_INCOMPLETE           104
#define GET_OSP_BREG            105
#define CUT_IF_LEADER           106
#define GET_PTCP	        107
#define GET_SUBGOAL_PTR	        108
#define DEREFERENCE_THE_BUCKET	109
#define PAIR_PSC		110
#define PAIR_NEXT		111
#define NEXT_BUCKET		112

#define SLG_NOT			114

#define IS_XWAMMODE             117
#define CLOSE_OPEN_TABLES       118

#define FILE_FUNCTION           123
#define SLASH_BUILTIN           124

#define ABOLISH_TABLE_INFO      126
#define ZERO_OUT_PROFILE        127
#define WRITE_OUT_PROFILE       128
#define ASSERT_CODE_TO_BUFF	129
#define ASSERT_BUFF_TO_CLREF	130

#define FILE_READ_CANONICAL	133
#define GEN_RETRACT_ALL		134
#define COMPILED_TO_DYNAMIC	135
#define DB_RETRACT0		136
#define DB_GET_CLAUSE		137
#define DB_BUILD_PRREF		138
#define DB_REMOVE_PRREF		139
#define DB_RECLAIM0		140

#define FORMATTED_IO            142
#define TABLE_STATUS            143
#define GET_DELAY_LISTS		144

#define ABOLISH_TABLE_PREDICATE 146
#define TRIE_ASSERT		147
#define TRIE_RETRACT		148
#define TRIE_DELETE_TERM	149
#define TRIE_GET_RETURN		150
#define TRIE_GET_CALL		151
#define GET_LASTNODE_CS_RETSKEL 152
#define CONSTRUCT_RET_FOR_CALL  153
#define BREG_RETSKEL		154
#define TRIE_RETRACT_SAFE	155

#define TRIMCORE		158
#define NEWTRIE                 159
#define TRIE_INTERN             160
#define TRIE_INTERNED           161
#define TRIE_DISPOSE            162
#define BOTTOM_UP_UNIFY         163
#define DELETE_TRIE             164
#define TRIE_DISPOSE_NR         165
#define TRIE_UNDISPOSE          166

#define SET_SUBSUMPTIVE_EVAL	170

/* This is the builtin where people should put their private, experimental
   builtin code. SEE THE EXAMPLE IN private_builtin.c to UNDERSTAND HOW TO DO
   IT. Note: even though this is a single builtin, YOU CAN SIMULATE ANY NUMBER
   OF BUILTINS WITH IT.  */
#define PRIVATE_BUILTIN	        180

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

#define HiLog_ARG		204
#define HiLog_UNIV		205

#define ATOM_CODES		207
#define ATOM_CHARS		208
#define NUMBER_CHARS		209
#define PUT			210
#define TAB			211
#define NUMBER_CODES		212
#define IS_CHARLIST		213

#define SORT			220
#define KEYSORT			221

#define ORACLE_QUERY		230
#define ODBC_EXEC_QUERY		231

/* added by Bart Demoen & Kostis Sagonas for debugging and convenience */
#define PRINT_CHAT              239
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
#define FORCE_TRUTH_VALUE	254
