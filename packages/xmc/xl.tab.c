
/*  A Bison parser, made from xl.y with Bison version GNU Bison version 1.22
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	DEFINE	258
#define	MOD	259
#define	EQ_EQ	260
#define	NOT_EQ	261
#define	PREF	262
#define	IF	263
#define	THEN	264
#define	ELSE	265
#define	ID	266
#define	VAR	267
#define	SYNTAX_ERROR	268
#define	ASSIGN	269
#define	NOT_EQ_EQ	270
#define	PLUS_EQ	271
#define	MINUS_EQ	272
#define	IS	273
#define	INT_CONST	274
#define	COL_HY	275
#define	PREDICATE	276
#define	FROM	277
#define	GREATER_EQ	278
#define	LESSER_EQ	279
#define	AND	280
#define	OR	281
#define	TRUE	282
#define	FALSE	283
#define	CHANNEL	284
#define	DIAM_ALL	285
#define	DIAM_MINUS	286
#define	OP	287
#define	DIV	288
#define	DIAM_MIMUS	289

#line 1 "xl.y"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "attr.h"

  /* #define YYERROR_VERBOSE*/

extern FILE* yyout;

extern int yyerror(char *);     /* defined in driver.c */
 extern int yylex();             /* defined in lex.yy.c */
static char buffer[4096];
static int arity;		/* to count the predicate's arity */

void warning(char*, char*);

void set(AttrType*, char*, AttrType*, AttrType*);
void infix(char* op, AttrType* a0, AttrType* a1, AttrType* a2);
void comp1(AttrType*, AttrType*, AttrType*, char*, AttrType*);
void comp2(AttrType*, AttrType*, AttrType*, char*, AttrType*, AttrType*);
void compproc(AttrType*, AttrType*, AttrType*, char*, AttrType*, AttrType*);
void compproc3(AttrType*, AttrType*, AttrType*, char*, AttrType*, AttrType*, AttrType*);

/* variable management */
#define TYPE_UNDEFINED	((char*)0)
#define TYPE_TYPE	((char*)1)

void add_variable(char*, char*);
void dump_variables();


#line 105 "xl.y"
typedef union{
    AttrType attr;
} YYSTYPE;

#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		234
#define	YYFLAG		-32768
#define	YYNTBASE	57

#define YYTRANSLATE(x) ((unsigned)(x) <= 289 ? yytranslate[x] : 86)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    28,     2,    23,     2,     2,     2,     2,    10,
    11,    16,    14,     3,    15,     7,    20,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     5,     4,    43,
    32,    42,    27,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     8,     2,     9,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    12,    22,    13,    46,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     6,    17,    18,
    19,    21,    24,    25,    26,    29,    30,    31,    33,    34,
    35,    36,    37,    38,    39,    40,    41,    44,    45,    47,
    48,    49,    50,    51,    52,    53,    54,    55,    56
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     4,     7,     9,    11,    13,    15,    18,    24,
    28,    30,    34,    36,    41,    43,    47,    51,    58,    60,
    64,    66,    70,    72,    74,    79,    81,    85,    90,    95,
    97,    99,   103,   107,   111,   115,   122,   127,   131,   135,
   139,   143,   147,   149,   151,   156,   158,   160,   162,   165,
   169,   175,   181,   183,   187,   189,   193,   195,   198,   201,
   204,   208,   212,   216,   220,   224,   228,   232,   236,   240,
   244,   248,   252,   256,   260,   264,   268,   272,   276,   280,
   285,   290,   295,   297,   301,   305,   310,   313,   318,   323,
   325,   327,   329,   333,   338,   340,   344,   348,   353,   356,
   361,   366,   368,   370,   372,   376,   379,   381,   383,   385,
   389,   394,   396,   398,   400,   402
};

static const short yyrhs[] = {    58,
     0,    59,     0,    58,    59,     0,    69,     0,    60,     0,
    64,     0,    77,     0,     1,     7,     0,    40,    61,    41,
    29,     7,     0,    40,    61,     7,     0,    62,     0,    61,
     3,    62,     0,    29,     0,    29,    10,    63,    11,     0,
    67,     0,    63,     3,    67,     0,    51,    29,     7,     0,
    51,    29,    10,    65,    11,     7,     0,    66,     0,    65,
     3,    66,     0,    30,     0,    30,     5,    67,     0,    29,
     0,    30,     0,    29,    10,    68,    11,     0,    67,     0,
    68,     3,    67,     0,    70,     6,    71,     7,     0,    29,
    10,    65,    11,     0,    29,     0,    76,     0,    72,    27,
    73,     0,    72,    27,    16,     0,    72,    28,    73,     0,
    72,    28,    16,     0,    24,    76,    25,    71,    26,    71,
     0,    24,    76,    25,    71,     0,    71,    21,    71,     0,
    71,     4,    71,     0,    71,    23,    71,     0,    71,    22,
    71,     0,    12,    71,    13,     0,    66,     0,    29,     0,
    29,    10,    75,    11,     0,    29,     0,    66,     0,    38,
     0,     8,     9,     0,     8,    74,     9,     0,     8,    74,
    22,    73,     9,     0,    10,    73,     3,    74,    11,     0,
    73,     0,    74,     3,    73,     0,    76,     0,    75,     3,
    76,     0,    73,     0,    14,    76,     0,    15,    76,     0,
    46,    76,     0,    76,    14,    76,     0,    76,    15,    76,
     0,    76,    16,    76,     0,    76,    17,    76,     0,    76,
    55,    76,     0,    76,    20,    76,     0,    76,    43,    76,
     0,    76,    42,    76,     0,    76,    44,    76,     0,    76,
    45,    76,     0,    76,    32,    76,     0,    76,    18,    76,
     0,    76,    19,    76,     0,    76,    34,    76,     0,    76,
    47,    76,     0,    76,    48,    76,     0,    76,    33,    76,
     0,    76,    37,    76,     0,    10,    76,    11,     0,    78,
    35,    79,     7,     0,    78,    36,    81,     7,     0,    29,
    10,    65,    11,     0,    29,     0,    79,    47,    79,     0,
    79,    48,    79,     0,    43,    82,    42,    79,     0,    52,
    79,     0,    53,    82,    42,    79,     0,     8,    82,     9,
    79,     0,    49,     0,    50,     0,    78,     0,    10,    79,
    11,     0,    29,    10,    65,    11,     0,    29,     0,    81,
    47,    81,     0,    81,    48,    81,     0,    43,    82,    42,
    81,     0,    52,    81,     0,    53,    82,    42,    81,     0,
     8,    82,     9,    81,     0,    49,     0,    50,     0,    80,
     0,    10,    81,    11,     0,    15,    83,     0,    15,     0,
    83,     0,    84,     0,    12,    85,    13,     0,    29,    10,
    85,    11,     0,    29,     0,    30,     0,    38,     0,    84,
     0,    85,     3,    84,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   165,   168,   169,   172,   173,   174,   175,   176,   180,   186,
   192,   193,   197,   199,   208,   209,   215,   220,   228,   229,
   233,   235,   239,   240,   243,   247,   248,   254,   263,   265,
   268,   270,   271,   272,   273,   275,   279,   282,   284,   285,
   286,   287,   290,   291,   294,   296,   297,   298,   299,   300,
   301,   304,   309,   310,   313,   314,   317,   318,   319,   320,
   321,   322,   323,   324,   325,   326,   327,   328,   329,   330,
   331,   332,   333,   334,   336,   338,   339,   340,   341,   345,
   357,   374,   376,   379,   380,   381,   383,   384,   386,   389,
   390,   391,   392,   395,   397,   400,   402,   404,   406,   408,
   410,   412,   413,   414,   415,   418,   419,   420,   423,   424,
   427,   436,   443,   444,   447,   448
};

static const char * const yytname[] = {   "$","error","$illegal.","','","';'",
"':'","DEFINE","'.'","'['","']'","'('","')'","'{'","'}'","'+'","'-'","'*'","MOD",
"EQ_EQ","NOT_EQ","'/'","PREF","'|'","'#'","IF","THEN","ELSE","'?'","'!'","ID",
"VAR","SYNTAX_ERROR","'='","ASSIGN","NOT_EQ_EQ","PLUS_EQ","MINUS_EQ","IS","INT_CONST",
"COL_HY","PREDICATE","FROM","'>'","'<'","GREATER_EQ","LESSER_EQ","'~'","AND",
"OR","TRUE","FALSE","CHANNEL","DIAM_ALL","DIAM_MINUS","OP","DIV","DIAM_MIMUS",
"prog","speclist","spec","preddecl","predlist","predtype","predtypelist","cdecl",
"vardecllist","vardecl","type","typelist","pdefn","pname","pexp","cname","term",
"termlist","explist","exp","fdefn","fterm","fexp","fterm_ng","fexp_ng","modality",
"posmodal","unit","unitlist",""
};
#endif

static const short yyr1[] = {     0,
    57,    58,    58,    59,    59,    59,    59,    59,    60,    60,
    61,    61,    62,    62,    63,    63,    64,    64,    65,    65,
    66,    66,    67,    67,    67,    68,    68,    69,    70,    70,
    71,    71,    71,    71,    71,    71,    71,    71,    71,    71,
    71,    71,    72,    72,    73,    73,    73,    73,    73,    73,
    73,    73,    74,    74,    75,    75,    76,    76,    76,    76,
    76,    76,    76,    76,    76,    76,    76,    76,    76,    76,
    76,    76,    76,    76,    76,    76,    76,    76,    76,    77,
    77,    78,    78,    79,    79,    79,    79,    79,    79,    79,
    79,    79,    79,    80,    80,    81,    81,    81,    81,    81,
    81,    81,    81,    81,    81,    82,    82,    82,    83,    83,
    84,    84,    84,    84,    85,    85
};

static const short yyr2[] = {     0,
     1,     1,     2,     1,     1,     1,     1,     2,     5,     3,
     1,     3,     1,     4,     1,     3,     3,     6,     1,     3,
     1,     3,     1,     1,     4,     1,     3,     4,     4,     1,
     1,     3,     3,     3,     3,     6,     4,     3,     3,     3,
     3,     3,     1,     1,     4,     1,     1,     1,     2,     3,
     5,     5,     1,     3,     1,     3,     1,     2,     2,     2,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     4,
     4,     4,     1,     3,     3,     4,     2,     4,     4,     1,
     1,     1,     3,     4,     1,     3,     3,     4,     2,     4,
     4,     1,     1,     1,     3,     2,     1,     1,     1,     3,
     4,     1,     1,     1,     1,     3
};

static const short yydefact[] = {     0,
     0,    83,     0,     0,     0,     2,     5,     6,     4,     0,
     7,     0,     8,     0,    13,     0,    11,     0,     3,     0,
     0,     0,    21,     0,    19,     0,     0,    10,     0,    17,
     0,     0,     0,     0,     0,     0,     0,    46,    48,     0,
    47,     0,     0,    57,    31,     0,     0,    83,     0,    90,
    91,     0,     0,    92,     0,     0,     0,    95,     0,   102,
   103,     0,     0,   104,     0,     0,     0,    82,    23,    24,
     0,    15,    12,     0,     0,    49,     0,    46,    47,    53,
     0,    57,     0,     0,    58,    59,     0,     0,    60,     0,
    28,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   107,   112,   113,   114,     0,
   108,   109,     0,     0,     0,    87,     0,    80,     0,     0,
     0,     0,     0,     0,    99,     0,    81,     0,     0,    22,
    20,     0,     0,    14,     9,     0,     0,     0,    50,     0,
     0,    79,    42,     0,     0,    55,    39,    38,    41,    40,
    33,    32,    35,    34,    61,    62,    63,    64,    72,    73,
    66,    71,    77,    74,    78,    68,    67,    69,    70,    75,
    76,    65,   115,     0,   106,     0,     0,    93,     0,     0,
     0,    84,    85,     0,   105,     0,     0,     0,    96,    97,
    26,     0,    16,    18,    54,     0,     0,    37,     0,    45,
     0,   110,     0,    89,    82,    86,    88,   101,    94,    98,
   100,     0,    25,    51,    52,     0,    56,   116,   111,    27,
    36,     0,     0,     0
};

static const short yydefgoto[] = {   232,
     5,     6,     7,    16,    17,    71,     8,    24,    79,    72,
   202,     9,    10,    42,    43,    44,    81,   155,    45,    11,
    54,    55,    64,    65,   120,   121,   122,   184
};

static const short yypact[] = {     7,
    12,     8,    83,   105,    15,-32768,-32768,-32768,-32768,   137,
-32768,     6,-32768,   122,   126,    30,-32768,   160,-32768,   379,
   101,   106,   152,    32,-32768,    88,    83,-32768,   142,-32768,
   122,   201,   382,   379,   382,   382,   382,    22,-32768,   382,
   120,    41,   195,-32768,   242,   150,   101,   172,   150,-32768,
-32768,   101,   150,-32768,     4,   150,   106,   177,   150,-32768,
-32768,   106,   150,-32768,    20,    88,   122,   191,   198,-32768,
   121,-32768,-32768,   217,   130,-32768,   243,   216,-32768,-32768,
   104,   235,   158,   116,    14,    14,   200,   382,   333,   379,
-32768,   379,   379,   379,   394,   397,   382,   382,   382,   382,
   382,   382,   382,   382,   382,   382,   382,   382,   382,   382,
   382,   382,   382,   382,    93,   156,   230,-32768,-32768,   232,
-32768,-32768,    -8,   122,   204,-32768,   208,-32768,   101,   101,
   254,    27,   122,   210,-32768,   212,-32768,   106,   106,-32768,
-32768,    88,    88,-32768,-32768,   257,   235,   243,-32768,   243,
   243,-32768,-32768,   379,   178,   242,     0,     0,   162,   245,
-32768,-32768,-32768,-32768,    14,    14,-32768,-32768,   340,   340,
-32768,   340,   284,   340,   284,   340,   340,   340,   340,   333,
   291,-32768,-32768,    43,-32768,    93,   101,-32768,   193,   101,
   101,-32768,   222,   106,-32768,   196,   106,   106,-32768,   223,
-32768,   218,-32768,-32768,-32768,   256,   224,   251,   382,-32768,
    93,-32768,   225,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,    88,-32768,-32768,-32768,   379,   242,-32768,-32768,-32768,
-32768,   271,   278,-32768
};

static const short yypgoto[] = {-32768,
-32768,   275,-32768,-32768,   255,-32768,-32768,   -18,   -14,   -61,
-32768,-32768,-32768,   -33,-32768,   -23,   132,-32768,   -11,-32768,
    65,   -45,-32768,   -34,   380,   175,  -103,   102
};


#define	YYLAST		443


static const short yytable[] = {    25,
    84,   123,   188,    90,   140,    41,   126,     1,    80,    82,
   128,   183,    75,   -30,    -1,     1,    25,    14,    13,    41,
    92,    83,   132,    85,    86,    87,   137,   135,    89,    99,
   100,    88,    27,   103,    67,     2,    28,   195,   129,   130,
    21,    22,    68,     2,    90,   211,     3,    91,   -44,   -44,
   129,   130,   141,   147,     3,   212,   157,     4,   158,   159,
   160,    92,    93,    94,    12,     4,   138,   139,   114,    12,
    29,   162,   164,   138,   139,    41,   156,    41,    41,    41,
   201,   203,   183,   192,   193,   165,   166,   167,   168,   169,
   170,   171,   172,   173,   174,   175,   176,   177,   178,   179,
   180,   181,   182,   199,   200,   189,   148,   228,    46,    25,
    47,    15,   149,    56,   196,    57,    69,    70,    25,    90,
   208,   117,   118,   143,   205,   150,   206,    80,   153,    48,
   119,   144,    67,    18,    58,    26,    92,    93,    94,    41,
   146,   214,    20,    49,   216,   217,   -43,   -43,    59,    50,
    51,    23,    52,    53,    60,    61,    66,    62,    63,   218,
   230,   115,   220,   221,   116,    90,    30,   115,   152,    31,
    74,    97,    98,    99,   100,   101,   102,   103,   117,   118,
   209,   124,    92,    93,   117,   118,   133,   119,   210,   104,
   105,   106,   231,   119,   107,    67,   -29,   227,    67,   108,
   109,   110,   111,   215,   112,   113,   219,   142,    32,    76,
    77,    41,   114,    97,    98,    99,   100,   101,   102,   103,
   222,    95,    96,   145,   154,    88,   148,   211,   223,    78,
    23,   104,   105,   106,   225,   229,   107,   151,    39,   186,
   187,   108,   109,   110,   111,   190,   112,   113,    90,   191,
    32,   197,    77,   198,   114,    97,    98,    99,   100,   101,
   102,   103,   194,   204,   224,    92,    93,    94,   129,   138,
   233,    78,    23,   104,   105,   106,   226,   234,   107,    19,
    39,    73,   207,   108,   109,   110,   111,   213,   112,   113,
   185,     0,     0,     0,     0,     0,   114,    97,    98,    99,
   100,   101,   102,   103,    97,    98,    99,   100,   101,   102,
   103,     0,     0,     0,     0,   104,-32768,   106,     0,     0,
-32768,     0,   104,     0,   106,   108,   109,   110,   111,     0,
   112,   113,   108,   109,   110,   111,     0,   112,   114,     0,
     0,     0,     0,     0,     0,   114,    97,    98,    99,   100,
   101,   102,   103,    97,    98,    99,   100,-32768,-32768,   103,
     0,     0,     0,     0,   104,     0,   106,     0,     0,     0,
     0,-32768,     0,-32768,   108,   109,   110,   111,     0,     0,
     0,-32768,-32768,-32768,-32768,     0,    32,   114,    33,    32,
    34,    33,    35,    36,   114,    35,    36,     0,     0,     0,
     0,    32,    37,    77,    32,     0,    77,    38,    23,   161,
    78,    23,   163,     0,     0,     0,    39,     0,     0,    39,
     0,     0,    78,    23,    40,    78,    23,    40,   125,     0,
     0,    39,   127,     0,    39,   131,     0,     0,   134,     0,
     0,     0,   136
};

static const short yycheck[] = {    14,
    34,    47,    11,     4,    66,    20,    52,     1,    32,    33,
     7,   115,    31,     6,     0,     1,    31,    10,     7,    34,
    21,    33,    57,    35,    36,    37,     7,    62,    40,    16,
    17,    10,     3,    20,     3,    29,     7,    11,    47,    48,
    35,    36,    11,    29,     4,     3,    40,     7,    27,    28,
    47,    48,    67,    77,    40,    13,    90,    51,    92,    93,
    94,    21,    22,    23,     0,    51,    47,    48,    55,     5,
    41,    95,    96,    47,    48,    90,    88,    92,    93,    94,
   142,   143,   186,   129,   130,    97,    98,    99,   100,   101,
   102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
   112,   113,   114,   138,   139,   124,     3,   211,     8,   124,
    10,    29,     9,     8,   133,    10,    29,    30,   133,     4,
   154,    29,    30,     3,   148,    22,   150,   151,    13,    29,
    38,    11,     3,    29,    29,    10,    21,    22,    23,   154,
    11,   187,     6,    43,   190,   191,    27,    28,    43,    49,
    50,    30,    52,    53,    49,    50,     5,    52,    53,   194,
   222,    12,   197,   198,    15,     4,     7,    12,    11,    10,
    29,    14,    15,    16,    17,    18,    19,    20,    29,    30,
     3,    10,    21,    22,    29,    30,    10,    38,    11,    32,
    33,    34,   226,    38,    37,     3,     6,   209,     3,    42,
    43,    44,    45,    11,    47,    48,    11,    10,     8,     9,
    10,   226,    55,    14,    15,    16,    17,    18,    19,    20,
     3,    27,    28,     7,    25,    10,     3,     3,    11,    29,
    30,    32,    33,    34,    11,    11,    37,     3,    38,    10,
     9,    42,    43,    44,    45,    42,    47,    48,     4,    42,
     8,    42,    10,    42,    55,    14,    15,    16,    17,    18,
    19,    20,     9,     7,     9,    21,    22,    23,    47,    47,
     0,    29,    30,    32,    33,    34,    26,     0,    37,     5,
    38,    27,   151,    42,    43,    44,    45,   186,    47,    48,
   116,    -1,    -1,    -1,    -1,    -1,    55,    14,    15,    16,
    17,    18,    19,    20,    14,    15,    16,    17,    18,    19,
    20,    -1,    -1,    -1,    -1,    32,    33,    34,    -1,    -1,
    37,    -1,    32,    -1,    34,    42,    43,    44,    45,    -1,
    47,    48,    42,    43,    44,    45,    -1,    47,    55,    -1,
    -1,    -1,    -1,    -1,    -1,    55,    14,    15,    16,    17,
    18,    19,    20,    14,    15,    16,    17,    18,    19,    20,
    -1,    -1,    -1,    -1,    32,    -1,    34,    -1,    -1,    -1,
    -1,    32,    -1,    34,    42,    43,    44,    45,    -1,    -1,
    -1,    42,    43,    44,    45,    -1,     8,    55,    10,     8,
    12,    10,    14,    15,    55,    14,    15,    -1,    -1,    -1,
    -1,     8,    24,    10,     8,    -1,    10,    29,    30,    16,
    29,    30,    16,    -1,    -1,    -1,    38,    -1,    -1,    38,
    -1,    -1,    29,    30,    46,    29,    30,    46,    49,    -1,
    -1,    38,    53,    -1,    38,    56,    -1,    -1,    59,    -1,
    -1,    -1,    63
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/local/lib/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		yylex(&yylval, &yylloc)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int yyparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 184 "/usr/local/lib/bison.simple"
int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 8:
#line 176 "xl.y"
{;
    break;}
case 9:
#line 181 "xl.y"
{
		    /* output import directive */
		    fprintf(yyout, ":- import %s from %s.\n",
			   yyvsp[-3].attr.val.str, yyvsp[-1].attr.val.str);
		;
    break;}
case 10:
#line 187 "xl.y"
{
		    /* no import print out */
		;
    break;}
case 11:
#line 192 "xl.y"
{ yyval.attr = yyvsp[0].attr; ;
    break;}
case 12:
#line 194 "xl.y"
{ comp2(&(yyval.attr), &(yyvsp[-2].attr),&(yyvsp[0].attr), "%s, %s", &(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
case 13:
#line 197 "xl.y"
{ fprintf(yyout, "predicate(%s).\n", yyvsp[0].attr.val.lexeme);
			  comp1(&(yyval.attr), &(yyvsp[0].attr),&(yyvsp[0].attr), "%s/0", &(yyvsp[0].attr)); ;
    break;}
case 14:
#line 200 "xl.y"
{ /* output datatype directive */
			    fprintf(yyout, "predicate(%s(%s)).\n",
				   yyvsp[-3].attr.val.lexeme, yyvsp[-1].attr.val.str);
			    sprintf(buffer, "%s/%d",
				    yyvsp[-3].attr.val.lexeme, arity);
			    set(&(yyval.attr), buffer, &(yyvsp[-3].attr), &(yyvsp[0].attr)); ;
    break;}
case 15:
#line 208 "xl.y"
{ arity = 1; yyval.attr = yyvsp[0].attr; ;
    break;}
case 16:
#line 210 "xl.y"
{ arity++;
			  comp2(&(yyval.attr), &(yyvsp[-2].attr),&(yyvsp[0].attr), "%s,%s", &(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
case 17:
#line 216 "xl.y"
{
		    fprintf(yyout, "cdef(%s,[%d,%d],[]).\n\n",
			   yyvsp[-1].attr.val.lexeme, yyvsp[-1].attr.l1_no, yyvsp[-1].attr.c1_no);
		;
    break;}
case 18:
#line 221 "xl.y"
{
		    fprintf(yyout, "cdef(%s(%s),[%d,%d],",
			   yyvsp[-4].attr.val.lexeme, yyvsp[-2].attr.val.str, yyvsp[-4].attr.l1_no, yyvsp[-4].attr.c1_no);
		    dump_variables();
		;
    break;}
case 19:
#line 228 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.str, &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 20:
#line 230 "xl.y"
{ comp2(&(yyval.attr), &(yyvsp[-2].attr),&(yyvsp[0].attr), "%s,%s", &(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
case 21:
#line 233 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.lexeme, &(yyvsp[0].attr), &(yyvsp[0].attr));
			  add_variable(yyvsp[0].attr.val.lexeme, TYPE_UNDEFINED); ;
    break;}
case 22:
#line 235 "xl.y"
{ comp2(&(yyval.attr), &(yyvsp[-2].attr),&(yyvsp[0].attr), "%s:%s", &(yyvsp[-2].attr),&(yyvsp[0].attr));
			  add_variable(yyvsp[-2].attr.val.lexeme, yyvsp[0].attr.val.str); ;
    break;}
case 23:
#line 239 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.str, &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 24:
#line 240 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.lexeme, &(yyvsp[0].attr), &(yyvsp[0].attr));
			  add_variable(yyvsp[0].attr.val.lexeme, TYPE_TYPE);
			;
    break;}
case 25:
#line 244 "xl.y"
{ comp2(&(yyval.attr), &(yyvsp[-3].attr),&(yyvsp[0].attr), "%s(%s)", &(yyvsp[-3].attr),&(yyvsp[-1].attr)); ;
    break;}
case 26:
#line 247 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.str, &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 27:
#line 249 "xl.y"
{ comp2(&(yyval.attr), &(yyvsp[-2].attr),&(yyvsp[0].attr), "%s,%s", &(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
case 28:
#line 255 "xl.y"
{
		  fprintf(yyout, "pdef([%s,[%d,%d,%d,%d]],\n\t[%s,[%d,%d,%d,%d]],\n\t",
			 yyvsp[-3].attr.val.str, yyvsp[-3].attr.l1_no, yyvsp[-3].attr.c1_no, yyvsp[-3].attr.l2_no, yyvsp[-3].attr.c2_no,
			 yyvsp[-1].attr.val.str, yyvsp[-1].attr.l1_no, yyvsp[-1].attr.c1_no, yyvsp[-1].attr.l2_no, yyvsp[-1].attr.c2_no);
		  dump_variables();
                ;
    break;}
case 29:
#line 264 "xl.y"
{ comp2(&(yyval.attr), &(yyvsp[-3].attr),&(yyvsp[0].attr), "%s(%s)", &(yyvsp[-3].attr),&(yyvsp[-1].attr));;
    break;}
case 30:
#line 265 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.lexeme, &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 31:
#line 268 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.str, &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 32:
#line 270 "xl.y"
{comp2(&(yyval.attr), &(yyvsp[-2].attr),&(yyvsp[0].attr), "in(%s,%s)", &(yyvsp[-2].attr),&(yyvsp[0].attr));;
    break;}
case 33:
#line 271 "xl.y"
{comp1(&(yyval.attr), &(yyvsp[-2].attr),&(yyvsp[0].attr), "in(%s,*)", &(yyvsp[-2].attr)); ;
    break;}
case 34:
#line 272 "xl.y"
{comp2(&(yyval.attr), &(yyvsp[-2].attr),&(yyvsp[0].attr), "out(%s,%s)",&(yyvsp[-2].attr),&(yyvsp[0].attr));;
    break;}
case 35:
#line 273 "xl.y"
{ comp1(&(yyval.attr), &(yyvsp[-2].attr),&(yyvsp[0].attr), "out(%s,*)", &(yyvsp[-2].attr)); ;
    break;}
case 36:
#line 277 "xl.y"
{ compproc3(&(yyval.attr),&(yyvsp[-5].attr),&(yyvsp[0].attr),"if",&(yyvsp[-4].attr),&(yyvsp[-2].attr),&(yyvsp[0].attr));;
    break;}
case 37:
#line 280 "xl.y"
{ compproc(&(yyval.attr), &(yyvsp[-3].attr),&(yyvsp[0].attr),"if",&(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
case 38:
#line 283 "xl.y"
{ compproc(&(yyval.attr), &(yyvsp[-2].attr),&(yyvsp[0].attr), "pref", &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 39:
#line 284 "xl.y"
{ compproc(&(yyval.attr), &(yyvsp[-2].attr),&(yyvsp[0].attr), "pref", &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 40:
#line 285 "xl.y"
{ compproc(&(yyval.attr), &(yyvsp[-2].attr),&(yyvsp[0].attr), "choice",&(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
case 41:
#line 286 "xl.y"
{ compproc(&(yyval.attr), &(yyvsp[-2].attr),&(yyvsp[0].attr), "par", &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 42:
#line 287 "xl.y"
{ set(&(yyval.attr), yyvsp[-1].attr.val.str, &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 43:
#line 290 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.str, &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 44:
#line 291 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.str, &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 45:
#line 294 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-3].attr),&(yyvsp[0].attr),"%s(%s)",&(yyvsp[-3].attr),&(yyvsp[-1].attr)); ;
    break;}
case 46:
#line 296 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.str, &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 47:
#line 297 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.str, &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 48:
#line 298 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.lexeme, &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 49:
#line 299 "xl.y"
{ set(&(yyval.attr), "[]", &(yyvsp[-1].attr), &(yyvsp[0].attr)); ;
    break;}
case 50:
#line 300 "xl.y"
{ comp1(&(yyval.attr), &(yyvsp[-2].attr),&(yyvsp[0].attr), "[%s]", &(yyvsp[-1].attr)); ;
    break;}
case 51:
#line 301 "xl.y"
{
				  comp2(&(yyval.attr), &(yyvsp[-4].attr),&(yyvsp[0].attr), 
					"[%s | %s]", &(yyvsp[-3].attr), &(yyvsp[-1].attr)); ;
    break;}
case 52:
#line 304 "xl.y"
{
				  comp2(&(yyval.attr), &(yyvsp[-4].attr), &(yyvsp[0].attr),
					"(%s,%s)", &(yyvsp[-3].attr), &(yyvsp[-1].attr)); ;
    break;}
case 53:
#line 309 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.str, &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 54:
#line 310 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-2].attr),&(yyvsp[0].attr),"%s,%s",&(yyvsp[-2].attr),&(yyvsp[0].attr));;
    break;}
case 55:
#line 313 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.str, &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 56:
#line 314 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-2].attr),&(yyvsp[0].attr),"%s,%s",&(yyvsp[-2].attr),&(yyvsp[0].attr));;
    break;}
case 57:
#line 317 "xl.y"
{ yyval.attr = yyvsp[0].attr; ;
    break;}
case 58:
#line 318 "xl.y"
{ set(&(yyval.attr), yyvsp[-1].attr.val.str, &(yyvsp[-1].attr), &(yyvsp[-1].attr)); ;
    break;}
case 59:
#line 319 "xl.y"
{ comp1(&(yyval.attr), &(yyvsp[-1].attr),&(yyvsp[0].attr), "(- %s)", &(yyvsp[0].attr)); ;
    break;}
case 60:
#line 320 "xl.y"
{ comp1(&(yyval.attr), &(yyvsp[-1].attr),&(yyvsp[0].attr), "not(%s)", &(yyvsp[0].attr)); ;
    break;}
case 61:
#line 321 "xl.y"
{ infix("+",   &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 62:
#line 322 "xl.y"
{ infix("-",   &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 63:
#line 323 "xl.y"
{ infix("*",   &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 64:
#line 324 "xl.y"
{ infix("mod", &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 65:
#line 325 "xl.y"
{ infix("//", &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 66:
#line 326 "xl.y"
{ infix("/",   &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 67:
#line 327 "xl.y"
{ infix("<",    &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 68:
#line 328 "xl.y"
{ infix(">",    &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 69:
#line 329 "xl.y"
{ infix(">=",&(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 70:
#line 330 "xl.y"
{ infix("=<", &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 71:
#line 331 "xl.y"
{ infix("=",    &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 72:
#line 332 "xl.y"
{ infix("==",   &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 73:
#line 333 "xl.y"
{ infix("\\=",  &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 74:
#line 335 "xl.y"
{ infix("\\==", &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 75:
#line 336 "xl.y"
{ infix(",",    &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 76:
#line 338 "xl.y"
{ infix(";",    &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 77:
#line 339 "xl.y"
{ infix(":=",  &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 78:
#line 340 "xl.y"
{ infix("is",  &(yyval.attr), &(yyvsp[-2].attr), &(yyvsp[0].attr)); ;
    break;}
case 79:
#line 341 "xl.y"
{ set(&(yyval.attr),yyvsp[-1].attr.val.str, &(yyvsp[-2].attr), &(yyvsp[-2].attr)); ;
    break;}
case 80:
#line 346 "xl.y"
{
		    yyval.attr.l1_no=yyvsp[-3].attr.l1_no;
		    yyval.attr.c1_no=yyvsp[-3].attr.c1_no;
		    yyval.attr.l2_no=yyvsp[0].attr.l1_no;
		    yyval.attr.c2_no=yyvsp[0].attr.c1_no;
		    fprintf(yyout, "fdef(%s, %s).\n",
			   yyvsp[-3].attr.val.str,yyvsp[-1].attr.val.str);
		    fprintf(yyout, "fneg(%s, neg_form(%s)).\n",
			   yyvsp[-3].attr.val.str,yyvsp[-3].attr.val.str);
		;
    break;}
case 81:
#line 358 "xl.y"
{
		    yyval.attr.l1_no=yyvsp[-3].attr.l1_no;
		    yyval.attr.c1_no=yyvsp[-3].attr.c1_no;
		    yyval.attr.l2_no=yyvsp[0].attr.l1_no;
		    yyval.attr.c2_no=yyvsp[0].attr.c1_no;
		    /* neg_xxx = neg_form(neg_xxx) */
		    fprintf(yyout, "fdef(%s, neg_form(neg_%s)).\n",
			    yyvsp[-3].attr.val.str,yyvsp[-3].attr.val.str);
		    /* neg_xxx = form(neg. of expr) */
		    fprintf(yyout, "fdef(neg_%s, %s).\n",
			    yyvsp[-3].attr.val.str, yyvsp[-1].attr.val.str);
		    fprintf(yyout, "fneg(%s, form(neg_%s)).\n",
			   yyvsp[-3].attr.val.str,yyvsp[-3].attr.val.str);
		;
    break;}
case 82:
#line 375 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-3].attr),&(yyvsp[0].attr),"%s(%s)",&(yyvsp[-3].attr),&(yyvsp[-1].attr)); ;
    break;}
case 83:
#line 376 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.str, &(yyvsp[0].attr), &(yyvsp[0].attr));	;
    break;}
case 84:
#line 379 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-2].attr),&(yyvsp[0].attr), "and(%s,%s)",&(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
case 85:
#line 380 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-2].attr),&(yyvsp[0].attr), "or(%s,%s)", &(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
case 86:
#line 382 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-3].attr),&(yyvsp[0].attr),"diam%s,%s)",&(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
case 87:
#line 383 "xl.y"
{ comp1(&(yyval.attr),&(yyvsp[-1].attr),&(yyvsp[0].attr),"diamAll(%s)",&(yyvsp[0].attr)); ;
    break;}
case 88:
#line 385 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-3].attr),&(yyvsp[-2].attr),"diamMinus%s,%s)",&(yyvsp[-2].attr),&(yyvsp[0].attr));;
    break;}
case 89:
#line 387 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-3].attr),&(yyvsp[0].attr),"box%s,%s)",&(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
case 90:
#line 389 "xl.y"
{ set(&(yyval.attr), "tt", &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 91:
#line 390 "xl.y"
{ set(&(yyval.attr), "ff", &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 92:
#line 391 "xl.y"
{ comp1(&(yyval.attr),&(yyvsp[0].attr),&(yyvsp[0].attr),"form(%s)",&(yyvsp[0].attr)); ;
    break;}
case 93:
#line 392 "xl.y"
{ set(&(yyval.attr), yyvsp[-1].attr.val.str, &(yyvsp[-2].attr), &(yyvsp[0].attr));	;
    break;}
case 94:
#line 396 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-3].attr),&(yyvsp[0].attr),"%s(%s)",&(yyvsp[-3].attr),&(yyvsp[-1].attr)); ;
    break;}
case 95:
#line 397 "xl.y"
{ yyval.attr = yyvsp[0].attr; ;
    break;}
case 96:
#line 401 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-2].attr),&(yyvsp[0].attr), "or(%s,%s)",&(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
case 97:
#line 403 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-2].attr),&(yyvsp[0].attr), "and(%s,%s)",&(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
case 98:
#line 405 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-3].attr),&(yyvsp[0].attr),"box%s,%s)",&(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
case 99:
#line 407 "xl.y"
{ comp1(&(yyval.attr),&(yyvsp[-1].attr),&(yyvsp[0].attr),"boxAll(%s)",&(yyvsp[0].attr)); ;
    break;}
case 100:
#line 409 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-3].attr),&(yyvsp[-2].attr),"boxMinus%s,%s)",&(yyvsp[-2].attr),&(yyvsp[0].attr));;
    break;}
case 101:
#line 411 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-3].attr),&(yyvsp[0].attr),"diam%s,%s)",&(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
case 102:
#line 412 "xl.y"
{ set(&(yyval.attr), "ff", &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 103:
#line 413 "xl.y"
{ set(&(yyval.attr), "tt", &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 104:
#line 414 "xl.y"
{ comp1(&(yyval.attr), &(yyvsp[0].attr),&(yyvsp[0].attr), "neg(%s)", &(yyvsp[0].attr)); ;
    break;}
case 105:
#line 415 "xl.y"
{ set(&(yyval.attr), yyvsp[-1].attr.val.str, &(yyvsp[-2].attr), &(yyvsp[0].attr));	;
    break;}
case 106:
#line 418 "xl.y"
{ comp1(&(yyval.attr),&(yyvsp[-1].attr),&(yyvsp[0].attr),"Minus%s",&(yyvsp[0].attr)); ;
    break;}
case 107:
#line 419 "xl.y"
{ set(&(yyval.attr), "MinusSet([]", &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 108:
#line 420 "xl.y"
{ set(&(yyval.attr), yyvsp[0].attr.val.str, &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 109:
#line 423 "xl.y"
{ comp1(&(yyval.attr),&(yyvsp[0].attr),&(yyvsp[0].attr),"(%s",&(yyvsp[0].attr)); ;
    break;}
case 110:
#line 425 "xl.y"
{ comp1(&(yyval.attr),&(yyvsp[-2].attr),&(yyvsp[0].attr),"Set([%s]",&(yyvsp[-1].attr)); ;
    break;}
case 111:
#line 428 "xl.y"
{
			    if (strcmp(yyvsp[-3].attr.val.str, "in") != 0 &&
				strcmp(yyvsp[-3].attr.val.str, "out") != 0) {
				comp2(&(yyval.attr),&(yyvsp[-3].attr),&(yyvsp[0].attr),"action(%s(%s))",
				      &(yyvsp[-3].attr),&(yyvsp[-1].attr));
			    } else
				comp2(&(yyval.attr),&(yyvsp[-3].attr),&(yyvsp[0].attr),"%s(%s)",&(yyvsp[-3].attr),&(yyvsp[-1].attr));
			;
    break;}
case 112:
#line 436 "xl.y"
{
			    if (strcmp(yyvsp[0].attr.val.str, "nop") != 0 &&
				strcmp(yyvsp[0].attr.val.str, "tau") != 0) {
				comp1(&(yyval.attr), &(yyvsp[0].attr),&(yyvsp[0].attr),"action(%s)",&(yyvsp[0].attr));
			    } else
				yyval.attr = yyvsp[0].attr;
			;
    break;}
case 113:
#line 443 "xl.y"
{ yyval.attr = yyvsp[0].attr; ;
    break;}
case 114:
#line 444 "xl.y"
{ set(&(yyval.attr),yyvsp[0].attr.val.lexeme, &(yyvsp[0].attr), &(yyvsp[0].attr)); ;
    break;}
case 115:
#line 447 "xl.y"
{ yyval.attr = yyvsp[0].attr; ;
    break;}
case 116:
#line 449 "xl.y"
{ comp2(&(yyval.attr),&(yyvsp[-2].attr),&(yyvsp[0].attr),"%s,%s",&(yyvsp[-2].attr),&(yyvsp[0].attr)); ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 465 "/usr/local/lib/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 452 "xl.y"


void set(AttrType *nt, char* bd, AttrType* a1, AttrType* a2)
{
    if (bd == buffer)
	/* copy from temporary string */
	nt->val.str = strdup(bd);
    else
	nt->val.str = bd;
    nt->l1_no = a1->l1_no;
    nt->c1_no = a1->c1_no;
    nt->l2_no = a2->l2_no;
    nt->c2_no = a2->c2_no;
}

void infix(char* op, AttrType* a0, AttrType* a1, AttrType* a2)
{
    sprintf(buffer, "(%s %s %s)",
	    a1->val.str, op, a2->val.str);
    set(a0, buffer, a1, a2);
}


void comp1(AttrType* a0, AttrType* first, AttrType* last,
		char* format, AttrType* op1)
{
    sprintf(buffer, format, op1->val.str);
    set(a0, buffer, first, last);
}

void comp2(AttrType* a0, AttrType* first, AttrType* last,
		char* format, AttrType* op1, AttrType* op2)
{
    sprintf(buffer, format, op1->val.str, op2->val.str);
    set(a0, buffer, first, last);
}

void compproc(AttrType* a0, AttrType* first, AttrType* last,
		char* op, AttrType* op1, AttrType* op2)
{
    sprintf(buffer, "%s([%s,[%d,%d,%d,%d]],[%s,[%d,%d,%d,%d]])",
	    op,
	    op1->val.str, op1->l1_no, op1->c1_no, op1->l2_no, op1->c2_no,
	    op2->val.str, op2->l1_no, op2->c1_no, op2->l2_no, op2->c2_no);
    set(a0, buffer, first, last);
}

void compproc3(AttrType* a0, AttrType* first, AttrType* last,
		char* op, AttrType* op1, AttrType* op2, AttrType* op3)
{
    sprintf(buffer, "%s([%s,[%d,%d,%d,%d]],[%s,[%d,%d,%d,%d]],[%s,[%d,%d,%d,%d]])",
	    op,
	    op1->val.str, op1->l1_no, op1->c1_no, op1->l2_no, op1->c2_no,
	    op2->val.str, op2->l1_no, op2->c1_no, op2->l2_no, op2->c2_no,
	    op3->val.str, op3->l1_no, op3->c1_no, op3->l2_no, op3->c2_no);
    set(a0, buffer, first, last);
}

/* -------------------- variable management -------------------- */

typedef struct node
{
    char* varname;
    char* type;
    struct node* next;
} node;

static node* head = NULL;

void add_variable(char* newname, char* newtype)
{
    node* t;

    /* skip unnamed variable */
    if (newname[0] == '_' && newname[1] == '\0')
	return;

    /* check whether the variable is recorded */
    for (t = head; t != NULL; t = t->next)
	if (strcmp(newname, t->varname) == 0) {
	    if (t->type == TYPE_UNDEFINED) {
		if (newtype == TYPE_TYPE) 
		    warning("Data variable used as type", newname);
		else 
		    t->type = newtype;
	    } else if (t->type == TYPE_TYPE) {
		if (newtype != TYPE_TYPE)
		    warning("Type variable %s used as data", newname);
	    } else {
		if (newtype == TYPE_TYPE)
		    warning("Data variable %s used as type", newname);
		else if (newtype != TYPE_UNDEFINED 
			 && strcmp(t->type, newtype) != 0)
		    warning("Data variable %s's type defined twice", newname);
	    }
	    return;
	}

    /* add the variable at head */
    t = (node*)malloc(sizeof(node));
    t->varname = strdup(newname);
    t->type = newtype;
    t->next = head;
    head = t;
}

void dump_variables()
{
    node* t;

    fprintf(yyout, "[");
    /* print the first data variable */
    for (t = head; t != NULL; ) {
	head = t->next;
	if (t->type != TYPE_TYPE) {
	    fprintf(yyout, "(%s,'%s')", t->varname, t->varname);
	    free(t);
	    break;
	}
	free(t);
    }

    /* print the remaining data variables */
    for (t = head; t != NULL; ) {
	if (t->type != TYPE_TYPE)
	    fprintf(yyout, ", (%s,'%s')", t->varname, t->varname);
	head = t->next;
	free(t);
	t = head;
    }
    fprintf(yyout, "]).\n\n");
}
