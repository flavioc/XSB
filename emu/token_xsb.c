/* File:      token_xsb.c
** Author(s): Richard A. O'Keefe, Deeporn H. Beardsley, Baoqiu Cui,
**    	      C.R. Ramakrishnan 
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

#include "xsb_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "auxlry.h"
#include "token_xsb.h"
#include "cell_xsb.h"
#include "psc_xsb.h"
#include "subp.h"
#include "register.h"
#include "error_xsb.h"

#define exit_if_null(x) {\
  if(x == NULL){\
   xsb_exit("Malloc Failed !\n");\
  }\
}

#define Char unsigned char
#define AlphabetSize 256
 
#define InRange(X,L,U) ((unsigned)((X)-(L)) <= (unsigned)((U)-(L)))
#define IsLayout(X) InRange(InType(X), SPACE, EOLN)
 

/*  VERY IMPORTANT NOTE: I assume that the stdio library returns the value
    EOF when character input hits the end of the file, and that this value
    is actually the integer -1.  You will note the DigVal(), InType(), and
    OuType() macros below, and there is a ChType() macro used in crack().
    They all depend on this assumption.
*/
 
#define InType(c)       (intab.chtype+1)[c]
#define DigVal(c)       (digval+1)[c]

#ifdef BITS64
#define MY_MAXINT ((long)0x07fffffffffffff)
#else
#define MY_MAXINT ((int)0x07ffffff)	/* Modified by Kostis */
#endif

Char outqt[EOFCH+1];   /* All the "+1" appear because of the EOF char */
 
struct CHARS
    {
        int     eolcom;       /* End-of-line comment, default % */
        int     endeol;       /* early terminator of eolcoms, default none */
        int     begcom;       /* In-line comment start, default / */
        int     astcom;       /* In-line comment second, default * */
        int     endcom;       /* In-line comment finish, default / */
        int     radix;        /* Radix character, default ' */
        int     dpoint;       /* Decimal point, default . */
        int     escape;       /* String escape character, default \ */
        int     termin;       /* Terminates a clause */
        char    chtype[AlphabetSize+1];
    };
 
struct CHARS intab =   /* Special character table */
    {
        '%',                  /* eolcom: end of line comments */
        -1,                   /* endeol: early end for eolcoms */
        '/',                  /* begcom: in-line comments */
        '*',                  /* astcom: in-line comments */
        '/',                  /* endcom: in-line comments */
        '\'',                 /* radix : radix separator */
        '.',                  /* dpoint: decimal point */
        -1, /*'\\',*/         /* escape: string escape character */
        '.',                  /* termin: ends clause, sign or solo */
    {
        EOFCH,                /* really the -1th element of the table: */
    /*  ^@      ^A      ^B      ^C      ^D      ^E      ^F      ^G      */
        SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,
    /*  ^H      ^I      ^J      ^K      ^L      ^M      ^N      ^O      */
        SPACE,  SPACE,  EOLN,   SPACE,  EOLN,   SPACE,  SPACE,  SPACE,
    /*  ^P      ^Q      ^R      ^S      ^T      ^U      ^V      ^W      */
        SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,
    /*  ^X      ^Y      ^Z      ^[      ^\      ^]      ^^      ^_      */
        SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,
    /*  sp      !       "       #       $       %       &       '       */
        SPACE,  NOBLE,  LISQT,  SIGN,   SIGN,  PUNCT,  SIGN,   ATMQT,
    /*  (       )       *       +       ,       -       .       /       */
        PUNCT,  PUNCT,  SIGN,   SIGN,   PUNCT,  SIGN,   SIGN,   SIGN,
    /*  0       1       2       3       4       5       6       7       */
        DIGIT,  DIGIT,  DIGIT,  DIGIT,  DIGIT,  DIGIT,  DIGIT,  DIGIT,
    /*  8       9       :       ;       <       =       >       ?       */
        DIGIT,  DIGIT,  SIGN,   PUNCT,  SIGN,   SIGN,   SIGN,   SIGN,
    /*  @       A       B       C       D       E       F       G       */
        SIGN,   UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,
    /*  H       I       J       K       L       M       N       O       */
        UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,
    /*  P       Q       R       S       T       U       V       W       */
        UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,
    /*  X       Y       Z       [       \       ]       ^       _       */
        UPPER,  UPPER,  UPPER,  PUNCT,  SIGN,   PUNCT,  SIGN,   BREAK,
    /*  `       a       b       c       d       e       f       g       */
        SIGN,   LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,
    /*  h       i       j       k       l       m       n       o       */
        LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,
    /*  p       q       r       s       t       u       v       w       */
        LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,
    /*  x       y       z       {       |       }       ~       ^?      */
        LOWER,  LOWER,  LOWER,  PUNCT,  PUNCT,  PUNCT,  SIGN,   SPACE,
    /*  128     129     130     131     132     133     134     135     */
        SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,
    /*  136     137     138     139     140     141     142     143     */
        SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,
    /*  144     145     146     147     148     149     150     151     */
        SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,
    /*  152     153     154     155     156     157     158     159     */
        SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,  SPACE,
    /*  NBSP    !-inv   cents   pounds  ching   yen     brobar  section */
        SPACE,  SIGN,   SIGN,   SIGN,   SIGN,   SIGN,   SIGN,   SIGN,
    /*  "accent copyr   -a ord  <<      nothook SHY     (reg)   ovbar   */
        SIGN,   SIGN,   LOWER,  SIGN,   SIGN,   SIGN,   SIGN,   SIGN,
    /*  degrees +/-     super 2 super 3 -       micron  pilcrow -       */
        SIGN,   SIGN,   LOWER,  LOWER,  SIGN,   SIGN,   SIGN,   SIGN,
    /*  ,       super 1 -o ord  >>      1/4     1/2     3/4     ?-inv   */
        SIGN,   LOWER,  LOWER,  SIGN,   SIGN,   SIGN,   SIGN,   SIGN,
    /*  `A      'A      ^A      ~A      "A      oA      AE      ,C      */
        UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,
    /*  `E      'E      ^E      "E      `I      'I      ^I      "I      */
        UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,
    /*  ETH     ~N      `O      'O      ^O      ~O      "O      x times */
        UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  SIGN,
    /*  /O      `U      'U      ^U      "U      'Y      THORN   ,B      */
        UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  UPPER,  LOWER,
    /*  `a      'a      ^a      ~a      "a      oa      ae      ,c      */
        LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,
    /*  `e      'e      ^e      "e      `i      'i      ^i      "i      */
        LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,
    /*  eth     ~n      `o      'o      ^o      ~o      "o      -:-     */
#ifdef  vms
        LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,
#else
        LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  SIGN,
#endif
    /*  /o      `u      'u      ^u      "u      'y      thorn  "y       */
#ifdef  vms
        LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  SPACE
#else
        LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER,  LOWER
#endif
}};
 
char digval[AlphabetSize+1] =
    {
        99,                     /* really the -1th element of the table */
    /*  ^@      ^A      ^B      ^C      ^D      ^E      ^F      ^G      */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  ^H      ^I      ^J      ^K      ^L      ^M      ^N      ^O      */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  ^P      ^Q      ^R      ^S      ^T      ^U      ^V      ^W      */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  ^X      ^Y      ^Z      ^[      ^\      ^]      ^^      ^_      */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  sp      !       "       #       $       %       &       '       */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  (       )       *       +       ,       -       .       /       */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  0       1       2       3       4       5       6       7       */
        0,      1,      2,      3,      4,      5,      6,      7,
    /*  8       9       :       ;       <       =       >       ?       */
        8,      9,      99,     99,     99,     99,     99,     99,
    /*  @       A       B       C       D       E       F       G       */
        99,     10,     11,     12,     13,     14,     15,     99,
    /*  H       I       J       K       L       M       N       O       */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  P       Q       R       S       T       U       V       W       */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  X       Y       Z       [       \       ]       ^       _       */
        99,     99,     99,     99,     99,     99,     99,     0,  /*NB*/
    /*  `       a       b       c       d       e       f       g       */
        99,     10,     11,     12,     13,     14,     15,     99,
    /*  h       i       j       k       l       m       n       o       */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  p       q       r       s       t       u       v       w       */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  x       y       z       {       |       }       ~       ^?      */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  128     129     130     131     132     133     134     135     */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  136     137     138     139     140     141     142     143     */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  144     145     146     147     148     149     150     151     */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  152     153     154     155     156     157     158     159     */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  160     161     162     163     164     165     166     167     */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  168     169     170(-a) 171     172     173     174     175     */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  176     177     178(2)  179(3)  180     181     182     183     */
        99,     99,     2,      3,      99,     99,     99,     99,
    /*  184     185(1)  186(-o) 187     188     189     190     191     */
        99,     1,      99,     99,     99,     99,     99,     99,
    /*  192     193     194     195     196     197     198     199     */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  200     201     202     203     204     205     206     207     */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  208     209     210     211     212     213     214     215     */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  216     217     218     219     220     221     222     223     */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  224     225     226     227     228     229     230     231     */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  232     233     234     235     236     237     238     239     */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  240     241     242     243     244     245     246     247     */
        99,     99,     99,     99,     99,     99,     99,     99,
    /*  248     249     250     251     252     253     254     255     */
        99,     99,     99,     99,     99,     99,     99,     99
    };

int intype(int c)
{
  return (intab.chtype+1)[c];
}

static void SyntaxError(char *description)
{
	char message[100];

	sprintf(message, "++Error: %s (syntax error)\n", description);
	pcreg = exception_handler(message);
}
 

void unGetC(int d, FILE *card, STRFILE *instr)
{
  if (instr) {
    ++(instr)->strcnt;
    *(--(instr)->strptr) = d;
  }
  else ungetc(d, card);
}

 
 
/*  GetToken() reads a single token from the input stream and returns
    its type, which is one of the following:

        TK_INT		-- an integer 
        TK_INTFUNC	-- an integer functor
        TK_VARFUNC	-- a HiLog variable( pair 
        TK_FUNC		-- an atom( pair
        TK_ATOM		-- an atom
        TK_VAR		-- a variable
        TK_PUNC		-- a single punctuation mark
        TK_HPUNC	-- punctuation ) followed by a ( in HiLog terms
        TK_LIST		-- a quoted list of character codes (in buffer)
        TK_STR		-- a quoted string
        TK_EOC		-- end of clause (normally '.\n').
        TK_EOF		-- signifies end-of-file.
	TK_REAL		-- a real, in double_v.
	TK_REALFUNC	-- a real, in double_v.

    In most of the above cases (except the last two), the text of the 
    token is in AtomStr.
    There are two questions: between which pairs of adjacent tokens is
    a space (a) necessary, (b) desirable?  There is an additional
    dummy token type used by the output routines, namely
        NOBLE		-- extra space is definitely not needed.
    I leave it as an exercise for the reader to answer question (a).
    Since this program is to produce output I find palatable (even if
    it isn't exactly what I'd write myself), extra spaces are ok.  In
    fact, the main use of this program is as an editor command, so it
    is normal to do a bit of manual post-processing.  Question (b) is
    the one to worry about then.  My answer is that a space is never
    written
        - after  PUNCT ( [ { |
        - before PUNCT ) ] } | , <ENDCL>
    is written after comma only sometimes, and is otherwise always
    written.  The variable lastput thus takes these values:

        ALPHA      -- put a space except before PUNCT
        SIGN       -- as alpha, but different so ENDCL knows to put a space.
        NOBLE      -- don't put a space
        ENDCL      -- just ended a clause
        EOFCH      -- at beginning of file
*/

struct token res_str;
struct token *token = &res_str;

#define InitStrLen	1000

int     lastc = ' ';    /* previous character */
char*   strbuff = NULL;             /* Pointer to token buffer; Will be
				       allocated on first call to GetToken */
int     strbuff_len = InitStrLen;   /* length of first allocation will be
				       InitStrLen; will be doubled on
				       subsequent overflows */
double  double_v;
long	rad_int;

char    tok2long[]      = "token too long";
char    eofinrem[]      = "end of file in comment";
char    badexpt[]       = "bad exponent";
char    badradix[]      = "radix not 0 or 2..36";
 
 
/*  read_character(FILE* card, STRFILE* instr, Char q)
    reads one character from a quoted atom, list, string, or character.
    Doubled quotes are read as single characters, otherwise a
    quote is returned as -1 and lastc is set to the next character.
    If the input syntax has character escapes, they are processed.
    Note that many more character escape sequences are accepted than
    are generated.  There is a divergence from C: \xhh sequences are
    two hexadecimal digits long, not three.
    Note that the \c and \<space> sequences combine to make a pretty
    way of continuing strings.  Do it like this:
        "This is a string, which \c
       \ has to be continued over \c
       \ several lines.\n".

    -- If encounters the EOF, then return -2. (Baoqiu, 2/16/1997)
*/
 
static int read_character(register FILE *card,
			  register STRFILE *instr,
			  register int q)
{
        register int c;
 
        c = GetC(card,instr);
BACK:   if (c < 0) {
ERROR:      if (q < 0) {
                SyntaxError("end of file in character constant");
		return -2;		/* encounters EOF */
            } else {
                char message[80];
                sprintf(message, "end of file in %cquoted%c constant", q, q);
                SyntaxError(message);
		return -2;		/* encounters EOF */
            }
        }
        if (c == q) {
            c = GetC(card,instr);
            if (c == q) return c;
            lastc = c;
            return -1;
        } else
        if (c != intab.escape) {
            return c;
        }
        /*  If we get here, we have read the "\" of an escape sequence  */
        c = GetC(card,instr);
        switch (c) {
            case EOF:
		clearerr(card);
                goto ERROR;
            case 'n': case 'N':         /* newline */
                return 10;
            case 't': case 'T':         /* tab */
                return  9;
            case 'r': case 'R':         /* reeturn */
                return 13;
            case 'v': case 'V':         /* vertical tab */
                return 11;
            case 'b': case 'B':         /* backspace */
                return  8;
            case 'f': case 'F':         /* formfeed */
                return 12;
            case 'e': case 'E':         /* escape */
                return 27;
            case 'd': case 'D':         /* delete */
                return 127;
            case 's': case 'S':         /* space */
                return 32;
            case 'a': case 'A':         /* alarm */
                return  7;
            case '^':                   /* control */
                c = GetC(card,instr);
                if (c < 0) goto ERROR;
                return c == '?' ? 127 : c&31;
            case 'c': case 'C':         /* continuation */
                while (IsLayout(c = GetC(card,instr))) ;
                goto BACK;
            case 'x': case 'X':         /* hexadecimal */
                {   int i, n;
                    for (n = 0, i = 2; --i >= 0; n = (n<<4) + DigVal(c))
                        if (DigVal(c = GetC(card,instr)) >= 16) {
                            if (c < 0) goto ERROR;
                            (void)unGetC(c, card, instr);
                            break;
                        }
                    return n & 255;
                }
            case 'o': case 'O':         /* octal */
                c = GetC(card,instr);
                if (DigVal(c) >= 8) {
                    if (c < 0) goto ERROR;
                    (void) unGetC(c, card, instr);
                    return 0;
                }
            case '0': case '1': case '2': case '3':
            case '4': case '5': case '6': case '7':
                {   int i, n;
                    for (n = c-'0', i = 2; --i >= 0; n = (n<<3) + DigVal(c))
                        if (DigVal(c = GetC(card,instr)) >= 8) {
                            if (c < 0) goto ERROR;
                            (void) unGetC(c, card, instr);
                            break;
                        }
                    return n & 255;
                }
            default:
                if (!IsLayout(c)) return c;
                c = GetC(card,instr);
                goto BACK;
        }
    }
 
 
 
 
/*  com0plain(card, instr, endeol)
    These comments have the form
        <eolcom> <char>* <newline>                      {PUNCT}
    or  <eolcom><eolcom> <char>* <newline>              {SIGN }
    depending on the classification of <eolcom>.  Note that we could
    handle ADA comments with no trouble at all.  There was a Pop-2
    dialect which had end-of-line comments using "!" where the comment
    could also be terminated by "!".  You could obtain the effect of
    including a "!" in the comment by doubling it, but what you had
    then was of course two comments.  The endeol parameter of this
    function allows the handling of comments like that which can be
    terminated either by a new-line character or an <endeol>, whichever
    comes first.  For ordinary purposes, endeol = -1 will do fine.
    When this is called, the initial <eolcom>s have been consumed.
    We return the first character after the comment.
    If the end of the source file is encountered, we do not treat it
    as an error, but quietly close the comment and return EOF as the
    "following" character.
 
*/
static int com0plain(register FILE *card,	/* source file */
       		     register STRFILE *instr,	/* source string, if non-NULL */
		     register int endeol)	/* The closing character "!" */
{
    register int c;
 
    while ((c = GetC(card,instr)) >= 0 && c != '\n' && c != endeol) ;
    if (c >= 0) c = GetC(card,instr);
    return c;
}
 
 
/*  The states in the next two functions are
        0       - after an uninteresting character
        1       - after an "astcom"
        2       - after a  "begcom"
    Assuming begcom = "(", astom = "#", endcom = ")",
    com2plain will accept "(#)" as a complete comment.  This can
    be changed by initialising the state to 0 rather than 1.
    The same is true of com2nest, which accepts "(#(#)#) as a
    complete comment.  Changing it would be rather harder.
    Fixing the bug where the closing <astcom> is copied if it is
    not an asterisk may entail rejecting "(#)".
*/
 
/*  com2plain(card, instr, stcom, endcom)
    handles PL/I-style comments, that is, comments which begin with
    a pair of characters <begcom><astcom> and end with a pair of
    chracters <astcom><endcom>, where nesting is not allowed.  For
    example, if we take begcom='(', astcom='*', endcom=')' as in
    Pascal, the comment "(* not a (* plain *)^ comment *) ends at
    the "^".
    For this kind of comment, it is perfectly sensible for any of
    the characters to be equal.  For example, if all three of the
    bracket characters are "#", then "## stuff ##" is a comment.
    When this is called, the initial <begcom><astcom> has been consumed.
*/
static int com2plain(register FILE *card,	/* source file */
		     register STRFILE *instr,	/* source string, if non-NULL */
		     int astcom,		/* The asterisk character "*" */
		     int endcom)		/* The closing character "/" */
{
        register int c;
        register int state;
 
        for (state = 0; (c = GetC(card,instr)) >= 0; ) {
            if (c == endcom && state) break;
            state = c == astcom;
        }
        if (c < 0) return 1; 
	else return 0; 
}
 
int token_too_long_warning = 1;

void realloc_strbuff(char **pstrbuff, char **ps, int *pn)
     /* Expand token buffer when needed.
      * pstrbuff: base address of current buffer
      * ps: tail of current buffer
      * pn: number of elements remaining in the current buffer
      * --  C.R., 7/27/97
     */
{ 
  char *newbuff;

  newbuff = (char *)realloc(*pstrbuff, strbuff_len * 2);
  exit_if_null(newbuff);
  if (token_too_long_warning) {
    xsb_warn("Extra-long token. Runaway string?");
    token_too_long_warning = 0;
  }

  if (*pstrbuff != newbuff) {
    /* Aha, base address has changed, so change s too*/
    *ps += newbuff - *pstrbuff;
  }
  *pstrbuff = newbuff;
  *pn += strbuff_len;
  strbuff_len *= 2;
  return;
}

struct token *GetToken(FILE *card, STRFILE *instr, int prevch)
{
        char *s;
        register int c, d = 0;
        long oldv = 0, newv = 0; 
        int n;

	if (strbuff == NULL)
	  {
	    /* First call for GetToken, so allocate a buffer */
	    strbuff = (char *)malloc(strbuff_len);
	    exit_if_null(strbuff);
	  }
	s = strbuff;
	n = strbuff_len;

        c = prevch; 
START:
        switch (InType(c)) {
 
            case DIGIT:
                /*  The following kinds of numbers exist:
                      (1) unsigned decimal integers: d+
                      (2) unsigned based integers: d+Ro+[R]
                      (3) unsigned floats: d* [. d*] [e +/-] d+
                      (4) characters: 0Rc[R]
                    We allow underscores in numbers too, ignoring them.
                */
                do {
                    if (c != '_') *s++ = c;
                    c = GetC(card,instr);
                } while (InType(c) <= BREAK);
                if (c == intab.radix) { 
                    *s = 0;
                    for (d = 0, s = strbuff; (c = *s++);) {
		      d = d*10-'0'+c;}
                        if (d == 1 || d > 36) {
				SyntaxError(badradix);
				token->type = TK_ERROR;
				return token;
                        }
                    if (d == 0) {
		      /*  0'c['] is a character code  */
		      d = read_character(card, instr, -1);
		      /* TLS: changed to handle some of the important cases
			 of character code constants, sec 6.4.4 of ISO.
			 Here is what it does and doesnt do: 
			 A character code constant is 0' followed by 
			 a number of possible items.  
			 1) non quote char
			 2) single quote char ',' single quote char
			 3) double quote char
			 4) back quote char

			 We handle 3,4, do not handle 2, and handle (I
			 think) all the cases for 1 *except*

			 meta-escape sequence
			 octal escape sequence
			 hexadecimal escape sequence

			 and partially handle control escape
			 sequences, which is what got me into this in
			 the first place.  For these last, my change
			 is somewhat kludgy, and I am open to
			 suggestions about what library function to
			 use to convert, e.g.  \n to 10.  
			 
			 If someone can tell me a better way to do
			 this, then we still need to add \a, \b, \f,
			 \r, \v for ISO.  */

		      if (d == '\\') {
			d = GetC(card,instr);
			if (d == 'n') {
			  rad_int = 10; 
			  token->value = (char *)(&rad_int);
			  token->nextch = GetC(card,instr);
			  token->type = TK_INT;
			  return token;
			} 
			if (d == 't') {
			  rad_int = 9;  
			  token->value = (char *)(&rad_int);
			  token->nextch = GetC(card,instr);
			  token->type = TK_INT;
			  return token;
			} 
			if (d == ' ') {
			  rad_int = 47;  /* handle 0'\ */
			  token->value = (char *)(&rad_int);
			  token->nextch = GetC(card,instr);
			  token->type = TK_INT;
			  return token;
			}
		      }
		      else {
                        sprintf(strbuff, "%d", d);
                        d = GetC(card,instr);
			rad_int = atoi(strbuff);
                        token->nextch = d == intab.radix ? GetC(card,instr):d;
			token->value = (char *)(&rad_int);
			token->type = TK_INT;
                        return token;
		      }
                    }
                    while (c = GetC(card,instr), DigVal(c) < 99)
                        if (c != '_') {
			    oldv = newv;
			    newv = newv*d + DigVal(c);
			    if (newv < oldv || newv > MY_MAXINT) {
				xsb_error("Overflow in radix notation");
			        double_v = oldv*1.0*d + DigVal(c);
				while (c = GetC(card,instr), DigVal(c) < 99)
                        	    if (c != '_') 
					double_v = double_v*d + DigVal(c);
                    		if (c == intab.radix) 
					c = GetC(card,instr);
                    		token->nextch = c;
				token->value = (char *)(&double_v);
				if (c == '(')	/* Modified for HiLog */	
					token->type = TK_REALFUNC;
				else
					token->type = TK_REAL;
				return token;
			    }
			}
		    rad_int = newv;
                    if (c == intab.radix) 
			c = GetC(card,instr);
                    token->nextch = c;
		    token->value = (char *)(&rad_int);
                    if (c == '(')	/* Modified for HiLog */
			token->type = TK_INTFUNC;
                    else
			token->type = TK_INT;
                    return token;
                }
		else if (c == intab.dpoint) {
                    d = GetC(card,instr);
                    if (InType(d) == DIGIT) {
DECIMAL:                *s++ = '.';
                        do {
                            if (d != '_') *s++ = d;
                            d = GetC(card,instr);
                        } while (InType(d) <= BREAK);
                        if ((d | 32) == 'e') {
                            *s++ = 'E';
                            d = GetC(card,instr);
                            if (d == '-') *s++ = d, d = GetC(card,instr);
                            else if (d == '+') d = GetC(card,instr);
                            if (InType(d) > BREAK) {
				SyntaxError(badexpt);
				token->type = TK_ERROR;
				return token;
			    }
                            do {
                                if (d != '_') *s++ = d;
                                d = GetC(card,instr);
                            } while (InType(d) <= BREAK);
                        }
                        c = d;
                        *s = 0;
			sscanf(strbuff, "%lf", &double_v);
			token->nextch = c;
			token->value = (char *)(&double_v);
			if (c == '(')	/* Modified for HiLog */	
				token->type = TK_REALFUNC;
			else
				token->type = TK_REAL;
                        return token;
                    } else {
                        unGetC(d, card, instr);
                        /* c has not changed */
                    }
		}
		else {
		  if (c == 'b' || c == 'x' || c == 'o' ) {
		    SyntaxError("ISO binary/hex/octal integer constants not yet implemented"); 
		    do {
		      if (d != '_') *s++ = d;
		      d = GetC(card,instr);
		    } while (InType(d) <= BREAK);
		    token->type = TK_ERROR;
		    return token;
		  }
		};
                *s = 0;
		rad_int = atoi(strbuff);
		token->nextch = c;
		token->value = (char *)(&rad_int);
		if (c == '(')	/* Modified for HiLog */
			token->type = TK_INTFUNC;
		else
			token->type = TK_INT;
                return token;
 
            case BREAK:        /* Modified for HiLog */
	      do {
                    if (--n < 0) {
		      realloc_strbuff(&strbuff, &s, &n); 
		      }
                    *s++ = c, c = GetC(card,instr);
                } while (InType(c) <= LOWER);
                *s = 0;
                if (c == '(') {
                    token->nextch = c;
                    token->value = strbuff;
                    token->type = TK_VVARFUNC;
                    return token;
                } else {
		    token->nextch = c;
		    token->value = strbuff;
                    token->type = TK_VVAR;
                    return token;
                }
 
            case UPPER:         /* Modified for HiLog */
                do {
                    if (--n < 0) {
		      realloc_strbuff(&strbuff, &s, &n);
		    }
                    *s++ = c, c = GetC(card,instr);
                } while (InType(c) <= LOWER);
                *s = 0;
                if (c == '(') {
                    token->nextch = c;
                    token->value = strbuff;
                    token->type = TK_VARFUNC; 
                    return token;
                } else {
	            token->nextch = c;
		    token->value = strbuff;
                    token->type = TK_VAR;
                    return token;
                }
 
            case LOWER:
                do {
                    if (--n < 0) {
		      realloc_strbuff(&strbuff, &s, &n);
		    }
                    *s++ = c, c = GetC(card,instr);
                } while (InType(c) <= LOWER);
                *s = 0;
SYMBOL:         if (c == '(') {
		    token->nextch = c;
		    token->value = strbuff;
		    token->type = TK_FUNC;
		    return token;
                } else {
		    token->nextch = c;
		    token->value = strbuff;
		    token->type = TK_ATOM;
		    return token;
                }
 
            case SIGN:
                *s = c, d = GetC(card,instr);
                if (c == intab.begcom && d == intab.astcom) {
ASTCOM:             if (com2plain(card, instr, d, intab.endcom)) {
			SyntaxError(eofinrem);
			token->type = TK_ERROR;
			return token;
		    }
                    c = GetC(card,instr);
                    goto START;
                } else
                if (c == intab.dpoint && InType(d) == DIGIT) {
                    *s++ = '0';
                    goto DECIMAL;
                }
                while (InType(d) == SIGN) {
                    if (--n == 0) {
		      realloc_strbuff(&strbuff, &s, &n);
		    }
                    *++s = d, d = GetC(card,instr);
                }
                *++s = 0;
                if (InType(d)>=SPACE && c==intab.termin && strbuff[1]==0) {
		    token->nextch = d;
		    token->value = 0;
		    token->type = TK_EOC;
		    return token;       /* i.e. '.' followed by layout */
                }
                c = d;
                goto SYMBOL;
 
            case NOBLE:
                if (c == intab.termin) {
                    *s = 0;
		    token->nextch = ' ';
		    token->value = 0;
		    token->type = TK_EOC;
		    return token;
                } else
                if (c == intab.eolcom) {
                    c = com0plain(card, instr, intab.endeol);
                    goto START;
                }
                *s++ = c, *s = 0;
                c = GetC(card,instr);
                goto SYMBOL;
 
            case PUNCT:
                if (c == intab.termin) {
                    *s = 0;
		    token->nextch = ' ';
		    token->value = 0;
		    token->type = TK_EOC;
		    return token;
                } else
                if (c == intab.eolcom) {
                    c = com0plain(card, instr, intab.endeol);
                    goto START;
                }
                d = GetC(card,instr);
                if (c == intab.begcom && d == intab.astcom) goto ASTCOM;
 
              /*  If we arrive here, c is an ordinary punctuation mark  */
/*                  if (c == '(')  *s++ = ' '; */
		    /* In PSBProlog (as in most other Prologs) it was     */
		    /* necessary to distinguish between atom( and atom (  */
		    /* This was originally used for operators but it was  */
		    /* deleted by Jiyang - seems to cause no problem for  */
		    /* HiLog.                                             */
                *s++ = c, *s = 0;
		token->nextch = d;
		token->value = strbuff;
	   /*  In HiLog we need the following distinction so that we do not */
	   /*  recognize terms of the form f(p) (c,d) which are not HiLog   */
	   /*  terms as the same HiLog term as f(p)(c,d) which is a legal   */
           /*  HiLog term. All this mess is caused by the fact that this    */
       	   /*  scanner throws away all the spaces and we have no other way  */
           /*  of recognizing whether the next left parenthesis belongs to  */
           /*  the same term being read, (especially since it is not        */
           /*  desirable to keep the previous character read).              */
		if (c == ')' && d == '(')
		  token->type = TK_HPUNC;
		else
		  token->type = TK_PUNC;
                return token;
 
            case CHRQT:
                /*  `c[`] is read as an integer.
                    Eventually we should treat characters as a distinct
                    token type, so they can be generated on output.
                    If the character quote, atom quote, list quote,
                    or string quote is the radix character, we should
                    generate 0'x notation, otherwise `x`.
                */
                d = read_character(card, instr, -1);
                sprintf(strbuff, "%d", d);
                d = GetC(card,instr);
		rad_int = atoi(strbuff);
		token->nextch = d == c ? GetC(card,instr) : d;
		token->value = (char *)(&rad_int);
		token->type = TK_INT;
                return token;
 
            case ATMQT:
                while ((d = read_character(card, instr, c)) >= 0) {
                    if (--n < 0) {
		      realloc_strbuff(&strbuff, &s, &n);
		    }
                    *s++ = d;
                }
                *s = 0;
                c = lastc;
                goto SYMBOL;

/**** this case deleted, messed up treatment of $, which was STRQT
            case STRQT:
                while ((d = read_character(card, instr, c)) >= 0) {
                    if (--n < 0) {
		      realloc_strbuff(&strbuff, &s, &n);
		    }
                    *s++ = d;
                }
                *s = 0;
		token->nextch = lastc;
		token->value = strbuff;
		token->type = TK_STR;
                return token;
case deleted ****/

	    case LISQT: 
                while ((d = read_character(card, instr, c)) >= 0) {
                    if (--n < 0) {
		      realloc_strbuff(&strbuff, &s, &n);
		    }
                    *s++ = d;
		}
		*s = 0;
		token->nextch = lastc;
		token->value = strbuff;
		token->type = TK_LIST;
                return token;

            case EOLN:
            case SPACE:
                c = GetC(card,instr);
                goto START;
 
            case EOFCH:
		if (!instr) clearerr(card);
		token->nextch = ' ';
		token->value = 0;
		token->type = TK_EOF;
                return token;

        }
        /* There is no way we can get here */
        xsb_abort("[Internal error] InType(%d)==%d\n", c, InType(c));
        /*NOTREACHED*/
	return FALSE; /* to pacify the compiler */
}

/* --- Testing routines (usually commented) ---  
 
void main(int arc, char *argv[])
{
  FILE *card;
  struct token *res;

  card = fopen(argv[1], "r");
  if (!card) exit(1);
  token->nextch = ' ';
  do {
    res = GetToken(card, NULL, token->nextch);
    print_token(res->type, res->value);
  } while (res->type != TK_EOF);
}

void print_token(int token_type, char *ptr)
{
  switch (token_type) {
  case TK_PUNC		: printf("TK_PUNC: %c\t", *ptr); break;
  case TK_VARFUNC	: printf("TK_VARFUNC: %s\t", ptr); break;
  case TK_VAR		: printf("TK_VAR: %s\t", ptr); break;
  case TK_FUNC		: printf("TK_FUNC: %s\t", ptr); break;
  case TK_INT		: printf("TK_INT: %ld\t", *(Integer *)ptr); break;
  case TK_ATOM		: printf("TK_ATOM: %s\t", ptr); break;
  case TK_EOC		: printf("\nTK_EOC\n"); break;
  case TK_VVAR		: printf("TK_VVAR: %s\t", ptr); break;
  case TK_VVARFUNC	: printf("TK_VVARFUNC: %s\t", ptr); break;
  case TK_REAL		: printf("TK_REAL: %f\t", *(double *)ptr); break;
  case TK_EOF		: printf("\nTK_EOF\n"); break;
  case TK_STR		: printf("TK_STR: %s\t", ptr); break;
  case TK_LIST		: printf("TK_LIST: %s\t", ptr); break;
  case TK_HPUNC		: printf("TK_HPUNC: %c\t", *ptr); break;
  case TK_INTFUNC	: printf("TK_INTFUNC: %ld\t", *(Integer *)ptr); break;
  case TK_REALFUNC	: printf("TK_REALFUNC: %f\t", *(double *)ptr); break;
  }
}

 ----------------------------------------------- */
