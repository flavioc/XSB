/* File:      token_xsb.h
** Author(s): Kostis F. Sagonas, Jiyang Xu
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1999
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


#include "token_defs_xsb.h"

 
#define strgetc(p) (--(p)->strcnt>=0? ((int)*(p)->strptr++): -1)

#define GetC(card,instr) (instr ? strgetc(instr) : getc(card))

struct strbuf {
  int strcnt;
  char *strptr;
  char *strbase;
};

#define STRFILE struct strbuf
#define MAXIOSTRS 5
extern STRFILE *iostrs[MAXIOSTRS];
#define iostrdecode(j) (-1-j)
#define strfileptr(desc) iostrs[iostrdecode(desc)]

struct token {
  int type;
  char *value;
  int nextch;
};

extern struct token *token;
extern struct token *GetToken(FILE *, STRFILE *, int);

extern int intype(int);

/*======================================================================*/
/*======================================================================*/

