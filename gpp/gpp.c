/* File:      gpp.c  -- generic preprocessor
** Author:    Denis Auroux
** Contact:   auroux@math.polytechnique.fr
** Version:   1.2
** 
** Copyright (C) Denis Auroux 1996, 1999
** 
** GPP is free software; you can redistribute it and/or modify it under the
** terms of the GNU Library General Public License as published by the Free
** Software Foundation; either version 2 of the License, or (at your option)
** any later version.
** 
** GPP is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
** FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
** more details.
** 
** You should have received a copy of the GNU Library General Public License
** along with this software; if not, write to the Free Software Foundation,
** Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
**
** $Id$
** 
*/

/* To compile under MS VC++, one must define WIN_NT */

#ifdef WIN_NT
#define popen   _popen
#define pclose  _pclose
#define strdup  _strdup
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>

#define STACKDEPTH 50
#define MAXARGS 100
#define MAXSPECS 10   /* max # of include dirs and of comment delimiters */

typedef struct MODE {
  char *mStart;		/* before macro name */
  char *mEnd;		/* end macro without arg */
  char *mArgS;		/* start 1st argument */
  char *mArgSep;	/* separate arguments */
  char *mArgE;		/* end last argument */
  char *mArgRef;	/* how to refer to arguments in a def */
  char quotechar;	/* quote next char */
  char *stackchar;      /* characters to stack */
  char *unstackchar ;   /* characters to unstack */
} MODE;

struct MODE User,Meta;	/* User = user-def, Meta = #define, #ifdef, #include */

                   /*  st   end    args  sep   arge  ref   quot  stk  unstk */
struct MODE CUser   = {"",  "",    "(",  ",",  ")",  "#",  '\001', "(", ")" };
struct MODE CMeta   = {"#", "\n",  "",   "",   "\n", "#",  '\001', "(", ")" };
struct MODE Tex     = {"\\", "",   "{",  "}{", "}",  "#",  '@',  "{", "}" };
struct MODE Html    = {"<#",">",   "",   "|",  ">",  "#",  '\\', "<", ">" };

char **username,**macrotext,***argnames;
int *macrolen,*nnamedargs;
int nmacros,nalloced;
char *includedir[MAXSPECS];
char *comment_start[MAXSPECS],*comment_end[MAXSPECS];
int stayahead;                    /* to detect comment starts in time */
int nincludedirs,ncommentspecs;
int execallowed,canalias;

typedef struct OUTPUTCONTEXT {
  char *buf;
  int len,bufsize;
  FILE *f;
} OUTPUTCONTEXT;

typedef struct INPUTCONTEXT {
  char *buf;
  char *malloced_buf; /* what was actually malloc-ed (buf may have shifted) */
  int len,bufsize;
  int lineno;
  char *filename;
  FILE *in;
  int argc;
  char **argv;
  char **namedargs;
  struct INPUTCONTEXT *next;
  struct OUTPUTCONTEXT *out;
  int eof;
} INPUTCONTEXT;

struct INPUTCONTEXT *C;
  
int commented[STACKDEPTH],iflevel;

void ProcessContext(); /* the main loop */

void usage() {
  fprintf(stderr,"GPP Version 1.2 - Generic Preprocessor - (c) Denis Auroux 1996-99\n");
  fprintf(stderr,"Usage : gpp [ -o outfile ] [ -I/include/path ] [ -Dname=val ... ] [-x] [-a]\n");
  fprintf(stderr,"            [ -C | -T | -H | -U ... [ -M ... ] ] [ -c str1 str2 ] [ infile ]\n\n");
  fprintf(stderr," -C : cpp-like    #define x y           macro(arg,...)       (default)\n");
  fprintf(stderr," -T : tex-like    \\define{x}{y}         \\macro{arg}{...}\n");
  fprintf(stderr," -H : html-like   <#define x|y>         <#macro arg|...>\n");
  fprintf(stderr," -U : user-defined syntax (specified in 9 following args, see manual)\n");
  fprintf(stderr," -M : user-defined syntax for meta-macros (specified in 7 following args)\n\n");
  fprintf(stderr," -x : enable #exec built-in macro\n");
  fprintf(stderr," -a : macros with no arguments are macro aliases\n");
  fprintf(stderr," -c : use next 2 args as comment start and comment end sequences\n\n");
  exit(1);
}

void bug(char *s)
{
  fprintf(stderr,"%s:%d: error: %s.\n",C->filename,C->lineno,s);
  exit(1);
}

void warning(char *s)
{
  fprintf(stderr,"%s:%d: warning: %s.\n",C->filename,C->lineno,s);
}

int isdelim(unsigned char c)
{
  if (c>=128) return 0;
  if ((c>='0')&&(c<='9')) return 0;
  if ((c>='A')&&(c<='Z')) return 0;
  if ((c>='a')&&(c<='z')) return 0;
  if (c=='_') return 0;
  return 1;
}

int iswhite(char c)
{
  if (c==' ') return 1;
  if (c=='\t') return 1;
  return 0;
}

void newmacro(char *s,int len)
{
  if (nmacros==nalloced) {
    nalloced=2*nalloced+1;
    username=(char **)realloc((char *)username,nalloced*sizeof(char *));
    macrotext=(char **)realloc((char *)macrotext,nalloced*sizeof(char *));
    macrolen=(int *)realloc((char *)macrolen,nalloced*sizeof(int));
    argnames=(char ***)realloc((char *)argnames,nalloced*sizeof(char **));
    nnamedargs=(int *)realloc((char *)nnamedargs,nalloced*sizeof(int));
    if ((username==NULL)||(macrotext==NULL)||(macrolen==NULL)||
        (argnames==NULL)||(nnamedargs==NULL)) bug("Out of memory");
  }
  username[nmacros]=malloc(len+1);
  strncpy(username[nmacros],s,len);
  username[nmacros][len]=0;
  argnames[nmacros]=NULL;
  nnamedargs[nmacros]=0;
}

void lookupArgRefs(int n)
{
  int i,l;
  char *p;
  
  if (nnamedargs[n]>0) return; /* don't mess with those */
  nnamedargs[n]=-1;
  l=strlen(User.mArgRef);
  for (i=0,p=macrotext[n];i<macrolen[n];i++,p++) {
    if (*p==User.quotechar) { i++; p++; }
    else if (!strncmp(p,User.mArgRef,l))
      if ((p[l]>='1')&&(p[l]<='9')) 
        { nnamedargs[n]=0; return; }
  }
}

char *strnl(char *s) /* replace "\\n" by "\n" in a cmd-line arg */
{
  char *t,*u;
  t=(char *)malloc(strlen(s)+1);
  u=t;
  while (*s!=0) {
    if ((*s=='\\')&&(s[1]=='n')) { *u='\n'; s++; }
    else *u=*s;
    s++; u++;
  }
  *u=0;
  return t;
}

void parseCmdlineDefine(char *s)
{
  int l;
  
  for (l=0;s[l]&&(s[l]!='=');l++);
  newmacro(s,l);
  if (s[l]=='=') l++;
  macrolen[nmacros]=strlen(s+l);
  macrotext[nmacros++]=strdup(s+l);
}

int readModeDescription(char **args,struct MODE *mode,int ismeta)
{
  if (!(*(++args))) return 0;
  mode->mStart=strnl(*args);
  if (!(*(++args))) return 0;
  mode->mEnd=strnl(*args);
  if (!(*(++args))) return 0;
  mode->mArgS=strnl(*args); 
  if (!(*(++args))) return 0;
  mode->mArgSep=strnl(*args); 
  if (!(*(++args))) return 0;
  mode->mArgE=strnl(*args); 
  if (!(*(++args))) return 0;
  mode->stackchar=strnl(*args); 
  if (!(*(++args))) return 0;
  mode->unstackchar=strnl(*args); 
  if (ismeta) return 1;
  if (!(*(++args))) return 0;
  mode->mArgRef=strnl(*args); 
  if (!(*(++args))) return 0;
  mode->quotechar=**args;
  if (mode->quotechar==0) return 0;
  return 1;
}

void initthings(int argc,char **argv)
{
  char **arg,*s;
  int i,isinput,isoutput,ishelp,ismode,hasmeta,usrmode;

  nmacros=0;
  nalloced=31;
  username=(char **)malloc(nalloced*sizeof(char *));
  macrotext=(char **)malloc(nalloced*sizeof(char *));
  macrolen=(int *)malloc(nalloced*sizeof(int));
  argnames=(char ***)malloc(nalloced*sizeof(char **));
  nnamedargs=(int *)malloc(nalloced*sizeof(int));
  
  User=CUser;
  Meta=CMeta;
  C=(struct INPUTCONTEXT *)malloc(sizeof(struct INPUTCONTEXT));
  C->in=stdin;
  C->argc=0;
  C->argv=NULL;
  C->filename=strdup("stdin");
  C->out=(struct OUTPUTCONTEXT *)malloc(sizeof(struct OUTPUTCONTEXT));
  C->out->f=stdout;
  C->out->bufsize=0;
  C->lineno=0;
  isinput=isoutput=ismode=ishelp=hasmeta=usrmode=0;
  nincludedirs=0;
  C->bufsize=80;
  C->len=0;
  C->buf=C->malloced_buf=malloc(C->bufsize);
  C->next=NULL;
  C->eof=0;
  C->namedargs=NULL;
  commented[0]=0;
  iflevel=0;
  execallowed=0;
  canalias=0;
  ncommentspecs=0;
  stayahead=0;
  
  for (arg=argv+1;*arg;arg++) {
    if (**arg!='-') {
      ishelp|=isinput; isinput=1;
      C->in=fopen(*arg,"r");
      free(C->filename); C->filename=strdup(*arg);
      if (C->in==NULL) bug("Cannot open input file");
    }
    else switch((*arg)[1]) {
      case 'I': if (nincludedirs==MAXSPECS) 
                   bug("too many include directories");
                includedir[nincludedirs++]=strdup((*arg)+2); break;
      case 'C': ishelp|=ismode|hasmeta|usrmode; ismode=1;
                User=CUser; Meta=CMeta;
                break;
      case 'T': ishelp|=ismode|hasmeta|usrmode; ismode=1;
                User=Meta=Tex;
                break;
      case 'H': ishelp|=ismode|hasmeta|usrmode; ismode=1;
                User=Meta=Html;
                break;
      case 'U': ishelp|=ismode|usrmode; usrmode=1;
                if (!readModeDescription(arg,&User,0)) usage();
                arg+=9;
                if (!hasmeta) Meta=User;
                break;
      case 'M': ishelp|=ismode|hasmeta; hasmeta=1;
                if (!readModeDescription(arg,&Meta,1)) usage();
                arg+=7;
                break;
      case 'o': if (!(*(++arg))) usage();
                ishelp|=isoutput; isoutput=1;
                C->out->f=fopen(*arg,"w");
                if (C->out->f==NULL) bug("Cannot create output file");
                break;
      case 'D': s=strnl((*arg)+2); parseCmdlineDefine(s); free(s); break;
      case 'x': execallowed=1; break;
      case 'a': canalias=1; break;
      case 'c': if (ncommentspecs==MAXSPECS)
                  bug("Too many comment delimiter specifications");
                if (!(*(++arg))) usage();
                comment_start[ncommentspecs]=strnl(*arg);
                if (!(*(++arg))) usage();
                comment_end[ncommentspecs]=strnl(*arg);
                if ((comment_start[ncommentspecs][0]==0)||
                    (comment_end[ncommentspecs][0]==0))
                   bug("Comment delimiters must be non-empty");
                i=strlen(comment_start[ncommentspecs])-1;
                if (i>stayahead) stayahead=i;
                ncommentspecs++;
                break;
      default:  ishelp=1;
    }
    if (hasmeta&&!usrmode) usage();
    if (ishelp) usage();
  }
  
  if (nincludedirs==0) {
    includedir[0]=strdup("/usr/include");
    nincludedirs=1;
  }
  for (i=0;i<nmacros;i++) lookupArgRefs(i); /* in case of canalias */
}

void outchar(char c)
{
  if (C->out->bufsize) {
    if (C->out->len+1==C->out->bufsize) { 
      C->out->bufsize=C->out->bufsize*2;
      C->out->buf=realloc(C->out->buf,C->out->bufsize);
      if (C->out->buf==NULL) bug("Out of memory");
    }
    C->out->buf[C->out->len++]=c;
  }
  else fputc(c,C->out->f);
}

void sendout(char *s,int l,int proc) /* only process the quotechar, that's all */
{
  int i;
  
  if (!commented[iflevel])
    for (i=0;i<l;i++) {
      if (proc&&(s[i]==User.quotechar)) { i++; if (i==l) return; }
      if (s[i]!=0) outchar(s[i]);
    }
}

void extendBuf(int pos)
{
  char *p;
  if (C->bufsize<=pos) {
    C->bufsize+=pos; /* approx double */
    p=(char *)malloc(C->bufsize);
    memcpy(p,C->buf,C->len);
    free(C->malloced_buf);
    C->malloced_buf=C->buf=p;
    if (C->buf==NULL) bug("Out of memory");
  }
}

char getChar(int pos)
{
  int c,i,l,n;

  if (C->in==NULL) {
    if (pos>=C->len) return 0;
    else return C->buf[pos];
  }
  extendBuf(pos+stayahead);
  while (pos+stayahead>=C->len) {
    c=fgetc(C->in);
    if (c=='\n') C->lineno++;
    if (c==EOF) c=0;
    C->buf[C->len++]=(char)c;
    
    /* handle comments */
    for (i=0;i<ncommentspecs;i++) {
      l=strlen(comment_start[i]);
      if ((C->len>=l)&&!strncmp(C->buf+C->len-l,comment_start[i],l)) {
        /* if it was quoted, don't take it as a comment */
        if ((C->len>l)&&(C->buf[C->len-l-1]==User.quotechar)) continue;
        C->len-=l;
        n=C->len;
        l=strlen(comment_end[i]);
        while ((n<l)||(C->buf[n-l]!=comment_end[i][0])||strncmp(C->buf+n-l,comment_end[i],l)) {
          extendBuf(n);
          c=fgetc(C->in);
          if (c=='\n') C->lineno++;
          if (c==EOF) bug("Input ended in a comment");
          C->buf[n++]=(char)c;
        }
        /* special magic when \n is comment end */
        if ((n>C->len+l)&&(l==1)&&(comment_end[i][0]=='\n'))
          C->buf[C->len++]='\n';
      }
    }
  }
  return C->buf[pos];
}

int whiteout(int *pos1,int *pos2) /* remove whitespace on both sides */
{
  while ((*pos1<*pos2)&&iswhite(getChar(*pos1))) (*pos1)++;
  while ((*pos1<*pos2)&&iswhite(getChar(*pos2-1))) (*pos2)--;
  return (*pos1<*pos2);
}

int identifierEnd(int start)
{
  char c;
  int i;
  
  i=start-1;
  do {
    c=getChar(++i);
    while (c==User.quotechar) { i+=2; c=getChar(i); }
  } while (!isdelim(c));
  return i;
}

int idequal(char *b,int l,char *s)
{
  int i;
  
  if ((int)strlen(s)!=l) return 0;
  for (i=0;i<l;i++) if (b[i]!=s[i]) return 0;
  return 1;
}

int findIdent(char *b,int l)
{
  int i;
  
  for (i=0;i<nmacros;i++)
    if (idequal(b,l,username[i])) return i;
  return -1;
}

int findNamedArg(char *b,int l)
{
  char *s; 
  int i;

  for (i=0;;i++) {
    s=C->namedargs[i];
    if (s==NULL) return -1;
    if (idequal(b,l,s)) return i;
  } 
}

/* look for a possible user macro.
   Input :  idstart = scan start
            idcheck = check id for long macro forms before splicing args ?
   Output : idstart/idend = macro name location
            sh_end/lg_end = macro form end (-1 if no match)
            argb/arge     = argument locations for long form
            argc          = argument count for long form
            id            = macro id, if idcheck was set at input 
*/

int SplicePossibleUser(int *idstart,int *idend,int *sh_end,int *lg_end,
                       int *argb,int *arge,int *argc,int idcheck,int *id)
{
  int match,match2,i,k,pos;

  for (i=0;User.mStart[i]!=0;i++)
    if (getChar(i+(*idstart))!=User.mStart[i]) return 0;
  *idstart+=i;
  *idend=identifierEnd(*idstart);
  if ((*idend)&&!getChar(*idend-1)) return 0;

  /* look for args */
  match=1;
  for (i=0;User.mEnd[i]!=0;i++)
    if (getChar(*idend+i)!=User.mEnd[i]) match=0;
  if (match) *sh_end=*idend+strlen(User.mEnd);
  else *sh_end=-1;
  match=1;
  for (i=0;User.mArgS[i]!=0;i++)
    if (getChar(*idend+i)!=User.mArgS[i]) match=0;
  *lg_end=-1;
  
  if (idcheck) {
    *id=findIdent(C->buf+*idstart,*idend-*idstart);
    if (*id<0) match=0;
  }
    
  if (match) {
    *argc=0;
    pos=*idend+strlen(User.mArgS);
    if (!User.mArgS[0]) /* if separator was whitespace */
      while (iswhite(getChar(pos))) {
        if (getChar(pos)==0) return (*sh_end>=0); /* EOF */
        pos++;
      }
    while (1)
    {
      if (*argc>=MAXARGS) bug("too many macro parameters");
      argb[*argc]=pos;
      k=0; pos--;
      do { /* look for mArgE or mArgSep */
        if (getChar(pos)==0) return (*sh_end>=0); /* EOF */
        pos=identifierEnd(pos+1);
        match=match2=0;
        if (strchr(User.stackchar,getChar(pos))) k++;
        if (k) { if (strchr(User.unstackchar,getChar(pos))) k--; }
        else {
          match=match2=1;
          for (i=0;User.mArgE[i]!=0;i++)
            if (getChar(pos+i)!=User.mArgE[i]) match2=0;
          for (i=0;User.mArgSep[i]!=0;i++)
            if (getChar(pos+i)!=User.mArgSep[i]) match=0;
        }
      }
      while ((!match)&&(!match2));
      arge[*argc]=pos;
      (*argc)++;
      if (match&&match2) { /* both end of macro and arg sep match */
        if (strlen(User.mArgE)>strlen(User.mArgSep)) match=0;
        else match2=0;
      }
      if (match2) { /* no more args */
        *lg_end=pos+strlen(User.mArgE);
        break;
      }
      pos=pos+strlen(User.mArgSep);
      if (!User.mArgSep[0]) /* if separator was whitespace, eat it */
        while (iswhite(getChar(pos))) {
          if (getChar(pos)==0) return (*sh_end>=0); /* EOF */
          pos++;
        }
    }
  }
  return ((*lg_end>=0)||(*sh_end>=0));
}

int findMetaArgs(int start,int *p1b,int *p1e,int *p2b,int *p2e,int *endm,int *argc,int *argb,int *arge)
{
  int pos,i,k,matchm,matcharg;
  int hyp_end1,hyp_end2;
  
  /* look for mEnd or mArgS */
  matchm=matcharg=1;
  for (i=0;Meta.mEnd[i]!=0;i++)
    if (getChar(start+i)!=Meta.mEnd[i]) matchm=0;
  for (i=0;Meta.mArgS[i]!=0;i++)
    if (getChar(start+i)!=Meta.mArgS[i]) matcharg=0;
  if ((!matchm)&&(!matcharg)) return -1;
  if (matchm&&matcharg) { /* both end of macro and arg start match */
    if (strlen(Meta.mEnd)>strlen(Meta.mArgS))
      matcharg=0;
    else matchm=0;
  }
  if (matchm) { /* no args */
    *endm=start+strlen(Meta.mEnd);
    return 0;
  }
  pos=start+strlen(Meta.mArgS);
  if (!Meta.mArgS[0]) /* if separator was whitespace, eat it */
    while (iswhite(getChar(pos))) {
      if (getChar(pos)==0) bug("unfinished macro");
      pos++;
    }
  *p1b=pos;

  /* special syntax for #define : 1st arg is a macro call */
  if ((*argc)&&
      SplicePossibleUser(&pos,p1e,&hyp_end1,&hyp_end2,argb,arge,argc,0,NULL))
  {
    *p1b=pos;
    if (hyp_end2>=0) pos=hyp_end2;
    else { pos=hyp_end1; *argc=0; }
    matchm=matcharg=1;
    for (i=0;Meta.mArgE[i]!=0;i++)
      if (getChar(pos+i)!=Meta.mArgE[i]) matchm=0;
    for (i=0;Meta.mArgSep[i]!=0;i++)
      if (getChar(pos+i)!=Meta.mArgSep[i]) matcharg=0;
    if (!matchm&&!matcharg) 
      bug("#define/#defeval requires an identifier or a single macro call");
  }
  else {
    *argc=0;
    k=0; pos--;
    do { /* look for mArgE or mArgSep */
      if (getChar(pos)==0) bug("unfinished macro argument");
      pos=identifierEnd(pos+1);
      matchm=matcharg=0;
      if (strchr(Meta.stackchar,getChar(pos))) k++;
      if (k) { if (strchr(Meta.unstackchar,getChar(pos))) k--; }
      else {
        matchm=matcharg=1;
        for (i=0;Meta.mArgE[i]!=0;i++)
          if (getChar(pos+i)!=Meta.mArgE[i]) matchm=0;
        for (i=0;Meta.mArgSep[i]!=0;i++)
          if (getChar(pos+i)!=Meta.mArgSep[i]) matcharg=0;
      }
    }
    while ((!matchm)&&(!matcharg));
    *p1e=pos;
  }
  
  if (matchm&&matcharg) { /* both end of macro and arg sep match */
    if (strlen(Meta.mArgE)>strlen(Meta.mArgSep))
      matcharg=0;
    else matchm=0;
  }
  if (matchm) { /* no more args */
    *endm=pos+strlen(Meta.mArgE);
    return 1;
  }
  pos=pos+strlen(Meta.mArgSep);
  if (!Meta.mArgSep[0]) /* if separator was whitespace, eat it */
    while (iswhite(getChar(pos))) {
      if (getChar(pos)==0) bug("unfinished macro");
      pos++;
    }
  *p2b=pos;
  
  k=0; pos--;
  do {  
    if (getChar(pos)==0) bug("unfinished macro");
    pos=identifierEnd(pos+1);
    if (strchr(Meta.stackchar,getChar(pos))) k++;
    matchm=0;
    if (k) { if (strchr(Meta.unstackchar,getChar(pos))) k--; }
    else {
      matchm=1;
      for (i=0;Meta.mArgE[i]!=0;i++)
        if (getChar(pos+i)!=Meta.mArgE[i]) matchm=0;
    }
  } while (!matchm);
  *p2e=pos;
  *endm=pos+strlen(Meta.mArgE);
  return 2;
}

void shiftIn(int l)
{
  int i;
  
  if (l==0) return;
  if (l<C->len) {
    if (C->len-l>100) { /* we want to shrink that buffer */
      C->buf+=l; C->bufsize-=l;
    } else
      for (i=l;i<C->len;i++) C->buf[i-l]=C->buf[i];
    C->len-=l;
    C->eof=(*(C->buf)==0);
  }
  else {
    C->len=0;
    if (C->in==NULL) C->eof=1;
      else C->eof=feof(C->in);
  }
}

char *ProcessText(char *buf,int l,struct INPUTCONTEXT *parent)
{
  char *s;
  struct INPUTCONTEXT *T;
  
  s=malloc(l+1);
  memcpy(s,buf,l);
  s[l]=0;
  if (l==0) return s; /* return "" if nothing to do */
  T=C;
  C=(struct INPUTCONTEXT *)malloc(sizeof(struct INPUTCONTEXT));
  C->out=(struct OUTPUTCONTEXT *)malloc(sizeof(struct OUTPUTCONTEXT));
  C->in=NULL;
  C->argc=parent->argc;
  C->argv=parent->argv;
  C->filename=T->filename;
  C->out->buf=malloc(80);
  C->out->len=0;
  C->out->bufsize=80;
  C->out->f=NULL;
  C->lineno=T->lineno;
  C->bufsize=l+1;
  C->len=l;
  C->buf=C->malloced_buf=s;
  C->next=NULL;
  C->eof=0;
  C->namedargs=parent->namedargs;
  
  ProcessContext();
  outchar(0); /* note that outchar works with the half-destroyed context ! */
  s=C->out->buf;
  free(C->out);
  free(C);
  C=T;
  return s;
}

int SpliceInfix(char *buf,int pos1,int pos2,char *sep,int *spl1,int *spl2)
{
  int pos,numpar,l;
  char *p;
  
  numpar=0; l=strlen(sep);
  for (pos=pos2-1,p=buf+pos;pos>=pos1;pos--,p--) {
    if (*p==')') numpar++;
    if (*p=='(') numpar--;
    if (numpar<0) return 0;
    if ((numpar==0)&&(pos2-pos>=l)&&!strncmp(p,sep,l))
      { *spl1=pos; *spl2=pos+l; return 1; }
  }
  return 0;
}

int DoArithmEval(char *buf,int pos1,int pos2,int *result)
{
  int spl1,spl2,result1,result2;
  char c,*p;
  
  while ((pos1<pos2)&&iswhite(buf[pos1])) pos1++;
  while ((pos1<pos2)&&iswhite(buf[pos2-1])) pos2--;
  if (pos1==pos2) return 0;
  
  /* look for C operators starting with lowest precedence */
  
  if (SpliceInfix(buf,pos1,pos2,"||",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2)) return 0;
    *result=result1||result2;
    return 1;
  }

  if (SpliceInfix(buf,pos1,pos2,"&&",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2)) return 0;
    *result=result1&&result2;
    return 1;
  }

  if (SpliceInfix(buf,pos1,pos2,"|",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2)) return 0;
    *result=result1|result2;
    return 1;
  }

  if (SpliceInfix(buf,pos1,pos2,"^",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2)) return 0;
    *result=result1^result2;
    return 1;
  }

  if (SpliceInfix(buf,pos1,pos2,"&",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2)) return 0;
    *result=result1&result2;
    return 1;
  }

  if (SpliceInfix(buf,pos1,pos2,"!=",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2))
    { /* revert to string comparison */
      while ((pos1<spl1)&&iswhite(buf[spl1-1])) spl1--;
      while ((pos2>spl2)&&iswhite(buf[spl2])) spl2++;
      if (spl1-pos1!=pos2-spl2) *result=1;
      else *result=(strncmp(buf+pos1,buf+spl2,spl1-pos1)!=0);
    }  
    else *result=(result1!=result2);
    return 1;
  }
  
  if (SpliceInfix(buf,pos1,pos2,"==",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2))
    { /* revert to string comparison */
      while ((pos1<spl1)&&iswhite(buf[spl1-1])) spl1--;
      while ((pos2>spl2)&&iswhite(buf[spl2])) spl2++;
      if (spl1-pos1!=pos2-spl2) *result=0;
      else *result=(strncmp(buf+pos1,buf+spl2,spl1-pos1)==0);
    }  
    else *result=(result1==result2);
    return 1;
  }

  if (SpliceInfix(buf,pos1,pos2,">=",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2)) return 0;
    *result=result1>=result2;
    return 1;
  }

  if (SpliceInfix(buf,pos1,pos2,">",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2)) return 0;
    *result=result1>result2;
    return 1;
  }
  
  if (SpliceInfix(buf,pos1,pos2,"<=",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2)) return 0;
    *result=result1<=result2;
    return 1;
  }

  if (SpliceInfix(buf,pos1,pos2,"<",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2)) return 0;
    *result=result1<result2;
    return 1;
  }
  
  if (SpliceInfix(buf,pos1,pos2,"-",&spl1,&spl2))
    if (spl1!=pos1) {
      if (!DoArithmEval(buf,pos1,spl1,&result1)||
          !DoArithmEval(buf,spl2,pos2,&result2)) return 0;
      *result=result1-result2;
      return 1;
    }

  if (SpliceInfix(buf,pos1,pos2,"+",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2)) return 0;
    *result=result1+result2;
    return 1;
  }
  
  if (SpliceInfix(buf,pos1,pos2,"%",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2)) return 0;
    if (result2==0) bug("Division by zero in expression");
    *result=result1%result2;
    return 1;
  }

  if (SpliceInfix(buf,pos1,pos2,"/",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2)) return 0;
    if (result2==0) bug("Division by zero in expression");
    *result=result1/result2;
    return 1;
  }
  
  if (SpliceInfix(buf,pos1,pos2,"*",&spl1,&spl2)) {
    if (!DoArithmEval(buf,pos1,spl1,&result1)||
        !DoArithmEval(buf,spl2,pos2,&result2)) return 0;
    *result=result1*result2;
    return 1;
  }

  if (buf[pos1]=='~') {
    if (!DoArithmEval(buf,pos1+1,pos2,&result1)) return 0;
    *result=~result1;
    return 1;
  }

  if (buf[pos1]=='!') {
    if (!DoArithmEval(buf,pos1+1,pos2,&result1)) return 0;
    *result=!result1;
    return 1;
  }

  if (buf[pos1]=='-') {
    if (!DoArithmEval(buf,pos1+1,pos2,&result1)) return 0;
    *result=-result1;
    return 1;
  }
  
  if (buf[pos1]=='(') {
    if (buf[pos2-1]!=')') return 0;
    return DoArithmEval(buf,pos1+1,pos2-1,result);
  }
  
  c=buf[pos2]; buf[pos2]=0;
  *result=(int)strtol(buf+pos1,&p,0);
  buf[pos2]=c;
  return (p==buf+pos2);
}

char *ArithmEval(int pos1,int pos2)
{
  char *s,*t;
  int i;
  
  /* first define the defined(...) operator */
  i=findIdent("defined",strlen("defined"));
  if (i>=0) warning("the defined(...) macro is already defined");
  else {
    newmacro("defined",strlen("defined"));
    macrolen[nmacros]=0;
    macrotext[nmacros]=malloc(1);
    macrotext[nmacros][0]=0;
    nnamedargs[nmacros]=-2; /* trademark of the defined(...) macro */
    nmacros++;
  }
  /* process the text in a usual way */
  s=ProcessText(C->buf+pos1,pos2-pos1,C);
  /* undefine the defined(...) operator */
  if (i<0) {
    i=findIdent("defined",strlen("defined"));
    if ((i<0)||(nnamedargs[i]!=-2))
      warning("the defined(...) macro was redefined in expression");
    else {
      nmacros--;
      free(username[i]);
      free(macrotext[i]); 
      username[i]=username[nmacros];
      macrotext[i]=macrotext[nmacros];
      macrolen[i]=macrolen[nmacros];
      nnamedargs[i]=nnamedargs[nmacros];
      argnames[i]=argnames[nmacros];
    }
  }

  if (!DoArithmEval(s,0,strlen(s),&i)) return s; /* couldn't compute */
  t=malloc(15);
  sprintf(t,"%d",i);
  free(s);
  return t;
}

int ParsePossibleMeta()
{
  int cklen,nameend;
  int id,expparams,nparam,i,j;
  int p1start,p1end,p2start,p2end,macend;
  int argc,argb[MAXARGS],arge[MAXARGS];
  
  for (cklen=0;Meta.mStart[cklen]!=0;cklen++)
    if (getChar(cklen)!=Meta.mStart[cklen]) return -1; /* check mStart */
  nameend=identifierEnd(cklen);
  if (nameend&&!getChar(nameend-1)) return -1;
  id=0;
  argc=0; /* for #define with named args */
  if (idequal(C->buf+cklen,nameend-cklen,"define"))      /* check identifier */
   { id=1; expparams=2; argc=1; }
  else if (idequal(C->buf+cklen,nameend-cklen,"undef"))
   { id=2; expparams=1; }
  else if (idequal(C->buf+cklen,nameend-cklen,"ifdef"))
   { id=3; expparams=1; }
  else if (idequal(C->buf+cklen,nameend-cklen,"ifndef"))
   { id=4; expparams=1; }
  else if (idequal(C->buf+cklen,nameend-cklen,"else"))
   { id=5; expparams=0; }
  else if (idequal(C->buf+cklen,nameend-cklen,"endif"))
   { id=6; expparams=0; }
  else if (idequal(C->buf+cklen,nameend-cklen,"include"))
   { id=7; expparams=2; } /* because filename is not always an identifier */
  else if (idequal(C->buf+cklen,nameend-cklen,"exec"))
   { id=8; expparams=2; }
  else if (idequal(C->buf+cklen,nameend-cklen,"defeval"))
   { id=9; expparams=2; argc=1; }
  else if (idequal(C->buf+cklen,nameend-cklen,"ifeq"))
   { id=10; expparams=2; }
  else if (idequal(C->buf+cklen,nameend-cklen,"ifneq"))
   { id=11; expparams=2; }
  else if (idequal(C->buf+cklen,nameend-cklen,"eval"))
   { id=12; expparams=2; }
  else if (idequal(C->buf+cklen,nameend-cklen,"if"))
   { id=13; expparams=2; }
  else return -1;
  nparam=findMetaArgs(nameend,&p1start,&p1end,&p2start,&p2end,&macend,&argc,argb,arge);
  if (nparam==-1) return -1; 
  if (expparams&&!nparam) bug("Missing argument in meta-macro");
  if ((nparam==2)&&(expparams!=2)) warning("Too many arguments in meta-macro");

  switch(id) {
    case 1: /* DEFINE */
     if (!commented[iflevel]) {
      whiteout(&p1start,&p1end);
      if ((p1start==p1end)||(identifierEnd(p1start)!=p1end)) 
        bug("#define requires an identifier (A-Z,a-z,0-9,_ only)");
      i=findIdent(C->buf+p1start,p1end-p1start);
      if (i<0) {
        newmacro(C->buf+p1start,p1end-p1start);
        i=nmacros;
        nmacros++;
      }
      else { 
        free(macrotext[i]); 
        if (argnames[i]!=NULL) {
          for (j=0;j<nnamedargs[i];j++) free(argnames[i][j]);
          free(argnames[i]);
          argnames[i]=NULL;
        }
      }
      if (nparam==1) { p2end=p2start=p1end; }
      macrolen[i]=p2end-p2start;
      macrotext[i]=malloc(p2end-p2start+1);
      if (p2end>p2start) 
        memcpy(macrotext[i],C->buf+p2start,p2end-p2start);
      macrotext[i][p2end-p2start]=0;
      nnamedargs[i]=argc;
      if (argc) {
        argnames[i]=(char **)malloc((argc+1)*sizeof(char *));
        argnames[i][argc]=NULL;
      }
      for (j=0;j<argc;j++) {
        whiteout(argb+j,arge+j);
        if ((argb[j]==arge[j])||(identifierEnd(argb[j])!=arge[j]))
          bug("#define with named args needs identifiers as arg names");
        argnames[i][j]=malloc(arge[j]-argb[j]+1);
        memcpy(argnames[i][j],C->buf+argb[j],arge[j]-argb[j]);
        argnames[i][j][arge[j]-argb[j]]=0;
      }
      lookupArgRefs(i);
     }
     break;
     
    case 2: /* UNDEF */
     if (!commented[iflevel]) {
      whiteout(&p1start,&p1end);
      if ((p1start==p1end)||(identifierEnd(p1start)!=p1end))
        bug("#undef requires an identifier (A-Z,a-z,0-9,_ only)");
      i=findIdent(C->buf+p1start,p1end-p1start);
      if (i!=-1) {
        nmacros--;
        free(username[i]);
        free(macrotext[i]); 
        if (argnames[i]!=NULL) {
          for (j=0;j<nnamedargs[i];j++) free(argnames[i][j]);
          free(argnames[i]);
          argnames[i]=NULL;
        }
        username[i]=username[nmacros];
        macrotext[i]=macrotext[nmacros];
        macrolen[i]=macrolen[nmacros];
        nnamedargs[i]=nnamedargs[nmacros];
        argnames[i]=argnames[nmacros];
      }
     }
     break;

    case 3: /* IFDEF */
     iflevel++;
     if (iflevel==STACKDEPTH) bug("Too many nested #ifdefs");
     commented[iflevel]=commented[iflevel-1];
     if (!commented[iflevel]) {
      whiteout(&p1start,&p1end);
      if ((p1start==p1end)||(identifierEnd(p1start)!=p1end))
        bug("#ifdef requires an identifier (A-Z,a-z,0-9,_ only)");
      i=findIdent(C->buf+p1start,p1end-p1start);
      commented[iflevel]=(i==-1);
     }
     break;

    case 4: /* IFNDEF */
     iflevel++;
     if (iflevel==STACKDEPTH) bug("Too many nested #ifdefs");
     commented[iflevel]=commented[iflevel-1];
     if (!commented[iflevel]) {
      whiteout(&p1start,&p1end);
      if ((p1start==p1end)||(identifierEnd(p1start)!=p1end))
        bug("#ifndef requires an identifier (A-Z,a-z,0-9,_ only)");
      i=findIdent(C->buf+p1start,p1end-p1start);
      commented[iflevel]=(i!=-1);
     }
     break;
    
    case 5: /* ELSE */
     if (iflevel==0) bug("#else without #if");
     if (!commented[iflevel-1]) commented[iflevel]=!commented[iflevel];
     break;

    case 6: /* ENDIF */
     if (iflevel==0) bug("#endif without #if");
     iflevel--;
     break;

    case 7: /* INCLUDE */
     if (!commented[iflevel]) {
       struct INPUTCONTEXT *N;
       FILE *f;
       char *s;

       if (nparam==2) {
         if (Meta.mArgSep[0]) warning("Too many arguments in meta-macro");
         else /* no arg separator -> the filename may be butchered */
           p1end=p2end;
       }
       if (!whiteout(&p1start,&p1end)) bug("Missing file name in #include");
       if (Meta.mArgS[0]==0)
         /* no argstart separator -> user may put "" or <> */
         if (((getChar(p1start)=='\"')&&(getChar(p1end-1)=='\"'))||
             ((getChar(p1start)=='<')&&(getChar(p1end-1)=='>')))
           { p1start++; p1end--; }
       if (p1start>=p1end) bug("Missing file name in #include");
       s=malloc(p1end-p1start+1);
       for (i=0;i<p1end-p1start;i++) s[i]=getChar(p1start+i);
       s[p1end-p1start]=0;
       f=fopen(s,"r");
       for (j=0;(f==NULL)&&(j<nincludedirs);j++) {
         s=realloc(s,p1end-p1start+strlen(includedir[j])+2);
         strcpy(s,includedir[j]);
         s[strlen(includedir[j])]='/';
         for (i=0;i<p1end-p1start;i++) 
           s[strlen(includedir[j])+1+i]=getChar(p1start+i);
         s[p1end-p1start+strlen(includedir[j])+1]=0;
         f=fopen(s,"r");
       }
       if (f==NULL) bug("Requested include file not found");
       N=(struct INPUTCONTEXT *)malloc(sizeof(struct INPUTCONTEXT));
       N->in=f;
       N->argc=0;
       N->argv=NULL;
       N->filename=s;
       N->out=C->out;
       N->lineno=0;
       N->bufsize=80;
       N->len=0;
       N->buf=N->malloced_buf=malloc(N->bufsize);
       N->next=C;
       N->eof=0;
       N->namedargs=NULL;
       shiftIn(macend);
       macend=0;
       C=N;
     }
     break;

    case 8: /* EXEC */
     if (!commented[iflevel])
       if (!execallowed)
         warning("Not allowed to #exec. Command output will be left blank");
       else {
         char *s,*t;
         int c;
         FILE *f;
         s=ProcessText(C->buf+p1start,p1end-p1start,C);
         if (nparam==2) {
           t=ProcessText(C->buf+p2start,p2end-p2start,C);
           i=strlen(s);
           s=realloc(s,i+strlen(t)+2);
           s[i]=' ';
           strcpy(s+i+1,t);
           free(t);
         }
         f=popen(s,"r");
         free(s);
         if (f==NULL) warning("Cannot #exec. Command not found ?");
         else {
           while ((c=fgetc(f)) != EOF) outchar((char)c);
           pclose(f);
         }
       }
     break;

    case 9: /* DEFEVAL */
     if (!commented[iflevel]) {
      whiteout(&p1start,&p1end);
      if ((p1start==p1end)||(identifierEnd(p1start)!=p1end)) 
        bug("#defeval requires an identifier (A-Z,a-z,0-9,_ only)");
      i=findIdent(C->buf+p1start,p1end-p1start);
      if (i<0) {
        newmacro(C->buf+p1start,p1end-p1start);
        i=nmacros;
        nmacros++;
      }
      else {
        free(macrotext[i]); 
        if (argnames[i]!=NULL) {
          for (j=0;j<nnamedargs[i];j++) free(argnames[i][j]);
          free(argnames[i]);
          argnames[i]=NULL;
        }
      }
      if (nparam==1) { p2end=p2start=p1end; }
      macrotext[i]=ProcessText(C->buf+p2start,p2end-p2start,C);
      macrolen[i]=strlen(macrotext[i]);
      nnamedargs[i]=argc;
      if (argc) {
        argnames[i]=(char **)malloc((argc+1)*sizeof(char *));
        argnames[i][argc]=NULL;
      }
      for (j=0;j<argc;j++) {
        if ((argb[j]==arge[j])||(identifierEnd(argb[j])!=arge[j]))
          bug("#defeval with named args needs identifiers as arg names");
        argnames[i][j]=malloc(arge[j]-argb[j]+1);
        memcpy(argnames[i][j],C->buf+argb[j],arge[j]-argb[j]);
        argnames[i][j][arge[j]-argb[j]]=0;
      }
      lookupArgRefs(i);
     }
     break;
     
    case 10: /* IFEQ */
     iflevel++;
     if (iflevel==STACKDEPTH) bug("Too many nested #ifeqs");
     commented[iflevel]=commented[iflevel-1];
     if (!commented[iflevel]) {
      char *s,*t;
      if (nparam!=2) bug("#ifeq requires two arguments");
      s=ProcessText(C->buf+p1start,p1end-p1start,C);
      t=ProcessText(C->buf+p2start,p2end-p2start,C);
      commented[iflevel]=(strcmp(s,t)!=0);
      free(s); free(t);
     }
     break;

    case 11: /* IFNEQ */
     iflevel++;
     if (iflevel==STACKDEPTH) bug("Too many nested #ifeqs");
     commented[iflevel]=commented[iflevel-1];
     if (!commented[iflevel]) {
      char *s,*t;
      if (nparam!=2) bug("#ifneq requires two arguments");
      s=ProcessText(C->buf+p1start,p1end-p1start,C);
      t=ProcessText(C->buf+p2start,p2end-p2start,C);
      commented[iflevel]=(strcmp(s,t)==0);
      free(s); free(t);
     }
     break;

    case 12: /* EVAL */
     if (!commented[iflevel]) {
       char *s,*t;
       if (nparam==2) p1end=p2end; /* we really want it all ! */
       s=ArithmEval(p1start,p1end);
       for (t=s;*t;t++) outchar(*t);
       free(s);
     }
     break;

    case 13: /* IF */
     iflevel++;
     if (iflevel==STACKDEPTH) bug("Too many nested #ifs");
     commented[iflevel]=commented[iflevel-1];
     if (!commented[iflevel]) {
       char *s;
       if (nparam==2) p1end=p2end; /* we really want it all ! */
       s=ArithmEval(p1start,p1end);
       commented[iflevel]=((s[0]=='0')&&(s[1]==0));
       free(s);
     }
     break;

    default: bug("Internal meta-macro identification error");
  }
  shiftIn(macend);
  return 0;
}

int ParsePossibleUser()
{
  int idstart,idend,sh_end,lg_end,macend;
  int argc,id,i,l;
  char *argv[MAXARGS];
  int argb[MAXARGS],arge[MAXARGS];
  struct INPUTCONTEXT *T;

  idstart=0;
  id=0;
  if (!SplicePossibleUser(&idstart,&idend,&sh_end,&lg_end,argb,arge,&argc,1,&id))
    return -1;
  if ((sh_end>=0)&&(C->namedargs!=NULL)) {
    i=findNamedArg(C->buf+idstart,idend-idstart);
    if (i>=0) {
      if (i<C->argc) sendout(C->argv[i],strlen(C->argv[i]),0);
      shiftIn(sh_end);
      return 0;
    }
  }
  if (id<0) return -1;
  if (lg_end>=0) macend=lg_end; else { macend=sh_end; argc=0; }

  if (nnamedargs[id]==-2) { /* special defined(...) macro for arithmetics */
    if (argc!=1) return -1;
    whiteout(argb,arge);
    if (findIdent(C->buf+argb[0],arge[0]-argb[0])>=0) outchar('1');
    else outchar('0');
    shiftIn(macend);
    return 0;
  }
  if (!macrotext[id][0]) { /* the empty macro */
    shiftIn(macend);
    return 0;
  }
  
  for (i=0;i<argc;i++)
    argv[i]=ProcessText(C->buf+argb[i],arge[i]-argb[i],C);
  /* process macro text */
  T=C;
  C=(struct INPUTCONTEXT *)malloc(sizeof(struct INPUTCONTEXT));
  C->out=T->out;
  C->in=NULL;
  C->argc=argc;
  C->argv=argv;
  C->filename=T->filename;
  C->lineno=T->lineno;
  if (canalias&&(nnamedargs[id]==-1)&&(lg_end>=0)&&(User.mEnd[0]==0)) {
    /* build an aliased macro call */
    l=strlen(macrotext[id])+1+strlen(User.mArgS)+strlen(User.mArgE)
      +(argc-1)*strlen(User.mArgSep);
    for (i=0;i<argc;i++) l+=strlen(argv[i]);
    C->buf=C->malloced_buf=malloc(l);
    l=strlen(macrotext[id]);
    strcpy(C->buf,macrotext[id]);
    while ((l>0)&&iswhite(C->buf[l-1])) l--;
    strcpy(C->buf+l,User.mArgS);
    for (i=0;i<argc;i++) {
      if (i>0) strcat(C->buf,User.mArgSep);
      strcat(C->buf,argv[i]);
    }
    strcat(C->buf,User.mArgE);
  } 
  else C->buf=C->malloced_buf=strdup(macrotext[id]);
  C->len=strlen(C->buf);
  C->bufsize=C->len+1;
  C->next=NULL;
  C->eof=0;
  C->namedargs=argnames[id];
  ProcessContext();
  free(C);
  C=T;
  
  for (i=0;i<argc;i++) free(argv[i]);
  shiftIn(macend);
  return 0;
}

void ParseText()
{
  int l,isref;
  char c;

  if (ParsePossibleMeta()>=0) return;
  if (ParsePossibleUser()>=0) return;
  
  isref=1;
  for (l=0;User.mArgRef[l]!=0;l++) 
    if (getChar(l)!=User.mArgRef[l]) isref=0;
  if (isref) {
    l=strlen(User.mArgRef);
    c=getChar(l);
    if ((c>='1')&&(c<='9')) {
      c=c-'1';
      if (c<C->argc)
        sendout(C->argv[c],strlen(C->argv[c]),0);
      shiftIn(l+1);
      return;
    }
  }
  
  l=identifierEnd(0);
  if (l==0) l=1;
  sendout(C->buf,l,1);
  shiftIn(l);
}

void ProcessContext()
{
  struct INPUTCONTEXT *c;

  while (1) {
    while (!C->eof)
      ParseText();
    if (C->in!=NULL)
      fclose(C->in);
    free(C->malloced_buf);
    if (C->next==NULL) return;
    c=C->next;
    free(C);
    C=c;
  }
}

main(int argc,char **argv)
{
  initthings(argc,argv); 
  ProcessContext();
  fclose(C->out->f);
  return 0;
}

