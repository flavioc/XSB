/* File:      xpathname.c -- utilities to manipulate path/file names
** Author(s): kifer
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1998
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

#include "configs/config.h"
#include "debugs/debug.h"

#ifdef WIN_NT
#include <direct.h>
#include <io.h>
#else
#include <unistd.h>
#endif

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <setjmp.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
/* special.h must be included after sys/stat.h */
#include "configs/special.h"

#ifndef NeXT
#include <malloc.h>
#endif

#if (!defined(WIN_NT))
#include <pwd.h>
#endif

#include "auxlry.h"
#include "psc.h"
#include "cinterf.h"
#include "cell.h"
#include "xsberror.h"


#define DDOT	".."
#define DOT	"."

extern char *user_home;  	  /* from xmain.c: the user $HOME dir or
				     install dir, if $HOME is null */ 
bool is_absolute_filename(char *);
char *tilde_expand_filename(char *);
char *tilde_expand_filename_norectify(char *, char *);
static char *rectify_pathname(char *, char *);


/* if file is a directory and is missing a trailing slash, add it 
   passes file name and the slash to be added. can be backslash on NT
   Also, strips off excessive trailing slashes. 
*/
char *dirname_canonic(char *filename) {
  static char canonicized[MAXPATHLEN];
  int retcode, len = strlen(filename);
  struct stat fileinfo;

  rectify_pathname(filename, canonicized);
  retcode = stat(canonicized, &fileinfo);

  /* if directory, add trailing slash */
  if ((retcode==0) && S_ISDIR(fileinfo.st_mode)
      && (canonicized[len-1] != SLASH)) {
    canonicized[len] = SLASH;
    canonicized[len+1] = '\0';
  }
  return canonicized;
}


/*
 *  Return full path name for the file passed in as argument.
 */

char *expand_filename(char *filename) {
  char aux_filename[MAXPATHLEN], aux_filename2[MAXPATHLEN];
  static char absolute_filename[MAXPATHLEN]; /* abs filename composed here */
  
#if defined(WIN_NT) || defined(DJGPP)

  if ( is_absolute_filename(filename) ) {
    return rectify_pathname(filename, absolute_filename);
  } else {
    getcwd(aux_filename2, MAXPATHLEN-1);
    sprintf(aux_filename, "%s%c%s", aux_filename2, SLASH, filename);
    return rectify_pathname(aux_filename, absolute_filename);
  }

#else /* For UNIX systems */

  if (is_absolute_filename(filename))
    /* rectify and put in string tbl */
    return rectify_pathname(filename, absolute_filename);

  /* The file name is absolute, but the initial `~' must be expanded. */
  else if (filename[0] == '~') 
    return tilde_expand_filename(filename);

  else {     /*  The file name is not absolute. */
    getcwd(aux_filename2, MAXPATHLEN-1);
    sprintf(aux_filename, "%s%c%s", aux_filename2, SLASH, filename);
    return rectify_pathname(aux_filename, absolute_filename);
  }
#endif /* of def for Unix */
}

/*
** Like expand_filename, but ONLY expands Unix tilde by replacing '~', '~user'
** with the home directory of the appropriate user.
** Does nothing on NT and DOS.
*/
char *tilde_expand_filename(char *filename) {
  char aux_filename[MAXPATHLEN];
  static char absolute_filename[MAXPATHLEN]; /* abs filename composed here */

  tilde_expand_filename_norectify(filename, aux_filename);
  return rectify_pathname(aux_filename, absolute_filename);
}

/* Like tilde_expand_filename, but doesn't rectify */
char *tilde_expand_filename_norectify(char *filename, char *expanded) {
  
#if defined(WIN_NT) || defined(DJGPP)
  strcpy(expanded, filename);
  return expanded;

#else /* Unix */
  char *path_prefix;        /* ptr to a (sub)string containing what will
			       become the prefix for the absolute filename. */
  char *path_suffix;        /* ptr to a (sub)string containing what will
			       become the suffix for the absolute filename. */
  static char username[MAXNAME]; /* the username if filename has ~<name> */
  int username_len;
  struct passwd *pw_struct;     /* receives passwd structure from getpwnum() */

  if (filename[0] != '~') {
    strcpy(expanded, filename);
    return expanded;
  } 
  if (filename[1] == '/' || filename[1] == '\0') {
    /*  The file name begins with "~/" or is simply "~" -- so replace
	'~' with the user's home directory. */
    path_prefix = user_home;
    path_suffix = filename + 1;
  } else {
    /*  The file name begins with ~<username>.  Use a system call to
	determine this directory's path. */
    path_prefix = path_suffix = filename + 1;
    while ( (*path_suffix != '\0') && (*path_suffix != '/') )
      path_suffix++;
    username_len = path_suffix - path_prefix;
    memmove(username, path_prefix, username_len);
    username[username_len] = '\0';
    
    pw_struct = (struct passwd *) getpwnam(username);
    if (!pw_struct) {
      /*  The system has no info on this user, so we can't
	  construct the absolute path -- abort. */
      char message[100];
      sprintf(message, "`%s': unknown user\n", username);
      xsb_abort(message);
    } else
      path_prefix = pw_struct -> pw_dir;
  }
    
  sprintf(expanded, "%s%c%s", path_prefix, SLASH, path_suffix);
  return expanded;
#endif /* Unix */
}


/* check if the path name is absolute */
bool is_absolute_filename(char *filename) {

#if defined(WIN_NT) || defined(DJGPP)
  /*  If the file name begins with a "\" or with an "X:", where X is some
   *  character, then the file name is absolute.
   *  Otherwise it's not. */
  if ( (filename[0] == SLASH) || (isalpha(filename[0]) && filename[1] == ':') )
    return TRUE;
#else /* Unix */
  if (filename[0] == '/')
    return TRUE;
#endif

  return FALSE;
}
  

/* strip names from the back of path 
   PATH is the path name from which to strip.
   HOW_MANY is the number of names to strip.
   E.g., strip_names_from_path("a/b/c/d", 2) returns "a/b" 

   This function is smart about . and ..: 
       	   strip_names_from_path("a/b/c/./d", 2)
   is still "a/b" and
           strip_names_from_path("a/b/c/../d", 2)
   is "a".
   If we ask to strip too many names from path, it'll abort.

   This function copies the result into a large buffer, so we can add more
   stuff to it. These buffers stay forever, but we call this func only a couple
   of times, so it's ok. */
char *strip_names_from_path(char* path, int how_many)
{
  int i, abort_flag=FALSE;
  char *cutoff_ptr;
  char *buffer = (char *) malloc(MAXPATHLEN);

  rectify_pathname(path,buffer);

  for (i=0; i < how_many; i++) {
    if (abort_flag) {
      char message[200];
      sprintf(message,
	      "There is no directory %d levels below %s\n",
	      how_many, path);
      xsb_abort(message);
    }
    cutoff_ptr = strrchr(buffer, SLASH);
    if (cutoff_ptr == NULL)
      return "";
    if ((cutoff_ptr - buffer) > 0)
      /* we have more than just a slash between the beginning of buffer and
	 cutoff_ptr: replace slash with end of string */
      *cutoff_ptr = '\0';
    else {       /* we are at the top of the file hierarchy */
      *(cutoff_ptr+1) = '\0';
      abort_flag=TRUE;
    }
  }
  return buffer;
}


/* Get the base name of PATH, e.g., basename of a/b/c is c if path is a/b/c/
   then basename is "". This op preserves extension, i.e., basename of a/b.c 
   is b.c */
char *get_file_basename(char *path) {
  char *ptr;
  ptr = strrchr(path, SLASH);
  if (ptr == NULL)
    return path;
  else
    return ptr+1;
}

/* get directory part of PATH. It first rectifies PATH, then returns result.
   E.g., a/b/.././c --> a/ (even if c is a directory itself)
   a/b/ --> a/b/ 
   Doesn't expand the directory name.
   Always leaves trailing slash at the end. 

   Expects a string storage as 2nd arg, returns second arg.
*/
char *get_file_dirname(char *path, char *dir) {
  char *ptr;
  ptr = strrchr(rectify_pathname(path,dir), SLASH);
  if (ptr == NULL)
    /* No slash in filename, return empty string */
    return "";
  /* the whole thing might be just "/". In this case, it is the dirname of the
     file */
  else if (*ptr==SLASH && *(ptr+1)=='\0')
    return dir;
  else {
    *(ptr+1) = '\0';
    return dir;
  }
}

/* Get file extension, i.e., "c" in abc.c, etc. If the file name is of the form
   ".abc", where none of a,b,c is a '.', then the extension is empty string. */

char *get_file_extension(char *path) {
  char *ptr, *base=get_file_basename(path);
  ptr = strrchr(base, '.');
  if ((ptr==base) || (ptr==NULL))
    return "";
  else return (ptr+1);
}

#define MAXPATHNAMES 256 /* max number of file names in a path name */

/* 
** Go over path name and get rid of `..', `.', and multiple slashes 
** Won't delete leading `..'.
** Expects two strings (with allocated storage) as params: the input path and
** the output path. Returns the second argument.
*/
static char *rectify_pathname(char *inpath, char *outpath) {
  char names[MAXPATHNAMES][MAXNAME];  /* array of filenames in inpath.
					 1st index: enumerates names in inpath;
					 2nd index: scan file names */
  static char expanded_inpath[MAXPATHLEN];
  char *inptr1, *inptr2, *inpath_end;
  int length; /* length=inptr2-inptr1 */
  int i, outidx=0, nameidx=0; /* nameidx: 1st index to names */
  bool leading_slash, trailing_slash;

  tilde_expand_filename_norectify(inpath, expanded_inpath);
  
  /* initialization */
  inptr1 = inptr2 = expanded_inpath;
  inpath_end = expanded_inpath + strlen(expanded_inpath);

  /* check if expanded inpath has trailing/leading slash */
  leading_slash = (*expanded_inpath == SLASH ? TRUE : FALSE);
  trailing_slash = (*(inpath_end - 1) == SLASH ? TRUE : FALSE);

  while ( inptr2 < inpath_end ) {
    inptr2 = strchr(inptr1, SLASH);
    if (inptr2==NULL)
      inptr2 = inpath_end;

    /* skip slashes */
    if ((length = inptr2 - inptr1) == 0) {
      if (inptr2 == inpath_end)
	break; /* out of the while loop */
      else {
	inptr2++;
	inptr1++;
	continue;
      }
    }

    switch (length) {
    case 1:
      if (*inptr1 == '.') {
	inptr1 = inptr2;
	continue; /* the loop */
      }
      break; /* we found a file name, will process it */
    case 2: 
      if ((*inptr1 == '.') && (*(inptr1+1) == '.')) {
	nameidx--; /* drop the previous file name from the names array */
	if (nameidx < 0) {
	  /* These are leading ..'s -- leave them */
	  nameidx++;
	  break;
	} else {
	  /* Discard .. and the previous file name */
	  inptr1 = inptr2;
	  continue;
	}
      }
      break;
    } /* done processing '.' and '..' */
    
    /* copy the filename just found to the right slot in the names array */
    strncpy(names[nameidx], inptr1, length);
    names[nameidx][length] = '\0'; /* make a string out of the file name */
    nameidx++;
    inptr1=inptr2;
    if (nameidx >= MAXPATHNAMES)
      xsb_abort("Number of file names in the full path name exceeds maximum");
  }

  /* at this point, we've copied all file names into names array and eliminated
     . and .. (in case of .. we also got rid of the preceding file name).
     So, we are ready to construct  the outpath. */

  if (leading_slash) {
    outpath[outidx] = SLASH;
    outidx++;
  }

  for (i=0; i<nameidx; i++) {
    strcpy(outpath+outidx, names[i]);
    outidx = outidx + strlen(names[i]);
    /* put shash in place of '\0', if we are not at the end yet */
    if (i < nameidx-1) {
      outpath[outidx] = SLASH;
      outidx++;
    }
  }

  /* don't add trailing slash if the file name is "/" */
  if (trailing_slash && (nameidx > 0)) {
    outpath[outidx] = SLASH;
    outpath[outidx+1] = '\0';
  }
  return(outpath);
}

/* Takes filename, gets back dir, base (sans the extension), and extension. 
   The dirname isn't expanded, even for tildas, but it is rectified.
   +++NOTE: this procedure modifies the input file name by inserting '\0' in
   place of the dot that separates the extension from the base name.
   This is ok, since this function is used only as a built in, so the input
   file name is discarded anyway. 
 */
void parse_filename(char *filename, char **dir, char **base, char **extension)
{
  static char absolute_dirname[MAXPATHLEN]; /* abs dirname composed here */
  static char basename[MAXNAME];    	    /* the rest of the filename  */

  *base = strcpy(basename, get_file_basename(filename));
  *dir = get_file_dirname(filename, absolute_dirname);
  *extension = get_file_extension(basename);
  /* cut off the extension from the base */
  if (*extension > *base)
    *(*extension-1) = '\0'; 
}


