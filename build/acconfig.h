/* acconfig.h
   This file is in the public domain.

   Descriptive text for the C preprocessor macros that are
   needed for XSB.

   Crafted out of the old config.h by  kifer


   Leave the following blank line there!!  Autoheader needs it.  */


/* #ifdef ___ALWAYS_TRUE___ is a hack to force autoheader copy the relevant
   statements into config.h */
#undef ___ALWAYS_TRUE___

#undef INSTALL_PREFIX

#undef CC

#undef RELEASE_DATE
#undef RELEASE_MONTH
#undef RELEASE_DAY
#undef RELEASE_YEAR

/* sometimes needed for C preprocessor under Linux */
#undef _GNU_SOURCE

/* sometimes needed for C preprocessor under HP-UX */
#undef _HPUX_SOURCE

/* Defined, if AMIGA */
#undef AMIGA

/* Defined if 64 bit machine */
#undef BITS64

/* this is used by many to check if config.h was included in proper order */
#ifndef CONFIG_INCLUDED
#define CONFIG_INCLUDED
#endif

/* Defined on DEC Alpha's */
#undef DECALPHA

/* Defined on DEC stations */
#undef DECstation

/* DOS gcc compiler? */
#undef DOS_GCC
#undef DJGPP

/* HP300 running HP-UX */
#undef HP300

/* HP700 running HP-UX */
#undef HP700

/* Actually, AIX unix of IBM */
#undef IBM

/* Defined under Linux */
#undef LINUX

/* Older Linux using a.out format */
#undef LINUX_AOUT

/* Newer Linux using ELF format */
#undef LINUX_ELF

/* Use local eval strategy. Default is `batched' */
#undef LOCAL_EVAL

/* MIPS based machines, such as SGI, DECstation, SONY-NEWS */
#undef MIPS_BASED

/* MK Linux on Power PC */
#undef MKLINUX_PPC

/* Next Step */
#undef NeXT

/* Defined, if XSB is built with support for ORACLE DB */
#undef ORACLE
#undef ORACLE_DEBUG

/* Defined on Sequent machines */
#undef SEQUENT

/* Defined on SGI machines */
#undef SGI
#undef SGI64

/* Use delay strategy */
#undef SLG_WITH_DELAY

/* Sun Solaris OS */
#undef SOLARIS
#undef BIG_MEM

/* Sun Solaris on Intel */
#undef SOLARIS_x86

/* Sony mips-based machines */
#undef SONY_NEWS

/* Old, pre-solaris SunOS */
#undef SUN

/* Version number of XSB beta. Will look like b25.
   Full version would look like 1.9-b25 */
#undef XSB_BETA_VERSION

/* The code name for XSB release */
#undef XSB_CODENAME

/* The major version number of the XSB release */
#undef XSB_MAJOR_VERSION

/* The minor version number of the XSB release */
#undef XSB_MINOR_VERSION

/* The version number of the XSB release */
#undef XSB_VERSION

/* XSB is built with support for ODBC */
#undef XSB_ODBC

/* DOS gcc stuff */
#ifdef DOS_GCC
#ifndef DJGPP
#define DJGPP
#endif
#endif


#undef HAVE_SNPRINTF

/* XSB's FOREIGN_ELF is designed in away such that it supports
   any system using the high level dlopen... functions, even if
   the actual object file format is COFF. That's the case on
   DECALPHA, running OSF1 v3.x
 */

#undef FOREIGN_ELF
#undef FOREIGN_OUT

#ifdef DJGPP
#define FOREIGN_COFF
#endif

#ifdef ___ALWAYS_TRUE___
#if (defined(FOREIGN_AOUT) || defined(FOREIGN_ELF) || defined(FOREIGN_COFF))
#define FOREIGN
#endif
#endif

#ifdef MKLINUX_PPC
#undef FOREIGN_ELF
#undef FOREIGN
#endif


#ifdef ___ALWAYS_TRUE___
#if (defined(SOLARIS) || defined(WIN_NT))
#define bcopy(A,B,L) memmove(B,A,L)
#endif
#endif

/* Debugging variables are set in debug.h, not here.
   We include these dummy definitions only to avoid the autoheader warnings */
#if 0
#define DEBUG
#define PROFILE
#define DEBUG_ORACLE
#endif


/*  File needed for XSB to get the configuration information */
#undef CONFIGURATION
#undef FULL_CONFIG_NAME

/* Whether profile mode, normal mode, optimal, debug mode */
#undef COMPILE_MODE




/* Leave that blank line there!!  Autoheader needs it.
   If you're adding to this file, keep in mind:
   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  */
