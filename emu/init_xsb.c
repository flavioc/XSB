/* File:      init_xsb.c
** Author(s): Warren, Swift, Xu, Sagonas, Johnson, Rao
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


#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN_NT
#include <windows.h>
#include <direct.h>
#include <io.h>
#include <fcntl.h>
#include <process.h>
#else
#include <unistd.h>	
#include <stddef.h>
#include <sys/wait.h>
#endif

#include "auxlry.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "inst_xsb.h"
#include "psc_xsb.h"
#include "hash_xsb.h"
#include "heap_xsb.h"
#include "memory_xsb.h"
#include "register.h"
#include "tries.h"
#include "choice.h"
#include "flags_xsb.h"
#include "loader_xsb.h"
#include "macro_xsb.h"
#include "tr_utils.h"
#include "export.h"
#include "io_builtins_xsb.h"
#include "timer_defs_xsb.h"
#include "sig_xsb.h"

/*-----------------------------------------------------------------------*/

/* Sizes of the Data Regions in K-byte blocks
   ------------------------------------------ */
#ifdef BITS64
#define PDL_DEFAULT_SIZE         (64*2)
#define GLSTACK_DEFAULT_SIZE    (768*2)
#define TCPSTACK_DEFAULT_SIZE   (768*2)
#define COMPLSTACK_DEFAULT_SIZE  (64*2)
#else
#define PDL_DEFAULT_SIZE         64
#define GLSTACK_DEFAULT_SIZE    768
#define TCPSTACK_DEFAULT_SIZE   768
#define COMPLSTACK_DEFAULT_SIZE  64
#endif

#ifndef fileno				/* fileno may be a  macro */
extern int    fileno(FILE *f);	        /* this is defined in POSIX */
#endif
/* In WIN_NT, this gets redefined into _fdopen by wind2unix.h */
extern FILE *fdopen(int fildes, const char *type);


long pspacesize = 0;	/* actual space dynamically allocated by loader.c */

/* The SLG-WAM data regions
   ------------------------ */
System_Stack
pdl = {NULL, NULL, 0,
       PDL_DEFAULT_SIZE},             /* PDL                   */
  glstack = {NULL, NULL, 0,
	     GLSTACK_DEFAULT_SIZE},     /* Global + Local Stacks */
    tcpstack = {NULL, NULL, 0,
		TCPSTACK_DEFAULT_SIZE},   /* Trail + CP Stack      */
      complstack = {NULL, NULL, 0,
		    COMPLSTACK_DEFAULT_SIZE};   /* Completion Stack  */

Exec_Mode xsb_mode;     /* How XSB is run: interp, disassem, user spec, etc. */

DllExport extern char * call_conv strip_names_from_path(char*, int);

/* real_alloc uses malloc only to keep pspacesize straight. */
#define real_alloc(X) malloc(X) 

Cell answer_return_inst;
Cell resume_compl_suspension_inst;
Cell check_complete_inst;
Cell hash_handle_inst;
Cell fail_inst;
Cell halt_inst;
Cell proceed_inst;
Cell reset_inst;

extern double realtime_count;

extern void perproc_reset_stat(void), reset_stat_total(void); 

/* these three are from orient_xsb.c */
extern char *install_dir; 
extern char *xsb_config_file; /* configuration.P */
extern char *user_home; /* the user HOME dir or install dir, if HOME is null */

/*==========================================================================*/

static void display_file(char *infile_name)
{
  FILE *infile;
  char buffer[MAXBUFSIZE];

  if ((infile = fopen(infile_name, "r")) == NULL) {
    xsb_error("\nCan't open `%s'; XSB installation might be corrupted\n",
	      infile_name);
    exit(1);
  }

  while (fgets(buffer, MAXBUFSIZE-1, infile) != NULL)
    fprintf(stdmsg, "%s", buffer);

  fclose(infile);
}


static void version_message(void)
{
  char licensemsg[MAXPATHLEN], configmsg[MAXPATHLEN];

  sprintf(licensemsg, "%s%cetc%ccopying.msg", install_dir, SLASH, SLASH);
  sprintf(configmsg, "%s%cbanner.msg", 
	  strip_names_from_path(xsb_config_file, 2), SLASH);

  display_file(configmsg);
  fprintf(stdmsg, "\n");
  display_file(licensemsg);

  exit(0);
}

static void help_message(void)
{
  char helpmsg[MAXPATHLEN];

  sprintf(helpmsg, "%s%cetc%chelp.msg", install_dir, SLASH, SLASH);
  puts("");
  display_file(helpmsg);

  exit(0);
}


/*==========================================================================*/

/* Initialize System Flags
   ----------------------- */
static void init_flags(void)
{
  int i;

  for (i=0; i<64; i++) flags[i] = 0;
  flags[SYS_TIMER]  = TIMEOUT_ERR; /* start with expired timer */
  flags[BANNER_CTL] = 1;           /* a product of prime numbers; each prime
				      determines which banner isn't shown */
}

/*==========================================================================*/

static void init_open_files(void)
{
  int i, msg_fd, dbg_fd, warn_fd, fdbk_fd;

  open_files[0] = stdin;
  open_files[1] = stdout;
  open_files[2] = stderr;

  /* stream for xsb warning msgs */
  if ((warn_fd = dup(fileno(stderr))) < 0)
    xsb_exit("Can't open the standard stream for warnings\n");
  stdwarn = fdopen(warn_fd, "w");
  open_files[3] = stdwarn;

  /* stream for xsb normal msgs */
  if ((msg_fd = dup(fileno(stderr))) < 0)
     xsb_exit("Can't open the standard stream for messages\n");
  stdmsg = fdopen(msg_fd, "w");
  open_files[4] = stdmsg;

  /* stream for xsb debugging msgs */
  if ((dbg_fd = dup(fileno(stderr))) < 0)
     xsb_exit("Can't open the standard stream for debugging messages\n");
  stddbg = fdopen(dbg_fd, "w");
  open_files[5] = stddbg;

  /* stream for xsb debugging msgs */
  if ((fdbk_fd = dup(fileno(stdout))) < 0)
     xsb_exit("Can't open the standard stream for XSB feedback messages\n");
  stdfdbk = fdopen(fdbk_fd, "w");
  open_files[6] = stdfdbk;

  /* NT doesn't seem to think that dup should preserve the buffering mode of
     the original file. So we make all new descriptors unbuffered -- dunno if
     this is good or bad. Line-buffering _IOLBF is the coarsest that can be
     allowed. Without the buffering NT users won't see anything on the
     screen. -mk */
  /* We should use setvbuf, but -no-cygwin doesn't seem to do the
     right thing with it, but it does with setbuf.... go figure. -dsw */

  setbuf(stdmsg, NULL);
  setbuf(stdwarn, NULL);
  setbuf(stddbg, NULL);
  setbuf(stdfdbk, NULL);
  setbuf(stderr, NULL);

  for (i=MIN_USR_OPEN_FILE; i < MAX_OPEN_FILES; i++) open_files[i] = NULL;
}

/*==========================================================================*/

/* if command line option is long --optionname, then the arg here is
   'optionname'. Process it and return.
*/
static void process_long_option(char *option)
{
  if (0==strcmp(option, "nobanner")) {
    flags[BANNER_CTL] *= NOBANNER;
  } else if (0==strcmp(option, "quietload")) {
    flags[BANNER_CTL] *= QUIETLOAD;
  } else if (0==strcmp(option, "noprompt")) {
    flags[BANNER_CTL] *= NOPROMPT;
  } else if (0==strcmp(option, "help")) {
    help_message();
  } else if (0==strcmp(option, "version")) {
    version_message();
  }

  return;
}

/*==========================================================================*/
FILE *stream_err, *stream_out; 

/* Initialize System Parameters
   ---------------------------- */
char *init_para(int argc, char *argv[])
{
  int i;
  char warning[80];
  /* Boot module is usually the loader that loads the Prolog code of XSB.
  ** Or it can be a code to disassemble.
  ** Cmd loop driver is usually the XSB interpreter (x_interp.P).
  ** However, it can be any program that communicates with XSB and drives its
  ** comand loop.
  */
  char *boot_module, *cmd_loop_driver;
  char cmd_line_goal[MAXBUFSIZE+1] = "";
  int  strlen_instdir, strlen_initfile, strlen_2ndfile;
  void tstInitDataStructs();

  init_flags();
  /* this needs to appear here as streams are used below in xsb_warn() */
  for (i=1; i<argc; i++) { /* check to see if should redirect output */
    if (!strcmp(argv[i],"-q")) {
      stream_err = freopen("XSB_errlog", "w", stderr);
      stream_out = freopen("XSB_outlog", "w", stdout);
      break;
    }
  }
  init_open_files();

  init_newtrie();
  init_trie_aux_areas();
  tstInitDataStructs();

  /* init stat. structures */
  perproc_reset_stat();
  reset_stat_total();

  flags[STACK_REALLOC] = TRUE;
#ifdef GC
  flags[GARBAGE_COLLECT] = COPYING_GC;
#else
  flags[GARBAGE_COLLECT] = NO_GC;
#endif
  flags[DCG_MODE] = XSB_STYLE_DCG;

  /* Set default Prolog files.
     ------------------------- */
#ifdef WIN_NT
  boot_module = "\\syslib\\loader.O";
#else
  boot_module = "/syslib/loader.O";
#endif

  /* File extensions are automatically added for Loader-loaded files. */
#ifdef WIN_NT
  cmd_loop_driver = "\\syslib\\x_interp";
#else
  cmd_loop_driver = "/syslib/x_interp";
#endif


  xsb_mode = DEFAULT;
  flags[TABLING_METHOD] = VARIANT_TEM;


  /* Modify Parameters Using Command Line Options
     -------------------------------------------- */
  for (i=1; i<argc; i++) {
    if (*argv[i] != '-') {        /* command-line module specified */
      if (xsb_mode != DEFAULT)
	help_message();
      xsb_mode = CUSTOM_CMD_LOOP_DRIVER;
      cmd_loop_driver = argv[i];
      continue;
    }

    /* Otherwise, get command-line switch (and arg).
       Will dump core if the accompanying argument is omitted. */
    switch((argv[i][1])) {
    case 'r':
      flags[STACK_REALLOC] = FALSE;
      break;
    case 'g':
      i++;
#ifdef GC
      if (i < argc) {
	if (strcmp(argv[i],"sliding")==0)
	  flags[GARBAGE_COLLECT] = SLIDING_GC;
	else
	if (strcmp(argv[i],"copying")==0)
	  flags[GARBAGE_COLLECT] = COPYING_GC;
	else
        if (strcmp(argv[i],"indirection")==0)
          flags[GARBAGE_COLLECT] = INDIRECTION_SLIDE_GC;
        else
	if (strcmp(argv[i],"none")==0)
	  flags[GARBAGE_COLLECT] = NO_GC;
	else
	xsb_warn("Unrecognized garbage collection type");
      } else
        xsb_warn("Missing garbage collection type");
#else
      xsb_warn("-g option does not make sense in this XSB configuration");
#endif
      break;
    case 'u':
      if (argv[i][2] != '\0')
	sscanf(argv[i]+2, "%ld", &pdl.init_size);
      else {
	i++;
	if (i < argc)
	  sscanf(argv[i], "%ld", &pdl.init_size);
	else
	  xsb_warn("Missing size value");
      }
      break;
    case 'm':
      if (argv[i][2] != '\0')
	sscanf(argv[i]+2, "%ld", &glstack.init_size);
      else {
	i++;
	if (i < argc)
	  sscanf(argv[i], "%ld", &glstack.init_size);
	else
	  xsb_warn("Missing size value");
      }
      break;
    case 'c':
      if (argv[i][2] != '\0')
	sscanf(argv[i]+2, "%ld", &tcpstack.init_size);
      else {
	i++;
	if (i < argc)
	  sscanf(argv[i], "%ld", &tcpstack.init_size);
	else
	  xsb_warn("Missing size value");
      }
      break;
    case 'o':
      if (argv[i][2] != '\0')
	sscanf(argv[i]+2, "%ld", &complstack.init_size);
      else {
	i++;
	if (i < argc)
	  sscanf(argv[i], "%ld", &complstack.init_size);
	else
	  xsb_warn("Missing size value");
      }
      break;
    case 's':
      flags[TRACE_STA] = 1;
      asynint_val |= MSGINT_MARK;
      break;
    case 'S':
      flags[TABLING_METHOD] = SUBSUMPTIVE_TEM;
      break;
    case 'd':
      if ( (xsb_mode != DEFAULT) && (xsb_mode != CUSTOM_BOOT_MODULE) )
	help_message();
      xsb_mode = DISASSEMBLE;
      break;
    case 'T': 
      flags[HITRACE] = 1;
      asynint_val |= MSGINT_MARK; 
      break;
    case 't': 
#ifdef DEBUG
      flags[PIL_TRACE] = 1;
      flags[HITRACE] = 1;
      asynint_val |= MSGINT_MARK;
#else
      xsb_exit("-t option unavailable for this executable (non-debug mode)");
#endif
      break;
    case 'i':
      if (xsb_mode != DEFAULT)
	help_message();
      xsb_mode = INTERPRETER;
      break;
    case 'l':
      flags[LETTER_VARS] = 1;
      break;
    case 'n':
      if (xsb_mode != DEFAULT)
	help_message();
      xsb_mode = C_CALLING_XSB;
#ifdef WIN_NT
      cmd_loop_driver = "\\syslib\\xcallxsb";
#else
      cmd_loop_driver = "/syslib/xcallxsb";
#endif
      break;
    case 'B':
      if (xsb_mode == DEFAULT)
	xsb_mode = CUSTOM_BOOT_MODULE;
      else if (xsb_mode != DISASSEMBLE)   /* retain disassemble command for */
	help_message();                /* -d -f <file> AWA -f <file> -d */
      if (argv[i][2] != '\0')
	boot_module = argv[i]+2;
      else {
	i++;
	if (i < argc)
	   boot_module = argv[i];
	 else
	   xsb_warn("Missing boot module's file name");
      }
      break;
    case 'D':
      if (xsb_mode == DEFAULT)
	xsb_mode = CUSTOM_CMD_LOOP_DRIVER;
      else if (xsb_mode != CUSTOM_BOOT_MODULE)
	help_message();
      if (argv[i][2] != '\0')
	cmd_loop_driver = argv[i]+2;
      else {
	i++;
	if (i < argc)
	   cmd_loop_driver = argv[i];
	 else
	   xsb_warn("Missing top-level command loop driver's file name");
      }
      break;
    case 'e': {
      char *tmp_goal=NULL;
      if (argv[i][2] != '\0')
	tmp_goal = argv[i]+2;
      else {
	i++;
	if (i < argc)
	   tmp_goal = argv[i];
	 else
	   xsb_warn("Missing command line goal");
      }

      if (strchr(tmp_goal, '.') == NULL) {
	xsb_exit("\n\nTerminating `.' missing in command line goal:\n\t`%s'",
		 tmp_goal);
      }

      if ((strlen(cmd_line_goal) + strlen(tmp_goal)) >= MAXBUFSIZE)
	xsb_exit("\n\nCommand line goal is too long (> %d)\n\n", MAXBUFSIZE);
      strcat(cmd_line_goal, " ");
      strcat(cmd_line_goal, tmp_goal);
      break;
    }
    case 'h':
      help_message();
      break;
    case 'v':
      version_message();
      break;
    case '-': /* this was a long option of the form --optionname */
      process_long_option(argv[i]+2);
      break;
    case 'q':
      break;
    default:
      sprintf(warning, "Unknown command line option %s", argv[i]);
      xsb_warn(warning);
    } /* switch */
  } /* for */
  /* Done with command line arguments */

  /* This is where we will be looking for the .xsb directory */
  flags[USER_HOME] = (Cell) malloc(strlen(user_home) + 1);
  strcpy( (char *)flags[USER_HOME], user_home );

  /* install_dir is computed dynamically at system startup (in main_xsb.c).
     Therefore, the entire directory tree can be moved --- only the relative
     positions count.
  */ 
  flags[INSTALL_DIR] = (Cell) malloc(strlen(install_dir) + 1);   
  strcpy( (char *)flags[INSTALL_DIR], install_dir );

  /* loader uses CONFIG_NAME flag before xsb_configuration is loaded */
  flags[CONFIG_NAME] = (Cell) malloc(strlen(CONFIGURATION) + 1);
  strcpy( (char *)flags[CONFIG_NAME], CONFIGURATION );

  flags[CONFIG_FILE] = (Cell) malloc(strlen(xsb_config_file) + 1);
  strcpy( (char *)flags[CONFIG_FILE], xsb_config_file );

  /* the default for cmd_line_goal goal is "" */
  flags[CMD_LINE_GOAL] = (Cell) malloc(strlen(cmd_line_goal) + 1);
  strcpy( (char *)flags[CMD_LINE_GOAL], cmd_line_goal );
  

  /* Set the Prolog startup files.
     ----------------------------- */
  /* Default execution mode is to load and run the interpreter. */
  if (xsb_mode == DEFAULT)
    xsb_mode = INTERPRETER;

  strlen_instdir = strlen(install_dir);
  strlen_initfile = strlen(boot_module);
  strlen_2ndfile = strlen(cmd_loop_driver);

  switch(xsb_mode) {
  case INTERPRETER:
  case C_CALLING_XSB:
    /*
     *  A "short-cut" option in which the loader is the loader file and
     *  an XSB-supplied "server" program is the interpreter file.  Since
     *  it is known where these files exist, the full paths are built.
     */
    flags[BOOT_MODULE] = (Cell) malloc(strlen_instdir + strlen_initfile + 1);
    flags[CMD_LOOP_DRIVER] = (Cell)malloc(strlen_instdir + strlen_2ndfile + 1);
    sprintf( (char *)flags[BOOT_MODULE], "%s%s", install_dir,
	     boot_module );
    sprintf( (char *)flags[CMD_LOOP_DRIVER], "%s%s", install_dir,
	     cmd_loop_driver );
    break;
  case CUSTOM_BOOT_MODULE:
    /*
     *  The user has specified a private loader to be used instead of the
     *  standard one and possibly a top-level command loop driver as well.  In
     *  either case, we can 
     *  make no assumptions as to where these files exist, and so the 
     *  user must supply an adequate filename in each case.
     */
    flags[BOOT_MODULE] = (Cell) malloc(strlen_initfile + 1);
    flags[CMD_LOOP_DRIVER ] = (Cell) malloc(strlen_2ndfile + 1);
    strcpy( (char *)flags[BOOT_MODULE], boot_module );
    strcpy( (char *)flags[CMD_LOOP_DRIVER], cmd_loop_driver );
    break;
  case CUSTOM_CMD_LOOP_DRIVER:
    /*
     *  The user has specified a private top-level command loop.
     *  The filename can be absolute; however if not, it will
     *  be looked for in XSB's library path.
     */
    flags[BOOT_MODULE] = (Cell) malloc(strlen_instdir + strlen_initfile + 1);
    flags[CMD_LOOP_DRIVER ] = (Cell) malloc(strlen_2ndfile + 1);
    sprintf( (char *)flags[BOOT_MODULE], "%s%s", install_dir,
	     boot_module );
    strcpy( (char *)flags[CMD_LOOP_DRIVER ], cmd_loop_driver );
    break;
  case DISASSEMBLE:
    /*
     *  A loader file should have been specified for disassembling.
     */
    flags[BOOT_MODULE] = (Cell) malloc(strlen_initfile + 1);
    strcpy( (char *)flags[BOOT_MODULE], boot_module );
    break;
  default:
    xsb_exit("Setting startup files: Bad XSB mode!");
    break;
  }

  return ( (char *) flags[BOOT_MODULE] );
}

/*==========================================================================*/

/* Initialize Memory Regions and Related Variables
   ----------------------------------------------- */
void init_machine(void)
{
  /* set special SLG_WAM instruction addresses */
  cell_opcode(&answer_return_inst) = answer_return;
  cell_opcode(&resume_compl_suspension_inst) = resume_compl_suspension;
  cell_opcode(&check_complete_inst) = check_complete;
  cell_opcode(&hash_handle_inst) = hash_handle;
  cell_opcode(&fail_inst) = fail;
  cell_opcode(&halt_inst) = halt;
  cell_opcode(&proceed_inst) = proceed;         /* returned by load_obj */
  cell_opcode(&reset_inst) = reset;
				/* instruction to initialize abort handler */

  /* Allocate Stack Spaces and set Boundary Parameters
     ------------------------------------------------- */
  pdl.low = (byte *)real_alloc(pdl.init_size * K);
  if (!pdl.low)
    xsb_exit("Not enough core for the PDL Stack!");
  pdl.high = pdl.low + pdl.init_size * K;
  pdl.size = pdl.init_size;

  glstack.low = (byte *)real_alloc(glstack.init_size * K);
  if (!glstack.low)
    xsb_exit("Not enough core for the Global and Local Stacks!");
  glstack.high = glstack.low + glstack.init_size * K;
  glstack.size = glstack.init_size;

  tcpstack.low = (byte *)real_alloc(tcpstack.init_size * K);
  if (!tcpstack.low)
    xsb_exit("Not enough core for the Trail and Choice Point Stack!");
  tcpstack.high = tcpstack.low + tcpstack.init_size * K;
  tcpstack.size = tcpstack.init_size;

  complstack.low = (byte *)real_alloc(complstack.init_size * K);
  if (!complstack.low)
    xsb_exit("Not enough core for the Completion Stack!");
  complstack.high = complstack.low + complstack.init_size * K;
  complstack.size = complstack.init_size;

  /* -------------------------------------------------------------------
     So, the layout of the memory looks as follows:

     pdl.low
     /\
     pdlreg   |
     pdl.high
     ===================
     glstack.low
     hreg   |
     \/
     /\
     ereg   |
     glstack.high
     ===================
     tcpstack.low
     trreg  |
     \/
     /\
     breg   |
     tcpstack.high
     ===================
     complstack.low

     /\
     openreg  |
     complstack.high
     --------------------------------------------------------------------- */

  /* Initialize Registers
     -------------------- */
  cpreg = (pb) &halt_inst;		/* halt on final success */

  pdlreg = (CPtr)(pdl.high) - 1;

/*   interrupt_reg = (CPtr)(glstack.low); */
  bld_int(interrupt_reg, 0);

  hbreg = hreg = (CPtr)(glstack.low);

  ebreg = ereg = (CPtr)(glstack.high) - 1;

  *(ereg-1) = (Cell) cpreg;

  trreg	= (CPtr *)(tcpstack.low);
  *(trreg) = (CPtr) trreg;

  reset_freeze_registers;
  openreg = ((CPtr) complstack.high);
  delayreg = NULL;

  /* Place a base choice point frame on the CP Stack: this choice point
     is needed for cut -- make sure you initialize all its fields.
     ------------------------------------------------------------------ */
  breg = (CPtr)(tcpstack.high) - CP_SIZE;
  cp_pcreg(breg) = (pb) &halt_inst; 	  /* halt on last failure */
  cp_ebreg(breg) = ebreg;
  cp_hreg(breg) = hreg;
  cp_trreg(breg) = trreg;
  cp_ereg(breg) = ereg;
  cp_prevbreg(breg) = breg;               /* note ! */
  cp_pdreg(breg) = delayreg;


  /* Other basic initializations
     --------------------------- */
  realtime_count = real_time();
  inst_begin = 0;

  symbol_table.table = (void **)calloc(symbol_table.size, sizeof(Pair));
  string_table.table = (void **)calloc(string_table.size, sizeof(char *));
}

/*==========================================================================*/

/* Initialize Standard PSC Records
   ------------------------------- */
void init_symbols(void)
{
  Psc  tables_psc;
  Pair temp, tp;
  int  i, new_indicator;

  /* insert mod name global */
  tp = insert_module(T_MODU, "global");	/* loaded */
  set_data(pair_psc(tp), (Psc)USERMOD_PSC);	/* initialize global mod PSC */
  global_mod = pair_psc(tp);

  /* insert "[]"/0 into String Table */
  nil_sym = string_find("[]", 1);

  /* insert "."/2 into global list */
  temp = insert(".", 2, global_mod, &new_indicator);
  list_str = temp;
  list_psc = pair_psc(temp);
  list_dot = get_name(list_psc);

  /* insert symbol ","/2 */
  temp = insert(",", 2, global_mod, &new_indicator);
  comma_psc = pair_psc(temp);

  /* insert symbol tnot/1 into module tables */
  tp = insert_module(0, "tables");		/* unloaded */
  tables_psc = pair_psc(tp);
  temp = insert("tnot", 1, tables_psc, &new_indicator);
  tnot_psc = pair_psc(temp);
  set_data(tnot_psc, tables_psc);
  set_env(tnot_psc, T_UNLOADED);
  set_type(tnot_psc, T_ORDI);
  temp = insert("DL", 3, global_mod, &new_indicator);
  delay_psc = pair_psc(temp);

  /*
   * Initialize ret PSCs.  Notice that ret_psc[0] is set to a pointer
   * to STRING "ret".
   */
  ret_psc[0] = (Psc) string_find("ret", 1);
  for (i = 1; i < MAX_ARITY; i++) ret_psc[i] = NULL;

  /* make another reference to global module -- "usermod" */
  tp = insert_module(T_MODU, "usermod");	/* loaded */
  set_data(pair_psc(tp), get_data(global_mod));
}

/*==========================================================================*/
