## File:      packages/flora2/Makefile
##
## Author(s): Michael Kifer
##            Guizhen Yang
##
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 1999-2001
## 
## XSB is free software; you can redistribute it and/or modify it under the
## terms of the GNU Library General Public License as published by the Free
## Software Foundation; either version 2 of the License, or (at your option)
## any later version.
## 
## XSB is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
## more details.
## 
## You should have received a copy of the GNU Library General Public License
## along with XSB; if not, write to the Free Software Foundation,
## Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##
## $Id$
## 
##


XSBDIR = ../..

ALLOBJS = flroperator.O \
	  flrnodefp.O \
	  flrnowsp.O \
	  flrarguments.O \
	  flrprolog.O \
	  flrfirstorder.O \
	  flrprint.O \
	  flrlibman.O \
	  flrlexer.O \
	  flrcomposer.O \
          flrparser.O \
	  flrcompiler.O \
	  flrcoder.O \
	  flrutils.O \
	  flrwrapper.O \
	  flrwraparound.O \
	  flrundefhook.O \
	  flrshell.O

EMU_INCLUDES = $(XSBDIR)/emu/builtin.h \
	       $(XSBDIR)/emu/flag_defs_xsb.h \
	       $(XSBDIR)/emu/io_defs_xsb.h \
               $(XSBDIR)/emu/syscall_xsb.h \
	       $(XSBDIR)/emu/system_defs_xsb.h \
               $(XSBDIR)/emu/socket_xsb.h 

XSB_CHAR_DEFS = $(XSBDIR)/prolog_includes/char_defs.h
XSB_STANDARD = $(XSBDIR)/prolog_includes/standard.h
XSB_EMU_FLAG_DEFS = $(XSBDIR)/emu/flag_defs_xsb.h

FLORA_INCLUDES_DIR = flrincludes

FLORA_TERMS = $(FLORA_INCLUDES_DIR)/flora_terms.flh
FLORA_ERRORS = $(FLORA_INCLUDES_DIR)/flora_errors.flh

OPTION = [optimize]

XSB = $(XSBDIR)/bin/xsb

PROLOG = $(XSB) -s -m 2000 -c 2000 -e 'segfault_handler(warn).'

# Make sure warnings are saved in a file.
SVFILE = '$(XSBDIR)/build/.flora_top_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling XSB/packages/flora2:"


all: init $(ALLOBJS) finish
	./CompileChangedFiles.sh $(PROLOG)
	@cat $(SVFILE)
	cd closure; $(MAKE)
	cd genincludes; $(MAKE)
	cd syslib; $(MAKE)
	cd trailer; $(MAKE)
	cd lib; $(MAKE)
	cd debugger; $(MAKE)
	cd pkgs; $(MAKE)
	cd demos; $(MAKE)
	@cat $(XSBDIR)/build/.flora*.tmp


flroperator.O : flroperator.P flroperator.H $(FLORA_TERMS)
	@echo 'mc(flroperator,$(OPTION)).' >> cmd...


flrnodefp.O : flrnodefp.P flrnodefp.H $(FLORA_TERMS)
	@echo 'mc(flrnodefp,$(OPTION)).' >> cmd...


flrnowsp.O : flrnowsp.P flrnowsp.H $(FLORA_TERMS)
	@echo 'mc(flrnowsp,$(OPTION)).' >> cmd...


flrarguments.O : flrarguments.P flrarguments.H $(FLORA_TERMS)
	@echo 'mc(flrarguments,$(OPTION)).' >> cmd...


flrprolog.O : flrprolog.P flrprolog.H $(FLORA_TERMS)
	@echo 'mc(flrprolog,$(OPTION)).' >> cmd...


flrfirstorder.O : flrfirstorder.P flrfirstorder.H $(FLORA_TERMS)
	@echo 'mc(flrfirstorder,$(OPTION)).' >> cmd...


flrprint.O : flrprint.P flrprint.H $(XSB_STANDARD)
	@echo 'mc(flrprint,$(OPTION)).' >> cmd...


flrlibman.O : flrlibman.P flrlibman.H $(FLORA_TERMS)
	@echo 'mc(flrlibman,$(OPTION)).' >> cmd...


flrlexer.O : flrlexer.P flrlexer.H $(XSB_CHAR_DEFS) $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrlexer,$(OPTION)).' >> cmd...


flrcomposer.O : flrcomposer.P flrcomposer.H $(XSB_CHAR_DEFS) $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrcomposer,$(OPTION)).' >> cmd...


flrparser.O : flrparser.P flrparser.H $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrparser,$(OPTION)).' >> cmd...


flrcompiler.O : flrcompiler.P flrcompiler.H $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrcompiler,$(OPTION)).' >> cmd...


flrcoder.O : flrcoder.P flrcoder.H $(XSB_CHAR_DEFS) $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrcoder,$(OPTION)).' >> cmd...


flrutils.O : flrutils.P flrutils.H $(XSB_CHAR_DEFS) $(FLORA_TERMS) $(XSB_EMU_FLAG_DEFS)
	@echo 'mc(flrutils,$(OPTION)).' >> cmd...


flrwrapper.O : flrwrapper.P flrwrapper.H $(XSB_STANDARD) $(FLORA_TERMS)
	@echo 'mc(flrwrapper,$(OPTION)).' >> cmd...


flrundefhook.O : flrundefhook.P flrundefhook.H $(FLORA_TERMS)
	@echo 'mc(flrundefhook,$(OPTION)).' >> cmd...


flrshell.O : flrshell.P flrshell.H $(FLORA_TERMS)
	@echo 'mc(flrshell,$(OPTION)).' >> cmd...

flrwraparound.O : flrwraparound.P flrwraparound.H $(FLORA_TERMS) $(XSB_EMU_FLAG_DEFS)
	@echo 'mc(flrwraparound,$(OPTION)).' >> cmd...


init:
	/bin/rm -f $(SVFILE)
	@echo "file_write($(STDWARN),'-------------- Warnings/Errors --------------------'), file_nl($(STDWARN))." >> cmd...
	@echo "file_write($(STDWARN),'$(HDRMSG)'), file_nl($(STDWARN))." >> cmd...
	@echo "file_flush($(STDWARN),_)." >> cmd...
	@echo "import bootstrap_flora/0 from flora2." >> cmd...hdr
	@echo "file_reopen($(SVFILE),a,$(STDWARN),_)." >> cmd...hdr
	@echo "file_clone($(STDWARN),$(STDERR),_)." >> cmd...hdr
	@echo "bootstrap_flora." >> cmd...hdr


finish:
	@echo "file_write($(STDWARN),'-------------------- End --------------------------'), file_nl($(STDWARN))." >> cmd...


clean:
	/bin/rm -f *~ *.O ../flora2.O
	cd closure; $(MAKE) clean
	cd genincludes; $(MAKE) clean
	cd syslib; $(MAKE) clean
	cd lib; $(MAKE) clean
	cd debugger; $(MAKE) clean
	cd p2h; $(MAKE) clean
	cd trailer; $(MAKE) clean
	cd demos; $(MAKE) clean
	/bin/rm -f headerinc/*~
	/bin/rm -f syslibinc/*~
	/bin/rm -f libinc/*~
	/bin/rm -f includes/*~
	/bin/rm -f flrincludes/*~
	/bin/rm -f $(HOME)/.xsb/flora/*.P $(HOME)/.xsb/flora/*.O


.DONE:
	@echo Done
