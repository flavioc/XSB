## File:      flora2/Makefile
##
## Author(s): Michael Kifer
##            Guizhen Yang
##
## Contact:   flora-users@lists.sourceforge.net
## 
## Copyright (C) The Research Foundation of SUNY, 1999-2001
## 
## FLORA-2 is free software; you can redistribute it and/or modify it under the
## terms of the GNU Library General Public License as published by the Free
## Software Foundation; either version 2 of the License, or (at your option)
## any later version.
## 
## FLORA-2 is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
## more details.
## 
## You should have received a copy of the GNU Library General Public License
## along with FLORA-2; if not, write to the Free Software Foundation,
## Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##
## $Id$
## 
##

OBJEXT=.xwam

PROLOGDIR = ../..
FLORADIR = .

ALLOBJS = flroperator${OBJEXT} \
	  flrnodefp${OBJEXT} \
	  flrnowsp${OBJEXT} \
	  flrarguments${OBJEXT} \
	  flrprolog${OBJEXT} \
	  flrfirstorder${OBJEXT} \
	  flrprint${OBJEXT} \
	  flrlibman${OBJEXT} \
	  flrlexer${OBJEXT} \
	  flrcomposer${OBJEXT} \
          flrparser${OBJEXT} \
	  flrcompiler${OBJEXT} \
	  flrdependency${OBJEXT} \
	  flrcoder${OBJEXT} \
	  flrundefined${OBJEXT} \
	  flrutils${OBJEXT} \
	  flrwrapper${OBJEXT} \
	  flrwraparound${OBJEXT} \
	  flrprettyprint${OBJEXT} \
	  flrundefhook${OBJEXT} \
	  flrporting${OBJEXT} \
	  flrshell${OBJEXT}

EMU_INCLUDES = $(PROLOGDIR)/emu/builtin.h \
	       $(PROLOGDIR)/emu/flag_defs_xsb.h \
	       $(PROLOGDIR)/emu/io_defs_xsb.h \
               $(PROLOGDIR)/emu/syscall_xsb.h \
	       $(PROLOGDIR)/emu/system_defs_xsb.h \
               $(PROLOGDIR)/emu/socket_xsb.h 

XSB_CHAR_DEFS = $(PROLOGDIR)/prolog_includes/char_defs.h
XSB_STANDARD = $(PROLOGDIR)/prolog_includes/standard.h
XSB_EMU_FLAG_DEFS = $(PROLOGDIR)/emu/flag_defs_xsb.h

FLORA_INCLUDES_DIR = flrincludes

FLORA_TERMS = $(FLORA_INCLUDES_DIR)/flora_terms.flh
FLORA_ERRORS = $(FLORA_INCLUDES_DIR)/flora_errors.flh

OPTION = [optimize]

PROLOG = $(PROLOGDIR)/bin/xsb

PROLOG_COMMAND = $(PROLOG) -s -m 2000 -c 2000 -e 'segfault_handler(warn).'

# Make sure warnings are saved in a file.
SVFILE = '$(FLORADIR)/.flora_top_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling  .../flora2:"


all: init system libs misc finish showerrors

core: init system finish showerrors

extra: init system libs pkgs finish showerrors

showerrors:
	@cat $(FLORADIR)/.flora*.tmp

system: $(ALLOBJS)
	./CompileChangedFiles.sh $(PROLOG_COMMAND)
	@cat $(SVFILE)
	cd closure; $(MAKE)
	cd genincludes; $(MAKE)
	cd syslib; $(MAKE)
	cd trailer; $(MAKE)
	cd debugger; $(MAKE)

libs:
	cd lib; $(MAKE)

misc:
	cd pkgs; $(MAKE)
	cd demos; $(MAKE)
	cd docs; $(MAKE)


flroperator${OBJEXT} : flroperator.P flroperator.H $(FLORA_TERMS)
	@echo 'mc(flroperator,$(OPTION)).' >> cmd...


flrnodefp${OBJEXT} : flrnodefp.P flrnodefp.H $(FLORA_TERMS)
	@echo 'mc(flrnodefp,$(OPTION)).' >> cmd...


flrnowsp${OBJEXT} : flrnowsp.P flrnowsp.H $(FLORA_TERMS)
	@echo 'mc(flrnowsp,$(OPTION)).' >> cmd...


flrarguments${OBJEXT} : flrarguments.P flrarguments.H $(FLORA_TERMS)
	@echo 'mc(flrarguments,$(OPTION)).' >> cmd...


flrprolog${OBJEXT} : flrprolog.P flrprolog.H $(FLORA_TERMS)
	@echo 'mc(flrprolog,$(OPTION)).' >> cmd...


flrfirstorder${OBJEXT} : flrfirstorder.P flrfirstorder.H $(FLORA_TERMS)
	@echo 'mc(flrfirstorder,$(OPTION)).' >> cmd...


flrprint${OBJEXT} : flrprint.P flrprint.H $(XSB_STANDARD)
	@echo 'mc(flrprint,$(OPTION)).' >> cmd...


flrlibman${OBJEXT} : flrlibman.P flrlibman.H $(FLORA_TERMS)
	@echo 'mc(flrlibman,$(OPTION)).' >> cmd...


flrlexer${OBJEXT} : flrlexer.P flrlexer.H $(XSB_CHAR_DEFS) $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrlexer,$(OPTION)).' >> cmd...


flrcomposer${OBJEXT} : flrcomposer.P flrcomposer.H $(XSB_CHAR_DEFS) $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrcomposer,$(OPTION)).' >> cmd...


flrparser${OBJEXT} : flrparser.P flrparser.H $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrparser,$(OPTION)).' >> cmd...


flrcompiler${OBJEXT} : flrcompiler.P flrcompiler.H $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrcompiler,$(OPTION)).' >> cmd...


flrdependency${OBJEXT} : flrdependency.P flrdependency.H $(FLORA_TERMS)
	@echo 'mc(flrdependency,$(OPTION)).' >> cmd...


flrcoder${OBJEXT} : flrcoder.P flrcoder.H $(XSB_CHAR_DEFS) $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrcoder,$(OPTION)).' >> cmd...


flrundefined${OBJEXT} : flrundefined.P flrundefined.H $(FLORA_TERMS) 
	@echo 'mc(flrundefined,$(OPTION)).' >> cmd...


flrutils${OBJEXT} : flrutils.P flrutils.H $(XSB_CHAR_DEFS) $(FLORA_TERMS) $(XSB_EMU_FLAG_DEFS)
	@echo 'mc(flrutils,$(OPTION)).' >> cmd...


flrwrapper${OBJEXT} : flrwrapper.P flrwrapper.H $(XSB_STANDARD) $(FLORA_TERMS)
	@echo 'mc(flrwrapper,$(OPTION)).' >> cmd...


flrundefhook${OBJEXT} : flrundefhook.P flrundefhook.H $(FLORA_TERMS)
	@echo 'mc(flrundefhook,$(OPTION)).' >> cmd...


flrshell${OBJEXT} : flrshell.P flrshell.H $(FLORA_TERMS)
	@echo 'mc(flrshell,$(OPTION)).' >> cmd...

flrwraparound${OBJEXT} : flrwraparound.P flrwraparound.H $(FLORA_TERMS) $(XSB_EMU_FLAG_DEFS)
	@echo 'mc(flrwraparound,$(OPTION)).' >> cmd...

flrprettyprint${OBJEXT} : flrprettyprint.P $(FLORA_TERMS)
	@echo 'mc(flrprettyprint,$(OPTION)).' >> cmd...

flrporting${OBJEXT} : flrporting.P $(FLORA_TERMS)
	@echo 'mc(flrporting,$(OPTION)).' >> cmd...


init:
	/bin/rm -f $(FLORADIR)/.*flora*.tmp
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
	/bin/rm -f core *~ .*~ *${OBJEXT} *.O *.bak ../flora2${OBJEXT} .#*
	cd closure; $(MAKE) clean
	cd genincludes; $(MAKE) clean
	cd syslib; $(MAKE) clean
	cd lib; $(MAKE) clean
	cd debugger; $(MAKE) clean
	cd p2h; $(MAKE) clean
	cd trailer; $(MAKE) clean
	cd pkgs; $(MAKE) clean
	cd demos; $(MAKE) clean
	cd docs; $(MAKE) clean
	cd emacs; $(MAKE) clean
	/bin/rm -f headerinc/*~ headerinc/.*~
	/bin/rm -f syslibinc/*~ syslibinc/.*~
	/bin/rm -f libinc/*~ libinc/.*~
	/bin/rm -f includes/*~ includes/.*~
	/bin/rm -f flrincludes/*~ flrincludes/.*~
	/bin/rm -f $(HOME)/.xsb/flora/*.P $(HOME)/.xsb/flora/*${OBJEXT}


.DONE:
	@echo Done
