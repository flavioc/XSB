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
	  flrregistry${OBJEXT} \
	  flrshell${OBJEXT} \
	  flora2devel${OBJEXT}


FLORA_CHAR_DEFS = $(FLORADIR)/flrincludes/flora_characters.flh

FLORA_INCLUDES_DIR = flrincludes

FLORA_TERMS = $(FLORA_INCLUDES_DIR)/flora_terms.flh
FLORA_ERRORS = $(FLORA_INCLUDES_DIR)/flora_errors.flh

OPTION = [optimize]

# PROLOG is passed as an argument to make. Default is none, to trigger an error
PROLOG=none
PROLOG_COMMAND = $(PROLOG) -s -m 2000 -c 2000 -e 'segfault_handler(warn).'

# Make sure warnings are saved in a file.
SVFILE = '$(FLORADIR)/.flora_top_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling  .../flora2:"


# acceptable targets are all (default), system, base, top
all: core libs pkgs misc showerrors

system: core libs showerrors

base: core showerrors

showerrors:
	@cat $(FLORADIR)/.flora*.tmp
	@rm $(FLORADIR)/.flora*.tmp

top: init $(ALLOBJS) finish
	./CompileChangedFiles.sh $(PROLOG_COMMAND)
	@cat $(SVFILE)

core: top
	cd closure; $(MAKE) PROLOG=$(PROLOG)
	cd genincludes; $(MAKE) PROLOG=$(PROLOG)
	cd syslib; $(MAKE) PROLOG=$(PROLOG)
	cd trailer; $(MAKE) PROLOG=$(PROLOG)
	cd debugger; $(MAKE) PROLOG=$(PROLOG)

libs:
	cd lib; $(MAKE) PROLOG=$(PROLOG)

misc:
	cd pkgs; $(MAKE) PROLOG=$(PROLOG)
	cd demos; $(MAKE) PROLOG=$(PROLOG)
	cd docs; $(MAKE) PROLOG=$(PROLOG)


# always compile flora2devel to make sure that .xwam has the latest .flora_path
flora2devel${OBJEXT} : flora2devel.P
	@echo 'mc(flora2devel,$(OPTION)).' >> cmd...

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


flrprint${OBJEXT} : flrprint.P flrprint.H
	@echo 'mc(flrprint,$(OPTION)).' >> cmd...


flrlibman${OBJEXT} : flrlibman.P flrlibman.H $(FLORA_TERMS)
	@echo 'mc(flrlibman,$(OPTION)).' >> cmd...


flrlexer${OBJEXT} : flrlexer.P flrlexer.H $(FLORA_CHAR_DEFS) $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrlexer,$(OPTION)).' >> cmd...


flrcomposer${OBJEXT} : flrcomposer.P flrcomposer.H $(FLORA_CHAR_DEFS) $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrcomposer,$(OPTION)).' >> cmd...


flrparser${OBJEXT} : flrparser.P flrparser.H $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrparser,$(OPTION)).' >> cmd...


flrcompiler${OBJEXT} : flrcompiler.P flrcompiler.H $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrcompiler,$(OPTION)).' >> cmd...


flrdependency${OBJEXT} : flrdependency.P flrdependency.H $(FLORA_TERMS)
	@echo 'mc(flrdependency,$(OPTION)).' >> cmd...


flrcoder${OBJEXT} : flrcoder.P flrcoder.H $(FLORA_CHAR_DEFS) $(FLORA_TERMS) $(FLORA_ERRORS)
	@echo 'mc(flrcoder,$(OPTION)).' >> cmd...


flrundefined${OBJEXT} : flrundefined.P flrundefined.H $(FLORA_TERMS) 
	@echo 'mc(flrundefined,$(OPTION)).' >> cmd...


flrutils${OBJEXT} : flrutils.P flrutils.H $(FLORA_CHAR_DEFS) $(FLORA_TERMS) 
	@echo 'mc(flrutils,$(OPTION)).' >> cmd...


flrwrapper${OBJEXT} : flrwrapper.P flrwrapper.H $(FLORA_TERMS)
	@echo 'mc(flrwrapper,$(OPTION)).' >> cmd...


flrundefhook${OBJEXT} : flrundefhook.P flrundefhook.H $(FLORA_TERMS)
	@echo 'mc(flrundefhook,$(OPTION)).' >> cmd...


flrshell${OBJEXT} : flrshell.P flrshell.H $(FLORA_TERMS)
	@echo 'mc(flrshell,$(OPTION)).' >> cmd...

flrwraparound${OBJEXT} : flrwraparound.P flrwraparound.H $(FLORA_TERMS)
	@echo 'mc(flrwraparound,$(OPTION)).' >> cmd...

flrprettyprint${OBJEXT} : flrprettyprint.P $(FLORA_TERMS)
	@echo 'mc(flrprettyprint,$(OPTION)).' >> cmd...

flrporting${OBJEXT} : flrporting.P $(FLORA_TERMS)
	@echo 'mc(flrporting,$(OPTION)).' >> cmd...

flrregistry${OBJEXT} : flrregistry.P $(FLORA_TERMS)
	@echo 'mc(flrregistry,$(OPTION)).' >> cmd...


init:
	/bin/rm -f $(FLORADIR)/.*flora*.tmp
	@echo "file_write($(STDWARN),'-------------- Warnings/Errors --------------------'), file_nl($(STDWARN))." > cmd...
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
	/bin/rm -f .*.tmp .flora_path .prolog_path
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
