## File:      packages/flora2/trailer/Makefile
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


XSBDIR = ../../..

CLOSURE = ../closure

HEADERINC = ../headerinc

INCLUDES = ../includes

ALLOBJS = standard.P \
	  eqlbasic.P \
          eqlflogic.P \
          patch.P

XSB = $(XSBDIR)/bin/xsb

PROLOG = $(XSB) -s -m 2000 -c 2000 -e 'set_dcg_style(standard),segfault_handler(warn).'

FLORA_INCLUDES_DIR = ../flrincludes

FLORA_TERMS = $(FLORA_INCLUDES_DIR)/flora_terms.flh

# Make sure warnings are saved in a file.
SVFILE = '$(XSBDIR)/build/.flora_top_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling XSB/packages/flora2/trailer:"


all: init $(ALLOBJS) finish
	@$(XSBDIR)/build/touch.sh cmd...
	$(PROLOG) < cmd...
	@rm cmd...
	@cat $(SVFILE)


standard.P : $(CLOSURE)/flrdbdyn.flh $(CLOSURE)/flrcommon.flh \
             $(CLOSURE)/flrtrailer.flh $(HEADERINC)/flrheader_inc.flh \
             $(HEADERINC)/flrdbdyn_inc.flh $(HEADERINC)/flrtrailer_inc.flh
	@echo 'flsetuptrailer(none).' >> cmd...


eqlbasic.P : $(CLOSURE)/flrdbdyn.flh $(CLOSURE)/flrcommon.flh \
             $(CLOSURE)/flreqltrailer.flh $(HEADERINC)/flrheader_inc.flh \
             $(HEADERINC)/flrdbdyn_inc.flh $(HEADERINC)/flreqltrailer_inc.flh
	@echo 'flsetuptrailer(basic).' >> cmd...


eqlflogic.P : $(CLOSURE)/flrdbdyn.flh $(CLOSURE)/flrcommon.flh \
              $(CLOSURE)/flreqltrailer.flh $(CLOSURE)/flrscalareql.flh \
              $(HEADERINC)/flrheader_inc.flh $(HEADERINC)/flrdbdyn_inc.flh \
              $(HEADERINC)/flreqltrailer_inc.flh $(HEADERINC)/flrscalareql_inc.flh
	@echo 'flsetuptrailer(flogic).' >> cmd...


patch.P : $(INCLUDES)/flrpatch.flh $(HEADERINC)/flrheader_inc.flh \
          $(HEADERINC)/flrdefinition_inc.flh
	@echo 'flsetuppatch.' >> cmd...


$(CLOSURE)/flrdbdyn.flh : $(CLOSURE)/flrdbdyn.fli $(CLOSURE)/flrpred.fli
	@cd $(CLOSURE); make


$(CLOSURE)/flrcommon.flh : $(CLOSURE)/flrcommon.fli $(CLOSURE)/flrpred.fli
	@cd $(CLOSURE); make


$(CLOSURE)/flrtrailer.flh : $(CLOSURE)/flrtrailer.fli $(CLOSURE)/flrpred.fli
	@cd $(CLOSURE); make


$(CLOSURE)/flreqltrailer.flh : $(CLOSURE)/flreqltrailer.fli $(CLOSURE)/flrpred.fli
	@cd $(CLOSURE); make


$(CLOSURE)/flrscalareql.flh : $(CLOSURE)/flrscalareql.fli $(CLOSURE)/flrpred.fli
	@cd $(CLOSURE); make


$(INCLUDES)/flrpatch.flh : $(INCLUDES)/flrpatch.fli $(FLORA_TERMS)
	@cd $(INCLUDES); make


init:
	/bin/rm -f $(SVFILE)
	@echo "import bootstrap_flora/0 from flora2." >> cmd...
	@echo "bootstrap_flora." >> cmd...
	@echo "[flrutils]." >> cmd...
	@echo "import flsetuptrailer/1 from flrutils." >> cmd...
	@echo "import flsetuppatch/0 from flrutils." >> cmd...
	@echo "file_reopen($(SVFILE),w,$(STDWARN),_)." >> cmd...
	@echo "file_clone($(STDWARN),$(STDERR),_)." >> cmd...
	@echo 'fmt_write($(STDWARN),"-------------- Warnings/Errors --------------------\n",_).' >> cmd...
	@echo 'fmt_write($(STDWARN),"%s\n",args($(HDRMSG))).' >> cmd...
	@echo "file_flush($(STDWARN),_)." >> cmd...


finish:
	@echo 'fmt_write($(STDWARN),"-------------------- End --------------------------\n",_).' >> cmd...


clean:
	/bin/rm -f *~ *.P *.O *.flh


.DONE:
	@echo Done
