## File:      packages/flora2/closure/Makefile
##
## Author(s): Michael Kifer
##            Guizhen Yang
##
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 1999-2000
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

ALLOBJS = flrtrailer.flh \
	  flreqltrailer.flh \
          flrdbdyn.flh

XSB = $(XSBDIR)/bin/xsb

PROLOG = $(XSB) -s -m 2000 -c 2000 -e 'set_dcg_style(standard),segfault_handler(warn).'

# Make sure warnings are saved in a file.
SVFILE = '$(XSBDIR)/build/.flora_top_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling XSB/packages/flora2/closure:"


all: init $(ALLOBJS) finish
	@$(XSBDIR)/build/touch.sh cmd...
	$(PROLOG) < cmd...
	@rm cmd...
	@cat $(SVFILE)


flrtrailer.flh : flrtrailer.fli
	@echo 'flcompinc(flrtrailer).' >> cmd...


flreqltrailer.flh : flreqltrailer.fli
	@echo 'flcompinc(flreqltrailer).' >> cmd...


flrdbdyn.flh : flrdbdyn.fli
	@echo 'flcompinc(flrdbdyn).' >> cmd...


init:
	/bin/rm -f $(SVFILE)
	@echo "import bootstrap_flora/0 from flora2." >> cmd...
	@echo "bootstrap_flora." >> cmd...
	@echo "[flrutils]." >> cmd...
	@echo "import flcompinc/1 from flrutils." >> cmd...
	@echo "import flsetuptrailer/1 from flrutils." >> cmd...
	@echo "file_reopen($(SVFILE),w,$(STDWARN),_)." >> cmd...
	@echo "file_clone($(STDWARN),$(STDERR),_)." >> cmd...
	@echo 'fmt_write($(STDWARN),"-------------- Warnings/Errors --------------------\n",_).' >> cmd...
	@echo 'fmt_write($(STDWARN),"%s\n",args($(HDRMSG))).' >> cmd...
	@echo "file_flush($(STDWARN),_)." >> cmd...


finish:
	@echo 'fmt_write($(STDWARN),"-------------------- End --------------------------\n",_).' >> cmd...


.DONE:
	@echo Done
