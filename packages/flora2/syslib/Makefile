## File:      packages/flora2/syslib/Makefile
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

ALLOBJS = flranswer.O \
	  flrdisplay.O \
	  flrload.O \
	  flraggavg.O \
	  flraggcolbag.O \
	  flraggcolset.O \
	  flraggcount.O \
	  flraggmax.O \
	  flraggmin.O \
	  flraggsum.O \
	  flrdbop.O \
	  flrshdirect.O

EMU_INCLUDES = $(XSBDIR)/emu/builtin.h \
	       $(XSBDIR)/emu/flag_defs_xsb.h \
	       $(XSBDIR)/emu/io_defs_xsb.h \
               $(XSBDIR)/emu/syscall_xsb.h \
	       $(XSBDIR)/emu/system_defs_xsb.h \
               $(XSBDIR)/emu/socket_xsb.h 

XSB_CHAR_DEFS = $(XSBDIR)/prolog_includes/char_defs.h
XSB_STANDARD = $(XSBDIR)/prolog_includes/standard.h

FLORA_INCLUDES_DIR = ../includes

FLORA_TERMS = $(FLORA_INCLUDES_DIR)/flora_terms.flh
FLORA_ERRORS = $(FLORA_INCLUDES_DIR)/flora_errors.flh


OPTION = [optimize]

XSB = $(XSBDIR)/bin/xsb

PROLOG = $(XSB) -s -m 2000 -c 2000 -e 'set_dcg_style(standard),segfault_handler(warn).'

# Make sure warnings are saved in a file.
SVFILE = '$(XSBDIR)/build/.flora_top_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling XSB/packages/flora2/syslib:"


all: init $(ALLOBJS) finish
	@$(XSBDIR)/build/touch.sh cmd...
	$(PROLOG) < cmd...
	@rm cmd...
	@cat $(SVFILE)


flranswer.O : flranswer.P $(XSB_CHAR_DEFS) $(XSB_STANDARD) $(FLORA_TERMS)
	@echo 'mc(flranswer,$(OPTION)).' >> cmd...


flrdisplay.O : flrdisplay.P $(FLORA_TERMS)
	@echo 'mc(flrdisplay,$(OPTION)).' >> cmd...


flrload.O : flrload.P $(XSB_STANDARD) $(FLORA_TERMS)
	@echo 'mc(flrload,$(OPTION)).' >> cmd...


flraggavg.O :flraggavg.P $(FLORA_TERMS)
	@echo 'mc(flraggavg,$(OPTION)).' >> cmd...


flraggcolbag.O : flraggcolbag.P $(FLORA_TERMS)
	@echo 'mc(flraggcolbag,$(OPTION)).' >> cmd...


flraggcolset.O : flraggcolset.P $(FLORA_TERMS)
	@echo 'mc(flraggcolset,$(OPTION)).' >> cmd...


flraggcount.O : flraggcount.P $(FLORA_TERMS)
	@echo 'mc(flraggcount,$(OPTION)).' >> cmd...


flraggmax.O : flraggmax.P $(FLORA_TERMS)
	@echo 'mc(flraggmax,$(OPTION)).' >> cmd...


flraggmin.O : flraggmin.P $(FLORA_TERMS)
	@echo 'mc(flraggmin,$(OPTION)).' >> cmd...


flraggsum.O : flraggsum.P $(FLORA_TERMS)
	@echo 'mc(flraggsum,$(OPTION)).' >> cmd...


flrdbop.O : flrdbop.P $(FLORA_TERMS)
	@echo 'mc(flrdbop,$(OPTION)).' >> cmd...


flrshdirect.O : flrshdirect.P $(FLORA_TERMS)
	@echo 'mc(flrshdirect,$(OPTION)).' >> cmd...


init:
	/bin/rm -f $(SVFILE)
	@echo "import bootstrap_flora/0 from flora2." >> cmd...
	@echo "bootstrap_flora." >> cmd...
	@echo "file_reopen($(SVFILE),w,$(STDWARN),_)." >> cmd...
	@echo "file_clone($(STDWARN),$(STDERR),_)." >> cmd...
	@echo 'fmt_write($(STDWARN),"-------------- Warnings/Errors --------------------\n",_).' >> cmd...
	@echo 'fmt_write($(STDWARN),"%s\n",args($(HDRMSG))).' >> cmd...
	@echo "file_flush($(STDWARN),_)." >> cmd...


finish:
	@echo 'fmt_write($(STDWARN),"-------------------- End --------------------------\n",_).' >> cmd...


.DONE:
	@echo Done
