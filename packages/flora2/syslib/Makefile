## File:      flora2/syslib/Makefile
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

PROLOG = ../../../bin/xsb
FLORADIR = ..

ALLOBJS = flranswer${OBJEXT} \
	  flrcontrol${OBJEXT} \
	  flrdisplay${OBJEXT} \
	  flrload${OBJEXT} \
	  flraggavg${OBJEXT} \
	  flraggcolbag${OBJEXT} \
	  flraggcolset${OBJEXT} \
	  flraggcount${OBJEXT} \
	  flraggmax${OBJEXT} \
	  flraggmin${OBJEXT} \
	  flraggsum${OBJEXT} \
	  flrstorage${OBJEXT} \
	  flrdbop${OBJEXT} \
	  flrbtdbop${OBJEXT} \
	  flrshdirect${OBJEXT} \
	  flrdynmod${OBJEXT} \
	  flrequality${OBJEXT} \
	  flrimport${OBJEXT} \
	  flrexpunge${OBJEXT}

FLORA_INCLUDES_DIR = ../flrincludes
DYN_INCLUDES_DIR = ../includes

FLORA_TERMS = $(FLORA_INCLUDES_DIR)/flora_terms.flh
FLORA_ERRORS = $(FLORA_INCLUDES_DIR)/flora_errors.flh
FLRHEADER = $(DYN_INCLUDES_DIR)/flrheader.flh


OPTION = [optimize]

PROLOG_COMMAND = $(PROLOG) -s -m 2000 -c 2000 -e 'segfault_handler(warn).'

# Make sure warnings are saved in a file.
SVFILE = '$(FLORADIR)/.flora_syslib_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling .../flora2/syslib:"


all: init $(ALLOBJS) finish
	./CompileChangedFiles.sh $(PROLOG_COMMAND)
	@cat $(SVFILE)


flranswer${OBJEXT} : flranswer.P $(FLORA_TERMS)
	@echo 'mc(flranswer,$(OPTION)).' >> cmd...


flrcontrol${OBJEXT} : flrcontrol.P $(FLORA_TERMS) $(XSB_STANDARD)
	@echo 'mc(flrcontrol,$(OPTION)).' >> cmd...

flrdisplay${OBJEXT} : flrdisplay.P $(FLORA_TERMS) $(XSB_STANDARD)
	@echo 'mc(flrdisplay,$(OPTION)).' >> cmd...


flrload${OBJEXT} : flrload.P $(FLORA_TERMS)
	@echo 'mc(flrload,$(OPTION)).' >> cmd...


flraggavg${OBJEXT} :flraggavg.P $(FLORA_TERMS)
	@echo 'mc(flraggavg,$(OPTION)).' >> cmd...


flraggcolbag${OBJEXT} : flraggcolbag.P $(FLORA_TERMS)
	@echo 'mc(flraggcolbag,$(OPTION)).' >> cmd...


flraggcolset${OBJEXT} : flraggcolset.P $(FLORA_TERMS)
	@echo 'mc(flraggcolset,$(OPTION)).' >> cmd...


flraggcount${OBJEXT} : flraggcount.P $(FLORA_TERMS)
	@echo 'mc(flraggcount,$(OPTION)).' >> cmd...


flraggmax${OBJEXT} : flraggmax.P $(FLORA_TERMS)
	@echo 'mc(flraggmax,$(OPTION)).' >> cmd...


flraggmin${OBJEXT} : flraggmin.P $(FLORA_TERMS)
	@echo 'mc(flraggmin,$(OPTION)).' >> cmd...


flraggsum${OBJEXT} : flraggsum.P $(FLORA_TERMS)
	@echo 'mc(flraggsum,$(OPTION)).' >> cmd...


flrstorage${OBJEXT} : flrstorage.P $(FLORA_TERMS)
	@echo 'mc(flrstorage,$(OPTION)).' >> cmd...


flrdbop${OBJEXT} : flrdbop.P $(FLORA_TERMS)
	@echo 'mc(flrdbop,$(OPTION)).' >> cmd...


flrbtdbop${OBJEXT} : flrbtdbop.P $(FLORA_TERMS)
	@echo 'mc(flrbtdbop,$(OPTION)).' >> cmd...


flrshdirect${OBJEXT} : flrshdirect.P $(FLORA_TERMS)
	@echo 'mc(flrshdirect,$(OPTION)).' >> cmd...


flrdynmod${OBJEXT} : flrdynmod.P $(FLORA_TERMS)
	@echo 'mc(flrdynmod,$(OPTION)).' >> cmd...


flrequality${OBJEXT} : flrequality.P $(FLORA_TERMS)
	@echo 'mc(flrequality,$(OPTION)).' >> cmd...

flrimport${OBJEXT}: flrimport.P $(FLORA_TERMS) $(FLRHEADER)
	@echo 'mc(flrimport,$(OPTION)).' >> cmd...


flrexpunge${OBJEXT} : flrexpunge.P $(FLORA_TERMS)
	@echo 'mc(flrexpunge,$(OPTION)).' >> cmd...


init:
	/bin/rm -f $(SVFILE)
	@echo "file_write($(STDWARN),'-------------- Warnings/Errors --------------------'), file_nl($(STDWARN))." >> cmd...
	@echo "file_write($(STDWARN),'$(HDRMSG)'), file_nl($(STDWARN))." >> cmd...
	@echo "file_flush($(STDWARN),_)." >> cmd...
	@echo "['../flora2devel']." >> cmd...hdr
	@echo "import bootstrap_flora/0 from flora2." >> cmd...hdr
	@echo "bootstrap_flora." >> cmd...hdr
	@echo "file_reopen($(SVFILE),a,$(STDWARN),_)." >> cmd...hdr
	@echo "file_clone($(STDWARN),$(STDERR),_)." >> cmd...hdr


finish:
	@echo "file_write($(STDWARN),'-------------------- End --------------------------'), file_nl($(STDWARN))." >> cmd...


clean:
	/bin/rm -f core *~ .*~ *${OBJEXT} *.O *.bak .#*


.DONE:
	@echo Done
