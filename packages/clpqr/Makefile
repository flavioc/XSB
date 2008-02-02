## File:      packages/slx/Makefile
## Author(s): kifer
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 2008
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

OBJEXT=.xwam

XSBDIR=../..

#ALL = 	bv_r${OBJEXT} bv_q${OBJEXT} class${OBJEXT} \
#	fourmotz_r${OBJEXT} geler${OBJEXT} \
#	ineq_r${OBJEXT} itf${OBJEXT} itf_r${OBJEXT} nf_r${OBJEXT} \
#	ordering${OBJEXT} project${OBJEXT} redund${OBJEXT} \
#	store_r${OBJEXT} swi${OBJEXT} clpr${OBJEXT}
ALL = clpr_make${OBJEXT}

Option = [optimize]

#-----------------------------------------------------------------------#
#
# Self-make: Version 1.4.1
#
XSB=$(XSBDIR)/bin/xsb
Prolog = $(XSB) -e 'segfault_handler(warn).'
#
#-----------------------------------------------------------------------#

# Make sure warnings are saved in a file.
SVFILE = '$(XSBDIR)/build/.clpqr_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling XSB/packages/clpqr:"



# continue to the next when one error occurs
.IGNORE:

# suffix rules
.SUFFIXES: .P .H ${OBJEXT}

.P${OBJEXT}:
	echo 'mc($*, $(Option)).' >> cmd...

all: init $(ALL) finish
	@$(XSBDIR)/build/touch.sh cmd...
	echo 'import make_clpr/0 from clpr_make.' >> cmd...
	echo 'make_clpr.' >> cmd...
	$(Prolog) < cmd...
	@rm cmd...


init:
	/bin/rm -f $(SVFILE)
	@echo "file_reopen($(SVFILE),w,$(STDWARN),_)." >> cmd...
	@echo "file_clone($(STDWARN), $(STDERR),_)." >> cmd...
	@echo 'fmt_write($(STDWARN),"-------------- Warnings/Errors --------------------\n",_).' >> cmd...
	@echo 'fmt_write($(STDWARN),"%s\n",arg($(HDRMSG))).' >> cmd...
	@echo "file_flush($(STDWARN), _)." >> cmd...

finish:
	@echo 'fmt_write($(STDWARN),"-------------------- End --------------------------\n",_).' >> cmd...

.PHONY: clean

clean:
	/bin/rm -f *${OBJEXT} *.O *~ .*~ *.bak


.DONE:
	echo Done
