## File:      syslib/Makefile
## Author(s): Kostis Sagonas, Jiyang Xu
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
## Copyright (C) ECRC, Germany, 1990
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

ALL =	assert.O basics.O consult.O curr_sym.O dcg.O debugger.O \
	domain.O eval.O file_op.O foreign.O hilogsym.O loader.O \
	machine.O setof.O standard.O std_sbp.O \
	string.O subsumes.O term_exp.O \
	x_interp.O xcallxsb.O xsb_read.O xsb_tok.O xsb_writ.O \
	packaging.O sysinitrc.O tables.O unix.O


# The following file(s) are not used any more do not make them!
# dcg_qsc.O

Option = [sysmod,optimize]

#-------------------------------------------------------------------#
### Please select ONE Prolog below to run this makefile.
### At developement stage, an older version of XSB or PSB-Prolog is 
### suggested.
###
### Note: if you select self-make, be careful to save all the
###	  current *.O files, so that when the new files have
###	  errors, the old *.O can be re-installed.
#
# Self-make: Version 1.4.1
#
XSB=../bin/xsb
Prolog = $(XSB) -i -s -m 1200 -c 2000
#
#-------------------------------------------------------------------#

# continue to the next file when an error occurs
.IGNORE:

# suffix rules
.SUFFIXES: .P .H .O

.H.P:
	echo 'mc($*, $(Option)).' >> cmd...

.P.O:
	echo 'mc($*, $(Option)).' >> cmd...

all: $(ALL)
	echo 'statistics.' >> cmd...
	$(Prolog) < cmd...
	/bin/rm cmd...


# Unfortunately, this needs to be commented on the NeXT
# %.O:	%.H

.DONE:
	echo Done
