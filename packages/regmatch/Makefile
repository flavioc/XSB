## File:      packages/regmatch/Makefile
## Author(s): kifer
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 1999
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


ALL = 	regmtchconfig.O regmtchshell.O

Option = [optimize]

#-----------------------------------------------------------------------#
#
# Self-make: Version 1.4.1
#
XSB=../../bin/xsb
Prolog = $(XSB) -i -s -m 2000 -c 2000
#
#-----------------------------------------------------------------------#

# continue to the next when one error occurs
.IGNORE:

# suffix rules
.SUFFIXES: .P .O .H
.P.O:
	echo 'mc($*, $(Option)).' >> cmd...

all: $(ALL)
	@touch cmd...
	$(Prolog) < cmd...
	/bin/rm cmd...



# Unfortunately, this needs to be commented on the NeXT.
# %.O:	%.H

.DONE:
	echo Done
