##############################################################################
#									     #
# This is a sample Makefile for making the examples in this directory.	     #
# It assumes Linux as an example    	    	    	    	    	     #
#									     #
##############################################################################

## Copyright (C) The Research Foundation of SUNY, 1998
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

##################### You may need to edit some of these!


# Where the architectue specific XSB stuff is
ARCH=i686-pc-linux-gnu
#ARCH=i686-pc-linux-gnu-dbg
arch_dir=$(HOME)/XSB/XSB/config/$(ARCH)

CC=gcc
CFLAGS= -O4 -ansi -pedantic -Wall
#CFLAGS= -ansi -pedantic -Wall -g
LDFLAGS= -lm -ldl -Wl,-export-dynamic
# For Soraris use:
#LDFLAGS=  -lsocket -lm -lnsl -ldl -Wl,-export-dynamic

## DON'T EDIT BELOW THIS LINE
############################################################################

# Where to install XSB binaries
bindir=$(arch_dir)/bin
# where the object code goes
arch_config_objlibdir=$(arch_dir)/saved.o

xsbmodule=$(arch_config_objlibdir)/xsb.o

# just to be sure the sh is used 
SHELL=/bin/sh



test1: cmain.o
	gcc -o $(bindir)/xsbtest1 $(xsbmodule) cmain.o $(LDFLAGS)
	@echo ""
	@echo "***************************************************"
	@echo "The executable is in:"
	@echo "     $(bindir)/xsbtest1"
	@echo ""
	@echo ""

test2: cmain2.o
	gcc -o $(bindir)/xsbtest2 $(xsbmodule) cmain2.o $(LDFLAGS)
	@echo ""
	@echo "***************************************************"
	@echo "The executable is in:"
	@echo "     $(bindir)/xsbtest2"
	@echo ""
	@echo ""


cmain.o: cmain.c
	gcc -c $(CFLAGS) cmain.c

cmain2.o: cmain2.c
	gcc -c $(CFLAGS) cmain2.c

