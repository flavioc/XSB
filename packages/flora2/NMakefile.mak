## File:      flora2/NMakefile.mak -- Make file for Microsoft NMAKE
##
## Author(s): Michael Kifer
##
## Contact:   flora-users@lists.sourceforge.net
## 
## Copyright (C) The Research Foundation of SUNY, 1999-2002
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

OBJEXT = .xwam
PROLOGEXT = .P

ALLOBJS = flrarguments$(OBJEXT) \
	  flrcomposer$(OBJEXT) \
	  flrcompiler$(OBJEXT) \
	  flrcoder$(OBJEXT) \
	  flrdependency$(OBJEXT) \
	  flora2devel$(OBJEXT) \
	  flrhilogtable$(OBJEXT) \
	  flrlexer$(OBJEXT) \
	  flrlibman$(OBJEXT) \
	  flrnodefp$(OBJEXT) \
	  flrnowsp$(OBJEXT) \
	  flroperator$(OBJEXT) \
	  flrparser$(OBJEXT) \
	  flrporting$(OBJEXT) \
	  flrpretifydump$(OBJEXT) \
	  flrprint$(OBJEXT) \
	  flrprolog$(OBJEXT) \
	  flrregistry$(OBJEXT) \
	  flrshell$(OBJEXT) \
	  flrundefined$(OBJEXT) \
	  flrundefhook$(OBJEXT) \
	  flrutils$(OBJEXT) \
	  flrversion$(OBJEXT) \
	  flrwraparound$(OBJEXT) \
	  flrwrapper$(OBJEXT)

OPTIONS = [optimize]

!INCLUDE .prolog_path_wind

.SUFFIXES: $(PROLOGEXT) $(OBJEXT)

ALL:: $(ALLOBJS)
	cd p2h
	nmake /f NMakefile.mak
	cd ..\closure
	nmake /f NMakefile.mak
	cd ..\genincludes
	nmake /f NMakefile.mak
	cd ..\syslib
	nmake /f NMakefile.mak
	cd ..\trailer
	nmake /f NMakefile.mak
	cd ..\lib
	nmake /f NMakefile.mak
	cd ..\debugger
	nmake /f NMakefile.mak
	cd ..\pkgs
	nmake /f NMakefile.mak
	cd ..\demos
	nmake /f NMakefile.mak
	cd ..

CLEAN :
	-@erase *~
	-@erase *$(OBJEXT)
	-@erase *.bak
	-@erase .#*
	-@erase ..\flora2$(OBJEXT)
	cd p2h
	nmake /f NMakefile.mak clean
	cd ..\closure
	nmake /f NMakefile.mak clean
	cd ..\genincludes
	nmake /f NMakefile.mak clean
	cd ..\syslib
	nmake /f NMakefile.mak clean
	cd ..\trailer
	nmake /f NMakefile.mak clean
	cd ..\debugger
	nmake /f NMakefile.mak clean
	cd ..\pkgs
	nmake /f NMakefile.mak clean
	cd ..\demos
	nmake /f NMakefile.mak clean
	cd ..\docs
	nmake /f NMakefile.mak clean
	cd ..\emacs
	nmake /f NMakefile.mak clean
	cd ..


$(PROLOGEXT)$(OBJEXT):
	$(PROLOG) -e "[flora2devel]. import bootstrap_flora/0 from flora2. bootstrap_flora,mc(%|fF,$(OPTIONS)). halt."


