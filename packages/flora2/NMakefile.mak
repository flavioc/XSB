# Make file for Microsoft NMAKE

OBJEXT = .xwam

ALLOBJS =  flroperator$(OBJEXT) \
	   flrnodefp$(OBJEXT) \
	   flrnowsp$(OBJEXT) \
	   flrarguments$(OBJEXT) \
	   flrprolog$(OBJEXT) \
	   flrfirstorder$(OBJEXT) \
	   flrprint$(OBJEXT) \
	   flrlibman$(OBJEXT) \
	   flrlexer$(OBJEXT) \
	   flrcomposer$(OBJEXT) \
	   flrparser$(OBJEXT) \
	   flrcompiler$(OBJEXT) \
	   flrcoder$(OBJEXT) \
	   flrutils$(OBJEXT) \
	   flrwrapper$(OBJEXT) \
	   flrwraparound$(OBJEXT) \
	   flrundefhook$(OBJEXT) \
	   flrshell$(OBJEXT)

OPTIONS = [optimize]
PROLOG = ..\..\config\x86-pc-windows\bin\xsb.exe

.SUFFIXES: .P $(OBJEXT)

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


.P$(OBJEXT):
	$(PROLOG) -e "bootstrap_flora,mc(%|fF,$(OPTIONS)). halt."


