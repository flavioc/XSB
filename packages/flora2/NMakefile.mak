# Make file for Microsoft NMAKE

ALLOBJS =  flroperator.O \
	   flrnodefp.O \
	   flrnowsp.O \
	   flrarguments.O \
	   flrprolog.O \
	   flrfirstorder.O \
	   flrprint.O \
	   flrlibman.O \
	   flrlexer.O \
	   flrcomposer.O \
	   flrparser.O \
	   flrcompiler.O \
	   flrcoder.O \
	   flrutils.O \
	   flrwrapper.O \
	   flrwraparound.O \
	   flrundefhook.O \
	   flrshell.O

OPTIONS = [optimize]
XSB = ..\..\config\x86-pc-windows\bin\xsb.exe

.SUFFIXES: .P .O

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
	-@erase *.O
	-@erase *.bak
	-@erase .#*
	-@erase ..\flora2.O
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


.P.O:
	$(XSB) -e "bootstrap_flora,mc(%|fF,$(OPTIONS)). halt."


