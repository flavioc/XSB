# Make file for Microsoft NMAKE

ALLOBJS = flrcommon.flh \
          flrtrailer.flh \
	  flreqltrailer.flh \
          flrscalareql.flh

PROLOG_COMMAND = ..\..\..\config\x86-pc-windows\bin\xsb.exe

.SUFFIXES: .flh .fli

ALL: $(ALLOBJS)

CLEAN :
	-@erase *~
	-@erase *.flh
	-@erase *.bak
	-@erase .#*
	-@erase includes\*~

.fli.flh:
	$(PROLOG_COMMAND) -e "bootstrap_flora. [flrutils]. import flCompileInclude/1 from flrutils. flCompileInclude(%|fF). halt."

