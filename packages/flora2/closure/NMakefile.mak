# Make file for Microsoft NMAKE

ALLOBJS = flrcommon.flh \
          flrtrailer.flh \
	  flreqltrailer.flh \
          flrscalareql.flh

XSB=..\..\..\config\x86-pc-windows\bin\xsb.exe

.SUFFIXES: .flh .fli

ALL: $(ALLOBJS)

CLEAN :
	-@erase *~
	-@erase *.flh
	-@erase includes\*~

.fli.flh:
	$(XSB) -e "bootstrap_flora. [flrutils]. import flCompileInclude/1 from flrutils. flCompileInclude(%|fF). halt."

