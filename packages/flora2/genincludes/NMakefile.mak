# Make file for Microsoft NMAKE


ALLOBJS = flrpatch.flh

XSB=..\..\..\config\x86-pc-windows\bin\xsb.exe

.SUFFIXES: .fli .flh

ALL: $(ALLOBJS)

CLEAN :
	-@erase *~
	-@erase *.flh


.fli.flh:
	$(XSB) -e "bootstrap_flora. [flrutils]. import flCompileInclude/1 from flrutils. flCompileInclude(%|fF). halt."

