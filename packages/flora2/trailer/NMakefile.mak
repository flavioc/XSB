# Make file for Microsoft NMAKE

OBJEXT = .O

OPTIONS = [optimize]
XSB = ..\..\..\config\x86-pc-windows\bin\xsb.exe

.SUFFIXES: .P .O

ALL:: 
	$(XSB) -e "bootstrap_flora. [flrutils]. import flsetuptrailer/1, flsetuppatch/0 from flrutils. flsetuptrailer(none). flsetuptrailer(basic). flsetuptrailer(flogic). flsetuppatch. halt."

CLEAN :
	-@erase *~
	-@erase *$(OBJEXT)
	-@erase *.P
	-@erase *.flh
	-@erase *.bak
	-@erase .#*




