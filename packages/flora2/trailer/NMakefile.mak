# Make file for Microsoft NMAKE


OPTIONS = [optimize]
XSB = ..\..\..\config\x86-pc-windows\bin\xsb.exe

.SUFFIXES: .P .O

ALL:: 
	$(XSB) -e "bootstrap_flora. [flrutils]. import flsetuptrailer/1, flsetuppatch/0 from flrutils. flsetuptrailer(none). flsetuptrailer(basic). flsetuptrailer(flogic). flsetuppatch. halt."

CLEAN :
	-@erase *~
	-@erase *.O
	-@erase *.P
	-@erase *.flh
	-@erase *.bak
	-@erase .#*




