# Make file for Microsoft NMAKE

ALLOBJS=  flrdebugger.O dynamic_data.dat static_data.dat

OPTIONS=[optimize]
XSB=..\..\..\config\x86-pc-windows\bin\xsb.exe

.SUFFIXES:  .in .dat .P .H .O

ALL:: $(ALLOBJS)

CLEAN :
	-@erase *~
	-@erase *.O
	-@erase .#*
	-@erase *.dat
	-@erase *.bak

.P.O:
	$(XSB) -e "bootstrap_flora,mc(%|fF,$(OPTIONS)). halt."

static_data.dat: static_data.in
	copy static_data.in static_data.dat

dynamic_data.dat: dynamic_data.in
	$(XSB) -e "bootstrap_flora. [flrwraparound]. import flrWrapAround/1 from flrwraparound. flWrapAround(%|fF). halt.


