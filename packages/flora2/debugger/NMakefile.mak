# Make file for Microsoft NMAKE

OBJEXT = .O

ALLOBJS=  flrdebugger$(OBJEXT) dynamic_data.dat static_data.dat

OPTIONS=[optimize]
PROLOG=..\..\..\config\x86-pc-windows\bin\xsb.exe

.SUFFIXES:  .in .dat .P .H $(OBJEXT)

ALL:: $(ALLOBJS)

CLEAN :
	-@erase *~
	-@erase *$(OBJEXT)
	-@erase .#*
	-@erase *.dat
	-@erase *.bak

.P$(OBJEXT):
	$(PROLOG) -e "bootstrap_flora,mc(%|fF,$(OPTIONS)). halt."

static_data.dat: static_data.in
	copy static_data.in static_data.dat

dynamic_data.dat: dynamic_data.in
	$(PROLOG) -e "bootstrap_flora. [flrwraparound]. import flrWrapAround/1 from flrwraparound. flWrapAround(%|fF). halt.


