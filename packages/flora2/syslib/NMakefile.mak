# Make file for Microsoft NMAKE

OBJEXT = .xwam

ALLOBJS =  flranswer$(OBJEXT) \
	   flrcontrol$(OBJEXT) \
	   flrdisplay$(OBJEXT) \
	   flrload$(OBJEXT) \
	   flraggavg$(OBJEXT) \
	   flraggcolbag$(OBJEXT) \
	   flraggcolset$(OBJEXT) \
	   flraggcount$(OBJEXT) \
	   flraggmax$(OBJEXT) \
	   flraggmin$(OBJEXT) \
	   flraggsum$(OBJEXT) \
	   flrstorage$(OBJEXT) \
	   flrdbop$(OBJEXT) \
	   flrbtdbop$(OBJEXT) \
	   flrshdirect$(OBJEXT) \
	   flrdynmod$(OBJEXT) \
	   flrequality$(OBJEXT) \
	   flrimport$(OBJEXT) \
	   flrexpunge$(OBJEXT)

OPTIONS = [optimize]
XSB = ..\..\..\config\x86-pc-windows\bin\xsb.exe

.SUFFIXES: .P $(OBJEXT)

ALL:: $(ALLOBJS)

CLEAN :
	-@erase *~
	-@erase *$(OBJEXT)
	-@erase *.bak
	-@erase .#*


.P$(OBJEXT):
	$(XSB) -e "bootstrap_flora,mc(%|fF,$(OPTIONS)). halt."


