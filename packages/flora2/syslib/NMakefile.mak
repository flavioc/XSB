# Make file for Microsoft NMAKE

ALLOBJS=  flranswer.O \
	  flrcontrol.O \
	  flrdisplay.O \
	  flrload.O \
	  flraggavg.O \
	  flraggcolbag.O \
	  flraggcolset.O \
	  flraggcount.O \
	  flraggmax.O \
	  flraggmin.O \
	  flraggsum.O \
	  flrstorage.O \
	  flrdbop.O \
	  flrbtdbop.O \
	  flrshdirect.O \
	  flrdynmod.O \
	  flrequality.O \
	  flrimport.O \
	  flrexpunge.O

OPTIONS=[optimize]
XSB=..\..\..\config\x86-pc-windows\bin\xsb.exe

.SUFFIXES: .P .O

ALL:: $(ALLOBJS)

CLEAN :
	-@erase *~
	-@erase *.O


.P.O:
	$(XSB) -e "bootstrap_flora,mc(%|fF,$(OPTIONS)). halt."


