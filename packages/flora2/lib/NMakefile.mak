# Make file for Microsoft NMAKE

OBJEXT = .xwam

ALLOBJS = flrprettyprint.P flrio.P flrstorage.P flrsystem.P

XSB = ..\..\..\config\x86-pc-windows\bin\xsb.exe

OPTIONS = [optimize]

.SUFFIXES: .P .flr

ALL: $(ALLOBJS)


.flr.P:
	$(XSB) -e "bootstrap_flora. import flora_compile_system_module/1 from flrutils. flora_compile_system_module(%|fF). halt."


CLEAN:
	-@del *~
	-@del *$(OBJEXT)
	-@del *.P
	-@del *.fdb
	-@del *.fld
	-@del *.bak
	-@del .#*
