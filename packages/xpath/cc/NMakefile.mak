 # Make file for driver_manager.dll

XSBDIR=..\..\..
MYPROGRAM=xpathparser
LIBXML2DIR=C:\cse524\libxml2-2.6.14\libxml2-2.6.14\win32

CPP=cl.exe
OUTDIR=$(XSBDIR)\config\x86-pc-windows\bin
INTDIR=.

ALL : "$(OUTDIR)\$(MYPROGRAM).dll"

CLEAN :
	-@erase "$(INTDIR)\$(MYPROGRAM).obj"
	-@erase "$(INTDIR)\$(MYPROGRAM).dll"
	-@erase "$(INTDIR)\$(MYPROGRAM).exp"


CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "$(XSBDIR)\config\x86-pc-windows" \
		 /I "$(XSBDIR)\emu" /I "$(XSBDIR)\prolog_includes" /I "$(XSBDIR)\packages\xpath\cc"\
		/I "$(LIBXML2DIR)\include" \
		 /D "WIN32" /D "WIN_NT" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" \
		 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /c 
	

SOURCE=fetch_file.c xpathparser.c 
"$(INTDIR)\$(MYPROGRAM).obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
		advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib \
		odbc32.lib odbccp32.lib xsb.lib wsock32.lib libxml2.lib\
		/nologo /dll \
		/machine:I386 /out:"$(OUTDIR)\$(MYPROGRAM).dll" \
		/libpath:"$(XSBDIR)\config\x86-pc-windows\bin"	\
		/libpath:"$(LIBXML2DIR)\lib"	
LINK32_OBJS=  "$(INTDIR)\$(MYPROGRAM).obj"

"$(OUTDIR)\$(MYPROGRAM).dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<