# Make file for prolog2hilog.dll

XSBDIR = ..\..\..
MYPROGRAM = prolog2hilog


CPP = cl.exe
OUTDIR = .
INTDIR = .

ALL : "$(OUTDIR)\$(MYPROGRAM).dll"

CLEAN :
	-@erase *~
	-@erase .#*
	-@erase *.bak


CPP_PROJ = /nologo /MT /W3 /GX /O2 /I "$(XSBDIR)\config\x86-pc-windows" \
		 /I "$(XSBDIR)\emu" /I "$(XSBDIR)\prolog_includes" \
		 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" \
		 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /c 
	

SOURCE = $(MYPROGRAM).c

"$(INTDIR)\$(MYPROGRAM).obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)

LINK32 = link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
		advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib \
		odbc32.lib odbccp32.lib xsb.lib /nologo /dll \
		/machine:I386 /out:"$(OUTDIR)\$(MYPROGRAM).dll" \
		/libpath:"$(XSBDIR)\config\x86-pc-windows\bin" 
LINK32_OBJS =  "$(INTDIR)\$(MYPROGRAM).obj"

"$(OUTDIR)\$(MYPROGRAM).dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<
