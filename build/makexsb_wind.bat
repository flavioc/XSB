REM   makexsb_wind.bat
REM   Script for compiling XSB under Windows using VC++

del ..\emu\configs\config.h
del ..\emu\debugs\debug.h

set XSBCONFIGdir=..\config\x86-pc-windows

copy %XSBCONFIGdir%\config.h  ..\emu\configs
copy %XSBCONFIGdir%\debug.h   ..\emu\debugs

mkdir %XSBCONFIGdir%\saved.o
mkdir %XSBCONFIGdir%\bin
mkdir %XSBCONFIGdir%\lib


@cd ..\emu

# Concatenate config\MSVC_Makefile.mak and MSVC.dep into emu\MSVC_Makefile.mak
copy %XSBCONFIGdir%\MSVC_Makefile.mak+%XSBCONFIGdir%\MSVC.dep MSVC_Makefile.mak

nmake /f "MSVC_Makefile.mak" %1 %2 %3 %4 %5 %6 %7

del MSVC_Makefile.mak

@cd ..\gpp

nmake /f "MSVC_Makefile.mak"

@cd ..\build
