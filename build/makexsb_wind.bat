REM   makexsb_wind.bat
REM   Script for compiling XSB under Windows using VC++

del ..\emu\configs\config.h
del ..\emu\debugs\debug.h

set XSBCONFIGdir=..\config\x86-pc-windows

copy %XSBCONFIGdir%\MS_VC_Mfile.mak  ..\emu
copy %XSBCONFIGdir%\config.h  ..\emu\configs
copy %XSBCONFIGdir%\debug.h   ..\emu\debugs

mkdir %XSBCONFIGdir%\saved.o
mkdir %XSBCONFIGdir%\bin
mkdir %XSBCONFIGdir%\lib


@cd ..\emu

nmake /f "MS_VC_Mfile.mak" %1 %2 %3 %4 %5 %6 %7

@cd ..\gpp

nmake /f "MS_VC_Mfile.mak"

@cd ..\build
