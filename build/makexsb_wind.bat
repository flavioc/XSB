REM   makexsb_wind.bat
REM   Script for compiling XSB under Windows using VC++

del ..\emu\configs\config.h
del ..\emu\debugs\debug.h
copy ..\config\x86-pc-windows\MS_VC_Mfile.mak  ..\emu
copy ..\config\x86-pc-windows\config.h  ..\emu\configs
copy ..\config\x86-pc-windows\debug.h   ..\emu\debugs


@cd ..\emu

nmake /f "MS_VC_Mfile.mak" %1 %2 %3 %4 %5 %6 %7

@cd ..\build
