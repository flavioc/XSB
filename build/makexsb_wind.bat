@echo off
REM   makexsb_wind.bat
REM   Script for compiling XSB under Windows using VC++

set XSBCONFIGdir=..\config\x86-pc-windows

mkdir %XSBCONFIGdir%\saved.o
mkdir %XSBCONFIGdir%\bin
mkdir %XSBCONFIGdir%\lib


@cd ..\emu

REM Concatenate MSVC_mkfile.mak & MSVC.dep into emu\MSVC_mkfile.mak
copy %XSBCONFIGdir%\MSVC_mkfile.mak+%XSBCONFIGdir%\MSVC.dep MSVC_mkfile.mak

nmake /f "MSVC_mkfile.mak" %1 %2 %3 %4 %5 %6 %7

del MSVC_mkfile.mak

@cd ..\gpp
nmake /f "MSVC_mkfile.mak" %1 %2 %3 %4 %5 %6 %7

@cd ..\packages\dbdrivers
nmake /f NMakefile.mak

@cd ..\..\build


REM Local Variables:
REM coding-system-for-write:  iso-2022-7bit-dos
REM End:
