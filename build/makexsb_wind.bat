@echo off
REM   makexsb_wind.bat
REM   Script for compiling XSB under Windows using VC++

set XSBCONFIGdir=..\config\x86-pc-windows

IF NOT EXIST %XSBCONFIGdir%\saved.o MKDIR %XSBCONFIGdir%\saved.o
IF NOT EXIST %XSBCONFIGdir%\bin mkdir %XSBCONFIGdir%\bin
IF NOT EXIST %XSBCONFIGdir%\lib mkdir %XSBCONFIGdir%\lib

@cd ..\emu

REM Concatenate MSVC_mkfile.mak & MSVC.dep into emu\MSVC_mkfile.mak
@copy %XSBCONFIGdir%\MSVC_mkfile.mak+%XSBCONFIGdir%\MSVC.dep MSVC_mkfile.mak

@nmake /nologo /f "MSVC_mkfile.mak" %1 %2 %3 %4 %5 %6 %7

@del MSVC_mkfile.mak

@cd ..\gpp
@nmake /nologo /s /f "MSVC_mkfile.mak" %1 %2 %3 %4 %5 %6 %7

@cd ..\packages

@cd dbdrivers
@nmake /nologo /s /f NMakefile.mak
@cd ..

@cd sgml\cc
@nmake /nologo /f NMakefile.mak
@cd ..\..

@cd xpath\cc
@nmake /nologo /f NMakefile.mak
@cd ..\..

@cd ..\build

