@echo off

REM  Call:
REM     makeflora [-c] full-path-to-prolog
REM     makeflora clean

REM   makeflora -c full-path-to-prolog means use the C compiler
REM                  to recompile the directory p2h\ - developer's option

REM  NOTE: DOS batch language is very brittle. For instance, replacing
REM        %1 with %ARG%, where set ARG=%1 will not work if
REM        full-path-to-prolog has a file extension, e.g., \xsb\bin\wxsb.bat

if "%1" == "-c"  set PROLOG=%2 -s -m 40000 -c 4000
if not "%1" == "-c"  set PROLOG=%1 -s -m 40000 -c 4000

@echo.
if "%PROLOG%" == "" echo Usage:  makeflora [-c] full-path-to-prolog
if "%PROLOG%" == "" goto end

if "%1" == "clean" nmake /f NMakefile.mak clean
if "%1" == "clean" goto end

if not exist .prolog_path_wind  echo. > .prolog_path_wind

REM  Generate the files that contain the Prolog & Flora installation directories
REM  Generates runflora.bat file that can be used to run flora

if exist runflora.bat  del runflora.bat
call %PROLOG% -e "[flrconfig]. halt."

cd p2h
del prolog2hilog.dll prolog2hilog.lib prolog2hilog.def prolog2hilog.exp
del prolog2hilog.obj prolog2hilog.a prolog2hilog.o

if "%1" == "-c" nmake /f NMakefile.mak
if not "%1" == "-c" copy windows\*

cd ..
nmake /f NMakefile.mak

:end
@echo.

REM Local Variables:
REM coding-system-for-write:  iso-2022-7bit-dos
REM End:
