@echo off

REM  Arg 1 is a path to prolog or `clean'
REM  In the latter case, cleans the distribution

@set PROLOG=%1

@echo.
if "%PROLOG%" == "" echo Usage:  makeflora full-path-to-prolog
if "%PROLOG%" == "" goto end

if "%PROLOG%" == "clean" nmake /f NMakefile.mak clean
if "%PROLOG%" == "clean" goto end

if not exist .prolog_path_wind  echo. > .prolog_path_wind

REM  Generate the files that contain the Prolog & Flora installation directories
REM  Generates runflora.bat file that can be used to run flora

if exist runflora.bat  del runflora.bat
call %PROLOG% -e "[flrconfig]. halt."

cd p2h
nmake /f NMakefile.mak
cd ..
nmake /f NMakefile.mak

:end
@echo.

REM Local Variables:
REM coding-system-for-write:  iso-2022-7bit-dos
REM End:
