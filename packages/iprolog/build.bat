set JAVA_BIN=~/Summer/j2sdk1.4.0_01/bin
set XSB_DIR=~/Summer/XSB
@echo off
set build_title=InterProlog Build
title InterProlog Build 
rem - Simple building with JDK, to use only if you do not use an IDE
rem - Note: Needs to specify all of the below files to compile otherwise won't run because .class files won't be included inside the jar file

title %build_title% - creating dir TempCompiled
@echo on
rmdir /S /Q tempCompiled
mkdir tempCompiled

@echo off
title %build_title% - compiling Java files
rem - Compile Java files
@echo on
%JAVA_BIN%\javac -d tempCompiled -classpath .;junit.jar;test;tempCompiled tests\com\declarativa\interprolog\*.java tests\com\xsb\interprolog\*.java com\declarativa\interprolog\*.java com\declarativa\interprolog\util\*.java com\declarativa\interprolog\gui\*.java com\declarativa\interprolog\examples\*.java com\xsb\interprolog\*.java


@echo off
title %build_title% - compiling .P files
rem - Compile .P files
@echo on
call compile_Ps.bat %XSB_DIR%

copy com\declarativa\interprolog\*.O tempCompiled\com\declarativa\interprolog
copy com\declarativa\interprolog\gui\*.O tempCompiled\com\declarativa\interprolog\gui
copy com\declarativa\interprolog\gui\images\* tempCompiled\com\declarativa\interprolog\gui\images

cd tempCompiled 

@echo off
title %build_title% - deleting interprolog.jar Jar file
rem - delete interprolog.jar Jar file
@echo on

del ..\interprolog.jar

@echo off
title %build_title% - creating interprolog.jar Jar file
rem - Create interprolog.jar file
@echo on
%JAVA_BIN%\jar cf ..\interprolog.jar *

cd ..
rmdir /S /Q tempCompiled

@echo off
title %build_title% - Finished
pause

