set JAVA_BIN=\mc\XSBInc\XSBROOT\XSBENV\jdk\bin
set XSB_DIR=\mc\XSBInc\XSBROOT\XSBENV\xsb
%JAVA_BIN%\java -Xdebug -Xrunjdwp:transport=dt_shmem,address=jdbconn,server=y,suspend=n -classpath %CLASSPATH%;interprolog.jar com.declarativa.interprolog.gui.SubprocessEngineWindow -debug %XSB_DIR%\config\x86-pc-windows\bin\xsb.exe
pause