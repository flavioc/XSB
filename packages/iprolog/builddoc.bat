set JAVA_BIN=\mc\XSBInc\XSBROOT\XSBENV\jdk\bin
if not exist htmldocs mkdir htmldocs
%JAVA_BIN%\javadoc -link http://java.sun.com/products/jdk/1.3/docs/api -public -d htmldocs com.declarativa.interprolog com.declarativa.interprolog.gui com.declarativa.interprolog.util com.xsb.interprolog
pause