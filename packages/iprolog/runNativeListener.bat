JAVA_BIN=/home/miti/Summer/j2sdk1.4.0_01/bin 
XSB_DIR=/home/miti/Summer/XSB 

$JAVA_BIN/java -classpath $CLASSPATH .:interprolog.jar com.xsb.interprolog.NativeEngineWindow $XSB_DIR
pause
