package com.declarativa.interprolog; 
import com.xsb.interprolog.*;
import junit.framework.*;
import junit.extensions.*;
public class TestNativeEngine{
	// JUnit reloads all classes including this one, so we can not get this easily through the command line:
	public static String startDir = "/home/lmc/Summer02/tarfiles/Aug19/XSB";
	public static String startCommand = startDir + "/config/i686-pc-linux-gnu/bin/xsb";
	public static void main(String args[]) { 
		/*startCommand=args[0];
		if (args.length>1) System.out.println("Invoke tests with a single argument");
    	else*/ 
	    System.out.println("start command : "+startCommand);
	    junit.swingui.TestRunner.run(TestNativeEngine.class);
	    
	}
	
	public static Test suite(){
		TestSuite suite= new TestSuite("Testing InterProlog"); 	
		suite.addTest(new NativeEngineTest("testNativeEngine"));
		return suite;
	}
}

