package com.xsb.interprolog;
import java.util.*;
import com.declarativa.interprolog.*;
import com.declarativa.interprolog.util.*;
import javax.swing.*;

public class MyNETest{
	public PrologEngine engine=null;
	public static String startDir = "/home/miti/Summer/XSB";
    //	public static String startCommand = startDir + "/config/i686-pc-linux-gnu/bin/xsb";
    public static String startCommand = startDir;
	

	public MyNETest(){
	  engine = new NativeEngine(startCommand,true);	
	  System.out.println("Started the engine");  
      }

	public void runTest() {
	  System.out.println("CHANGED ");

	  // 1. JAVA   --------------->>>>  PROLOG (simple built-in predicates)
        // Ask Prolog to execute a simple goal like writeln 
	  engine.deterministicGoal("writeln('1.miti Adiecha')");
	  System.out.println("Step 1 ");

        // 2. JAVA ----------->>> PROLOG  ------------->>> JAVA
	  // Ask Prolodg to call java and display a string	
	  engine.deterministicGoal("javaMessage('java.lang.System'-out, println(string('2. Miti Adiecha')))");
	  System.out.println("Step 2 ");
	  /*
        // 3. JAVA ----------->>> PROLOG (simple/user written predicates)
	  // Ask prolog to consult a file
	  String ff = "\'pFile.P\'";
        engine.deterministicGoal("consult("+ff+")");
	  engine.deterministicGoal("myGoal");
	  String echoString = "argString";
	  engine.deterministicGoal("echo("+echoString+")");
	  System.out.println("Step 3 ");	
	  
	//	Added to test yetAnother
	System.out.println("Test yetAnother");
	Object[] objects = {new Float(16.25),new Float(0.0), new Float(15.5)};

	java.util.Vector v = new java.util.Vector();
	v.addElement(objects); v.addElement("Hello there");
	for(int i=0;i<30;i++){
			Vector vv=new Vector();
			for (int j=0;j<100;j++)
				vv.addElement(new Integer(j));
			v.addElement(vv);
		}
	Object [] toSerialize = {v};
	String g = "streamContents([Object],handles(NH,_),Bytes,[]), length(Bytes,NB), ";
	g += "ipObjectSpec('java.lang.Integer',IntegerNH,[NH],_), ipObjectSpec('java.lang.Integer',IntegerNB,[NB],_)";

	Object [] yetanother = engine.deterministicGoal(g,"[Object]",toSerialize,"[Object,IntegerNH,IntegerNB]");
	System.out.println("Test yetAnother....STOP");
	*/
	
/*

	  // 4. 
	  // Open a window from PROLOG
	  JFrame myFrame = new JFrame("Example 1");
	  JTextField text = new JTextField(15);
	  myFrame.getContentPane().add(text);
	  myFrame.setSize(200,100);
	
	  Object ref = engine.makeInvisible(text);
	  String result = null ;
	  engine.deterministicGoal("showWindow(Text)", "[Text]", new Object[]{ref}, result);
		
	  myFrame.show();	
  */  
     
	}

	public static void main(String[] args) {
	  MyNETest myTest = new MyNETest();
	  myTest.runTest();
	}
	
}
