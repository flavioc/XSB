package com.xsb.interprolog;
import com.declarativa.interprolog.*;
import com.declarativa.interprolog.util.*;

public class NativeEngineTest extends PrologEngineTest{
    public NativeEngineTest(String name){
		super(name);
		engine = new NativeEngine(AllTests.startDir/*,true*/);
		thisID = engine.registerJavaObject(this);
		loadTestFile();
   }
    protected void setUp() throws java.lang.Exception{
    }
    protected void tearDown() throws java.lang.Exception{
    }
    public void testNativeEngine(){ // to avoid multiple NativeEngines
       	testNewInterrupt();
    	testNumbers();
    	testPrototypeStuff();
    	testAutoTermModel();
    	testBuildTermModel();
    	testDeterministicGoal();
    	testDG2();
        testJavaMessage();
        testIPobjects();
        testXSBstuff();
        testLoops();
        testSomaN();
        testFibonaci();
       	testFactorial();
       	testMultipleThreads();
    }
	// Callback torture:
	// SUBPROCSS ENGINE:
	// Win98, Celeron 400 MHz: 220 ms/message
	// Win NT4 Workstation, Pentium 400 MHz: 441 mS/message
	// Win 2k, Pentium 400 MHz: 57 mS/message
	// NATIVE ENGINE, 19/9/2001:
	// Win 2k, Pentium 400 MHz: 9 mS/message
	
	// Bulk torture:
	// SUBPROCESS ENGINE:
	// Win98, Celeron 400 MHz: 4670 ms
	// Win NT4 Workstation, Pentium 400 MHz: 4607 mS (7128 bytes gone and returned / second)
	// Win 2k, Pentium 400 MHz: 7221 mS (4548 bytes gone and returned / second, 3072 handles)
	// NATIVE ENGINE, 19/9/2001:
	// Win 2k, Pentium 400 MHz: 3255 mS (10090 bytes gone and returned / second, 3072 handles)
	
	// Busy torture:
	// SUBPROCESS ENGINE:
	// Win98, Celeron 400 MHz: 203 ms/goal
	// Win NT4 Wokstation, Pentium 400 MHz: 402 mS/goal
	// Win 2k, Pentium 400 MHz: 36 mS/goal
	// NATIVE ENGINE, 19/9/2001:
	// Win 2k, Pentium 400 MHz: 7 mS/goal	
	boolean didInterrupt;
	void testNewInterrupt(){
		didInterrupt = false;
		Thread t = new Thread(){
			public void run(){
				try{
					//System.out.println("Calling Prolog endless loop");
					engine.deterministicGoal("repeat,fail"); 
					fail("should have thrown IPInterruptedException"); 
				}catch (IPInterruptedException e){
					didInterrupt = true;
				}
			}
		};
		//engine.setDebug(true);
		t.start();
		try {Thread.sleep(100);} catch(Exception e){}
		engine.interrupt();
		while (!didInterrupt) Thread.yield();
		//System.out.println("calling another goal...");
		Object s = engine.deterministicGoal("R=string(still_alive)","[R]")[0];
		assertEquals(s,"still_alive");
	}
	
	
	void testMultipleThreads(){
		DGClient client1 = new DGClient("Heavenly",10,10,10,10,50,50);
		DGClient client2 = new DGClient("Mount Snow",5,5,10,10,10,10);
		client1.start();
		// adding this second client hangs the Feb 27 version, but not the Mar 3 version :-):
		client2.start(); 
		while (client1.isAlive()||client2.isAlive()) Thread.yield();
	}
	
	public class DGClient extends Thread{
		int myID;
		long T1,T2,T3,T4,T5,T6;
		DGClient(String name,long T1, long T2, long T3, long T4, long T5, long T6){
			myID = engine.registerJavaObject(DGClient.this);
			this.T1=T1; this.T2=T2; this.T3=T3; this.T4=T4; this.T5=T5; this.T6=T6;
			setName(name);
		}
		public void run(){
			try{
				Thread.sleep(T1);
				//System.out.println(getName()+" calling first top dG...");
				assertTrue(engine.deterministicGoal("javaMessage("+myID+",method1)"));
				Thread.sleep(T2);
				//System.out.println(getName()+" calling second top dG...");
				assertTrue(engine.deterministicGoal("javaMessage("+myID+",method1)"));
				//System.out.println(getName()+" ended top dGs.");
			} catch (Exception e){
				throw new RuntimeException(e.toString());
			}
		}
		public void method1(){
			try{
				Thread.sleep(T3);
				//System.out.println(getName()+" calling 1st of second level dGs...");
				assertTrue(engine.deterministicGoal("javaMessage("+myID+",method2)"));
				Thread.sleep(T4);
				//System.out.println(getName()+" calling 2nd of second level dGs...");
				assertTrue(engine.deterministicGoal("javaMessage("+myID+",method2)"));
				//System.out.println(getName()+" ended second level dGs.");
			} catch (Exception e){
				throw new RuntimeException(e.toString());
			}
		}
		public void method2(){
			try{
				Thread.sleep(T5);
				//System.out.println(getName()+" calling 1st of third level dGs...");
				assertTrue(engine.deterministicGoal("true"));
				Thread.sleep(T6);
				//System.out.println(getName()+" calling 2nd of third level dGs...");
				assertTrue(engine.deterministicGoal("true"));
				//System.out.println(getName()+" ended third level dGs.");
			} catch (Exception e){
				throw new RuntimeException(e.toString());
			}
		}
	}
}
