package com.declarativa.interprolog;
import junit.framework.*;
import java.util.*;
import com.declarativa.interprolog.util.*;

public class SubprocessEngineTest extends PrologEngineTest {
	public SubprocessEngineTest(String name){
		super(name);
	}
	protected void setUp() throws java.lang.Exception{
		engine = new SubprocessEngine(AllTests.startCommand/*,true*/);
		thisID = engine.registerJavaObject(this);
		loadTestFile(); engine.waitUntilAvailable();
    }
	protected void tearDown() throws java.lang.Exception{
 		engine.shutdown();
    }
    
    
    public void testDeterministicGoal(){ // crashes JNI version (and this one too)
	super.testDeterministicGoal();
		/*
		try{
			engine.deterministicGoal("nowaythisisdefined");
			fail("should raise an IPException... with undefined predicate message");
		} catch (IPException e){
			// Too strict for the stream-based recognizers:
			// assertTrue("proper message in exception",e.toString().indexOf("Undefined")!=-1);
			assertTrue("No more listeners",((SubprocessEngine)engine).errorTrigger.numberListeners()==0);
		}*/
    }
    
    public void testManyEngines(){
		SubprocessEngine[] engines = new SubprocessEngine[2]; // 3 hangs on my Windows 98, at least 10 work on NT 4 Workstation
		for (int i=0;i<engines.length;i++) {
			//System.out.println("Creating engine "+i);
			engines[i] = new SubprocessEngine(AllTests.startCommand);
		}
		for (int i=0;i<engines.length;i++) 
			assertTrue(engines[i].isAvailable());
		for (int i=0;i<engines.length;i++) 
			engines[i].shutdown();
    }
	StringBuffer buffer;
	public void testOutputListening(){
		buffer = new StringBuffer();
		PrologOutputListener listener = new PrologOutputListener(){
			public void print(String s){
				buffer.append(s);
			}
		};
		assertEquals(0,((SubprocessEngine)engine).listeners.size());
		((SubprocessEngine)engine).addPrologOutputListener(listener);
		assertEquals(1,((SubprocessEngine)engine).listeners.size());
		
		engine.deterministicGoal("write('hello,'), write(' tester'), nl");
		try{Thread.sleep(100);} catch(Exception e){fail(e.toString());}
		assertTrue("printed something",buffer.toString().indexOf("hello, tester") != -1);
		
		assertTrue("available",engine.isAvailable());
		assertTrue("detecting regular and break prompts",((SubprocessEngine)engine).isDetectingPromptAndBreak());
		engine.command("thisIsUndefined");
		engine.waitUntilAvailable();
		
		//FAILING ALWAYS:
		//engine.sendAndFlushLn("bad term."); 
		//System.out.println("now waiting; buffer:"); System.out.println(buffer);
		//engine.sendAndFlushLn("true."); //This "fixes" the problem on Win98 but not on NT4...
		engine.waitUntilAvailable();
		
		((SubprocessEngine)engine).removePrologOutputListener(listener);
		assertEquals(0,((SubprocessEngine)engine).listeners.size());
	}
	
	/*
	public void testInterrupt(){ // crashes JNI version (and for now this one too)
		System.out.println("May be...");
		engine.command("import conget/2 from gensym");
		engine.command("repeat,fail");
		assertTrue("Busy",!engine.isAvailable());
		engine.setDebug(true);
		engine.interrupt();
		assertTrue("Free",engine.isAvailable()); 
		// FAILING SOMETIMES:
		System.out.println("this...");
		assertTrue("Not in break mode1", engine.deterministicGoal("conget('_$break_level', 0)"));
		
		engine.command("repeat,fail");
		assertTrue("Busy2",!engine.isAvailable());
		System.out.println("...flushes Windows sockets.");
		engine.interrupt();
		assertTrue("Free2",engine.isAvailable());
		assertTrue("Not in break mode2", engine.deterministicGoal("conget('_$break_level', 0)"));
		engine.setDebug(false);
	}*/
}
