package com.declarativa.interprolog;
import junit.framework.*;
import java.util.*;
import com.declarativa.interprolog.util.*;
import javax.swing.*;

public abstract class PrologEngineTest extends TestCase{
	public PrologEngine engine=null;
	protected int thisID;
	public PrologEngineTest(String name){
		super(name);
	}
	//defined in subclasses:
	//protected abstract void setUp() throws java.lang.Exception;
	//protected abstract void tearDown() throws java.lang.Exception;
	
	public void loadTestFile(){
		engine.consultFromPackage("tests.P",PrologEngineTest.class);
	}
	public static class NumberTypes implements java.io.Serializable{
		byte b;
		short s;
		int i;
		float f;
		public NumberTypes(byte b,short s,int i, float f){
			this.b=b; this.s=s; this.i=i; this.f=f;
		}
		public static ObjectExamplePair example(){
			return new ObjectExamplePair(
				new NumberTypes(Byte.MIN_VALUE,Short.MIN_VALUE,PrologEngine.MIN_INT_VALUE,SMALL_FLOAT_VALUE),
				new NumberTypes(Byte.MAX_VALUE,Short.MAX_VALUE,PrologEngine.MAX_INT_VALUE,LARGE_FLOAT_VALUE)
			);
		}
		public String toString(){
			return "b=="+b+",s=="+s+",i=="+i+",f=="+f;
		}
		public boolean equals(Object o){
			if (!(o instanceof NumberTypes)) return false;
			NumberTypes io = (NumberTypes)o;
			return io.b==b && io.s==s && io.i==i && io.f==f;
		}
	}
	// floats not very precise on XSB Prolog, let's use reasonable values:
	static final float SMALL_FLOAT_VALUE = (float)-3.14159;
	static final float LARGE_FLOAT_VALUE = (float)817.3E4;
	public void testNumbers(){
		ObjectExamplePair[] examples = {NumberTypes.example()};
		engine.teachMoreObjects(examples);
		engine.waitUntilIdle(); 
		
		String g = "ipObjectSpec('com.declarativa.interprolog.PrologEngineTest$NumberTypes',Min,[";
		g += Byte.MIN_VALUE+","+SMALL_FLOAT_VALUE+","+PrologEngine.MIN_INT_VALUE+","+Short.MIN_VALUE+"],_), ";
		g += "ipObjectSpec('com.declarativa.interprolog.PrologEngineTest$NumberTypes',Max,[";
		g += Byte.MAX_VALUE+","+LARGE_FLOAT_VALUE+","+PrologEngine.MAX_INT_VALUE+","+Short.MAX_VALUE+"],_)";
		NumberTypes MIN = new NumberTypes(Byte.MIN_VALUE,Short.MIN_VALUE,PrologEngine.MIN_INT_VALUE,SMALL_FLOAT_VALUE);
		NumberTypes MAX = new NumberTypes(Byte.MAX_VALUE,Short.MAX_VALUE,PrologEngine.MAX_INT_VALUE,LARGE_FLOAT_VALUE);
		Object [] args = {MIN,MAX};
		assertTrue("Numbers well sent",engine.deterministicGoal(g,"[Min,Max]",args));

		Object [] bindings = engine.deterministicGoal(g,"[Min,Max]");
		assertEquals("MIN arrived well",MIN,bindings[0]);
		assertEquals("MAX arrived well",MAX,bindings[1]);
	}
	public static class MyClass implements java.io.Serializable{
		int one,two;
		public MyClass(){one=0;two=0;}
	}
	public void testPrototypeStuff(){
		engine.teachOneObject(new MyClass());
		String g = "ipObjectSpec('com.declarativa.interprolog.PrologEngineTest$MyClass',[one=1,two=2],Obj)";
		g += ", ipObjectSpec('com.declarativa.interprolog.PrologEngineTest$MyClass',[one=One,two=Two],Obj), Three is One+Two, ";
		g += "ipObjectSpec('java.lang.Integer',[value=Three],Integer)";
		Object[] bindings = engine.deterministicGoal(g,"[Obj,Integer]");
		MyClass result0 = (MyClass)bindings[0];
		assertTrue( 1==result0.one );assertTrue( 2==result0.two );
		assertEquals(new Integer(3),bindings[1]);
	}
	public void testAutoTermModel(){
		Object[] bindings = engine.deterministicGoal("X=1,Y=hello(complex(term))",null);
		assertEquals(bindings.length,1);
		assertTrue(bindings[0] instanceof TermModel);
	}
    public void testBuildTermModel(){
    	TermModel t1 = (TermModel)engine.deterministicGoal("buildTermModel(a(b,c(1)),Model)","[Model]")[0];
    	assertEquals("a(b,c(1))",t1.toString());
    	assertEquals("a(_,_)",t1.getTemplate());
    	assertEquals(2,t1.getChildCount());
    	
    	TermModel t2 = (TermModel)engine.deterministicGoal("buildTermModel(a(X,c(X)),Model)","[Model]")[0];
    	TermModel t2child = (TermModel)t2.getChild(0);
    	assertTrue("Child is var",t2child.isVar());
    	Object t3 = t2.clone();
    	assertEquals(t2.toString(),t3.toString());
    	t2.assignToVar((VariableNode) t2child.node, "someX");
    	assertEquals("a(someX,c(someX))",t2.toString());
    	
    	Object[] passed = {t2,t3};
    	String g = "recoverTermModel(Model3,T3), arg(1,T3,X), arg(2,T3,c(XX)), XX==X, recoverTermModel(Model2,T2), T2=T3, ";
    	g += "arg(1,T2,someX), functor(T2,F,N), ipObjectSpec('java.lang.Integer',Integer,[N],_)";
    	Object[] bindings = engine.deterministicGoal(g, "[Model2,Model3]", passed,"[string(F),Integer]");
    	assertTrue(bindings!=null);
    	assertEquals("a",bindings[0]);
    	assertEquals(new Integer(2),bindings[1]);
    }
	public void testDeterministicGoal(){
		engine.command("import append/3,length/2 from basics");
		engine.waitUntilAvailable(); 
		Object[] objects = {new Float(16.25),new Float(0.0), new Float(15.5)};
		Object[] bindings = engine.deterministicGoal(
			"append([97,98],[99,100],L), length(L,N), ipObjectSpec('java.lang.Integer',Integer,[N],_), name(A,L),"+
			"assert(foofoo(Objects))", 
			"Objects",
			objects,
			"[Integer,string(A)]"
			);
		assertTrue("Got a result",bindings!=null);
		assertEquals("First result",bindings[0],new Integer(4));
		assertEquals("Second result",bindings[1],"abcd");
		Object[] floats = engine.deterministicGoal("foofoo([F1,F2,F3])","[F1,F2,F3]");
		assertTrue("succeeded",floats!=null);
		assertEquals(floats[0],new Float(16.25));
		assertEquals(floats[1],new Float(0.0));
		assertEquals("Third float OK",floats[2],new Float(15.5));
		assertTrue(engine.deterministicGoal("true"));
	
		try{
			engine.deterministicGoal("foofoo(BadThingCausingGrammarFailure)","[BadThingCausingGrammarFailure]");
			fail("should raise an IPException due to grammar failure");
		} catch (IPException e){
			assertTrue("IPException should complain about spec. of result bindings:"+e,e.toString().indexOf("bindings")!=-1);
		}
		
		try{
			engine.deterministicGoal("bad goal");
			fail("should raise an IPException with syntax error");
		} catch (IPException e){
			assertTrue("IPException should denote syntax error:"+e,e.toString().indexOf("Syntax")!=-1);
		}
		assertTrue("Engine ready2",engine.isIdle());
		assertTrue("Engine working",engine.deterministicGoal("true"));
		try{
			engine.deterministicGoal("true","BadResultsList");
			fail("should raise an IPException complaining about lack of a Prolog list");
		} catch (IPException e){}
		assertTrue("Engine ready1",engine.isIdle());
		java.util.Vector v = new java.util.Vector();
		v.addElement(objects); v.addElement("Hello there");
		for(int i=0;i<30;i++){
			Vector vv=new Vector();
			for (int j=0;j<100;j++)
				vv.addElement(new Integer(j));
			v.addElement(vv);
		}
		assertTrue(engine.deterministicGoal("import streamContents/4 from interprolog"));
        engine.waitUntilAvailable();
        engine.command("import ground/1, length/2, append/3 from basics");
		Object [] toSerialize = {v};
		String g = "streamContents([Object],handles(NH,_),Bytes,[]), length(Bytes,NB), ";
		g += "ipObjectSpec('java.lang.Integer',IntegerNH,[NH],_), ipObjectSpec('java.lang.Integer',IntegerNB,[NB],_)";
		long tortureStart= System.currentTimeMillis();
		Object [] yetanother = engine.deterministicGoal(g,"[Object]",toSerialize,"[Object,IntegerNH,IntegerNB]");
		long duration = System.currentTimeMillis()-tortureStart;
		int nbytes = ((Integer)yetanother[2]).intValue();
		System.out.println("Bulk torture took "+(duration)+" mS ("+(1000*nbytes/duration)+" bytes gone and returned / second)");
		// Win98, Celeron 400 MHz: 4670 ms
		// Win NT4 Workstation, Pentium 400 MHz: 4607 mS (7128 bytes gone and returned / second)
		// Win 2k, Pentium 400 MHz: 7221 mS (4548 bytes gone and returned / second)
		/*
		System.out.println(yetanother[0]);
		System.out.println(yetanother[1]);
		System.out.println(yetanother[2]);*/
		// If Vector's internals change in subsequent Java releases review these values by printing them above:
		assertEquals(new Integer(3072),yetanother[1]); 
		assertEquals(new Integer(32843),yetanother[2]);
		assertTrue(yetanother[0] instanceof java.util.Vector);
		
		tortureStart= System.currentTimeMillis();
		int ngoals=3;
		for (int i=0;i<ngoals;i++)
			assertTrue(engine.deterministicGoal("true"));
		System.out.println("Busy torture took "+(System.currentTimeMillis()-tortureStart)/ngoals+ " mS/goal");
		
		
		// Win98, Celeron 400 MHz: 203 ms/goal
		// Win NT4 Wokstation, Pentium 400 MHz: 402 mS/goal
		// Win 2k, Pentium 400 MHz: 36 mS/goal
	}
	/* Java programmers never need this; use instead findall or findNsolutions on the Prolog side!
	might be useful for Prolog engines with threads...
	public void testNonDeterministicGoal(){
		engine.deterministicGoal("import member/2 from basics");
		engine.goal("member(X,[1,2,3,4,5]), ipObjectSpec('java.lang.Integer',I,[X],_)","[I]");
		Object[] bindings; int i=1;
		while ( (bindings=engine.nextSolution())!=null) {
			assertEquals(new Integer(i++),bindings[0]);
		}
		assertTrue(i==6);
		try{ 
			engine.nextSolution();
			fail("Should throw IPException");
		} catch (IPException e){assertTrue(!(e.toString.indexOf("...")!=-1));}
	}*/
	
	/* This fails, the gramamr needs to be completed to cover blockdata, I think
	public void testSwingSerialization(){
		engine.setDebug(true);
		JFrame window1 = new JFrame("hello");
		Object window2 = engine.deterministicGoal("true","[W]",new Object[]{window1},"[W]")[0];
		assertTrue(window2 instanceof JFrame);
	}*/
	
	public void testDG2(){
		String G = "(X=a;X=b)";
		String T = "X";
		String GG = "findall(TM, ("+G+",buildTermModel("+T+",TM)), L), ipObjectSpec('ArrayOfObject',L,LM)";
		Object[] solutions = (Object[])engine.deterministicGoal(GG,"[LM]")[0];
		assertEquals(2,solutions.length);
		assertEquals("a",solutions[0].toString());
	}
	public int luckyNumber(){return 13;}
	public void testJavaMessage(){
		assertEquals(engine.registerJavaObject(this),thisID);
		String callback = "javaMessage("+thisID+",R,luckyNumber), ipObjectSpec('java.lang.Integer',R,[13],_)";
		assertTrue("Succeeded 1st",engine.deterministicGoal(callback));
		String clauseAsserts = "assert((tortureJM(0) :- !)), ";
		clauseAsserts += "assert((tortureJM(N) :- NN is N-1, "+callback+", tortureJM(NN)))";
		assertTrue("Succeeded 2nd",engine.deterministicGoal(clauseAsserts));
		long tortureStart= System.currentTimeMillis();
		int ngoals=10;
		assertTrue("Succeeded torture",engine.deterministicGoal("tortureJM("+ngoals+")"));		
		System.out.println("Callback torture took "+(System.currentTimeMillis()-tortureStart)/ngoals+" mS/message");
		// Win98, Celeron 400 MHz: 220 ms/message
		// Win NT4 Workstation, Pentium 400 MHz: 441 mS/message
		// Win 2k, Pentium 400 MHz: 57 mS/message
	}
	public void testIPobjects(){
		assertTrue(engine.deterministicGoal("import length/2 from basics"));
		String goal = "findall(foo,ipObjectSpec(_,_,_,_), L), length(L,_N), ";
		goal += "findall(foo,ipObjectTemplate(_,_,_,_,_), LL), length(LL,_N), ";
		goal += "ipObjectSpec('java.lang.Integer',Integer,[_N],_) ";
		Integer result = (Integer)engine.deterministicGoal(goal,"[Integer]")[0];
		assertTrue(result.intValue()>20);
	}
	public static class ConfigurationItem implements java.io.Serializable{
		String feature,value;
		public String toString(){
			return "FEATURE "+feature+" HAS VALUE "+value;
		}
	}
	public void testXSBstuff(){
		// The following is too complicated for the assertion presently tested, but ready for deeper stuff:
		engine.teachOneObject(new ConfigurationItem());
		String g = "F=install_dir, findall(Obj, (xsb_configuration(F,V), ";
		g += "ipObjectSpec('com.declarativa.interprolog.PrologEngineTest$ConfigurationItem',[feature=string(F),value=string(V)],Obj)";
		g += "),L), ipObjectSpec('ArrayOfObject',L,Array)";
		Object[] items = (Object[])engine.deterministicGoal(g,"[Array]")[0];
		ConfigurationItem item = (ConfigurationItem) items[0];
		assertTrue(AllTests.startCommand.indexOf(item.value)!=-1);
	}
	public static class Loop implements java.io.Serializable{
		Loop next;
	}
	public void testLoops(){
		engine.teachOneObject(new Loop());
		Loop L = new Loop();
		L.next=L;
		assertTrue(engine.deterministicGoal("true","[L]",new Object[]{L})); // but if you replace true by write(L)..
	}
     public int somaN(int n) {
        Object[] bindings = engine.deterministicGoal (
            "somaN("+n+",X), ipObjectSpec('java.lang.Integer',Spec,[X],_)", "[Spec]"
        );
        return ((Integer)bindings[0]).intValue(); 
    }
    
    public void testSomaN(){
		engine.waitUntilIdle();
       	engine.command("ipObjectSpec('InvisibleObject',_T,["+thisID+"],_),"+
                        "assert(ipSomaN(_T))"); 
 		engine.waitUntilIdle();
       Object[] bindings = engine.deterministicGoal (
            "somaN(10,X), ipObjectSpec('java.lang.Integer',Spec,[X],_)", "[Spec]"
            );
        Integer result = (Integer)bindings[0];
        assertTrue ("Got a result",bindings!=null);
        engine.progressMessage("result: "+bindings[0]);
        assertEquals ("First result",result, new Integer (55));
    }
    
    public int fibonaci(int n) {
        Object[] bindings = engine.deterministicGoal (
            "fib("+n+",X), ipObjectSpec('java.lang.Integer',Spec,[X],_)", "[Spec]"
        );
        return ((Integer)bindings[0]).intValue(); 
    }
    public void testFibonaci(){
        engine.command("ipObjectSpec('InvisibleObject',_T,["+thisID+"],_),"+
                        "assert(ipFibonaci(_T))"); 
        engine.waitUntilIdle();
        Object[] bindings = engine.deterministicGoal (
            "fib(10,X), ipObjectSpec('java.lang.Integer',Spec,[X],_)", "[Spec]"
            );
        Integer result = (Integer)bindings[0];
        assertTrue ("Got a result",bindings!=null);
        engine.progressMessage("result: "+bindings[0]);
        assertEquals ("First result",result, new Integer (89));
    }
    
    public int factorial(int n) {
        Object[] bindings = engine.deterministicGoal (
            "fac("+n+",X), ipObjectSpec('java.lang.Integer',Spec,[X],_)", "[Spec]"
        );
        return ((Integer)bindings[0]).intValue(); 
    }
    
    public void testFactorial(){
        engine.command("ipObjectSpec('InvisibleObject',_T,["+thisID+"],_),"+
                        "assert(ipFactorial(_T))"); 
        engine.waitUntilIdle();
        Object[] bindings = engine.deterministicGoal (
            "fac(7,X), ipObjectSpec('java.lang.Integer',Spec,[X],_)", "[Spec]"
            );
        Integer result = (Integer)bindings[0];
        assertTrue ("Got a result",bindings!=null);
        engine.progressMessage("result: "+bindings[0]);
        assertEquals ("First result",result, new Integer (5040));
    } 
    
}
