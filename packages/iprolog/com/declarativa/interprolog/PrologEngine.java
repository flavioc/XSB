/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/

package com.declarativa.interprolog;
import com.declarativa.interprolog.util.*;
import java.io.*;
import java.net.*;
import java.util.Vector;
import java.lang.reflect.*;

/** The heart of InterProlog; a PrologEngine represents a Prolog abstract machine instance. 
This is an abstract class; you should use it just to declare variables, but must instantiate only a subclass.
*/
public abstract class PrologEngine{
    protected static int numberOfInstances = 0;
    /** InterProlog version */
    public final static String version = "2.0b1";
    /** File path to Prolog machine */
    public String startPrologCommand;
    protected long startTime;
    /** Convenience for newline */
    public final static String nl = System.getProperty("line.separator");
    /** Temporary directory where Prolog files are unjared to. It is automatically declared as a Prolog library_directory */
    protected File tempDirectory; 
    
    /** Table of object references that can be referred from Prolog without being serialized */
    protected ObjectRegistry knownObjects;
    protected boolean shutingDown = false;
    /** Prolog is handling an interrupt */
    public boolean interrupting = false;
    protected boolean debug=false;
    protected boolean topGoalHasStarted=false;
    /** goal counter */
    protected int goalTimestamp;
    
    /** Prolog Goals whose execution has not yet finished or whose results have not yet been returned to their Java clients */
    Vector goalsToExecute;
    /** javaMessage requests that have started execution and whose results have not yet been returned to Prolog. */
    Vector messagesExecuting;
    
    /** "Constant" used for some special javaMessage handling */
	final Method getRealJavaObjectMethod;
    /** Name of first message sent to Java*/
	public final String firstJavaMessageName = "firstJavaMessage";
	/** Maximum integer value. 28 bits is all we got on XSB Prolog*/
	/* unfortunately it is pointless to intercept writeInt
	in ObjectOutputStream, because (1) the method is onvoked in several situations, not just to serialize
	int values, and (2) some classes may serialize in very weird ways; so in general it is not possible 
	to detect int values beyond these boundaries */
	public static final int MAX_INT_VALUE = 134217727;
	/** Minimum integer value.*/
	public static final int MIN_INT_VALUE = -134217728;

	/** Create a Prolog executor, possibly spawning it in a different process or loading it into memory, depending on the implementation by our subclass.
	@param startPrologCommand File path to Prolog machine, see subclass docs for precise semantics
	@param debug if true, print progress messages to aid debugging
	*/
	public PrologEngine(String startPrologCommand, boolean debug){	    
	        startTime= System.currentTimeMillis();
		if (startPrologCommand==null) 
			throw new IPException("PrologEngine with null startPrologCommand");
		setDebug(debug);
		this.startPrologCommand = startPrologCommand;
   		numberOfInstances++;
		makeTempDirectory();
		knownObjects = new ObjectRegistry();
		goalsToExecute = new Vector();
		messagesExecuting = new Vector();

        try{ getRealJavaObjectMethod = findMethod (getClass(),"getRealJavaObject",new Class[]{Object.class});} 
        catch(Exception ex){throw new IPException("could not find special getRealJavaObject method:"+ex);}
	}
		
	/** Release Prolog engine resources, making it unusable*/
	public abstract void shutdown();

	/** This implementation returns false */
	protected boolean isShutingDown(){
		return false;
	}
	
	void makeTempDirectory(){
		if (tempDirectory!=null)
                    throw new IPException("Inconsistency in makeTempDirectory");
		try{
			File dummyFile = File.createTempFile("IP_","");
			if (!dummyFile.delete())
                            throw new IPException("Could not delete dummy file");
			tempDirectory = new File(dummyFile.getAbsolutePath());
			if (!tempDirectory.mkdir())
                            throw new IPException("Could not create temporary directory");
			tempDirectory.deleteOnExit();
		} catch (IOException e){
			throw new IPException("Problems creating temporary directory:"+e);
		}
	}
	
	/** Returns the directory containing the jar with the engine class */
	public File getJarDirectory(){
		return getJarDirectory(getClass());
	}
	
	/** Returns the directory containing the jar with the given class */
	public static File getJarDirectory(Class aClass){
	    String classPath=aClass.getName().replace('.','/') + ".class";
		URL u = aClass.getClassLoader().getResource(classPath);
		if (!u.getProtocol().equals("jar")) return null;
		String s = u.getFile();
	    if (!s.startsWith("file:/"))
		throw new IPException("Jar file is not in the local file system");
		int end = s.indexOf("!");
		if (end==-1) throw new IPException("Bad jar URL");
	    // MK changed 6 to 5!!!
	    String path = s.substring(5,end);
	    if (!path.endsWith(".jar"))
		throw new IPException("Jar file name does not end with .jar");
		path=path.replace('/',File.separatorChar);
		return new File(path).getParentFile();
	}
	
	File copyToTemp(String filename,Object requester){
		File tempFile = null;
		Class rc;
		if (requester instanceof Class) rc = (Class)requester;
		else rc = requester.getClass();
		// rc.getPackage() null when using JUnit's class loader...
		// String packageName = rc.getPackage().getName();
		String className = rc.getName();
		String packageName = className.substring(0,className.lastIndexOf('.'));
		String path = packageName.replace('.','/' /*File.separatorChar*/);

		if (filename.indexOf('.')==-1) throw new IPException("File name missing extension");
		String suffix = filename.substring(filename.lastIndexOf('.'));
		String rootName = filename.substring(0,filename.lastIndexOf('.'));
		try {
		    		    
		    String resourceName = path+/*File.separatorChar*/'/'+filename;
		    InputStream resourceStream =
		    	rc.getClassLoader().getResourceAsStream(resourceName);
		    if (resourceStream==null) throw new IPException("Resource not found");
		    
		    //tempFile = File.createTempFile("IP_",suffix /*,directory*/);
			tempFile = new File(tempDirectory,filename);
			tempFile.deleteOnExit();
			
			FileOutputStream fos = new FileOutputStream(tempFile);
			byte[] buffer = new byte[512]; int len;
			while ((len = resourceStream.read(buffer, 0, buffer.length)) != -1) 
			    fos.write(buffer, 0, len);
			fos.close(); resourceStream.close();
		} catch (IOException e){
		    throw new IPException("I/O problem obtaining "+filename+": "+e);
		}
		return tempFile;
	}
	
	/** 
         * Extracts a Prolog file from the jar file or directory where the requester's class came from,
         * and asks the background Prolog process to consult it. You should use this method only after your
         * program is stable.
         * The Prolog file is extracted to a temporary file, and automatically deleted on exiting the application.
         * @param filename The Prolog file name, including suffix; if absent ".P" is appended
         * @param requester Defines where the Prolog file resides
         * @see #consultRelative(String, Object)
         * @see #load_dynRelative(String, Object)
         */
	
	public void consultFromPackage(String filename,Object requester){
		if (filename.indexOf('.')==-1) filename=filename+".P";
		String path = copyToTemp(filename,requester).getPath();
		progressMessage("consultFromPackage:"+path);
		
		if (!command("reconsult('"+path+"')"))
		throw new IPException("Problem consulting from package archive");
	}
	
	/** Consults a Prolog file from the directory where the requester's class would come from if it
	did not come from a jar file. Adds that directory to the library_directory relation, so modules can be found there
@param filename The Prolog file name, including suffix; if absent ".P" is appended
@param requester Defines where the Prolog file resides
	*/
	public void consultRelative(String filename,Object requester){
		operationRelative("reconsult", filename, requester);
	}
	
	public void load_dynRelative(String filename,Object requester){
		operationRelative("load_dyn", filename, requester);
	}
	
	protected void operationRelative(String operation,String filename,Object requester){
		Class rc;
		if (requester instanceof Class) rc = (Class)requester;
		else rc = requester.getClass();
		String path = rc.getPackage().getName().replace('.',File.separatorChar);
		path=getJarDirectory().getPath()+File.separatorChar+path; 
		// make this insensitive to Prolog's current directory
		//if (filename.indexOf('.')==-1) filename=filename+".P";
		filename = path+File.separatorChar+filename;
		String g = "((library_directory('"+path+"')->true;assert(library_directory('"+path+"'))), "+
			operation+"('"+filename+"'))";
		if (!command(g))
		throw new IPException("Problem in operationRelative");
	}
			

	
	/** Interrupt Prolog and make it return to its top level. This is the equivalent to performing a ctrl+c or similar command
	 when using Prolog under a standard console shell. */
	public void interrupt(){
	    interrupting = true;
	    doInterrupt();
	    waitUntilIdle();
	    interrupting = false;
	}
	
	protected abstract void doInterrupt();
	
	/** Execute a Prolog "command" */
    public boolean command(String s){
        if (topGoalHasStarted) return deterministicGoal(s);
        else return realCommand(s);
    }
	
	/** Implementation of a simple parameterless Prolog goal; does not support recursive nor multithreaded operation, use command instead
	@see #command(String) */
	protected abstract boolean realCommand(String s);
	
	/** Debugging aid */
	public void progressMessage(String s){
		if (debug) System.out.println(System.currentTimeMillis()-startTime+ "ms: "+s);
	}

	/** Debug messages are being written
	*/
	public boolean isDebug(){ return debug;}
	
	public void setDebug(boolean d){debug=d;}
	
	/** Convenience for debugging */
	public static void printBindings(Object[] b){
		if (b==null) System.out.println("Empty bindings");
		else 
			for (int i=0;i<b.length;i++) System.out.println("Binding "+i+":"+b[i]);
	}
		
	protected final void teachIPobjects(ObjectOutputStream obs) throws IOException{
		Object [] oa1 = {new Integer(13)};
		obs.writeObject(
			new ObjectExamplePair(
				"GoalFromJava",
				new GoalFromJava(1,"a","aa",new Object[0],"A"),
				new GoalFromJava(2,"b","bb",oa1,"B")
			)); 
		obs.writeObject(
			new ObjectExamplePair(
				"ResultFromProlog",
				new ResultFromProlog(1,true,0,null),
				new ResultFromProlog(2,false,1,"some error")
			)); 
		MessageFromProlog mpA = new MessageFromProlog();
		mpA.methodName="methodA";
		mpA.arguments=new Object[0];
		mpA.returnArguments=true;
		MessageFromProlog mpB = new MessageFromProlog();
		mpB.timestamp=2;
		mpB.target = "target";
		mpB.methodName="methodB";
		mpB.arguments=new Object[1];
		mpB.returnArguments=false;
		obs.writeObject(
			new ObjectExamplePair("MessageFromProlog",mpA,mpB)
		); 
		/* crashed JRE 1.4 with new Exception():*/
		obs.writeObject(
			new ObjectExamplePair("ResultFromJava",
				new ResultFromJava(1,null,null,new Object[0]),
				new ResultFromJava(2,"result here","new Exception()...",new Object[1])
			)
		); 
		obs.writeObject(
			new ObjectExamplePair("InvisibleObject",new InvisibleObject(1),new InvisibleObject(2))
		); 
		obs.writeObject(
			new ObjectExamplePair("IPClassObject",new IPClassObject("A"),new IPClassObject("B"))
		); 
		obs.writeObject(
			new ObjectExamplePair("IPClassVariable",
				new IPClassVariable("ClassA","VariableA"), new IPClassVariable("ClassB","VariableB")
				)
		); 
	}

    /** Provide Prolog with material to construct special objects that 
	actually represent basic type values, so that Prolog can call
	(through javaMessage) methods with basic type (non-object) arguments.
	Also provides material for some other convenience objects */
	protected void teachBasicObjects(ObjectOutputStream obs) throws IOException{
		obs.writeObject(
			new ObjectExamplePair(
				"boolean",new BasicTypeWrapper(new Boolean(true)),new BasicTypeWrapper(new Boolean(false))
			)); 
		obs.writeObject(
			new ObjectExamplePair(
				"byte",
				new BasicTypeWrapper(new Byte((new Integer(1)).byteValue())),
				new BasicTypeWrapper(new Byte((new Integer(2)).byteValue()))
			)); 
		obs.writeObject(
			new ObjectExamplePair(
				"char",new BasicTypeWrapper(new Character('A')),new BasicTypeWrapper(new Character('B'))
			)); 
		obs.writeObject(
			new ObjectExamplePair(
				"double",new BasicTypeWrapper(new Double(1)),new BasicTypeWrapper(new Double(2))
			)); 
		obs.writeObject(
			new ObjectExamplePair(
				"float",new BasicTypeWrapper(new Float(1)),new BasicTypeWrapper(new Float(2))
			)); 
		obs.writeObject(
			new ObjectExamplePair(
				"int",new BasicTypeWrapper(new Integer(1)),new BasicTypeWrapper(new Integer(2))
			)); 
		obs.writeObject(
			new ObjectExamplePair(
				"long",new BasicTypeWrapper(new Long(1)),new BasicTypeWrapper(new Long(2))
			)); 
		obs.writeObject(
			new ObjectExamplePair(
				"short",
				new BasicTypeWrapper(new Short( (new Integer(1)).shortValue() )),
				new BasicTypeWrapper(new Short( (new Integer(2)).shortValue() ))
			)); 
		obs.writeObject(new ObjectExamplePair(new Boolean(true),new Boolean(false)));
		obs.writeObject(new ObjectExamplePair(new Integer(1),new Integer(2)));
		obs.writeObject(new ObjectExamplePair(new Float(20.59375),new Float(2)));
		obs.writeObject(TermModel.example());
		obs.writeObject(VariableNode.example());
		obs.writeObject(new ObjectExamplePair("ArrayOfTermModel",new TermModel[0],new TermModel[1]));
		obs.writeObject(new ObjectExamplePair("ArrayOfString",new String[0]));
		obs.writeObject(new ObjectExamplePair("ArrayOfObject",new Object[0]));
	}

	/** Same as #teachMoreObjects(ObjectExamplePair[]), but the single example pair is constructed repeating the object
	@see #teachMoreObjects(ObjectExamplePair[]) */
	public boolean teachOneObject(Object example){
		return teachMoreObjects(new Object[]{example});
	}
	
	/** Same as #teachMoreObjects(ObjectExamplePair[]), but example pairs are constructed with (2) repeated examples for each object
	@see #teachMoreObjects(ObjectExamplePair[]) */
	public boolean teachMoreObjects(Object[] examples){
		if (examples[0] instanceof ObjectExamplePair) 
			throw new IPException("Bad method invocation in teachMoreObjects");
		ObjectExamplePair[] pairs = new ObjectExamplePair[examples.length];
		for (int i=0;i<pairs.length;i++)
			pairs[i] = new ObjectExamplePair(examples[i]);
		return teachMoreObjects(pairs);		
	}
	
	/** Send an array of object example pairs to Prolog and generate ipObjectSpec facts. 
	Returns true if this succeeds, false otherwise.
	@param examples The examples
	@see ObjectExamplePair */
	public boolean teachMoreObjects(ObjectExamplePair[] examples){
		// simply call "dg" to teach examples...
		Object[] temp = new Object[examples.length];
		for (int i=0;i<examples.length;i++)
			temp[i] = examples[i];
		return deterministicGoal("ipProcessExamples(Examples)", "Examples", temp);
	}
	
	protected GoalFromJava makeDGoalObject(String G, String OVar, Object[] objectsP, String RVars,int timestamp){
		// RVars can not be tested here, only at the Prolog side after the goal
       	if (G==null)
            throw new IPException ("Null Goal in deterministicGoal");
        if (OVar==null)
            OVar="_";
        if (objectsP == null)
            objectsP = new Object[0];
        if (G.trim ().endsWith ("."))
            throw new IPException ("Goal argument should have no trailing '.', in deterministicGoal");
        return new GoalFromJava(timestamp,G,OVar,objectsP,RVars);
	}
	
	/** Synchronously calls a Prolog goal. 
	Only the first solution is considered. G should contain a syntactically correct
	Prolog term, without the trailing dot (.). Throws an IPAbortedException if a Prolog abort happens, and an
	IPInterruptedException if the interrupt() method was invoked.
	@see #deterministicGoal(String)
	@see #deterministicGoal(String,String)
	@see #deterministicGoal(String,String,Object[])
	@return	a new array containing an object for each term in the rVars list, or null if goal fails
	@param G Prolog goal term
	@param OVar Prolog variable that will be bound to objectsP array
	@param objectsP Array of Java objects to pass to Prolog goal
	@param RVars Prolog list with object specifications, typically containing variables occurring in g. 
	If null a single binding will be returned, containing a TermModel object representing the goal term solution
	*/	
	public Object[] deterministicGoal(String G, String OVar, Object[] objectsP, String RVars){
        //available=false;
 		int mytimestamp = incGoalTimestamp();
       	GoalFromJava GO = makeDGoalObject(G, OVar, objectsP, RVars, mytimestamp);
        Object[] resultToReturn=null;
        try{
            progressMessage("Schedulling (in PrologEngine) goal "+G+", timestamp "+mytimestamp+" in thread "+Thread.currentThread().getName());
            GoalToExecute goalToDo = new GoalToExecute(GO);
            scheduleGoal(goalToDo);
            ResultFromProlog result = goalToDo.waitForResult();
            // goalToDo is forgotten by handleCallback
                       
            if (result==null) throw new IPException("Problems in goal result");
            if (goalToDo.wasAborted()) throw new IPAbortedException(G+" was aborted");
            if (goalToDo.wasInterrupted()) throw new IPInterruptedException(G+" was interrupted");
            if (result.error!=null) throw new IPException (result.error);
            if (result.timestamp!=mytimestamp)
                throw new IPException ("bad timestamp in deterministicGoal, got "+result.timestamp+" instead of "+goalTimestamp);
            if (result.succeeded)
                resultToReturn = result.rVars;
        } catch (IPException e) {
            throw e;
        } catch (Exception e) {
            throw new IPException ("Problem in deterministicGoal:"+e);
        }
        return resultToReturn;
    }
	
	/** A parameterless goal with no result other than success/failure. Same as deterministicGoal(G, null,null,"[]") */
	public boolean deterministicGoal(String G){
		return (deterministicGoal(G, null,null,"[]")!=null);
	}
	
	/** Useful when you're constructing objects from Prolog, but don't need to pass any from Java. Same as deterministicGoal(G,null,null,RVars) */
	public Object[] deterministicGoal(String G,String RVars){	 
		return deterministicGoal(G,null,null,RVars);
	}

	/** Useful when you want to pass objects to Prolog but don't need objects returned. Same as deterministicGoal(G, OVar,objectsP,"[]") */
	public boolean deterministicGoal(String G, String OVar, Object[] objectsP){
		return (deterministicGoal(G, OVar,objectsP,"[]")!=null);
	}
	
	// Increment the goal counter
	protected int incGoalTimestamp(){
		goalTimestamp++; 
		if (goalTimestamp<0) throw new IPException("goalTimestamp did wrap around, please improve it...");
		return goalTimestamp;
	}
	
	/** Adds goal to pool awaiting execution by Prolog */
	protected synchronized void scheduleGoal(GoalToExecute g){
		goalsToExecute.addElement(g);
	}	
	
	protected synchronized GoalToExecute moreRecentToExecute(){
		for (int i=goalsToExecute.size()-1; i>=0; i--){
			GoalToExecute gte = (GoalToExecute)goalsToExecute.elementAt(i);
			if (!gte.hasStarted()) return gte;
		}
		return null;
	}
	
	/** Currently does a dumb linear search, enough for our scenarios. 
	2 possibilities: use Hashtable; make ResultFromProlog bring back a reference to GoalFromJava */
	protected synchronized GoalToExecute forgetGoal(int timestamp){
		for (int i=0; i<goalsToExecute.size(); i++){
			GoalToExecute gte = (GoalToExecute)goalsToExecute.elementAt(i);
			if (gte.getTimestamp()==timestamp){
				goalsToExecute.removeElementAt(i);
				return gte;
			}
		}
		return null;
	}
	
	/** Just adds to messagesExecuting */
	protected synchronized void addMessage(MessageExecuting m){
		messagesExecuting.addElement(m);
	}
	
	protected synchronized void forgetMessage(MessageExecuting m){
		messagesExecuting.removeElement(m);
	}
	
	protected synchronized MessageExecuting lastMessageRequest(){
		MessageExecuting last;
		if (messagesExecuting.size()==0) last=null;
		else last = (MessageExecuting)messagesExecuting.lastElement();
		return last;
	}
	
	/** The engine is doing nothing */
	public synchronized boolean isIdle(){
		return messagesExecuting.size()==0 && goalsToExecute.size()==0;
	}
	
	/** Do not invoke this */
	public synchronized void abortTasks(){
		for (int i=0; i<goalsToExecute.size(); i++){
			GoalToExecute gte = (GoalToExecute)goalsToExecute.elementAt(i);
			gte.abort();
		}
		goalsToExecute.removeAllElements();
		messagesExecuting.removeAllElements();
	}
	
	/** Do not invoke this */
	public synchronized void interruptTasks(){
		for (int i=0; i<goalsToExecute.size(); i++){
			GoalToExecute gte = (GoalToExecute)goalsToExecute.elementAt(i);
			gte.interrupt();
		}
		goalsToExecute.removeAllElements();
		messagesExecuting.removeAllElements();
	}
	
	/** Present implementation is always available, so this always returns true */
	public boolean isAvailable(){
		return true;
	}
	
	public void waitUntilAvailable(){
		try{
			while(!isAvailable()) Thread.sleep(0,1);
		} catch (InterruptedException e){
			throw new IPException("Bad interrupt:"+e); 
		}
	}
	
	/** Sleeps the current Java thread until this engine is available. If this never happens, we're in trouble. */
	public void waitUntilIdle(){
		try{
			while(!isIdle()) Thread.sleep(0,1);
		} catch (InterruptedException e){
			throw new IPException("Bad interrupt:"+e); 
		}
	}
	
	/** Handling of javaMessages and deterministicGoals. This is where most things happen. 
	@param x Argument of the callback predicate*/
	public Object handleCallback(Object x){
		progressMessage("Entering handleCallback");
		if (x instanceof ClassNotFoundException) 
			return new ResultFromJava(0,null,(ClassNotFoundException)x,null); 
		else if (x instanceof MessageFromProlog){
			MessageFromProlog mfp = (MessageFromProlog)x;
			progressMessage("handling "+mfp);
			// first message is a dummy just to get us here:
			if (!isFirstJavaMessage(mfp)){
				MessageExecuting me = new MessageExecuting(mfp,this);
				me.start();
				addMessage(me);
			} else progressMessage("received first (dummy) javaMessage");
		} else if (x instanceof ResultFromProlog){
			ResultFromProlog rfp = (ResultFromProlog)x;
			progressMessage("handling "+rfp);
			GoalToExecute gte = forgetGoal(rfp.timestamp);
			progressMessage("forgot goal "+gte+"; isIdle()=="+isIdle());
			if (gte==null) throw new IPException("Could not find goal "+rfp.timestamp);
			gte.setResult(rfp);
		} else throw new IPException("bad object in handleCallback:"+x);
		// no errors so far
		return doSomething();
	}
	/** return result to last javaMessage or pick more recent GoalToExecute */	
	protected Object doSomething(){
		while(true){
			// let some work proceed elsewhere; this seems light enough, if not use wait/notify around here too:
			if (isIdle())
				try { Thread.sleep(0,100); }
				catch (InterruptedException e){throw new IPException("Bad interrupt");}
			else Thread.yield();
			MessageExecuting last = lastMessageRequest();
			if (last!=null && last.hasEnded()) {
				forgetMessage(last);
				return last.getResult();
			}
			GoalToExecute gte = moreRecentToExecute();
			if (gte!=null){
				gte.prologWasCalled();
				return gte.getGoal();
			}
		}
	}
	
	private boolean isFirstJavaMessage(MessageFromProlog mfp){
		if (!mfp.methodName.equals(firstJavaMessageName)) return false;
		if ((mfp.target instanceof InvisibleObject)&& getRealJavaObject((InvisibleObject)mfp.target)==this)
			return true;
		else return false;
	}
	
	/** Dummy method, whose name is used to start the callback thread */
	public final void firstJavaMessage(){
		throw new IPException("This should never be called, bad javaMessage handling");
	}
	
	/** Execute a Prolog->Java call */
    public ResultFromJava doCallback(Object x){
        progressMessage ("Starting handling of XSB->Java callback:"+PrologEngine.nl+x);
        if (x==null | ! (x instanceof MessageFromProlog) )
            return new ResultFromJava (0,null,null,null);
        MessageFromProlog callback = (MessageFromProlog)x;
        Class formalArguments[] = new Class[callback.arguments.length];
        Object[] localArguments = new Object[callback.arguments.length];
        
        Object target=null;
        Object result = null; Exception exception=null;
        
        try {
            if (callback.target instanceof InvisibleObject){
                target = getRealJavaObject ((InvisibleObject)callback.target);
            } else if(callback.target instanceof IPClassObject){
                target = Class.forName (((IPClassObject)(callback.target)).classname);
            } else if (callback.target instanceof IPClassVariable) {
                IPClassVariable tempTarget = (IPClassVariable)(callback.target);
                Class tempClass = Class.forName (tempTarget.className);
                target = tempClass.getField (tempTarget.variableName).get (tempClass);
            } else
                target = callback.target;
            
            for(int a=0;a<formalArguments.length;a++){
                localArguments[a] = callback.arguments[a];
                if (localArguments[a]!=null){
                    if(localArguments[a] instanceof BasicTypeWrapper) {
                        BasicTypeWrapper btw = (BasicTypeWrapper)(localArguments[a]);
                        formalArguments[a] = btw.basicTypeClass ();
                        localArguments[a] = btw.wrapper; // Integer instead of int ???
                    } else if (localArguments[a] instanceof InvisibleObject) {
                        localArguments[a] = getRealJavaObject ((InvisibleObject)localArguments[a]);
                        formalArguments[a] = localArguments[a].getClass ();
                    } else if (localArguments[a] instanceof IPClassObject) {
                        localArguments[a] = Class.forName (((IPClassObject)(localArguments[a])).classname);
                        formalArguments[a] = Class.class;
                    } else if (localArguments[a] instanceof IPClassVariable) {
                        IPClassVariable IPCV = (IPClassVariable)(localArguments[a]);
                        Class tempClass = Class.forName (IPCV.className);
                        localArguments[a] = tempClass.getField (IPCV.variableName).get (null);
                        formalArguments[a] = localArguments[a].getClass ();
                    } else formalArguments[a] = localArguments[a].getClass ();
                }
            }
            Method method=null;
            if (target instanceof Class){
                if (shortClassName ((Class)target).equals (callback.methodName)) {
                    // It's a (public...) constructor invocation
                    //Constructor constructor = ((Class)target).getConstructor(formalArguments);
                    Constructor constructor = findConstructor ((Class)target,formalArguments);
                    result = constructor.newInstance (localArguments);
                } else {
                    // It's a class (static) method invocation
                    // Method method = ((Class)target).getMethod(callback.methodName,formalArguments);
                    method = findMethod ((Class)target,callback.methodName,formalArguments);
                    //result = method.invoke(target,localArguments);
                    result = method.invoke (target,localArguments);
                }
            } else {
                // An instance method invocation
                // Method method = target.getClass().getMethod(callback.methodName,formalArguments);
                //result = findMethod(target.getClass(),callback.methodName,formalArguments).invoke(target,localArguments);
                method = findMethod (target.getClass (),callback.methodName,formalArguments);
                result = method.invoke (target,localArguments);
            }
            // The result will be an invisible object, except if a String or a wrapper or a TermModel...
            // ...or if this is a getRealJavaObject message sent to the PrologEngine
            if (result!=null && !(target==this && method.equals(getRealJavaObjectMethod)) && !(result instanceof InvisibleObject) 
            && !(result instanceof String) && !(result instanceof TermModel)
            && ! BasicTypeWrapper.instanceOfWrapper (result))
                result = makeInvisible (result);
        } catch (Exception e) {
            exception=e;
        }
        if (exception!=null) {
            System.out.println ("Courtesy of CallbackHandler:");
            exception.printStackTrace ();
        }
        if (callback.returnArguments)
            return new ResultFromJava (callback.timestamp,result,exception,callback.arguments);
        else
            return new ResultFromJava (callback.timestamp,result,exception,null);
    }

    /** An utility building on the functionality of getMethod(), to provide the javaMessage predicate with method argument
     * polimorphism. If the type signatures do not match exactly, searches all method signatures to see if their arguments
     * are type-compatible.
     */
    public static Method findMethod (Class targetClass,String name,Class[] formalArguments) throws NoSuchMethodException{
        Method m = null;
        try{
            m = targetClass.getMethod (name,formalArguments);
            return m;
        }
        catch(NoSuchMethodException e) {
            // Let's try to find "by hand" an acceptable method
            Method[] allMethods = targetClass.getMethods ();
            for (int i=0; i<allMethods.length;i++){
                if (allMethods[i].getName ().equals (name) && allMethods[i].getParameterTypes ().length == formalArguments.length){
                    boolean compatible = true;
                    for (int j=0; j<formalArguments.length; j++) {
                        if (!assignableType (allMethods[i].getParameterTypes ()[j],formalArguments[j])) {
                            compatible = false;
                            break;
                        }
                    }
                    if (compatible){
                        m = allMethods[i];
                        break;
                    }
                }
            }
            if (m==null) {
                
                System.err.println ("Could not find "+name+" in "+targetClass);
                System.err.println ("Argument types:");
                for (int i=0; i<formalArguments.length;i++)
                    System.out.println (i+":"+formalArguments[i]);
                
                throw e;
            }else return m;
        }
    }
    
    /** Similar to findMethod(), but for constructors rather than regular methods */
    public static Constructor findConstructor (Class targetClass,Class[] formalArguments) throws NoSuchMethodException{
        Constructor m = null;
        try{
            m = targetClass.getConstructor (formalArguments);
            return m;
        }
        catch(NoSuchMethodException e) {
            // Let's try to find "by hand" an acceptable constructor
            Constructor[] allConstructors = targetClass.getConstructors ();
            for (int i=0; i<allConstructors.length;i++){
                if (allConstructors[i].getParameterTypes ().length == formalArguments.length){
                    boolean compatible = true;
                    for (int j=0; j<formalArguments.length; j++) {
                        if (!assignableType (allConstructors[i].getParameterTypes ()[j],formalArguments[j])) {
                            compatible = false;
                            break;
                        }
                    }
                    if (compatible){
                        m = allConstructors[i];
                        break;
                    }
                }
            }
            if (m==null) throw e;
            else return m;
        }
    }
    
    /** It is OK to assign an expression typed right to a variable typed left. Delegates on isAssignableFrom*/
    public static boolean assignableType (Class left,Class right){
        if (right==null) return true;
        else return left.isAssignableFrom (right);
    }
    
    public static String shortClassName (Class c){
        String s = c.getName ();
        int dot = s.lastIndexOf (".");
        int dollar = s.lastIndexOf ("$");
        if (dot==-1 && dollar==-1) return s;
        else return s.substring (Math.max (dot,dollar)+1,s.length ());
    }

	/** Register an object with this Engine, so it later can be referred from Prolog without serializing it.
@param x Object to be registered
@result Integer denoting the object. In Prolog one can then refer to it by using the InvisibleObject class.
@see InvisibleObject
	*/
	public int registerJavaObject(Object x){
		return knownObjects.registerJavaObject(x);
	}
	
	/** Register an object with this Engine, so it later can be referred from Prolog without serializing it, and returns
	an InvisibleObject encapsulating the reference.
@param x Object to be registered
@result InvisibleObject denoting the object. In Prolog one can then refer to it by using the InvisibleObject class.
@see InvisibleObject
	*/
	public Object makeInvisible(Object x){
		return new InvisibleObject(knownObjects.registerJavaObject(x));
	}
	
	/** Get the object referred by the integer in a InvisibleObject wrapper.
@param o An InvisibleObject
@result The real object denoted by o in the context of this engine
@see InvisibleObject
	*/
	public Object getRealJavaObject(InvisibleObject o){
		return knownObjects.getRealJavaObject(o);
	}
	/** Same as getRealJavaObject(InvisibleObject), but accepts an integer ID as argument instead */
	public Object getRealJavaObject(int ID){
		return knownObjects.getRealJavaObject(ID);
	}

	/** Just returns the object, untouched (but "dereferenced" if called from Prolog). This serves the need to get objects in 
	javaMessage because of the way CallbackHandler.doCallback works. For example:
	ipPrologEngine(_E), stringArraytoList(_O,[miguel,calejo]), 
	javaMessage(_E,_R,getRealJavaObject(_O)),stringArraytoList(_R,List).
	... will bind List to [miguel,calejo] and not to an InvisibleObject specification as ordinarly would happen
	 */
	public Object getRealJavaObject(Object o){
		return o;
	}

}







