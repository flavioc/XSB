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
import java.util.*;
import java.lang.reflect.*;
import java.util.zip.*;

/** A PrologEngine implemented over TCP/IP sockets. A SubprocessEngine object represents and gives access to a running Prolog process in background.
Multiple instances correspond to multiple Prolog processes, outside the Java Virtual Machine. 
*/
public class SubprocessEngine extends PrologEngine{
    Process prolog;
    PrintWriter prologStdin;
    OutputHandler stdoutHandler, stderrHandler;
    ServerSocket serverSocket;
    protected Socket socket;
    ServerSocket intServerSocket=null; Socket intSocket=null; // Used only for Windows
    String interruptCommand=null; // Used only for UNIX
    Vector listeners = new Vector ();
    boolean available;
    
    static class ClientRecognizer extends Recognizer implements RecognizerListener{
        PrologOutputListener client;
        ClientRecognizer (PrologOutputListener client){
            this.client=client;
            addRecognizerListener (this);
        }
        public void recognized (Recognizer source,Object extra){
            client.print ((String)extra);
        }
    }
    
    /** Add a PrologOutputListener to this engine.  All stdout and stderr output will be routed to the client.
     * @param client An object interested in receiving messages depicting Prolog's progress
     * @see com.declarativa.interprolog.PrologOutputListener
     */
    public synchronized void addPrologOutputListener (PrologOutputListener client){
        ClientRecognizer RE = new ClientRecognizer (client);
        listeners.addElement (RE);
        addPrologStdoutListener (RE);
        addPrologStderrListener (RE);
    }
	
	public synchronized void removePrologOutputListener(PrologOutputListener client){
		for (int i=0;i<listeners.size();i++) {
			ClientRecognizer cr = (ClientRecognizer)(listeners.elementAt(i));
			if (cr.client==client) {
				listeners.removeElementAt(i);
				removePrologStdoutListener(cr);
				removePrologStderrListener(cr);
			}
		}
	}
	
	/** 
         * Add a OutputListener to get output from Prolog's standard output.
         * This is a lower level interface than addPrologOutputListener(PrologOutputListener).
         * @param client An object interested in Prolog's standard output
         * @see com.declarativa.interprolog.util.OutputListener
         */
	public void addPrologStdoutListener(OutputListener l){
		stdoutHandler.addOutputListener(l);
	}
	
	public void addPrologStderrListener(OutputListener l){
		stderrHandler.addOutputListener(l);
	}
	
	public void removePrologStdoutListener(OutputListener l){
		stdoutHandler.removeOutputListener(l);
	}
	
	public void removePrologStderrListener(OutputListener l){
		stderrHandler.removeOutputListener(l);
	}
	
	Recognizer promptTrigger = new Recognizer("| ?-");
	Recognizer breakTrigger = new Recognizer(": ?-");
            
        /** Construct a SubprocessEngine, launching a Prolog process in background.
         * @param startPrologCommand The command to launch Prolog, as if given from a console shell.
         * Must not be null.
         * @param debug If true this engine will send debugging messages to System.out
         * @see SubprocessEngine#shutdown
         * @see SubprocessEngine#teachMoreObjects(ObjectExamplePair[])
         * @see SubprocessEngine#setDebug(boolean)
         */
	public SubprocessEngine(String startPrologCommand, boolean debug) {
            super(startPrologCommand,debug);
            // Let's make sure PrologEngines get their finalize() message when we exit:
            // Now (JDK 1.2) considered unsafe, so make sure to message shutdown():  System.runFinalizersOnExit(true);
            try {
                RecognizerListener availableSetter = new RecognizerListener (){
                    public void recognized(Recognizer source,Object extra){
                        available=true;
                    }
                };
                promptTrigger.addRecognizerListener(availableSetter);
                breakTrigger.addRecognizerListener(availableSetter);
                
                progressMessage("Launching subprocess "+startPrologCommand);
                prolog = Runtime.getRuntime().exec(startPrologCommand);

                // prolog -> java.lang.UNIXProcess@f9dc36 

                // No explicit buffering, because it's already being done by our Process's streams
                // If not, OutputHandler will handle the issue
                stdoutHandler = new OutputHandler(prolog.getInputStream());
                stderrHandler = new OutputHandler(prolog.getErrorStream());
                setDetectPromptAndBreak(true);
                
                stdoutHandler.start();
                stderrHandler.start();
                
                Thread.yield(); // let's try to catch Prolog output ASAP
                
                prologStdin = new PrintWriter(prolog.getOutputStream());
                
                progressMessage("Loading initial files...");
                command("assert(library_directory('"+tempDirectory.getAbsolutePath()+"'))");
		//consultFromPackage("interprolog.O",SubprocessEngine.class);
		consultFromPackage("interprolog.xwam",SubprocessEngine.class);
                
                String myHost="127.0.0.1"; // to avoid annoying Windows dialup attempt
                progressMessage ("Allocating the ServerSocket...");
                serverSocket = new ServerSocket (0); // let the system pick a port
                progressMessage ("server port:"+serverSocket.getLocalPort ());
                                
		command("ipinitialize('"+myHost+"',"+
			serverSocket.getLocalPort ()+","+
			registerJavaObject (this)+")");
                
                progressMessage("Waiting for the socket to accept...");
                socket = serverSocket.accept();
                
                progressMessage("Teaching examples to XSB...");
                
                PrologOutputObjectStream bootobjects = new PrologOutputObjectStream(socket.getOutputStream());
		
                // slightly more tortuous to ease reuse of these methods
		ObjectOutputStream oos = bootobjects.getObjectStream();	
                teachIPobjects(oos);
                teachBasicObjects(oos);		
                bootobjects.flush();	
                // Now for OS-dependent Prolog interrupt generation
                //prepareInterrupt(myHost);
		

		// Added later to setup the call back server first
			
		  setupCallbackServer();
		  waitUntilAvailable();
		  prepareInterrupt(myHost);
	          
	    } catch (IOException e){
                    throw new IPException("Could not launch XSB:"+e);
            }
	    
           // setupCallbackServer();
           // waitUntilAvailable();
	    
        }
	
	public SubprocessEngine(String startPrologCommand){
		this(startPrologCommand,false);
	}
	
	/** Prolog is thought to be idle */
	public boolean isAvailable(){
		return available;
	}
	
	protected void setupCallbackServer(){
		Thread server = new Thread (){
			public void run(){
				try{
					while(!shutingDown) {
						progressMessage("Waiting to receive object");
						Object x = receiveObject();
						progressMessage("Received object:"+x);
						Object y = handleCallback(x);
						progressMessage("Handled object and computed:"+y);
						if (y!=null) sendObject(y);
					}
				} catch (IOException e){
					if (!shutingDown) 
					throw new IPException("Bad exception in setupCallbackServer:"+e);
				}
			}
		};
		progressMessage ("Starting up callback service...");
		server.setName("IP javaMessage handler");
		server.start();
	}
	
	protected Object receiveObject() throws IOException{
     	progressMessage("entering receiveObject()");
   		Object x=null;
    	try{
			ObjectInputStream ios = new ObjectInputStream(socket.getInputStream());
			progressMessage("got input stream");
			x = ios.readObject();
		} catch (ClassNotFoundException e){
			x = e;
		}
     	progressMessage("exiting receiveObject():"+x);
		return x;
	}
	
	protected void sendObject (Object y) throws IOException{
    	progressMessage("entering sendObject("+y+")");
		PrologOutputObjectStream poos = 
		    new PrologOutputObjectStream(socket.getOutputStream());
		poos.writeObject(y);
		poos.flush(); // this actually writes to the socket stream
    	progressMessage("exiting sendObject("+y+")");
	}
	
	/** Shuts down the background Prolog process as well as the dependent Java threads.
	*/
	public synchronized void shutdown(){
		shutingDown=true;
		available=false;
		stdoutHandler.setIgnoreStreamEnd(true);
		stderrHandler.setIgnoreStreamEnd(true);
		
		// Miti: Added for testit to work properly on Linux
		try{
		    socket.close();
		    serverSocket.close();
		}catch(IOException e) {}
		    
		// Miti: Added the check so that the intSocket, intServerSocket are closed only for Windows
		if(needsSocketInterrupt()){
		    try {
			// closing sockets will stop them, no need to deprecate:
			// stdoutHandler.stop(); stderrHandler.stop(); cbhandler.stop();
			socket.close(); serverSocket.close();
			intSocket.close(); intServerSocket.close();
		    }
		    catch (IOException e) {throw new IPException("Problems closing sockets:"+e);}
		    
		    finally{prolog.destroy();}
		
		}	       
		progressMessage("shutdown performed ");
	}
	
	protected boolean isShutingDown(){
		return shutingDown;
	}
	
	/** Kill the Prolog background process. If you wish to make sure this message is sent on exiting, 
	use System.runFinalizersOnExit(true) on initialization
	*/
	protected void finalize() throws Throwable{
		if (prolog!=null) prolog.destroy();
	}
	
	protected void setDetectPromptAndBreak(boolean yes){
		if (yes==isDetectingPromptAndBreak()) return;
		if(yes){
			stdoutHandler.addOutputListener(promptTrigger);
			stdoutHandler.addOutputListener(breakTrigger);
			//stderrHandler.addOutputListener(promptTrigger);
			//stderrHandler.addOutputListener(breakTrigger);
		} else{
			stdoutHandler.removeOutputListener(promptTrigger);
			stdoutHandler.removeOutputListener(breakTrigger);
			//stderrHandler.removeOutputListener(promptTrigger);
			//stderrHandler.removeOutputListener(breakTrigger);
		}
	}
	protected boolean isDetectingPromptAndBreak(){
		return stdoutHandler.hasListener(promptTrigger) /*&& stderrHandler.hasListener(promptTrigger)*/ &&
			stdoutHandler.hasListener(breakTrigger) /*&& stderrHandler.hasListener(breakTrigger)*/;
	}
	
	/** Sends a String to Prolog's input. Its meaning will naturally depend on the current state of Prolog: it can be
        a top goal, or input to an ongoing computation */
	public synchronized void sendAndFlush(String s){
		available=false;
		prologStdin.print(s); prologStdin.flush();
	}
	
	public void sendAndFlushLn(String s){
		sendAndFlush(s+nl);
	}
	
	protected static boolean needsSocketInterrupt(){ // if we're under Windows
	    	    return (System.getProperty("os.name").toLowerCase().indexOf("windows")!=-1);
	    //return true;
	  
	}
	
	protected void prepareInterrupt(String myHost) throws IOException{ // requires successful startup steps
		if (needsSocketInterrupt()){ 
			intServerSocket = new ServerSocket(0);
			command("setupWindowsInterrupt('"+myHost+"',"+intServerSocket.getLocalPort()+")");
			intSocket = intServerSocket.accept();
		} else {
			//available=true; // sort of a hack... but when will the state of 'available' become valid ?
			waitUntilAvailable();
			Object bindings[] = deterministicGoal("getPrologPID(N), ipObjectSpec('java.lang.Integer',Integer,[N],_)",
				"[Integer]");		
			if (bindings!=null) interruptCommand = "/bin/kill -s INT "+bindings[0];
			else throw new IPException("Could not find XSB's PID");
		}
	}
	protected synchronized void doInterrupt(){
	    setDetectPromptAndBreak(true);
	    try {
		if(needsSocketInterrupt()){
				// Windows
		    byte[] ctrlc = {3};
		    progressMessage("Attempting to interrupt XSB...");
		    OutputStream IS = intSocket.getOutputStream();
		    IS.write(ctrlc); IS.flush();
		} else{
				// Probably Solaris: we'll just use a standard UNIX signal
		    progressMessage("Interrupting XSB with "+interruptCommand);
		    Runtime.getRuntime().exec(interruptCommand);
		}
			
	    } 
	    catch(IOException e) {throw new IPException("Exception in interrupt():"+e);}
	    waitUntilAvailable();
	    sendAndFlushLn("abort."); // leave break mode
	}
	
	/** This implementation may get stuck if the command includes variables, because the Prolog
	top level interpreter may offer to compute more solutions; use variables prefixed with '_' */
	protected boolean realCommand(String s){
		progressMessage("COMMAND:"+s+".");
		sendAndFlushLn(s+".");
		return true; // we do not really know
	}
	
	public Object[] deterministicGoal(String G, String OVar, Object[] objectsP, String RVars){
	   
		boolean first=false;
		synchronized(this){
		if (!topGoalHasStarted){
			topGoalHasStarted = true;
				first=true;
			}
		}
		if (first){
			if (!isIdle()) throw new IPException("Inconsistency in deterministicGoal:");		
			Object[] result = firstGoal(G, OVar, objectsP, RVars);
			return result;
		} else return super.deterministicGoal(G, OVar, objectsP, RVars);
	}

	/** Very alike deterministicGoal except that it sends the initial GoalFromJava object over the socket */
	protected Object[] firstGoal(String G, String OVar, Object[] objectsP, String RVars){
 		int mytimestamp = incGoalTimestamp();
       	GoalFromJava GO = makeDGoalObject(G, OVar, objectsP, RVars, mytimestamp);
        Object[] resultToReturn=null;
        try{
            progressMessage("Schedulling (first) goal "+G+", timestamp "+mytimestamp+" in thread "+Thread.currentThread().getName());
            GoalToExecute goalToDo = new GoalToExecute(GO);
            goalToDo.setFirstGoalStatus();
            scheduleGoal(goalToDo);
 			goalToDo.prologWasCalled();
            //setupErrorHandling();
            sendObject(GO);
	    realCommand("deterministicGoal"); // assynchronous
	    ResultFromProlog result = goalToDo.waitForResult();
	    // goalToDo is forgotten by handleCallback
            progressMessage("got dG result for timestamp "+mytimestamp);         
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
        } finally{
			topGoalHasStarted = false; // is this OK? this assumes no initiative from the Prolog side, which is probably correct
			//removeErrorHandling();
			progressMessage("Leaving firstGoal for "+G+", timestamp "+mytimestamp+" isIdle()=="+isIdle());
        }
        return resultToReturn;
    }

	protected Object doSomething(){
		if (onlyFirstGoalSchedulled()) return null;
		else return super.doSomething();
	}
	
	protected synchronized boolean onlyFirstGoalSchedulled(){
		return isIdle() || (messagesExecuting.size()==0 && goalsToExecute.size()==1 && 
			((GoalToExecute)goalsToExecute.elementAt(0)).isFirstGoal());
	}
	
	// deterministicGoal helpers
	
    protected void setupErrorHandling(){
		setDetectPromptAndBreak(false);
		stderrHandler.addOutputListener(errorTrigger); // no need to listen to stdout
		abortMessage = "";
		final Thread current = Thread.currentThread();
		// We could dispense creating this every time:
		errorHandler = new RecognizerListener(){
			public void recognized(Recognizer source,Object extra){
			    abortMessage = (String)extra;
			    current.interrupt(); 
			}
		    };
		errorTrigger.addRecognizerListener(errorHandler);
    }
    
    protected void removeErrorHandling(){
    	errorTrigger.removeRecognizerListener(errorHandler);
    	stderrHandler.removeOutputListener(errorTrigger);
    	errorHandler=null;
		setDetectPromptAndBreak(true);
    }
    private RecognizerListener errorHandler=null;
    Recognizer errorTrigger = new Recognizer("++Error",true); // was "++Error: " for XSB 2.4
    private String abortMessage;

}


