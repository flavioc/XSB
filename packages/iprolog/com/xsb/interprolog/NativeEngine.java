/* 
** Author(s): Miguel Calejo, Vera Pereira
** Contact:   interprolog@declarativa.com, http://www.declarativa.com, http://www.xsb.com
** Copyright (C) XSB Inc., USA, 2001
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.xsb.interprolog;

import com.declarativa.interprolog.*;
import com.declarativa.interprolog.util.*;
import java.io.*;

/** A PrologEngine implemented using the Java Native Interface. This class depends on interprolog_callback.c and other files, 
that are included in the emu directory of XSB Prolog 2.5 and later */
public class NativeEngine extends PrologEngine{
    static { System.out.println("\nLoading xsb library .....\n\n");}
    static { System.loadLibrary("xsb"); }
    
    private ByteArrayOutputStream bao = new ByteArrayOutputStream();
    
	/** C-side debug flag */
    private native void xsb_setDebug(boolean debug);
    protected native int xsb_init_internal(String jXSBPath);
    protected native int xsb_command_string(String jCommandString);
    protected native int xsb_close_query();
    /** Returns the xsb_query result code */
    protected native int put_bytes(byte[] b, int size, int args, String jStr);
    protected native byte[] get_bytes();
    /** Simulates a ctrl-C */
    protected native void xsb_interrupt();
    
    public NativeEngine(String startPrologCommand){
        this(startPrologCommand,false);
    }
    
    /**
     * The XSB dll must be in a directory in the Windows PATH
     * so the Java loader finds it
     */
    public NativeEngine(String XSB_DIR, boolean debug) {	
        super(XSB_DIR,debug);	
        
        if (numberOfInstances > 1)
            throw new IPException("Can't have more than one instance of NativeEngine");
        
        int ret = xsb_init_internal(XSB_DIR);
        if (ret != 0)
            throw new IPException("XSB Initialization error");
        try {     	
            progressMessage("Loading initial files...");
            command("assert(library_directory('"+tempDirectory.getAbsolutePath()+"'))");
	    //  consultFromPackage("interprolog.O", PrologEngine.class);
	    consultFromPackage("interprolog.xwam", PrologEngine.class);
            progressMessage("Teaching examples to XSB...");
            ByteArrayOutputStream serializedTemp = new ByteArrayOutputStream();
            ObjectOutputStream bootObjects = new ObjectOutputStream(serializedTemp);
            teachIPobjects(bootObjects);
            teachBasicObjects(bootObjects);
            bootObjects.flush();
            byte[] b = serializedTemp.toByteArray();
            // more bytes in 1.4 with Throwable: System.out.println(b.length+" bytes to teach");
            // this is the only teaching of objects not occurring over the deterministicGoal/javaMessage mechanism:
            if (put_bytes(b, b.length, 1, "ipLearnExamples")!=0) 
            	throw new IPException("ipLearnExamples failed");
            progressMessage("Initial examples taught.");
	    xsb_close_query();
	 
            if (!command("ipObjectSpec('InvisibleObject',E,["+registerJavaObject(this)+"],_), assert(ipPrologEngine(E))"))
            	throw new IPException("assert of ipPrologEngine/1 failed");;
        } catch (Exception e){
            throw new IPException("Could not initialize XSB:"+e);
        }
        startTopGoal();
    }
    
	/** Calls the first Prolog goal in a background thread. That goal will return only if an interrupt or error occurs;
	this method should handle these conditions properly  */
	protected void startTopGoal(){
		Thread top = new Thread(){
			public void run(){
				boolean ended = false;
				while(!ended){
					progressMessage("Calling "+firstJavaMessageName+"...");
					progressMessage("Will call xsb_command_string");
					// under normal operation the following never returns:
					int rc = xsb_command_string("ipPrologEngine(E), javaMessage(E,"+firstJavaMessageName+").");

					progressMessage("xsb_command_string done ...");
					if (rc==1 && interrupting) {
						interruptTasks();
					} else if (rc==1){
						System.err.println("Prolog execution aborted and restarted");
						abortTasks();
					}else {
						ended=true;
						System.out.println("NativeEngine ending abnormally, rc=="+rc);
					}
				}
			}
		};
		topGoalHasStarted = true;
		top.setName("Prolog handler");
		top.start();
	}

    public void setDebug(boolean d){
        super.setDebug(d);
	xsb_setDebug(d);
    }
    
    public void shutdown(){
        System.err.println("NO SHUTDOWN YET!!!");
    }
    
    protected void doInterrupt(){
        xsb_interrupt();
    }
    
    protected boolean realCommand(String s){
        int result = xsb_command_string(s+".");
        if (result==0)
            return true;
        else if (result==1)
            return false;
        else
            throw new IPException("Problem executing Prolog command");
    }    
 
 	public Object[] deterministicGoal(String G, String OVar, Object[] objectsP, String RVars){
		if (!topGoalHasStarted) 
			throw new IPException("Premature invocation of deterministicGoal");
   		return super.deterministicGoal(G, OVar, objectsP, RVars);
   	}
   	
    /** Accepts a serialized object in the argument and returns another; handleCallback does the actual work*/
    protected byte[] callback(byte[] in) {
    	progressMessage("entering callback(byte[])");
    	byte[] out;
    	Object x;
    	try{
	        ByteArrayInputStream bai = new ByteArrayInputStream(in);
			ObjectInputStream ios = new ObjectInputStream(bai);
			x = ios.readObject();
			ios.close();
		} catch (ClassNotFoundException e){
			x = e;
		} catch (IOException e){
			throw new IPException("Bad exception before callback handling:"+e);
		}
		Object y = handleCallback(x);
		try {
			synchronized(this){
				bao.reset();
    			ObjectOutputStream oos = new ObjectOutputStream(bao);
				oos.writeObject(y); oos.flush();
				out = bao.toByteArray();
			}
		} catch (IOException e){
			throw new IPException("Bad exception after callback handling:"+e);
		}
		return out;
    }
    
    public boolean isIdle(){
    	return super.isIdle();
    }
}
