/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.util;
import java.io.*;
import java.util.*;
import com.declarativa.interprolog.*;

/** An object consuming input from a stream, analysing it and sending messages to a list 
of OutputListener objects  */
public class OutputHandler extends Thread {
	InputStream sourceStream;
	Vector listeners;
	private boolean ignoreStreamEnd;
	
	public OutputHandler(InputStream s){
		if (s instanceof BufferedInputStream) sourceStream = s;
		else sourceStream = new BufferedInputStream(s);
		listeners = new Vector();
		ignoreStreamEnd=false;
	}
	
	public synchronized void addOutputListener(OutputListener ol){
		listeners.addElement(ol);
	}
	public synchronized void removeOutputListener(OutputListener ol){
		listeners.removeElement(ol);
	}
	
	public boolean hasListener(OutputListener ol){
		return listeners.contains(ol);
	}
	
	public void run(){
		byte[] buffer = new byte[1024]; 
		while(true) {
			try{
				int nchars = sourceStream.read(buffer,0,buffer.length);
				if (nchars==-1){
					fireStreamEnded();
					break;
				} else fireABs(buffer,nchars);
			} catch (IOException ex){ throw new IPException("Problem fetching output:"+ex);}
		}
	}
	synchronized void fireStreamEnded(){
		if (ignoreStreamEnd) return;
		for (int L=0; L<listeners.size(); L++)
			((OutputListener)(listeners.elementAt(L))).streamEnded();
	}
	synchronized void fireABs(byte[] buffer,int nbytes){
		for (int L=0; L<listeners.size(); L++)
			((OutputListener)(listeners.elementAt(L))).analyseBytes(buffer,nbytes);
	}
	
	public void setIgnoreStreamEnd(boolean ignore){
		ignoreStreamEnd=ignore;
	}
}

