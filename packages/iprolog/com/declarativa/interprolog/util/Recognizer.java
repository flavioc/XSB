/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.util;
import com.declarativa.interprolog.util.*;
import java.util.*;

/** A simple pattern recognizor used in error detection */
public class Recognizer implements OutputListener{
	Vector listeners;
	int nextInPattern; // next byte to recognize
	byte[] bytePattern;
	boolean collectRestOfBuffer;

	public Recognizer(){
		this(null);
	}
	public Recognizer(String pattern){
		this(pattern,false);
	}
	public Recognizer(String pattern,boolean collectRestOfBuffer){
		listeners = new Vector();
		if (pattern==null) bytePattern=new byte[0];
		else bytePattern = pattern.getBytes();
		nextInPattern=0;
		this.collectRestOfBuffer=collectRestOfBuffer;
	}
	
	public int numberListeners(){ return listeners.size(); }

	// OutputListener methods:
	public void analyseBytes(byte[] buffer,int nbytes){
		if (bytePattern.length==0) 
			fireRecognized(new String(buffer,0,nbytes));
		else for(int b=0;b<nbytes;b++) {
			if(buffer[b]==bytePattern[nextInPattern]){
				nextInPattern++;
				if (nextInPattern>=bytePattern.length) {
					nextInPattern = 0;
					if (collectRestOfBuffer && b+1<nbytes) {
						fireRecognized(new String(buffer,b+1,nbytes-(b+1)));
						break;
					}
					else fireRecognized("");
				}
			}
			else nextInPattern = 0;
		}
	}
	public void streamEnded(){
		throw new IPException("Unexpected end of stream, Prolog may have died abruptly");
	}
	
	public synchronized void addRecognizerListener(RecognizerListener l){
		listeners.addElement(l);
	}
	public synchronized void removeRecognizerListener(RecognizerListener l){
		listeners.removeElement(l);
	}
	void fireRecognized(String extra){
		for (int l=0; l<listeners.size(); l++)
			((RecognizerListener)(listeners.elementAt(l))).recognized(this,extra);
	}
}

