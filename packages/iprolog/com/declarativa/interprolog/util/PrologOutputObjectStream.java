/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.util;
import java.io.*;
/** Vaguely similar to an ObjectOutputStream, but sends the total number of serialized bytes up front,
 so Prolog can keep grammar and socket reads separate without hanging for input*/
public class PrologOutputObjectStream {
	OutputStream os;
	ObjectOutputStream tempObjects;
	ByteArrayOutputStream serializedTemp;
	boolean flushed = false;
	public PrologOutputObjectStream(OutputStream os) throws IOException{
		this.os=os;
		serializedTemp = new ByteArrayOutputStream();
		tempObjects = new ObjectOutputStream(serializedTemp);
	}
	
	public ObjectOutputStream getObjectStream(){
		return tempObjects;
	}
	
	public void flush() throws IOException{
		tempObjects.close();
       	(new DataOutputStream(os)).writeInt(size()); // byte count up front...
       	serializedTemp.writeTo(os);
       	serializedTemp.close();
       	os.flush(); 
       	flushed=true;
	}
	public void writeObject(Object obj) throws IOException{
		if (flushed) throw new Error("A PrologOutputObjectStream can be used only once.");
		tempObjects.writeObject(obj);
	}
	public int size(){
		return serializedTemp.size();
	}
}
