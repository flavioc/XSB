/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.util;
import com.declarativa.interprolog.*;

/** Remembers a set of Java objects, allowing access to them by an (integer) ID */
public class ObjectRegistry{
   	private static final int MAXOBJECTS = 2000;
    private Object objectTable[] = new Object[MAXOBJECTS]; int nObjects=0;

	public Object getRealJavaObject(InvisibleObject o){
		return getRealJavaObject(o.ID);
	}
	
	public Object getRealJavaObject(int ID){
		if (ID<0 | ID>nObjects-1) 
			throw new RuntimeException("Bad object ID in ObjectRegistry");
		return objectTable[ID];
	}
	
	public int registerJavaObject(Object x){
		int i = getObjectID(x);
		if (i>=0) return i;
		if (nObjects>=MAXOBJECTS) 
			throw new IPException("Too many Java objects in ObjectRegistry");
		if (x==null)
			throw new IPException("Null object in ObjectRegistry");
		objectTable[nObjects++] = x;
		return nObjects-1;
	}
	
	private int getObjectID(Object x){
		int i = 0;
		while (i<nObjects)
			if (objectTable[i]==x) return i;
			else i++;
		return -1;
	}
}
