/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog;
import java.io.Serializable;
import com.declarativa.interprolog.util.*;
/** The class wrapping the information that will allow Prolog to later specify Java objects. 
An optional Prolog-friendly name can be specified.
This class must be ***strictly in sync*** with the corresponding Prolog code, 
which is critical to InterProlog's startup, cf. predicate ipProcessExamples/1 in interprolog.P. */
public class ObjectExamplePair implements Serializable{
	String name;
	Object A,B;
	public ObjectExamplePair(Object A){this(null,A,A);}
	public ObjectExamplePair(String n,Object A){this(n,A,A);}
	public ObjectExamplePair(Object A,Object B){this(null,A,B);}
	/** Create an object pair, adequate for later teaching it to a Prolog engine and have this
	produce an ipObjectSpec fact on the Prolog side.
@param n Optional name for the class, as viewed from the Prolog side. 
@param A An object instance. 
@param n Another object instance, may or not be different from the first. 
@see com.declarativa.interprolog.PrologEngine#teachMoreObjects(ObjectExamplePair[])
	*/
	public ObjectExamplePair(String n,Object A,Object B){
		if (A==null | B==null) 
			throw new Error("Bad ObjectExamplePair, at least first object must be non-null");
		if (n==null) 
			name=A.getClass().getName();
		else
			name=n;
		if (A.getClass()!=B.getClass())
			throw new IPException("Bad ObjectExamplePair, objects must belong to same class");
		if (!(A instanceof Serializable)|| !(B instanceof Serializable))
			throw new IPException("Bad ObjectExamplePair, objects must be Serializable");
		this.A=A; this.B=B;
	}
	
	public String toString(){
		return "ObjectExamplePair for class named "+name+". A:"+A+" B:"+B;
	}
}

