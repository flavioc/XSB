/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.util;
import java.io.*;
/** Represents a Java class object; avoids the need to use a full blown object specifier for the real
Java class object (a Class instance) on the Prolog side, by relying on the Java call-back mechanism to interpret the contents
of this object at callback time */
public class IPClassObject implements Serializable{
	public String classname;
	public IPClassObject(String s){
		if (s==null) throw new Error("null classname in IPClassObject");
		classname=s;
	}
	public String toString(){return "IPClassObject:"+classname;}
}

