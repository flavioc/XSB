/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.util;
import java.io.Serializable;
import com.declarativa.interprolog.*;

/** Represents a message from Prolog, performed by predicate javaMessage */
public class MessageFromProlog implements Serializable{
	public int timestamp;
	public Object target; // To contain an InvisibleObject if target (de)serialization is not desired
	public String methodName;
	public Object[] arguments;
	public boolean returnArguments;
	public String toString(){
		StringBuffer args = new StringBuffer(500);
		for (int i=0; i<arguments.length; i++)
			args.append(PrologEngine.nl+"arguments["+i+"]="+arguments[i]);
		return "MessageFromProlog, timestamp="+timestamp+PrologEngine.nl+"target="+target+
			PrologEngine.nl+"methodName="+methodName+args;
	}
}

