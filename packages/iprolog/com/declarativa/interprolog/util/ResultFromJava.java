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
/** Used to serialize results for javaMessage */
public class ResultFromJava implements Serializable{
	int timestamp; // same as of the corresponding MessageFromProlog
	Object result;
	/** exception thrown directly by java.lang.reflect.Method#invoke(). 
	It may eventually encapsulate an exception thrown by the invoked method
	@see      java.lang.reflect.Method#invoke(Object,Object[])*/
	public Object /*instead of Exception... to avoid serialization difficulties...*/ exception;
	/** So Prolog may get the new state of the (object) arguments */
	Object[] arguments;
	public ResultFromJava(int t,Object r,Object e,Object[] a){
		timestamp=t; result=r; exception=e; 
		if (a==null) arguments = new Object[0];
		else arguments=a;
	}
	public String toString(){
		StringBuffer args = new StringBuffer(500);
		for (int i=0; i<arguments.length; i++)
			args.append(PrologEngine.nl+"arguments["+i+"]="+arguments[i]);
		return "timestamp="+timestamp+PrologEngine.nl+"result="+result+PrologEngine.nl+"exception="+exception+args;
	}
}

