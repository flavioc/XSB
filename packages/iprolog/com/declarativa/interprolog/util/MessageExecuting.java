/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.util;
import com.declarativa.interprolog.*;

/** Object to help coordinate a javaMessage execution in the Java side. Current policy spawns a thread for each javaMessage, should probably be more economic*/
public class MessageExecuting extends Thread{
	PrologEngine engine;
	private MessageFromProlog m;
	private ResultFromJava result;
	private boolean ended;
	
	public MessageExecuting(MessageFromProlog m,PrologEngine engine){
		this.m = m; 
		result=null;
		this.engine=engine;
		ended=false;
	}
	
	private void setResult(ResultFromJava result){
		if (this.result!=null) throw new IPException("Inconsistency in MessageExecuting");
		this.result=result;
		ended = true;
	}
	public void run(){
		setResult(engine.doCallback(m));
	}
	public boolean hasEnded(){
		return ended;
	}
	public ResultFromJava getResult(){
		if (!hasEnded()) throw new IPException("bad use of MessageExecuting");
		return result;
	}
	public int getTimestamp(){
		return m.timestamp;
	}
}