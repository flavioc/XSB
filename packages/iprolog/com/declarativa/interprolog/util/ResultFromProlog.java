/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.util;
import java.io.Serializable;
/** Used to serialize results for deterministicGoal */
public class ResultFromProlog implements Serializable{
	/** Same as passed in GoalFromJava*/
	public int timestamp;
	/** Goal has succeeded */
	public boolean succeeded;
	/** Object array corresponding to the result variable list */
	public Object[] rVars;
	/** Error message, null if none */
	public String error;
	public ResultFromProlog(int t,boolean s,int size,String e){
		rVars = new Object[size];
		timestamp=t; succeeded=s; error=e;
	}
	public String toString(){
		return "ResultFromProlog: timestamp=="+timestamp+", error=="+error;
	}
}

