/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.util;
import java.io.Serializable;
/** Represents a goal from Java, called through deterministicGoal */
public class GoalFromJava implements Serializable{
	int timestamp;
	String G /* includes OVar, RVars*/;
	Object[] objects;
	public GoalFromJava(int t,String G,String OVar,Object[] objects,String RVars){
		this.objects=objects;
		if (RVars==null) RVars="null";
		this.G="gfj( ( "+G+" ), ( "+OVar+" ), ("+RVars+") )."; timestamp=t; 
	}
}
