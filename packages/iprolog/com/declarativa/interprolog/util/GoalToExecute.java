/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.util;
import com.declarativa.interprolog.*;

/** A goal scheduled to execute in Prolog whenever possible */
public class GoalToExecute{
	private GoalFromJava goal;
	private ResultFromProlog result;
	private boolean executing;
	private boolean ended;
	private boolean firstGoalStatus = false;
	
	public GoalToExecute(GoalFromJava goal){
		this.goal = goal; 
		result=null; ended=false; executing=false;
	}
	
	/** Obtain result for a Prolog goal, blocking until it is available */
	public synchronized ResultFromProlog waitForResult(){
		if (ended) return result;
		System.out.println("---> Now it will hang here ....");
		try { wait();}
		catch(InterruptedException e){throw new IPException("Unexpected:"+e);}
		if (result==null) throw new IPException("Inconsistency in GoalToExecute");
		return result;
	}
	
	public synchronized void setResult(ResultFromProlog result){
		if (this.result!=null || hasEnded()) {
			throw new IPException("Inconsistency in GoalToExecute");
		}
		this.result=result;
		ended=true;
		notify();
	}
	
	public boolean wasInterrupted(){
		return hasEnded() && "interrupted".equals(result.error);
	}

	public boolean wasAborted(){
		return hasEnded() && "aborted".equals(result.error);
	}

	public synchronized void interrupt(){
		raiseError("interrupted");
	}
	
	public synchronized void abort(){
		raiseError("aborted");
	}
	
	private void raiseError(String s){
		if (result==null) result = new ResultFromProlog(-1,false,0,null);
		result.error=s;
		ended=true;
		notify();
	}
	
	public GoalFromJava getGoal(){ return goal;}
	
	public void prologWasCalled(){
		if (executing) throw new IPException("Bad use of prologWasCalled");
		executing=true;
	}
	
	public boolean hasStarted(){
		return executing;
	}
	
	public boolean hasEnded(){
		return ended;
	}
	
	public int getTimestamp(){return goal.timestamp;}
	
	public void setFirstGoalStatus(){
		firstGoalStatus = true;
	}
	
	public boolean isFirstGoal(){
		return firstGoalStatus;
	}
	
	public String toString(){
		return "ResultFromProlog: timestamp=="+getTimestamp();
	}
}
