/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.gui;
import com.declarativa.interprolog.*;

/** A ListenerWindow for a SubprocessEngine */
public class SubprocessEngineWindow extends ListenerWindow implements PrologOutputListener{
	
	public SubprocessEngineWindow(SubprocessEngine e){
		this(e,true);
		setTitle("SubprocessEngine listener");
	}
	public SubprocessEngineWindow(SubprocessEngine e,boolean autoDisplay){
		super(e,autoDisplay);
		((SubprocessEngine)engine).addPrologOutputListener(this); // so we get output and prompt "events"
	}
	public void sendToProlog(){
		String goal = prologInput.getText();
		prologOutput.append(goal+"\n");
		((SubprocessEngine)engine).sendAndFlushLn(goal);
		focusInput();
		addToHistory();
	}
	
	// PrologOutputListener method:
	public void print(String s){
		if (debug) System.out.println("print("+s+")");
		prologOutput.append(s);
		scrollToBottom();
	} 
	/** Useful for launching the system, by passing the full Prolog executable path and 
	optionally extra arguments, that are passed to the Prolog command */
	public static void main(String[] args){
		commonMain(args);
		new SubprocessEngineWindow(new SubprocessEngine(prologStartCommand,debug));
	       
	}

}
