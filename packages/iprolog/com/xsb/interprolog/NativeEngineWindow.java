/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com, http://www.xsb.com
** Copyright (C) XSB Inc., USA, 2001
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.xsb.interprolog;
import com.declarativa.interprolog.*;
import com.declarativa.interprolog.util.*;
import com.declarativa.interprolog.gui.*;
import javax.swing.*;

/** A ListenerWindow for a NativeEngine */
public class NativeEngineWindow extends ListenerWindow {
	
	public NativeEngineWindow(NativeEngine e){
		this(e,true);
		setTitle("NativeEngine listener");
	}
	public NativeEngineWindow(NativeEngine e,boolean autoDisplay){
		super(e,autoDisplay);
		prologInput.setToolTipText("Prolog goal, sent when you press enter");
		prologOutput.setToolTipText("Goals and their first solutions");
	}
	public void sendToProlog(){
		final String goal = prologInput.getText().trim();
		if (goal.length()==0 || goal.endsWith(".")) {
			beep();
			prologOutput.append("Goal must be nonempty and without trailing '.'\n");
			return;
		}
		prologOutput.append(goal+"\n");
		addToHistory();
		// Better not call Prolog in the event thread, it would break modal dialogs
		Thread t = new Thread(){
			String result;
			public void run(){
				try{
					Object[] bindings = engine.deterministicGoal(goal,null);
					if (bindings==null) result = "FAILED\n";
					else result = bindings[0].toString()+"\n";
					
				} catch (IPInterruptedException e){
					result = "Goal was interrupted!";
				} catch (IPAbortedException e){
					result = "Goal was aborted!";
				}
				SwingUtilities.invokeLater(new Runnable() {
					public void run(){
					prologOutput.append(result+"\n");
						scrollToBottom();
						focusInput();
					}
				});
			}
		};
		t.start();
	}
	
	/** Useful for launching the system, by passing the full Prolog directory path and 
	optionally extra arguments, that are passed to the Prolog command */
	public static void main(String[] args){
		commonMain(args);	       
		new NativeEngineWindow(new NativeEngine(prologStartCommand,debug));
	}

}
