/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.gui;
import com.declarativa.interprolog.*;
import java.awt.event.*;
import java.awt.*;
import javax.swing.*;
/** A PrologEventBroker can listen to ActionEvents, and calls a Prolog goal to handle them.
If a goal is not specified, Event(ID) goals will be called instead, where ID is the reference of 
the event source object in the engine's knownObjects table.
If a JComponent is specified, its tooltip text will be set; if not, the first JComponent sending an event
will have its tooltip text set */

public class PrologEventBroker implements ActionListener {
	PrologEngine engine;
	String goal;
	JComponent component;
	public PrologEventBroker(PrologEngine e,String g){
		engine=e; 
		goal=g;
		component=null;
		maySetTooltipText("Calls an Event(this) goal");
	}
	public PrologEventBroker(PrologEngine e,Object g){
		engine=e; 
		if (g!=null) goal=g.toString();
		component=null;
		maySetTooltipText("Calls an Event(this) goal");
	}
	public PrologEventBroker(PrologEngine e){
		this(e,null);
	}
	void maySetTooltipText(String defaultTip){
		if (component!=null) {
			if (goal!=null)
				component.setToolTipText("Calls "+goal);
			else
				component.setToolTipText(defaultTip);
		}
	}
	public void actionPerformed(ActionEvent e){
		String thisGoal; Object theSource = e.getSource();
		if (component==null) {
			if (theSource instanceof JComponent) component=(JComponent)theSource;
		}
		if (goal!=null) thisGoal=goal;
		else {
			thisGoal=PrologEngine.shortClassName(e.getClass()) + "("+
				engine.registerJavaObject(theSource) + ")";
		}
		maySetTooltipText("Last called "+thisGoal);
		engine.deterministicGoal(thisGoal);
	}
}
