/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.gui;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;

/** Shows a multiple-pane hierarchical browser, defined by a Prolog tree
*/
public class TermTreeWindow extends JFrame{
	public TermTreeWindow(TermTreeModel m){
		this(m,null);
	}
	public TermTreeWindow(TermTreeModel m, Object[] levelTitles){
		super();
		System.out.println("Creating window for "+m);
		getContentPane().add("Center",new TermTreePane(m,levelTitles));
		if (levelTitles.length>0) setTitle(levelTitles[0].toString());
		else setTitle(m.node.toString());
		setSize(400,200);
		setVisible(true);
	}
	
}

