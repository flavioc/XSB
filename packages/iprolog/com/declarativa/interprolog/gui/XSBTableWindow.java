/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.gui;
import java.util.*;
import java.text.*;
import javax.swing.*;

/** Shows a XSB Prolog table
*/
public class XSBTableWindow extends JFrame{
	public XSBTableWindow(XSBTableModel m, Object[] columnTitles){
		super();
		//System.out.println("Creating XSBTableWindow for "+m);
		getContentPane().add("Center",new TermTreePane(m,columnTitles));
		String time = DateFormat.getTimeInstance().format(new Date());
		setTitle(m.node.toString()+": table at "+time);
		getContentPane().add("North",new JLabel("state was "+m.state/*,SwingConstants.CENTER*/));
		setSize(300,200);
		setVisible(true);
	}
	
}
