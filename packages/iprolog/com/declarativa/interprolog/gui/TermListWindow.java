/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.gui;
import javax.swing.*;
import javax.swing.table.*;
import javax.swing.event.*;
import java.awt.event.*;

/** Displays a list of Prolog terms
*/
public class TermListWindow extends JFrame{
	public TermListWindow(TermListModel ptm){
		super();
		setTitle("List of "+ ptm.getSize()+" terms");
		final JList list = new JList(ptm);
		JScrollPane scrollPane = new JScrollPane(list);
		getContentPane().add("Center",scrollPane);
		final TermListModel theModel=ptm;
     	list.addMouseListener(new MouseAdapter() {
     		public void mouseClicked(MouseEvent e) {
         		if (e.getClickCount() == 2) {
             		int index = list.locationToIndex(e.getPoint());
             		if (index != -1) new TermModelWindow(theModel.terms[index]);
     			}
     		};
     	});
		setVisible(true);
		// System.out.println("Created window for TermListWindow "+ptm);
	}
}
