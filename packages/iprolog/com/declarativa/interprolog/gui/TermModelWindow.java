/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.gui;
import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.io.Serializable;
import com.declarativa.interprolog.*;

/** Displays a Prolog term, using a JTree
*/
public class TermModelWindow extends JFrame{
	TermModel model; PrologEngine engine;
	public TermModelWindow(TermModel tm){
		this(tm,null);
	}
	public TermModelWindow(TermModel tm,PrologEngine e){
		super("A term");
		this.model=tm; this.engine=e;
		JTree termTree = new JTree(tm);
		termTree.setCellRenderer(new TermTreeCellRenderer());
		getContentPane().add("Center",new JScrollPane(termTree));
		setSize(200,200);
		JMenuBar mb; 
		mb = new JMenuBar(); setJMenuBar(mb);
		JMenu testMenu = new JMenu("Tests"); mb.add(testMenu);
		JMenuItem test = new JMenuItem("Assert foobar(T)..."); testMenu.add(test);
		test.addActionListener(new ActionListener(){
			Object[] objects = {model};
			public void actionPerformed(ActionEvent e){
				System.out.println("Trying to recover and assert...");
				Object[] bindings = engine.deterministicGoal("assert(foobar(TM))", "TM", objects, null);
				System.out.println("success=" + (bindings!=null));
			}
		});

		setVisible(true);
	}
}

/** Adapted from: 
	http://developer.javasoft.com/developer/onlineTraining/swing2/exercises/TreeRender/Solution/TreeRender.java
*/
class TermTreeCellRenderer extends JLabel implements TreeCellRenderer {

    // Create instance variable to save selected state
    private boolean selected;

    public Component  getTreeCellRendererComponent(JTree tree,
    	Object value, boolean selected, boolean expanded,boolean leaf, int row, boolean hasFocus) {

		if (!(value instanceof TermModel))
			throw new RuntimeException("getTreeCellRendererComponent demands a TermModel");
			
		TermModel term = (TermModel)value;
      	// Save selected state
      	this.selected = selected;		
      	if (expanded) {
        	setText(term.node.toString());
      	} else {
        	setText(term.toString());
      	}
      	doLayout(); // Make sure our size accomodates current text ??
      	return this;
   	}

    public void paint (Graphics g) {

      	// Change background color based on selected state
        Color background = (selected ? Color.lightGray : Color.white);
        g.setColor(background);

      	g.fillRect (getX(), getY(), getWidth(), getHeight());
      	super.paint (g);
   	}

}
