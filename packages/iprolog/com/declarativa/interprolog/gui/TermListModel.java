/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.gui;
import com.declarativa.interprolog.*;
import javax.swing.*;
import javax.swing.event.*;
import java.io.Serializable;


/** The Swing model to support a JList displaying Prolog terms
*/
public class TermListModel implements Serializable,ListModel{
	TermModel[] terms;
	
	public static ObjectExamplePair example(){
		TermModel[] Atuples = new TermModel[1];
		Atuples[0]=new TermModel("c");
		return new ObjectExamplePair("TermListModel",
			new TermListModel(Atuples),
			new TermListModel(new TermModel[0])
			);
	}
	
	public void addListDataListener(ListDataListener l) {
		//System.out.println("Should add a ListDataListener...");
	}
	public void removeListDataListener(ListDataListener l) {
		//System.out.println("Should remove a ListDataListener...");
	}
	public Object getElementAt(int index) {
		return terms[index];
	}
	public int getSize() {
		return terms.length;
	}
	public TermListModel(TermModel[] terms){
		if (terms == null) 
			throw new RuntimeException("The TermListModel constructor needs a non-null argument");
		for (int t=0; t<terms.length; t++) {
			if (terms[t]==null)
				throw new RuntimeException("Null term in TermListModel term "+t);
		}
		this.terms=terms;
	}
}