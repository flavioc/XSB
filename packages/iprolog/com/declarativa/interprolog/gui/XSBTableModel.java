/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.gui;
import com.declarativa.interprolog.*;
/** Swing model for full contents of a XSB table
*/
public class XSBTableModel extends TermTreeModel{
	String state;
	public XSBTableModel(TermModel root,TermTreeModel[] children,String state){
		super(root,children);
		this.state=state;
	}
	public static ObjectExamplePair example(){
		return new ObjectExamplePair("XSBTableModel",new XSBTableModel(new TermModel("A"),null,"complete"));
		// we don't mind passing a pair of similar objects, because in this case we'll be using ipObjectTemplate,
		// rather than ipObjectSpec
	}
}
