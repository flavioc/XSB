/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.gui;
import com.declarativa.interprolog.*;
import java.io.Serializable;

/** Swing model for a multiple-pane hierarchical browser, defined by a Prolog tree
*/
public class TermTreeModel implements Serializable { 
	TermModel node;
	TermTreeModel[] children;
	
	public static ObjectExamplePair example(){
		TermTreeModel[] fooB = new TermTreeModel[1];
		fooB[0]=new TermTreeModel(new TermModel(new Integer(2)),null);
		return new ObjectExamplePair("TermTreeModel",
			new TermTreeModel(new TermModel(new Integer(1)),null),
			new TermTreeModel(new TermModel("term",new TermModel[1]),fooB)
			);
	}
	 
	public TermTreeModel(TermModel root,TermTreeModel[] children){
		if (children==null) this.children = new TermTreeModel[0];
		else this.children=children;
		node = root;
	}
	int depth(){
		if (children.length==0) return 1;
		int maxdepth=0;
		for(int c=0; c<children.length; c++){
			int cache = children[c].depth();
			if (cache>maxdepth) maxdepth=cache;
		}
		return maxdepth+1;
	}
	TermListModel topList(){
		TermModel[] temp = new TermModel[children.length];
		for (int i=0; i<children.length;i++)
			temp[i]=children[i].node;
		return new TermListModel(temp);
	}
}