/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.util;
import java.io.Serializable;
import com.declarativa.interprolog.*;
/** TermModel node for a free Prolog variable. Each free Prolog variable at TermModel construction time
corresponds to a globally unique Integer number, as enforced by ip_inc_var_counter 
in interprolog.P. */
public class VariableNode implements Serializable{
	Integer number;
	public static ObjectExamplePair example(){
		return new ObjectExamplePair("VariableNode",
			new VariableNode(1),
			new VariableNode(2)
			);
	}
	VariableNode(int n){
		number = new Integer(n);
	}
	
	public String toString(){
		return "Var"+number;
	}
	public boolean equals(Object x){
		return (x.getClass()==getClass() && number.equals( ((VariableNode)x).number ));
	}
}
