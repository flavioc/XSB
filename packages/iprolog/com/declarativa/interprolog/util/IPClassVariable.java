/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.util;
import java.io.Serializable;
/** Represents a Java class variable; allow the Prolog side to conveniently refer any class variable */
public class IPClassVariable implements Serializable{
	public String className;
	public String variableName;
	public IPClassVariable(String c,String v){
		if (c==null || v==null) throw new Error("null argument in IPClassVariable");
		className=c; variableName=v;
	}
	public String toString(){return "IPClassVariable:"+className+"."+variableName;}
}

