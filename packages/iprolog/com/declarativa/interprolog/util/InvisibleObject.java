/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.util;
import com.declarativa.interprolog.*;
import java.io.Serializable;

/** Instances represent objects which are not serialized to/from Prolog, 
	and that are kept in a table by a Prolog engine */
public class InvisibleObject implements Serializable{
	int ID;
	public InvisibleObject(int ID){this.ID=ID;}
	public String toString(){return "InvisibleObject:ID="+ID;}
}

