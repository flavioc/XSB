/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog;
/** If you're interested in Prolog's textual output, implement this interface on your class and make your 
object a listener to a SubprocessEngine */
public interface PrologOutputListener {
	/** Prolog stream output was sent */
	public void print(String s);
}
