/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog;
/** If you're interested in knowing that a (Java) term representation has changed implement this.
Prior to listening, a TermModel must have its root set with setRoot(). 
All changes to TermModel node and children variables will be reported */
public interface TermModelListener{
	public void termChanged(TermModel source);
}