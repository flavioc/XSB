/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.util;
/** Interface by which an OutputHandler client can receive output to analyse */
public interface OutputListener {
	/** nBytes new output bytes are in buffer to analyse */
	public void analyseBytes(byte[] buffer,int nbytes);
	public void streamEnded();
}
