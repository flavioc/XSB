/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/

package com.declarativa.interprolog;
import java.util.*;
import com.declarativa.interprolog.util.*;

/** Represents a set of Prolog operator declarations, to provide TermModel with knowledge
for toString()*/

public class PrologOperatorsContext {
	Hashtable prefixOperators,postfixOperators,infixOperators;
	boolean prefixOperator(Object node){
		return prefixOperators.containsKey(node);
	}
	boolean postfixOperator(Object node){
		return postfixOperators.containsKey(node);
	}
	boolean infixOperator(Object node){
		return infixOperators.containsKey(node);
	}
	PrologOperatorsContext(PrologOperator[] operators){
		int nOperators=0;
		if (operators!=null) nOperators=operators.length;
		prefixOperators = new Hashtable(nOperators+1);
		postfixOperators = new Hashtable(nOperators+1);
		infixOperators = new Hashtable(nOperators+1);
		for (int o=0;o<nOperators;o++){
			int type=operators[o].type;
			Hashtable placeholder = null;
			if (type==fx||type==fy) 
				placeholder=prefixOperators;
			else if (type==xfx||type==xfy||type==yfx) 
				placeholder=infixOperators;
			else if (type==xf||type==yf)
				placeholder=postfixOperators;
			placeholder.put(operators[o].name,operators[o]);
		}
	}
	PrologOperatorsContext(){
		this(standardXSBOperators);
	}
	static final int xfx=1,xfy=2,yfx=3,fx=4,fy=5,xf=6,yf=7;
	static final PrologOperator[] standardXSBOperators={
		//current_op(P,T,Name), write(prologOperator(P,T,Name)), nl, fail. ...
		new PrologOperator(1200,xfx,":-"),
		new PrologOperator(1200,xfx,"-->"),
		new PrologOperator(1200,fx,"?-"), 
		new PrologOperator(1198,xfx,"::-"), 
		new PrologOperator(1150,fx,"hilog"),
		new PrologOperator(1150,fx,"dynamic"),new PrologOperator(1150,fx,"multifile"),
		new PrologOperator(1100,xfy,";"),
		new PrologOperator(1100,fx,"table"),
		new PrologOperator(1100,fx,"edb"),
		new PrologOperator(1100,fy,"index"),
		new PrologOperator(1100,fy,"ti"), 
		new PrologOperator(1100,fy,"ti_off"),
		new PrologOperator(1100,fx,"mode"),
		new PrologOperator(1100,fx,"export"),
		new PrologOperator(1100,fx,"parallel"),
		new PrologOperator(1100,fx,"local"),
		new PrologOperator(1050,fy,"import"), 
		new PrologOperator(1050,xfx,"from"),
		new PrologOperator(1050,xfy,"->"),
		new PrologOperator(1000,xfy,","),
		new PrologOperator(900,fy,"not"),
		new PrologOperator(900,fy,"\\+"),
		new PrologOperator(900,fy,"spy"),
		new PrologOperator(900,fy,"nospy"),
		new PrologOperator(700,xfx,"="),
		new PrologOperator(700,xfx,"\\="),
		new PrologOperator(700,xfx,"=="),
		new PrologOperator(700,xfx,"\\=="),
		new PrologOperator(700,xfx,"@<"),
		new PrologOperator(700,xfx,"@=<"),
		new PrologOperator(700,xfx,"@>"),
		new PrologOperator(700,xfx,"@>="),
		new PrologOperator(700,xfx,"=.."),
		new PrologOperator(700,xfx,"^=.."),
		new PrologOperator(700,xfx,"is"),
		new PrologOperator(700,xfx,"=:="),
		new PrologOperator(700,xfx,"=\\="),
		new PrologOperator(700,xfx,"<"),
		new PrologOperator(700,xfx,"=<"),
		new PrologOperator(700,xfx,">"),
		new PrologOperator(700,xfx,">="),
		new PrologOperator(661,xfy,"."),
		new PrologOperator(600,xfy,":"),
		new PrologOperator(500,yfx,"+"),
		new PrologOperator(500,yfx,"-"),
		new PrologOperator(500,yfx,"/\\"),
		new PrologOperator(500,yfx,"\\/"),
		new PrologOperator(500,fx,"+"),
		new PrologOperator(500,fx,"-"),
		new PrologOperator(400,yfx,"*"),
		new PrologOperator(400,yfx,"/"),
		new PrologOperator(400,yfx,"//"),
		new PrologOperator(400,yfx,"mod"),
		new PrologOperator(400,yfx,"<<"),
		new PrologOperator(400,yfx,">>"),
		new PrologOperator(400,yfx,"\\"),
		new PrologOperator(200,xfy,"^")
		};
	
	static class PrologOperator{
		int precedence;
		int type;
		String name;
		PrologOperator(int p,int t,String n){
			if (t<xfx||t>yf||p<1||p>1200||n==null)
				throw new IPException("Bad arguments in PrologOperator constructor");
			precedence=p; type=t; name=n;
		}
	}
}
