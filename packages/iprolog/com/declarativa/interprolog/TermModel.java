/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog;
import com.declarativa.interprolog.util.*;
import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;
import java.io.*;
import java.util.*;

/** Represents a Prolog term, as a tree of TermModel objects, each containing a term node and a children list.
 Implements TreeModel, therefore easily supporting display in a JTree.
   It includes the funcionality of Prolog's Edinburgh syntax write within toString(); if you wish to reflect
  operator declarations dynamically you should provide a different PrologOperatorsContext object  */
public class TermModel implements Serializable,TreeModel{
	/** public for convenience, but should not be set outside this class */
	public Object node;
	/** public for convenience, but should not be set outside this class;
	children == null means children == new TermModel[0]*/
	public TermModel[] children; 
	private transient Vector treeListeners=null /*new Vector() here doesn't work on deserialization...*/;
	private transient Vector termListeners;
	/** The TermModel containing this; relevant to define variable scope. For this to contain the root, 
	the root TermModel must have been messaged once with setRoot() */
	public transient TermModel root; 
	
	static PrologOperatorsContext defaultOperatorContext = new PrologOperatorsContext();
	
	public static ObjectExamplePair example(){
		return new ObjectExamplePair("TermModel",
			new TermModel(new Integer(1),(TermModel[])null),
			new TermModel("functor",new TermModel[1])
			);
	}
	
	/** Clones by serialization, to keep variable bindings; these should form a closed graph
	within this term, otherwise the new term instance would contain dangling references */
	public Object clone(){
		Object x = null;
		try{
			ByteArrayOutputStream buffer = new ByteArrayOutputStream(100);
			ObjectOutputStream oos = new ObjectOutputStream(buffer);
			oos.writeObject(this); oos.close();
			ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray()));
			x = ois.readObject();
			ois.close();
		} catch (Exception e){
			throw new IPException("Failed to clone");
		}
		return x;
	}
	
	/** True if both terms are structurally similar with equal nodes */
	public boolean equals(Object x){
		if (!(x instanceof TermModel)) return false;
		TermModel tx = (TermModel)x;
		if (!(node.equals(tx.node))) return false;
		if (getChildCount()!=tx.getChildCount()) return false;
		for (int c=0; c<getChildCount(); c++)
			if (!(getChild(c).equals(tx.getChild(c)))) return false;
		return true;
	}
	
	/** Set the node, notifying TermModelListeners */
	public void setNodeValue(Object v){
		if (v!=null) {
			if (!(v instanceof Serializable))
			throw new IPException("TermModel nodes must be Serializable");
			if (!((v instanceof Number)||(v instanceof String)||(v instanceof VariableNode)))
			throw new IPException("Bad TermModel node type");
		}
		boolean changed = !(v==node);
		node=v;
		if (changed) fireTermChanged();
	}
	
	/** Set the child, notifying TermModelListeners */
	public void setChild(int index,TermModel child){
		boolean changed = !(child==children[index]);
		children[index] = child;
		if (changed) fireTermChanged();
	}
	
	/** Set the children array, notifying TermModelListeners */
	public void setChildren(TermModel[] c){
		boolean changed = !(children==c);
		children=c;
		if (changed) fireTermChanged();
	}
	
	public TermModel[] getChildren(){
		return children;
	}
	
	/** Add children to this term node; a new children array is created and old children are copied to it */
	public void addChildren(TermModel[] more){
		if (more.length==0) return;
		TermModel[] newChildren = new TermModel[getChildCount()+more.length];
		for (int i=0;i<getChildCount();i++)
			newChildren[i] = children[i];
		for (int i=getChildCount();i<getChildCount()+more.length;i++)
			newChildren[i] = more[i-getChildCount()];
		setChildren(newChildren);
	}
	
	/** Delete children with specified indexes; a new children array is created and old children are copied to it */
	public void deleteChildren(int[] less){
		if (less.length==0) return;
		TermModel[] newChildren = new TermModel[getChildCount()-less.length];
		int oldIndex=0;
		for (int newIndex=0;newIndex<newChildren.length;newIndex++){
			while (inArray(oldIndex,less)) oldIndex++;
			newChildren[newIndex] = children[oldIndex];
			oldIndex++;
		}
		setChildren(newChildren);
	}
	
	/** Removes the children in array; it finds children to remove by using the equivalent of Prolog's == */
	public void deleteChildren(TermModel[] less){
		if (less.length==0) return;
		TermModel[] newChildren = new TermModel[getChildCount()-less.length];
		int oldIndex=0;
		for (int newIndex=0;newIndex<newChildren.length;newIndex++){
			while (inArray(children[oldIndex],less)) oldIndex++;
			newChildren[newIndex] = children[oldIndex];
			oldIndex++;
		}
		setChildren(newChildren);
	}
	
	static boolean inArray(int x,int[] a){
		for(int i=0;i<a.length;i++)
			if (x==a[i]) return true;
		return false;
	}
	
	static boolean inArray(Object x,Object[] a){
		for(int i=0;i<a.length;i++)
			if (x.equals(a[i])) return true;
		return false;
	}
	
	/** Replaces all occurrences of v in this subterm by value, using setNodeValue */
	public void assignToVar(VariableNode v,Object value){
		if (v.equals(node)) {
			setNodeValue(value);
		}
		for (int i=0; i<getChildCount(); i++)
			children[i].assignToVar(v,value);
	}

	/** Set the root variables of nodes in this subterm to refer this term node as their root */
	public void setRoot(){
		setRoot(this);
	}
		
	/** Set the root variables of nodes in this subterm to refer r as their root */
	public void setRoot(TermModel r){
		if (root!=null) return; // subtree already set, ignore "new" root
		root=r;
		propagateRoot();
	}
	
	/** This node is its own root */
	public boolean isRoot(){ return root==this;}
	
	protected void propagateRoot(){
		for(int i=0;i<getChildCount();i++)
			children[i].setRoot(root);
	}
	
	/** Return a Node(_,..._) String */
	public String getTemplate(){
		StringBuffer temp = new StringBuffer(node.toString());
		for (int i=0; i<getChildCount(); i++) {
			if (i==0) temp.append("(");
			if (i>0) temp.append(",");
			temp.append("_");
		}
		if (getChildCount()>0) temp.append(")");
		return temp.toString();
	}

	/** Return a node/arity String */
	public String getFunctorArity(){
		return node+"/"+getChildCount();
	}
	
	// Swing TreeModel methods:
	
	/** @see javax.swing.tree.TreeModel */
	public Object getRoot()	{
		return this;
	}
	/** @see javax.swing.tree.TreeModel */
	public Object getChild(Object parent,int index) {
		return ((TermModel)parent).children[index];
	}
	/** @see javax.swing.tree.TreeModel */
    public int getChildCount(Object parent){
    	TermModel[] c = ((TermModel)parent).children;
    	if (c==null) return 0;
		else return c.length;
    }
	/** @see javax.swing.tree.TreeModel */
    public boolean isLeaf(Object node){
    	return (getChildCount(node) == 0);
    }
	/** @see javax.swing.tree.TreeModel */
    public void valueForPathChanged(TreePath path,Object newValue) {
    	throw new RuntimeException("I can not handle term edition!");
    }   
	/** @see javax.swing.tree.TreeModel */
    public int getIndexOfChild(Object parent,Object child){
    	TermModel[] c = ((TermModel)parent).children;
    	if (c==null) return 0;
    	for (int i=0; i<c.length; i++)
    		if (c[i]==child) return i+1;
    	return 0;
    }
	/** @see javax.swing.tree.TreeModel */
	public void addTreeModelListener(TreeModelListener l){
		if (treeListeners==null) treeListeners = new Vector();
		treeListeners.addElement(l);
	}
	/** @see javax.swing.tree.TreeModel */
	public void removeTreeModelListener(TreeModelListener l){
		if (!treeListeners.removeElement(l)) 
		throw new IPException("Bad removal of TermModel listener");
	}
	
	// simpler versions
	public Object getChild(int index) {
		return getChild(this,index);
	}	
    public int getChildCount(){
		return getChildCount(this);
    }
	public boolean isLeaf(){
    	return isLeaf(this);
    }
	
	/** Start notifying listener l of changes to this term. The term is considered changed iff either a node or children
	is set to a different object; a nonreported change is the setting of a same object as node/children, 
	not the setting of an "equals" object */
	public void addTermModelListener(TermModelListener l){
		if (!isRoot())
			throw new IPException("addTermModelListener can be sent to the TermModel root only");
		if (termListeners==null) termListeners = new Vector();
		termListeners.addElement(l);
	}
	public void removeTermModelListener(TermModelListener l){
		if (!termListeners.removeElement(l)) 
		throw new IPException("Bad removal of TermModelListener");
	}
	public void fireTermChanged(){
		if (isRoot()) {
			if (termListeners!=null)
				for (int l=0;l<termListeners.size();l++)
					((TermModelListener)termListeners.elementAt(l)).termChanged(this);
		} else if (root!=null) root.fireTermChanged();
	}
    // two convenience methods for simple node searching
    
   	public static TreePath findPathForNode(String label,TermModel tree,boolean exactMatch){
		Vector subtrees = new Vector();
		foundPathForNode(label, tree, exactMatch, subtrees);
		if (subtrees.size()==0) return null;
		Object[] path = new Object[subtrees.size()];
		for (int n=0; n<path.length; n++)
			path[n] = subtrees.elementAt(path.length-n-1);
		return new TreePath(path);				
	}
	
	static boolean foundPathForNode(String label,TermModel tree,boolean exactMatch,Vector bag){
		if ((exactMatch?tree.node.equals(label):tree.node.toString().startsWith(label))){
			bag.addElement(tree);
			return true;
		} else{
			for (int c=0;c<tree.getChildCount();c++){
				if (foundPathForNode(label,(TermModel)(tree.getChild(c)),exactMatch,bag)){
					bag.addElement(tree); return true;
				}
			}
			return false;
		}
	}
	
	public TermModel(){}
	
	public TermModel(Object n){this(n,(TermModel[])null);}
	
	public TermModel(Object n,TermModel[] c){
		children=c;
		node=n; 
	}
	
	public TermModel(Object n,Vector v){
		node=n; 
		if (v.size()==0) children=null;
		else {
			children = new TermModel[v.size()];
			for(int c=0;c<children.length;c++)
				children[c] = (TermModel)v.elementAt(c);
		}
	}
	
	public static TermModel makeList(TermModel[] terms){
		if (terms==null) return new TermModel(".");
		else return makeList(0,terms);
	}
	
	public static TermModel makeList(Vector terms){
		if (terms==null) return new TermModel(".");
		else return makeList(0,terms);
	}
	
	protected static TermModel makeList(int t, TermModel[] terms){
		TermModel[] cc = new TermModel[2];
		cc[0] = terms[t];
		if (t == terms.length - 1) {
			cc[1] = new TermModel("[]");
		} else {
			cc[1] = makeList(t+1,terms);
		}
		return new TermModel(".",cc);
	}
	
	protected static TermModel makeList(int t, Vector terms){
		TermModel[] cc = new TermModel[2];
		cc[0] = (TermModel)terms.elementAt(t);
		if (t == terms.size() - 1) {
			cc[1] = new TermModel("[]");
		} else {
			cc[1] = makeList(t+1,terms);
		}
		return new TermModel(".",cc);
	}
	
	/** Assuming this is a list of numbers, returns a Vector containing one Integer for each number in the list*/
	public Vector makeIntegerVector(){
		TermModel[] elements = flatList(this);
		Vector iv = new Vector();
		for(int i=0;i<elements.length;i++)
			iv.addElement(new Integer(elements[i].intValue()));
		return iv;
	}
	
	/** Returns node object as an int, assuming it is a Number */
	public int intValue(){
		if (! isLeaf() || !(node instanceof Number))
			throw new RuntimeException("intValue() requires a Number leaf");
		return ((Number)node).intValue();
	}
	
	public String toString(){
		return toString(defaultOperatorContext);
		// return node.toString(); // For a plain JTree...
	}
	
	public String toString(PrologOperatorsContext ops){
		if (getChildCount()==0) 
			return node.toString();
		else if (isList()) return listToString(ops);
		else if (children.length==1){
			if (ops.prefixOperator(node))
				return node+" "+children[0].toString(ops);
			else if (ops.postfixOperator(node))
				return children[0].toString(ops)+" "+node;
			else
				return node+"("+children[0].toString(ops)+")";
		} else if (children.length==2 && ops.infixOperator(node)) {
		    //return children[0].toString(ops)+node+children[1].toString(ops);
		    return "("+children[0].toString(ops)+node+children[1].toString(ops)+")";
		} else { // children.lenght>=2
		    StringBuffer s= new StringBuffer(node.toString()+"("+children[0].toString(ops));
			for (int i=1;i<children.length;i++){
				s.append(","+children[i].toString(ops));
			}
			return s+")";
		}
	}
	
	public static final int listMaxLength=100;
	
	public String listToString(PrologOperatorsContext ops){
		int i;
		StringBuffer s = new StringBuffer("[");
		TermModel temp = this;
		for( i = 0 ; i < listMaxLength ; i++ ){
			s.append(temp.children[0].toString(ops)); // head
			temp = temp.children[1];
			if( ! temp.isList() ) break ; // tail is not a list
			s.append(',') ;
		}
		if( i == listMaxLength )
			s.append("...");
		else if ( ! temp.isListEnd() )
		{
			s.append('|') ;
			s.append(temp.toString(ops)); 
		}
		return s + "]";
	}
	
	public boolean isListEnd(){
		return (isLeaf() && node.equals("[]"));
	}
	
	public boolean isList(){
		// return (children.length==2 && node.equals("."));
		return node.equals(".");
	}
	
	public boolean isAtom(){
		return (isLeaf() && node instanceof String);
	}
	
	public boolean isNumber(){
		return (isLeaf() && node instanceof Number);
	}
	
	public boolean isInteger(){
		return (isLeaf() && node instanceof Integer);
	}
	
	public boolean isVar(){
		return (isLeaf() && nodeIsVar());
	}
	
	public boolean nodeIsVar(){
		return (node instanceof VariableNode);
	}
	
	/** Flattens this list into a new TermModel array, but not completely: 
	the result may still contain lists */
	public TermModel[] flatList(){
		return flatList(this);
	}
	
	/** Flattens a list into a TermModel array, but not completely: 
	the result may still contain lists */
	public static TermModel[] flatList(TermModel list){
		Vector temp = new Vector();
		flatList(list,temp);
		TermModel[] result = new TermModel[temp.size()];
		for (int i=0;i<result.length;i++)
			result[i] = (TermModel) (temp.elementAt(i));
		return result;
	}
	static void flatList(TermModel x,Vector bag){
		if (x.isList()) {
			if (x.children!=null){
				bag.addElement(x.children[0]);
				flatList(x.children[1],bag);
			} // else it's an empty list
		} else if (!x.isListEnd()) {
			bag.addElement(x);
		}
	}
	
	public static Hashtable props2Hashtable(TermModel[] terms){
		Hashtable result = new Hashtable();
		for (int t=0;t<terms.length;t++){
			TermModel term = terms[t];
			if (term.isLeaf()) result.put(term.node.toString(),term.node);
			else if ((!term.node.equals("=") || term.getChildCount()!=2)) 
				throw new RuntimeException("bad proplist");
			else result.put(term.children[0].toString(),term.children[1]);
		}
		return result;
	}
}


