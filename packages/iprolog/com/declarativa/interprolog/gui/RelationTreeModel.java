package com.declarativa.interprolog.gui;
import com.declarativa.interprolog.*;
import com.xsb.interprolog.*;
import com.declarativa.interprolog.util.*;
import java.util.*;
import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;
import java.awt.event.*;

public class RelationTreeModel implements TreeModel{
	protected Vector listeners;
	PrologEngine engine;
	RelationTreeNode root;
	String relationFunctor;
	
	public RelationTreeModel(PrologEngine engine,String relationFunctor,Object root){
		this.engine=engine; 
		this.root=new RelationTreeNode(this,root); 
		this.relationFunctor=relationFunctor;
	}
	
	public static void browseRelation(PrologEngine engine,String relationFunctor,Object top){
		RelationTreeModel model = new RelationTreeModel(engine,relationFunctor,top);
		JTree tree = new JTree(model);
		JFrame w = new JFrame("Relation Tree Browser ("+top+")");
		w.getContentPane().add("Center",new JScrollPane(tree));
		w.setSize(300,300);
		w.setVisible(true);
	}
	
	public static class BrowseAction extends AbstractAction{
		String relationFunctor; Object top; PrologEngine engine;
		public BrowseAction(PrologEngine engine,String relationFunctor,Object top){
			super(relationFunctor+"("+top+")");
			this.engine=engine;
			this.relationFunctor=relationFunctor;
			this.top=top;
		}
		public void actionPerformed(ActionEvent e){
			browseRelation(engine,relationFunctor,top);
		}
	}
	 	
	// Swing TreeModel methods:
	
	public Object getRoot()	{
		return root;
	}
	public Object getChild(Object parent,int index) {
		return ((RelationTreeNode)parent).getChild(index);
	}
    public int getChildCount(Object parent){
		return ((RelationTreeNode)parent).getChildCount();
    }
    public boolean isLeaf(Object node){
		return ((RelationTreeNode)node).isLeaf();
    }
    public void valueForPathChanged(TreePath path,Object newValue) {
    	throw new RuntimeException("I can not handle term edition!");
    }   
    public int getIndexOfChild(Object parent,Object child){
		return ((RelationTreeNode)parent).getIndexOfChild(child);
    }
	public void addTreeModelListener(TreeModelListener l){
		if (listeners==null) listeners = new Vector();
		listeners.addElement(l);
	}
	public void removeTreeModelListener(TreeModelListener l){
		if (!listeners.removeElement(l)) 
		throw new IPException("Bad removal of RelationTreeModel listener");
	}
	
	public static class NodeDescriptionTriple implements java.io.Serializable{
		Object node,description;
		int childCount;
	}
	public static ObjectExamplePair example(){
		return new ObjectExamplePair(new NodeDescriptionTriple());
	}
}


class RelationTreeNode {
	static int dgcalls;
	RelationTreeModel myModel;
	private Object node;
	private Object[] nodeInArray;
	private int childCountCache;
	private String toStringCache;
	private Vector childrenCache;
	
	// node is just a reference, not the node 'description', for that cf. toString()
	RelationTreeNode(RelationTreeModel model, Object node, String description, int childCount){
		myModel = model;
		if (!(node instanceof String) && !(node instanceof Number))
		throw new IPException("Bad value type for RelationTreeNode");
		this.node=node;
		nodeInArray = new Object[]{node};
		childCountCache = childCount; // -1; // nothing in cache
		toStringCache = description;
		childrenCache = null;
	}
	RelationTreeNode(RelationTreeModel model, Object node){
		this(model,node,null,-1);
	}
	int getChildCount(){
		if (childCountCache!=-1) return childCountCache;
		System.err.println("getChildCount should be cached!!!");
		//long start = System.currentTimeMillis();
		String G = "NodeArray=[Node], countSolutions("+ getRelationFunctor()+ "(_,_,Node),N), ipObjectSpec('java.lang.Integer',Int,[N],_)";
		Integer N = (Integer)getEngine().deterministicGoal(G,"NodeArray",nodeInArray,"[Int]")[0];
		childCountCache = N.intValue();
		//System.out.println("Exit getChildCount "+node+", result "+childCountCache+":"+ (System.currentTimeMillis()-start) + " mS, "+(dgcalls++)+ " dGcalls");
		dgcalls++;
		return childCountCache;
	}
	boolean isLeaf(){
		return getChildCount()==0;
	}
	Object getChild(int index){
		if (childrenCache==null) {
			childrenCache=new Vector();
			long start = System.currentTimeMillis();
			// Assuming constant ordering of solutions, e.g. no tabling
			/* Get description; to be totally cacheless we might not get the desciption
			index = index+1;
			String G = "NodeArray=[Node], get_solutionN("+ getRelationFunctor()+"(_,_,Node), "+index+",S), arg(1,S,Child), arg(2,S,Description)";
			Object[] bindings = getEngine().deterministicGoal(G,"NodeArray",nodeInArray,"[Child,Description]");
			Object child = bindings[0];
			Object description = bindings[1];
			return new RelationTreeNode(myModel,child,description.toString());
			*/
			//System.out.println("Exit getChild "+index+" of "+node+":"+ (System.currentTimeMillis()-start) + " mS, "+(dgcalls++)+ " dGcalls");
			// Get all offspring together:
			String G = "NodeArray=[Node], ";
			G += "findall( Child, (";
			G +=    getRelationFunctor()+"(C,D,Node), countSolutions("+ getRelationFunctor()+ "(_,_,C),N), ";
			G += "  ipObjectSpec('com.declarativa.interprolog.gui.RelationTreeModel$NodeDescriptionTriple',[node=C,description=D,childCount=N],Child)";
			G += "), L),";
			G += "ipObjectSpec('ArrayOfObject',L,Children)";	
			Object[] children = (Object[])getEngine().deterministicGoal(G,"NodeArray",nodeInArray,"[Children]")[0];
			for (int i=0;i<children.length;i++) {
				RelationTreeModel.NodeDescriptionTriple c = (RelationTreeModel.NodeDescriptionTriple)children[i];
				childrenCache.addElement(new RelationTreeNode(myModel,c.node,c.description.toString(),c.childCount));
			}
			System.out.println("Exit getChild "+index+" of "+node+":"+ (System.currentTimeMillis()-start) + " mS, "+(dgcalls++)+ " dGcalls");
		}
		return childrenCache.elementAt(index); // hopefully consistent with getChildCount()'s separate Prolog computation...
	}
	int getIndexOfChild(Object child){
		System.out.println("getIndexOfChild");
		//long start = System.currentTimeMillis();
		RelationTreeNode childRTN = (RelationTreeNode)child;
		// Assuming constant ordering of solutions, e.g. no tabling
		String G = "Nodes=[Node,Child], get_solutionN("+ getRelationFunctor()+"(_,_,Node), N, "+getRelationFunctor()+"(Child,_,Node)), ";
		G += "ipObjectSpec('java.lang.Integer',Int,[N],_)";
		Integer N = (Integer)getEngine().deterministicGoal(G,"Nodes",new Object[]{node,childRTN.node},"[Int]")[0];
		//System.out.println("Exit getIndexOfChild "+childRTN.node+" in "+node+":"+ (System.currentTimeMillis()-start) + " mS, "+(dgcalls++)+ " dGcalls");
		dgcalls++;
		return N.intValue();
	}
	public String toString(){
		if (toStringCache!=null) return toStringCache;
		System.err.println("toString should be cached!!!");
		
		//long start = System.currentTimeMillis();
		String G = "NodeArray=[Node], "+getRelationFunctor()+"(Node,Description,_)";
		Object description = getEngine().deterministicGoal(G,"NodeArray",nodeInArray,"[Description]")[0];
		//System.out.println("Exit toString "+ node+":"+ (System.currentTimeMillis()-start) + " mS, "+(dgcalls++)+ " dGcalls");
		dgcalls++;
		toStringCache = description.toString();
		return toStringCache;
	}
		
	PrologEngine getEngine(){
		return myModel.engine;
	}
	String getRelationFunctor(){
		return myModel.relationFunctor;
	}
}