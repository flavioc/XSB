/* 
** Author(s): Miguel Calejo
** Contact:   interprolog@declarativa.com, http://www.declarativa.com
** Copyright (C) Declarativa, Portugal, 2000-2002
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/
package com.declarativa.interprolog.gui;
import com.declarativa.interprolog.*;
import com.declarativa.interprolog.util.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.table.*;
import javax.swing.event.*;

/** A simple Prolog listener, with a Reconsult menu and an history mechanism. 
This should be sub-classes, in order to define sendToProlog()*/
public abstract class ListenerWindow extends JFrame implements WindowListener{	
	public JTextArea prologOutput, prologInput; 
	JMenu historyMenu, fileMenu; 
	Vector loadedFiles;
	private static int topLevelCount = 0;
	public PrologEngine engine = null;
	
	public ListenerWindow(PrologEngine e){
		this(e,true);
	}
	public ListenerWindow(PrologEngine e, boolean autoDisplay){
		super("PrologEngine listener (Swing)");		
		if (e!=null) engine=e;
		else throw new IPException("missing Prolog engine");		
		//engine.consultFromPackage("visualization.O",ListenerWindow.class);	
		engine.consultFromPackage("visualization.xwam",ListenerWindow.class);	
		engine.teachMoreObjects(guiExamples());
		engine.command("write('Welcome to an InterProlog/XSB top level!')");
		engine.command("xsb_configuration(version,_V), write('XSB version '), writeln(_V)");
				
		if (engine==null) dispose(); // no interface object permitted!
		else topLevelCount++;
		debug=engine.isDebug();
		
		loadedFiles = new Vector();
		
		constructWindowContents();
		constructMenu();
		
				
		addWindowListener(this);
		prologInput.addKeyListener(new KeyAdapter(){
			public void keyPressed(KeyEvent e){
				if (e.getKeyCode()==KeyEvent.VK_ENTER) {
					sendToProlog();
					e.consume();
				}
			}
		});
        if (autoDisplay) {
		    show();
		    focusInput();	
		}
		
	}
			
	// WindowListener methods
	public void windowOpened(WindowEvent e){}
	public void windowClosed(WindowEvent e){}
	public void windowIconified(WindowEvent e){}
	public void windowClosing(WindowEvent e){
		dispose();
		engine.shutdown();
		topLevelCount--;
		if (topLevelCount <= 0) System.exit(0);
			// should check whether any relevant windows are changed...
	}
	public void windowActivated(WindowEvent e){
		prologInput.requestFocus();
	}
	public void windowDeactivated(WindowEvent e){}
	public void windowDeiconified(WindowEvent e){}
	
	public static ObjectExamplePair[] guiExamples() {
		ObjectExamplePair[] examples = {
			PredicateTableModel.example(),
			TermListModel.example(),
			TermTreeModel.example(),
			new ObjectExamplePair("ArrayOfTermTreeModel",new TermTreeModel[0]),
			XSBTableModel.example()
		};
		return examples;
	}
	
	void constructWindowContents(){
		Font prologFont = new Font("Courier",Font.PLAIN,12);
		Container c = getContentPane();
		c.setLayout(new BorderLayout());
		prologOutput = new JTextArea(20,80); prologOutput.setFont(prologFont); 
		prologOutput.setEditable(false); 
		prologOutput.setToolTipText("Here's XSB console output");
		prologOutput.setLineWrap(true);  // Swing used to crash with large ammounts of text...
		prologOutput.setDoubleBuffered(true); // Use Swing double screen buffer
		JScrollPane piscroller = new JScrollPane();
		prologInput = new JTextArea(4,80); prologInput.setFont(prologFont); prologInput.setLineWrap(true);
		prologInput.setToolTipText("XSB input, sent when you press enter");
		piscroller.getViewport().add(prologInput); 

		JScrollPane scroller = new JScrollPane();
		scroller.getViewport().add(prologOutput);
		JSplitPane j = new JSplitPane (JSplitPane.VERTICAL_SPLIT, scroller, prologInput);
		c.add("Center",j);
		setSize(600,600);
		j.setDividerLocation(500);
		//j.resetToPreferredSizes();
		validate();
	}
	
	void constructMenu(){
		JMenuBar mb; 
		mb = new JMenuBar(); setJMenuBar(mb);
		
		fileMenu = new JMenu("File"); mb.add(fileMenu);
		
		addItemToMenu(fileMenu,"Reconsult...",new ActionListener(){
			public void actionPerformed(ActionEvent e){
				reconsultFile();
			}
		});
		
		addItemToMenu(fileMenu,"Load dynamically...",new ActionListener(){
			public void actionPerformed(ActionEvent e){
				load_dynFile();
			}
		});
		
		fileMenu.addSeparator();
		
		JMenu toolMenu = new JMenu("Tools"); mb.add(toolMenu);
		
		final JCheckBoxMenuItem debugging = new JCheckBoxMenuItem("Engine debugging");
		toolMenu.add(debugging);
		debugging.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				engine.setDebug(debugging.isSelected());
			}
		});

		addItemToMenu(toolMenu,"See Object Specifications",new ActionListener(){
			public void actionPerformed(ActionEvent e){
				engine.command("showObjectVariables");
			}
		});
		
				
		addItemToMenu(toolMenu,"Interrupt XSB",new ActionListener(){
			public void actionPerformed(ActionEvent e){
				engine.interrupt();
			}
		});
		
		historyMenu = new JMenu("History",true); mb.add(historyMenu); 
		historyMenu.addSeparator(); // to avoid Swing bug handling key events
	}
	
	class HistoryListener implements ActionListener{
		JTextComponent targetText;
		String memory;
		HistoryListener(JTextComponent t,String s){
			targetText=t; memory=s;
		}
		public void actionPerformed(ActionEvent e){
			targetText.replaceSelection(memory);
		}
	}
	
	static void addItemToMenu(JMenu menu,String item,ActionListener handler) {
		JMenuItem menuItem = new JMenuItem(item);
		menu.add(menuItem);
		menuItem.addActionListener(handler);
	}
	
	public abstract void sendToProlog();
	
	protected void addToHistory(){
		JMenuItem item;
		String goal = prologInput.getText();
		if (goal.equals(";")) return; // not worthy remembering
		if (goal.length()>20) historyMenu.add(item = new JMenuItem(goal.substring(0,19)+"..."));
		else historyMenu.add(item = new JMenuItem(goal.substring(0,goal.length())));
		item.addActionListener(new HistoryListener(prologInput,goal));
	}
	
	static class LoadedFile{
		File file; String method;
		LoadedFile(File file,String method){
			this.file=file; this.method=method;
			if (!(method.equals("reconsult") || method.equals("load_dyn")))
			throw new IPException("bad load method");
		}
		public boolean equals(LoadedFile o){
			return file.equals(o.file) && method.equals(o.method);
		}
	}
	
	void addToReloaders(File file,String method){
		final LoadedFile lf = new LoadedFile(file,method);
		if (!loadedFiles.contains(lf)){
			loadedFiles.addElement(lf);
			addItemToMenu(fileMenu,file.getName(),new ActionListener(){
				public void actionPerformed(ActionEvent e){
					engine.command(lf.method+"('"+lf.file.getAbsolutePath()+ "')");
				}
			});
		}
	}
	
	void reconsultFile(){
		String nome,directorio; File filetoreconsult=null;
		FileDialog d = new FileDialog(this,"Reconsult file...");
		d.show();
		nome = d.getFile(); directorio = d.getDirectory();
		if (nome!=null) {
			filetoreconsult = new File(directorio,nome);
			engine.command("reconsult('"+filetoreconsult.getAbsolutePath()+ "')");
			addToReloaders(filetoreconsult,"reconsult");
		}
	}

	void load_dynFile(){
		String nome,directorio; File filetoreconsult=null;
		FileDialog d = new FileDialog(this,"load_dyn file...");
		d.show();
		nome = d.getFile(); directorio = d.getDirectory();
		if (nome!=null) {
			filetoreconsult = new File(directorio,nome);
			engine.command("load_dyn('"+filetoreconsult.getAbsolutePath()+ "')");
			addToReloaders(filetoreconsult,"load_dyn");
		}
	}

	public void focusInput(){
		prologInput.selectAll();
		prologInput.requestFocus();
	}
		
	public void scrollToBottom(){
		if (prologOutput.isShowing()) {
			prologOutput.setCaretPosition(prologOutput.getDocument().getEndPosition().getOffset()-1 /* OBOB hack */);
			try {
				// If we're in a JScrollPane, force scrolling to bottom and left
				JScrollBar scrollbarV = ((JScrollPane)((JViewport)(prologOutput.getParent())).getParent()).getVerticalScrollBar();
				scrollbarV.setValue(scrollbarV.getMaximum());
				JScrollBar scrollbarH = ((JScrollPane)((JViewport)(prologOutput.getParent())).getParent()).getHorizontalScrollBar();
				scrollbarH.setValue(scrollbarH.getMinimum());
			} catch (Exception e) {/* We're not in a JScrollPane, forget it! */};
		}
	}
	
	public static boolean debug = false;
	public static String prologStartCommand=null;

	public static void commonMain(String args[]) {
		System.out.println("Welcome "+System.getProperty("user.name")+" to InterProlog "+PrologEngine.version+" on Java "+
			System.getProperty("java.version") + " ("+
			System.getProperty("java.vendor") + "), "+ 
			System.getProperty("os.name") + " "+
			System.getProperty("os.version"));
			
		if (args.length>=1){
			int i=0;
			while(true){
				if (args[i].toLowerCase().startsWith("-d")) {
					debug=true;
					i=i+1;
				} else {
					prologStartCommand = remainingArgs(args,i);
					break;
				}
			}
		} else System.err.println("Usage: ...thisclassname [-debug] PrologExecutablePath OtherPrologArgs");

	}
	
	public static String commandArgs(String[] args){
		return remainingArgs(args,0);
	}
	public static String remainingArgs(String[] args,int first){
		if (args.length==0) throw new IPException("Missing arguments in command line");
		StringBuffer temp = new StringBuffer();
		for (int i=first;i<args.length;i++){
			if (i>first) temp.append(" ");
			temp.append(args[i]);
		}
		return temp.toString();
	}
	
	public static void beep(){
		Toolkit.getDefaultToolkit().beep();
	}
}
