package com.declarativa.interprolog.examples;
import com.declarativa.interprolog.*;
import com.declarativa.interprolog.util.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
public class HelloWindow extends JFrame{
	PrologEngine myEngine;
	public HelloWindow(PrologEngine pe){
		super("Java-Prolog-Java call example");
		myEngine = pe;
		JTextField text = new JTextField(15);
		final Object fieldObject = myEngine.makeInvisible(text);
		text.setBorder(BorderFactory.createTitledBorder("text"));
		JButton button = new JButton("Greet");
		Box box = new Box(BoxLayout.Y_AXIS);
		box.add(text); box.add(button);
		getContentPane().add(box);
		setSize(200,100); show();
		
		button.addActionListener(
			new ActionListener(){
				public void actionPerformed(ActionEvent e){
					myEngine.deterministicGoal("greetat(Obj)","[Obj]",new Object[]{fieldObject});
				}
			});
	}
}
/*
In Prolog call this to create the window:
ipPrologEngine(Engine), javaMessage('HelloWindow','HelloWindow'(Engine)).

then
assert( (greetat(TextID) :- javaMessage( TextID, setText(string('Hello world!')) )) ).

*/