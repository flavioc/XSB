/* Example for Java-side UI construction, with event handler setup on the Prolog side
*/
package com.declarativa.interprolog.examples;
import com.declarativa.interprolog.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
// Hello World, Prolog-biased implementation; the Java part knows nothing about event handling
public class HelloWindow2 extends JFrame{
	public HelloWindow2(PrologEngine pe /* this argument not really needed, cf. comments below */){
		super("Java-Prolog-Java call test2");
		JTextField text = new JTextField(15);
		text.setBorder(BorderFactory.createTitledBorder("text"));
		JButton button = new JButton("Greet");
		Box box = new Box(BoxLayout.Y_AXIS);
		box.add(text); box.add(button);
		getContentPane().add(box);
		setSize(200,100); validate(); show();
		// The following 2 lines are not strictly necessary, as long as someone is able to register
		// button and text with an engine of choice, hence the constructor argument above
		// Concluding, pe and these 2 messages below are just for the benefit of dynamically introspecting
		// into 2 graphical objects, telling our engine about them, and letting the Prolog 
		// programmer know their allocated IDs, nothing more
		System.out.println("Button ID:"+pe.registerJavaObject(button));
		System.out.println("Text ID:"+pe.registerJavaObject(text));
	}
}
/*
To create the window, in Prolog call:
ipPrologEngine(Engine), javaMessage('HelloWindow2','HelloWindow2'(Engine)).

watch the console window and write down the Text and Button IDs

Then, assuming we have defined in Prolog an event handler predicate, like:
assert( ( greetat(TextID) :- javaMessage( TextID, setText( string('Hello world!')) )) ).

...we can now call:
ipPrologEngine(Engine), buildTermModel(greetat(TextID),TM), 
javaMessage('com.declarativa.interprolog.PrologEventBroker',R,'PrologEventBroker'(Engine,TM)),
javaMessage(ButtonID,addActionListener(R)).

*/