package com.declarativa.interprolog.examples;
import com.declarativa.interprolog.*;
import com.xsb.interprolog.*;
public class HelloWorld{
	public static void main(String args[]) {
		PrologEngine engine = new NativeEngine(args[0]);
		engine.command("import append/3 from basics");
		Object[] bindings = engine.deterministicGoal(
			"name(User,UL),append(\"Hello,\", UL, ML), name(Message,ML)",
			"[string(User)]",
			new Object[]{System.getProperty("user.name")},
			"[string(Message)]");
		String message = (String)bindings[0];
		System.out.println("\nMessage:"+message);
		// the above demonstrates object passing both ways; 
		// since we may simply concatenate strings, an alternative coding would be:
		bindings = engine.deterministicGoal(
			"name('"+System.getProperty("user.name")+"',UL),append(\"Hello,\", UL, ML), name(Message,ML)",
			"[string(Message)]");
		// (notice the ' surrounding the user name, unnecessary in the first case)
		System.out.println("Same:"+bindings[0]);
		System.exit(0);
	}
}