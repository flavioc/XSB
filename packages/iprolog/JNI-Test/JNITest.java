class JNITest {
    public native void displayHelloWorld();
    
    static {
	System.loadLibrary("hello");
    }

    public static void main(String[] args) {
	new JNITest().displayHelloWorld();
    }
}
