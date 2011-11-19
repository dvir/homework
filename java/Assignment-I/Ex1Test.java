import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;


public class Ex1Test {
	static final String lineSeparator = System.getProperty ( "line.separator" );
	/**
	 * @param args
	 *
	 */
	public static void main(String[] args) throws IOException{
		ByteArrayOutputStream byteArr = new ByteArrayOutputStream();
		PrintStream tempOut = System.out;
		PrintStream ps = new PrintStream(byteArr);
		System.setOut(ps);
		Ex1.main(null);
		System.setOut(tempOut);  // returns the standard output
		String expectedOutput = "";
		for (int i = 100; i >= 1; i--) {
			expectedOutput+= i+lineSeparator;
		}
		if (byteArr.toString().equals(expectedOutput))
			System.out.println("GOOD WORK!!!");
		else
			System.out.println("Your output is wrong. You need to fix Ex1.java");


	}


}
