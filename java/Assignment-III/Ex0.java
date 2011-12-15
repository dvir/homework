/***************************************************
 * intro121/ipis121: Third assignment              *
 *                                                 *
 * This class is for assignment #3 - Part 0        *
 *                                                 *
 * Author(s): ### Dvir Azulay (dvirazu@post.bgu.ac.il), Ory Band (@post.bgu.ac.il) ##### *
 * Date: 20/12/2011                                *
 *                                                 *
 ***************************************************/

/*
 * Important! Add comments to every method!
 *
 * The "return" statement at the end of each method and the initial value 
 * is just so this skeleton file compiles, change it according to your needs
 */
 
public class Ex0 {

	public static int f(int x) {
		if (x <= 0) {
			// stopping condition
			return 1;
		}
		else if (x % 3 == 0){
			// x divides by 3, execute this formula and divide it by 3 (so we get to end condition at some point)
			return 2*x * f(x / 3);
		}
		else {
			// otherwise, just sum the result with the x-1 result.
			return x + f(x - 1);
		}
	}

	public static void main(String[] args) {
		// Test cases in format of pairs <input, expected output>
		int[][] inputOutput = {{5, 21}, {12, 384}, {0, 1}, {20, 1767}};
		
		// Execute the test cases one after another
		for (int testCase=0; testCase<inputOutput.length; testCase++) {
			int x = inputOutput[testCase][0];
			int exp = inputOutput[testCase][1];
			int actual = Ex0.f(x);
			boolean isPassed = (actual == exp);
			
			System.out.print("Test " + testCase + ": ");
			if (isPassed)
				System.out.println("passed :)");
			else
				System.out.println("failed! f(" + x + ") expected: " + exp + " actual: " + actual);
		}
	}
}
