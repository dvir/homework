import java.util.Scanner;
public class Ex4 {
	public static void main (String[] args) {
		int x, a, b, c; // holds the values we are testing our equation against, "x" being the input num.
		Scanner scanner = new Scanner(System.in);
		x = scanner.nextInt();
		
		// we will go over the sets of three numbers and test if the equation is correct for that set.
		// if so, we will print it to the screen.
		// NOTICE: we are going over the numbers from [3, x), [2, c), [1, b) (c, b, a)
		//         in order to not repeat the same equations twice, 
		//         and to follow the "x > c > b > a > 0" rule.
		for (c = 3; c < x; ++c) {
			for (b = 2; b < c; ++b) {
				for (a = 1; a < b; ++a) {
					// test the equation. if it's correct, print it.
					if ((a*a + b*b) == c*c) {
						System.out.println("(" + a + "," + b + "," + c + ") : " + 
											a + "*" + a + " + " + b + "*" + b + " = " + c + "*" + c);
					}
				}
			}
		}
	}


}
