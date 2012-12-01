import java.util.Scanner;
public class Ex3 {
	public static void main (String[] args) {
		Scanner scanner = new Scanner(System.in);
		int num1 = 0, num2 = 0, num3 = 0, tmp; // holds the top 3 values the user inputs, 
											   // and a temp variable to help switching between values.
		int inputNum = scanner.nextInt();
		while (inputNum != 0) { // runs as long as the user doesn't input 0
			// we need the top three values the user inputs.
			// we will insert the new value to num3 if its higher than him.
			// then, we will see if we need to re-order the top three list.
			
			if (inputNum > num3) {
				num3 = inputNum;
			}
			
			if (num3 > num2) {
				tmp = num2;
				num2 = num3;
				num3 = tmp;
			}
			
			if (num2 > num1) {
				tmp = num1;
				num1 = num2;
				num2 = tmp;
			}			
			
			inputNum = scanner.nextInt();
		}
		
		// print the third highest value the user entered.
		System.out.println(num3);
	}
}
