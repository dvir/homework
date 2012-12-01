import java.util.Scanner;
public class Ex6 {
	public static void main (String[] args) {
		Scanner scanner = new Scanner(System.in);
		int currentNum, num = scanner.nextInt();
		int sum = 0; // holds the total sum of numbers that their digits sum dividing by 7.
		int digitsSum; // holds the current number digits sum.
		
		// we will run on the numbers smaller than "num" and higher than 6 
		// (because it will obviously not divide by 7)
		for (int i = num; i > 6; --i) {
			digitsSum = 0; // reset the digits sum for this number
			currentNum = i; // set the current number we are working on
			while (currentNum > 0) {
				// find the first digit, add it to the sum and divide the number by 10.
				digitsSum += currentNum % 10;
				currentNum = currentNum / 10;
			}
			
			// if the digits sum for the current number is 7, add it to the total sum.
			if (digitsSum % 7 == 0) {
				sum += i;
			}
		}
		
		System.out.println(sum);
	}
}
