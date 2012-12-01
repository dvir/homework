import java.util.Scanner;
public class Ex5 {
	public static void main (String[] args) {
		Scanner scanner = new Scanner(System.in);
		int num = scanner.nextInt();
		int currentPrime = 2; // holds the number we are testing against.
		
		// run until the number is equal or lower than 1.
		// will end because we are dividing the number by numbers lower than him,
		// and at some point we will get to the number itself.
		while (num > 1) {
			// test if the number divides by the current prime we are testing against.
			// if so, print the prime number and divide the number.
			if (num % currentPrime == 0) {
				num = num / currentPrime;
				System.out.println(currentPrime);
			}
			else {
				// our number doesn't divide by currentPrime, so we shall increase it.
				++currentPrime;
			}
		}
	}
}
