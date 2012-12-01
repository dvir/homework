import java.util.Scanner;
public class Ex2 {
	public static void main (String[] args) {
		Scanner scanner = new Scanner(System.in);
		int sum = 0, count = 0; // will hold the sum and count of prime numbers lower than the input number.
		double avg = 0;
		int num = scanner.nextInt();
		boolean isPrime; // determines if a number is a prime or not
		
		// run through the numbers [2, num)
		for (int i = 2; i < num; ++i) {
			isPrime = true; // we assume a number is a prime 
							// unless he divides by a number small than him except 1.
							
			// run through the numbers [2, i) to find out if "i" is a prime or not.
			for (int j = 2; j < i && isPrime; ++j) {
				if (i % j == 0) {
					// since i mod j equals 0, and j is between 1 and i (not inclusive), 
					// i is NOT a prime number.
					isPrime = false;
				}
			}
			
			if (isPrime) { // i is a prime number, add it to the sum it and increase the primes count.
				sum += i;
				++count;
			}
		}
		
		// calculate the average of prime numbers we found and print it.
		if (count > 0) {
			avg = (double)sum / count;
		}
		System.out.println(avg);
	}
}
