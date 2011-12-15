/***************************************************
 * intro121/ipis121: Third assignment              *
 *                                                 *
 * This class is for assignment #3 - Part 1        *
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

public class Ex1 {

	/******************** Task 1 ********************/
	public static long tilesPack1a(int n) {
		// stop at 0 or 1 tiles - there's only one option for that to happen
		if (n < 2) {
			return 1;
		}
		
		// sum the options to place a single tile and a double tile
		return tilesPack1a(n-1) + tilesPack1a(n-2);
	}
	
	public static long tilesPack1b(int n) {
		long[] mem = new long[n + 1];
		// initiate the mem array with -1 values
		for (int i = 0; i < mem.length; ++i) {
			mem[i] = -1;
		}
		
		// call the recursive function to calculate the amount of possibilities to place a single and double tile on a row with n slots
		// NOTE: this is another function (different signature)
		return tilesPack1b(n, mem);
	}
	
	public static long tilesPack1b(int n, long[] mem) {
		// stop at 0 or 1 tiles - there's only one option for that		
		if (n < 2) {
			return 1;
		}
		
		// if we didn't pre-calculate the value, calculate it and place it in mem[n] for future usage
		if (mem[n] == -1) {
			// sum the options to place a single tile and a double tile
			mem[n] = tilesPack1b(n-1, mem) + tilesPack1b(n-2, mem);
		}

		// return the pre-calculated value
		return mem[n];
	}

	/******************** Task 2 ********************/
	public static long tilesPack2(int n) {
		// return the amount of possibilities to place a single tile and a double tile in a 2*n board
		return (long)Math.pow(tilesPack1b(n), 2);
	}
	
	/******************** Task 3 ********************/
	public static long tilesPack3(int n) {
		long[] mem = new long[n + 1];
		// initiate the mem array with -1 values
		for (int i = 0; i < mem.length; ++i) {
			mem[i] = -1;
		}
		
		// return the amount of possibilities to place a single tile, a double tile and a triple tile in a 2*n board
		return (long)Math.pow(tilesPack3(n, mem), 2);
	}
	
	static long tilesPack3(int n, long[] mem) {
		// stop at 0 or 1 tiles - there's only one option for that		
		if (n < 2) {
			return 1;
		}
		
		// if we didn't pre-calculate the value, calculate it and place it in mem[n] for future usage
		if (mem[n] == -1) {
			// sum the options to place a single tile and a double tile
			mem[n] = tilesPack3(n-1, mem) + tilesPack3(n-2, mem);
			if (n > 2) {
				// we can place a triple tile as well. add it to the sum
				mem[n] += tilesPack3(n-3, mem);
			}
		}
		
		// return the pre-calculated value
		return mem[n];		
	}

	public static void main(String[] args) {
		// Test task 1a
		System.out.println("Test 1: expected=" + 1 + " actual=" + tilesPack1a(0));
		System.out.println("Test 2: expected=" + 2 + " actual=" + tilesPack1a(2));
		System.out.println("Test 3: expected=" + 5 + " actual=" + tilesPack1a(4));
		
		// Test task 1b
		System.out.println("Test 4: expected=" + 5 + " actual=" + tilesPack1b(4));
		System.out.println("Test 5: expected=" + 225851433717L + " actual=" + tilesPack1b(55));
		
		// Test task 2
		System.out.println("Test 6: expected=" + 1 + " actual=" + tilesPack2(1));
		System.out.println("Test 7: expected=" + 4 + " actual=" + tilesPack2(2));
		System.out.println("Test 8: expected=" + 9 + " actual=" + tilesPack2(3));
		
		// Test task 3
		System.out.println("Test 9: expected=" + 4 + " actual=" + tilesPack3(2));
		System.out.println("Test 10: expected=" + 16 + " actual=" + tilesPack3(3));
		System.out.println("Test 11: expected=" + 49 + " actual=" + tilesPack3(4));
	}

}
