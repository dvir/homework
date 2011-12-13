/***************************************************
 * intro121/ipis121: Third assignment              *
 *                                                 *
 * This class is for assignment #3 - Part 1        *
 *                                                 *
 * Author(s): ### YOUR NAME(S) AND EMAIL(S). ##### *
 * Date: ##/##/####                                *
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
		long res = 0;
		if (n < 0) {
			return 1;
		}
		res += tilesPack1a(n-1);
		if (n > 1) {
			res += tilesPack1a(n-2);
		}
		return res;
	}
	
	public static long tilesPack1b(int n) {
		long[] mem = new long[n + 1];
		for (int i = 0; i < mem.length; ++i) {
			mem[i] = -1;
		}
		return tilesPack1b(n, mem);
	}
	
	public static long tilesPack1b(int n, long[] mem) {
		if (n <= 0) {
			return 1;
		}
		
		if (mem[n] == -1) {
			mem[n] = 0;
			if (n > 0) {
				mem[n] += tilesPack1b(n-1, mem);
				if (n > 1) {
					mem[n] += tilesPack1b(n-2, mem);
				}
			}
		}
		return mem[n];
	}

	/******************** Task 2 ********************/
	public static long tilesPack2(int n) {
		return (long)Math.pow(tilesPack1b(n), 2);
	}
	
	/******************** Task 3 ********************/
	public static long tilesPack3(int n) {
		long[] mem = new long[n + 1];
		for (int i = 0; i < mem.length; ++i) {
			mem[i] = -1;
		}
		return (long)Math.pow(tilesPack3(n, mem), 2);
	}
	
	static long tilesPack3(int n, long[] mem) {
		if (n <= 0) {
			return 1;
		}
		
		if (mem[n] == -1) {
			mem[n] = 0;
			if (n > 0) {
				mem[n] += tilesPack3(n-1, mem);
				if (n > 1) {
					mem[n] += tilesPack3(n-2, mem);
				}
				
				if (n > 2) {
					mem[n] += tilesPack3(n-3, mem);
				}				
			}
		}
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
