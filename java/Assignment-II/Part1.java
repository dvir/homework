import java.util.Scanner;
public class Part1 {	
	public static void main (String[] args) {
		boolean status;
		Scanner scanner = new Scanner(System.in);
		
		/* Test randomSolver(n, m, c, numResets, numFixes) */
		System.out.println("Enter nXm / c:");
		int n = scanner.nextInt();
		System.out.print(" X ");
		int m = scanner.nextInt();
		System.out.print("   / ");
		int c = scanner.nextInt();
		System.out.print("Resets: ");
		int numResets = scanner.nextInt();
		System.out.print("Fixes: ");
		int numFixes = scanner.nextInt();
		
		int[][] board = randomSolver(n, m, c, numResets, numFixes);
		if (board == null) {
			System.out.println("Didn't find any matching board :(");
		}
		else { 
			System.out.println("Found a match!");
			for (int i = 0; i < board.length; ++i) {
				System.out.println(join(",", board[i]));
			}
			System.out.println("----------------------");			
		}
		
		/* Test randomFix(board, c, twoCorners) */
		/*
		int[] twoCorners = {0, 1, 1, 2};
		int[][] board = {{3, 0, 0},
						 {1, 0, 0}};
		int c = 5;
		
		randomFix(board, c, twoCorners);
		System.out.println("Random fixed board:");
		for (int i = 0; i < board.length; ++i) {
			System.out.println(join(",", board[i]));
		}
		System.out.println("----------------------");		
		*/
		
		/* Test randomBoard(n, m, c) */
		/*
		System.out.println("Enter nXm / c:");
		int n = scanner.nextInt();
		System.out.print(" X ");
		int m = scanner.nextInt();
		System.out.print("   / ");
		int c = scanner.nextInt();
		int[][] result = randomBoard(n, m, c);
		System.out.println("Random generated board:");
		for (int i = 0; i < result.length; ++i) {
			System.out.println(join(",", result[i]));
		}
		System.out.println("----------------------");		
		*/
		
		/* Test solver(n, m, c) */
		/*
		System.out.println("Enter nXm / c:");
		int n = scanner.nextInt();
		System.out.print(" X ");
		int m = scanner.nextInt();
		System.out.print("   / ");
		int c = scanner.nextInt();
		int[][] result = solver(n, m, c);
		if (result == null) {
			System.out.println("Nope, no match for " + n + "x" + m + " / " + c);
		}
		else {
			System.out.println("Found a match! result:");
			for (int i = 0; i < result.length; ++i) {
				System.out.println(join(",", result[i]));
			}
			System.out.println("----------------------");
		}
		*/
		
		/* Test isValidSolution(board, c) */
		/*
		int c = 3;
		
		int[][] board = {{0, 1, 2, 0},
						 {1, 2, 0, 1},
						 {2, 1, 0, 2}};
		System.out.println("Testing first board, result: " + isValidSolution(board, c));

		int[][] board2 = {{0, 1, 2},
						 {1, 2},
						 {2, 1, 0}};
		System.out.println("Testing second board, result: " + isValidSolution(board2, c));
		
		int[][] board3 = {{0, 1, 2},
						 {1, 2, 3}};
		System.out.println("Testing third board, result: " + isValidSolution(board3, c));

		int[][] board4 = {{0, 1, 0},
						 {1, 2, 1},
						 {0, 1, 0}};
		System.out.println("Testing fourth board, result: " + isValidSolution(board4, c));		
		*/
		
		/* Test findSameColorRec(board) */
		/*
		int[][] board = {{2, 1, 2, 1},
						 {1, 3, 3, 2},
						 {3, 1, 2, 1},
						 {1, 2, 3, 2}};

		System.out.print("Trying a none c-colored board... result: ");
		if (findSameColorRec(board) == null) {
			System.out.println("null");
		}
		else {
			System.out.println(join(",", findSameColorRec(board)));
		}						 
		
		int[][] boardCColored = {{0, 1, 2},
								 {1, 2, 0},
								 {2, 1, 0}};

		System.out.print("Trying a c-colored board... result: ");
		if (findSameColorRec(boardCColored) == null) {
			System.out.println("null");
		}
		else {
			System.out.println(join(",", findSameColorRec(boardCColored)));
		}
		*/
		
		/* Test findSameColorRec(board, y, x) */
		/*
		int[][] board = {{2, 1, 2, 1},
						 {1, 3, 3, 2},
						 {3, 1, 2, 1},
						 {1, 2, 3, 2}};
		System.out.print("(0,0) -- ");
		if (findSameColorRec(board, 0, 0) == null) {
			System.out.println("null");
		}
		else {
			System.out.println(" -- " + join(",", findSameColorRec(board, 0, 0)));
		}
		
		System.out.print("(1,0) -- ");
		if (findSameColorRec(board, 0, 1) == null) {
			System.out.println("null");
		}
		else {
			System.out.println(join(",", findSameColorRec(board, 0, 1)));
		}
		*/		
		
		/* Test increment on a matrix */
		/*
		int[][] testMatrix10 = {{0,4},{9,8,3},{7,9,9,9}};
		int[][] testMatrix4 = {{0,1,2,3},{1,2,0,3},{3,3,3,3}};
		int[][] testMatrix4_2 = {{3,3,3,3},{3,3,3},{3,3,3,3}};
		status = increment(testMatrix10, 10);
		System.out.println(status + " -- {" + join(",", "},{", testMatrix10) + "}");
		
		status = increment(testMatrix4, 4);
		System.out.println(status + " -- {" + join(",", "},{", testMatrix4) + "}");		
		
		status = increment(testMatrix4_2, 4);
		System.out.println(status + " -- {" + join(",", "},{", testMatrix4_2) + "}");		
		*/
		
		/* Test increment on a single array */
		/*
		int[] testVec10 = {0, 2, 9, 9, 9};
		int[] testVec4 = {1, 0, 1, 3, 3};
		int[] testVec4_2 = {3, 3, 3, 3, 3};
				
		status = increment(testVec10, 10);
		System.out.println(status + " -- " + join(",", testVec10));
		
		status = increment(testVec4, 4);
		System.out.println(status + " -- " + join(",", testVec4));

		status = increment(testVec4_2, 4);
		System.out.println(status + " -- " + join(",", testVec4_2));		
		*/
	}	
	
	public static String join (String delimiter, int[] array) {
		String str = "";
		if (array.length > 0) {
			str = Integer.toString(array[0]);
			for (int i = 1; i < array.length; ++i) {
				str += delimiter + Integer.toString(array[i]);
			}
		}
		return str;
	}
	
	public static String join (String delimiter, String arrayDelimiter, int[][] array) {
		String str = "";
		if (array.length > 0) {
			str = join(",", array[0]);
			for (int i = 1; i < array.length; ++i) {
				str += arrayDelimiter + join(delimiter, array[i]);
			}
		}
		return str;
	}
	
	/* ********************************** *
	 * *  Part 1 - increment            * *
	 * ********************************** */
	// Task 1.1
	public static boolean increment(int[] vec, int base) {
		boolean succ = true;
		boolean keepIncreasing = true; // should we keep increasing the numbers?
		for (int i = vec.length-1; i >= 0 && keepIncreasing; --i) {
			vec[i]++; // increase the current digit by one
			if (vec[i] > base-1) {
				vec[i] -= base;
				// are we trying to increase the most significant num?
				if (i == 0) {
					// yes. change the flag
					succ = false;
				}
			}
			else {
				keepIncreasing = false;
			}
		}
		
		return succ;
	}		

	// Task 1.2
	public static boolean increment(int[][] matrix, int base) {
		boolean succ = true;
		boolean keepIncreasing = true;
		for (int i = matrix.length-1; i >= 0 && !increment(matrix[i], base); --i) {
			if (i == 0) {
				succ = false;
			}
		}
		return succ;
	}
	
	/* ********************************** *
	 * *  Part 2 - validate solution    * *
	 * ********************************** */
	// Task 2.1
	public static int[] findSameColorRec(int[][] board, int topRow, int leftColumn) {
		int[] res=null;
		boolean found = false;
		int color = board[topRow][leftColumn]; // the color in question
		for (int x = leftColumn+1; x < board[topRow].length && !found; ++x) {
			if (board[topRow][x] == color) {
				// we found the top right corner of a rectangular, test it
				for (int y = topRow+1; y < board.length && !found; ++y) {
					if (board[y][leftColumn] == color && board[y][x] == color) {
						// found a four-cornered rectangular with the same color!
						res = new int[]{y, x};
						found = true;
					}
				}
			}
		}
		
		// will get here with a none empty array
		// if we found any rectangular originating 
		// from the given top left corner, and null if not.
		return res;
	}
	
	// Task 2.2
	public static int[] findSameColorRec(int[][] board) {
		int[] res = null; // holds the function result
		int[] rectangular;
		boolean found = false;
		
		for (int y = 0; y < board.length && !found; ++y) {
			for (int x = 0; x < board[y].length && !found; ++x) {
				rectangular = findSameColorRec(board, y, x);
				if (rectangular != null) {
					// found a rectangular with four corners of the same color!
					found = true;
					res = new int[]{y, x, rectangular[0], rectangular[1]};
				}
			}
		}
		return res;
	}

	// Task 2.3
	public static boolean isValidSolution(int[][] board, int c) {
		boolean res = false;
		if (board == null) {
			return false;
		}
		
		int lineLength = -1; // the length of the first line in the board,
							   // so we can compare it to the rest.
		for (int y = 0; y < board.length; ++y) {
			if (board[y] == null) {
				// invalid row given! exit.
				return false;
			}
			
			// if we didn't set it before, set it now
			if (lineLength == -1) {
				lineLength = board[y].length;
			}
			else if (board[y].length != lineLength) {
				// the rows aren't the same size! exit the loops.
				return false;
			}
			
			// run over the columns in this row, and make sure it's defined correctly
			for (int x = 0; x < board[y].length; ++x) {
				if (board[y][x] > c-1 || board[y][x] < 0) {
					// invalid color given for (x, y) coordinate. exit.
					return false;
				}
			}
		}
			
		// if we got here, our board is a valid candidate to be a solution;
		// we just have to test if it's a c-colored board
		if (findSameColorRec(board) == null) {
			// we didn't find any same four corners colored rectangular inside our board,
			// therefore, it's a perfectly valid solution! WOHOO!
			res = true;
		}
		return res;
	}
	
	/* ********************************** *
	 * *  Part 3 - Basic solver         * *
	 * ********************************** */
	public static int[][] solver(int n, int m, int c) {
		int[][] board = new int[n][m]; // create a board with the sizes defined by nXm.
		boolean found; // holds whether we found a matching board or not
		
		// fill the board with zeros
		for (int i = 0; i < n; ++i) {
			for (int j = 0; j < m; ++j) {
				board[i][j] = 0;
			}
		}
		
		// test new boards until we find a match or finish going over the board
		while (!(found = isValidSolution(board, c)) && increment(board, c));
		
		if (!found) {
			// reset the board back to null because we didn't find any match
			board = null;
		}
		return board;
	}

	/* ********************************** *
	 * *  Part 4 - Random solver        * *
	 * ********************************** */
	// Task 4.1
	public static int[][] randomBoard(int n, int m, int c) {
		int[][] board = new int[n][m];
		
		for (int i = 0; i < n; ++i) {
			for (int j = 0; j < m; ++j) {
				board[i][j] = (int)(Math.random()*c);
			}
		}
		
		return board;
	}
	
	// Task 4.2
	public static void randomFix(int[][] board, int c, int[] twoCorners) {
		// choose a random corner from the rectangular
		// meaning: choose a random value from [0]-[1] and a random value from [2]-[3]
		int n = twoCorners[(int)(Math.random()*2)];
		int m = twoCorners[(int)(Math.random()*2)+2];
		
		// set a variable with the previous color so we can keep rolling for a new one
		int previous = board[n][m];
		while (board[n][m] == previous) {
			board[n][m] = (int)(Math.random()*(c-1));
		}
	}
	
	// Task 4.3
	public static int[][] randomSolver(int n, int m, int c, int numFixes) {
		int[][] board = null;
		boolean found = false;
		while (!found && numFixes > 0) {
			board = randomBoard(n, m, c);
			found = isValidSolution(board, c);
			if (!found) {
				randomFix(board, c, findSameColorRec(board));
				found = isValidSolution(board, c);
			}
			--numFixes;
		}
		
		if (!found) {
			board = null;
		}
		return board;
	}
	
	// Task 4.3
	public static int[][] randomSolver(int n, int m, int c, int numResets, int numFixes) {
		int[][] board = null;
		while (board == null && numResets > 0) {
			board = randomSolver(n, m, c, numFixes);
			--numResets;
		}

		return board;
	}

	
	/* ********************************** *
	 * *  Main you may want to use      * *
	 * ********************************** */
/*
	public static void main(String[] args) {
		int n=4, m=4, c=3;
		long startTime=System.currentTimeMillis();
		int[][] sol=solver(n, m, c);
//		int[][] sol=randomSolver(n, m, c, n*m, n*m);
		long endTime=System.currentTimeMillis();
		System.out.println("Solution time : "+(endTime-startTime)+" ms");
		System.out.println("Solution found: "+(sol!=null));
		if(sol!=null) {
			System.out.println("Valid solution: "+isValidSolution(sol,c));
		}
	}
*/
}
