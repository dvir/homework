/***************************************************
 * intro121/ipis121: Third assignment              *
 *                                                 *
 * This class is for assignment #3 - Part 3        *
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

public class Ex3 {

	public static int north(int[] tile){
		return tile[0];
	}

	public static int east(int[] tile){
		return tile[1];
	}

	public static int south(int[] tile){
		return tile[2];
	}

	public static int west(int[] tile){
		return tile[3];
	}


	/******************** Task 1 ********************/
	public static boolean canPut(int[] tile, int x, int y, int[][][] board) {
		boolean ans = true;
		if (board[x][y] != null || y >= board.length || x >= board[0].length) {
			System.out.println(board[x][y]);
			return false;
		}
		
		if (x == 0) {
			// check the west side
			if (west(tile) != 0) {
				// not grey!
				return false;
			}
			
			// check the north side if we are the on the highest row
			if (y == 0 && north(tile) != 0) {
				return false;
			}
			
			// check the south side if we are on the lowest row
			if (y == board.length-1 && south(tile) != 0) {
				return false;
			}
		}
		else if (x == board[0].length-1) {
			// check the east side
			if (east(tile) != 0) {
				// not grey!
				return false;
			}
			
			// check the north side if we are the on the highest row
			if (y == 0 && north(tile) != 0) {
				return false;
			}
			
			// check the south side if we are on the lowest row
			if (y == board.length-1 && south(tile) != 0) {
				return false;
			}			
		}
		
		// test indexes - the north, east, south and west squares around the square
		
		int testX, testY;
		int[][] testIndexes = new int[][]{{0, -1}, {1, 0}, {0, 1}, {-1, 0}};
		for (int i = 0; i < testIndexes.length && ans; ++i) {
			testX = x+testIndexes[i][0];
			testY = y+testIndexes[i][1];
			if (testX < 0 || testY < 0 || testX >= board.length || testY >= board[0].length) {
				continue;
			}
			
			if (board[testX][testY] != null && tile[i] != board[testX][testY][(i+2)%4]) {
				ans = false;
			}
		}
		return ans;
	}



	/******************** Task 2 ********************/
	public static int[][][] put(int[] tile, int x, int y, int[][][] board) {
		int n = board.length;
		int[][][] newBoard= new int[n][n][];
		// YOUR CODE HERE
		return newBoard;
	}



	/******************** Task 3 ********************/
	public static int[][] delete(int i, int[][] tiles) {
		int[][] restTiles = null;
		// YOUR CODE HERE
		return restTiles;
	}

	public static int[] rotate(int j, int[] tile){
		int[] nextTile = null;
		// YOUR CODE HERE
		return nextTile;
	}



	/******************** Task 4 ********************/
	public static int[][][] solve(int[][] tiles){
		int size = (int) Math.sqrt(tiles.length);
		int[][][] board = new int[size][size][];
		return solve(board,tiles);
	}

	public static int[][][] solve(int[][][] board, int[][] tiles){
		int[][][] solution = null;
		// YOUR CODE HERE
		return solution;
	}

	/******************** Auxiliary functions ********************/
	
	/**
	 * Compare two boards and return true iff they are equal.
	 * @param board1
	 * @param board2
	 * @return true iff the boards are equal
	 */
	public static boolean equalBoards(int[][][] board1, int[][][] board2) {
		boolean ans = true;
		for (int i = 0; i < board1.length && ans; i++) {
			for (int j = 0; j < board1.length && ans; j++) {
				int[] tile1 = board1[i][j];
				int[] tile2 = board2[i][j];
				if ((tile1 == null && tile2 != null)
						|| (tile1 != null && tile2 == null))
					ans = false;
				else if (tile1 != null && tile2 != null) {
					for (int k = 0; k < 4 && ans; k++)
						if (tile1[k] != tile2[k])
							ans = false;
				}
			}
		}
		return ans;
	}


	public static void main(String[] args) {
		int[][][] board = { { { 0, 2, 1, 0 }, null, { 1, 3, 0, 0 } },
							{ { 0, 2, 4, 2 }, null, { 4, 4, 0, 3 } },
							{ { 0, 0, 4, 2 }, { 4, 0, 3, 3 }, { 3, 0, 0, 4 } } };

		// Test task 1
		int[] test1tile = {1, 2, 1, 0};
		System.out.println("Test 1: expected=true actual="
				+ canPut(test1tile, 0, 1, board));

		int[] test2tile = {2, 2, 1, 0};
		System.out.println("Test 2: expected=false actual="
				+ canPut(test2tile, 0, 1, board));

		int[] test3tile = {1, 2, 1, 1};
		System.out.println("Test 3: expected=false actual="
				+ canPut(test3tile, 0, 1, board));

		// Test task 2
		int[] test4tile = {1, 2, 1, 0};
		int[][][] test4exp = { { { 0, 2, 1, 0 }, { 1, 2, 1, 0 }, { 1, 3, 0, 0 } },
				{ { 0, 2, 4, 2 }, null, { 4, 4, 0, 3 } },
				{ { 0, 0, 4, 2 }, { 4, 0, 3, 3 }, { 3, 0, 0, 4 } } };
		System.out.println("Test 4: "
				+ (equalBoards(test4exp, put(test4tile, 0, 1, board)) ? "passed :)"
						: "failed!"));
		
		// Test task 3
		int[] test5tile= {1, 2, 3, 4};
		int[] test5exp = {4, 1, 2, 3};
		System.out.println("Test 5: expected=" + Ex2.arrayToString(test5exp)  + 
				" actual=" + Ex2.arrayToString(rotate(1, test5tile)));
		
		int[] test6tile= {1, 2, 3, 4};
		int[] test6exp = {3, 4, 1, 2};
		System.out.println("Test 6: expected=" + Ex2.arrayToString(test6exp)  + 
				" actual=" + Ex2.arrayToString(rotate(2, test6tile)));
		
		int[][] test7tiles = {{1, 2, 3, 4}, {0, 2, 4, 5}, {5, 2, 5, 1}};
		int[][] test7exp = {{1, 2, 3, 4}, {5, 2, 5, 1}};
		System.out.println("Test 7: expected=" + Ex2.matrixToString(test7exp)  + 
				" actual=" + Ex2.matrixToString(delete(1, test7tiles)));
	}
}
