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
		// make sure the given slot isn't occupied or out of bounds
		if (board[x][y] != null || x < 0 || y < 0 || x >= board.length || y >= board[0].length) {
			return false;
		}
		
		// test indexes - the north, east, south and west squares around the square
		int testX, testY;
		int[][] testIndexes = new int[][]{{0, -1}, {1, 0}, {0, 1}, {-1, 0}};
		for (int i = 0; i < testIndexes.length; ++i) {
			// set the (x,y) of the square we want to test against.
			testX = x + testIndexes[i][0];
			testY = y + testIndexes[i][1];
			if (testX < 0 || testY < 0 || testX >= board.length || testY >= board[0].length) {
				// we reached a square that is out of bounds; that means our tile is a side square
				// which means we should check if the relevant side is gray.
				if (tile[i] != 0) {
					// not gray!
					return false;
				}
			}
			else if (board[testX][testY] != null && tile[i] != board[testX][testY][(i+2)%4]) {
				// colors aren't matching in the respective tile sides, can't put the tile here
				return false;
			}
		}

		// if we reached this point, we can safely place the tile in this slot.
		return true;
	}



	/******************** Task 2 ********************/
	public static int[][][] put(int[] tile, int x, int y, int[][][] board) {
		// create a new board that we will fill with the old board and the new tile.
		int[][][] newBoard = new int[board.length][board.length][];
		for (int iX = 0; iX < board.length; ++iX) {
			for (int iY = 0; iY < board[iX].length; ++iY) {
				if (iX == x && iY == y) {
					// we reached the coordinates of the new tile. place it.
					newBoard[iX][iY] = tile;
				}
				else {
					// fill the new board with the old board data.
					newBoard[iX][iY] = board[iX][iY];
				}
			}
		}
		
		return newBoard;
	}



	/******************** Task 3 ********************/
	public static int[][] delete(int i, int[][] tiles) {
		// create the new tiles array
		int[][] restTiles = new int[tiles.length-1][4];
		
		// go over the tiles array and copy every tile from it to the new array, 
		// except from the tile we want to delete from it.
		for (int j = 0, k = 0; j < tiles.length; ++j) {
			if (j != i) {
				restTiles[k++] = tiles[j];
			}
		}
		
		return restTiles;
	}

	public static int[] rotate(int j, int[] tile){ 
		// create a new tile
		int[] nextTile = new int[4];
		
		// rotate the tile j times
		// every old tile color index should move to the (j+i)%4 index in the new tile,
		// which means we use j as the offset for the new tile indexes.
		for (int i = 0; i < 4; ++i) {
			nextTile[(j+i)%4] = tile[i];
		}
		return nextTile;
	}



	/******************** Task 4 ********************/
	public static int[][][] solve(int[][] tiles){
		int size = (int) Math.sqrt(tiles.length);
		int[][][] board = new int[size][size][];
		return solve(board,tiles);
	}

	public static int[][][] solve(int[][][] board, int[][] tiles){
		// if we are out of tiles, return the board as we have nothing to do with it.
		if (tiles.length == 0) {
			return board;
		}
		
		int[][][] solution = null;
		
		// go over all the open spaces and try to match the first tile to them
		for (int x = 0; x < board.length; ++x) {
			for (int y = 0; y < board[x].length; ++y) {
				if (board[x][y] == null) {
					// open slot. try to fit a tile in it.
					// try and rotate the tile k times (0<=k<4)
					for (int k = 0; k < 4; ++k) {
						// check if we can place the tile after rotating it k times
						if (canPut(rotate(k, tiles[0]), x, y, board)) {
							// check if we can solve the board by placing the tile in this slot
							solution = solve(put(rotate(k, tiles[0]), x, y, board), delete(0, tiles));
							if (solution != null) {
								// the solution for placing the tile in this open slot
								// is valid! return the solved board.
								return solution;
							}
						}
					}
				}
			}
		}
		
		// we couldn't match all the tiles to the open slots.
		return null;
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
		
		int[][] tiles1 = new int[][]{{3,1,0,2},{3,2,0,1},{2,0,0,2},{0,2,1,0},{0,0,1,1},{1,3,1,0},
				{4,4,3,3},{2,0,2,3},{3,3,3,4},{1,2,0,0},{1,4,1,0},{0,2,4,2},
				{0,1,4,2},{4,3,4,4},{4,4,3,3},{1,0,2,4}};
		
		int[][] tiles2 = new int[][]
				{{3,1,0,2},{0,1,3,2},{0,0,2,2},{0,2,1,0},{0,0,1,1},{1,3,1,0},
				{4,4,3,3},{2,0,2,3},{3,3,3,4},{0,0,1,2},{1,4,1,0},{0,2,4,2},
				{0,1,4,2},{4,3,4,4},{4,4,3,3},{1,0,2,4}};
		
		int[][] tiles3 = new int[][]{{2,0,2,3},{3,3,3,4},{0,0,1,2},{1,4,1,0}};

		EternityPrint.showBoard(solve(tiles3)); // showing a game board
		EternityPrint.showBoard(solve(tiles2)); // showing a game board
		EternityPrint.showBoard(solve(tiles1)); // showing a game board
		
		
	}
}
