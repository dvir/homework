public class Part2 {	
	/* ********************************** *
	 * *  Part 5 - SAT Based solver     * *
	 * ********************************** */

	// provided
	public static int[][][] map(int n, int m, int c) {
		int[][][] boardMap = new int[n][m][c];
		for(int y=0; y<n; y=y+1) {
			for(int x=0; x<m; x=x+1) {
				for(int clr=0; clr<c; clr=clr+1) {
					boardMap[y][x][clr] = clr + x*c + y*c*m + 1;
				}
			}
		}
		return boardMap;
	}
	
	// Task 5.1
	public static int[][] atLeastOne(int[] lits) {
		int[][] clauses = new int[1][lits.length];
		for (int i = 0; i < lits.length; ++i) {
			clauses[0][i] = lits[i];
		}
		return clauses;
	}

	// Task 5.2
	public static int[][] atMostOne(int[] lits) {
		// calculate the amount of clauses we will need - just like arithmetic progression - n*(n+1) / 2 - also, this will always be an int since one of n,n+1 will be even
		int clausesAmount = ((lits.length-1)*(lits.length))/2;
		int[][] clauses = new int[clausesAmount][2];
		clausesAmount = 0; // holds the current clause index we are placing the clause into
		for (int i = 0; i < lits.length; ++i) {
			for (int j = i + 1; j < lits.length; ++j) {	
				// translates two literals into the structure of ( !L1 | !L2 )
				clauses[clausesAmount++] = new int[]{lits[i]*(-1), lits[j]*(-1)};
			}
		}
	
		return clauses;
	}

	// Task 5.3
	public static int decodeCell(int[] cellMap, boolean[] satSol) {
		int cellIndex;
		// go over the cell literals. once we hit a literal that is positive and his SAT solution value is true - that is the color of the cell.
		for (int i = 0; i < cellMap.length; ++i) {
			cellIndex = cellMap[i];
			if ((cellIndex > 0 && satSol[cellIndex]) || (cellIndex < 0 && !satSol[Math.abs(cellIndex)])) {
				return i; // since i goes over the range [0-cellMap.length) it actually holds the current color we are checking 
			}
		}
		
		// no color found for some mysterious reasons
		return -1;
	}
	
	public static int[][] decode(int[][][] boardMap, boolean[] satSol) {
		int[][] res = null;
		// if the solution is valid, go over the board and decode the literals into the actual colors
		if (satSol != null && satSol.length > 0) {
			res = new int[boardMap.length][boardMap[0].length];
			for (int i = 0; i < res.length; ++i) {
				for (int j = 0; j < res[i].length; ++j) {
					res[i][j] = decodeCell(boardMap[i][j], satSol); // decodes the cell SAT literals data into the actual color of the cell found in the solution
				}
			}
		}
		return res;
	}
	
	// Task 5.4
	public static int[][] notSameColor(int[] x1, int[] x2, int[] x3, int[] x4) {
		int[][] clauses = new int[x1.length][4];
		// create clauses for all the possible colors in the rectangular corners
		// the clauses are in the structure of ( !C1 | !C2 | !C3 | !C4 ) to make sure we are not painting a rectangular with four corners of the same color.
		for (int i = 0; i < x1.length; ++i) {
			clauses[i] = new int[]{x1[i]*(-1), x2[i]*(-1), x3[i]*(-1), x4[i]*(-1)};
		}
		return clauses;
	}

	// Task 5.5
	public static int[][] satBasedSolver(int n, int m, int c) {
		// create map
		int[][][] boardMap = map(n,m,c);
		// initialize SAT solver
		SATSolver.init(n*m*c);

		// add trivial conditions - a cell should represent at least one color
		for (int y = 0; y < n; ++y) {		
			for (int x = 0; x < m; ++x) {
				SATSolver.addClauses(atLeastOne(boardMap[y][x]));
			}
		}
		
		// add conditions for each and every rectangular we can form inside our n*m board, and make sure we aren't painting any of its four corners with the same color
		for (int yLeft = 0; yLeft < n-1; ++yLeft) {		
			for (int xLeft = 0; xLeft < m-1; ++xLeft) {
				for (int xRight = xLeft + 1; xRight < m; ++xRight) { 
					for (int yRight = yLeft + 1; yRight < n; ++yRight) {
						SATSolver.addClauses(notSameColor(
													boardMap[yLeft][xLeft],
													boardMap[yRight][xLeft],
													boardMap[yLeft][xRight],
													boardMap[yRight][xRight]));
					}
				}
			}
		}
		

		// get solution from SAT Solver
		boolean[] satSol=SATSolver.getSolution();
		// decode solution
		return decode(boardMap,satSol);
	}

	/* ********************************** *
	 * *  Main you may want to use      * *
	 * ********************************** */

	public static void main(String[] args) {
		int n = 10, m = 10, c = 4;
		if (args.length > 0) {
			// we received arguments from command line.
			n = Integer.parseInt(args[0]);
			m = Integer.parseInt(args[1]);
			c = Integer.parseInt(args[2]);
		}
		
		long startTime=System.currentTimeMillis();
		
		// launch our SAT solver for the given board dimensions and colors
		int[][] result = satBasedSolver(n, m, c);
		
		long endTime=System.currentTimeMillis();
					
		if (result == null) {
			// we didn't find any solution :(
			System.out.println("Couldn't find a valid solution. :(");
		}
		else {
			// we found a valid solution!
			// print the result board
			System.out.println(" -----------------------------------------");
			for (int i = 0; i < result.length; ++i) {
				for (int j = 0; j < result[i].length; ++j) {
					System.out.print(" | " + result[i][j]);
				}
				System.out.println(" |");
				System.out.println(" -----------------------------------------");
			}
		}
		
		System.out.println("Solution for board " + n + "x" + m + " / " + c);
		System.out.println("Solution time : "+(endTime-startTime)+" ms");		
		
		System.out.println();
	}
}
