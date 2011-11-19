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
		int[][] clauses=null;
		// YOUR CODE HERE
		return clauses;
	}

	// Task 5.2
	public static int[][] atMostOne(int[] lits) {
		int[][] clauses=null;
		// YOUR CODE HERE
		return clauses;
	}

	// Task 5.4
	public static int[][] notSameColor(int[] x1, int[] x2, int[] x3, int[] x4) {
		int[][] clauses=null;
		// YOUR CODE HERE
		return clauses;
	}

	// Task 5.3
	public static int[][] decode(int[][][] boardMap, boolean[] satSol) {
		int[][] res=null;
		// YOUR CODE HERE
		return res;
	}

	// Task 5.5
	public static int[][] satBasedSolver(int n, int m, int c) {
		// create map
		int[][][] boardMap = map(n,m,c);
		// initialize SAT solver
		SATSolver.init(n*m*c);

		// YOUR CODE HERE
		// add clauses to SAT solver
		// using SATSolver.addClauses(int[][]) function

		// get solution from SAT Solver
		boolean[] satSol=SATSolver.getSolution();
		// decode solution
		return decode(boardMap,satSol);
	}
	
	/* ********************************** *
	 * *  Main you may want to use      * *
	 * ********************************** */
/*
	public static void main(String[] args) {
		int n=4, m=4, c=3;
		long startTime=System.currentTimeMillis();
		int[][] sol=satBasedSolver(n, m, c);
		long endTime=System.currentTimeMillis();
		System.out.println("Solution time : "+(endTime-startTime)+" ms");
		System.out.println("Solution found: "+(sol!=null));
		if(sol!=null) {
			System.out.println("Valid solution: "+Part1.isValidSolution(sol,c));
		}
	}
*/
}
