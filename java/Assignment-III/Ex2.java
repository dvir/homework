/***************************************************
 * intro111/ipis111: Third assignment              *
 *                                                 *
 * This class is for assignment #3 - Part 2        *
 *                                                 *
 * Author(s): ### Dvir Azulay (dvirazu@post.bgu.ac.il), Ory Band (oryb@post.bgu.ac.il) ##### *
 * Date: 20/12/2011                                *
 *                                                 *
 ***************************************************/

/*
 * Important! Add comments to every method!
 *
 * The "return" statement at the end of each method and the initial value 
 * is just so this skeleton file compiles, change it according to your needs
 */

public class Ex2 {

	public static double distance(int[] point1, int[] point2) {
		return Math.sqrt(Math.pow(point1[0] - point2[0], 2)
				+ Math.pow(point1[1] - point2[1], 2));
	}

	public static boolean lexGreaterThan(int[] p1, int[] p2) {
		return (p1[0] > p2[0]) || (p1[0] == p2[0] && p1[1] > p2[1]);
	}

	/******************** Task 1 ********************/
	public static int[][] findClosestSimple(int[][] points) {
		// no pairs to find if there are less than 2 points
		if (points == null || points.length < 2) {
			return null;
		}
		
		int[][] res = new int[2][2]; // holds the two closest points in the points array
		double shortestDistance = -1; // the shortest distance
		for (int i = 0; i < points.length-1; ++i) {
			for (int j = i + 1; j < points.length; ++j) {
				// if shortestDistance is -1, it means we didn't fill res with a pair yet, so we have no distance to compare to. fill it
				// else, check if the distance between the current two points is shorter than the ones we already have, and if so, replace them
				// NOTE: we are changing the shortestDistance everytime we are changing the points so we don't have to recalculate the distance between them.
				if (shortestDistance == -1 || distance(points[i], points[j]) < shortestDistance) {
					shortestDistance = distance(points[i], points[j]);
					res[0] = points[i];
					res[1] = points[j];
				}
			}
		}
		
		return res;
	}

	/******************** Task 2 ********************/
	static int[][] splitLeft(int[][] points) {
		// calculate the amount of points we are going to place in the new array
		int pointsAmount = (int)(points.length / 2);
		
		// create the new array based on the amount of points we need to place in it
		int[][] leftSide = new int[pointsAmount][2];
		for (int i = 0; i < pointsAmount; ++i) {
			leftSide[i][0] = points[i][0];
			leftSide[i][1] = points[i][1];
		}
		
		return leftSide;
	}
	
	static int[][] splitRight(int[][] points) {
		// calculate the amount of points we are going to place in the new array
		// when we split, we might have an odd point. make sure we take it when creating the right side
		int pointsAmount = points.length - (int)(points.length / 2);
		
		// create the new array based on the amount of points we need to place in it		
		int[][] rightSide = new int[pointsAmount][2];
		
		// we are running on the indexes from the end of the array this time, until pointsAmount
		for (int i = points.length-1, j = 0; i > points.length - 1 - pointsAmount; --i, ++j) {
			rightSide[j][0] = points[i][0];
			rightSide[j][1] = points[i][1];
		}		
		
		return rightSide;
	}
	
	static int[][] merge(int[][] leftSide, int[][] rightSide) {
		// we are merging two arrays; the new array is the sum of the size of both of them
		int[][] res = new int[leftSide.length + rightSide.length][2];
		int i = 0, j = 0, k = 0; // indexes for the left, right and result arrays
		// run until we hit the end of one of the arrays
		while(i < leftSide.length && j < rightSide.length) {
			// we need to take the smaller value of the current array indexes values.
			// compare them and increase the index of the relevant array
			if (lexGreaterThan(rightSide[j], leftSide[i])) {
				res[k][0] = leftSide[i][0];
				res[k][1] = leftSide[i][1];
				++i; // increasing the left side array index
			}
			else {
				res[k][0] = rightSide[j][0];
				res[k][1] = rightSide[j][1];
				++j; // increasing the right side array inedx
			}
			
			++k; // increasing the result array index
		}
		
		// if we have any leftovers in one of the arrays, stick them to the end of our result array
		for (; i < leftSide.length; ++i, ++k) {
			res[k][0] = leftSide[i][0];
			res[k][1] = leftSide[i][1];
		}
		
		for (; j < rightSide.length; ++j, ++k) {
			res[k][0] = rightSide[j][0];
			res[k][1] = rightSide[j][1];
		}		
		
		return res;
	}
	
	public static int[][] sortByLex(int[][] points) {
		// nothing to sort if we have less than 2 points in the array
		if (points == null || points.length < 2) {
			return points;
		}
		
		// split the arrays to left and right and sort both sides seperately, and then merge them back.
		return merge(sortByLex(splitLeft(points)), sortByLex(splitRight(points)));
	}

	/*
	 * swapCoordinates takes an array of points with (x,y) coordinates and swaps between the x and y
	 */
	static void swapCoordinates(int[][] points) {
		int temp;
		
		// swap between the x and y coordinates in the array
		for (int i = 0; i < points.length; ++i) {
			temp = points[i][0];
			points[i][0] = points[i][1];
			points[i][1] = temp;
		}
	}
	
	/*
	 * copyPoints takes an array of points and copies it to a new array
	 */
	static int[][] copyPoints(int[][] points) {
		// create the new array
		int[][] res = new int[points.length][2];
		
		// copy the points to the new array _by value_
		for (int i = 0; i < points.length; ++i) {
			res[i][0] = points[i][0];
			res[i][1] = points[i][1];
		}
		
		return res;
	}
	
	public static int[][] sortByY(int[][] points) {
		// create a new array containing a copy of the points array (so we won't change the original array)
		int[][] res = copyPoints(points);
		
		// swap between the x and y coordinates so we can use our previous sorting function
		swapCoordinates(res);
		
		// sort the reversed array
		res = sortByLex(res);
		
		// swap back between the x and y coordinates so we get the correct array back, sorted by Y
		swapCoordinates(res);
		
		return res;
	}

	public static int[] duplicates(int[][] points) {
		// nothing to do if we have less than 2 points
		if (points == null || points.length < 2) {
			return null;
		}
		
		// go over the array, checking pairs of points each time to see if we have any duplicates.
		// if we find a duplicate point, return it.
		for (int i = 0; i < points.length-1; ++i) {
			for (int j = i + 1; j < points.length; ++j) {
				if (points[i][0] == points[j][0] && points[i][1] == points[j][1]) {
					// we found a duplicate point! return it
					return new int[]{points[i][0], points[i][1]};
				}
			}
		}
		
		return null;
	}

	public static int[][] filterPointsByRange(double fromX, double toX, int[][] points) {
		int pointsAmount = 0; // holds the amount of points in the fromX <= x <= toX range
		for (int i = 0; i < points.length; ++i) {
			if (points[i][0] >= fromX && points[i][0] <= toX) {
				++pointsAmount;
			}
		}
		
		// a new array that holds only the points that has an x coordinate in the range of fromX <= x <= toX
		int[][] res = new int[pointsAmount][2]; // we pre-calculated the array size earlier
		
		// if we have more than one point, create the new array that will hold only the points in the range
		if (pointsAmount > 0) {
			int j = 0;
			for (int i = 0; i < points.length; ++i) {
				// if the x coordinate is in the correct range, add it to the new array
				if (points[i][0] >= fromX && points[i][0] <= toX) {
					res[j][0] = points[i][0];
					res[j][1] = points[i][1];
					++j; // increase the new array index
				}				
			}
		}
		
		// return the filtered points array
		return res;
	}

	/******************** Task 3 ********************/
	public static int[][] findClosest(int[][] points) {
		int[][] res;
		int[][] pointsSortedByLex = sortByLex(points);
		int[] p = duplicates(pointsSortedByLex);
		if (p == null)
			res = findClosestRecursive(pointsSortedByLex);
		else {
			res = new int[1][];
			res[0] = p;
		}
		return res;
	}

	public static int[][] findClosestRecursive(int[][] points) {
		// nothing to do if we have less than 2 points
		if (points == null || points.length < 2) {
			return null;
		}
		
		// if we have between 2 to 4 points, calculate it with the simple function
		if (points.length > 1 && points.length < 5) {
			return findClosestSimple(points);
		}
		else {
			// we have more than 4 points, use merge sort algorithm to find the closest pair
			int[][] pointsSortedByLexLeft = splitLeft(points); // holds the left side of the array, sorted
			int[][] pointsSortedByLexRight = splitRight(points); // holds the right side of the array, sorted 
																 // (note: contains the odd point if there's one after splitting the array)
			
			int[][] closestPair1 = findClosestRecursive(pointsSortedByLexLeft); // find the closest pair in the left side array
			int[][] closestPair2 = findClosestRecursive(pointsSortedByLexRight); // find the closest pair in the right side array
			
			double distance = distance(closestPair1[0], closestPair1[1]); // holds the distance between the closest pair
			int[][] res = new int[][]{closestPair1[0], closestPair1[1]}; // holds the closest pair
			
			// we start with the pair in closestPair1 and check if the pair in closestPair2 is closer. 
			// if so, use it instead as the starting values.
			if (distance(closestPair2[0], closestPair2[1]) < distance) {
				distance = distance(closestPair2[0], closestPair2[1]);
				res[0] = closestPair2[0];
				res[1] = closestPair2[1];
			}
			
			// the offset X coordinate for the middle points
			int mX = pointsSortedByLexRight[0][0];
			
			// holds all the points that has an X coordinate between mX-distance and mx+distance.
			// we only hold the points in the range of +-distance from the middle of the array as only
			// they might be the ones that are closer to each other than the ones we found in the sides already.
			// we also sort by Y to make it easier to conclude our search with a relevant match after filtering.
			int[][] middle = sortByY(filterPointsByRange(mX-distance, mX+distance, points));
			for (int i = 0; i < middle.length-1; ++i) {
				for (int j = i + 1; j < middle.length; ++j) {
					// if the differences between the Y coordinates is equal or larger than distance,
					// halt the search as we know it won't be closer than the one we already found.
					if (middle[j][1] - middle[i][1] >= distance) {
						return res;
					}
					
					// if the two points are closer than what we already had,
					// use these instead.
					if (distance(middle[i], middle[j]) < distance) {
						distance = distance(middle[i], middle[j]);
						res[0] = middle[i];
						res[1] = middle[j];						
					}
				}
			}
			
			return res;
		}
	}

	/******************** Auxiliary functions ********************/

	/**
	 * @param arr
	 *          the input 2D array
	 * @return a string representation of a 2D array
	 */
	public static String matrixToString(int[][] arr) {
		String ret = "{ ";

		if (arr == null)
			ret = "null";
		else {
			for (int i = 0; i < arr.length; i++) {
				if (arr[i] != null) {
					ret += "{ ";
					for (int j = 0; j < arr[i].length; j++)
						ret += arr[i][j] + " ";
					ret += "} ";
				}
			}
			ret += "}";
		}

		return ret;
	}

	/**
	 * @param arr the input array
	 * @return a string representation of an array
	 */
	public static String arrayToString(int[] arr) {
		String ret = "{ ";

		if (arr == null)
			ret = "null";
		else {
			for (int i = 0; i < arr.length; i++)
				ret += arr[i] + " ";
			ret += "}";
		}

		return ret;
	}

	public static void main(String[] args) {
		// Test task 1
		int[][] test1in = { { 9, 5 }, { 2, 9 }, { 2, 6 }, { 8, 6 }, { 1, 2 },
				{ 1, 3 }, { 8, 9 }, { 0, 7 }, { 5, 9 }, { 9, 8 } };
		int[][] test1exp = { { 1, 2 }, { 1, 3 } };
		System.out.println("Test 1: expected=" + matrixToString(test1exp)
				+ " actual=" + matrixToString(findClosestSimple(test1in)));

		// Test task 2a
		int[][] test2in = { { 1, 2 }, { 0, 7 }, { 2, 9 }, { 2, 6 }, { 1, 3 } };
		int[][] test2exp = { { 0, 7 }, { 1, 2 }, { 1, 3 }, { 2, 6 }, { 2, 9 } };
		System.out.println("Test 2: expected=" + matrixToString(test2exp)
				+ " actual=" + matrixToString(sortByLex(test2in)));

		// Test task 2b
		int[][] test3in = { { 9, 8 }, { 5, 9 }, { 9, 5 }, { 8, 9 }, { 8, 6 } };
		int[][] test3exp1 = { { 9, 5 }, { 8, 6 }, { 9, 8 }, { 8, 9 }, { 5, 9 } };
		int[][] test3exp2 = { { 9, 5 }, { 8, 6 }, { 9, 8 }, { 5, 9 }, { 8, 9 } };
		System.out.println("Test 3: expected=" + matrixToString(test3exp1) + " or "
				+ matrixToString(test3exp2) + " actual="
				+ matrixToString(sortByY(test3in)));

		// Test task 2c
		int[][] test4in = { { 0, 7 }, { 1, 2 }, { 1, 3 }, { 2, 6 }, { 2, 9 } };
		int[] test4exp = null;
		System.out.println("Test 4: expected=" + arrayToString(test4exp)
				+ " actual=" + arrayToString(duplicates(test4in)));

		int[][] test5in = { { 0, 7 }, { 1, 2 }, { 1, 2 }, { 2, 6 }, { 2, 6 } };
		int[] test5exp1 = { 1, 2 };
		int[] test5exp2 = { 2, 6 };
		System.out.println("Test 5: expected=" + arrayToString(test5exp1) + " or "
				+ arrayToString(test5exp2) + " actual="
				+ arrayToString(duplicates(test5in)));

		// Test task 2d
		int[][] test6in = { { 3, 0 }, { 3, 5 }, { 4, 3 }, { 6, 4 }, { 7, 3 } };
		int[][] test6exp = { { 3, 0 }, { 3, 5 }, { 4, 3 } };
		System.out.println("Test 6: expected=" + matrixToString(test6exp)
				+ " actual=" + matrixToString(filterPointsByRange(2.9, 4.2, test6in)));

		// Test task 3
		 int[][] test7in = {{0,7},{1,2},{1,3},{2,6},{2,9}};
		 int[][] test7exp = {{1,2},{1,3}};
		 System.out.println("Test 7: expected=" + matrixToString(test7exp) +
		 " actual=" + matrixToString(findClosestRecursive(test7in)));

		// Test task 3
		int[][] test8in = { { 9, 5 }, { 2, 9 }, { 2, 6 }, { 8, 6 }, { 1, 2 },
				{ 1, 3 }, { 8, 9 }, { 0, 7 }, { 5, 9 }, { 9, 8 } };
		int[][] test8exp = { { 1, 2 }, { 1, 3 } };
		System.out.println("Test 8: expected=" + matrixToString(test8exp)
				+ " actual=" + matrixToString(findClosest(test8in)));
	}
}
