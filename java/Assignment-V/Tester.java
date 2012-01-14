/**
 * This is a testing framework. Use it extensively to verify that your code is working
 * properly.
 */
public class Tester {

	private static boolean testPassed = true;
	private static int testNum = 0;
	
	/**
	 * This entry function will test all classes created in this assignment.
	 * @param args command line arguments
	 */
	public static void main(String[] args) {
		
		// Each function here should test a different class.
		testPoint();
		testTriangle();
		/* TODO - write a function for each class */
		
		// Notifying the user that the code have passed all tests. 
		if (testPassed) {
			System.out.println("All " + testNum + " tests passed!");
		}
	}

	/**
	 * This utility function will count the number of times it was invoked. 
	 * In addition, if a test fails the function will print the error message.  
	 * @param exp The actual test condition
	 * @param msg An error message, will be printed to the screen in case the test fails.
	 */
	private static void test(boolean exp, String msg) {
		testNum++;
		
		if (!exp) {
			testPassed = false;
			System.out.println("Test " + testNum + " failed: "  + msg);
		}
	}
	
	/**
	 * Checks the Point class.
	 */
	private static void testPoint() {
		// Test Point class
		Point p1 = new Point(10, 20);

		test(p1.getX() == 10, "X should be 10.");
		test(p1.getY() == 20, "Y should be 20.");
		
		Point p2 = new Point(p1);
		test(p2.equals(p1), "Point should be equal.");
		test(p2 != p1, "Point should be different points.");
		test(p2.distance(p1) == 0, "Distance should be zero.");
		
		p2.move(p1);
		test(p2.getX() == 2*p1.getX(), "P2 x should be twice as P1 x.");
		test(p2.getY() == 2*p1.getY(), "P2 y should be twice as P1 y.");
		test(p2.distance(p1) == 22.360679774997898, "Distance should be 22.360679774997898");
		test(!p2.equals(p1), "Point should not be equal.");
		
		/* TODO add more tests to the Point class! */
	}

	/**
	 * Checks the Triangle class.
	 */
	private static void testTriangle() {
		Point p1 = new Point(0, 0);
		Point p2 = new Point(0, 10);
		Point p3 = new Point(15, 0);
		
		Triangle t = new Triangle(p1, p2, p3);
		Triangle t2 = new Triangle(p2, p1, p3);
		Triangle t3 = new Triangle(t);
		
		test(t.getNumOfPoints() == 3, "Triangle has more or less than 3 points.");
		test(t.contains(new Point(2, 3)), "Triangle reports it doesn't contain a point when it should.");
		test(t.equals(t2), "Triangle should be equal to another triangle constructed of the same points.");
		
		t3.move(new Point(8, 8));
		test(t.contains(new Point(2, 3)), "Triangle contains a point it shouldn't contain.");
		
		Triangle t4 = new Triangle(new Point(1,1), new Point(2,2), new Point(3, 1));
		test(t4.getPerimeter() == 4.82842712474619, "Perimeter should be 4.82842712474619 | Actual: " + t4.getPerimeter());
		test(t4.getArea() == 1.7071067811865475, "Area should be 1.7071067811865475 | Actual: " + t4.getArea());
		
		t4.move(p2);
		test(t4.getPerimeter() == 4.82842712474619, "Perimeter should be 4.82842712474619 | Actual: " + t4.getPerimeter());
		test(t4.getArea() == 1.7071067811865475, "Area should be 1.7071067811865475 | Actual: " + t4.getArea());	}
}