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
			testPolygon();
			testTriangle();
			testQuadrangle();
			testCircle();
			
			testShapesContainer();
			
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
	
			// testing deep copy constructors
			Point pDeep = new Point(7,7);
			Point pDeepCopy = new Point(pDeep);
			pDeep.move(1, 1);
			test(pDeepCopy.getX() != 1, "Point class doesn't perform deep copy!");
		}
	
		/**
		 * Checks the Polygon class.
		 */
		private static void testPolygon() {
			Point p1=new Point(1,2);
			Point p2=new Point(1,-2);
			Point p3=new Point(-1,-2);
			Point p4=new Point(-1,2);
			Point p5=new Point(1,2);
			Polygon t1=new Triangle(p1,p2,p3);
			Polygon t2=new Triangle(p5,p2,p3);
			
			Polygon t3=new Triangle(t1.getPoints()[0],t1.getPoints()[1],t1.getPoints()[2]);
			test(t1.equals(t3),"Polygons should be equal.");	//testing getPoints
			p5.move(-4,-4);
			test(t1.equals(t2),"Polygons should be equal.");	//testing deep copy in Polygon's constructor
			
			Polygon q1=new Quadrangle(p1,p2,p3,p4);
			test(!(t1.equals(q1)),"Polygons should not be equal.");
			test(!(q1.equals(t1)),"Polygons should not be equal.");
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
			test(t.contains(new Point(0, 1)), "Triangle should contain a point on its sides.");
			
			test(new Triangle(new Point(0,0), new Point(0,10), new Point(10,0)).contains(new Point(5, 5)), "Triangle should contain a point on its sides.");
			
			
			t3.move(new Point(8, 8));
			test(!t3.contains(new Point(2, 3)), "Triangle contains a point it shouldn't contain.");
			
			Triangle t4 = new Triangle(new Point(1,1), new Point(2,2), new Point(3, 1));
			test(t4.getPerimeter() == 4.82842712474619, "Triangle Perimeter should be 4.82842712474619 | Actual: " + t4.getPerimeter());
			test(t4.getArea() == 0.9999999999999996, "Triangle Area should be 1.7071067811865475 | Actual: " + t4.getArea());
			
			t4.move(p2);
			test(t4.getPerimeter() == 4.82842712474619, "Triangle Perimeter after move should be 4.82842712474619 | Actual: " + t4.getPerimeter());
			test(t4.getArea() == 0.9999999999999996, "Triangle Area after move should be 1.7071067811865475 | Actual: " + t4.getArea());	
	
			test(t.getP1().equals(p1), "Triangle.getP1 doesn't match the original point.");
			test(!t4.getP1().equals(p1), "Triangle.getP1 matches a point it shouldn't.");
			
			// testing deep copy constructors
			Triangle tDeep = new Triangle(new Point(0, 0), new Point(0, 10), new Point(15, 0));
			Triangle tDeepCopy = new Triangle(tDeep);
			tDeep.move(new Point(1, 1));
			test(!tDeep.equals(tDeepCopy), "Triangle class doesn't perform deep copy!");
		}
		
		/**
		 * Checks the Quadrangle class.
		 */
		private static void testQuadrangle() {
			Point p1 = new Point(0, 0);
			Point p2 = new Point(0, 10);
			Point p3 = new Point(15, 0);
			Point p4 = new Point(15, 15);
			
			Quadrangle q = new Quadrangle(p1, p2, p3, p4);
			Quadrangle q2 = new Quadrangle(p2, p3, p4, p1);
			Quadrangle q3 = new Quadrangle(q);
			
			test(q.getNumOfPoints() == 4, "Quadrangle has more or less than 4 points.");
			test(q.contains(new Point(2, 3)), "Quadrangle reports it doesn't contain a point when it should.");
			test(q.equals(q2), "Quadrangle should be equal to another Quadrangle constructed of the same points.");
			test(q.contains(new Point(0, 1)), "Quadrangle should contain a point on its sides.");
			
			q3.move(new Point(8, 8));
			test(!q3.contains(new Point(2, 3)), "Quadrangle contains a point it shouldn't contain.");
			
			Quadrangle q4 = new Quadrangle(new Point(0,0), new Point(0,1), new Point(1, 1), new Point(1, 0));
			test(q4.getPerimeter() == 4.0, "Quadrangle Perimeter should be 4.82842712474619 | Actual: " + q4.getPerimeter());
			test(q4.getArea() == 0.9999999999999997, "Quadrangle Area should be 1.7071067811865475 | Actual: " + q4.getArea());
			
			q4.move(p2);
			test(q4.getPerimeter() == 4.0, "Quadrangle Perimeter after move should be 4.82842712474619 | Actual: " + q4.getPerimeter());
			test(q4.getArea() == 0.9999999999999997, "Quadrangle Area after move should be 1.7071067811865475 | Actual: " + q4.getArea());	
	
			test(q.getP1().equals(p1), "Quadrangle.getP1 doesn't match the original point.");
			test(!q4.getP1().equals(p1), "Quadrangle.getP1 matches a point it shouldn't.");
			
			// testing deep copy constructors
			Quadrangle qDeep = new Quadrangle(new Point(0, 0), new Point(0, 10), new Point(15, 0), new Point(15, 15));
			Quadrangle qDeepCopy = new Quadrangle(qDeep);
			qDeep.move(new Point(1, 1));
			test(!qDeep.equals(qDeepCopy), "Quadrangle class doesn't perform deep copy!");
		}	
		
		
		/**
		 * Checks the Circle class.
		 */
		private static void testCircle() {
			Point p1 = new Point(0, 0);
			Point p2 = new Point(0, 10);
			Point p3 = new Point(15, 0);
	//		Point p4 = new Point(15, 15);
			
			Circle c = new Circle(p1, 10);
	//		Circle c2 = new Circle(p2, 2);
			Circle c3 = new Circle(p1, 10);
			
			test(c.getCenter().equals(p1), "Circle center doesn't match its defining center point.");
			test(c.contains(new Point(2, 3)), "Circle reports it doesn't contain a point when it should.");
			test(c.equals(c3), "Circle should be equal to another Circle constructed of the same points.");
			
			c3.move(new Point(8, 8));
			test(!c3.contains(new Point(0, 0)), "Circle contains a point it shouldn't contain.");
			
			Circle c4 = new Circle(new Point(0,0), 1);
			test(c4.getPerimeter() == Math.PI * 2, "Circle Perimeter should be 6.28~ | Actual: " + c4.getPerimeter());
			test(c4.getArea() == Math.PI, "Circle Area should be 3.14~ | Actual: " + c4.getArea());
			test(c4.getRadius() == 1, "Circle Radius should be 1.0 | Actual: " + c4.getRadius());
			
			c4.move(p2);
			test(c4.getPerimeter() == Math.PI * 2, "Circle Perimeter after move should be 6.28~ | Actual: " + c4.getPerimeter());
			test(c4.getArea() == Math.PI, "Circle Area after move should be 3.14~ | Actual: " + c4.getArea());	
			test(c4.getRadius() == 1, "Circle Radius should be 1.0 | Actual: " + c4.getRadius());
			
			test(!c4.getCenter().equals(p3), "Circle.getCenter matches a point it shouldn't.");
			
			// testing deep copy constructors
			Circle cDeep = new Circle(new Point(0, 0), 1);
			Circle cDeepCopy = new Circle(cDeep);
			cDeep.move(new Point(1, 1));
			test(!cDeep.equals(cDeepCopy), "Circle class doesn't perform deep copy!");		
		}	
		
		/**
		 * Tests the ShapesContainer class.
		 */
		private static void testShapesContainer() {
			ShapesContainer shapes = new ShapesContainer();
			shapes.add(new Triangle(new Point(122.0, 145.0), new Point(133.0, 146.0), new Point(123.0, 63.0)));
			shapes.add(new Triangle(new Point(122.0, 144.0), new Point(129.0, 143.0), new Point(166.0, 71.0)));
			shapes.add(new Triangle(new Point(122.0, 144.0), new Point(131.0, 145.0), new Point(191.0, 91.0)));
			shapes.add(new Triangle(new Point(132.0, 143.0), new Point(124.0, 143.0), new Point(85.0, 79.0)));
			shapes.add(new Triangle(new Point(132.0, 143.0), new Point(123.0, 143.0), new Point(65.0, 101.0)));
			shapes.add(new Triangle(new Point(125.0, 142.0), new Point(99.0, 172.0), new Point(145.0, 174.0)));
			
			test(shapes.getShapesNum() == 6, "ShapesContainer should have 6 shapes in it. (Got " + shapes.getShapesNum() + " instead)");
			shapes.remove(0);
			test(shapes.getShapesNum() == 5, "ShapesContainer should have 5 shapes in it after the first remove. (Got " + shapes.getShapesNum() + " instead)");
			
			shapes.add(new Triangle(new Point(125.0, 142.0), new Point(99.0, 172.0), new Point(145.0, 174.0)));
			test(shapes.getShapesNum() == 6, "ShapesContainer should have 6 shapes in it. (After remove and adding back) (Got " + shapes.getShapesNum() + " instead)");
			
			test(shapes.sumArea() == 2163.5000000000023, "ShapesContainer.sumArea should be 2163.5000000000023. Actual: " + shapes.sumArea());
			test(shapes.sumPerimeter() == 985.776779266402, "ShapesContainer.sumPerimeter should be 985.776779266402. Actual: " + shapes.sumPerimeter());
			
			shapes.move(new Point(1, 1));
			test(((Triangle) shapes.getShape(0)).getPoints()[0].getX() == 126.0, "First point in the first shape in shapesContainer X coordinate should be 126.0. Actual: " + ((Triangle) shapes.getShape(0)).getPoints()[0].getX());
			test(((Triangle) shapes.getShape(0)).getPoints()[0].getY() == 143.0, "First point in the first shape in shapesContainer Y coordinate should be 143.0. Actual: " + ((Triangle) shapes.getShape(0)).getPoints()[0].getY());
		}
	}