/**
 * Triangle defines the methods on a triangle in a 2d world.
 */

/**
 * @author Dvir
 *
 */
public class Triangle extends Polygon {
	/**
	 * Constructor that takes care of initiating our Triangle by sending the given points to be processed by the Polygon super class.
	 * @param p1 First point of the triangle
	 * @param p2 Second point of the triangle
	 * @param p3 Third point of the triangle
	 */
	public Triangle(Point p1, Point p2, Point p3) {
		super(new Point[]{p1, p2, p3}); // call the Polygon constructor to initialize the points array.
	}
	
	/**
	 * Constructor that takes care of creating a deep copy of another triangle.
	 * @param t The triangle to copy into the new one.
	 */
	public Triangle(Triangle t) {
		super(t.getPoints()); // call the Polygon constructor to initialize the points array.
	}
	
	/**
	 * @return Returns the first point that constructs the triangle.
	 */
	public Point getP1() {
		// returns the first point in the array of points defined in the super class Polygon.
		return new Point(this.getPoints()[0]);
	}

	/**
	 * @return Returns the second point that constructs the triangle.
	 */
	public Point getP2() {
		// returns the second point in the array of points defined in the super class Polygon.		
		return new Point(this.getPoints()[1]);
	}
	
	/**
	 * @return Returns the third point that constructs the triangle.
	 */
	public Point getP3() {
		// returns the third point in the array of points defined in the super class Polygon.		
		return new Point(this.getPoints()[2]);
	}
	
	/**
	 * Compares between two objects. Two triangles are equal if and only if they are constructed from the same points.
	 * NOTE: The order of the points doesn't matter.
	 * 
	 * @param other The other object to test equality against.
	 * @return true or false according to the equality status between the objects.
	 */
	public boolean equals(Object other) {
		if (other instanceof Triangle) {
			Point[] thisPoints = this.getPoints(); // an array of all the points in this triangle
			Point[] otherPoints = ((Triangle) other).getPoints(); // an array of all the points in the other triangle
			
			// we are checking out if every point in this triangle
			// has a matching point in the other triangle.
			// since we can assume a valid triangle has 3 points that exist,
			// different than each other and not on the same line,
			// this test is enough.
			boolean found = false; // determines whether we found a matching point
			for (int i = 0; i < thisPoints.length; ++i) {
				found = false;
				// find a matching point in this triangle within the other triangle
				for (int j = 0; j < otherPoints.length && !found; ++j) {
					if (thisPoints[i].equals(otherPoints[j])) {
						// we found a matching point; we can stop searching
						found = true;
					}
				}
				
				if (!found) {
					// found no matching point for point i in this triangle.
					// that means the triangles aren't equal.
					return false;
				}
			}
			
			// if we got here, every point forming our triangle has a match in the other triangle,
			// and therefore they are logically equal triangles.
			return true;
		}
		
		return false;
	}

	public double getArea() {
		// calculate the area of a triangle according to the formula: (d = half the perimeter)
		// AreaOfTriangle(p1, p2, p3) = (D * (D-dist(p1,p2)) * (D-dist(p2,p3)) * (D-dist(p3,p1)))^(1/2)
		double d = this.getPerimeter() / 2; // D - half the perimeter
		double formula = d; // holds the inner formula
		for (int i = 0; i < this.getSides().length; ++i) {
			formula *= (d - this.getSides()[i]); // *(d - dist(Pi, Pi+1))
		}
		return Math.sqrt(formula); // ^(1/2)
	}
	
	public boolean contains(Point p) {
		if (p == null) {
			return false;
		}		
		
		// a triangle contains a point if the sum of the area of the three triangles we can construct
		// with the point and two of the triangle points is equal to the area of the triangle.
		// since we can't compare with precision when using really small numbers, we will check if the
		// absolute value of the subtraction of the sum of areas and the triangle area is <= 0.001.
		return (0.001 >= Math.abs((new Triangle(p, this.getP1(), this.getP2()).getArea() 
								+ new Triangle(p, this.getP2(), this.getP3()).getArea() 
								+ new Triangle(p, this.getP3(), this.getP1()).getArea())
						- this.getArea()));
	}
}
