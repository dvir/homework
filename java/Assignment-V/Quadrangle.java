/**
 * Quadrangle defines the methods on a Quadrangle in a 2d world.
 */

/**
 * @author Dvir
 *
 */
public class Quadrangle extends Polygon {
	/**
	 * Constructor that takes care of initiating our Quadrangle by sending the given points to be processed by the Polygon super class.
	 * @param p1 First point of the quadrangle
	 * @param p2 Second point of the quadrangle
	 * @param p3 Third point of the quadrangle
	 * @param p4 Fourth point of the quadrangle
	 */	
	public Quadrangle(Point p1, Point p2, Point p3, Point p4) {
		super(new Point[]{p1, p2, p3, p4}); // call the Polygon constructor to initialize the points array.
	}
	
	/**
	 * Constructor that takes care of creating a deep copy of another quadrangle.
	 * @param q The quadrangle to copy into the new one.
	 */	
	public Quadrangle(Quadrangle q) {
		super(q.getPoints()); // call the Polygon constructor to initialize the points array.
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
	 * @return Returns the fourth point that constructs the triangle.
	 */	
	public Point getP4() {
		// returns the fourth point in the array of points defined in the super class Polygon.
		return new Point(this.getPoints()[3]);
	}	
	
	/**
	 * Compares between two objects. Two quadrangles are equal if and only if they are constructed from the same points.
	 * NOTE: A quadrangle constructed from points 1-2-3-4 is equal to a 2-3-4-1, 3-4-1-2 and 4-1-2-3 quadrangle.
	 * 
	 * @param other The other object to test equality against.
	 * @return true or false according to the equality status between the objects.
	 */	
	public boolean equals(Object other) {
		if (other instanceof Quadrangle) {
			Point[] thisPoints = this.getPoints();
			Point[] otherPoints = ((Quadrangle) other).getPoints();
			
			// we are checking out if every point in this Quadrangle
			// matches the respective point in the other Quadrangle.
			// we are also checking for a match in a clock-wise rotation - 1,2,3,4 should be equal to 2,3,4,1, etc.
			boolean noMatch; // determines whether we found a matching sequence of points
			for (int j = 0; j < 4; ++j) {
				noMatch = false;
				for (int i = 0; i < thisPoints.length && !noMatch; ++i) {
					if (!thisPoints[i].equals(otherPoints[(i+j)%4])) {
						// the points in the i and j slot aren't equal
						noMatch = true;
					}
				}
				
				if (!noMatch) {
					// we found a matching sequence between the Quadrangles; that means they are equal.
					return true;
				}
			}
		}
		
		// if we got here, the object we are comparing to is not a Quadrangle
		// or they don't have a matching sequence between their constructing points,
		// and therefore they aren't equal.
		return false;
	}

	public double getArea() {
		// divide the quadrangle into two triangles and sum their areas
		return (new Triangle(this.getP1(), this.getP2(), this.getP3()).getArea() 
				+ new Triangle(this.getP3(), this.getP4(), this.getP1()).getArea());
	}
	
	public boolean contains(Point p) {
		if (p == null) {
			throw new RuntimeException("Quadrangle.contains(Point p) received a null point object.");
		}		
		
		// divide the quadrangle into two triangles and search for the point in either of them		
		return (new Triangle(this.getP1(), this.getP2(), this.getP3()).contains(p) 
				|| new Triangle(this.getP3(), this.getP4(), this.getP1()).contains(p));
	}
}
