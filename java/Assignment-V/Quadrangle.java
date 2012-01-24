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
	 * @return Returns the first point that constructs the quadrangle.
	 */	
	public Point getP1() {
		// returns the first point in the array of points defined in the super class Polygon.
		return new Point(this.getPoints()[0]);
	}
	
	/**
	 * @return Returns the second point that constructs the quadrangle.
	 */	
	public Point getP2() {
		// returns the second point in the array of points defined in the super class Polygon.
		return new Point(this.getPoints()[1]);
	}
	
	/**
	 * @return Returns the third point that constructs the quadrangle.
	 */	
	public Point getP3() {
		// returns the third point in the array of points defined in the super class Polygon.
		return new Point(this.getPoints()[2]);
	}

	/**
	 * @return Returns the fourth point that constructs the quadrangle.
	 */	
	public Point getP4() {
		// returns the fourth point in the array of points defined in the super class Polygon.
		return new Point(this.getPoints()[3]);
	}	
	
	/**
	 * Compares between two objects. Two quadrangles are equal if and only if they are constructed from the same points.
	 * NOTE: we can skip the order of points check since we can assume both are valid quadrangles.
	 * 
	 * @param other The other object to test equality against.
	 * @return true or false according to the equality status between the objects.
	 */	

	public boolean equals(Object other) {
		if (other instanceof Quadrangle) {
			Point[] thisPoints = this.getPoints(); // an array of all the points in this quadrangle
			Point[] otherPoints = ((Quadrangle) other).getPoints(); // an array of all the points in the other quadrangle
			
			// we are checking out if every point in this quadrangle
			// has a matching point in the other quadrangle.
			// since we can assume a valid quadrangle has 4 points that exist,
			// different than each other and not on the same line,
			// this test is enough.
			boolean found = false; // determines whether we found a matching point
			for (int i = 0; i < thisPoints.length; ++i) {
				found = false;
				// find a matching point in this quadrangle within the other quadrangle
				for (int j = 0; j < otherPoints.length && !found; ++j) {
					if (thisPoints[i].equals(otherPoints[j])) {
						// we found a matching point; we can stop searching
						found = true;
					}
				}
				
				if (!found) {
					// found no matching point for point i in this quadrangle.
					// that means the quadrangles aren't equal.
					return false;
				}
			}
			
			// if we got here, every point forming our quadrangle has a match in the other quadrangle,
			// and therefore they are logically equal quadrangles.
			return true;
		}
		
		return false;
	}	
	
	public double getArea() {
		// divide the quadrangle into two triangles and sum their areas
		return (new Triangle(this.getP1(), this.getP2(), this.getP3()).getArea() 
				+ new Triangle(this.getP3(), this.getP4(), this.getP1()).getArea());
	}
	
	public boolean contains(Point p) {
		if (p == null) {
			return false;
		}		
		
		// divide the quadrangle into two triangles and search for the point in either of them		
		return (new Triangle(this.getP1(), this.getP2(), this.getP3()).contains(p) 
				|| new Triangle(this.getP3(), this.getP4(), this.getP1()).contains(p));
	}
}
