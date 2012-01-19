/**
 * Quadrangle defines the methods on a Quadrangle in a 2d world.
 */

/**
 * @author Dvir
 *
 */
public class Quadrangle extends Polygon {
	public Quadrangle(Point p1, Point p2, Point p3, Point p4) {
		super(new Point[]{p1, p2, p3, p4});
	}
	
	public Quadrangle(Quadrangle q) {
		super(q.getPoints()); // call the Polygon constructor to initialize the points array.
		/*
		if (t == null) {
			throw new RuntimeException("Quadrangle.Quadrangle(Quadrangle) received a null Quadrangle object.");
		}
		
		this.points = new Point[t.getNumOfPoints()];
		for (int i = 0; i < this.points.length; ++i) {
			this.points[i] = new Point(t.getPoints()[i]);
		}
		*/
	}
	
	public Point getP1() {
		return new Point(this.getPoints()[0]);
	}
	
	public Point getP2() {
		return new Point(this.getPoints()[1]);
	}
	
	public Point getP3() {
		return new Point(this.getPoints()[2]);
	}

	public Point getP4() {
		return new Point(this.getPoints()[3]);
	}	
	
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
			
	public double getPerimeter() {
		return this.getP1().distance(this.getP2()) 
				+ this.getP2().distance(this.getP3()) 
				+ this.getP3().distance(this.getP4()) 
				+ this.getP4().distance(this.getP1());
	}

	public double getArea() {
		return (new Triangle(this.getP1(), this.getP2(), this.getP3()).getArea() 
				+ new Triangle(this.getP3(), this.getP4(), this.getP1()).getArea());
	}
	
	public boolean contains(Point p) {
		if (p == null) {
			throw new RuntimeException("Quadrangle.contains(Point p) received a null point object.");
		}		
		
		return (new Triangle(this.getP1(), this.getP2(), this.getP3()).contains(p) 
				|| new Triangle(this.getP3(), this.getP4(), this.getP1()).contains(p));
	}
}
