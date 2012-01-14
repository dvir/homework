/**
 * Triangle defines the methods on a triangle in a 2d world.
 */

/**
 * @author Dvir
 *
 */
public class Triangle extends Polygon {
	public Triangle(Point p1, Point p2, Point p3) {
		super(new Point[]{p1, p2, p3});
	}
	
	public Triangle(Triangle t) {
		super(t.getPoints()); // call the Polygon constructor to initialize the points array.
		/*
		if (t == null) {
			throw new RuntimeException("Triangle.Triangle(Triangle) received a null triangle object.");
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
	
	public boolean equals(Object o) {
		if (o instanceof Triangle) {
			Point[] thisPoints = this.getPoints();
			Point[] otherPoints = ((Triangle) o).getPoints();
			
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
			
	public double getPerimeter() {
		return this.getP1().distance(this.getP2()) 
				+ this.getP2().distance(this.getP3()) 
				+ this.getP3().distance(this.getP1());
	}

	public double getArea() {
		return (this.getP1().distance(this.getP2()) + this.getP1().distance(this.getP3())) / 2;
	}
	
	public boolean contains(Point p) {
		if (p == null) {
			throw new RuntimeException("Triangle.contains(Point p) received a null point object.");
		}		
		
		return (0.01 <= (new Triangle(p, this.getP1(), this.getP2()).getArea() 
								+ new Triangle(p, this.getP2(), this.getP3()).getArea() 
								+ new Triangle(p, this.getP3(), this.getP1()).getArea())
						- this.getArea());
	}
}
