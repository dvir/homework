/**
 * The abstract Polygon class which defines general methods on a shape which is a polygon.
 */

/**
 * @author Dvir
 *
 */
public abstract class Polygon implements Shape {
	private Point[] points; // the points defining the shape
	
	/**
	 * Default Polygon constructor. Performs a deep copy on the points given.
	 * @param points An array of Point objects.
	 * @throws RuntimeException if the points array or one of the objects in it are null.
	 */
	public Polygon(Point[] points) {
		if (points == null) {
			throw new RuntimeException("Polygon.Polygon(Point[]) received a null points array.");
		}

		// create a new array of points to fill and go over the given array and copy the points from it.
		this.points = new Point[points.length];
		for (int i = 0; i < this.points.length; ++i) {
			if (points[i] == null) {
				throw new RuntimeException("Polygon.Polygon(Point[]) received a null point object in the points array.");
			}
			
			// add a deep copy of the current point to the array.
			this.points[i] = new Point(points[i]);
		}
	}
	
	/**
	 * @return The amount of points in the internal Polygon points array.
	 */
	public int getNumOfPoints() {
		return this.points.length;
	}
	
	/**
	 * Calculates the lengths of the polygon sides.
	 * @return An array containing the length of each side of the polygon.
	 */
	public double[] getSides() {
		// calculate the length between every subsequence points
		// and reutrn it in as an array of doubles. 
		// (NOTE: a shape with n points has n-1 sides)
		double[] sideLengths = new double[this.getNumOfPoints()-1];
		for (int i = 0; i < sideLengths.length; ++i) {
			// calculate the distance between this point and the next one in the array.
			// we use the % operator to make sure the last point will calculate it's distance
			// according to the first point.
			sideLengths[i] = this.getPoints()[i].distance(this.getPoints()[i%this.getNumOfPoints()]);
		}
		
		return sideLengths;
	}
	
	/**
	 * @return An array of Point objects.
	 */
	public Point[] getPoints() {
		return this.points;
	}
	
	public abstract double getPerimeter();

	public abstract double getArea();

	public void move(Point p) {
		if (p == null) {
			throw new RuntimeException("Polygon.move(Point p) received a null point object.");
		}

		// go over the points array and move each point by the given point.
		for (int i = 0; i < this.points.length; ++i) {
			this.points[i].move(p);
		}
	}

	public abstract boolean contains(Point p);
}
