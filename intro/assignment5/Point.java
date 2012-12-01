/**
 * Point defines the methods on a point in a 2d world.
 */

/**
 * @author Dvir
 *
 */
public class Point {
	private double x, y; // the point coordinates
	
	/**
	 * Constructor that receives actual x and y coordinates to create our new Point from.
	 * @param x The X coordinate
	 * @param y The Y coordinate
	 */
	public Point(double x, double y) {
		this.x = x;
		this.y = y;
	}
	
	/**
	 * Constructor that receives another Point object and performs a deep copy into the new object.
	 * @param other The point to copy
	 * @throws RuntimeException If the given Point object is null.
	 */
	public Point(Point other) {
		if (other == null) {
			throw new RuntimeException("Point.Point(Point other) received a null point.");
		}
		
		this.x = other.getX();
		this.y = other.getY();
	}
	
	/**
	 * @return Returns the X coordinate of the point.
	 */
	public double getX() {
		return this.x;
	}

	/**
	 * @return Returns the Y coordinate of the point.
	 */	
	public double getY() {
		return this.y;
	}
	
	/**
	 * Moves the point by X and Y coordinates.
	 * @param x The X coordinate to move the point by.
	 * @param y The Y coordinate to move the point by.
	 */
	public void move(int x, int y) {
		this.x += x;
		this.y += y;
	}
	
	/**
	 * Moves the point by adding another point coordinates with this one.
	 * @param p The point to move this point by.
	 * @throws RuntimeException If the given Point object is null. 
	 */
	public void move(Point p) {
		if (p == null) {
			throw new RuntimeException("Point.move(Point p) received a null point.");
		}
		
		this.x += p.getX();
		this.y += p.getY();
	}
	
	/**
	 * Compares between two Point objects. Two points are equal if they are both of type Point and their coordinates are matching.
	 * 
	 * @param other The object to test equality against.
	 * @return true or false according to the equality status between the objects.
	 */
	public boolean equals(Object other) {
		return (other instanceof Point && this.getX() == ((Point) other).getX() && this.getY() == ((Point) other).getY());
	}
	
	/**
	 * Calculates the distance between a point to another point.
	 * @param p The other point to calculate the distance from.
	 * @return The distance between this point to the given point.
	 * @throws RuntimeException If the given Point object is null.
	 */
	public double distance(Point p) {
		if (p == null) {
			throw new RuntimeException("Point.distance(Point p) received a null point.");
		}
			
		return Math.sqrt(Math.pow(this.getX() - p.getX(), 2) + Math.pow(this.getY() - p.getY(), 2));
	}
}
