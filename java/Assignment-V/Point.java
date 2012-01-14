/**
 * Point defines the methods on a point in a 2d world.
 */

/**
 * @author Dvir
 *
 */
public class Point {
	private double x, y; // the point coordinates
	
	public Point(double x, double y) {
		this.x = x;
		this.y = y;
	}
	
	public Point(Point other) {
		if (other == null) {
			throw new RuntimeException("Point.Point(Point other) received a null point.");
		}
		
		this.x = other.getX();
		this.y = other.getY();
	}
	
	public double getX() {
		return this.x;
	}
	
	public double getY() {
		return this.y;
	}
	
	public void move(int x, int y) {
		this.x += x;
		this.y += y;
	}
	
	public void move(Point p) {
		if (p == null) {
			throw new RuntimeException("Point.move(Point p) received a null point.");
		}
		
		this.x += p.getX();
		this.y += p.getY();
	}
	
	public boolean equals(Object other) {
		return (other instanceof Point && this.getX() == ((Point) other).getX() && this.getY() == ((Point) other).getY());
	}
	
	public double distance(Point p) {
		if (p == null) {
			throw new RuntimeException("Point.distance(Point p) received a null point.");
		}
			
		return Math.sqrt(Math.pow(this.getX() - p.getX(), 2) + Math.pow(this.getY() - p.getY(), 2));
	}
}
