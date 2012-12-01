/**
 * Circle defines the methods on a Circle in a 2d world.
 */

/**
 * @author Dvir
 *
 */
public class Circle implements Shape {
	private Point center; // the circle's center point
	private double radius; // the circle's radius 
	
	/**
	 * Constructor that creates a circle with a given center point and a radius length.
	 * @param center The center point object
	 * @param radius The radius length
	 * @throws RuntimeException if the center point object is null.
	 * @throws RuntimeException if the radius length given is negative.
	 */
	public Circle(Point center, double radius) {
		if (center == null) {
			throw new RuntimeException("Circle.Circle(Point, double) received a null point object.");
		}
		
		if (radius < 0) {
			throw new RuntimeException("Circle.Circle(Point, double) received a negative radius value");
		}
		
		this.center = new Point(center);
		this.radius = radius;
	}
	
	/**
	 * Copy constructor that creates a deep copy of another circle object.
	 * @param c The other circle
	 * @throws RuntimeException if the given circle object is null.
	 */
	public Circle(Circle c) {
		if (c == null) {
			throw new RuntimeException("Circle.Circle(Circle) received a null circle object.");
		}
		
		// create a deep copy of the center point
		this.center = new Point(c.getCenter());
		this.radius = c.getRadius();
	}
	
	/**
	 * @return The center point of a circle.
	 */	
	public Point getCenter() {
		return new Point(this.center);
	}
	
	/**
	 * @return The radius of the circle.
	 */
	public double getRadius() {
		return this.radius;
	}
	
	/**
	 * Compares between two objects. Two circles are equal if and only if they share the same center and have the same radius.
	 * 
	 * @param other The other object to test equality against.
	 * @return true or false according to the equality status between the objects.
	 */	
	public boolean equals(Object other) {
		return (other instanceof Circle && this.getCenter().equals(((Circle) other).getCenter()) && this.getRadius() == ((Circle) other).getRadius());
	}
	
	public double getPerimeter() {
		// returns the perimeter of the circle according to the formula:
		// 2*PI*radius
		return 2 * Math.PI * this.getRadius();
	}

	public double getArea() {
		// returns the area of the circle according to the formula:
		// PI*radius^2
		return Math.PI * Math.pow(this.getRadius(), 2);
	}

	public void move(Point p) {
		if (p == null) {
			throw new RuntimeException("Circle.move(Point p) received a null point object.");
		}

		// move the center point of the circle by point p.
		this.center.move(p);
	}

	public boolean contains(Point p) {
		if (p == null) {
			return false;
		}
		
		// to find out if a point is inside a circle, we should check if the distance
		// between the point and the center of the circle is less than the radius.
		return (p.distance(this.getCenter()) < this.getRadius());
	}
}
