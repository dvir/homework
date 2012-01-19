/**
 * 
 */

/**
 * @author Dvir
 *
 */
public class Circle implements Shape {
	private Point center; // the circle's center point
	private double radius; // the circle's radius 
	
	public Circle(Point center, double radius) {
		if (center == null) {
			throw new RuntimeException("Circle.Circle(Point, double) received a null point object.");
		}
		
		if (radius < 0) {
			throw new RuntimeException("Circle.Circle(Point, double) received a negative radius value");
		}
		
		this.center = center;
		this.radius = radius;
	}
	
	public Circle(Circle c) {
		if (c == null) {
			throw new RuntimeException("Circle.Circle(Circle) received a null circle object.");
		}
		
		this.center = new Point(c.getCenter());
		this.radius = c.getRadius();
	}
	
	public Point getCenter() {
		return new Point(this.center);
	}
	
	public double getRadius() {
		return this.radius;
	}
	
	public boolean equals(Object other) {
		return (other instanceof Circle && this.getCenter().equals(((Circle) other).getCenter()) && this.getRadius() == ((Circle) other).getRadius());
	}
	
	public double getPerimeter() {
		return 2 * Math.PI * this.getRadius();
	}

	public double getArea() {
		return Math.PI * Math.pow(this.getRadius(), 2);
	}

	public void move(Point p) {
		if (p == null) {
			throw new RuntimeException("Circle.move(Point p) received a null point object.");
		}

		this.center.move(p);
	}

	public boolean contains(Point p) {
		if (p == null) {
			throw new RuntimeException("Circle.contains(Point p) received a null point object.");
		}
		
		// to find out if a point is inside a circle, we should check if the distance
		// between the point and the center of the circle is less than the radius.
		return (p.distance(this.getCenter()) < this.getRadius());
	}
}
