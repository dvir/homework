/**
 * 
 */

/**
 * @author Dvir
 *
 */
public abstract class Polygon implements Shape {
	private Point[] points; // the points defining the shape
	
	public Polygon(Point[] points) {
		if (points == null) {
			throw new RuntimeException("Polygon.Polygon(Point[]) received a null points array.");
		}

		this.points = new Point[points.length];
		for (int i = 0; i < this.points.length; ++i) {
			if (points[i] == null) {
				throw new RuntimeException("Polygon.Polygon(Point[]) received a null point object in the points array.");
			}
			
			this.points[i] = points[i];
		}
	}
	
	public int getNumOfPoints() {
		return this.points.length;
	}
	
	public double[] getSides() {
		double[] sideLengths = new double[this.getNumOfPoints()-1];
		for (int i = 0; i < sideLengths.length; ++i) {
			sideLengths[i] = this.getPoints()[i].distance(this.getPoints()[i%this.getNumOfPoints()]);
		}
		
		return sideLengths;
	}
	
	public Point[] getPoints() {
		return this.points;
	}
	
	public abstract double getPerimeter();

	public abstract double getArea();

	public void move(Point p) {
		if (p == null) {
			throw new RuntimeException("Polygon.move(Point p) received a null point object.");
		}

		for (int i = 0; i < this.points.length; ++i) {
			this.points[i].move(p);
		}
	}

	public abstract boolean contains(Point p);
}
