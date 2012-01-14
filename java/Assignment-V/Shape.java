/**
 * Shape defines the methods a general geometric shape should implement.
 */

/**
 * @author Dvir
 *
 */
public interface Shape {
	/**
	 * @return Returns the perimeter of a shape.
	 */
	public double getPerimeter();
	
	/**
	 * @return Returns the area of a shape.
	 */
	public double getArea();
	
	/**
	 * Moves a shape by another point, by moving every point the shape is constructed of by that point.
	 * @param p The point we are moving the shape by
	 */
	public void move(Point p);
	
	/**
	 * Determines whether a point is inside the shape or not.
	 * NOTE: Returns false if the point is on one of the edges of the shape.
	 * @param p The point we are searching for
	 * @return true if the point is in the shape, false otherwise.
	 */
	public boolean contains(Point p);
}
