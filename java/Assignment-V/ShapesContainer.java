/**
 * ShapesContainer contains a list of shapes and defines methods on that list, like adding, removing or several other actions.
 */

/**
 * @author Dvir
 *
 */
public class ShapesContainer {
	public static final int INIT_SIZE=10; // the initial size of the shapes array
	public static final int RESIZE=10; // the amount of slots to add to the shapes array when it fills up
	
	private Shape[] shapes; // the internal shapes array
	private int size; // the actual shapes amount inside the shapes array
	
	/**
	 * Default constructor. Initiate the internal shapes array with a size of INIT_SIZE and resets the shapes counter.
	 */
	public ShapesContainer() {
		this.shapes = new Shape[INIT_SIZE];
		this.size = 0;
	}
	
	/**
	 * Copy constructor. Creates a copy of another ShapesContainer object.
	 * @param other The shapes container to copy from.
	 * @throws RuntimeException if the given shapes container object is null.
	 */
	public ShapesContainer(ShapesContainer other) {
		if (other == null) {
			throw new RuntimeException("ShapesContainer.ShapesContainer(ShapesContainer) received a null shapes container.");
		}
		
		// reset the actual shapes amount counter
		this.size = 0;
		
		// create a new array of shapes to fill and go over the given set of shapes and copy the shapes from it.
		this.shapes = new Shape[other.getShapesNum()];
		for (int i = 0; i < other.getShapesNum(); ++i) {
			// copy the shape to our array
			this.add(other.getShape(i));
		}
	}
	
	/**
	 * @return The actual amount of shapes in the shapes container.
	 */
	public int getShapesNum() {
		return this.size;
	}
	
	/**
	 * Adds the given shape object into the shapes container.
	 * @param newShape The shape to add
	 * @return true if the action succeeded, false if the shape is already in the container.
	 * @throws RuntimeException if the given shape object is null.
	 */
	public boolean add(Shape newShape) {
		if (newShape == null) {
			// we can't add a null shape to the container
			return false;
		}
		
		// keeping track of where should we insert our shape.
		// our shapes container is sorted by area, from higher to lower,
		// so we are searching for the last index of a shape that has a larger
		// area than the new shape we want to insert.
		int higherAreaIndex = -1; 
		
		// search for newShape in the existing shapes array
		for (int i = 0; i < this.getShapesNum(); ++i) {
			if (this.getShape(i).getArea() > newShape.getArea()) {
				higherAreaIndex = i;
			}

			if (this.getShape(i) == newShape) {
				// the new shape is already in the array, so we shouldn't add it
				return false;
			}
		}
		
		// check if we have room for a new shape
		if (this.getShapesNum() == this.shapes.length) {
			// we are out of room. resize the array
			Shape[] newShapes = new Shape[this.shapes.length + RESIZE];
			for (int i = 0; i < this.getShapesNum(); ++i) {
				newShapes[i] = this.shapes[i];
			}
			
			// swap between the resized array to the old one
			this.shapes = newShapes;
		}
		
		// we are going to put the new shape in the slot we pre-calculated for it
		// when searching for it in the container.
		// we should place it right after the least higher area shape than the new one
		// which means we should move every shape from that index one slot down
		// NOTE: we are safe to assume we can move everything down since we made up some room in the container
		for (int i = this.getShapesNum()-1; i > higherAreaIndex; --i) {
			this.shapes[i+1] = this.shapes[i];
		}
		
		this.shapes[higherAreaIndex+1] = newShape; // put the new shape in its place
		this.size++; // increase the internal shapes array size indicator

		return true;
	}
	
	/**
	 * Removes a given shape object from the container.
	 * @param toRemove The shape to remove
	 * @return true if the action succeeded, false if the shape wasn't found in the container.
	 * @throws RuntimeException if the given shape object is null.
	 */
	public boolean remove(Shape toRemove) {
		if (toRemove == null) {
			// nothing to be removed
			return false;
		}	
		
		int shapeIndex = 0; // holds the shape index, so we could work on it if we found the shape
		boolean found = false; // holds whether we found the shape in the array
		// search for toRemove in the existing shapes array
		for (; shapeIndex < this.getShapesNum() && !found; ++shapeIndex) {
			if (this.getShape(shapeIndex) == toRemove) {
				// we found the shape we want to remove
				found = true;
				break;
			}
		}		
		
		if (!found) {
			// we couldn't find the shape to remove in the shapes array
			return false;
		}
		
		// we have the index of the shape we want to remove, pass it to the method
		// dealing with removing a shape by index.
		return this.remove(shapeIndex);
	}
	
	/**
	 * Removes the i'th shape in the shapes container.
	 * @param i The index of the shape to be removed.
	 * @return true if the action succeeded, false if not.
	 * @throws RuntimeException if the given index is out of the container bounds.
	 */
	public boolean remove(int i) {
		// make sure the given index is in the range of our array
		if (i < 0 || i >= this.shapes.length) {
			throw new RuntimeException("ShapesContainer.remove(int) received an out of bounds index. (" + i + ")");
		}
		
		// we are going to move every shape after the shape we want to remove 
		// one slot higher in the array
		for (; i < this.getShapesNum()-1; ++i) {
			this.shapes[i] = this.shapes[i+1];
		}
		
		// set the last element to null
		this.shapes[this.getShapesNum()-1] = null;
		
		// decrease the internal shapes array size indicator
		this.size--;
		
		return true;
	}
	
	/**
	 * Returns the i'th shape in the shapes container.
	 * @param i The shape index
	 * @return The shape in the i'th slot in the shapes container.
	 * @throws RuntimeException if the given index is out of the container bounds.
	 */
	public Shape getShape(int i) {
		// NOTE: this time we are making sure the index is in the actual
		// existing shapes group, because the array might have a few null slots
		// in the end of it.
		if (i < 0 || i >= this.getShapesNum()) {
			throw new RuntimeException("ShapesContainer.getShape(int) received an out of bounds index. (" + i + ")");
		}		
		
		return this.shapes[i];
	}
	
	/**
	 * @return The sum of the areas of every shape in the shapes container.
	 */
	public double sumArea() {
		double sum = 0; // the sum of all the areas of the shapes in the container
		
		// go over the shapes in the container and sum their areas
		for (int i = 0; i < this.getShapesNum(); ++i) {
			sum += this.getShape(i).getArea();
		}
		
		return sum;
	}
	
	/**
	 * @return The sum of the perimeters of every shape in the shapes container.
	 */
	public double sumPerimeter() {
		double sum = 0; // the sum of all the perimeters of the shapes in the container
		
		// go over the shapes in the container and sum their perimeters
		for (int i = 0; i < this.getShapesNum(); ++i) {
			sum += this.getShape(i).getPerimeter();
		}
		
		return sum;		
	}
	
	/**
	 * Move every shape by the given point.
	 * @param p The point to move the shapes by.
	 * @throws RuntimeException if the given point object is null.
	 */
	public void move(Point p) {
		if (p == null) {
			throw new RuntimeException("ShapesContainer.move(Point) received a null point object.");
		}
		
		// go over all the shapes in the container and move them by the given point
		for (int i = 0; i < this.getShapesNum(); ++i) {
			this.getShape(i).move(p);
		}		
	}
}
