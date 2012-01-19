/**
 * 
 */

/**
 * @author Dvir
 *
 */
public class ShapesContainer {
	public static final int INIT_SIZE=10;
	public static final int RESIZE=10;
	
	private Shape[] shapes;
	private int size;
	
	public ShapesContainer() {
		this.shapes = new Shape[INIT_SIZE];
		this.size = 0;
	}
	
	public ShapesContainer(ShapesContainer other) {
		if (other == null) {
			throw new RuntimeException("ShapesContainer.ShapesContainer(ShapesContainer) received a null shapes container.");
		}
		
		this.size = 0;
		this.shapes = new Shape[other.getShapesNum()];
		for (int i = 0; i < other.getShapesNum(); ++i) {
			if (other.getShape(i) == null) {
				throw new RuntimeException("ShapesContainer.ShapesContainer(ShapesContainer) received a null shape object.");
			}
			
			this.add(other.getShape(i));
		}
	}
	
	public int getShapesNum() {
		return this.size;
	}
	
	public boolean add(Shape newShape) {
		if (newShape == null) {
			throw new RuntimeException("ShapesContainer.add(Shape) received a null shape object.");
		}
		
		// keeping track of where should we insert our shape.
		// our shapes container is sorted by perimeter, from higher to lower,
		// so we are searching for the last index of a shape that has a larger
		// perimeter than the new shape we want to insert.
		int higherPerimeterIndex = -1; 
		
		// search for newShape in the existing shapes array
		for (int i = 0; i < this.getShapesNum(); ++i) {
			if (this.getShape(i).getPerimeter() > newShape.getPerimeter()) {
				higherPerimeterIndex = i;
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
			for (int i = 0; i < this.shapes.length; ++i) {
				newShapes[i] = this.shapes[i];
			}
			
			// swap between the resized array to the old one
			this.shapes = newShapes;
		}
		
		// we are going to put the new shape in the slot we pre-calculated for it
		// when searching for it in the container.
		// we should place it right after the least higher perimeter shape than the new one
		// which means we should move every shape from that index one slot down
		// NOTE: we are safe to assume we can move everything down since we made up some room in the container
		for (int i = this.getShapesNum()-1; i > higherPerimeterIndex; --i) {
			this.shapes[i+1] = this.shapes[i];
		}
		
		this.shapes[higherPerimeterIndex+1] = newShape; // put the new shape in its place
		this.size++; // increase the internal shapes array size indicator
		
		return true;
	}
	
	public boolean remove(Shape toRemove) {
		if (toRemove == null) {
			throw new RuntimeException("ShapesContainer.remove(Shape) received a null shape object.");
		}	
		
		int shapeIndex = 0; // holds the shape index, so we could work on it if we found the shape
		boolean found = false; // holds whether we found the shape in the array
		// search for toRemove in the existing shapes array
		for (;shapeIndex < this.getShapesNum() && !found; ++shapeIndex) {
			if (this.getShape(shapeIndex).equals(toRemove)) {
				// we found the shape we want to remove
				found = true;
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
	
	public boolean remove(int i) {
		// make sure the given index is in the range of our array
		if (i < 0 || i >= this.shapes.length) {
			throw new RuntimeException("ShapesContainer.remove(int) received an out of bounds index. (" + i + ")");
		}
		
		// we are going to move every shape after the shape we want to remove 
		// one slot higher in the array
		for (; i < this.shapes.length-2; ++i) {
			this.shapes[i] = this.shapes[i+1];
		}
		
		// set the last element to null
		this.shapes[this.shapes.length-1] = null;
		
		// decrease the internal shapes array size indicator
		this.size--;
		
		return true;
	}
	
	public Shape getShape(int i) {
		// NOTE: this time we are making sure the index is in the actual
		// existing shapes group, because the array might have a few null slots
		// in the end of it.
		if (i < 0 || i >= this.getShapesNum()) {
			throw new RuntimeException("ShapesContainer.getShape(int) received an out of bounds index. (" + i + ")");
		}		
		
		return this.shapes[i];
	}
	
	public double sumArea() {
		double sum = 0;
		for (int i = 0; i < this.getShapesNum(); ++i) {
			sum += this.getShape(i).getArea();
		}
		
		return sum;
	}
	
	public double sumPerimeter() {
		double sum = 0;
		for (int i = 0; i < this.getShapesNum(); ++i) {
			sum += this.getShape(i).getPerimeter();
		}
		
		return sum;		
	}
	
	public void move(Point p) {
		if (p == null) {
			throw new RuntimeException("ShapesContainer.move(Point) received a null point object.");
		}
		
		for (int i = 0; i < this.getShapesNum(); ++i) {
			this.getShape(i).move(p);
		}		
	}
}
