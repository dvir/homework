import java.awt.Color;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

/**
 * The ShapesFrame class is responsible to display shapes in a frame.
 */
public class ShapesFrame extends Frame
{	
	// ************* Fields *************
	
	private static final long serialVersionUID = 1L;
	private ShapesContainer shapes;
	
	// ************* Constructors *************
	
	/**
	 * This Constructor will set of the frame's default behavior.
	 * @param shapes the shapes container object to display.
	 */
    public ShapesFrame(ShapesContainer shapes) {
    	// Notice: the shapes frame holds the same pointer!
    	// this means that Shapes instance is modified outside of this class
    	// then it will affect the shape frams's state.
    	this.shapes = shapes;
    	
        setTitle("Assignment 5 tester app");
        setSize(300, 300);
        addWindowListener(new WindowAdapter() {

            public void windowClosing(WindowEvent windowevent)
            {
                System.exit(0);
            }

        });
    }

	// ************* Public methods *************
    
    public void paint(Graphics g) {
    	// Displaying each of the shapes
        for(int i = 0; i < shapes.getShapesNum(); i++)
        {
            Shape shape = shapes.getShape(i);
            g.setColor(getKnownColor(i));
            
            if (shape instanceof Polygon) {
            	Polygon polygon = (Polygon) shape;
            	
            	Point[] points = polygon.getPoints();
            	int[] xPoints = new int[points.length];
            	int[] yPoints = new int[points.length];
            	
            	for (int j = 0; j < points.length; j++) {
					xPoints[j] = (int) points[j].getX();
					yPoints[j] = (int) points[j].getY();
				}
            	
            	g.fillPolygon(xPoints, yPoints, points.length);
            } 
            else if (shape instanceof Circle) {
            	Circle circle = (Circle) shape;
            	int xPoint = (int)circle.getCenter().getX();
            	int yPoint = (int)circle.getCenter().getY();
            	int radius = (int)circle.getRadius();
            	g.fillOval(xPoint, yPoint, radius, radius);
        	} else {
            	throw new UnsupportedOperationException("This type of shape is not supported graphically.");
            }
        }
    }
    
    /**
     * Converts an integer to a known color.
     * @param colorNum an integer between 0 and 13.
     * @return a known color object.
     */
    private Color getKnownColor(int colorNum) {
    	colorNum = colorNum % 13;
    	
    	switch (colorNum) {
		case 0:
			return Color.BLACK;
		case 1:
			return Color.BLUE;
		case 2:
			return Color.CYAN;
		case 3:
			return Color.DARK_GRAY;
		case 4:
			return Color.GRAY;
		case 5:
			return Color.GREEN;
		case 6:
			return Color.LIGHT_GRAY;
		case 7:
			return Color.MAGENTA;
		case 8:
			return Color.ORANGE;
		case 9:
			return Color.PINK;
		case 10:
			return Color.RED;
		case 11:
			return Color.WHITE;
		case 12:
			return Color.YELLOW;
		default:
			return Color.BLACK;
		}
    }
}
