/**
 * This is a graphical user interface tester. In this class a small graphical demo
 * will test your code.
 */
public class GraphicalTester {
	
	// ************* Fields *************
	
	private static ShapesContainer shapes;
	private static ShapesFrame frame;

	// ************* Methods *************
	
	/**
	 * This entry function will test the classes created in this assignment in a graphical manner.
	 * @param args command line arguments
	 */
	public static void main(String[] args) {
				
		shapes = new  ShapesContainer();
		frame = new ShapesFrame(shapes);
		
		addShapes();
		
		frame.setVisible(true);
	}

	/**
	 * Creating some shapes. Feel free to modify these shapes.
	 * You could for example, add a circle is the bonus part was implemented.
	 */
	private static void addShapes() {
		shapes.add(new Triangle(new Point(122.0, 145.0), new Point(133.0, 146.0), new Point(123.0, 63.0)));
		shapes.add(new Triangle(new Point(122.0, 144.0), new Point(129.0, 143.0), new Point(166.0, 71.0)));
		shapes.add(new Triangle(new Point(122.0, 144.0), new Point(131.0, 145.0), new Point(191.0, 91.0)));
		shapes.add(new Triangle(new Point(132.0, 143.0), new Point(124.0, 143.0), new Point(85.0, 79.0)));
		shapes.add(new Triangle(new Point(132.0, 143.0), new Point(123.0, 143.0), new Point(65.0, 101.0)));
		shapes.add(new Triangle(new Point(125.0, 142.0), new Point(99.0, 172.0), new Point(145.0, 174.0)));
		shapes.add(new Triangle(new Point(120.0, 169.0), new Point(98.0, 220.0), new Point(138.0, 222.0)));
		shapes.add(new Triangle(new Point(107.0, 218.0), new Point(98.0, 217.0), new Point(82.0, 274.0)));
		shapes.add(new Triangle(new Point(86.0, 262.0), new Point(83.0, 271.0), new Point(66.0, 258.0)));
		shapes.add(new Triangle(new Point(125.0, 218.0), new Point(135.0, 217.0), new Point(144.0, 271.0)));
		shapes.add(new Triangle(new Point(145.0, 271.0), new Point(141.0, 258.0), new Point(152.0, 257.0)));
		shapes.add(new Triangle(new Point(135.0, 171.0), new Point(142.0, 171.0), new Point(176.0, 202.0)));
		shapes.add(new Triangle(new Point(169.0, 194.0), new Point(173.0, 199.0), new Point(184.0, 174.0)));
		shapes.add(new Triangle(new Point(101.0, 167.0), new Point(106.0, 163.0), new Point(50.0, 144.0)));
		shapes.add(new Triangle(new Point(54.0, 146.0), new Point(65.0, 148.0), new Point(58.0, 130.0)));
		shapes.add(new Triangle(new Point(162.0, 269.0), new Point(188.0, 270.0), new Point(176.0, 215.0)));
		shapes.add(new Triangle(new Point(67.0, 268.0), new Point(47.0, 269.0), new Point(51.0, 207.0)));
		shapes.add(new Quadrangle(new Point(67.0, 268.0), new Point(54.0, 146.0), new Point(47.0, 269.0), new Point(51.0, 277.0)));
		shapes.add(new Quadrangle(new Point(17.0, 168.0), new Point(24.0, 246.0), new Point(68.4, 269.4), new Point(45.0, 207.0)));
		
		// Uncomment the next line if you have completed the BONUS part.
		shapes.add(new Circle(new Point(250, 40), 30));
		
		for (int i = 0; i < shapes.getShapesNum(); ++i) {
			System.out.println(i+". "+shapes.getShape(i).getArea()+ " ("+shapes.getShape(i).getClass()+")");
		}		
	}
}
