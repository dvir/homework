public class Main {
	public static void main(String[] args) {
		VariableExpression x = new VariableExpression('x');
		VariableExpression y = new VariableExpression('y');
		VariableExpression z = new VariableExpression('z');
		
		Assignments assignments = new ArrayAssignments();
		assignments.addAssignment(new ValueAssignment(x, +1.1));
		assignments.addAssignment(new ValueAssignment(y, +2.0));
		assignments.addAssignment(new ValueAssignment(z, +3.0));

		Expression exp1 = new Addition(x, y);
		Expression exp2 = exp1.derivative(x);
		Expression exp3 = new Addition(x, y);
		Expression exp4 = new Addition(y, x);	
		Expression exp5 = new Subtraction(y, z);
		Expression exp51 = new Subtraction(z, y);
		Expression exp52 = exp5.derivative(z);
		Expression exp6 = new Multiplication(exp1, exp5);
		Expression exp61 = new Multiplication(x, y);
		Expression exp62 = exp6.derivative(x);
		Expression exp7 = new Power(exp6, 2.2);
		Expression exp71 = new Power(y, 3.0);
		Expression exp72 = exp71.derivative(y);

		double[] coefficients = {1.0, 3.0};
		double[] coefficients2 = {2.0, 3.0, 4.0};
		Expression cons1 = new Constant(1.0);
		Expression cons2 = new Constant(3.0);
		Expression exp8 = new Polynomial(x, coefficients);
		Expression exp81 = new Addition(cons1, new Multiplication(cons2, x));
		Expression exp82 = new Polynomial(y, coefficients2);
				
		
		System.out.println("Testing Addition:");
		System.out.println("* (evaluate) Expected: 3.1 | Actual: " + exp1.evaluate(assignments));
		System.out.println("* (derivative) Expected: (1.0+0.0) | Actual: " + exp2);
		System.out.println("* (toString, toString, equals) Expected: (x+y),(y+x),false | Actual: " + exp3 + "," + exp4 + "," + exp3.equals(exp4));

		System.out.println();
		System.out.println("Testing Subtraction:");
		System.out.println("* (evaluate) Expected: -1.0 | Actual: " + exp5.evaluate(assignments));
		System.out.println("* (derivative) Expected: (0.0-1.0) | Actual: " + exp52);
		System.out.println("* (toString, equals) Expected: (z-y),true | Actual: " + exp51 + "," + exp51.equals(exp51));
		
		System.out.println();
		System.out.println("Testing Multiplication:");
		System.out.println("* (evaluate) Expected: 2.2 | Actual: " + exp61.evaluate(assignments));
		System.out.println("* (derivative) Expected: ((1.0+0.0)*(0.0+0.0)) | Actual: " + exp62);
		System.out.println("* (toString, equals) Expected: (x*y),((x+y)*(y-z)),false | Actual: " + exp61 + "," + exp6 + "," + exp6.equals(exp61));
		
		System.out.println();
		System.out.println("Testing Power:");
		System.out.println("* (evaluate) Expected: 8.0 | Actual: " + exp71.evaluate(assignments));
		System.out.println("* (derivative) Expected: (3*(y^2.0)) | Actual: " + exp72);
		System.out.println("* (toString, equals) Expected: (y^3.0),true | Actual: " + exp71 + "," + exp71.equals(exp71));

		System.out.println();
		System.out.println("Testing Polynomial:");
		System.out.println("* (evaluate) Expected: 10.0 | Actual: " + exp82.evaluate(assignments));
		System.out.println("* (derivative) Expected: (3.0+8.0*y) | Actual: " + exp82.derivative(y));
		System.out.println("* (toString, equals) Expected: (2+3.0*y+4.0*y^2),true | Actual: " + exp82 + "," + exp8.equals(exp8));
		
		System.out.println();
		System.out.println("General tests:");
		System.out.println("* (toString of Addition, Subtraction, Multiplication, Power) Expected: (((x+y)*(y-z))^2.2) | Actual: " + exp7);
		System.out.println(" * Expected: (1+3.0*x),(1.0+(3.0*x)),false | Actual: " + exp8 + "," + exp81 + "," + exp8.equals(exp81));	
	}
}
