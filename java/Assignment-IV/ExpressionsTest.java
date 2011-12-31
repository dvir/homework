public class ExpressionsTest {
	public static void main(String[] args) {
		VariableExpression x = new VariableExpression('x');
		VariableExpression y = new VariableExpression('y');
		VariableExpression z = new VariableExpression('z');

        Assignments assignments = new ArrayAssignments();
        assignments.addAssignment(new ValueAssignment(x, +1.0));
        assignments.addAssignment(new ValueAssignment(y, +2.5));
        assignments.addAssignment(new ValueAssignment(z, -3.4));

		Expression exp1 = new Addition(x, y); 
		Expression exp2 = new Subtraction(y, z);
		Expression exp3 = new Multiplication(exp1, exp2);
		Expression exp4 = new Power(exp3, 4.2);

		double[] coefficients = {-2, 4.1, -0.32};
		Expression exp5 = new Polynomial(x, coefficients);

		Expression exp6 = exp5.derivative(x);
		Expression exp7 = exp5.derivative(y);
		Expression exp8 = exp4.derivative(y);

		Expression[] expressions = {
			exp1, exp2, exp3, 
			exp4, exp5, exp6,
			exp7, exp8
		};

		System.out.println("expressions:");
		printExpressionArray(expressions, assignments);
		
		Expression exp9 = new Addition(x, y);
		Expression exp10 = new Addition(y, x);  
		Expression exp11 = new Subtraction(x, y);
		
		System.out.println("\nBasic equality check:");

		System.out.println("\nFirst expression:" + exp1);
		System.out.println("Second expression:" + exp9);
		System.out.println("Is first equal to second? " + exp1.equals(exp9));

		System.out.println("\nFirst expression:" + exp1);
		System.out.println("Second expression:" + exp10);
		System.out.println("Is first equal to second? " + exp1.equals(exp10));
	
		System.out.println("\nFirst expression:" + exp1);
		System.out.println("Second expression:" + exp11);
		System.out.println("Is first equal to second? " + exp1.equals(exp11));
	}
	
	/**
	 * Prints an array of expressions to the screen.
	 * 
	 * @param expressions an array of expressions to be printed.
	 */
	private static void printExpressionArray(Expression[] expressions, Assignments assignments) {
		for (int i=0; i<expressions.length; i = i+1){
			printEvaluatedExpression(expressions[i], assignments);
		}
	}

	/**
	 * Prints an expression along with its evaluated value.
	 *  
	 * @param exp an expression to be printed.
	 */
	private static void printEvaluatedExpression(Expression exp, Assignments assignments){
		System.out.println(exp + " = " + exp.evaluate(assignments));
	}
}
