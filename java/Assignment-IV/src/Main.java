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
		
		String[][] testCases = new String[][]{
				{"Addition Class:", "", ""},
				{"evaluate", "3.1", "" + exp1.evaluate(assignments)},
				{"derivative", "(1.0+0.0)", "" + exp2},
				{"toString", "(x+y)", "" + exp3},
				{"toString", "(y+x)", "" + exp4},
				{"equals", "false", "" + exp3.equals(exp4)},
				
				{"Subtraction Class:", "", ""},
				{"evaluate", "-1.0", "" + exp5.evaluate(assignments)},
				{"derivative", "(0.0-1.0)", "" + exp52},
				{"toString", "(z-y)", "" + exp51},
				{"equals", "true", "" + exp51.equals(exp51)},
				
				{"Multiplication Class:", "", ""},
				{"evaluate", "2.2", "" + exp61.evaluate(assignments)},
				{"derivative", "(((x+y)*(0.0-0.0))+((1.0+0.0)*(y-z)))", "" + exp62},
				{"toString", "(x*y)", "" + exp61},
				{"toString", "((x+y)*(y-z))", "" + exp6},
				{"equals", "false", "" + exp6.equals(exp61)},
				
				{"Power Class:", "", ""},
				{"evaluate", "8.0", "" + exp71.evaluate(assignments)},
				{"derivative", "((3.0*(y^2.0))*1.0)", "" + exp72},
				{"toString", "(y^3.0)", "" + exp71},
				{"equals", "true", "" + exp71.equals(exp71)},
				
				{"Polynomial Class:", "", ""},
				{"evaluate", "24.0", "" + exp82.evaluate(assignments)},
				{"derivative", "(3.0+8.0*y)", "" + exp82.derivative(y)},
				{"toString", "(2.0+3.0*y+4.0*y^2)", "" + exp82},
				{"equals", "true", "" + exp8.equals(exp8)},
				
				{"General:", "", ""},
				{"toString", "(((x+y)*(y-z))^2.2)", "" + exp7},
				{"toString", "(1.0+3.0*x)", "" + exp8},
				{"toString", "(1.0+(3.0*x))", "" + exp81},
				{"equals", "false", "" + exp8.equals(exp81)}
		};
		
		boolean success = true;
		for (int i = 0; i < testCases.length; ++i) {
			if (testCases[i][1].equals("")) {
				System.out.println();
				System.out.println(testCases[i][0]);
			}
			else {
				System.out.print("* (" + testCases[i][0] + ") - ");
				if (testCases[i][1].equals(testCases[i][2])) {
					System.out.println("passed.");
				}
				else {
					success = false;
					System.out.println("failed. - Expected: " + testCases[i][1] + " | Actual: " + testCases[i][2]);
				}
			}
		}
		
		System.out.println();
		if (success) {
			System.out.println("All passed! woohoo!");
		}
	}
}
