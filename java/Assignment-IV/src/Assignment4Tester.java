import java.util.Scanner;

/**
 * 
 * @author Eran Samocha
 * thanks to Mark Bloch for helping
 */
public class Assignment4Tester {

	public static void main(String[] args) {
		if (args.length > 0 && args[0].equals("auto")) {
			System.out.println("****************      performValidityChecks      ****************");
			performValidityChecks();
			System.out.println("*****************************************************************");
			System.out.println("****************      performOutputChecks      ******************");
			performOutputChecks();
			System.out.println("*****************************************************************");
			System.out.println("****************      performComparisonChecks      **************");
			performComparisonChecks();
			System.out.println("*****************************************************************");
			System.out.println("***************      performEvaluationChecks       **************");
			performEvaluationChecks();
			System.out.println("*****************************************************************");
			System.out.println("***************      performDerivativeChecks      ***************");
			performDerivativeChecks();
			System.out.println("*****************************************************************");
			return;                       
		}
		Scanner scan = new Scanner(System.in);
		while (true) {
			try {
				System.out.println("Enter the required action:");
				System.out.println("1. Validity checks (null, correct behavior etc)");
				System.out.println("2. Output checks (toString).");
				System.out.println("3. Comparison checks (equals).");
				System.out.println("4. Evaluation checks.");
				System.out.println("5. Derivative checks.");
				System.out.println("6. Exit.");
				int req = Integer.parseInt(scan.next());
				switch (req) {
					default:
						return;
					case 1:
						performValidityChecks();
						break;
					case 2:
						performOutputChecks();
						break;
					case 3:
						performComparisonChecks();
						break;
					case 4:
						performEvaluationChecks();
						break;
					case 5:
						performDerivativeChecks();
						break;
				}
			} catch (NumberFormatException e) {
				System.err.println("Please enter a valid number!");
			}
		}
	}

	/**
	 * Checks if the expressions you return when calling .derivative() are what they should be
	 */
	private static void performDerivativeChecks() {
		System.out.println("Testing Constant..");
		System.out.println("Test #1 .. " + (new Constant(1).derivative(null).toString().equals("0.0") ? "passed!" : "failed!"));
		System.out.println("Testing VariablExpression..");
		VariableExpression x = new VariableExpression('x'), y = new VariableExpression('y');
		System.out.println("Test #1 .. " + (x.derivative(x).toString().equals("1.0") ? "passed!" : "failed!"));
		System.out.println("Test #2 .. " + (x.derivative(y).toString().equals("0.0") ? "passed!" : "failed!"));
		System.out.println("Testing Addition..");
		Expression exp = new Addition(x, y);
		System.out.println("Test #1 .. " + (exp.derivative(x).toString().equals("(1.0+0.0)") ? "passed!" : "failed!"));
		System.out.println("Test #2 .. " + (exp.derivative(y).toString().equals("(0.0+1.0)") ? "passed!" : "failed!"));
		System.out.println("Testing Subtraction..");
		exp = new Subtraction(x, y);
		System.out.println("Test #1 .. " + (exp.derivative(x).toString().equals("(1.0-0.0)") ? "passed!" : "failed!"));
		System.out.println("Test #2 .. " + (exp.derivative(y).toString().equals("(0.0-1.0)") ? "passed!" : "failed!"));
		System.out.println("Testing Multiplication..");
		exp = new Multiplication(x, y);
		System.out.println("Test #1 .. " + (exp.derivative(x).toString().equals("((x*0.0)+(1.0*y))") ? "passed!" : "failed!"));
		System.out.println("Test #2 .. " + (exp.derivative(y).toString().equals("((x*1.0)+(0.0*y))") ? "passed!" : "failed!"));
		System.out.println("Testing Power..");
		exp = new Power(x, 5);
		System.out.println("Test #1 .. " + (exp.derivative(x).toString().equals("((5.0*(x^4.0))*1.0)") ? "passed!" : "failed!"));
		System.out.println("Test #2 .. " + (exp.derivative(y).toString().equals("((5.0*(x^4.0))*0.0)") ? "passed!" : "failed!"));
		System.out.println("Testing Polynomial..");
		exp = new Polynomial(x, new double[] { 1, -2, 5, -1, 5 });
		System.out.println("Test #1 .. " + (exp.derivative(x).toString().equals("(-2.0+10.0*x-3.0*x^2+20.0*x^3)") ? "passed!" : "failed!"));
		System.out.println("Test #2 .. " + (exp.derivative(y).toString().equals("0.0") ? "passed!" : "failed!"));
	}

	/**
	 * Checks if the result of assigning values to variables and then evaluating is what it should be
	 */
	private static void performEvaluationChecks() {
		ArrayAssignments assignments = new ArrayAssignments();
		VariableExpression x = new VariableExpression('x'), y = new VariableExpression('y'), z = new VariableExpression('z');
		VariableExpression w = new VariableExpression('w'), t = new VariableExpression('t'), s = new VariableExpression('s');
		assignments.addAssignment(new ValueAssignment(x, 1));
		assignments.addAssignment(new ValueAssignment(y, 0));
		assignments.addAssignment(new ValueAssignment(z, 5.5));
		assignments.addAssignment(new ValueAssignment(w, -7.8));
		assignments.addAssignment(new ValueAssignment(t, 1.0/3.0));
		assignments.addAssignment(new ValueAssignment(s, 8));
		Expression exp;
		System.out.println( "Test #1 .. " + (String.valueOf(x.evaluate(assignments)).equals("1.0") ? "passed!" : "failed!"));
		exp = new Addition(x, t);
		System.out.println( "Test #2 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("1.33333") ? "passed!" : "failed!"));
		exp = new Addition(y, w);
		System.out.println( "Test #3 .. " + (String.valueOf(exp.evaluate(assignments)).equals("-7.8") ? "passed!" : "failed!"));
		exp = new Subtraction(x, w);
		System.out.println( "Test #4 .. " + (String.valueOf(exp.evaluate(assignments)).equals("8.8") ? "passed!" : "failed!"));
		exp = new Subtraction(t, z);
		System.out.println( "Test #5 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("-5.16666") ? "passed!" : "failed!"));
		exp = new Multiplication(x, w);
		System.out.println( "Test #6 .. " + (String.valueOf(exp.evaluate(assignments)).equals("-7.8") ? "passed!" : "failed!"));
		exp = new Multiplication(z, t);
		System.out.println( "Test #7 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("1.83333") ? "passed!" : "failed!"));
		exp = new Power(w, 2.0);
		System.out.println( "Test #8 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("60.8399") ? "passed!" : "failed!"));
		exp = new Power(z, t.evaluate(assignments));
		System.out.println( "Test #9 .. "  +(String.valueOf(exp.evaluate(assignments)).startsWith("1.76517") ? "passed!" : "failed!"));
		exp = new Power(w, t.evaluate(assignments));
		System.out.println( "Test #10 .. " + (Double.isNaN(exp.evaluate(assignments)) ? "passed!" : "failed!"));
		double[] coefficients = new double[] { 1, -5, 7, 8 , 0, -17 };
		exp = new Polynomial(w, coefficients);
		System.out.println( "Test #11 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("487489.106559") ? "passed!" : "failed!"));
		exp = new Polynomial(t, coefficients);
		System.out.println( "Test #12 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("0.337448") ? "passed!" : "failed!"));
		exp = new Polynomial(y, coefficients);
		System.out.println( "Test #13 .. " + (String.valueOf(exp.evaluate(assignments)).equals("1.0") ? "passed!" : "failed!"));
		exp = new Power(new Polynomial(t, coefficients), t.evaluate(assignments));
		System.out.println( "Test #14 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("0.696202") ? "passed!" : "failed!"));
		exp = new Power(new Addition(new Subtraction(x, w), new Multiplication(s, t)), t.evaluate(assignments));
		System.out.println( "Test #15 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("2.25499") ? "passed!" : "failed!"));
		exp = new Addition(new Power(new Subtraction(new Addition(t, s), new Multiplication(z, w)), z.evaluate(assignments)), 
				new Polynomial(t, new double[] { 0, 0, 1, -1, 1}));
		System.out.println("Test #16 ..  " + (String.valueOf(exp.evaluate(assignments)).startsWith("2.526616") ? "passed!" : "failed!"));
		exp = new Power(new Multiplication(new Addition(t, new Power(s, t.evaluate(assignments))), 
				new Polynomial(z, new double[] { 0, 0, 0, 0, 1, 1})), w.evaluate(assignments));
		System.out.println("Test #17 ..  " + (String.valueOf(exp.evaluate(assignments)).startsWith("4.89486") ? "passed!" : "failed!"));
		exp = new Addition(new Power(new Multiplication(new Polynomial(t, new double[] { 0, 0, 0, 2, 5 }), new Addition(new Power(w, 2.0), new Power(s, 3.0))), 1.5), 
				new Multiplication(new Addition(new Power(s, 4.0), new Power(z, 5.0)), new Polynomial(y, new double[] { 0, 0, 0, 1, 1 })));
		System.out.println( "Test #18 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("686.138049") ? "passed!" : "failed!"));
	}

	/**
	 * Checks if you return correct answers when comparing two objects
	 */
	private static void performComparisonChecks() {
		System.out.println("Testing Constant..");
		System.out.println("Test #1 .. " + (new Constant(1).equals(new Constant(1)) ? "passed!" : "failed!"));
		System.out.println("Test #2 .. " + (new Constant().equals(new Constant(1)) ? "failed!" : "passed!"));
		System.out.println("Testing VariableExpression..");
		VariableExpression x = new VariableExpression('x'), y = new VariableExpression('y');
		System.out.println("Test #1 .. " + (x.equals(new VariableExpression('x')) ? "passed!" : "failed!"));
		System.out.println("Test #2 .. " + (x.equals(new VariableExpression('y')) ? "failed!" : "passed!"));
		System.out.println("Testing Addition..");
		Expression exp = new Addition(x, y);
		System.out.println("Test #1 .. " + (exp.equals(new Addition(x, y)) ? "passed!" : "failed!"));
		System.out.println("Test #2 .. " + (exp.equals(new Addition(y, x)) ? "failed!" : "passed!"));
		System.out.println("Testing Subtraction..");
		exp = new Subtraction(x, new Constant(1));
		System.out.println("Test #1 .. " + (exp.equals(new Subtraction(x, new Constant(1))) ? "passed!" : "failed!"));
		System.out.println("Test #2 .. " + (exp.equals(new Subtraction(new Constant(1), x)) ? "failed!" : "passed!"));
		System.out.println("Testing Multiplication..");
		exp = new Multiplication(x, y);
		System.out.println("Test #1 .. " + (exp.equals(new Multiplication(x, y)) ? "passed!" : "failed!"));
		System.out.println("Test #2 .. " + (exp.equals(new Multiplication(y, x)) ? "failed!" : "passed!"));
		System.out.println("Testing Power..");
		exp = new Power(x, 5);
		System.out.println("Test #1 .. " + (exp.equals(new Power(x, 5)) ? "passed!" : "failed!"));
		System.out.println("Test #2 .. " + (exp.equals(new Power(y, 5)) ? "failed!" : "passed!"));
		System.out.println("Test #3 .. " + (exp.equals(new Power(x, 6)) ? "failed!" : "passed!"));
		System.out.println("Testing Polynomial..");
		exp = new Polynomial(x, new double[] { 0, 1, 0, -1, 5 });
		System.out.println("Test #1 .. " + (exp.equals(new Polynomial(x, new double[] { 0, 1, 0, -1, 5 })) ? "passed!" : "failed!"));
		System.out.println("Test #2 .. " + (exp.equals(new Polynomial(x, new double[] { 0, 1, 0, 0, -2, 5 })) ? "failed!" : "passed!"));
		System.out.println("Test #3 .. " + (exp.equals(new Polynomial(y, new double[] { 0, 1, 0, -2, 5 })) ? "failed!" : "passed!"));
		System.out.println("Testing complex expressions..");
		exp = new Addition(new Power(new Subtraction(new Addition(new VariableExpression('t'), new VariableExpression('s')), 
				new Multiplication(x, new VariableExpression('w'))), 5), new Polynomial(x, new double[] { 0, 0, 1, -1, 1}));
		Expression exp2 = new Addition(new Power(new Subtraction(new Addition(new VariableExpression('t'), new VariableExpression('s')), 
				new Multiplication(x, new VariableExpression('w'))), 5), new Polynomial(x, new double[] { 0, 0, 1, -1, 1}));
		System.out.println("Test #1 .. " + (exp.equals(exp2) ? "passed!" : "failed!"));
		exp2 = new Addition(new Power(new Subtraction(new Addition(new VariableExpression('a'), new VariableExpression('b')), 
				new Multiplication(x, new VariableExpression('w'))), 5), new Polynomial(x, new double[] { 0, 0, 1, -1, 1}));
		System.out.println("Test #2 .. " + (exp.equals(exp2) ? "failed!" : "passed!"));
		exp2 = new Addition(new Power(new Subtraction(new Addition(new VariableExpression('t'), new VariableExpression('s')), 
				new Multiplication(x, new VariableExpression('w'))), 3), new Polynomial(x, new double[] { 1, 0, 1, -1, 1}));
		System.out.println("Test #3 .. " + (exp.equals(exp2) ? "failed!" : "passed!"));
	}

	/**
	 * Check if your toString functions return the output they're supposed to
	 */
	private static void performOutputChecks() {
		System.out.println("Testing Constant..");
		Expression exp = new Constant(5);
		System.out.println("Test #1 .. " + (exp.toString().equals("5.0") ? "passed!" : "failed!"));
		System.out.println("Testing VariableExpression..");
		exp = new VariableExpression('x');
		System.out.println("Test #1 .. " + (exp.toString().equals("x") ? "passed!" : "failed!"));
		System.out.println("Testing ValueAssignment..");
		ValueAssignment valA = new ValueAssignment(new VariableExpression('x'), 5.0);
		System.out.println("Test #1 .. " + (valA.toString().equals("x=5.0") ? "passed!" : "failed!"));
		System.out.println("Testing Addition..");
		exp = new Addition(new VariableExpression('x'), new Constant(7));
		System.out.println("Test #1 .. " + (exp.toString().equals("(x+7.0)") ? "passed!" : "failed!"));
		System.out.println("Testing Subtraction..");
		exp = new Subtraction(new VariableExpression('z'), new VariableExpression('A'));
		System.out.println("Test #1 .. " + (exp.toString().equals("(z-A)") ? "passed!" : "failed!"));
		System.out.println("Testing Mutliplication..");
		exp = new Multiplication(new VariableExpression('y'), new Constant(1.0 / 3));
		System.out.println("Test #1 .. " + (exp.toString().equals("(y*0.3333333333333333)") ? "passed!" : "failed!"));
		System.out.println("Testing Power..");
		exp = new Power(new VariableExpression('y'), 0.5);
		System.out.println("Test #1 .. " + (exp.toString().equals("(y^0.5)") ? "passed!" : "failed!"));
		System.out.println("Testing Polynomial..");
		exp = new Polynomial(new VariableExpression('x'), new double[] { 1, 1, 0, -7.5 });
		System.out.println("Test #1 .. " + (exp.toString().equals("(1.0+1.0*x+0.0*x^2-7.5*x^3)") ? "passed!" : "failed!" + exp.toString()));
		//New test
		exp = new Polynomial(new VariableExpression('x'), new double[] { 4.4 });
		System.out.println("Test #2 .. " + (exp.toString().equals("(4.4)") ? "passed!" : "failed!" ));
	}

	/**
	 * 1) Checks if you throw exceptions where you're supposed to. 
	 * 2) Checks if you added 'instanceof' checks where needed.
	 * 3) Checks if you handled special cases correctly.
	 */
	private static void performValidityChecks() {
		System.out.println("This test checks if you throw RunTimeExceptions correctly");
		System.out.println("If the error message is different from what you wrote, you did something wrong.");
		VariableExpression var = new VariableExpression('x');
		System.out.println("Testing Constant..");
		try {
			new Constant().equals(new VariableExpression('x'));
			System.out.println("Test #1 passed!");
		} catch (Exception e) {
			System.out.println("Test #1 failed!");
		}
		System.out.println("Testing VariableExpression..");
		try {
			var.derivative(null);
			System.out.println("Test #1 failed!");
		} catch (Exception e) {
			System.out.println("Test #1 passed!");
		}
		try {
			new VariableExpression('x').equals(new Constant());
			System.out.println("Test #2 passed!");
		} catch (Exception e) {
			System.out.println("Test #2 failed!");
		}
		System.out.println("Testing Addition..");
		try {
			new Addition(null, var);
			System.out.println("Test #1 failed!");
		} catch (Exception e) {
			System.out.println("Test #1 passed!");
		}
		try {
			new Addition(var, null);
			System.out.println("Test #2 failed!");
		} catch (Exception e) {
			System.out.println("Test #2 passed!");
		}
		try {
			new Addition(null, null);
			System.out.println("Test #3 failed!");
		} catch (Exception e) {
			System.out.println("Test #3 passed!");
		}
		try {
			new Addition(var, var).derivative(null);
			System.out.println("Test #4 failed!");
		} catch (Exception e) {
			System.out.println("Test #4 passed!");
		}
		try {
			new Addition(var, var).equals(new Constant());
			System.out.println("Test #5 passed!");
		} catch (Exception e) {
			System.out.println("Test #5 failed!");
		}
		System.out.println("Testing Subtraction..");
		try {
			new Subtraction(null, var);
			System.out.println("Test #1 failed!");
		} catch (Exception e) {
			System.out.println("Test #1 passed!");
		}
		try {
			new Subtraction(var, null);
			System.out.println("Test #2 failed!");
		} catch (Exception e) {
			System.out.println("Test #2 passed!");
		}
		try {
			new Subtraction(null, null);
			System.out.println("Test #3 failed!");
		} catch (Exception e) {
			System.out.println("Test #3 passed!");
		}
		try {
			new Subtraction(var, var).derivative(null);
			System.out.println("Test #4 failed!");
		} catch (Exception e) {
			System.out.println("Test #4 passed!");
		}
		try {
			new Subtraction(var, var).equals(new Constant());
			System.out.println("Test #5 passed!");
		} catch (Exception e) {
			System.out.println("Test #5 failed!");
		}
		System.out.println("Testing Multiplication..");
		try {
			new Multiplication(null, var);
			System.out.println("Test #1 failed!");
		} catch (Exception e) {
			System.out.println("Test #1 passed!");
		}
		try {
			new Multiplication(var, null);
			System.out.println("Test #2 failed!");
		} catch (Exception e) {
			System.out.println("Test #2 passed!");
		}
		try {
			new Multiplication(null, null);
			System.out.println("Test #3 failed!");
		} catch (Exception e) {
			System.out.println("Test #3 passed!");
		}
		try {
			new Multiplication(var, var).derivative(null);
			System.out.println("Test #4 failed!");
		} catch (Exception e) {
			System.out.println("Test #4 passed!");
		}
		try {
			new Multiplication(var, var).equals(new Constant());
			System.out.println("Test #5 passed!");
		} catch (Exception e) {
			System.out.println("Test #5 failed!");
		}
		System.out.println("Testing Power..");
		try {
			new Power(null, 0);
			System.out.println("Test #1 failed!");
		} catch (Exception e) {
			System.out.println("Test #1 passed!");
		}
		try {
			new Power(var, 0).derivative(null);
			System.out.println("Test #2 failed!");
		} catch (Exception e) {
			System.out.println("Test #2 passed!");
		}
		try {
			new Power(var, 0).equals(new Constant());
			System.out.println("Test #3 passed!");
		} catch (Exception e) {
			System.out.println("Test #3 failed!");
		}
		System.out.println("Testing Polynomial..");
		try {
			new Polynomial(null, null);
			System.out.println("Test #1 failed!");
		} catch (Exception e) {
			System.out.println("Test #1 passed!");
		}
		try {
			new Polynomial(null, new double[] { });
			System.out.println("Test #2 failed!");
		} catch (Exception e) {
			System.out.println("Test #2 passed!");
		}
		try {
			new Polynomial(null, new double[] { -5 });
			System.out.println("Test #3 failed!");
		} catch (Exception e) {
			System.out.println("Test #3 passed!");
		}
		try {
			new Polynomial(var, new double[] { });
			System.out.println("Test #4 failed!");
		} catch (Exception e) {
			System.out.println("Test #4 passed!");
		}
		try {
			new Polynomial(var, new double[] { 1 }).derivative(var);
			System.out.println("Test #5 passed!");
		} catch (Exception e) {
			System.out.println("Test #5 failed!");
		}
		try {
			new Polynomial(var, new double[] { 1, 2 }).derivative(null);
			System.out.println("Test #6 failed!");
		} catch (Exception e) {
			System.out.println("Test #6 passed!");
		}
		try {
			new Polynomial(var, new double[] { 1, 2 }).equals(new Constant());
			System.out.println("Test #7 passed!");
		} catch (Exception e) {
			System.out.println("Test #7 failed!");
		}
		System.out.println("Testing ValueAssignment..");
		try {
			new ValueAssignment(null, 0);
			System.out.println("Test #1 failed!");
		} catch (Exception e) {
			System.out.println("Test #1 passed!");
		}
		try {
			new ValueAssignment(var, 2).equals(new Constant());
			System.out.println("Test #2 passed!");
		} catch (Exception e) {
			System.out.println("Test #2 failed!");
		}
		System.out.println("Testing ArrayAssignments..");
		try {
			new ArrayAssignments(null);
			System.out.println("Test #1 failed!");
		} catch (Exception e) {
			System.out.println("Test #1 passed!");
		}
		try {
			new ArrayAssignments(new Assignment[] { new ValueAssignment(new VariableExpression('x'), 1), null });
			System.out.println("Test #2 failed!");
		} catch (Exception e) {
			System.out.println("Test #2 passed!");
		}
		ArrayAssignments assignments = new ArrayAssignments(new Assignment[] { new ValueAssignment(new VariableExpression('x'), 1) });
		try {
			VariableExpression y = new VariableExpression('y');
			y.evaluate(assignments);
			System.out.println("Test #3 failed!");
		} catch (Exception e) {
			System.out.println("Test #3 passed!");
		}
		assignments.addAssignment(new ValueAssignment(new VariableExpression('x'), 5));
		if (assignments.valueOf(new VariableExpression('x')) != 5.0)
			System.out.println("Test #4 failed!");
		else
			System.out.println("Test #4 passed!");
		try {
			assignments.addAssignment(null);
			System.out.println("Test #5 failed!");
		} catch (Exception e) {
			System.out.println("Test #5 passed!");
		}
		try {
			assignments.valueOf(null);
			System.out.println("Test #6 failed!");
		} catch (Exception e) {
			System.out.println("Test #6 passed!");
		}
	}
}