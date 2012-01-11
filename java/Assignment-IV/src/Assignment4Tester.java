import java.util.Scanner;

/**
 * @version 1.8.3
 *  
 * @author Eran Samocha
 * thanks to Mark Bloch for helping
 */
public class Assignment4Tester {
	private static void testResult(String result) {
		if (result.indexOf("passed") == -1) {
			System.out.println(result);
		}
	}
	
	public static void main(String[] args) {
		if (args.length > 0 && args[0].equals("auto")) {
			final long time = System.currentTimeMillis();
			testResult("****************      performValidityChecks      ****************");
			performValidityChecks();
			testResult("*****************************************************************");
			testResult("****************      performOutputChecks      ******************");
			performOutputChecks();
			testResult("*****************************************************************");
			testResult("****************      performComparisonChecks      **************");
			performComparisonChecks();
			testResult("*****************************************************************");
			testResult("***************      performEvaluationChecks       **************");
			performEvaluationChecks();
			testResult("*****************************************************************");
			testResult("***************      performDerivativeChecks      ***************");
			performDerivativeChecks();
			testResult("*****************************************************************");
			testResult("Performed tests in: " + (System.currentTimeMillis() - time) + "ms");
			return;                       
		}
		Scanner scan = new Scanner(System.in);
		while (true) {
			try {
				testResult("Enter the required action:");
				testResult("1. Validity checks (null, correct behavior etc)");
				testResult("2. Output checks (toString).");
				testResult("3. Comparison checks (equals).");
				testResult("4. Evaluation checks.");
				testResult("5. Derivative checks.");
				testResult("6. Exit.");
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
		testResult("Testing VariablExpression..");
		VariableExpression x = new VariableExpression('x'), y = new VariableExpression('y');
		testResult("Test #1 .. " + (x.derivative(x).toString().equals("1.0") ? "passed!" : "*** FAILED ***"));
		testResult("Test #2 .. " + (x.derivative(y).toString().equals("0.0") ? "passed!" : "*** FAILED ***"));
		testResult("Testing Addition..");
		Expression exp = new Addition(x, y);
		testResult("Test #1 .. " + (exp.derivative(x).toString().equals("(1.0+0.0)") ? "passed!" : "*** FAILED ***"));
		testResult("Test #2 .. " + (exp.derivative(y).toString().equals("(0.0+1.0)") ? "passed!" : "*** FAILED ***"));
		testResult("Testing Subtraction..");
		exp = new Subtraction(x, y);
		testResult("Test #1 .. " + (exp.derivative(x).toString().equals("(1.0-0.0)") ? "passed!" : "*** FAILED ***"));
		testResult("Test #2 .. " + (exp.derivative(y).toString().equals("(0.0-1.0)") ? "passed!" : "*** FAILED ***"));
		testResult("Testing Multiplication..");
		exp = new Multiplication(x, y);
		testResult("Test #1 .. " + (exp.derivative(x).toString().equals("((x*0.0)+(1.0*y))") ? "passed!" : "*** FAILED ***"));
		testResult("Test #2 .. " + (exp.derivative(y).toString().equals("((x*1.0)+(0.0*y))") ? "passed!" : "*** FAILED ***"));
		testResult("Testing Power..");
		exp = new Power(x, 5);
		testResult("Test #1 .. " + (exp.derivative(x).toString().equals("((5.0*(x^4.0))*1.0)") ? "passed!" : "*** FAILED ***"));
		testResult("Test #2 .. " + (exp.derivative(y).toString().equals("((5.0*(x^4.0))*0.0)") ? "passed!" : "*** FAILED ***"));
		testResult("Testing Polynomial..");
		exp = new Polynomial(x, new double[] { 1, -2, 5, -1, 5 });
		testResult("Test #1 .. " + (exp.derivative(x).toString().equals("(-2.0+10.0*x-3.0*x^2+20.0*x^3)") ? "passed!" : "*** FAILED ***"));
		testResult("Test #2 .. " + (exp.derivative(y).toString().equals("0.0") ? "passed!" : "*** FAILED ***"));
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
		testResult( "Test #1 .. " + (String.valueOf(x.evaluate(assignments)).equals("1.0") ? "passed!" : "*** FAILED ***"));
		exp = new Addition(x, t);
		testResult( "Test #2 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("1.33333") ? "passed!" : "*** FAILED ***"));
		exp = new Addition(y, w);
		testResult( "Test #3 .. " + (String.valueOf(exp.evaluate(assignments)).equals("-7.8") ? "passed!" : "*** FAILED ***"));
		exp = new Subtraction(x, w);
		testResult( "Test #4 .. " + (String.valueOf(exp.evaluate(assignments)).equals("8.8") ? "passed!" : "*** FAILED ***"));
		exp = new Subtraction(t, z);
		testResult( "Test #5 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("-5.16666") ? "passed!" : "*** FAILED ***"));
		exp = new Multiplication(x, w);
		testResult( "Test #6 .. " + (String.valueOf(exp.evaluate(assignments)).equals("-7.8") ? "passed!" : "*** FAILED ***"));
		exp = new Multiplication(z, t);
		testResult( "Test #7 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("1.83333") ? "passed!" : "*** FAILED ***"));
		exp = new Power(w, 2.0);
		testResult( "Test #8 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("60.8399") ? "passed!" : "*** FAILED ***"));
		exp = new Power(z, t.evaluate(assignments));
		testResult( "Test #9 .. "  +(String.valueOf(exp.evaluate(assignments)).startsWith("1.76517") ? "passed!" : "*** FAILED ***"));
		exp = new Power(w, t.evaluate(assignments));
		testResult( "Test #10 .. " + (Double.isNaN(exp.evaluate(assignments)) ? "passed!" : "*** FAILED ***"));
		double[] coefficients = new double[] { 1, -5, 7, 8 , 0, -17 };
		exp = new Polynomial(w, coefficients);
		testResult( "Test #11 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("487489.106559") ? "passed!" : "*** FAILED ***"));
		exp = new Polynomial(t, coefficients);
		testResult( "Test #12 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("0.337448") ? "passed!" : "*** FAILED ***"));
		exp = new Polynomial(y, coefficients);
		testResult( "Test #13 .. " + (String.valueOf(exp.evaluate(assignments)).equals("1.0") ? "passed!" : "*** FAILED ***"));
		exp = new Power(new Polynomial(t, coefficients), t.evaluate(assignments));
		testResult( "Test #14 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("0.696202") ? "passed!" : "*** FAILED ***"));
		exp = new Power(new Addition(new Subtraction(x, w), new Multiplication(s, t)), t.evaluate(assignments));
		testResult( "Test #15 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("2.25499") ? "passed!" : "*** FAILED ***"));
		exp = new Addition(new Power(new Subtraction(new Addition(t, s), new Multiplication(z, w)), z.evaluate(assignments)), 
				new Polynomial(t, new double[] { 0, 0, 1, -1, 1}));
		testResult("Test #16 ..  " + (String.valueOf(exp.evaluate(assignments)).startsWith("2.526616") ? "passed!" : "*** FAILED ***"));
		exp = new Power(new Multiplication(new Addition(t, new Power(s, t.evaluate(assignments))), 
				new Polynomial(z, new double[] { 0, 0, 0, 0, 1, 1})), w.evaluate(assignments));
		testResult("Test #17 ..  " + (String.valueOf(exp.evaluate(assignments)).startsWith("4.89486") ? "passed!" : "*** FAILED ***"));
		exp = new Addition(new Power(new Multiplication(new Polynomial(t, new double[] { 0, 0, 0, 2, 5 }), new Addition(new Power(w, 2.0), new Power(s, 3.0))), 1.5), 
				new Multiplication(new Addition(new Power(s, 4.0), new Power(z, 5.0)), new Polynomial(y, new double[] { 0, 0, 0, 1, 1 })));
		testResult( "Test #18 .. " + (String.valueOf(exp.evaluate(assignments)).startsWith("686.138049") ? "passed!" : "*** FAILED ***"));
	}

	/**
	 * Checks if you return correct answers when comparing two objects
	 */
	private static void performComparisonChecks() {
		testResult("Testing VariableExpression..");
		testResult("Test #1 .. " + (new VariableExpression('x').equals(new VariableExpression('x')) ? "passed!" : "*** FAILED ***"));
		testResult("Test #2 .. " + (new VariableExpression('x').equals(new VariableExpression('y')) ? "*** FAILED ***" : "passed!"));
		testResult("Testing Addition..");
		Expression exp = new Addition(new VariableExpression('x'), new VariableExpression('y'));
		testResult("Test #1 .. " + (exp.equals(new Addition(new VariableExpression('x'), new VariableExpression('y'))) ? "passed!" : "*** FAILED ***"));
		testResult("Test #2 .. " + (exp.equals(new Addition(new VariableExpression('y'), new VariableExpression('x'))) ? "*** FAILED ***" : "passed!"));
		testResult("Testing Subtraction..");
		exp = new Subtraction(new VariableExpression('x'), new Constant(1));
		testResult("Test #1 .. " + (exp.equals(new Subtraction(new VariableExpression('x'), new Constant(1))) ? "passed!" : "*** FAILED ***"));
		testResult("Test #2 .. " + (exp.equals(new Subtraction(new Constant(1), new VariableExpression('x'))) ? "*** FAILED ***" : "passed!"));
		testResult("Testing Multiplication..");
		exp = new Multiplication(new VariableExpression('x'), new VariableExpression('y'));
		testResult("Test #1 .. " + (exp.equals(new Multiplication(new VariableExpression('x'), new VariableExpression('y'))) ? "passed!" : "*** FAILED ***"));
		testResult("Test #2 .. " + (exp.equals(new Multiplication(new VariableExpression('y'), new VariableExpression('x'))) ? "*** FAILED ***" : "passed!"));
		testResult("Testing Power..");
		exp = new Power(new VariableExpression('x'), 5);
		testResult("Test #1 .. " + (exp.equals(new Power(new VariableExpression('x'), 5)) ? "passed!" : "*** FAILED ***"));
		testResult("Test #2 .. " + (exp.equals(new Power(new VariableExpression('x'), 4)) ? "*** FAILED ***" : "passed!"));
		testResult("Test #3 .. " + (exp.equals(new Power(new VariableExpression('x'), 6)) ? "*** FAILED ***" : "passed!"));
		testResult("Testing Polynomial..");
		exp = new Polynomial(new VariableExpression('x'), new double[] { 0, 1, 0, -1, 5 });
		testResult("Test #1 .. " + (exp.equals(new Polynomial(new VariableExpression('x'), new double[] { 0, 1, 0, -1, 5 })) ? "passed!" : "*** FAILED ***"));
		testResult("Test #2 .. " + (exp.equals(new Polynomial(new VariableExpression('x'), new double[] { 0, 1, 0, -2, 5 })) ? "*** FAILED ***" : "passed!"));
		testResult("Test #3 .. " + (exp.equals(new Polynomial(new VariableExpression('y'), new double[] { 0, 1, 0, -1, 5 })) ? "*** FAILED ***" : "passed!"));
		testResult("Test #4 .. " + (exp.equals(new Polynomial(new VariableExpression('x'), new double[] { 0, 1, 0, -1, 5, 0 })) ? "*** FAILED ***" : "passed!"));
		testResult("Testing complex expressions..");
		exp = new Addition(new Power(new Subtraction(new Addition(new VariableExpression('t'), new VariableExpression('s')), 
				new Multiplication(new VariableExpression('x'), new VariableExpression('w'))), 5),
				new Polynomial(new VariableExpression('x'), new double[] { 0, 0, 1, -1, 1}));
		Expression exp2 = new Addition(new Power(new Subtraction(new Addition(new VariableExpression('t'), new VariableExpression('s')), 
				new Multiplication(new VariableExpression('x'), new VariableExpression('w'))), 5),
				new Polynomial(new VariableExpression('x'), new double[] { 0, 0, 1, -1, 1}));
		testResult("Test #1 .. " + (exp.equals(exp2) ? "passed!" : "*** FAILED ***"));
		exp2 = new Addition(new Power(new Subtraction(new Addition(new VariableExpression('a'), new VariableExpression('b')), 
				new Multiplication(new VariableExpression('x'), new VariableExpression('w'))), 5),
				new Polynomial(new VariableExpression('x'), new double[] { 0, 0, 1, -1, 1}));
		testResult("Test #2 .. " + (exp.equals(exp2) ? "*** FAILED ***" : "passed!"));
		exp2 = new Addition(new Power(new Subtraction(new Addition(new VariableExpression('t'), new VariableExpression('s')), 
				new Multiplication(new VariableExpression('x'), new VariableExpression('w'))), 3),
				new Polynomial(new VariableExpression('x'), new double[] { 1, 0, 1, -1, 1}));
		testResult("Test #3 .. " + (exp.equals(exp2) ? "*** FAILED ***" : "passed!"));
	}

	/**
	 * Check if your toString functions return the output they're supposed to
	 */
	private static void performOutputChecks() {
		testResult("Testing VariableExpression..");
		Expression exp = new VariableExpression('x');
		testResult("Test #1 .. " + (exp.toString().equals("x") ? "passed!" : "*** FAILED ***"));
		testResult("Testing ValueAssignment..");
		ValueAssignment valA = new ValueAssignment(new VariableExpression('x'), 5.0);
		testResult("Test #1 .. " + (valA.toString().equals("x=5.0") ? "passed!" : "*** FAILED ***"));
		testResult("Testing Addition..");
		exp = new Addition(new VariableExpression('x'), new Constant(7));
		testResult("Test #1 .. " + (exp.toString().equals("(x+7.0)") ? "passed!" : "*** FAILED ***"));
		testResult("Testing Subtraction..");
		exp = new Subtraction(new VariableExpression('z'), new VariableExpression('A'));
		testResult("Test #1 .. " + (exp.toString().equals("(z-A)") ? "passed!" : "*** FAILED ***"));
		testResult("Testing Mutliplication..");
		exp = new Multiplication(new VariableExpression('y'), new Constant(1.0 / 3));
		testResult("Test #1 .. " + (exp.toString().equals("(y*0.3333333333333333)") ? "passed!" : "*** FAILED ***"));
		testResult("Testing Power..");
		exp = new Power(new VariableExpression('y'), 0.5);
		testResult("Test #1 .. " + (exp.toString().equals("(y^0.5)") ? "passed!" : "*** FAILED ***"));
		testResult("Testing Polynomial..");
		exp = new Polynomial(new VariableExpression('x'), new double[] { 1, 1, 0, -7.5 });
		testResult("Test #1 .. " + (exp.toString().equals("(1.0+1.0*x+0.0*x^2-7.5*x^3)") ? "passed!" : "*** FAILED ***"));
		exp = new Polynomial(new VariableExpression('x'), new double[] { 4.4 });
		testResult("Test #2 .. " + (exp.toString().equals("(4.4)") ? "passed!" : "*** FAILED ***" ));
		exp = new Polynomial(new VariableExpression('x'), new double[] { });
		testResult("Test #3 .. " + ((exp.toString().equals("0.0") || exp.toString().equals("(0.0)") || exp.toString().isEmpty() || exp.toString().equals("()")) ? "passed!" : "*** FAILED ***"));
	}

	/**
	 * 1) Checks if you throw exceptions where you're supposed to. 
	 * 2) Checks if you added 'instanceof' checks where needed.
	 * 3) Checks if you handled special cases correctly.
	 */
	private static void performValidityChecks() {
		VariableExpression var = new VariableExpression('x');
		try {
			new Constant().derivative(null);
			testResult("******** PLEASE UPDATE Constant.java *********");
		} catch (Exception e) {
		}
		testResult("Testing VariableExpression..");
		try {
			var.derivative(null);
			testResult("Test #1 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #1 *** FAILED ***");
			else
				testResult("Test #1 passed!");
		}
		try {
			new VariableExpression('x').equals(new Constant());
			testResult("Test #2 passed!");
		} catch (Exception e) {
			testResult("Test #1 *** FAILED ***");
		}
		try {
			new VariableExpression('x').evaluate(null);
			testResult("Test #3 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #3 *** FAILED ***");
			else
				testResult("Test #3 passed!");
		}
		testResult("Testing Addition..");
		try {
			new Addition(null, var);
			testResult("Test #1 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #1 passed!");
		}
		try {
			new Addition(var, null);
			testResult("Test #2 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #2 passed!");
		}
		try {
			new Addition(null, null);
			testResult("Test #3 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #3 passed!");
		}
		try {
			new Addition(var, var).derivative(null);
			testResult("Test #4 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #4 *** FAILED ***");
			else
				testResult("Test #4 passed!");
		}
		try {
			new Addition(var, var).equals(new Constant());
			testResult("Test #5 passed!");
		} catch (Exception e) {
			testResult("Test #5 *** FAILED ***");
		}
		try {
			new Addition(var, var).evaluate(null);
			testResult("Test #6 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #6 *** FAILED ***");
			else
				testResult("Test #6 passed!");
		}
		testResult("Testing Subtraction..");
		try {
			new Subtraction(null, var);
			testResult("Test #1 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #1 passed!");
		}
		try {
			new Subtraction(var, null);
			testResult("Test #2 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #2 passed!");
		}
		try {
			new Subtraction(null, null);
			testResult("Test #3 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #3 passed!");
		}
		try {
			new Subtraction(var, var).derivative(null);
			testResult("Test #4 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #4 *** FAILED ***");
			else
				testResult("Test #4 passed!");
		}
		try {
			new Subtraction(var, var).equals(new Constant());
			testResult("Test #5 passed!");
		} catch (Exception e) {
			testResult("Test #5 *** FAILED ***");
		}
		try {
			new Subtraction(var, var).evaluate(null);
			testResult("Test #6 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #6 *** FAILED ***");
			else
				testResult("Test #6 passed!");
		}
		testResult("Testing Multiplication..");
		try {
			new Multiplication(null, var);
			testResult("Test #1 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #1 passed!");
		}
		try {
			new Multiplication(var, null);
			testResult("Test #2 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #2 passed!");
		}
		try {
			new Multiplication(null, null);
			testResult("Test #3 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #3 passed!");
		}
		try {
			new Multiplication(var, var).derivative(null);
			testResult("Test #4 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #4 *** FAILED ***");
			else
				testResult("Test #4 passed!");
		}
		try {
			new Multiplication(var, var).equals(new Constant());
			testResult("Test #5 passed!");
		} catch (Exception e) {
			testResult("Test #5 *** FAILED ***");
		}
		try {
			new Multiplication(var, var).evaluate(null);
			testResult("Test #6 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #6 *** FAILED ***");
			else
				testResult("Test #6 passed!");
		}
		testResult("Testing Power..");
		try {
			new Power(null, 0);
			testResult("Test #1 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #1 passed!");
		}
		try {
			new Power(var, 0).derivative(null);
			testResult("Test #2 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #4 *** FAILED ***");
			else
				testResult("Test #4 passed!");
		}
		try {
			new Power(var, 0).equals(new Constant());
			testResult("Test #3 passed!");
		} catch (Exception e) {
			testResult("Test #3 *** FAILED ***");
		}
		try {
			new Power(var, 0).evaluate(null);
			testResult("Test #4 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #4 *** FAILED ***");
			else
				testResult("Test #4 passed!");
		}
		if (new Power(var, 0).derivative(var).equals(new Constant()))
			testResult("Test #5 passed!");
		else
			testResult("Test #5 *** FAILED ***");
		testResult("Testing Polynomial..");
		try {
			new Polynomial(null, null);
			testResult("Test #1 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #1 passed!");
		}
		try {
			new Polynomial(var, null);
			testResult("Test #2 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #2 *** FAILED ***");
			else
				testResult("Test #2 passed!");
		}
		try {
			new Polynomial(null, new double[] { -5 });
			testResult("Test #3 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #3 passed!");
		}
		try {
			new Polynomial(var, new double[] { });
			testResult("Test #4 passed!");
		} catch (Exception e) {
			testResult("Test #4 *** FAILED ***");
		}
		try {
			new Polynomial(var, new double[] { 1 }).derivative(var);
			testResult("Test #5 passed!");
		} catch (Exception e) {
			testResult("Test #5 *** FAILED ***");
		}
		try {
			new Polynomial(var, new double[] { 1, 2 }).derivative(null);
			testResult("Test #6 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #6 *** FAILED ***");
			else
				testResult("Test #6 passed!");
		}
		try {
			new Polynomial(var, new double[] { 1, 2 }).equals(new Constant());
			testResult("Test #7 passed!");
		} catch (Exception e) {
			testResult("Test #7 *** FAILED ***");
		}
		double[] coeff = new double[] { 1 };
		Expression exp = new Polynomial(var, coeff);
		coeff[0] = -1;
		if (exp.equals(new Polynomial(var, coeff)))
			testResult("Test #8 *** FAILED ***");
		else
			testResult("Test #8 passed!");
		try {
			new Polynomial(var, new double[] { 1, 2 }).evaluate(null);
			testResult("Test #9 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #9 *** FAILED ***");
			else
				testResult("Test #9 passed!");
		}
		try {
			if (new Polynomial(var, new double[0]).derivative(var).equals(new Constant()) 
					|| new Polynomial(var, new double[0]).derivative(var).equals(new Polynomial(var, new double[0])))
				testResult("Test #10 passed!");
			else
				testResult("Test #10 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #10 *** FAILED ***");
		}
		try {
			new Polynomial(var, new double[0]).toString();
			testResult("Test #11 passed!");
		} catch (Exception e) {
			testResult("Test #11 *** FAILED ***");
		}
		testResult("Testing ValueAssignment..");
		try {
			new ValueAssignment(null, 0);
			testResult("Test #1 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #1 passed!");
		}
		try {
			new ValueAssignment(var, 2).equals(new Constant());
			testResult("Test #2 passed!");
		} catch (Exception e) {
			testResult("Test #2 *** FAILED ***");
		}
		testResult("Testing ArrayAssignments..");
		try {
			new ArrayAssignments(null);
			testResult("Test #1 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #1 passed!");
		}
		try {
			new ArrayAssignments(new Assignment[] { new ValueAssignment(new VariableExpression('x'), 1), null });
			testResult("Test #2 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #2 *** FAILED ***");
			else
				testResult("Test #2 passed!");
		}
		ArrayAssignments assignments = new ArrayAssignments(new Assignment[] { new ValueAssignment(new VariableExpression('x'), 1) });
		try {
			VariableExpression y = new VariableExpression('y');
			y.evaluate(assignments);
			testResult("Test #3 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #3 passed!");
		}
		assignments.addAssignment(new ValueAssignment(new VariableExpression('x'), 5));
		if (assignments.valueOf(new VariableExpression('x')) != 5.0)
			testResult("Test #4 *** FAILED ***");
		else
			testResult("Test #4 passed!");
		try {
			assignments.addAssignment(null);
			testResult("Test #5 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #5 *** FAILED ***");
			else
				testResult("Test #5 passed!");
		}
		try {
			assignments.valueOf(null);
			testResult("Test #6 *** FAILED ***");
		} catch (Exception e) {
			if (e instanceof NullPointerException)
				testResult("Test #6 *** FAILED ***");
			else
				testResult("Test #6 passed!");
		}
		Assignment y = new ValueAssignment(new VariableExpression('y'), -1);
		assignments.addAssignment(y);
		y.setValue(-2);
		if (assignments.valueOf(new VariableExpression('y')) == -2.0)
			testResult("Test #7 passed!");
		else
			testResult("Test #7 *** FAILED *** (shallow copy required!)");
		assignments = new ArrayAssignments(new Assignment[] { y });
		y.setValue(-1);
		if (assignments.valueOf(new VariableExpression('y')) == -1.0)
			testResult("Test #8 passed!");
		else
			testResult("Test #8 *** FAILED *** (shallow copy required!)");
		try {
			Assignment y2 = new ValueAssignment(y.getVar(), -2);
			assignments = new ArrayAssignments(new Assignment[] { y, y2 });
			if (assignments.valueOf(y.getVar()) == -2.0)
				testResult("Test #9 passed!");
			else
				testResult("Test #9 *** FAILED ***");
		}
		catch (Exception e) {
			testResult("Test #9 passed!");
		}
		try {
			new ArrayAssignments(new Assignment[0]);
			testResult("Test #10 *** FAILED ***");
		} catch (Exception e) {
			testResult("Test #10 passed!");
		}
	}
}