/**
 * Addition is a class that represents the addition action on two expressions, and defines methods on that expression.
 */

/**
 * @author Dvir
 *
 */
public class Addition implements Expression {
	private Expression first, second; // the two expressions that form our addition expression.
	
	/**
	 * Constructor that initiates our internal expressions.
	 * @param first First expression
	 * @param second Second expression
	 * @throws RuntimeException if any of the given expressions are null.
	 */
	public Addition(Expression first, Expression second) {
		if (first == null || second == null) {
			throw new RuntimeException("Addition.Addition() received a null expression.");
		}		
		
		this.first = first;
		this.second = second;
	}
	
	/**
	 * @return The first expression
	 */
	public Expression getFirst() {
		return this.first;
	}
	
	/**
	 * @return The second expression
	 */
	public Expression getSecond() {
		return this.second;
	}

	public double evaluate(Assignments assignments) {
		// evaluate each expression and return the value of their addition.		
		return this.first.evaluate(assignments) + this.second.evaluate(assignments);
	}

	public Expression derivative(Variable var) {
		if (var == null) {
			throw new RuntimeException("Addition.derivative() received a null variable.");
		}				

		// returns a new Addition expression according to the derivative formula - (f(x)+g(x))' = f'(x)+g'(x)
		return new Addition(this.first.derivative(var), this.second.derivative(var));
	}
	
	/**
	 * Compares between the current object with another object - this and other are equal if they are both instances of Addition,
	 * and the first and second expressions are equal.
	 * @param other Object we are checking equality against.
	 * @return true or false according to the conditions. 
	 */		
	public boolean equals(Object other) {
		return (other instanceof Addition && this.getFirst().equals(((Addition) other).getFirst()) && this.getSecond().equals(((Addition) other).getSecond()));
	}

	public String toString() {
		return "(" + this.first + "+" + this.second + ")";
	}
}
