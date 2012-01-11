/**
 * VariableExpression is a class that represents a mathematical variable and defines methods on it.
 */

/**
 * @author Dvir
 *
 */
public class VariableExpression implements Variable, Expression {
	private char varName;
	
	/**
	 * Constructor that receives a character that will symbolize the new variable.
	 * @param name The character that will represent the variable
	 */
	public VariableExpression(char name) {
		this.varName = name;
	}	
	
	public char getName() {
		return this.varName;
	}	
	
	public double evaluate(Assignments assignments) {
		if (assignments == null) {
			throw new RuntimeException("VariableExpression.evaluate() received a null assignments array.");
		}		
		
		// returns the value of the current variable according to the assignments list.
		return assignments.valueOf(this);
	}
	
	public Expression derivative(Variable var) {
		if (var == null) {
			throw new RuntimeException("VariableExpression.derivative() received a null variable.");
		}
		
		// if we are deriving by our variable, the derivative is the constant 1.
		if (this.equals(var)) {
			return new Constant(1);
		}
		
		// else, the derivative is the constant 0 for any other variable.
		return new Constant(0);
	}
	
	/**
	 * Compares between the current object with another object - this and other are equals if they are both instances of VariableExpression
	 * and their variables symbol are the same.
	 * @param other Object we are checking equality against.
	 * @return true or false according to the conditions.
	 */
	public boolean equals(Object other) {
		return (other instanceof VariableExpression && ((VariableExpression)other).getName() == this.getName());
	}
	
	/**
	 * Returns the string that represents the variable - its symbol.
	 */
	public String toString() {
		return "" + this.getName();
	}
}