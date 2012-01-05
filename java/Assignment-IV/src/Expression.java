/**
 * Expression defines the methods on a mathematical expression.
 */

/**
 * @author Dvir
 *
 */
public interface Expression {
	/**
	 * Receives an array of assignments for variables and evaluates the mathematical expression according to these values.
	 * @param assignments The assignments for the variables in the expression.
	 * @return The value of the expression depending on the variable assignments.
	 */
	public double evaluate(Assignments assignments);
	
	/**
	 * Receives a variable object and creates the mathematical expression corresponding to its derivative.
	 * @param var The variable object we will differentiate the expression by.
	 * @return The derivative mathematical expression of the expression.
	 */
	public Expression derivative(Variable var);
}
