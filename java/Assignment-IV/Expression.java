/**
 * 
 */

/**
 * @author Dvir
 *
 */
public interface Expression {
	double evaluate(Assignments assignments);
	
	Expression derivative(Variable var);
}
