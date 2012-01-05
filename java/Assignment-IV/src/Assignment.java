/**
 * Assignment defines the methods on a pair of a variable object and a value.
 */

/**
 * @author Dvir
 *
 */
public interface Assignment {
	/**
	 * Returns the variable object for this assignment.
	 * @return Variable var
	 */
	public Variable getVar();
	
	/**
	 * Returns the value of the assignment.
	 * @return double value
	 */
	public double getValue();
	
	/**
	 * Sets the value of the assignment.
	 * @param value New assignment value.
	 */
	public void setValue(double value);
}
