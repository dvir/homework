/**
 * ValueAssignment is a class that defines methods on a variable and its value.
 */

/**
 * @author Dvir
 *
 */
public class ValueAssignment implements Assignment {
	private Variable var; // the variable we are assigning a value to
	private double value; // the value of the assignment
	
	/**
	 * Constructor that initiates our object with a non-null variable and a value.
	 * @param var The variable we are assigning a value to
	 * @param value The value of our assignment.
	 * @throws RuntimeException if we were given a null variable object.
	 */
	public ValueAssignment(Variable var, double value) {
		if (var == null) {
			throw new RuntimeException("ValueAssignment.ValueAssignment() received a null variable.");
		}				
		
		this.value = value;
		this.var = var;
	}
	
	public Variable getVar() {
		return var;
	}
	
	public double getValue() {
		return value;
	}
	
	public void setValue(double value) {
		this.value = value;
	}
	
	/**
	 * Compares between the current object with another object - this and other are equals if they are both instances of ValueAssignment,
	 * their variables are the same, and they have the same value. 
	 * @param other Object we are checking equality against.
	 * @return true or false according to the conditions. 
	 */	
	public boolean equals(Object other) {
		return (other instanceof ValueAssignment && this.var.equals(((ValueAssignment) other).getVar()) && this.value == ((ValueAssignment) other).value);
	}
	
	/**
	 * Returns the string that represents our value assignment - in this case, "var=value".
	 */
	public String toString() {
		return var.toString() + "=" + value;
	}
}
