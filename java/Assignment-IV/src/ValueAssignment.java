/**
 * 
 */

/**
 * @author Dvir
 *
 */
public class ValueAssignment implements Assignment {
	private Variable var;
	private double value;
	
	public ValueAssignment(Variable var, double value) {
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
	
	public boolean equals(ValueAssignment other) {
		if (!(other instanceof ValueAssignment)) {
			throw new RuntimeException("ValueAssignment.equals() received a non-ValueAssignment object.");
		}
		
		return (other != null && this.var == other.var && this.value == other.value);
	}
	
	public String toString() {
		return var.getName() + "=" + value;
	}
}
