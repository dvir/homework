/**
 * 
 */

/**
 * @author Dvir
 *
 */
public class VariableExpression implements Variable, Expression {
	private char varName;
	
	public VariableExpression(char name) {
		this.varName = name;
	}	
	
	public char getName() {
		return this.varName;
	}	
	
	public double evaluate(Assignments assignments) {
		return assignments.valueOf(this);
	}
	
	public Expression derivative(Variable var) {
		if (this.equals(var)) {
			return new Constant(1);
		}
		
		return new Constant(0);
	}
	
	
	public boolean equals(Object other) {
		return (other != null && (other instanceof VariableExpression) && ((VariableExpression)other).getName() == this.getName());
	}
	
	public String toString() {
		return "" + this.getName();
	}
}