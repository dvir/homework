/**
 * 
 */

/**
 * @author Dvir
 *
 */
public class Addition implements Expression {
	private Expression x, y;
	
	public Addition(Expression x, Expression y) {
		this.x = x;
		this.y = y;
	}
	
	public Expression getX() {
		return this.x;
	}
	
	public Expression getY() {
		return this.y;
	}
	
	/* (non-Javadoc)
	 * @see Expression#evaluate(Assignments)
	 */
	@Override
	public double evaluate(Assignments assignments) {
		return this.x.evaluate(assignments) + this.y.evaluate(assignments);
	}

	/* (non-Javadoc)
	 * @see Expression#derivative(Variable)
	 */
	@Override
	public Expression derivative(Variable var) {
		return new Addition(this.x.derivative(var), this.y.derivative(var));
	}
	
	
	public boolean equals(Object other) {
		return (other != null && (other instanceof Addition) && this.getX().equals(((Addition)other).getX()) && this.getY().equals(((Addition)other).getY()));
	}

	public String toString() {
		return "(" + this.x + "+" + this.y + ")";
	}
}
