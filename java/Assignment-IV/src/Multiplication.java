/**
 * 
 */

/**
 * @author Dvir
 *
 */
public class Multiplication implements Expression {
	private Expression x, y;
	
	public Multiplication(Expression x, Expression y) {
		if (x == null || y == null) {
			throw new RuntimeException("Multiplication.Multiplication() received a null expression.");
		}				
		
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
		return this.x.evaluate(assignments) * this.y.evaluate(assignments);
	}

	/* (non-Javadoc)
	 * @see Expression#derivative(Variable)
	 */
	@Override
	public Expression derivative(Variable var) {
		if (var == null) {
			throw new RuntimeException("Multiplication.derivative() received a null variable.");
		}			
		
		return new Addition(
					new Multiplication(
							this.x, 
							this.y.derivative(var)
					),				
					new Multiplication(
							this.x.derivative(var), 
							this.y
					)
				);
	}
	
	public boolean equals(Object other) {
		return (other instanceof Multiplication && this.getX().equals(((Multiplication)other).getX()) && this.getY().equals(((Multiplication)other).getY()));	
	}

	public String toString() {
		return "(" + this.x + "*" + this.y + ")";
	}
}
