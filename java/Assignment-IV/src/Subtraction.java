/**
 * 
 */

/**
 * @author Dvir
 *
 */
public class Subtraction implements Expression {
	private Expression first, second; // the two expressions that form our subtraction expression.
	
	public Subtraction(Expression first, Expression second) {
		if (first == null || second == null) {
			throw new RuntimeException("Subtraction.Subtraction() received a null expression.");
		}			
		
		this.first = first;
		this.second = second;
	}
	
	public Expression getFirst() {
		return this.first;
	}
	
	public Expression getSecond() {
		return this.second;
	}	
	
	public double evaluate(Assignments assignments) {
		return this.first.evaluate(assignments) - this.second.evaluate(assignments);
	}
	
	public Expression derivative(Variable var) {
		if (var == null) {
			throw new RuntimeException("Subtraction.derivative() received a null variable.");
		}				
		
		return new Subtraction(this.first.derivative(var), this.second.derivative(var));
	}

	/**
	 * Compares between the current object with another object - this and other are equals if they are both instances of Subtraction,
	 * and the first and second expressions are equal.
	 * @param other Object we are checking equality against.
	 * @return true or false according to the conditions. 
	 */			
	public boolean equals(Object other) {
		return (other instanceof Subtraction && this.getFirst().equals(((Subtraction)other).getFirst()) && this.getSecond().equals(((Subtraction)other).getSecond()));		
	}

	public String toString() {
		return "(" + this.first + "-" + this.second + ")";
	}
}
