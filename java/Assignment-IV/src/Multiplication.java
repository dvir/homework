/**
 * 
 */

/**
 * @author Dvir
 *
 */
public class Multiplication implements Expression {
	private Expression first, second;
	
	public Multiplication(Expression first, Expression second) {
		if (first == null || second == null) {
			throw new RuntimeException("Multiplication.Multiplication() received a null expression.");
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
		return this.first.evaluate(assignments) * this.second.evaluate(assignments);
	}

	public Expression derivative(Variable var) {
		if (var == null) {
			throw new RuntimeException("Multiplication.derivative() received a null variable.");
		}			
		
		return new Addition(
					new Multiplication(
							this.first, 
							this.second.derivative(var)
					),				
					new Multiplication(
							this.first.derivative(var), 
							this.second
					)
				);
	}
	
	public boolean equals(Object other) {
		return (other instanceof Multiplication && this.getFirst().equals(((Multiplication) other).getFirst()) && this.getSecond().equals(((Multiplication) other).getSecond()));	
	}

	public String toString() {
		return "(" + this.first + "*" + this.second + ")";
	}
}
