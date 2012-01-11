/**
 * 
 */

/**
 * @author Dvir
 *
 */
public class Power implements Expression {
	private Expression base;
	private double exponent;
	
	public Power(Expression base, double exponent) {
		if (base == null) {
			throw new RuntimeException("Power.Power() received a null base expression.");
		}			
		
		this.base = base;
		this.exponent = exponent;
	}
	
	public Expression getBase() {
		return this.base;
	}
	
	public double getExponent() {
		return this.exponent;
	}
		
	public double evaluate(Assignments assignments) {
		return Math.pow(this.base.evaluate(assignments), this.exponent);
	}

	public Expression derivative(Variable var) {
		if (var == null) {
			throw new RuntimeException("Power.derivative() received a null variable.");
		}		
		
		if (this.exponent == 0) {
			return new Constant(0);
		}
		
		double newExponent = this.exponent - 1;
		return new Multiplication(
					new Multiplication(
								new Constant(this.exponent), 
								new Power(this.base, newExponent)
					), 
					this.base.derivative(var)
				);
	}
	
	public boolean equals(Object other) {
		return (other instanceof Power && this.getBase().equals(((Power)other).getBase()) && this.getExponent() == ((Power)other).getExponent());
	}

	public String toString() {
		return "(" + this.base + "^" + this.exponent + ")";
	}
}
