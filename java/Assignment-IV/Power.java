/**
 * Power is a class that represents the power action on a base expression and an exponent, 
 * and defines methods on that expression.
 */

/**
 * @author Dvir
 *
 */
public class Power implements Expression {
	private Expression base; // the base expression for the power action
	private double exponent; // the exponent for the power action
	
	/**
	 * Constructor that initiates our internal expressions.
	 * @param base The power base expression
	 * @param exponent The power exponent
	 * @throws RuntimeException if the base expression is null.
	 */		
	public Power(Expression base, double exponent) {
		if (base == null) {
			throw new RuntimeException("Power.Power() received a null base expression.");
		}			
		
		this.base = base;
		this.exponent = exponent;
	}
	
	/**
	 * @return The base expression
	 */
	public Expression getBase() {
		return this.base;
	}
	
	/**
	 * @return The power exponent
	 */
	public double getExponent() {
		return this.exponent;
	}
		
	public double evaluate(Assignments assignments) {
		// returns the power of the value represented by the base expression by the exponent.
		return Math.pow(this.base.evaluate(assignments), this.exponent);
	}

	
	public Expression derivative(Variable var) {
		if (var == null) {
			throw new RuntimeException("Power.derivative() received a null variable.");
		}		
		
		// if the exponent is 0, that means our number is a constant number, 
		// and in that case its derivative is the constant 0.
		if (this.exponent == 0) {
			return new Constant(0);
		}
		
		double newExponent = this.exponent - 1; // decrease the exponent.
		// returns a new expression according to the derivative formula - (f(x)^y)' = y*(f(x)^(y-1))		
		return new Multiplication(
					new Multiplication(
								new Constant(this.exponent), 
								new Power(this.base, newExponent)
					), 
					this.base.derivative(var)
				);
	}
	
	/**
	 * Compares between the current object with another object - this and other are equal if they are both instances of Power,
	 * and the base expression and their exponent are equal.
	 * @param other Object we are checking equality against.
	 * @return true or false according to the conditions. 
	 */	
	public boolean equals(Object other) {
		return (other instanceof Power && this.getBase().equals(((Power) other).getBase()) && this.getExponent() == ((Power) other).getExponent());
	}

	public String toString() {
		return "(" + this.base + "^" + this.exponent + ")";
	}
}
