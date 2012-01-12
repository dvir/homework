/**
 * Polynomial is a class that represents the polynomial expression on a variable
 * and a list of coefficients, and defines methods on that expression.
 */

/**
 * @author Dvir
 *
 */
public class Polynomial implements Expression {
	private Variable var; // the variable in the expression
	private double[] coefficients; // the coefficients list of the expression
	
	/**
	 * Constructor that initiates our internal expressions.
	 * @param var The expression variable
	 * @param coefficients The coefficients array
	 * @throws RuntimeException if the variable or coefficients are null.
	 */
	public Polynomial(Variable var, double[] coefficients) {
		if (var == null || coefficients == null) {
			throw new RuntimeException("Polynomial.Polynomial() received a null variable or a null coefficients array.");
		}
		
		this.var = var;
		
		// making a deep copy of the coefficients array
		this.coefficients = new double[coefficients.length];
		for (int i = 0; i < this.coefficients.length; ++i) {
			this.coefficients[i] = coefficients[i];
		}
	}
	
	/**	
	 * @return The coefficients array
	 */
	public double[] getCoefficients() {
		return this.coefficients;
	}
	
	public double evaluate(Assignments assignments) {
		if (assignments == null) {
			throw new RuntimeException("Polynomial.evaluate() received a null assignments array.");
		}			
		
		double sum = 0;
		
		// go over the coefficients array and sum the results of the polynomial equation
		// each index is the exponent of the current array index (0, 1, ...)
		for (int i = 0; i < coefficients.length; ++i) {
			sum = sum + (coefficients[i] * Math.pow(assignments.valueOf(var), i));
		}
		
		return sum;
	}

	public Expression derivative(Variable var) {
		if (var == null) {
			throw new RuntimeException("Polynomial.derivative() received a null variable.");
		}		
		
		// if our coefficients array only has one value, that means our number is a constant number, 
		// and in that case its derivative is the constant 0.
		// also, if we are deriving by a different var than what we have, the derivative is
		// the constant 0 as well.
		if (!this.var.equals(var) || this.coefficients.length == 1) {
			return new Constant(0);
		}
		
		// if we have more than one coefficient, we should derive it by the formula:
		// (1+x+x^2+x^3+...+x^n)' = 1+2x+3x^2+...+nx^(n-1) 
		if (this.coefficients.length > 1) {
			// we are removing the first coefficient because it will no longer be relevant
			// after deriving the polynomial expression.
			double[] newCoefficients = new double[this.coefficients.length-1];
			for (int i = 0; i < newCoefficients.length; ++i) {
				// the new coefficient is the exponent multiplied by the old coefficient.
				newCoefficients[i] = (i+1) * this.coefficients[i+1];
			}
			
			// returns the new polynomial expression with the new coefficients array we calculated
			return new Polynomial(this.var, newCoefficients);
		}
		
		// if we got here, we had no coefficients, 
		return new Polynomial(this.var, new double[0]);
	}
	
	
	/**
	 * Compares between the current object with another object - this and other are equal if they are both instances of Polynomial,
	 * their variable is the same and the coefficients are equal.
	 * @param other Object we are checking equality against.
	 * @return true or false according to the conditions. 
	 */		
	public boolean equals(Object other) {
		if (other instanceof Polynomial && this.var.equals(((Polynomial) other).var) && this.getCoefficients().length == ((Polynomial) other).getCoefficients().length) {
			// retrieve the coefficients of the other polynomial
			double[] otherCoefficients = ((Polynomial) other).getCoefficients();
			
			// compare between the two coefficients arrays
			for (int i = 0; i < this.coefficients.length; ++i) {
				if (this.coefficients[i] != otherCoefficients[i]) {
					// values in the coefficients array doesn't match; the Polynomials aren't equal.
					return false;
				}
			}
			
			// if we got here, all the coefficients are equal.
			return true;
		}
		
		// if we got here, the objects aren't equal.
		return false;
	}

	/**
	 * @return Returns the string representing the polynomial expression in the format (c0+c1*x+c2*x^2+...+cn*x^n)
	 */
	public String toString() {
		String result = "";
		for (int i = 0; i < coefficients.length; ++i) {
			if (i == 0) {
				result = result + coefficients[i];
			}
			else {
				if (coefficients[i] >= 0) {
					result = result + "+";
				}
				result = result + coefficients[i] + "*" + var.getName();
				if (i > 1) {
					result = result + "^" + i;
				}
			}
		}
		
		return "(" + result + ")";
	}
}
