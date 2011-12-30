/**
 * 
 */

/**
 * @author Dvir
 *
 */
public class Polynomial implements Expression {
	private Variable var;
	private double[] coefficients;
	
	public Polynomial(Variable var, double[] coefficients) {
		if (coefficients == null) {
			coefficients = new double[0];
		}
		
		this.var = var;
		this.coefficients = coefficients;
	}
	
	public getCoefficients() {
		return this.coefficients;
	}
	
	/* (non-Javadoc)
	 * @see Expression#evaluate(Assignments)
	 */
	@Override
	public double evaluate(Assignments assignments) {
		double sum = 0;
		
		// go over the coefficients array and sum the results of the polynomial equation
		// each index is the exponent of the current array index (0, 1, ..)
		for (int i = 0; i < coefficients.length; ++i) {
			sum = sum + (coefficients[i] * Math.pow(assignments.valueOf(var), i));
		}
		
		return sum;
	}

	/* (non-Javadoc)
	 * @see Expression#derivative(Variable)
	 */
	@Override
	public Expression derivative(Variable var) {
		if (this.coefficients.length > 1) {
			double[] newCoefficients = new double[this.coefficients.length-1];
			for (int i = 0; i < newCoefficients.length; ++i) {
				newCoefficients[i] = (i+1) * this.coefficients[i+1];
			}
			
			return new Polynomial(this.var, newCoefficients);
		}
		
		// if we got here, we had no coefficients, 
		// or we had only one, which means we had a constant number and therefore we have none now.
		return new Polynomial(this.var, new double[0]);
	}
	
	public boolean equals(Expression other) {
		if (other != null && (other instanceof Polynomial) && this.var.equals(other.var) && this.getCoefficients().length == other.getCoefficients().length) {
			double[] otherCoefficients = other.getCoefficients();
			for (int i = 0; i < this.coefficients.length; ++i) {
				if (this.coefficients[i] != otherCoefficients[i]) {
					// values mis-match; the Polynomials aren't equal.
					return false;
				}
			}
			
			// if we got here, all the coefficients are equal.
			return true;
		}
		
		return false;
	}

	public String toString() {
		String result = "";
		for (int i = 0; i < coefficients.length; ++i) {
			if (i == 0) {
				result = result + coefficients[i];
			}
			else {
				if (coefficients[i] > 0) {
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
