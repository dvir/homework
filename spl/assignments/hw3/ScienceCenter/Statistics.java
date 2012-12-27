/**
 * 
 */
package ScienceCenter;

/**
 * @author Dvir, Or
 *
 */
public class Statistics {
	private double budget = 0;
	
	public void reduceBudget(double amount) {
		budget -= amount;
	}
	
	public void increaseBudget(double amount) {
		budget += amount;
	}
}
