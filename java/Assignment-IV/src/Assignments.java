/**
 * Assignments defines the methods on an array of assignments.
 */

/**
 * @author Dvir
 *
 */
public interface Assignments {
	/**
	 * Receives a variable object and searches the array of assignments for an assignment to the given variable.
	 * If we didn't find it in the array, or the given var is null, a runtime exception will be thrown. 
	 * @param var The variable we are searching for
	 * @return The value of the variable in the assignment we found for it.
	 */
	public double valueOf(Variable var);
	
	/**
	 * Receives an assignment object and searches the array of assignments for an assignment to the same variable.
	 * If found, we will change the old assignment value to the new one.
	 * Else, we will create a new assignments array and insert the assignment to it.
	 * @param assignment The new assignment to add.
	 */
	public void addAssignment(Assignment assignment);
}
