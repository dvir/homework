/**
 * ArrayAssignments is an object that holds an array of assignments and defines methods on that array.
 */

/**
 * @author Dvir
 *
 */
public class ArrayAssignments implements Assignments {
	private Assignment[] assignments;
	
	/**
	 * Default constructor. Create an empty array of assignments.
	 */
	public ArrayAssignments() {
		this.assignments = new Assignment[0];
	}
	
	/**
	 * Constructor that receives an array of assignments.
	 * @param assignments Array of assignments.
	 */
	public ArrayAssignments(Assignment[] assignments) {
		this.assignments = assignments;
	}
	
	public double valueOf(Variable var) {
		if (var == null) {
			throw new RuntimeException("ArrayAssignments.valueOf() received a variable that is null.");
		}
		
		// go over the assignments array and try to find the value of the requested var in it
		for (int i = 0; i < this.assignments.length; ++i) {
			if (this.assignments[i].getVar().equals(var)) {
				// we found the variable we searched for! return its value.
				return this.assignments[i].getValue();
			}
		}
				
		// if we got here, var isn't in the array of assignments.
		// throw an appropriate exception
		throw new RuntimeException("ArrayAssignments.valueOf() received a variable (" + var.getName() + ") that is not in the array of assignments.");
	}
	
	public void addAssignment(Assignment assignment) {
		for (int i = 0; i < this.assignments.length; ++i) {
			if (this.assignments[i].getVar().equals(assignment.getVar())) {
				// we found the var in the array of assignments,
				// so instead of adding a new assignment, we will replace the old value.
				this.assignments[i].setValue(assignment.getValue());
				return;
			}
		}
		
		// if we got here, we didn't find the variable in the array of assignments,
		// so we need to add a new assignment.
		Assignment[] newAssignments = new Assignment[this.assignments.length+1];
		for (int i = 0; i < this.assignments.length; ++i) {
			newAssignments[i] = this.assignments[i];
		}
		
		newAssignments[this.assignments.length] = assignment;
		this.assignments = newAssignments;
	}
}