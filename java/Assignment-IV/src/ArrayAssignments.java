/**
 * ArrayAssignments is an object that holds an array of assignments and defines methods on that array.
 */

/**
 * @author Dvir
 *
 */
public class ArrayAssignments implements Assignments {
	private Assignment[] assignments; // list of assignments
	
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
		if (assignments == null) {
			throw new RuntimeException("ArrayAssignments.ArrayAssignments(assignments) received a null assignments array.");
		}			

		// make sure we have some assignments to add
		if (assignments.length == 0) {
			throw new RuntimeException("ArrayAssignments.ArrayAssignments(assignments) received an empty assignments array.");
		}					
		
		// go over the given array of assignments and add it using addAssignment.
		// we do that to make sure every assignment we add follows the rules we defined in addAssignment.
		this.assignments = new Assignment[0];
		for (int i = 0; i < assignments.length; ++i) {
			this.addAssignment(assignments[i]);
		}
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
		if (assignment == null) {
			throw new RuntimeException("ArrayAssignments.addAssignment() received a null assignment object.");
		}				
		
		// go over our assignments array looking for an existing assignment to the given variable.
		for (int i = 0; i < this.assignments.length; ++i) {
			// compare the variables in the current assignment and the new one.
			if (this.assignments[i].getVar().equals(assignment.getVar())) {
				// we found the variable in the array of assignments,
				// so instead of adding a new assignment, we will replace the old value.
				this.assignments[i].setValue(assignment.getValue());
				return;
			}
		}
		
		// if we got here, we didn't find the variable in the array of assignments, 
		// so we need to add a new assignment.
		// we do that by making a new assignments array with a room for the new assignment,
		// copying the old array and placing the new assignment in the available spot.
		Assignment[] newAssignments = new Assignment[this.assignments.length+1];
		for (int i = 0; i < this.assignments.length; ++i) {
			newAssignments[i] = this.assignments[i];
		}
		
		// place the new assignment in the available spot.
		newAssignments[this.assignments.length] = assignment;
		
		// replace our old array with the new one.
		this.assignments = newAssignments;
	}
}