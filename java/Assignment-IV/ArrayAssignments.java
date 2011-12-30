/**
 * 
 */

/**
 * @author Dvir
 *
 */
public class ArrayAssignments implements Assignments {
	private Assignment[] assignments;
	
	public ArrayAssignments() {
		this.assignments = new Assignment[0];
	}
	
	public ArrayAssignments(Assignment[] assignments) {
		this.assignments = assignments;
	}
	
	public double valueOf(Variable var) {
		if (var == null) {
			throw new RuntimeException("ArrayAssignments.valueOf() received a variable that is null.");
		}
		
		for (int i = 0; i < assignments.length; ++i) {
			if (assignments[i].getVar().equals(var)) {
				return assignments[i].getValue();
			}
		}
				
		// if we got here, var isn't in the array of assignments.
		// throw an appropriate exception
		throw new RuntimeException("ArrayAssignments.valueOf() received a variable (" + var.getName() + ") that is not in the array of assignments.");
	}
	
	public void addAssignment(Assignment assignment) {
		for (int i = 0; i < assignments.length; ++i) {
			if (assignments[i].getVar().equals(assignment.getVar())) {
				// we found the var in the array of assignments,
				// so instead of adding a new assignment, we will replace the old value.
				assignments[i].setValue(assignment.getValue());
				return;
			}
		}
		
		// if we got here, we didn't find the variable in the array of assignments,
		// so we need to add a new assignment.
		Assignment[] newAssignments = new Assignment[assignments.length+1];
		for (int i = 0; i < assignments.length; ++i) {
			newAssignments[i] = assignments[i];
		}
		
		newAssignments[assignments.length] = assignment;
		assignments = newAssignments;
	}
}
