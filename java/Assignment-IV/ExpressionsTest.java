import java.util.Comparator;

public class ExpressionsTest {

	/**
	 * A basic testing code for assignment 4. See the
	 * TestOutput.tex file in the course web-site for
	 * expected output. Note that different systems may
	 * represent {@code double} values slightly different, and that
	 * the order of sub-expressions might change for
	 * different derivative implementations.
	 */
	public static void main(String[] args) {
		VariableExpression x = new VariableExpression('x');
		VariableExpression y = new VariableExpression('y');
		VariableExpression z = new VariableExpression('z');

        Assignments assignments = new ArrayAssignments();
        assignments.addAssignment(new ValueAssignment(x, +1.1));
        assignments.addAssignment(new ValueAssignment(y, +2.2));
        assignments.addAssignment(new ValueAssignment(z, -3.3));

		Expression exp1 = new Addition(x, y); 
		Expression exp2 = new Subtraction(y, z);
		Expression exp3 = new Multiplication(exp1, exp2);
		Expression exp4 = new Sine(exp3);
		Expression exp5 = new Cosine(exp3);
		Expression exp6 = new Power(exp3, 4.2);

		double[] coefficients = {-2, 4.1, -0.32};
		Expression exp7 = new Polynomial(x, coefficients);
		
		Expression exp8 = exp7.derivative(x);
		Expression exp9 = exp7.derivative(y);
		Expression exp10 = exp5.derivative(z); 
		Expression exp11 = exp6.derivative(y);

		Expression[] expressions = {
				exp1, exp2, exp3, exp4,
				exp5, exp6, exp7, exp8,
				exp9, exp10, exp11
		};

		System.out.println("Unsorted expressions:");
		printExpressionArray(expressions, assignments);
		
		Expression exp12 = new Addition(x, y);
		Expression exp13 = new Addition(y, x);  
		Expression exp14 = new Subtraction(x, y);
		
		System.out.println("\nBasic equality check:");

		System.out.println("\nFirst expression:" + exp1);
		System.out.println("Second expression:" + exp12);
		System.out.println("Is first equal to second? " + exp1.equals(exp12));

		System.out.println("\nFirst expression:" + exp1);
		System.out.println("Second expression:" + exp13);
		System.out.println("Is first equal to second? " + exp1.equals(exp13));
	
		System.out.println("\nFirst expression:" + exp1);
		System.out.println("Second expression:" + exp14);
		System.out.println("Is first equal to second? " + exp1.equals(exp14));
	}
	
	/**
	 * Prints an array of expressions to the screen.
	 * 
	 * @param expressions an array of expressions to be printed.
	 */
	private static void printExpressionArray(Expression[] expressions, Assignments assignments) {
		for (int i=0; i<expressions.length; i = i+1){
			printEvaluatedExpression(expressions[i], assignments);
		}
	}

	/**
	 * Prints an expression along with its evaluated value.
	 *  
	 * @param exp an expression to be printed.
	 */
	private static void printEvaluatedExpression(Expression exp, Assignments assignments){
		System.out.println(exp + " = " + exp.evaluate(assignments));
	}
	
	/**
	 * Sorting an array of objects, with respect to the order which
	 * is dictated by a given Comparator object.
	 * 
	 * @param arr an array of objects to be sorted.
	 * @param comparator a Comparator which define the order of the elements.
	 */
	private static void sort(Object[] arr, Comparator comparator){
		mergeSort(arr, 0, arr.length, comparator);
	}

	/**
	 * A slightly different implementation of "Merge-Sort" than
	 * that presented in class. The recursive procedure sorts the 
	 * elements in the sub-array between indices {@code from} (inclusive) 
	 * and {@code to} (exclusive).
	 * 
	 * @param arr an array of objects to sort.
	 * @param from a starting index (inclusive).
	 * @param to an ending index (exclusive).
	 * @param comparator a Comparator which defines the order of the elements.
	 */
	@SuppressWarnings("unchecked")
	private static void mergeSort(Object[] arr, int from, int to,
			Comparator comparator) {
		if (to-from > 1){
			int mid = (to+from)/2;
			mergeSort(arr, from, mid, comparator);
			//Condition: the sub-array arr[from], arr[from+1], ..., arr[mid-1] is sorted.
			mergeSort(arr, mid, to, comparator);
			//Condition: the sub-array arr[mid], arr[mid+1], ..., arr[to-1] is sorted.
			merge(arr, from, mid, to, comparator);
			//Post-condition: the sub-array arr[from], arr[from+1], ..., arr[to-1] is sorted.
		}
	}

	/**
	 * Merging two adjacent sorted sub-arrays.
	 * Pre-condition: the two sub-arrays {@code arr[from...mid-1]}
	 * and {@code arr[mid...to-1]} are sorted.
	 * 
	 * @param arr an array of objects to sort.
	 * @param from a starting index (inclusive).
	 * @param mid a middle index (excluded from the first sub-array and included in the second sub-array).
	 * @param to an ending index (exclusive).
	 * @param comparator a Comparator which define the order of the elements.
	 */
	@SuppressWarnings("unchecked")
	private static void merge(Object[] arr, int from, int mid, int to, Comparator comparator) {
		// Pre-condition: the two sub-arrays arr[from...mid-1] 
		// and arr[mid...to-1] are sorted.
		
		int intervalLength = to-from; // the length of the merged sub-array. 
		Object[] auxArr = new Object[intervalLength]; // an auxiliary array for the merging procedure.
		int firstIndex = from; // an index of the next smallest element in the first sub-array.
		int secondIndex = mid; // an index of the next smallest element in the second sub-array.
		
		// Loop invariant:
		// 1. arr[from...mid-1] and arr[mid...to-1] are sorted.
		// 2. firstIndex <= mid and secondIndex <= to.
		// 3. auxArr[0...i-1] contains the elements of arr[from...firstIndex-1]
		//    and arr[mid...secondIndex-1], sorted with increasing order.
		// 4. None of the elements in arr[from...firstIndex-1] is bigger than
		//	  an element in arr[secondIndex...to-1], and none of the elements in 
		//	  arr[mid...secondIndex-1] is bigger than an element in arr[firstIndex...mid-1]
		for (int i = 0; i < intervalLength; i = i+1){
			if (firstIndex == mid){
				auxArr[i] = arr[secondIndex];
				secondIndex = secondIndex+1;
			}
			else if (secondIndex == to){
				auxArr[i] = arr[firstIndex];
				firstIndex = firstIndex+1;
			}
			else if (comparator.compare(
					arr[firstIndex], arr[secondIndex]) <= 0){
				auxArr[i] = arr[firstIndex];
				firstIndex = firstIndex+1;
			}
			else{
				auxArr[i] = arr[secondIndex];
				secondIndex = secondIndex+1;
			}
		}
		// Loop post-condition:
		// auxArr[0...intervalLength-1] contains the elements of arr[from...to-1]
		// sorted with increasing order.
		
		// Copying the elements from auxArr to arr[from...to-1]: 
		for (int i = 0; i < intervalLength; i = i+1){
			arr[from + i] = auxArr[i];
		}
	}

}
