/**
 * 
 */

/**
 * @author Dvir
 *
 */

import java.util.Arrays;

public class MemoryManagementSystem {
	private String[] secondaryMemory;	//This array will hold the secondary memory
	private boolean useLRU; 				// true if LRU is used, false if FIFO is used
	/*
	 * You can add more fields here
	 */	

	public MemoryManagementSystem(boolean useLRU) {
		secondaryMemory = new String[1000];	
		this.useLRU = useLRU;
		/*
		 * Your code comes next
		 */
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "secondaryMemory=" + Arrays.toString(secondaryMemory);
	}
	
	//This method returns the data you read. Notice that this data is not used by our main, but you can use it for testing your code.
	public String read(int index) {
		/*
		 * Your code here
		 */
	}

	public void write(int index, char c) {
		/*
		 * Your code here
		 */
	}
	
	/*
	 * You can add more methods here
	 */
}