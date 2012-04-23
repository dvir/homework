import java.util.Arrays;


public class MemoryManagementSystem {
	private String[] secondaryMemory;	//This array will hold the secondary memory
	private boolean useLRU; 				// true if LRU is used, false if FIFO is used
	private RAM primaryMemory;

	public MemoryManagementSystem(boolean useLRU) {
		secondaryMemory = new String[1000];
		this.useLRU = useLRU;
		primaryMemory = new RAM(50,1000,useLRU);
		for (int i=0;i<1000;i++) {
			secondaryMemory[i]="";
		}
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
		if (!primaryMemory.exists(index)) {
			if (primaryMemory.isFull()) {
				Page last = primaryMemory.dequeue();
				secondaryMemory[last.getIndex()] = last.getData();
			}
			primaryMemory.enqueue(new Page(index, secondaryMemory[index]));
		}
		return primaryMemory.read(index);
		}
	}

	public void write(int index, char c) {
		if (!primaryMemory.exists(index)) {
			if (primaryMemory.isFull()) {
				Page last = primaryMemory.dequeue();
				secondaryMemory[last.getIndex()] = last.getData();
			}
			primaryMemory.enqueue(new Page(index, secondaryMemory[index]));
		}
		primaryMemory.write(index,c);
	}
	
	/*
	 * You can add more methods here
	 */
}
