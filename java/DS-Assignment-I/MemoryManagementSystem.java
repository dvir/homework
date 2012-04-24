import java.util.Arrays;


public class MemoryManagementSystem {
	private String[] secondaryMemory;	//This array will hold the secondary memory
	private boolean useLRU; 				// true if LRU is used, false if FIFO is used
	private RAM primaryMemory; // the RAM

	public MemoryManagementSystem(boolean useLRU) {
		this.primaryMemory = new RAM(50, 1000, useLRU); // initialize the RAM
		this.useLRU = useLRU; // should we sort the RAM with LRU?
		
		this.secondaryMemory = new String[1000]; // the physical memory
		
		// initialize the physical memory with empty strings.
		for (int i = 0; i < 1000; i++) {
			secondaryMemory[i] = "";
		}
		
		// initialize the RAM with the first ramSize pages from the physical memory.
		for (int i = 0; i < 50; i++) {
			this.setupInRam(i);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "secondaryMemory=" + Arrays.toString(secondaryMemory);
	}
	
	/**
	 * Reads the data from a memory slot. Retrieves data from the RAM.
	 * @param index The memory slot to read from
	 * @return The data from the memory slot
	 */
	public String read(int index) {
		this.setupInRam(index);
		return primaryMemory.read(index);
	}

	/**
	 * Writes more data to a memory slot. Writes to the RAM.
	 * @param index The memory slot to write to
	 * @param c The character to write
	 */
	public void write(int index, char c) {
		this.setupInRam(index);
		primaryMemory.write(index,c);
	}
	
	/**
	 * Sets up the memory slot in the RAM
	 * @param index The index of the memory slot
	 */
	private void setupInRam(int index) {
		if (!primaryMemory.exists(index)) {
			if (primaryMemory.isFull()) {
				Page last = primaryMemory.dequeue();
				secondaryMemory[last.getIndex()] = last.getData();
			}
					
			primaryMemory.enqueue(new Page(index, secondaryMemory[index]));
		}		
	}
}
