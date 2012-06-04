/**
 * @author Dvir Azulay, Or Elmaliach
 * this class represents the primary memory component of the memory menagement system
 * used for writing data to the memory and reading from it
 */

public class RAM implements Queue, Memory{
	//state
	private int size; // counter for the number of elements, which are currently in the list
	private Link[] ram; // a circular double linked list
	private Link last; // pointer to the last link in the list
	private boolean useLRU; // true if we use LRU method, false if we use FIFO method
	
	//constructors
	public RAM(boolean useLRU) {
		this.size = 0;
		this.ram = new Link[MemoryManagementSystem.PHYSICAL_MEMORY_SIZE]; // initialize the RAM array, in the size of the physical memory, so the indexes will correspond to the physical memory's indexes
		for (int i = 0; i < MemoryManagementSystem.PHYSICAL_MEMORY_SIZE; i++) {
			this.ram[i] = null; // null = page is NOT in the RAM, otherwise - the page in the RAM
		}
		
		this.last = null;
		this.useLRU = useLRU; // determines whether we handle the RAM's queue by FIFO or by LRU
	}
	
	//behavior
	/**
	 * Reads data from the RAM
	 * @param key The physical memory's index to read from
	 * @return A string representing the data in this index
	 */
	public String read(int key) {
		if (this.useLRU) {
			this.pushToTop(key);
		}
		
		return this.ram[key].getPage().getData();
	}
	
	/**
	 * Write data into the a RAM slot
	 * @param key The memory slot to write to
	 * @param c The data to append to the memory slot
	 */
	public void write(int key, char c) {
		if (this.useLRU) {
			this.pushToTop(key);
		}
		
		this.ram[key].getPage().appendChar(c);		
	}
	
	/**
	 * Push a page in the RAM to the top of the queue
	 * @param key The page index in the RAM
	 */
	private void pushToTop(int key) {
		// the page is already in the queue, and we are using LRU (Least Recently Used), 
		// which means we should move this page to the head of the queue
		Link first = this.last.getNext();
		
		if (this.ram[key] != first) {
			Link temp = this.ram[key];
			
			//we're gapping over this page, effectively removing it from it's current place in the queue
			temp.getPrev().setNext(temp.getNext());
			temp.getNext().setPrev(temp.getPrev());
			
			//updating the current last link pointer if nessecery, crutial for following code
			if (this.ram[key] == this.last) {
				this.last = this.last.getPrev();
			}
			
			//inserting the current link to  the head of the line, and updating the 'next' and 'prev' pointers as needed
			temp.setNext(first);
			this.last.setNext(temp);
			temp.setPrev(this.last);
			first.setPrev(temp);
		}
	}	
	
	/**
	 * Checks if a page is currently in the RAM
	 * @param key The memory slot index
	 * @return True if the page is in the RAM, false otherwise
	 */
	public boolean exists(int key) {
		return (this.ram[key] != null);
	}
	
	/**
	 * Checks if the RAM queue is empty
	 * @return True if empty, false otherwise
	 */
	public boolean isEmpty() {
		return (this.size == 0);
	}
	
	/**
	 * Checks if the RAM queue is full
	 * @return True if it's full, false otherwise
	 */
	public boolean isFull() {
		return (this.size == MemoryManagementSystem.RAM_SIZE);
	}
	
	/**
	 * Enqueues a new page to the RAM queue
	 * @param page The page to enqueue
	 * @throws A runtime exception if the RAM is full
	 */
	public void enqueue(Page page) {
		if (this.isFull()) {
			// the queue is full! can't add any more pages to the RAM		
			throw new RuntimeException("QueueOverflow in RAM.enqueue()");
		}
		
		// push the page to the start of the queue		
		Link newLink = new Link(new Page(page.getIndex(), page.getData()));
		
		// if the queue is empty, initialize it with the new link
		if (this.last == null) {
			this.last = newLink;
			newLink.setNext(newLink);
			newLink.setPrev(newLink);
		}
		else {
			newLink.setPrev(this.last);
			newLink.setNext(this.last.getNext());
			this.last.getNext().setPrev(newLink);
			this.last.setNext(newLink);
		}
		
		this.ram[newLink.getPage().getIndex()] = newLink;
		
		this.size++;		
	}
	
	/**
	 * Dequeue's the last element of the RAM queue
	 * @return page The page we dequeued from the RAM
	 */
	public Page dequeue() {		
		if (this.isEmpty()) {
			// the queue is empty! nothing to dequeue
			throw new RuntimeException("QueueUnderflow in RAM.dequeue()");
		}
		
		Link toRemove = this.last; // the last Link in the RAM queue		
		Link first = this.last.getNext(); // the first link in the queue
		
		//gapping over the last link
		first.setPrev(this.last.getPrev());
		this.last.getPrev().setNext(this.last.getNext());
		
		this.last=this.last.getPrev(); //updating the last link pointer
		
		this.ram[toRemove.getPage().getIndex()] = null; //remove the link from the RAM index
		this.size--; //decrease the number of elements in the queue
		
		return toRemove.getPage();
	}
}
