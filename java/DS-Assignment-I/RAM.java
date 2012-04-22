/**
 * 
 */

/**
 * @author Dvir
 *
 */

public class RAM implements Queue {
	private int size;
	private int ramSize;
	private Link last; // a circular linked list
	private Link[] ram; // a list indexing the circular linked list
	
	private boolean useLRU;
	
	public RAM(int ramSize, int romSize, boolean useLRU) {
		this.size = 0;
		this.ramSize = ramSize;
		this.ram = new Link[romSize];
		for (int i = 0; i < romSize; i++) {
			this.ram[i] = null; // null = page is NOT in the RAM, otherwise - the page in the RAM
		}
		this.last = null;
		this.useLRU = useLRU;
	}
	
	public String read(int key) {
		if (this.useLRU) {
			// the page is already in the list, and we are using LRU (Least Recently Used), 
			// which means we should "push" the page up
			Link currLink = this.ram[key];
			Link prevLink = currLink.getPrev();
			
			prevLink.setNext(currLink.getNext());
			currLink.setNext(this.last.getNext());
			this.last.setNext(currLink);
		}
		
		return this.ram[key].getPage().getData();
	}
	
	public void write(int key, char c) {
		String oldData = this.ram[key].getPage().getData();
		this.ram[key].getPage().setData(oldData + c);
	}
	
	public boolean exists(int key) {
		return (this.ram[key] != null);
	}
	
	public boolean isEmpty() {
		return (this.size == 0);
	}
	
	public boolean isFull() {
		return (this.size == this.ramSize);
	}
	
	public void enqueue(Page page) {
		if (this.isFull()) {
			// the queue is full! can't add any more pages to the RAM.
			// throw an exception.
			// NOTE: we could've just dequeued and then enqueued, but in the RAM we need to save the dequeued pages,
			// so we can't just pop it.			
			throw new RuntimeException("QueueOverFlow in RAM.enqueue()");
		}
		
		// push the page to the start of the queue
		// first - move everything down one slot
		Link newLink = new Link(page);
		
		if (this.last == null) {
			this.last = newLink;
			this.last.setNext(this.last);
			this.last.setPrev(this.last);
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
	
	public Page dequeue() {
		if (this.isEmpty()) {
			// the queue is empty! nothing to dequeue.
			// throw an exception.
			throw new RuntimeException("QueueUnderFlow in RAM.dequeue()");
		}
		
		Link link = this.last;
		Page page = link.getPage();
		this.ram[link.getPage().getIndex()] = null;
		
		link.getPrev().setNext(link.getNext());
		
		this.size--;
		return page;
	}
}
