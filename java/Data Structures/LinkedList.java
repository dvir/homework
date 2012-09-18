public class LinkedList {
	protected Link head;
	protected Link tail;
	protected int size;
	
	LinkedList() {
		this.head = null;
		this.tail = null;
		this.size = 0;
	}
	
	LinkedList(Link root) {
		this.head = root;
		this.tail = root;
		this.size = 1;
	}
	
	public int getSize() {
		return this.size;
	}
	
	public Link search(int data) {
		if (this.head == null) {
			return null;
		}
		
		return this.head.search(data);
	}
	
	public Link insert(Link link) {
		if (this.head == null) {
			this.head = link;
			this.tail = link;
		} else {
			this.tail.setNext(link);
			
			// this line is needed only in circular linked lists
			// link.setNext(this.head);
		}
		
		this.size++;
		return link;
	}
	
	public Link insert(int data) {
		Link newLink = new Link(data);
		return this.insert(newLink);
	}
	
	public Link delete(int data) {
		Link link = this.search(data);
		if (link != null) {
			this.size--;
			//return link.delete();
		}
		
		return null;
	}
	
	public Link removeFirst() {
		Link link = this.head;
		if (link != null) {
			this.head = link.getNext();
			this.size--;
		}
		
		return link;
	}
	
	public Link addFirst(int data) {
		Link link = new Link(data);
		link.setNext(this.head);
		this.head = link;
		
		this.size++;
		return link;
	}	
	
	public Link addLast(int data) {
		return this.insert(data);
	}
	
	public boolean isEmpty() {
		return (this.head == null);
	}
}
