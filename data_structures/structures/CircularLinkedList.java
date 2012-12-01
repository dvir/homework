public class CircularLinkedList extends LinkedList {
	CircularLinkedList() {
		super();
	}
	
	CircularLinkedList(Link root) {
		super(root);
	}
	
	
	public Link insert(Link link) {
		if (this.head == null) {
			this.head = link;
			this.tail = link;
		} else {
			this.tail.setNext(link);
			
			// this line is needed only in circular linked lists
			link.setNext(this.head);
		}
		
		return link;
	}
}
