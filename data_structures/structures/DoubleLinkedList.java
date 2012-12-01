public class DoubleLinkedList extends LinkedList {
	DoubleLinkedList() {
		super();
	}
	
	DoubleLinkedList(Link root) {
		super(root);
	}
	
	public Link insert(Link link) {
		if (this.head == null) {
			this.head = link;
			this.tail = link;
		} else {
			this.tail.setNext(link);
			((DoubleLink) link).setPrev(this.tail);
		}
		
		return link;
	}
}
