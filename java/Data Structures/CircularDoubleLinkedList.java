public class CircularDoubleLinkedList extends DoubleLinkedList {
	CircularDoubleLinkedList() {
		super();
	}
	
	CircularDoubleLinkedList(Link root) {
		super(root);
	}
	
	public Link insert(Link link) {
		if (this.head == null) {
			this.head = link;
			this.tail = link;
		} else {
			this.tail.setNext(link);
			((DoubleLink) link).setPrev(this.tail);
			
			// this line is needed only in circular linked lists
			link.setNext(this.head);
		}
		
		return link;
	}
}
