public class DoubleLink extends Link {
	private Link prev = null;
	
	public DoubleLink(int data) {
		super(data);
		this.setPrev(this);
		this.setNext(this);
	}
	
	public Link getPrev() {
		return this.prev;
	}
	
	public void setPrev(Link prev) {
		this.prev = prev;
	}
	
	public Link deleteNext() {
		Link next = this.getNext();
		if (next != null) {
			Link nextNext = next.getNext();
			this.setNext(next.getNext());
			
			if (nextNext != null) {
				((DoubleLink) nextNext).setPrev(this);
			}
		}
		
		return next;
	}
	
	public Link delete() {
		Link prev = this.getPrev();
		if (prev != null) {
			Link next = this.getNext();
			prev.setNext(next);
			((DoubleLink) next).setPrev(prev);
		}
		
		return prev;
	}
}
