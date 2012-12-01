public class Link {
	private Link next = null;
	private int data;
	
	public Link(int data) {
		this.data = data;
		this.next = this;
	}
	
	public int getData() {
		return this.data;
	}
	
	public void setData(int data) {
		this.data = data;
	}
	
	public Link getNext() {
		return this.next;
	}
	
	public void setNext(Link next) {
		this.next = next;
	}
	
	public Link search(int data) {
		if (this.data == data) {
			return this;
		}
		
		if (this.getNext() != null) {
			return this.getNext().search(data);
		}
		
		return null;
	}
	
	public Link deleteNext() {
		Link next = this.getNext();
		if (next != null) {
			this.setNext(next.getNext());
		}
		
		return next;
	}
}
