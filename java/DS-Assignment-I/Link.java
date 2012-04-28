/**
 * @author Dvir Azulay, Or Elmaliach
 * This class represents a single link in a double-linked list
 * as such, it has fields referring to it's previous and next links in the list
 */
public class Link {
	//state
	private Link prev = null;
	private Link next = null;
	private Page page = null;
	
	//constructors
	public Link(Page page) {
		this.page = page;
		this.next = this;
		this.prev = this;
	}
	
	//behavior
	public Page getPage() {
		return this.page;
	}
	
	public Link getPrev() {
		return this.prev;
	}
	
	public void setPrev(Link prev) {
		this.prev = prev;
	}	
	
	public Link getNext() {
		return this.next;
	}
	
	public void setNext(Link next) {
		this.next = next;
	}
}
