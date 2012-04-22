/**
 * 
 */

/**
 * @author Dvir
 *
 */
public class Link {
	private Link prev = null;
	private Link next = null;
	private Page page = null;
	
	public Link(Page page) {
		this.page = page;
	}
	
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
