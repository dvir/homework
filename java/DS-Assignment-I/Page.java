/**
 * @author Dvir Azulay, Or Elmaliach
 * This class represents a single page in the memory
 * as such it has a string representing it's data, and an index
 */
public class Page {
	//state
	private String data;
	private int index;
	
	//constructors
	public Page(int index, String data) {
		this.data = data;
		this.index = index;
	}
	
	//behavior
	public String getData() {
		return this.data;
	}
	
	public void setData(String data) {
		this.data = data;
	}
	
	public void appendChar(char c) {
		this.data = data + c;
	}
	
	public int getIndex() {
		return this.index;
	}
	
	public String toString() {
		return "["+this.index+"]" + this.data;
	}
}
