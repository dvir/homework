/**
 * 
 */

/**
 * @author Dvir
 *
 */
public class Page {
	private String data;
	private int index;
	
	public Page(int index, String data) {
		this.data = data;
		this.index = index;
	}
	
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
