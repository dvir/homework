/**
 * @author Dvir Azulay, Or Elmaliach
 * This is a simple interface describing the mandatory actions of any queue-based ADT, which is working with Page
 */
public interface Queue {
	public void enqueue(Page page);
	public Page dequeue();
	public boolean isEmpty();
}
