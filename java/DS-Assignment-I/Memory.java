/**
 * @author Dvir Azulay, Or Elmaliach
 * This is a simple interface to describe the mandatory actions that are required from any memory-type class
 */
public interface Memory {
	public String read(int key);
	public void write(int key, char c);
}
