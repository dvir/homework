/**
 * @author Or Elmaliach 301482220 ; Dvir Azulay 200534014
 * HashSlot is a slot in a hash table that holds air plane passengers ids.
 * The data of each slot is stored in an AVL tree structure.
 */
public class HashSlot {
	private AVLNode tree; // the tree containing the ids in the hash slot
	private int count; // the amount of ids in the tree
	
	public HashSlot () {
		this.tree = null;
		this.count = 0;
	}
	
	public void insert (int id) {
		if (this.tree == null) {
			// we don't have a tree yet, create it with the root as the id we received 
			this.tree = new AVLNode(id);
		} else {
			// insert the id into the AVL tree
			// and update the tree root if given a new one from insert
			this.tree = this.tree.insert(id);
		}
		
		this.count++;
	}
	
	public int search (int id) {
		if (this.tree == null) {
			// tree is empty, we don't need to search it
			return -1;
		}
		
		// search the tree for the id and return
		int searchResult = this.tree.search(id);
		if (searchResult > 0) {
			return searchResult+1;
		}
		
		return searchResult-1;
	}	
	
	public int getCount () {
		return this.count;
	}
	
	public int getHeight () {
		if (this.tree == null) {
			// tree is empty; as defined, it's height is zero
			return 0;
		}
		
		// get the height of the tree
		return this.tree.height();
	}
	
	public String toString () {
		// output the height and count of elements in the slot, in the format 'height,count'
		return this.getHeight() + "," + this.count;
	}
}
