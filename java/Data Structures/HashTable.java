/**
 * @author Or Elmaliach 301482220 ; Dvir Azulay 200534014
 * Implementation of a hash table holding air plane passengers ids.
 * Each slot in the hash table is of type HashSlot.
 */
public class HashTable {
	private int size; // the size of the hash table. (N / 3)
	private Link[] table; // the hash table; an array of hash slots
	
	public HashTable (int N) {
		this.size = N;
		this.table = new Link[this.size];
		
		// initialize the hash table with empty hash slots.
		for (int i = 0; i < this.size; i++) {
			this.table[i] = null;
		}
	}
		
	public boolean insert (int data) {
		// activate the hash function on the id and insert it into the respective hash slot
		int key = this.hash(data);
		
		if (this.table[key] == null) {
			this.table[key] = new Link(data);
		} else {
			// search for the id in the current slot. if it exists, don't add it. if it doesn't, chain it
			Link element = this.table[key];
			while (element != null) {
				if (element.getData() == data) {
					// id already exists in the table. Don't add anything
					return false;
				}
			}
			
			// if we got here, we need to create a new link with the given id and chain it.
			// we are chaining to the start of the linked list.
			Link newLink = new Link(data);
			newLink.setNext(this.table[key]);
			this.table[key] = newLink;
		}
		
		return true;
	}
	
	public Link search (int data) {
		// activate the hash function on the id and search the respective hash slot
		return this.table[hash(data)].search(data);
	}
	
	public String toString () {
		String result = "";
		
		// check to see if the table is empty; if it is, return an empty string
		if (this.size == 0) {
			return "";
		}
		
		// if we got here, the table isn't empty. Set the result string to be the representation
		// of the first hash slot.
		result = this.table[0].toString(); // returns a string with the height and count of elements in the first hash slot
		for (int i = 1; i < this.size; i++) {
			// append the height and count of elements of the hash slot to the result string
			result += " " + this.table[i].toString(); 
		}
		
		return result;
	}
	
	private int hash (int data) {
		// hash the id; defined to be "id modulo N/3" where N is the amount of input.
		return (data % this.size);
	}
}