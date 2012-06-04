
public class HashTable {
	int size; // the size of the hash table. (N / 3)
	HashSlot[] table; // the hash table; an array of hash slots
	int count;
	
	public HashTable (int N) {
		this.count = N;
		this.size = N / 3;
		this.table = new HashSlot[this.size];
		
		// initialize the hash table with empty hash slots.
		for (int i = 0; i < this.size; i++) {
			this.table[i] = new HashSlot();
		}
	}
	
	public int getCount () {
		return this.count;
	}
	
	public void insert (int id) {
		// activate the hash function on the id and insert it into the respective hash slot		
		this.table[hash(id)].insert(id);
	}
	
	public int search (int id) {
		// activate the hash function on the id and search the respective hash slot
		return this.table[hash(id)].search(id);
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
		for (int i = 1; i < this.table.length; i++) {
			// append the height and count of elements of the hash slot to the result string
			result += " " + this.table[i].toString(); 
		}
		
		return result;
	}
	
	private int hash (int id) {
		// hash the id; defined to be "id modulo N/3" where N is the amount of input.
		return (id % this.size);
	}
}