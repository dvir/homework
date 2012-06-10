/**
 * @author Or Elmaliach 301482220 ; Dvir Azulay 200534014
 */
public class CheckInTable {
	private int size; // the size of the hash table. (N)
	private int count; // the amount of people in the hash table
	private int[] table; // the hash table; an array of ids
	
	public CheckInTable (int N) {
		this.count = 0;
		this.size = N;
		this.table = new int[this.size];
		
		// initialize the hash table with empty hash slots (-1).
		for (int i = 0; i < this.size; i++) {
			this.table[i] = -1;
		}
	}

	/**
	 * Finds a seat for a given person id.
	 * @param id
	 * @param useFirstHash
	 * @return The amount of searches we did in order to find an empty seat.
	 */
	public int checkIn (int id, boolean useFirstHash) {
		if (this.count >= this.size) {
			// the hash table is full! don't check-in anyone.
			return 0;
		}
		
		// activate the hash function on the id and insert it into the respective hash slot
		int hashKey;
		if (useFirstHash) {
			hashKey = this.hash1(id);
		} else {
			hashKey = this.hash2(id);
		}
		
		//System.out.println("id: " + id + " - hash1: " + this.hash1(id) + " - hash2: " + this.hash2(id));

		// jump +1, -1, +2, -2, +3, -3, ...
		// NOTE: this relies on the fact that we won't try to insert a new id into a full hash table.
		int result = 1;
		boolean found = false;
		if (this.table[hashKey] == -1) {
			found = true;
		}
		
		for (int i = 1; i < this.size && !found; i++) {
			if (hashKey+i < this.size) {
				result++;
				if (this.table[hashKey+i] == -1) {
					hashKey += i;
					found = true;
				}
			} 
			
			if (!found && hashKey-i >= 0) {
				result++;
				if (this.table[hashKey-i] == -1) {
					hashKey -= i;
					found = true;
				}
			}
		}
		
		// we found a slot! put the id in it
		this.table[hashKey] = id;
		this.count++;
		
		return result;
	}

	private int reverse (int id) {
		int result = 0;
		while (id > 0) {
			result = result * 10;
			result += (id % 10);
			id = id / 10;
		}
		
		return result;
	}
	
	private int hash1 (int id) {
		// hash the id; defined to be "id modulo N" where N is the amount of input.
		return (id % this.size);
	}
	
	private int hash2 (int id) {
		// hash the id; defined to be "reverse(id) modulo N" where N is the amount of input.
		return (this.reverse(id) % this.size);
	}	
}