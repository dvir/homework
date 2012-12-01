public class MaxHeap {
	private int[] data;
	private int count;
	private int size;
	
	MaxHeap() {
		this(0);
	}
	
	MaxHeap(int size) {
		this.data = new int[size];
		this.count = 0;
		this.size = size;
	}
	
	MaxHeap(int[] data) {
		this(data.length);
		for (int i = 0; i < data.length; i++) {
			this.data[i] = data[i];
		}
		
		this.count = data.length;
		
		for (int i = (data.length / 2) - 1; i >= 0; i--) {
			maxHeapify(i);
		}
	}
	
	public int getSize() {
		return this.size;
	}
	
	public boolean isEmpty() {
		return this.count <= 0;
	}
	
	public int peek() {
		if (this.count <= 0) {
			throw new RuntimeException("EmptyHeapException: Cannot peek an empty heap.");
		}
		
		return this.data[0];
	}
	
	public int extractMax() {
		if (this.count <= 0) {
			throw new RuntimeException("EmptyHeapException: Cannot extract max from an empty heap.");
		}
		
		int max = this.data[0];
		this.data[0] = this.data[this.count-1];
				
		this.count--;
		
		this.resize(-1);
		
		this.maxHeapify(0);
		
		return max;
	}
	
	private void resize(int size_increment) {
		// only increase the data array size if we actually need it.
		// (we won't need it if we deleted some elements and there's enough room for it)
		if (this.getSize() + size_increment > this.data.length) {
			int[] newData = new int[this.getSize() + size_increment];
			for (int i = 0; i < Math.min(this.getSize(), this.getSize() + size_increment); i++) {
				newData[i] = this.data[i];
			}
			
			this.data = newData;
		}
		this.size += size_increment;
	}
	
	public void insert(int newData) {
		this.resize(+1);

		this.data[this.getSize()-1] = newData;

		this.count++;	
		
		this.heapify(this.getSize()-1);
	}
	
	public void insert(int[] newData) {
		for (int i = 0; i < newData.length; i++) {
			this.insert(newData[i]);
		}
	}
	
	private void heapify(int i) {
		if (i > 1 && this.data[i] > this.data[parent(i)]) {
			swap(i, parent(i));
			heapify(parent(i));
		}
	}
	
	private void maxHeapify(int i) {
		int largest = i;
		
		if (leftChild(i) < this.getSize() && this.data[largest] < this.data[leftChild(i)]) {
			largest = leftChild(i);
		}
		
		if (rightChild(i) < this.getSize() && this.data[largest] < this.data[rightChild(i)]) {
			largest = rightChild(i);
		}
		
		if (largest != i) {
			swap(largest, i);
			maxHeapify(largest);
		}
	}
	
	private void swap(int first, int second) {
		int temp = this.data[first];
		this.data[first] = this.data[second];
		this.data[second] = temp;
	}
	
	private int parent(int i) {
		return (i % 2 == 0) ? (i-2) / 2 : (i-1) / 2;
	}
	
	private int leftChild(int i) {
		return 2*i + 1;
	}
	
	private int rightChild(int i) {
		return 2*i + 2;
	}
	
	public String toString() {
		String str = "";
		if (this.count > 0) {
			str = "" + this.data[0];
		}
		
		for (int i = 1; i < this.getSize(); i++) {
			str = str + "," + this.data[i];
		}
		
		return str;
	}
}
