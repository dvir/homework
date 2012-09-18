public class MinHeap {
	private int[] data;
	private int count;
	private int size;
	
	MinHeap() {
		this(0);
	}
	
	MinHeap(int size) {
		this.data = new int[size];
		this.count = 0;
		this.size = size;
	}
	
	MinHeap(int[] data) {
		this(data.length);
		for (int i = 0; i < data.length; i++) {
			this.data[i] = data[i];
		}
		
		this.count = data.length;
		
		for (int i = (data.length / 2) - 1; i >= 0; i--) {
			minHeapify(i);
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
	
	public int extractMin() {
		if (this.count <= 0) {
			throw new RuntimeException("EmptyHeapException: Cannot extract min from an empty heap.");
		}
		
		int min = this.data[0];
		this.data[0] = this.data[this.count-1];
				
		this.count--;
		
		this.resize(-1);
		
		this.minHeapify(0);
		
		return min;
	}
	
	private void resize(int size_increment) {
		int[] newData = new int[this.getSize() + size_increment];
		for (int i = 0; i < Math.min(this.getSize(), this.getSize() + size_increment); i++) {
			newData[i] = this.data[i];
		}
		
		this.size += size_increment;
		this.data = newData;
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
		if (i > 1 && this.data[i] < this.data[parent(i)]) {
			swap(i, parent(i));
			heapify(parent(i));
		}
	}
	
	private void minHeapify(int i) {
		int largest = i;
		
		if (leftChild(i) < this.getSize() && this.data[largest] > this.data[leftChild(i)]) {
			largest = leftChild(i);
		}
		
		if (rightChild(i) < this.getSize() && this.data[largest] > this.data[rightChild(i)]) {
			largest = rightChild(i);
		}
		
		if (largest != i) {
			swap(largest, i);
			minHeapify(largest);
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
