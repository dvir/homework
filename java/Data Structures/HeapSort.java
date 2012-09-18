public class HeapSort {
	private int[] data;
	private int count;
	private int size;
	
	HeapSort(int[] data) {
		this.data = data;	
		this.size = data.length;
		
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
	
	public int extractMax() {
		if (this.count <= 0) {
			throw new RuntimeException("EmptyHeapException: Cannot extract max from an empty heap.");
		}
		
		int min = this.data[0];
		this.data[0] = this.data[this.count-1];
		this.data[this.count-1] = min;
				
		this.count--;
		this.size--;
		
		this.maxHeapify(0);
		
		return min;
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
	
	private int leftChild(int i) {
		return 2*i + 1;
	}
	
	private int rightChild(int i) {
		return 2*i + 2;
	}
	
	public int[] sort() {
		while (!this.isEmpty()) {
			this.extractMax();
		}
		
		return this.data;
	}
	
	public static int[] sort(int[] data) {
		HeapSort heapSort = new HeapSort(data);
		return heapSort.sort();
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
