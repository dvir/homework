public class Queue extends LinkedList {
	public Queue() {
		
	}

	public Link enqueue(int data) {
		return this.insert(data);
	}

	public int dequeue() {
		if (this.head == null) {
			throw new RuntimeException("EmptyQueueException: Cannot dequeue from an empty queue.");
		}
		
		Link first = this.removeFirst();
		return first.getData();
	}
}
