public class AVLNode {
	// state
	private int key;
	private AVLNode leftChild;
	private AVLNode rightChild;
	private AVLNode father;
	private int balanceFactor; // 0 for balanced; 1 for higher right son; -1 for
								// higher left son

	// constructors
	public AVLNode(int id) {
		key = id;
		rightChild = null;
		leftChild = null;
		balanceFactor = 0;
		father = null;
	}

	public AVLNode(int id, AVLNode father) {
		this(id);
		this.father = father;
	}

	// behavior
	public int search(int id) {
		return search(id, 0);
	}

	private int search(int id, int accumulator) {
		if ((key > id) && (leftChild != null)) {
			return leftChild.search(id, accumulator + 1);
		} else {
			if ((key < id) && (rightChild != null)) {
				return rightChild.search(id, accumulator + 1);
			} else {
				if (id == key) {
					return accumulator + 1;
				} else {
					return (-1) * (accumulator + 1);
				}
			}
		}
	}

	public void insert(int id) {
		if (key > id) {
			if (leftChild != null) {
				leftChild.insert(id);
			} else {
				leftChild = new AVLNode(id, this);
				balanceAfterInsert();
			}
		} else {
			if (rightChild != null) {
				rightChild.insert(id);
			} else {
				rightChild = new AVLNode(id, this);
				balanceAfterInsert();
			}
		}
	}

	private void balanceAfterInsert() {
		AVLNode currentNode = this;
		while (currentNode != null) {
			currentNode.fixBalanceFactor();
			int rightChildHeight = 0;
			int leftChildHeight = 0;

			if (leftChild != null) {
				leftChildHeight = leftChild.height();
			}
			if (rightChild != null) {
				rightChildHeight = rightChild.height();
			}

			if (leftChildHeight + 1 < rightChildHeight) {
				if (rightChild.balanceFactor == -1) {
					rightChild.rotateRight();
				}
				rotateLeft();
			} else if (leftChildHeight > 1 + rightChildHeight) {
				if (leftChild.balanceFactor == 1) {
					leftChild.rotateLeft();
				}
				rotateRight();
			}

			currentNode = currentNode.father;
		}
	}

	private void rotateLeft() {
		if (father != null) {
			if (father.rightChild == this) {
				father.rightChild = rightChild;
			}
			if (father.leftChild == this) {
				father.leftChild = rightChild;
			}
		}
		rightChild.father = father;

		rightChild = rightChild.leftChild;
		if (rightChild != null) {
			rightChild.father = this;
		}

		father = rightChild;
		father.leftChild = this;

		if (father.father != null) {
			father.father.fixBalanceFactor();
		}
		father.fixBalanceFactor();
		fixBalanceFactor();
	}

	private void rotateRight() {
		if (father != null) {
			if (father.rightChild == this) {
				father.rightChild = leftChild;
			}
			if (father.leftChild == this) {
				father.leftChild = leftChild;
			}
		}
		
		leftChild.father = father;

		leftChild = leftChild.rightChild;
		if (leftChild != null) {
			leftChild.father = this;
		}

		father = leftChild;
		father.rightChild = this;

		if (father.father != null) {
			father.father.fixBalanceFactor();
		}
		father.fixBalanceFactor();
		fixBalanceFactor();
	}

	public int height() {
		int rightChildHeight = 0;
		int leftChildHeight = 0;
		if (leftChild != null) {
			leftChildHeight = leftChild.height();
		}
		if (rightChild != null) {
			rightChildHeight = rightChild.height();
		}
		return Math.max(rightChildHeight, leftChildHeight) + 1;
	}

	private void fixBalanceFactor() {
		int rightChildHeight = 0;
		int leftChildHeight = 0;
		if (leftChild != null) {
			leftChildHeight = leftChild.height();
		}
		if (rightChild != null) {
			rightChildHeight = rightChild.height();
		}
		if (rightChildHeight == leftChildHeight) {
			balanceFactor = 0;
		} else {
			if (rightChildHeight > leftChildHeight) {
				balanceFactor = 1;
			} else {
				balanceFactor = -1;
			}
		}
	}
}
