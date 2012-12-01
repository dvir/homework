/**
 * @author Or Elmaliach 301482220 ; Dvir Azulay 200534014
 * This class is a single node in an AVL Tree, modified for the needs of this assignment
 * Each node represents the AVL tree which is rooted in it
 * So we can invoke the search or height methods on any node (sub-tree)
 * but it is ill-advised to use the insert method on anything but the whole tree's root
 * as this will corrupt the BST feature of this AVL tree
 */
public class AVLNode {
	// state
	private int key; // the key in the node (id number in this assignment)
	private AVLNode leftChild = null; // a pointer to this node's left child, if exists
	private AVLNode rightChild = null; // a pointer to this node's right child, if exists
	private AVLNode father = null; // a pointer to this node's father (exists if this is not the root)

	// constructors
	public AVLNode(int id) {
		key = id;
	}

	public AVLNode(int id, AVLNode father) {
		this(id);
		this.father = father;
	}

	// behavior
	/**
	 * Search this AVL Node for the given id
	 * @param id The id to be searched in the tree
	 * @return The number of steps it took to find the id:
	 * 			positive value if the id was found, and negative if it wasn't
	 */
	public int search(int id) {
		return search(id, 0);
	}

	/**
	 * Private recursive method to assist the public search method
	 * @param accumulator A counter for how many steps we've already made
	 */
	private int search(int id, int accumulator) {
		if (key == id) {	//found
			return accumulator + 1;
		}

		if (key > id) {
			if (leftChild != null) {
				return leftChild.search(id, accumulator + 1);	//search left child
			}
		} else {
			if (rightChild != null) {
				return rightChild.search(id, accumulator + 1);	//search right child
			}
		}

		// didn't find the id in the tree
		// NOTE: we add 2 instead of 1 because if we got here we checked two children and got null
		return (-1) * (accumulator + 2);
	}

	/**
	 * Insert a new AVL Node containing the given id to this tree
	 * @param id The id for the new AVL Node to be created
	 * @return The new root of the tree (after rotations)
	 */
	public AVLNode insert(int id) {
		if (key > id) {
			if (leftChild != null) {
				return leftChild.insert(id);	//recursively inserting to left child
			} else {
				leftChild = new AVLNode(id, this);	//create a new node in the place that it should be
			}
		} else {
			if (rightChild != null) {
				return rightChild.insert(id);	//recursively inserting to right child
			} else {
				rightChild = new AVLNode(id, this);	//create a new node in the place that it should be
			}
		}
		
		//if we got here we are in the father of the leaf which we've just inserted
		return balanceAfterInsert();	//balancing the tree
	}

	/**
	 * Private method for balancing the AVL tree after we've made an insertion
	 * This method is invoked from the the father of the leaf which we've just inserted
	 * @return The new root of the tree (after rotations)
	 */
	private AVLNode balanceAfterInsert() {
		int rightChildHeight = 0;
		int leftChildHeight = 0;

		if (leftChild != null) {
			leftChildHeight = leftChild.height();
		}
		if (rightChild != null) {
			rightChildHeight = rightChild.height();
		}

		if (leftChildHeight + 1 < rightChildHeight) {	//this node is unbalanced
			if (rightChild.balanceFactor() == -1) {	//checking if we need a double rotation
				rightChild.rotateRight();
			}

			rotateLeft();
		} else {
			if (leftChildHeight > 1 + rightChildHeight) {	//this node is unbalanced
				if (leftChild.balanceFactor() == 1) {	//checking if we need a double rotation
					leftChild.rotateLeft();
				}

				rotateRight();
			}
		}

		if (father == null) {	//if we got to the root and it's balanced, we return it
			return this;
		}
		
		//if we got here then we need to keep going up, and check for unbalanced nodes
		//until we reach the root and return it
		return father.balanceAfterInsert();
	}

	/**
	 * A private method used in the balancing process of the tree
	 */
	private void rotateLeft() {
		//setting this node's old right child as the new right/left child of this node's old father
		if (father != null) {
			if (father.rightChild == this) {
				father.rightChild = rightChild;
			}
			if (father.leftChild == this) {
				father.leftChild = rightChild;
			}
		}
		rightChild.father = father;

		AVLNode oldRightChild = rightChild;	//temporary pointer

		//setting the left child of the old right child as this node's new right child
		rightChild = rightChild.leftChild;
		if (rightChild != null) {
			rightChild.father = this;
		}

		//setting the old right child as this node's new father
		father = oldRightChild;
		father.leftChild = this;
	}

	/**
	 * A private method used in the balancing process of the tree
	 */
	private void rotateRight() {
		//setting this node's old left child as the new right/left child of this node's old father
		if (father != null) {
			if (father.rightChild == this) {
				father.rightChild = leftChild;
			}
			if (father.leftChild == this) {
				father.leftChild = leftChild;
			}
		}
		leftChild.father = father;

		AVLNode oldLeftChild = leftChild;	//temporary pointer

		//setting the right child of the old left child as this node's new right child
		leftChild = leftChild.rightChild;
		if (leftChild != null) {
			leftChild.father = this;
		}

		//setting the old left child as this node's new father
		father = oldLeftChild;
		father.rightChild = this;
	}

	/**
	 * Get the height of this AVL Node
	 * @return The height of this node; 1 for a leaf node
	 */
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

	/**
	 * Check the balance factor of this node
	 * @return 0 for balanced; 1 for higher right child; -1 for higher left child
	 */
	private int balanceFactor() {
		int rightChildHeight = 0;
		int leftChildHeight = 0;
		if (leftChild != null) {
			leftChildHeight = leftChild.height();
		}
		if (rightChild != null) {
			rightChildHeight = rightChild.height();
		}
		if (rightChildHeight == leftChildHeight) {	// the two children have the same height (or this is a leaf)
			return 0;
		} else {
			if (rightChildHeight > leftChildHeight) { // higher right child
				return 1;
			} else { // higher left child
				return -1;
			}
		}
	}
}