(* Setup for interpreter printing *)
Control.Print.printDepth := 100;
Control.Print.printLength := 100;
(* 
* Purpose: represent a formula in propositional logic.
*)
datatype prop = Atom of string
				| Neg of prop
				| Conj of prop * prop
				| Disj of prop * prop;
(*
* Purpose: datatype binary tree. Each node has a left child, a value and a right child. Alternatively, a node can be empty.
*)
datatype 'a binary_tree = Empty | Node of 'a binary_tree * 'a * 'a binary_tree;
(*
* Purpose: datatype labeled n-ary tree. Values are at the leaves and internal nodes. 
*          Internal nodes (brances) can have any number of children.
*)
datatype 'a labeled_n_tree = Leaf of 'a | Branch of 'a * 'a labeled_n_tree list;
(*
* Purpose: a datatype for lazy lists
* Example: 
  - Cons(1, fn()=> (Cons (2, fn()=> Cons (3, fn()=> Nil))));
  val it = Cons(1, fn) : int seq
*)
datatype 'a seq = Nil | Cons of 'a * (unit -> 'a seq);