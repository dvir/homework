
(******************* 2.1 *************************)
(*
* Signature: postorder_cps(binaryTree, condition, succ, fail)
* Purpose: Post-order traversal of a binary tree, with a condition applied to each node 
*          and success and failure continuations.
* Type: fn : 'a binary_tree * ('a -> bool) * ('a list -> 'b) * ('a list -> 'b) -> 'b
* Examples:  
 - val is_even = fn n => n mod 2 = 0;
 - val succ = fn succLst => 111 :: succLst;
 - val fail = fn failLst => 222 :: failLst;
 - postorder_cps ((Node(Node(Empty,2,Empty),0,Node(Empty,1,Empty))), is_even, succ, fail);  
 val it = [222,2] : int list
 - postorder_cps ((Node(Node(Empty,2,Empty),0,Node(Empty,4,Empty))), is_even, succ, fail);  
 val it = [111,2,4,0] : int list
 - postorder_cps ((Node(Node(Empty,3,Empty),0,Node(Empty,4,Empty))), is_even, succ, fail);  
 val it = [222] : int list
 - postorder_cps(Node(Node(Empty,2,Empty),0,Node(Node(Empty, 3, Empty),1,Node(Empty, 4, Empty))), is_even, succ, fail);
 val it = [222, 2] : int list
 - postorder_cps(Node(Node(Empty,0,Empty),1,Node(Node(Empty, 2, Empty),3,Node(Empty, 4, Empty))), is_even, succ, fail);
 val it = [222, 0,2,4] : int list
 - postorder_cps(Node(Node(Empty,0,Empty),4,Node(Node(Empty, 6, Empty),8,Node(Empty, 2, Empty))), is_even, succ, fail);
 val it = [111, 0,6,2,8,4] : int list
*)
val rec postorder_cps = fn (Empty, pred, succ, fail) => succ([])
                         | (Node(Empty, x, Empty), pred, succ, fail) => 
                             if pred(x)
                                  then succ([x])
                                  else fail([])
                         | (Node(left, x, right), pred, succ, fail) =>
                             postorder_cps(
                               left,
                               pred,
                               fn y => postorder_cps(
                                          right,
                                          pred,
                                          fn z => postorder_cps(
                                                    Node(Empty, x, Empty),
                                                    pred,
                                                    fn t => succ(y @ z @ t),
                                                    fn t => fail(y @ z)
                                                  ),
                                          fn z => fail(y)
                                       ),
                               fail
                             );

(******************* 2.2 *************************)
(*
* Signature: construct(lst)
* Purpose: Construct an ordered binary tree from its post-traversal list  
* Type: fn : int list -> int binary_tree
* Precondition: The list is a legal traversal of an ordered binary tree
* Example: 
 - construct([0,2,1]);
 val it = Node (Node (Empty,0,Empty),1,Node (Empty,2,Empty)) : int binary_tree
 - construct([0,2,4,3,1]);
 val it = Node (Node (Empty,0,Empty),1, Node (Node (Empty,2,Empty),3,Node (Empty,4,Empty))) : int binary_tree
*)

val rec reverse = fn ([]) => []
                   | (h::lst) => reverse(lst) @ [h];

val rec construct = fn [] => Empty
                     | (h::lst) => if lst = []
                                    then Node(Empty, h, Empty)
                                    else Node(
                                              construct([h]), 
                                              hd(reverse(lst)),
                                              construct(reverse(tl(reverse(lst))))
                                             );

(******************* 2.3 *************************)
(*
* Signature: labeled_n_tree_postorder(lnTree)
* Purpose: Post-order traversal of an labeled_n_tree lnTree
* Type: fn : 'a labeled_n_tree -> 'a list
* Example:  
 - labeled_n_tree_postorder(Leaf "3");
 val it = ["3"] : string list
 - labeled_n_tree_postorder(Branch("1", [Leaf "2", Leaf "5", Leaf "3", Leaf "8"]));
 val it = ["2","5","3","8","1"] : string list
 - labeled_n_tree_postorder(Branch(1,[Leaf 2,Branch(4,[Leaf 5,Leaf 3,Leaf 8])]));
 val it = [2,5,3,8,4,1] : int list
*)

val rec merge = fn (lst1, []) => lst1
               | (lst1, h::lst2) => if (List.exists (fn x => x = h) lst1)
                                      then merge(lst1, lst2)
                                      else merge(lst1 @ [h], lst2);

val rec fold = fn ([], f, init) => init
                | (h::lst, f, init) => f(h, fold(lst, f, init));

val rec map = fn (f, []) => []
                | (f, h::lst) => f(h)::map(f, lst);

val rec labeled_n_tree_postorder = fn 
                                    Leaf(v) => [v]
                                    | Branch(v, tree) =>
                                        fold(map(labeled_n_tree_postorder, tree), merge, []) @ [v];
