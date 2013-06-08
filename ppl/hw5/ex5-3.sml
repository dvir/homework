
(******************* 3.1 *************************)
(*	Write your datatype definition here... *)

(******************* 3.2 *************************)
(*
* Signature: generic_map (proc, items)
* Purpose: Same as map defined in class, but items can be either a regular list or a lazy list.
* Type: fn: ('a -> 'b) * 'a generic_list -> 'b generic_list	
* Examples:   
 - generic_map(fn(x)=>x+10, List([1,2,3]));
 val it = List[12,13,14]: int generic_list
 - generic_map(fn(x)=>x+10, Seq(Cons(1, fn()=> Cons(2, fn()=> Cons (3, fn()=>Nil)))));
 val it = Seq (Cons (11,fn)) : int generic_list
*)
(*	Write your code here... *)	 

(******************* 3.3 *************************)
(*
* Signature: generic_interleave (g_lst1, g_lst2)
* Purpose: interleave the two generic lists into a single generic list containing all the elements in the following order.
		   The resulting generic list will consist of the first element from g_lst1, followed by the first element from g_lst2,
           followed by the second element from g_lst1, followed by the second element from g_lst2, etc.
		   Starting from a point, if exists, in which all the elements of g_lst1 (or g_lst2, respectively) appear in the result, 
		   the result generic list will consist of the elements of the second list g_lst2 (or g_lst1, respectively).
		   If both of the lists are finite, then the result is finite.
		   If either g_lst1 or g_lst2 are of the form Seq(s), then the result is also of this form.
* Type: fn: 'a generic_list * 'a generic_list -> 'a generic_list	
* Example:  
	- generic_interleave(List([1,2,3]), List([10,20,30,40,50]));
	val it = List [1,10,2,20,3,30,40,50] : int generic_list
	- generic_interleave(Seq(Cons(1,fn()=>Cons(2,fn()=>Cons(3,fn()=>Nil)))), List([10,20,30,40,50]));
	val it = Seq (Cons (1,fn)) : int generic_list
	- generic_interleave(Seq(Cons(1,fn()=>Cons(2,fn()=>Cons(3,fn()=>Nil)))), 
						 Seq(Cons(10,fn()=>Cons(20,fn()=>Cons(30,fn()=>Nil)))));
	val it = Seq (Cons (1,fn)) : int generic_list
	- generic_interleave(List([1,2,3]), Seq(Cons(10,fn()=>Cons(20,fn()=>Cons(30,fn()=>Nil)))));
	val it = Seq (Cons (1,fn)) : int generic_list
	hint: You can apply the "take" procedure on these lazy lists to see their values.
*)
