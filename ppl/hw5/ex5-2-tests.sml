(*---------------------------------------- Qeustion 2 -----------------------------------------------*)
(*--q2.1--*)
val is_even = fn n => n mod 2 = 0;
val succ = fn succLst => 111 :: succLst;
val fail = fn failLst => 222 :: failLst;

val q2_1test1 = fn() => 
	let
		val res = postorder_cps ((Node(Node(Empty,2,Empty),0,Node(Empty,1,Empty))), is_even, succ, fail);
	in
		res = [222, 2]
	end;	


(*--q2.2--*)	
val q2_2test1 = fn() => 
	let
		val res = construct ([]);
	in
		res = Empty 
	end;	

(*--q2.3--*)	
val q2_3test1 = fn() => 
	let
		val res = labeled_n_tree_postorder(Branch(1,[Leaf 2,Branch(4,[Leaf 5,Leaf 3,Leaf 8])]));
	in
		res = [2,5,3,8,4,1]
	end;	

	
run("2.1", "1", q2_1test1());
run("2.2", "1", q2_2test1());
run("2.3", "1", q2_3test1());
