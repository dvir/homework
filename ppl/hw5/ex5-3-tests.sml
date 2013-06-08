(*---------------------------------------- Qeustion 3 -----------------------------------------------*)


exception Subscript;

val rec generic_take = fn (seq, 0) => []
				| (Seq(Nil), n) => raise Subscript
				| (Seq(Cons (h, tl)), n) => h :: generic_take( Seq(tl()), n-1)
				| (List([]), n) => raise Subscript
				| (List(h::t), n) => h :: generic_take(List(t), n-1);

(*--q3.1--*)
val q3_1test1 = fn() => 
	let
		val res = List([1,2,3]);
	in
		generic_take(res, 3) = [1,2,3]
	end;	

val q3_1test2 = fn() => 
	let
		val res = Seq(Cons(1, fn()=>Cons(2, fn()=> Cons (3, fn()=>Nil))));
	in
		generic_take(res, 3) = [1,2,3]
	end;	

(*--q3.2--*)
val q3_2test1 = fn() => 
	let
		val res = generic_map(fn(x)=>x+1, List([1,2,3]));
	in
		generic_take(res, 3) = [2,3,4]
	end;	


val q3_2test2 = fn() => 
	let
		val res = generic_map(fn(x)=>x+1, Seq(Cons(1, fn()=>Cons(2, fn()=> Cons (3, fn()=>Nil)))));
	in
		generic_take(res, 3) = [2,3,4]
	end;	
	
(*--q3.3--*)
val q3_3test1 = fn() => 
	let
		val res = generic_interleave(List([1,2,3]), List([10,20,30,40,50]));
	in
		generic_take(res, 8) = [1, 10, 2, 20, 3, 30, 40, 50]
	end;	

val rec ones = fn() => Cons(1, ones);
val rec int_from = fn(n) => Cons(n, fn()=>int_from(n+1));

val q3_3test2 = fn() => 
	let
		val res = generic_interleave(Seq(ones()), Seq(int_from(0)));
	in
		generic_take(res, 10) = [1,0,1,1,1,2,1,3,1,4]
	end;	

val q3_3test3 = fn() => 
	let
		val res = generic_interleave(Seq(int_from(1)), List[1,2,3]);
	in
		generic_take(res, 10) = [1,1,2,2,3,3,4,5,6,7]
	end;	

run("3.1", "1", q3_1test1());
run("3.1", "2", q3_1test2());
run("3.2", "1", q3_2test1());
run("3.2", "2", q3_2test2());
run("3.3", "1", q3_3test1());
run("3.3", "2", q3_3test2());
run("3.3", "3", q3_3test3());
