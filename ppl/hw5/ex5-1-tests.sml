(*---------------------------------------- Qeustion 1 -----------------------------------------------*)
(*--q1.1--*)
val q1_1test1 = fn () => 
	let 
		val f = Conj (Disj (Atom ("x1"), Atom ("x2")),Disj (Atom ("x1"), Neg (Atom ("x3"))))
		val res = get_all_vars(f)
	in  
			res = ["x1","x2","x3"] orelse
			res = ["x1","x3","x2"] orelse
			res = ["x2","x1","x3"] orelse
			res = ["x2","x3","x1"] orelse
			res = ["x3","x1","x2"] orelse
			res = ["x3","x2","x1"]
	end;
 
val q1_1test2 = fn() => 
	let 
		val f = Conj (Disj (Atom ("x1"), Atom ("x2")),Disj (Atom ("x1"), Neg (Atom ("x2"))))
		val res = get_all_vars(f)
	in  
			res = ["x1", "x2"] orelse
			res = ["x2", "x1"]
 	end;

 
(*--q1.2--*) 
val q1_2test1 = fn() => 
	let
		val formula = Conj (Disj (Atom ("x1"), Atom ("x2")), Disj (Atom ("x1"), Neg (Atom ("x3"))))
		val assignment =  [Atom("x1"), Atom("x2"), Atom("x3")]  
	in
		satisfies(formula, assignment) 
	end;	


val q1_2test2 = fn() => 
	let
		val formula = Conj (Disj (Atom ("x1"), Atom ("x2")), Disj (Atom ("x1"), Neg (Atom ("x3"))))
		val assignment = [Neg(Atom("x1")), Neg(Atom("x2")), Atom("x3")]  
	in
		satisfies(formula, assignment) = false
	end;	

val q1_2test3 = fn() => 
	let
		val formula = Disj (Neg (Conj (Disj (Neg (Atom "x1"), Atom "x2"),Neg (Conj (Atom "x3",Atom "x2" ) ) ) ) ,Disj (Neg (Atom "x1"), Neg (Atom "x3")))
		val assignment = [Neg(Atom("x1")), Atom("x2"), Atom("x3")]
	in
		satisfies(formula, assignment)
	end;	

run("1.1", "1", q1_1test1());
run("1.1", "2", q1_1test2());
run("1.2", "1", q1_2test1());
run("1.2", "2", q1_2test2());
run("1.2", "3", q1_2test3());
