(*----------------------------------------- Run tests -----------------------------------------------*)

val run = fn(q, num, true) => "question " ^ q ^ " - test " ^ num ^ " passed"
		| (q, num, false) => "question " ^ q ^ " - test " ^ num ^ " failed";
