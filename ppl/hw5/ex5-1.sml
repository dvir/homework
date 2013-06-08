
(******************* 1.1 *************************)
(*
* Signature: get_all_vars(prop)
* Purpose: takes a propositional formula as argument and returns a list of all variables (without duplications) in it
* Type: fn : prop -> string list
* Example: 
- get_all_vars(Disj(Conj (Atom ("x1"), Atom ("x2")), Disj (Atom ("x1"), Neg (Atom ("x3")))));
val it = ["x2","x1","x3"]: string list
*)
(*	Write your code here... *)

(******************* 1.2 *************************)
(*
* Signature: satisfies(formula, assignment)
* Purpose: returns true if and only if the assignment satisfies the formula
* Type: fn : prop * prop list -> bool
* Pre-Condition: assignment is a proper assignment, and get_all_vars(formula)=get_all_vars(assignment).
* Examples: 
 - satisfies(Atom("x1"), [Atom("x1"), Neg(Atom("x3"))]);
 val it = true : bool
 - satisfies(Atom("x3"), [Atom("x1"), Neg(Atom("x3"))]);
 val it = false : bool
 - satisfies(Neg(Atom("x1")), [Atom("x1"), Neg(Atom("x3"))]);
 val it = false : bool
 - satisfies(Neg(Atom("x3")), [Atom("x1"), Neg(Atom("x3"))]);
 val it = true : bool
 - satisfies(Conj(Atom("x1"),Neg(Atom("x3"))), [Atom("x1"), Neg(Atom("x3"))]);
 val it = true : bool
 - satisfies(Conj(Atom("x1"),Atom("x3")), [Atom("x1"), Neg(Atom("x3"))]);
 val it = false : bool
 - satisfies(Disj(Atom("x1"),Atom("x3")), [Atom("x1"), Neg(Atom("x3"))]);
 val it = true : bool
 - satisfies(Disj(Conj(Atom("x1"),Atom("x3")),Conj(Atom("x1"),Neg(Atom("x3")))), [Atom("x1"), Neg(Atom("x3"))]);
 val it = true : bool
 - satisfies(Disj(Conj(Disj(Atom("x1"),Neg(Atom("x3"))),Atom("x3")),Conj(Atom("x1"),Neg(Atom("x3")))), 
             [Atom("x1"), Neg(Atom("x3"))]);
 val it = true : bool
*)
(*	Write your code here... *)
