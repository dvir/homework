
(******************* 1.1 *************************)
(*
* Signature: get_all_vars(prop)
* Purpose: takes a propositional formula as argument and returns a list of all variables (without duplications) in it
* Type: fn : prop -> string list
* Example: 
- get_all_vars(Disj(Conj (Atom ("x1"), Atom ("x2")), Disj (Atom ("x1"), Neg (Atom ("x3")))));
val it = ["x2","x1","x3"]: string list
*)
val load = fn(file_name) => use("./" ^ file_name ^ ".sml");
load "ex5-aux";

val rec merge = fn (lst1, []) => lst1
               | (lst1, h::lst2) => if (List.exists (fn x => x = h) lst1)
                                      then merge(lst1, lst2)
                                      else merge(lst1 @ [h], lst2);

val rec get_all_vars = fn Atom(var) => [var]
                      | Disj(exp1, exp2) => merge(get_all_vars(exp1), get_all_vars(exp2))
                      | Conj(exp1, exp2) => merge(get_all_vars(exp1), get_all_vars(exp2))
                      | Neg(exp) => get_all_vars(exp);

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

val rec satisfies = fn (Atom(var), assignment) => (List.exists (fn x => x = Atom(var)) assignment)
                     | (Neg(formula), assignment) => not(satisfies(formula, assignment))
                     | (Conj(formula1, formula2), assignment) => satisfies(formula1, assignment) andalso satisfies(formula2, assignment)
                     | (Disj(formula1, formula2), assignment) => satisfies(formula1, assignment) orelse satisfies(formula2, assignment); 
