(* use("./ex5-aux.sml"); *)

(******************* 1.1 *************************)
(*
* Signature: get_all_vars(prop)
* Purpose: takes a propositional formula as argument and returns a list of all variables (without duplications) in it
* Type: fn : prop -> string list
* Example:
- get_all_vars(Disj(Conj (Atom ("x1"), Atom ("x2")), Disj (Atom ("x1"), Neg (Atom ("x3")))));
val it = ["x2","x1","x3"]: string list
*)
val rec get_all_vars = fn (p : prop) =>
  let val rec remove_duplicates =
        fn (l1, []) => l1
         | (l1, head::l2) => if (List.exists (fn x => x=head) l1)
                             then remove_duplicates(l1, l2)
                             else remove_duplicates(l1 @ [head], l2);
  val rec helper =
    fn (Atom(p), l) => remove_duplicates(l, [p])
     | (Neg(p), l) => remove_duplicates(l, helper(p, l))
     | (Conj(p1,p2), l) => remove_duplicates(helper(p1, l), helper(p2, l))
     | (Disj(p1,p2), l) => remove_duplicates(helper(p1, l), helper(p2, l));

  in helper(p, [])
  end;

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
val rec satisfies =
  fn (Atom(f), a : prop list) => (List.exists (fn x => x=Atom(f)) a)
   | (Neg(f), a : prop list) => not(satisfies(f, a))
   | (Conj(f1,f2), a : prop list) => satisfies(f1, a) andalso satisfies(f2, a)
   | (Disj(f1,f2), a : prop list) => satisfies(f1, a) orelse satisfies(f2, a);
