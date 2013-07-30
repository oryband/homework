use("./ex5-aux.sml");
use("./ex5-1.sml");
use("./ex5-2.sml");
use("./ex5-3.sml");

(*---------------------------------------- Question 1 -----------------------------------------------*)
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

(*----------------------------------------- Run tests -----------------------------------------------*)

val run = fn(q, num, true) => "question " ^ q ^ " - test " ^ num ^ " passed"
        | (q, num, false) => "question " ^ q ^ " - test " ^ num ^ " failed";

run("1.1", "1", q1_1test1());
run("1.1", "2", q1_1test2());
run("1.2", "1", q1_2test1());
run("1.2", "2", q1_2test2());
run("1.2", "3", q1_2test3());
run("2.1", "1", q2_1test1());
run("2.2", "1", q2_2test1());
run("2.3", "1", q2_3test1());
run("3.1", "1", q3_1test1());
run("3.1", "2", q3_1test2());
run("3.2", "1", q3_2test1());
run("3.2", "2", q3_2test2());
run("3.3", "1", q3_3test1());
run("3.3", "2", q3_3test2());
run("3.3", "3", q3_3test3());
