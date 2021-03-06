(* use("./ex5-aux.sml"); *)

(******************* 3.1 *************************)
datatype 'a generic_list = List of 'a list | Seq of 'a seq;


(******************* 3.2 *************************)
(*
* Signature: generic_map (proc, items)
* Purpose: Same as map defined in class, but items can be either a regular list or a lazy list.
* Type: fn: ('a -> 'b) * 'a generic_list -> 'b generic_list
* Examples:
 - generic_map(fn(x)=>x+10, List([1,2,3]));
 val it = List[11,12,13]: int generic_list
 - generic_map(fn(x)=>x+10, Seq(Cons(1, fn()=> Cons(2, fn()=> Cons (3, fn()=>Nil)))));
 val it = Seq (Cons (11,fn)) : int generic_list
*)
val rec generic_map =
  fn (_, List([])) => List([])
   | (_, Seq(Nil)) => Seq(Nil)
   | (f, List(l)) => List(map f l)
   | (f, Seq(l)) =>
       let val rec seq_map =
             fn (_, Nil) => Nil
              | (f1, Cons(hd, tl)) => Cons (f1(hd), fn () => seq_map(f1, tl()));
       in Seq(seq_map (f, l))
       end;


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
val rec interleave_lists =
  fn (l1, []) => l1
   | ([], l2) => l2
   | (hd1::tl1, hd2::tl2) => [hd1,hd2] @ interleave_lists(tl1,tl2);

val rec interleave_seqs =
  fn (s1, Nil) => s1
   | (Nil, s2) => s2
   | (Cons(hd1,tl1), Cons(hd2,tl2)) => Cons(hd1, fn () => Cons(hd2, fn () => interleave_seqs(tl1(),tl2())));

val rec interleave_list_seq =
  fn ([], Nil) => Nil
   | ([], Cons(hd,tl)) => Cons(hd, fn () => interleave_list_seq([], tl()))
   | (hd::tl, Nil) => Cons(hd, fn () => interleave_list_seq(tl, Nil))
   | (hd1::tl1, Cons(hd2,tl2)) => Cons(hd1, fn () => Cons(hd2, fn () => interleave_list_seq(tl1,tl2())));

val interleave_seq_list =
  fn (Nil, []) => Nil
   | (Nil, l) => interleave_list_seq(l, Nil)
   | (Cons(hd1,tl1), l) => Cons(hd1, fn () => interleave_list_seq(l,tl1()));

val generic_interleave =
  fn (List(l1), List(l2)) => List(interleave_lists(l1,l2))
    | (Seq(s1), Seq(s2)) => Seq(interleave_seqs(s1,s2))
    | (List(l), Seq(s)) => Seq(interleave_list_seq(l,s))
    | (Seq(s), List(l)) => Seq(interleave_seq_list(s,l));
