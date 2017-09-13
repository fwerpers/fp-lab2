(* 1. Iota *)

(* iota n
   TYPE: int -> int list
   PRE: n >= 0
   POST: returns a list [0,...,n]
*)
(* VARIANT: n *)
fun iota' 0 = []
  | iota' n = iota' (n-1) @ [n-1];

(* This is a lot faster for some reason *)
fun iota n =
  let
    fun helper 0 = []
      | helper n = (n-1) :: helper (n-1)
  in
    rev (helper (n))
  end;

(* 2. Intersection *)

(* member x l
   TYPE: int -> int list -> bool
   PRE: true
   POST: returns true if x is a member of l, otherwise false
*)
fun member x l = List.exists (fn y => y=x) l;

fun member' x [] = false
  | member' x (y::ys) = x=y orelse (member' x ys);

(* inter s1 s2
   TYPE: int list -> int list -> int list
   PRE: true
   POST: returns the intersection of s1 and s2
*)
fun inter s1 s2 =
  let
    fun helper [] s = s
      | helper (x::s1) s = if member x s2 then helper s1 (x::s) else helper s1 s
  in
    helper s1 []
  end;

(* inter s1 s2
   TYPE: int list -> int list -> int list
   PRE: true
   POST: returns the intersection of s1 and s2
*)
fun inter' s1 s2 =
  let
    fun helper [] s2 s = s
      | helper s1 [] s = s
      | helper (x::s1) (y::s2) s = 
        if x < y then
          helper s1 (y::s2) s
        else if x=y then
          helper s1 s2 (x::s)
        else
          helper (y::s2) (x::s1) s
  in
    rev (helper s1 s2 [])
  end;

(* 3. Fruit *)
datatype fruit = Apple of real | Banana of real | Lemon of int

fun sumPrice fruit_list apple_price banana_price lemon_price =
let
  fun sum [] = 0.0
    | sum (Apple x::xs) = x*apple_price + sum xs
    | sum (Banana x::xs) = x*banana_price + sum xs
    | sum (Lemon x::xs) = Real.fromInt(x)*lemon_price + sum xs
in
  sum fruit_list
end;

(* 4. Trees *)
(* Look at lecture example of binary tree and use a list of children *)
(* datatype 'a ltree = Leaf of 'a
                  | Node of 'a * 'a ltree list; *)

datatype 'a ltree = Node of 'a * 'a ltree list;

(* fun count (Node (node, [])) = 1; *)

fun count (Node (label, [])) = 1
  | count (Node (label, (c::cs))) = (count c) + count (Node (label, cs));

fun labels (Node (label, [])) = [label]
  | labels (Node (label, (c::cs))) = labels c @ labels (Node (label, cs));

fun is_present tree label = true;

fun height x = 1;
