(* 1. Iota *)

(* iota n
   TYPE: int -> int list
   PRE: n > 1
   POST: integers 0, 1, ... n-1
   EXAMPLE: iota 4 = [0, 1, 2, 3]
*)
fun iota N =
    let
        (* naturalInterval m n
     	   TYPE: int*int -> int list
     	   PRE: true
     	   POST: all integers of the interval [m, n] in order
     	   EXAMPLE: naturalInterval 0 4 = [0, 1, 2, 3, 4]
        *)
     	(* Similar to function 'upto' from Paulson: ML for the working programmer *)
        (* VARIANT: max(n - m, 0) *)
     	fun naturalInterval (m, n) =
     	    if m > n then [] else m :: (naturalInterval (m+1, n))
    in
 	    naturalInterval(0, N - 1)
    end

(* 2. Intersection *)

(* member x l
   TYPE: ''a -> ''a list -> bool
   PRE: true
   POST: returns true if x is a member of l, otherwise false
   EXAMPLES: member 4 [1, 2, 3] = false
*)
(* VARIANT: length of l *)
fun member x [] = false
  | member x (y::ys) = x=y orelse (member x ys);

(* inter s1 s2
   TYPE: ''a list -> ''a list -> ''a list
   PRE: elements of s1 and s2 are not repeated
   POST: intersection between s1 and s2
   EXAMPLES: inter [12, 8, 4, 1] [15, 12, 9, 3, 1] = [12, 1]
*)
(* VARIANT: length of s1 *)
fun inter [] s2 = []
  | inter (elem::r1) s2 = if member elem s2 then elem :: (inter r1 s2) else inter r1 s2

(* inter' s1 s2
TYPE: ''a list -> ''a list -> ''a list
PRE: elements of s1 and s2 are not repeated and ordered (ascending)
POST: intesection between s1 and s2
EXAMPLES: inter' [1, 4, 8, 12] [1, 3, 9, 12, 15] = [1, 12]
*)
(* VARIANT: length of s1 + length of s2 *)
fun inter' s1 [] = []
  | inter' [] s2 = []
  | inter' (elem1::r1) (elem2::r2) =
  if elem1 = elem2 then
    elem1 :: (inter' r1 r2)
  else if elem1 < elem2 then
    inter' r1 (elem2::r2)
  else
    inter' (elem1::r1) r2

(* 3. Fruit *)
(* REPRESENTATION CONVENTION: amount of fruit relevant for pricing
   REPRESENTATION INVARIANT: amounts should be non-negative
*)
datatype fruit = Apple of real | Banana of real | Lemon of int

(* sumPrice f a b l
   TYPE: fruit list -> real -> real -> real -> real
   PRE: a, b, l > 0
   POST: the cost of all fruit in f
   EXAMPLE: sumPrice [Apple 1.5, Banana 0.5, Lemon 5] 1.0 2.0 3.0 = 17.5
*)
fun sumPrice fruit_list apple_price banana_price lemon_price =
let
  (* sum basket
     TYPE: fruit list -> real
     PRE: true
     POST: summed price of fruit list
     EXAMPLE: as above
  *)
  (* VARIANT: number of fruits in basket *)
  fun sum [] = 0.0
    | sum (Apple x::xs) = x*apple_price + sum xs
    | sum (Banana x::xs) = x*banana_price + sum xs
    | sum (Lemon x::xs) = real(x)*lemon_price + sum xs
in
  sum fruit_list
end;

(* 4. Trees *)
(* REPRESENTATION CONVENTION: generic
   REPRESENTATION INVARIANT: none
*)
datatype 'a ltree = Node of 'a * 'a ltree list;

(* count tree
   TYPE: 'a ltree -> int
   PRE: true
   POST: the number of nodes in the tree
   EXAMPLE: count Node ("hej", []) = 1;
*)
(* VARIANT: number of nodes in the tree *)
fun count (Node (label, [])) = 1
  | count (Node (label, (c::cs))) = (count c) + count (Node (label, cs));

(* labels tree
   TYPE: 'a ltree -> 'a list
   PRE: true
   POST: list of labels in the tree
   EXAMPLE: labels Node ("hej", []) = ["hej"];
*)
(* VARIANT: count tree *)
fun labels (Node (label, [])) = [label]
  | labels (Node (label, (c::cs))) = labels c @ labels (Node (label, cs));

(* is_present tree x
   TYPE: 'a ltree -> 'a -> bool
   PRE: true
   POST: returns true if x is a label in tree, otherwise false
   EXAMPLE: is_present Node ("hej", []) "hej" = true;
*)
(* VARIANT: count tree *)
fun is_present tree x =
let
  fun helper (Node (label, [])) = if label=x then true else false
    | helper (Node (label, (c::cs))) = helper c orelse helper (Node (label, cs))
in
  helper tree
end;

(* height tree
   TYPE: 'a ltree -> int
   PRE: true
   POST: returns the height of the tree
   EXAMPLE: height Node ("hej", []) = 1;
*)
(* VARIANT: count tree *)
fun height (Node (label, [])) = 1
  | height (Node (label, (c::cs))) = 1 + Int.max((height c), height (Node (label, cs)) - 1);
