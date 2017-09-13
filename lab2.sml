(* 1. Iota *)

(* iota n
   TYPE: int -> int list
   PRE: n >= 0
   POST: returns a list [0,...,n]
*)
(* VARIANT: n *)
fun iota 0 = []
  | iota n = iota (n-1) @ [n-1];

(* 2. Intersection *)

(* member x l
   TYPE: int -> int list -> bool
   PRE: true
   POST: returns true if x is a member of l, otherwise false
*)
fun member (x : int) l = List.exists (fn y => y=x) l;

(* inter s1 s2
   TYPE: int list -> int list -> int list
   PRE: true
   POST: returns the intersection of s1 and s2
*)

(* Modify only one of the lists
For each recursion, check membership of the head
If member, move to tail of the list
Else remove from list
When do we know we're done? *)

fun inter (s1 : int list) (s2 : int list) =
  let
    fun helper [] (s2 : int list) (s : int list) = s
      | helper (x::s1) s2 s = if member x s2 then helper s1 s2 (x::s) else helper s1 s2 s
  in
    helper s1 s2 []
  end;

(* inter s1 s2
   TYPE: int list -> int list -> int list
   PRE: true
   POST: returns the intersection of s1 and s2
*)
fun inter' s1 s2 = [];

(* 3. Fruit *)
datatype fruit = Apple of real | Banana of real | Lemon of int
fun sumPrice a b c d = 2.0;

(* 4. Trees *)
(* Look at lecture example of binary tree and use a list of children *)
datatype 'a ltree = Node of 'a

fun count x = 3;

fun labels x = [];

fun is_present x y = true;

fun height x = 1;
