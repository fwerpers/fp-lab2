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

(* inter s1 s2
   TYPE: int list -> int list -> int list
   PRE: true
   POST: returns the intersection of s1 and s2
*)
fun inter s1 s2 = []

(* inter s1 s2
   TYPE: int list -> int list -> int list
   PRE: true
   POST: returns the intersection of s1 and s2
*)
fun inter' s1 s2 = []

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
