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
fun sumPrice a b c d = 2.0;

(* 4. Trees *)
(* Look at lecture example of binary tree and use a list of children *)
datatype 'a ltree = Node of 'a

fun count x = 3;

fun labels x = [];

fun is_present x y = true;

fun height x = 1;


(*

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
	   PRE: true OR m <= n ???
	   POST: all integers of the interval [m, n] in order
	   EXAMPLE: naturalInterval 0 4 = [0, 1, 2, 3, 4]
        *)
	(* Similar to function upto from Paulson: ML for the working programmer *)
        (* VARIANT: n - m *)
	fun naturalInterval (m, n) = 
	    if m > n then [] else m :: (naturalInterval (m+1, n)) 
    in
	naturalInterval(0, N - 1)
    end

(* 2. Intersection *)

(* member element set
  TYPE: 'a -> 'a list -> bool
  PRE: true
  POST: is the element a member of the set
  EXAMPLES: member 3 [5 43 3 1 ~13] = true
    member 2 [5 43 3 1 ~13] = false
*)
fun member element [] = false
  | member element (head::remainder) =
    if element = head then true else member element remainder

(* inter s1 s2
  TYPE: ''a list -> ''a list -> ''a list
  PRE: elements of s1 and s2 are not repeated
  POST: intersection between s1 and s2
  EXAMPLES: inter [12, 8, 4, 1] [15, 12, 9, 3, 1] = [12, 1]
*)
(* VARIANT: length of s1 *)
fun inter s1 [] = []
  | inter [] s2 = []
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
    if elem1 = elem2 then elem1 :: (inter' r1 r2) 
    else if elem1 < elem2 then inter' r1 (elem2::r2)
    else inter' (elem1::r1) r2 
 
(* 3. Fruit *)

datatype fruit = Apple of real | Banana of real | Lemon of int;

(* sumPrice f a b l
  TYPE: fruit list -> real -> real -> real -> real
  PRE: a, b, l > 0
  POST: the cost of all fruit in f
  EXAMPLE: sumPrice [Apple 1.5, Banana 0.5, Lemon 5] 1.0 2.0 3.0 = 17.5   
*)
fun sumPrice fbasket priceApples priceBananas priceLemons =
    let
	(* sum basket
	   TYPE: fruit list -> real
	   PRE: true
	   POST: summed price of fruit list
	   EXAMPLE: as above
	*)
	(* VARIANT: number of fruits in basket *)
	fun sum [] = 0.0
	    | sum ((Apple a)::remainder) = a * priceApples + sum remainder
	    | sum ((Banana b)::remainder) = b * priceBananas + sum remainder
	    | sum ((Lemon l):: remainder) = real(l) * priceLemons + sum remainder
    in
	sum fbasket
    end

(* 4. Trees *)

datatype 'a ltree = Leaf of 'a
	    	  | Node of 'a * 'a ltree list

(*
  EXAMPLES: Node("root", [Node("branch1", [Leaf("leaf1"), Leaf("leaf2")]), Node("branch2", [Leaf("leaf3"), Leaf("leaf4")])]) = 7
*)
fun count (Leaf l) = 1
  | count (Node (_, siblings)) = 
    let
	fun countSiblings [] = 0
	  | countSiblings (child::[]) = count child
          | countSiblings (child::siblings) = count child + countSiblings siblings
    in
	1 + countSiblings siblings
    end
  
val t = Node("root", [Node("branch1", [Leaf("leaf1"), Leaf("leaf2")]), Node("branch2", [Leaf("leaf3"), Leaf("leaf4")])]);

*)
