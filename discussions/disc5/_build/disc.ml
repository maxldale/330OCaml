(*
  Discussion 5 problems
  Upload this to the submit server by Friday 23rd.
  It's graded for correctness.
*)

(* You can use these functions for the following problems. *)

let rec map f l = 
  match l with
  | [] -> []
  | h :: t -> (f h) :: (map f t)
;;

let rec foldl f acc l = 
  match l with
  | [] -> acc
  | h :: t -> foldl f (f acc h) t
;;

(*
  name: sum_list_list
  type: int list list -> int
  desc: Returns the sum of the ints in the lists in l.

  sum_list_list [[]; [1]; [2; 3]] -> 6
*)
let sum_list l = 
	foldl (fun a h -> a + h) 0 l
;;

let sum_list_list l =
  foldl (fun a h -> a + (sum_list h)) 0 l
;;

(*
  name: full_names
  type: name_record list -> string list
  desc: Returns string representations of the name_records in l.

  full_names [
    { first = "Anwar"; middle = None; last = "Mamat" };
    { first = "Michael"; middle = Some "William"; last = "Hicks" }
  ]
  -> ["Anwar Mamat"; "Michael William Hicks"]
*)

type name_record = { first: string; middle: string option; last: string };;

let full_names l =
  map (fun {first= f; middle=m; last=l} -> begin
  	match m with
  	  Some mdl -> f ^ " " ^ mdl ^ " " ^ l
  	| None -> f ^ " " ^ l
  end) l
;;

(*
  name: sum_vectors
  type: vector list -> vector
  desc: Returns the sum of the vectors in l.

  sum_vectors [{ x = 1; y = 2 }; { x = 3; y = 4 }] -> { x = 4; y = 6 }
*)

type vector = { x: int; y: int };;

let sum_vectors l =
  let (x_sum,y_sum) = foldl (fun (a_x,a_y) {x=h_x;y=h_y} -> (a_x+h_x,a_y+h_y)) (0,0) l in
  {x=x_sum;y=y_sum}
;;

(*
  name: sum_leaves
  type: tree -> int
  desc: Returns the sum of the leaves of t.

  sum_leaves (Node (Leaf 1, Node (Leaf 2, Leaf 3))) -> 6
*)

type tree =
| Node of tree * tree
| Leaf of int

let rec sum_leaves t =
  match t with
    Node (l,r) -> (sum_leaves l) + (sum_leaves r)
  | Leaf y -> y
;;

(* Ungraded optional problem *)

(*
  name: is_balanced
  type: tree -> bool
  desc: Returns whether t is balanced (has uniform height).

  is_balanced (Node (Leaf 1, Node (Leaf 2, Leaf 3))) -> false
  is_balanced (Node (Node (Leaf 1, Leaf 2), Node (Leaf 3, Leaf 4))) -> true
*)

let rec rec_is_balanced lrTuple = 
	match lrTuple with
	  (Leaf x, Leaf y) -> true
	| (Leaf x, Node (l,r)) -> false
	| (Node (l,r), Leaf y) -> false
	| (Node (ll, rl), Node (lr, rr)) -> (rec_is_balanced (ll, lr) && rec_is_balanced (rl, rr))
;;
let is_balanced t =
  match t with
    Leaf y -> true
  | Node (l,r) -> rec_is_balanced (l,r)
;;

