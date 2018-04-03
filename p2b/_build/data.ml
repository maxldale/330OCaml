open Funs

(***********************)
(* Part 2: Integer BST *)
(***********************)

type int_tree =
    IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
    match t with
      IntLeaf -> IntNode(x,IntLeaf,IntLeaf)
    | IntNode (y,l,r) when x > y -> IntNode (y,l,int_insert x r)
    | IntNode (y,l,r) when x = y -> t
    | IntNode (y,l,r) -> IntNode(y,int_insert x l,r)

let rec int_mem x t =
    match t with
      IntLeaf -> false
    | IntNode (y,l,r) when x > y -> int_mem x r
    | IntNode (y,l,r) when x = y -> true
    | IntNode (y,l,r) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = 
	match t with
	  IntLeaf -> 0
	| IntNode (y,l,r) -> 1 + int_size l + int_size r
;;

let rec int_min t =
	match t with
	  IntLeaf -> raise (Invalid_argument "int_min")
	| IntNode (y,l,r) ->
		begin 
		match l with
		  IntLeaf -> y
		| IntNode (y_l,l_l,r_l) -> int_min l
		end
;;

let rec int_insert_all lst t = 
	fold_right int_insert lst t
;;

let concat_lists lst = 
	let inner_f i_a i_h = i_h :: i_a in
	let outer_f o_a o_h = fold inner_f o_a o_h in
	let rev_concat = fold outer_f [] lst in
	rev rev_concat
;;

let rec int_as_list t = 
	match t with
	  IntLeaf -> []
	| IntNode (y,l,r)-> concat_lists ([int_as_list l;[y];int_as_list r])
;;

let contains_both t x y = 
	(int_mem x t) && (int_mem y t)
;;

let rec int_common_helper t a x y =
	if (contains_both t x y) then
		match t with
		  IntLeaf -> a
		| IntNode (y_n,l,r) -> 
			begin
				let l_res = int_common_helper l y_n x y in
				if l_res < a then
					l_res
				else begin
					int_common_helper r y_n x y
				end
			end
	else begin
		a
	end
;;

let rec int_common t x y = 
	if (contains_both t x y) then
		match t with
		  IntLeaf -> raise (Invalid_argument "int_common")
		| IntNode (y_n,l,r) -> int_common_helper t y_n x y
	else begin
		raise (Invalid_argument "int_common")
	end
;;

(***************************)
(* Part 3: Polymorphic BST *)
(***************************)

type 'a atree =
    Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let rec rec_pinsert x cmpfn tree = 
	match tree with
      Leaf -> Node(x,Leaf,Leaf)
    | Node (y,l,r) when (cmpfn x y) > 0 -> Node (y,l,rec_pinsert x cmpfn r)
    | Node (y,l,r) when (cmpfn x y) = 0 -> tree
    | Node (y,l,r) -> Node(y,rec_pinsert x cmpfn l,r)
;;

let pinsert x t = 
	let (cmpfn, tree) = t in
	(cmpfn, rec_pinsert x cmpfn tree)
;;

let rec rec_pmem x cmpfn tree = 
	match tree with
      Leaf -> false
    | Node (y,l,r) when (cmpfn x y) > 0 -> rec_pmem x cmpfn r
    | Node (y,l,r) when (cmpfn x y) = 0 -> true
    | Node (y,l,r) -> rec_pmem x cmpfn l
;;

let pmem x t = 
	let (cmpfn, tree) = t in
	rec_pmem x cmpfn tree
;;

(*******************************)
(* Part 4: Graphs with Records *)
(*******************************)

type node = int
type edge = { src : node; dst : node; }
type graph = { nodes : int_tree; edges : edge list; }

let empty_graph = {nodes = empty_int_tree; edges = [] }

let add_edge e { nodes = ns; edges = es } =
    let { src = s; dst = d } = e in
    let ns' = int_insert s ns in
    let ns'' = int_insert d ns' in
    let es' = e::es in
    { nodes = ns''; edges = es' }

let add_edges es g = fold (fun g e -> add_edge e g) g es

(* Implement the functions below. *)

let graph_empty g =
	let { nodes = ns; edges = es } = g in
	match ns with
	  IntLeaf -> (
		match es with
		  [] -> true
		| h :: t -> false
	  )
	  | IntNode (y,l,r) -> false
;;

let graph_size g =
	let {nodes = ns; edges = es } = g in
	int_size ns
;;

let is_dst n e =
	let { src = s; dst = d } = e in
	if n = d then
		true
	else begin
		false
	end
;;

let is_src n e = 
	let { src = s; dst = d } = e in
	if n = s then
		true
	else begin
		false
	end
;;

let src_edges n g = 
	let { nodes = ns; edges = es } = g in
	fold (fun a h -> if is_src n h then h :: a else a) [] es
;;

let concat_lists lst = 
	let inner_f i_a i_h = i_h :: i_a in
	let outer_f o_a o_h = fold inner_f o_a o_h in
	let rev_concat = fold outer_f [] lst in
	rev rev_concat
;;

let rec in_list x l = 
	match l with
	  [] -> false
	| h :: t -> if h = x then true else in_list x t
;;

let rec rec_reachable toAdd added queue g ns = 
	let newNs = int_insert toAdd ns in
	let newAdded = toAdd :: added in
	let newEdges = src_edges toAdd g in
	let newDsts = fold (fun a {src=s;dst=d} -> if (in_list d added) || (in_list d queue) then a else d :: a) [] newEdges in
	let newQueue = concat_lists [queue;newDsts] in
	match newQueue with
	  [] -> newNs
	| h :: t ->	rec_reachable h newAdded newQueue g newNs
;;

let reachable n g =
	let { nodes = ns; edges = es } = g in
	if int_mem n ns then
		rec_reachable n [] [] g IntLeaf
	else begin
		IntLeaf
	end
;;

