let rec fold_right f l a =
  match l with
      [] -> a
    | h::t ->
      let a' = fold_right f t a in
      f h a'
;;

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

let rec int_common t x y = 
	if (contains_both t x y) then
		match t with
		  IntLeaf -> raise (Invalid_argument "int_common")
		| IntNode (y,l,r) ->
			begin
				let l_res = (match l with
				  IntLeaf -> y
				| IntNode (y_l,l_l,r_l) -> 
					begin
						if (contains_both l x y) then
							let () = print_string "here" in
							int_common l x y
						else begin
							let () = print_string "there" in
							y
						end
					end) in
				if l_res < y then
					l_res
				else begin
					match r with
					  IntLeaf -> y
					| IntNode (y_r, l_r, r_r) ->
						begin
							if (contains_both r x y) then
								int_common r x y
							else begin
								y
							end
						end
				end
			end
	else begin
		raise (Invalid_argument "int_common")
	end
;;
