open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let is_over_x x lst = 
	map (fun a -> a > x) lst
;;

let count_over_x x lst =
	let lstOver = is_over_x x lst in
	fold (fun a b -> if b then (a + 1) else a) 0 lstOver
;;

let mean lst = 
	let size = float_of_int (length lst) in
	if size = 0. then
		raise (Invalid_argument "mean")
	else begin
		let sum = fold (fun a b -> a +. b) 0. lst in
		let res = (sum /. size) in
		res
	end
;;

let concat_lists lst = 
	let inner_f i_a i_h = i_h :: i_a in
	let outer_f o_a o_h = fold inner_f o_a o_h in
	let rev_concat = fold outer_f [] lst in
	rev rev_concat
;;

let pred_succ lst =
	let mapped = map (fun a -> [a-1;a;a+1]) lst in
	concat_lists mapped
;;

let bind f lst = 
	let mapped = map (fun a -> f a) lst in
	concat_lists mapped
;;

let ap fns args = 
	let rev_args = rev args in
	let folded = fold (fun a f -> (map f rev_args) :: a) [] fns in
	let rev_ap = concat_lists folded in
	rev rev_ap
;;
