open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let is_over_x x lst = 
	map (fun a -> a > x) lst
;;

let count_over_x x lst =
	let lstOver = is_over_x x lst in
	fold (fun a b -> if b then a+1 else a) 0 lstOver
;;

let mean lst = failwith "unimplemented"

let pred_succ lst = failwith "unimplemented"

let bind f lst = failwith "unimplemented"

let ap fns args = failwith "unimplemented"
