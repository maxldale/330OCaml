(*
  Discussion 3 problems
  Upload this to the submit server by Friday 16th, 11:59 PM.
  It's graded for correctness.
*)

(*
  Implement the functions WITHOUT map and foldl.
*)

(*
  name: add_x
  type: int -> int list -> int list
  desc: adds x to the elements of l and returns a list w/ the results.
  e.g.: add_x 2 [1; 2; 3] -> [3; 4; 5]
*)

let rec add_x n l = 
	let length = List.length l in
	if length <= 0 then
		[]
	else begin
		let head = List.hd l in
		let tail = List.tl l in
		(head + n) :: (add_x n tail)
	end
;;

(*
  name: concat_strings
  type: string list -> int string
  desc: concatenates the elements of l.
  e.g.: concat_strings ["bulbasaur"; "squirtle"; "charmander"] ->
        "bulbasaursquirtlecharmander"
*)

let rec concat_strings l = 
	let length = List.length l in
	if length <= 0 then
		""
	else begin
		let head = List.hd l in
		let tail = List.tl l in
		head ^ (concat_strings tail)
	end
;;

(*
  name: firsts
  type: ('a * 'b) list -> 'a list
  desc: gets the first part of each pair and returns a list w/ the results.
  e.g.: firsts [(1, 2); (3, 4); (5, 6)] -> [1; 3; 5]

  hint: You can deconstruct a tuple w/
        (1) pattern matching: match (1, a) with (fst, snd) -> ...
        (2) let: let (fst, snd) = (1, a) in ...
*)

let rec firsts l = 
	let length = List.length l in
	if length <= 0 then
		[]
	else begin
		let head = List.hd l in
		let (first, _) = head in
		let tail = List.tl l in
		first :: (firsts tail)
	end
;;

(*
  name: apply_all
  type: 'a -> ('a -> 'a) list -> 'a
  desc: apply all elements (functions) of l to x from left to right.
  e.g.: apply_all 1 [(fun x -> x + 1); (fun x -> x * 2); (fun x -> x * (-1))] -> -4

  hint: Write a tail-recursive function.
*)

let rec apply_all x l = 
	let length = List.length l in
	if length <= 0 then
		x
	else begin
		let head = List.hd l in
		let xNew = (head x) in
		let tail = List.tl l in
		apply_all xNew tail
	end
;;

(*
  name: map
  type: ('a -> 'b) -> 'a list -> 'b list
  desc: map f [a1; ...; an] applies function f to a1, ..., an, and builds the
        list [f a1; ...; f an] with the results returned by f.
  e.g.: map (fun x -> x + 1) [1; 2; 3] -> [2; 3; 4]

  hint: Alter the code in add_x.
*)

let rec map f l = 
	let length = List.length l in
	if length <= 0 then
		[]
	else begin
		let head = List.hd l in
		let tail = List.tl l in
		(f head) :: (map f tail)
	end
;;

