(******************************)
(* Part 1: Non-List Functions *)
(******************************)

let divides x y = 
	if x=0 then
		false
	else begin
		let r =  y mod x in
		r=0
	end
;;

let rec gcd a b = 
	if a=0 || b=0 then
		max a b
	else begin
		let r = a mod b in
		gcd b r
	end
;;

let rec ack m n = 
	if m=0 then
		n+1
	else begin
		if m > 0 && n=0 then
			let a = m - 1 in
			ack a 1
		else begin
			let a = m - 1 in
			let b = n - 1 in
			let c = ack m b in
			ack a c
		end
	end
;;

(*********************************)
(* Part 2: Simple List Functions *)
(*********************************)

let second_element lst =
	if List.length lst < 2 then
		-1
	else begin
		let t = List.tl lst in
		let h = List.hd t in
		h
	end
;;

let rec max_first_n i lst = 
	if i <= 0 || List.length lst <= 0 then
		-1
	else begin
		let head = List.hd lst in
		if i=1 then
			head
		else begin
			let iOneLess = i - 1 in
			let tail = List.tl lst in
			let maxRest = max_first_n iOneLess tail in
			max head maxRest
		end
	end
;;

let max_first_three lst = 
	max_first_n 3 lst
;;

(************************************)
(* Part 3: Recursive List Functions *)
(************************************)

let rec partial_sum i lst = failwith "partial_sum unimplemented"

let rec partial_sums is lst = failwith "partial_sums unimplemented"

let rec zip lst1 lst2 = failwith "zip unimplemented"

let rec index_help x lst curr = failwith "index_help implement if needed"

let rec index x lst = failwith "index_x unimplemented"

(****************)
(* Part 4: Sets *)
(****************)

let rec elem x a = failwith "elem unimplemented"

let rec insert x a = failwith "insert unimplemented"

let rec subset a b = failwith "subset unimplemented"

let rec eq a b = failwith "eq unimplemented"

let rec remove x a = failwith "remove unimplemented"

let rec union a b = failwith "union unimplemented"

let rec intersection a b = failwith "intersection unimplemented"

let rec product_help x b = failwith "product_help implement if needed"

let rec product a b = failwith "product unimplemented"
