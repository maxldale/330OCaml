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

let rec list_length lst = 
	match lst with
	| [] -> 0
	| head :: tail -> 1 + list_length tail
;;

let list_head lst = 
	match lst with
	| [] -> failwith "Empty list!"
	| head :: tail -> head
;;


let list_tail lst = 
	match lst with
	| [] -> []
	| head :: tail -> tail
;;

let second_element lst =
	let length = list_length lst in
	if length < 2 then
		-1
	else begin
		let t = list_tail lst in
		let h = list_head t in
		h
	end
;;

let rec max_first_n i lst = 
	let length = list_length lst in
	if i <= 0 || length <= 0 then
		-1
	else begin
		let head = list_head lst in
		if i=1 then
			head
		else begin
			let iOneLess = i - 1 in
			let tail = list_tail lst in
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

let rec partial_sum i lst = 
	let length = list_length lst in
	if length <= 0 then
		0
	else begin
		if i < 0 then
			0
		else begin
			let head = list_head lst in
			let tail = list_tail lst in
			let iOneLess = i - 1 in
			head + (partial_sum iOneLess tail)
		end
	end
;;

let rec partial_sums is lst = 
	let length = list_length is in
	if length <= 0 then
		[]
	else begin
		let head = list_head is in
		let tail = list_tail is in
		(partial_sum head lst) :: (partial_sums tail lst)
	end
;;

let rec zip lst1 lst2 = 
	let length1 = list_length lst1 in
	let length2 = list_length lst2 in
	if length1 <= 0 || length2 <= 0 then
		[]
	else begin
		let head1 = list_head lst1 in
		let head2 = list_head lst2 in
		let tail1 = list_tail lst1 in
		let tail2 = list_tail lst2 in
		(head1, head2) :: (zip tail1 tail2)
	end
;;

let rec index_help x lst curr = 
	let length = list_length lst in
	if length <= 0 then
		-1
	else begin
		let head = list_head lst in
		if x=head then
			curr
		else begin
			let tail = list_tail lst in
			let currOneMore = curr + 1 in
			index_help x tail currOneMore
		end
	end
;;

let rec index x lst = 
	index_help x lst 0
;;

(****************)
(* Part 4: Sets *)
(****************)

let rec elem x a = 
	let length = list_length a in
	if length <= 0 then
		false
	else begin
		let head = list_head a in
		if x=head then
			true
		else begin
			let tail = list_tail a in
			elem x tail
		end
	end
;;

let rec insert x a = 
	let exists = elem x a in
	if exists then
		a
	else begin
		let length = list_length a in
		if length <= 0 then
			x :: a
		else begin
			let head = list_head a in
			if x < head then
				x :: a
			else begin
				let tail = list_tail a in
				head :: (insert x tail)
			end
		end
	end
;;

let rec subset a b = 
	let lengthA = list_length a in
	if lengthA <=0 then
		true
	else begin
		let lengthB = list_length b in
		if lengthB <= 0 then
			false
		else begin
			let headA = list_head a in
			let headB = list_head b in
			let tailA = list_tail a in
			let tailB = list_tail b in
			if headA = headB then
				subset tailA tailB
			else begin
				if headA > headB then
					subset a tailB
				else begin
					false
				end
			end
		end
	end
;;

let rec eq a b = 
	a=b
;;

let rec remove x a = 
	let exists = elem x a in
	if exists then
		let head = list_head a in
		let tail = list_tail a in
		if x = head then
			tail
		else begin
			head :: (remove x tail)
		end
	else begin
		a
	end
;;

let rec union a b = 
	let lengthA = list_length a in
	let lengthB = list_length b in
	if lengthA <= 0 then
			b
	else begin
		if lengthB <= 0 then
			a
		else begin
			let headA = list_head a in
			let headB = list_head b in
			let tailA = list_tail a in
			let tailB = list_tail b in
			if headA = headB then
				headA :: (union tailA tailB)
			else begin
				if headA < headB then
					headA :: (union tailA b)
				else begin
					headB :: (union a tailB)
				end
			end
		end
	end
;;

let rec intersection a b = 
	let lengthA = list_length a in
	let lengthB = list_length b in
	if lengthA <= 0 || lengthB <= 0 then
		[]
	else begin
		let headA = list_head a in
		let headB = list_head b in
		let tailA = list_tail a in
		let tailB = list_tail b in
		if headA = headB then
			headA :: (intersection tailA tailB)
		else begin
			if headA < headB then
				intersection tailA b
			else begin
				intersection a tailB
			end
		end
	end
;;

let rec product_help x b = 
	let lengthB = list_length b in
	if lengthB <= 0 then
		[]
	else begin
		let headB = list_head b in
		let tailB = list_tail b in
		(x, headB) :: (product_help x tailB)
	end
;;

let rec append a b = 
	let lengthB = list_length b in
	if lengthB <= 0 then
		a
	else begin
		let lengthA = list_length a in
		if lengthA <= 0 then
			b
		else begin
			let headA = list_head a in
			let tailA = list_tail a in
			headA :: append tailA b
		end
	end
;;

let rec product a b = 
	let lengthA = list_length a in
	if lengthA <= 0 then
		[]
	else begin
		let headA = list_head a in
		let tailA = list_tail a in
		let partialProduct = product_help headA b in
		let restOfProduct = product tailA b in
		append partialProduct restOfProduct
	end
;;

