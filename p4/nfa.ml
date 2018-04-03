open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q
type ('q, 's) nfa_t = {
  qs : 'q list; (* Finite set of states *)
  sigma : 's list; (* Finite alphabet *)
  delta : ('q, 's) transition list; (* Transitions *)
  q0 : 'q; (* Start state *)
  fs : 'q list; (* Set of accept states *)
}

(*********************)
(* Utility Functions *)
(*********************)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec fix comp f x0 =
  let next = f x0 in
  if comp x0 next then x0 else fix comp f next

(********************************************)
(* Using my own Sets, unsorted sets are bad *)
(********************************************)

let rec my_set_elem x a = 
	let length = List.length a in
	if length <= 0 then
		false
	else begin
		let head = List.hd a in
		if x=head then
			true
		else begin
			let tail = List.tl a in
			my_set_elem x tail
		end
	end
;;

let rec my_set_insert x a = 
	let exists = my_set_elem x a in
	if exists then
		a
	else begin
		let length = List.length a in
		if length <= 0 then
			x :: a
		else begin
			let head = List.hd a in
			if x < head then
				x :: a
			else begin
				let tail = List.tl a in
				head :: (my_set_insert x tail)
			end
		end
	end
;;

let rec my_set_subset a b = 
	let lengthA = List.length a in
	if lengthA <=0 then
		true
	else begin
		let lengthB = List.length b in
		if lengthB <= 0 then
			false
		else begin
			let headA = List.hd a in
			let headB = List.hd b in
			let tailA = List.tl a in
			let tailB = List.tl b in
			if headA = headB then
				my_set_subset tailA tailB
			else begin
				if headA > headB then
					my_set_subset a tailB
				else begin
					false
				end
			end
		end
	end
;;

let rec my_set_eq a b = 
	a=b
;;

let rec my_set_union a b = 
	let lengthA = List.length a in
	let lengthB = List.length b in
	if lengthA <= 0 then
			b
	else begin
		if lengthB <= 0 then
			a
		else begin
			let headA = List.hd a in
			let headB = List.hd b in
			let tailA = List.tl a in
			let tailB = List.tl b in
			if headA = headB then
				headA :: (my_set_union tailA tailB)
			else begin
				if headA < headB then
					headA :: (my_set_union tailA b)
				else begin
					headB :: (my_set_union a tailB)
				end
			end
		end
	end
;;

let rec my_set_intersection a b = 
	let lengthA = List.length a in
	let lengthB = List.length b in
	if lengthA <= 0 || lengthB <= 0 then
		[]
	else begin
		let headA = List.hd a in
		let headB = List.hd b in
		let tailA = List.tl a in
		let tailB = List.tl b in
		if headA = headB then
			headA :: (my_set_intersection tailA tailB)
		else begin
			if headA < headB then
				my_set_intersection tailA b
			else begin
				my_set_intersection a tailB
			end
		end
	end
;;

(****************)
(* Part 1: NFAs *)
(****************)

let rec filterTransitions state transitions symbol =
	match transitions with
	  [] -> []
	| h_transition :: t_transitions -> 
		begin
			let (start_state, transition_symbol, end_state) = h_transition in
			if state = start_state then
				begin
					let res_opt = (
						begin
							match (symbol, transition_symbol) with
							  (Some(frst), Some(snd)) ->
								begin
									if frst = snd then Some(end_state)
									else None
								end
							| (None, None) -> Some(end_state)
							| _ -> None
						end
					) in
					match res_opt with
					  Some(res) ->
						begin
					  		my_set_insert res (filterTransitions state t_transitions symbol)
					  	end
					  | None -> filterTransitions state t_transitions symbol
				end
			else filterTransitions state t_transitions symbol
		end
;;

let rec filterTransitionsManyStates states transitions symbol = 
	match states with
	  [] -> []
	| h_state :: t_states ->
		begin
			let h_res = (filterTransitions h_state transitions symbol) in
			let t_res = (filterTransitionsManyStates t_states transitions symbol) in
			my_set_union h_res t_res
		end
;;

let move m qs s = 
	filterTransitionsManyStates qs m.delta s
;;

let rec e_closureRec m states =
	let res = (my_set_union states (move m states None)) in
	if res = states then
		states
	else
		e_closureRec m res
;;
		
let e_closure m qs = 
	e_closureRec m qs
;;

let rec checkIfEndState end_states state =
	match end_states with
	  [] -> false
	| h_state :: t_states ->
		begin
			if state = h_state then
				true
			else
				checkIfEndState t_states state
		end
;;

let rec checkIfEndStateMany m states =
	match states with
	  [] -> false
	| h_state :: t_states ->
		begin
			if checkIfEndState m.fs h_state then
				true
			else
				checkIfEndStateMany m t_states
		end
;;

let rec acceptManyChars chars m states =
	let possible_states = (e_closure m states) in
	match chars with
	  [] -> (checkIfEndStateMany m possible_states)
	| h_char :: t_chars ->
		begin
			let new_states = (move m possible_states (Some(h_char))) in
			acceptManyChars t_chars m new_states
		end
;;
	
let accept m str =
	acceptManyChars (explode str) m [m.q0]
;;

let rec lookup_state state_map state =
	match state_map with
	  [] -> None
	| h_state :: t_map ->
		begin
			if h_state = state then
				Some(state)
			else
				lookup_state t_map state
		end
;;

let rec get_end_states final_states state_map =
	match state_map with
	  [] -> []
	| state :: t_s_m ->
		begin
			if ((my_set_intersection state final_states) != []) then
				my_set_insert state (get_end_states final_states t_s_m)
			else
				(get_end_states final_states t_s_m)
		end
;;

let rec convert_nfa m sigma curr_state state_map transitions =
	match sigma with
	  [] -> (transitions, state_map)
	| symbol :: symbols ->
		begin
			let moves = (move m curr_state (Some symbol)) in
			let new_state = (e_closure m moves) in
			if new_state = [] then
				convert_nfa m symbols curr_state state_map transitions
			else begin
				match (lookup_state state_map new_state) with
				  None ->
					begin
						let new_s_m = (Sets.insert new_state state_map) in
						let new_t = (curr_state, (Some symbol), new_state) in
						let new_ts = (my_set_insert new_t transitions) in
						let curr_index_res = convert_nfa m symbols curr_state new_s_m new_ts in
						let (curr_t, curr_s_m) = curr_index_res in
						convert_nfa m m.sigma new_state curr_s_m curr_t
					end
				| Some(existing_s) ->
					begin
						let new_t = (curr_state, (Some symbol), existing_s) in
						let new_ts = (my_set_insert new_t transitions) in
						convert_nfa m symbols curr_state state_map new_ts
					end
			end
		end
;;

let nfa_to_dfa m =
	let start_s = (e_closure m [m.q0]) in
	let init_s_m = [start_s] in
	let init_ts = [] in
	let (dfa_ts, dfa_s_m) = (convert_nfa m
		m.sigma
		start_s
		init_s_m
		init_ts
		) in
	let end_states = (get_end_states m.fs dfa_s_m) in
	{
		qs = dfa_s_m;
		sigma = m.sigma;
		delta = dfa_ts;
		q0 = start_s;
		fs = end_states
	}
;;
