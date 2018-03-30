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
					  		res :: (filterTransitions state t_transitions symbol)
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
			List.append h_res t_res
		end
;;

let move m qs s = 
	filterTransitionsManyStates qs m.delta s
;;

let rec e_closureManyStates states transitions =
	match states with
	  [] -> []
	| h_state :: t_states ->
		begin
			let h_res = (h_state :: (filterTransitions h_state transitions None)) in
			let t_res = (e_closureManyStates t_states transitions) in
			List.append h_res t_res
		end
;;
		
let e_closure m qs = 
	e_closureManyStates qs m.delta
;;

let rec acceptManyChars chars transitions =
	(* TODO finish this function *)
;;
	
let accept m str =
	acceptManyChars (explode str) m.delta
;;

let nfa_to_dfa m = failwith "unimplemented"
