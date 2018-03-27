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
let rec filterTransitionsManyStates states transitions symbol = 
	match states with
	  [] -> []
	| h_state :: t_states -> begin
			let h_res = (filterTransitions h_state transitions symbol) in
			let t_res = (filterTransitionsManyStates t_states transitions symbol) in
			List.append h_res t_res
		end
;;

let rec filterTransitions state transitions symbol =
	match transitions with
	  [] -> []
	| h_transition :: t_transitions -> 
		begin
			let (start_state, transition_symbol, end_state) = h_transition in
			if state = start_state then
				begin
					let res = begin
						match (symbol, transition_symbol) with
						  (same_opt, same_opt) -> end_state
						| _ -> []
					end
					res :: (filterTransitions states tail_transitions symbol)
				end
			else
				begin
					filterTransitions states tail_transitions symbol
				end
		end
;;

let move m qs s = 
	match s with
	  Some a ->
	| None -> 
;;

let e_closure m qs = failwith "unimplemented"

let accept m str = failwith "unimplemented"

let nfa_to_dfa m = failwith "unimplemented"
