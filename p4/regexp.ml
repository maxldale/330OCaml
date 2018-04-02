open Sets
open Nfa

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(*******************************)
(* Part 2: Regular Expressions *)
(*******************************)

let rec shift_qs l n =
	match l with
	  [] -> []
	| h :: t -> ((h + n) :: (shift_qs t n))
;;

let rec shift_deltas l n =
	match l with
	  [] -> []
	| (s, sym, e) :: t -> (((s + n), sym, (e + n)) :: (shift_deltas t n))
;;

let rec remove (l : 'a list) (v : 'a) : 'a list =
	match l with
	  [] -> []
	| h :: t -> 
		begin
			if (v = h) then (remove t v)
			else (h :: (remove t v))
		end
;;

let rec remove_dups (l1 : 'a list) (l2 : 'a list) : 'a list =
	match l1 with
	  [] -> l2
	| h :: t ->
		begin
			let new_l2 = (remove l2 h) in
			remove_dups t new_l2
		end
;;

let rec generate_nfa re =
	match re with
	  Empty_String ->
		begin
			{
				qs = [0];
				sigma = [];
				delta = [];
				q0 = 0;
				fs = [0];
			}
		end
	| Char c ->
		begin
			let new_qs = [0; 1] in
			let new_sigma = [c] in
			let new_delta = [(0, (Some c), 1)] in
			let new_q0 = 0 in
			let new_end_s = [1] in
			{
				qs = new_qs;
				sigma = new_sigma;
				delta = new_delta;
				q0 = new_q0;
				fs = new_end_s;
			}
		end
	| Union (exp1, exp2) ->
		begin
			let nfa1 = generate_nfa exp1 in
			let nfa2 = generate_nfa exp2 in
			
			let i_1_2 = (1 + (List.length nfa1.qs)) in
			let i_2_1 = (1 + i_1_2) in
			let i_2_2 = (i_2_1 + (List.length nfa2.qs) - 1) in
			
			let nfa1_qs = (shift_qs nfa1.qs 2) in
			let nfa2_qs = (shift_qs nfa2.qs i_2_1) in
			let new_qs = (0 :: 1 :: (List.append nfa1_qs nfa2_qs)) in
			
			let new_sigma = (List.append nfa1.sigma (remove_dups nfa1.sigma nfa2.sigma)) in
			
			let nfa1_deltas = (shift_deltas nfa1.delta 2) in
			let nfa2_deltas = (shift_deltas nfa2.delta i_2_1) in
			let new_delta = (
				(0, None, 2) ::
				(0, None, i_2_1) ::
				(i_1_2, None, 1) ::
				(i_2_2, None, 1) ::
				(List.append nfa1_deltas nfa2_deltas)) in
			
			let new_q0 = 0 in
			
			let new_end_s = [1] in
			
			{
				qs = new_qs;
				sigma = new_sigma;
				delta = new_delta;
				q0 = new_q0;
				fs = new_end_s;
			}
		end
	| Concat (exp1, exp2) ->
		begin
			let nfa1 = generate_nfa exp1 in
			let nfa2 = generate_nfa exp2 in
			
			let i_1 = (1 + (List.length nfa1.qs)) in
			let i_2 = (1 + i_1) in
			let i_3 = (i_2 + (List.length nfa2.qs) - 1) in
			
			let nfa1_qs = (shift_qs nfa1.qs 2) in
			let nfa2_qs = (shift_qs nfa2.qs i_2) in
			let new_qs = (0 :: 1 :: (List.append nfa1_qs nfa2_qs)) in
			
			let new_sigma = (List.append nfa1.sigma (remove_dups nfa1.sigma nfa2.sigma)) in
			
			let nfa1_deltas = (shift_deltas nfa1.delta 2) in
			let nfa2_deltas = (shift_deltas nfa2.delta i_2) in
			let new_delta = (
				(0, None, 2) ::
				(i_1, None, i_2) ::
				(i_3, None, 1) ::
				(List.append nfa1_deltas nfa2_deltas)) in
			
			let new_q0 = 0 in
			
			let new_end_s = [1] in
			
			{
				qs = new_qs;
				sigma = new_sigma;
				delta = new_delta;
				q0 = new_q0;
				fs = new_end_s;
			}
		end
	|Star exp ->
		begin
			let nfa = generate_nfa exp in
			
			let i_1 = (1 + (List.length nfa.qs)) in
			
			let nfa_qs = (shift_qs nfa.qs 2) in
			let new_qs = (0 :: 1 :: nfa_qs) in
			
			let new_sigma = (nfa.sigma) in
			
			let nfa_deltas = (shift_deltas nfa.delta 2) in
			let new_delta = (
				(0, None, 1) ::
				(0, None, 2) ::
				(1, None, 0) ::
				(i_1, None, 1) ::
				nfa_deltas) in
			
			let new_q0 = 0 in
			
			let new_end_s = [1] in
			
			{
				qs = new_qs;
				sigma = new_sigma;
				delta = new_delta;
				q0 = new_q0;
				fs = new_end_s;
			}
		end
;;

let regexp_to_nfa re =
	generate_nfa re
;;

let rec form_regex r =
	match r with
	  Empty_String -> "E"
	| Char c -> (String.make 1 c)
	| Union (exp1, exp2) -> "(" ^ (form_regex exp1) ^ "|" ^ (form_regex exp2) ^ ")"
	| Concat (exp1, exp2) -> "(" ^ (form_regex exp1) ^ (form_regex exp2) ^ ")"
	| Star exp -> (form_regex exp) ^ "*"
;;

let regexp_to_string r =
	form_regex r
;;

(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then
      [Tok_END]
    else begin
      if (Str.string_match re_var s pos) then
        let token = Str.matched_string s in
        (Tok_Char token.[0])::(tok (pos+1) s)
      else if (Str.string_match re_epsilon s pos) then
        Tok_Epsilon::(tok (pos+1) s)
      else if (Str.string_match re_union s pos) then
        Tok_Union::(tok (pos+1) s)
      else if (Str.string_match re_star s pos) then
        Tok_Star::(tok (pos+1) s)
      else if (Str.string_match re_lparen s pos) then
        Tok_LParen::(tok (pos+1) s)
      else if (Str.string_match re_rparen s pos) then
        Tok_RParen::(tok (pos+1) s)
      else
        raise (IllegalExpression("tokenize: " ^ s))
    end
  in
  tok 0 str

let tok_to_str t = ( match t with
      Tok_Char v -> (Char.escaped v)
    | Tok_Epsilon -> "E"
    | Tok_Union -> "|"
    | Tok_Star ->  "*"
    | Tok_LParen -> "("
    | Tok_RParen -> ")"
    | Tok_END -> "END"
  )

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list = match tok_list with
      [] -> raise (IllegalExpression "lookahead")
    | (h::t) -> (h,t)
  in

  let rec parse_S l =
    let (a1,l1) = parse_A l in
    let (t,n) = lookahead l1 in
    match t with
      Tok_Union -> (
        let (a2,l2) = (parse_S n) in
        (Union (a1,a2),l2)
      )
    | _ -> (a1,l1)

  and parse_A l =
    let (a1,l1) = parse_B l in
    let (t,n) = lookahead l1 in
    match t with
      Tok_Char c ->
      let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
    | Tok_Epsilon ->
      let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
    | Tok_LParen ->
      let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
    | _ -> (a1,l1)

  and parse_B l =
    let (a1,l1) = parse_C l in
    let (t,n) = lookahead l1 in
    match t with
      Tok_Star -> (Star a1,n)
    | _ -> (a1,l1)

  and parse_C l =
    let (t,n) = lookahead l in
    match t with
      Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
      let (a1,l1) = parse_S n in
      let (t2,n2) = lookahead l1 in
      if (t2 = Tok_RParen) then
        (a1,n2)
      else
        raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let (rxp, toks) = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")

let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
