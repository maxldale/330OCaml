(*
  Discussion 8 problems
  Upload this to the submit server by Friday 3/30/2018.
  It's graded for correctness.
*)

(*
  name: parse_S
  type: tok list -> tok list * ast
  desc: Uses recursive descent to parse a tok list and returns (1) the unparsed tokens and (2) the ast

  grammar:
  S -> S + S | E
  E -> n | (S)
  
  new grammar:
  S -> E + S | E
  E -> n | (S)

  (change the grammar so it can be parsed by a recursive descent parser.)

  parse_S [Tok_Int 1; Tok_Plus; Tok_LParen; Tok_Int 2; Tok_Plus; Tok_Int 3; Tok_RParen; EOF]
  (tok list representation of "1 + (2 + 3)")

  -> ([EOF], Plus (Int 1, Plus (Int 2, Int 3)))
*)

type tok =
| Tok_Int of int
| Tok_Plus
| Tok_LParen
| Tok_RParen
| EOF
;;

type ast =
| Int of int
| Plus of ast * ast
;;

let lookahead toks =
  match toks with
  | h::_ -> h
  | _ -> failwith "lookahead failed"
;;


let match_tok toks tok =
  match toks with
  | h::t when h = tok -> t
  | _ -> failwith "match_tok failed"
;;

let rec parse_S toks =
	let (toks', exp) = parse_E toks in
	if lookahead toks' =  Tok_Plus then
		let toks'' = match_tok toks' Tok_Plus in
		let (toks''', exp2) = parse_S toks'' in
		(toks''', Plus(exp, exp2))
	else
		(toks', exp)
	
	and parse_E toks = match lookahead toks with
	  Tok_Int n -> let toks' = match_tok toks (Tok_Int n) in (toks', Int n)
	| Tok_LParen -> let toks' = match_tok toks Tok_LParen in
		let (toks'', exp) = parse_S toks' in
		let toks''' = match_tok toks'' Tok_RParen in
		(toks''', exp)
	| _ -> failwith "parse_E error"
;;

