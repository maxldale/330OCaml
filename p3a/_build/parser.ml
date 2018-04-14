open SmallCTypes
open Utils

type stmt_result = token list * stmt
type expr_result = token list * expr

(* Provided helper function - takes a token list and an exprected token.
 * Handles error cases and returns the tail of the list *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))
;;

let rec ensureMatchingParen toks=
	match toks with
	  [] -> raise (InvalidInputException("non-matching parens"))
	| Tok_RParen :: _-> ()
	| _ :: t -> ensureMatchingParen t
;;

let createAdd e1 e2 =
	Add (e1, e2)
;;

let createSub e1 e2 =
	Sub (e1, e2)
;;

let rec createMult e1 e2 rparen lparen =
	(*let _ = (print_string ("MULT:e1:\n")) in
	let _ = (print_string (string_of_expr e1)) in
	let _ = (print_string ("e2:\n")) in
	let _ = (print_string (string_of_expr e2)) in
	let _ = (print_string ("rP:\n")) in
	let _ = (print_string (string_of_bool rparen)) in
	let _ = (print_string ("lP:\n")) in
	let _ = (print_string (string_of_bool lparen)) in*)
	match (rparen,lparen) with
	  (true, true) -> Mult (e1, e2)
	| (true, false) ->
	  	(match e2 with
	  	   Add (e3, e4) -> Add (Mult (e1,e3), e4)
	  	 | Sub (e3, e4) -> Sub (Mult (e1,e3), e4)
	  	 | _ -> Mult(e1, e2))
	| (false, true) ->
	  	(match e1 with
	  	   Add (e3, e4) -> Add (e3, Mult (e4,e2))
	  	 | Sub (e3, e4) -> Sub (e3, Mult (e4,e2))
	  	 | _ -> Mult(e1, e2))
	| (false, false) ->
	  	(match (e1, e2) with
	  	   (Add (e3, e4), Add (e5, e6)) -> Add (Add (e3, Mult (e4, e5)), e6)
	  	 | (Add (e3, e4), Sub (e5, e6)) -> Sub (Add (e3, Mult (e4, e5)), e6)
	  	 | (Sub (e3, e4), Add (e5, e6)) -> Add (Sub (e3, Mult (e4, e5)), e6)
	  	 | (Sub (e3, e4), Sub (e5, e6)) -> Sub (Sub (e3, Mult (e4, e5)), e6)
	  	 | (Add (e3, e4), _) -> Add (e3, (createMult e4 e2 false false))
	  	 | (Sub (e3, e4), _) -> Sub (e3, (createMult e4 e2 false false))
	  	 | (_, Add (e5, e6)) -> Add ((createMult e1 e5 false false), e6)
	  	 | (_, Sub (e5, e6)) -> Sub ((createMult e1 e5 false false), e6)
	  	 | _ -> Mult(e1, e2))

let rec createDiv e1 e2 rparen lparen =
	(*let _ = (print_string ("\nDIV:e1:\n")) in
	let _ = (print_string (string_of_expr e1)) in
	let _ = (print_string ("\n\ne2:\n")) in
	let _ = (print_string (string_of_expr e2)) in
	let _ = (print_string ("\n\nrP:\n")) in
	let _ = (print_string (string_of_bool rparen)) in
	let _ = (print_string ("\n\nlP:\n")) in
	let _ = (print_string (string_of_bool lparen)) in*)
	match (rparen,lparen) with
	  (true, true) -> Div (e1, e2)
	| (true, false) ->
	  	(match e2 with
	  	   Add (e3, e4) -> Add (Div (e1,e3), e4)
	  	 | Sub (e3, e4) -> Sub (Div (e1,e3), e4)
	  	 | _ -> Div(e1, e2))
	| (false, true) ->
	  	(match e1 with
	  	   Add (e3, e4) -> Add (e3, Div (e4,e2))
	  	 | Sub (e3, e4) -> Sub (e3, Div (e4,e2))
	  	 | _ -> Div(e1, e2))
	| (false, false) ->
	  	(match (e1, e2) with
	  	   (Add (e3, e4), Add (e5, e6)) -> Add (Add (e3, Div (e4, e5)), e6)
	  	 | (Add (e3, e4), Sub (e5, e6)) -> Sub (Add (e3, Div (e4, e5)), e6)
	  	 | (Sub (e3, e4), Add (e5, e6)) -> Add (Sub (e3, Div (e4, e5)), e6)
	  	 | (Sub (e3, e4), Sub (e5, e6)) -> Sub (Sub (e3, Div (e4, e5)), e6)
	  	 | (Add (e3, e4), _) -> Add (e3, (createDiv e4 e2 false false))
	  	 | (Sub (e3, e4), _) -> Sub (e3, (createDiv e4 e2 false false))
	  	 | (_, Add (e5, e6)) -> Add ((createDiv e1 e5 false false), e6)
	  	 | (_, Sub (e5, e6)) -> Sub ((createDiv e1 e5 false false), e6)
	  	 | _ -> Div(e1, e2))

let rec createPow e1 e2 rparen lparen =
	(*let _ = (print_string ("POW:e1:\n")) in
	let _ = (print_string (string_of_expr e1)) in
	let _ = (print_string ("e2:\n")) in
	let _ = (print_string (string_of_expr e2)) in
	let _ = (print_string ("rP:\n")) in
	let _ = (print_string (string_of_bool rparen)) in
	let _ = (print_string ("lP:\n")) in
	let _ = (print_string (string_of_bool lparen)) in*)
	match (rparen,lparen) with
	  (true, true) -> Pow (e1, e2)
	| (true, false) ->
	  	(match e2 with
	  	   Add (e3, e4) -> Add (Pow (e1,e3), e4)
	  	 | Sub (e3, e4) -> Sub (Pow (e1,e3), e4)
	  	 | Mult (e3, e4) -> Mult (Pow (e1,e3), e4)
	  	 | Div (e3, e4) -> Div (Pow (e1,e3), e4)
	  	 | _ -> Pow(e1, e2))
	| (false, true) ->
	  	(match e1 with
	  	   Add (e3, e4) -> Add (e3, Pow (e4,e2))
	  	 | Sub (e3, e4) -> Sub (e3, Pow (e4,e2))
	  	 | Mult (e3, e4) -> Mult (e3, Pow (e4,e2))
	  	 | Div (e3, e4) -> Div (e3, Pow (e4,e2))
	  	 | _ -> Pow(e1, e2))
	| (false, false) ->
	  	(match (e1, e2) with
	  	   (Add (e3, e4), Add (e5, e6)) -> Add (Add (e3, Pow (e4, e5)), e6)
	  	 | (Add (e3, e4), Sub (e5, e6)) -> Sub (Add (e3, Pow (e4, e5)), e6)
	  	 | (Add (e3, e4), Mult (e5, e6)) -> Mult (Add (e3, Pow (e4, e5)), e6)
	  	 | (Add (e3, e4), Div (e5, e6)) -> Div (Add (e3, Pow (e4, e5)), e6)
	  	 | (Sub (e3, e4), Add (e5, e6)) -> Add (Sub (e3, Pow (e4, e5)), e6)
	  	 | (Sub (e3, e4), Sub (e5, e6)) -> Sub (Sub (e3, Pow (e4, e5)), e6)
	  	 | (Sub (e3, e4), Mult (e5, e6)) -> Mult (Sub (e3, Pow (e4, e5)), e6)
	  	 | (Sub (e3, e4), Div (e5, e6)) -> Div (Sub (e3, Pow (e4, e5)), e6)
	  	 | (Mult (e3, e4), Add (e5, e6)) -> Add (Mult (e3, Pow (e4, e5)), e6)
	  	 | (Mult (e3, e4), Sub (e5, e6)) -> Sub (Mult (e3, Pow (e4, e5)), e6)
	  	 | (Mult (e3, e4), Mult (e5, e6)) -> Mult (Mult (e3, Pow (e4, e5)), e6)
	  	 | (Mult (e3, e4), Div (e5, e6)) -> Div (Mult (e3, Pow (e4, e5)), e6)
	  	 | (Div (e3, e4), Add (e5, e6)) -> Add (Div (e3, Pow (e4, e5)), e6)
	  	 | (Div (e3, e4), Sub (e5, e6)) -> Sub (Div (e3, Pow (e4, e5)), e6)
	  	 | (Div (e3, e4), Mult (e5, e6)) -> Mult (Div (e3, Pow (e4, e5)), e6)
	  	 | (Div (e3, e4), Div (e5, e6)) -> Div (Div (e3, Pow (e4, e5)), e6)
	  	 | (Add (e3, e4), _) -> Add (e3, (createPow e4 e2 false false))
	  	 | (Sub (e3, e4), _) -> Sub (e3, (createPow e4 e2 false false))
	  	 | (Mult (e3, e4), _) -> Mult (e3, (createPow e4 e2 false false))
	  	 | (Div (e3, e4), _) -> Div (e3, (createPow e4 e2 false false))
	  	 | (_, Add (e5, e6)) -> Add ((createPow e1 e5 false false), e6)
	  	 | (_, Sub (e5, e6)) -> Sub ((createPow e1 e5 false false), e6)
	  	 | (_, Mult (e5, e6)) -> Mult ((createPow e1 e5 false false), e6)
	  	 | (_, Div (e5, e6)) -> Div ((createPow e1 e5 false false), e6)
	  	 | _ -> Pow(e1, e2))
;;

let rec parse_expr toks = 
	(match toks with
	  [] -> (raise (InvalidInputException "failed to parse expr"))
	| tok :: t when (tokIsFirstTok tok)-> (formExpr (matchFirstTok tok t))
	| _ -> (raise (InvalidInputException "failed to parse expr")))

and formExpr ((toks: (token list)), (expr: expr)): (token list * expr) =
	(match toks with
	  [] -> (toks, expr)
	| EOF :: [] -> ([], expr)
	| EOF :: _ -> (raise (InvalidInputException "failed to parse expr"))
	| Tok_Or :: t ->
		(let (new_t, new_expr) = (parse_expr t) in
		let orExpr = (Or (expr, new_expr)) in
		(formExpr (new_t, orExpr)))
	| Tok_And :: t -> 
		(let (new_t, new_expr) = (parse_expr t) in
		let andExpr = (And (expr, new_expr)) in
		(formExpr (new_t, andExpr)))
	| Tok_Equal :: t -> 
		(let (new_t, new_expr) = (parse_expr t) in
		let eqExpr = (Equal (expr, new_expr)) in
		(formExpr (new_t, eqExpr)))
	| Tok_NotEqual :: t -> 
		(let (new_t, new_expr) = (parse_expr t) in
		let notEqExpr = (NotEqual (expr, new_expr)) in
		(formExpr (new_t, notEqExpr)))
	| Tok_Less :: t -> 
		(let (new_t, new_expr) = (parse_expr t) in
		let lessExpr = (Less (expr, new_expr)) in
		(formExpr (new_t, lessExpr)))
	| Tok_Greater :: t -> 
		(let (new_t, new_expr) = (parse_expr t) in
		let greaterExpr = (Greater (expr, new_expr)) in
		(formExpr (new_t, greaterExpr)))
	| Tok_LessEqual :: t -> 
		(let (new_t, new_expr) = (parse_expr t) in
		let leqExpr = (LessEqual (expr, new_expr)) in
		(formExpr (new_t, leqExpr)))
	| Tok_GreaterEqual :: t -> 
		(let (new_t, new_expr) = (parse_expr t) in
		let geqExpr = (GreaterEqual (expr, new_expr)) in
		(formExpr (new_t, geqExpr)))
	| Tok_GreaterEqual :: t -> 
		(let (new_t, new_expr) = (parse_expr t) in
		let geqExpr = (GreaterEqual (expr, new_expr)) in
		(formExpr (new_t, geqExpr)))
	| Tok_Add :: h :: t -> 
		(let (new_t, new_expr) = (matchFirstTok h t) in
		let addExpr = (createAdd expr new_expr) in
		(formExpr (new_t, addExpr)))
	| Tok_Sub :: h :: t -> 
		(let (new_t, new_expr) = (matchFirstTok h t) in
		let subExpr = (createSub expr new_expr) in
		(formExpr (new_t, subExpr)))
	| Tok_RParen :: Tok_Mult :: Tok_LParen :: t -> 
		(let (new_t, new_expr) = (matchFirstTok Tok_LParen t) in
		let multExpr = (createMult expr new_expr true true) in
		(formExpr (new_t, multExpr)))
	| Tok_RParen :: Tok_Mult :: h :: t -> 
		(let (new_t, new_expr) = (matchFirstTok h t) in
		let multExpr = (createMult expr new_expr true false) in
		(formExpr (new_t, multExpr)))
	| Tok_Mult :: Tok_LParen :: t -> 
		(let (new_t, new_expr) = (matchFirstTok Tok_LParen t) in
		let multExpr = (createMult expr new_expr false true) in
		(formExpr (new_t, multExpr)))
	| Tok_Mult :: h :: t -> 
		(let (new_t, new_expr) = (matchFirstTok h t) in
		let multExpr = (createMult expr new_expr false false) in
		(formExpr (new_t, multExpr)))
	| Tok_RParen :: Tok_Div :: Tok_LParen :: t -> 
		(let (new_t, new_expr) = (matchFirstTok Tok_LParen t) in
		let divExpr = (createDiv expr new_expr true true) in
		(formExpr (new_t, divExpr)))
	| Tok_RParen :: Tok_Div :: h :: t -> 
		(let (new_t, new_expr) = (matchFirstTok h t) in
		let divExpr = (createDiv expr new_expr true false) in
		(formExpr (new_t, divExpr)))
	| Tok_Div :: Tok_LParen :: t -> 
		(let (new_t, new_expr) = (matchFirstTok Tok_LParen t) in
		let divExpr = (createDiv expr new_expr false true) in
		(formExpr (new_t, divExpr)))
	| Tok_Div :: h :: t -> 
		(let (new_t, new_expr) = (matchFirstTok h t) in
		let divExpr = (createDiv expr new_expr false false) in
		(formExpr (new_t, divExpr)))
	| Tok_RParen :: Tok_Pow :: Tok_LParen :: t -> 
		(let (new_t, new_expr) = (matchFirstTok Tok_LParen t) in
		let powExpr = (createPow expr new_expr true true) in
		(formExpr (new_t, powExpr)))
	| Tok_RParen :: Tok_Pow :: h :: t -> 
		(let (new_t, new_expr) = (matchFirstTok h t) in
		let powExpr = (createPow expr new_expr true false) in
		(formExpr (new_t, powExpr)))
	| Tok_Pow :: Tok_LParen :: t -> 
		(let (new_t, new_expr) = (parse_expr (Tok_LParen :: t)) in
		let powExpr = (createPow expr new_expr false true) in
		(formExpr (new_t, powExpr)))
	| Tok_Pow :: h :: t -> 
		(let (new_t, new_expr) = (matchFirstTok h t) in
		let powExpr = (createPow expr new_expr false false) in
		(formExpr (new_t, powExpr)))
	| Tok_RParen :: t -> 
		(let (new_t, new_expr) = formExpr (t, expr) in
		if(new_expr = expr) then (toks, expr) else (new_t, new_expr))
	| _ -> (toks, expr))
	
and tokIsFirstTok tok =
	(match tok with
	  Tok_Int _ | Tok_Bool _ | Tok_ID _ -> true
	| Tok_Not | Tok_LParen -> true
	| _ -> false)
	
and matchFirstTok tok t =
	(match tok with
	  (Tok_Int i) -> (t, (Int i))
	| (Tok_Bool b) -> (t, (Bool b))
	| (Tok_ID id) -> (t, (ID id))
	| Tok_Not ->
		(let (new_t, new_expr) = (parse_expr t) in
		(new_t, Not new_expr))
	| Tok_LParen ->
		(let (new_t, new_expr) = (parse_expr t) in
		let () = (ensureMatchingParen t) in
		(new_t, new_expr))
	| _ -> (raise (InvalidInputException "failed to parse expr")))
;;

let rec parse_stmt toks = 
	(match toks with
	  [] -> ([], NoOp)
	| tks -> (firstStmt tks)
	| _ -> (raise (InvalidInputException "failed to parse stmt")))
	
and formStmt (toks, stmt) =
	(match toks with
	  EOF :: [] -> ([], Seq(stmt, NoOp))
	| EOF :: _ -> (raise (InvalidInputException "failed to parse stmt"))
	| Tok_Int_Type :: (Tok_ID id) :: Tok_Semi :: t ->
	  	(let (n_t, res) = (formStmt(t, (Declare (Int_Type, id)))) in
	  	(n_t, Seq(stmt, res)))
	| Tok_Bool_Type :: (Tok_ID id) :: Tok_Semi :: t ->
	  	(let (n_t, res) = (formStmt(t, (Declare (Bool_Type, id)))) in
	  	(n_t, Seq(stmt, res)))
	| (Tok_ID id) :: Tok_Assign :: t ->
	  	(let (new_t, expr) = (parse_expr t) in
	  	let n_t = (match_token new_t Tok_Semi) in
	  	let (nn_t, res) = (formStmt (n_t, (Assign (id, expr)))) in
	  	(nn_t, Seq(stmt, res)))
	| Tok_Print :: Tok_LParen :: t ->
	  	(let (new_t, expr) = (parse_expr t) in
	  	let n_t = (match_token new_t Tok_RParen) in
	  	let nn_t = (match_token n_t Tok_Semi) in
	  	let (nnn_t, res) = formStmt(nn_t, Print expr) in
	  	(nnn_t, Seq(stmt, res)))
	| Tok_If :: Tok_LParen :: t ->
	  	(let (new_t, expr) = (parse_expr t) in
	  	let n_t = (match_token new_t Tok_RParen) in
	  	let nn_t = (match_token n_t Tok_LBrace) in
	  	let (nnn_t, stmt1) = (parse_stmt nn_t) in
	  	let nnnn_t = (match_token nnn_t Tok_RBrace) in
	  	let () = print_string ("\n" ^ string_of_list string_of_token nnnn_t) in
	  	match nnnn_t with
	  	  Tok_Else :: Tok_LBrace :: tt -> 
	  	  	(let () = print_string ("\n" ^ string_of_list string_of_token tt) in
	  	  	let (nnew_t, stmt2) = (parse_stmt tt) in
	  	  	let () = print_string ("After:\n" ^ string_of_list string_of_token nnew_t) in
	  	  	let nnnnn_t = (match_token nnew_t Tok_RBrace) in
	  	  	
	  		let () = print_string ("\n" ^ string_of_stmt stmt1) in
	  		let () = print_string ("\n" ^ string_of_stmt stmt2) in
	  	  	let (nnnnnn_t, res) = (formStmt (nnnnn_t, (If (expr, stmt1, stmt2)))) in
	  	  	(nnnnnn_t, Seq(stmt, res)))
	  	| _ -> 
	  		(let (nnnnnnn_t, res) = (formStmt (nnnn_t, (If (expr, stmt1, NoOp)))) in
	  		(nnnnnnn_t, Seq(stmt, res))))
	| Tok_While :: Tok_LParen :: t ->
	  	(let (new_t, expr) = (parse_expr t) in
	  	let n_t = (match_token new_t Tok_RParen) in
	  	let nn_t = (match_token n_t Tok_LBrace) in
	  	let (nnn_t, stmt) = (parse_stmt nn_t) in
	  	let nnnn_t = (match_token nnn_t Tok_RBrace) in
	  	let (nnnnn_t, res) = (formStmt (nnnn_t, While (expr, stmt))) in
	  	(nnnnn_t, Seq(stmt, res)))
	| _ -> (toks, Seq(stmt, NoOp)))
	
and firstStmt toks =
	(match toks with
	  EOF :: [] -> ([], NoOp)
	| EOF :: _ -> (raise (InvalidInputException "failed to parse stmt"))
	| Tok_Int_Type :: (Tok_ID id) :: Tok_Semi :: t ->
	  	(formStmt (t, Declare (Int_Type, id)))
	| Tok_Bool_Type :: (Tok_ID id) :: Tok_Semi :: t ->
	  	(formStmt (t, Declare (Bool_Type, id)))
	| (Tok_ID id) :: Tok_Assign :: t ->
	  	(let (new_t, expr) = (parse_expr t) in
	  	let () = print_string (string_of_expr expr) in
	  	let n_t = (match_token new_t Tok_Semi) in
	  	(formStmt (n_t, Assign (id, expr))))
	| Tok_Print :: Tok_LParen :: t ->
	  	(let (new_t, expr) = (parse_expr t) in
	  	let n_t = (match_token new_t Tok_RParen) in
	  	let nn_t = (match_token n_t Tok_Semi) in
	  	(formStmt (nn_t, Print expr)))
	| Tok_If :: Tok_LParen :: t ->
	  	(let (new_t, expr) = (parse_expr t) in
	  	let n_t = (match_token new_t Tok_RParen) in
	  	let nn_t = (match_token new_t Tok_LBrace) in
	  	let (nnn_t, stmt1) = (parse_stmt nn_t) in
	  	let nnnn_t = (match_token new_t Tok_RBrace) in
	  	match nnnn_t with
	  	  Tok_Else :: Tok_LBrace :: t -> 
	  	  	(let (new_t, stmt2) = (parse_stmt t) in
	  	  	let n_t = (match_token new_t Tok_RBrace) in
	  	  	(formStmt (n_t, If (expr, stmt1, stmt2))))
	  	| _ -> (formStmt (nnnn_t, If (expr, stmt1, NoOp))))
	| Tok_While :: Tok_LParen :: t ->
	  	(let (new_t, expr) = (parse_expr t) in
	  	let n_t = (match_token new_t Tok_RParen) in
	  	let nn_t = (match_token new_t Tok_LBrace) in
	  	let (nnn_t, stmt) = (parse_stmt nn_t) in
	  	let nnnn_t = (match_token new_t Tok_RBrace) in
	  	(formStmt (nnnn_t, While (expr, stmt))))
	| _ -> (toks, NoOp))
;;

let parse_main toks = 
	let lessType = (match_token toks Tok_Int_Type) in
	let lessMain = (match_token lessType Tok_Main) in
	let lessLParen = (match_token lessMain Tok_LParen) in
	let lessRParen = (match_token lessLParen Tok_RParen) in
	let lessLBrace = (match_token lessRParen Tok_LBrace) in
	let (tks, stmt) = (parse_stmt lessLBrace) in
	let lessRBrace = (match_token tks Tok_RBrace) in
	let lessEOF = (match_token lessRBrace EOF) in
	if(lessEOF = []) then stmt else (raise (InvalidInputException "failed to parse main"))
;;
	
	
	
	
	
	
	
