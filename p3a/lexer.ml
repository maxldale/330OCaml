open SmallCTypes

(*********)
(* Regex *)
(*********)

let group expr =
	"\\(" ^ expr ^ "\\)"
;;

let block expr =
	"[" ^ expr ^ "]"
;;

let rest = (group ".*");;

let int_r =
	let digits = ((block "0-9") ^ "+") in
	let nums = (group ("-?" ^ digits)) in
	Str.regexp (nums ^ rest)
;;

let str_r =
	let ltr = (block "a-zA-Z") in
	let ltrsOrNums = ((block ("a-zA-Z0-9") ^ "*")) in
	let str = (group (ltr ^ ltrsOrNums)) in
	Str.regexp (str ^ rest)
;;

let paren_r =
	let parens = (group "(|)") in
	Str.regexp (parens ^ rest)
;;


let brace_r =
	let brace = (group "{|}") in
	Str.regexp (brace ^ rest)
;;


let eq_r =
	let fstSym = (block "=|!|>|<") in
	let sndSym = "=?" in
	let eq = (group (fstSym ^ sndSym)) in
	Str.regexp (eq ^ rest)
;;


let b_op_r =
	let or_op = (block "\\|\\|") in
	let and_op = "&&" in
	let bool_op = (group (or_op ^ "|" ^ and_op)) in
	Str.regexp (bool_op ^ rest)
;;


let s_c_r =
	let semi_colon = (group ";") in
	Str.regexp (semi_colon ^ rest)
;;


let math_r =
	let plus = (block "\\+") in
	let mult = (block "\\*") in
	let pow = (block "\\^") in
	let math = (group (plus ^ "-" ^ mult ^ "/" ^ pow)) in
	Str.regexp (math ^ rest)
;;

(*********)
(* Match *)
(*********)

let match_str regex_r str =
	Str.string_match regex_r str 0
;;

let match_tok str =
	Str.matched_group 1 str
;;

let match_rest str =
	Str.matched_group 2 str
;;

(************)
(* Tokenize *)
(************)

let tok_int int_str =
	let tok_match = (match_tok int_str) in
	Tok_Int (int_of_string tok_match)
;;

let tok_str str_str =
	let tok_match = (match_tok str_str) in
	match tok_match with
	  "true" | "false" -> (Tok_Bool (bool_of_string tok_match))
	| "int" -> Tok_Int_Type
	| "bool" -> Tok_Bool_Type
	| "printf" -> Tok_Print
	| "main" -> Tok_Main
	| "if" -> Tok_If
	| "else" -> Tok_Else
	| "while" -> Tok_While
	| _ -> (Tok_ID  tok_match)
;;

let tok_paren paren_str =
	let tok_match = (match_tok paren_str) in
	match tok_match with
	  "(" -> Tok_LParen
	| ")" -> Tok_RParen
	| _ -> failwith "Paren match error"
;;

let tok_brace brace_str =
	let tok_match = (match_tok brace_str) in
	match tok_match with
	  "{" -> Tok_LBrace
	| "}" -> Tok_RBrace
	| _ -> failwith "Brace match error"
;;

let tok_eq eq_str =
	let tok_match = (match_tok eq_str) in
	match tok_match with
	  "==" -> Tok_Equal
	| "!=" -> Tok_NotEqual
	| "=" -> Tok_Assign
	| ">" -> Tok_Greater
	| "<" -> Tok_Less
	| ">=" -> Tok_GreaterEqual
	| "<=" -> Tok_LessEqual
	| "!" -> Tok_Not
	| _ -> failwith "Equality match error"
;;

let tok_b_op b_op_str =
	let tok_match = (match_tok b_op_str) in
	match tok_match with
	  "||" -> Tok_Or
	| "&&" -> Tok_And
	| _ -> failwith "Boolean Operation match error"
;;

let tok_s_c s_c_str =
	let tok_match = (match_tok s_c_str) in
	match tok_match with
	  ";" -> Tok_Semi
	| _ -> failwith "Semi-Colon match error"
;;

let tok_math math_str =
	let tok_match = (match_tok math_str) in
	match tok_match with
	  "+" -> Tok_Add
	| "-" -> Tok_Sub
	| "*" -> Tok_Mult
	| "/" -> Tok_Div
	| "^" -> Tok_Pow
	| _ -> failwith "Math match error"


(*****************)
(* Process Input *)
(*****************)

let rec str_to_tok tok = 
	match tok with
	  "" -> []
	| str ->
		let () = (print_string str) in
		let tok = 
			(match str with
			  int_str when (match_str int_r int_str) -> (tok_int int_str)
			| str_str when (match_str str_r str_str) -> (tok_str str_str)
			| paren_str when (match_str paren_r paren_str) -> (tok_paren paren_str)
			| brace_str when (match_str brace_r brace_str) -> (tok_brace brace_str)
			| eq_str when (match_str eq_r eq_str) -> (tok_eq eq_str)
			| b_op_str when (match_str b_op_r b_op_str) -> (tok_b_op b_op_str)
			| s_c_str when (match_str s_c_r s_c_str) -> (tok_s_c s_c_str)
			| math_str when (match_str math_r math_str) -> (tok_math math_str)
			| _ -> failwith ("tok: " ^ str)) in
		let rest = (match_rest str) in
		tok :: (str_to_tok rest)
;;

let rec tokenize_str_list str_list =
	match str_list with
	  [] -> []
	| str :: tl_list -> (List.append (str_to_tok str)  (tokenize_str_list tl_list))
;;

let split_on_whitespace str =
	let white_space_r = (Str.regexp "[ \n\t]+") in
	Str.split white_space_r str
;;

let tokenize input =
	let split_input = (split_on_whitespace input) in
	tokenize_str_list split_input
;;
