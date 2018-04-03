open SmallCTypes

let str_to_tok tok = failwith "not implemented";;

let tokenize_str_list str_list =
	match str_list with
	  [] -> []
	| str :: tl_list -> (str_to_tok str) :: (tokenize_str_list tl_list)
;;

let split_on_whitespace str =
	let white_space_r = (Str.regexp "[ \n\r\t]+") in
	Str.split white_space_r str
;;

let tokenize input =
	let split_input = (split_on_whitespace input) in
	tokenize_str_list split_input
;;
