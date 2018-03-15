open Types
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError


let rec env_lookup env x = 
	match env with
	  [] -> raise (DeclareError x)
	| head :: tail ->
		let (id, res) = head in
		if id = x then
			res
		else
			env_lookup tail x
;;

let div a b =
	if b = 0 then
		raise DivByZeroError
	else
		(a / b)
;;

let declare_new dt = 
	match dt with
	  Int_Type -> Int_Val(0)
	| Bool_Type -> Bool_Val(false)
;;

let rec eval_expr env e = 
	match e with
	  Int i -> Int_Val i
	| Bool b -> Bool_Val b
	| ID id -> env_lookup env id
	| Add (arg1, arg2) -> 
		begin
			let val1 = eval_expr env arg1 in
			let val2 = eval_expr env arg2 in
			match (val1, val2) with
			  (Int_Val a, Int_Val b) -> Int_Val (a + b)
			| _ -> raise (TypeError "add: args not both ints")
		end
	| Sub (arg1, arg2) ->
		begin
			let val1 = eval_expr env arg1 in
			let val2 = eval_expr env arg2 in
			match (val1, val2) with
			  (Int_Val a, Int_Val b) -> Int_Val (a - b)
			| _ -> raise (TypeError "sub: args not both ints")
		end
	| Mult (arg1, arg2) ->
		begin
			let val1 = eval_expr env arg1 in
			let val2 = eval_expr env arg2 in
			match (val1, val2) with
			  (Int_Val a, Int_Val b) -> Int_Val (a * b)
			| _ -> raise (TypeError "mult: args not both ints")
		end
	| Div (arg1, arg2) ->
		begin
			let val1 = eval_expr env arg1 in
			let val2 = eval_expr env arg2 in
			match (val1, val2) with
			  (Int_Val a, Int_Val b) -> Int_Val (div a b)
			| _ -> raise (TypeError "div: args not both ints")
		end
	| Pow (arg1, arg2) ->
		begin
			let val1 = eval_expr env arg1 in
			let val2 = eval_expr env arg2 in
			match (val1, val2) with
			  (Int_Val a, Int_Val b) -> 
			  	begin
			  		let f_a = (float_of_int a) in 
			  		let f_b = (float_of_int b) in
			  		let res = int_of_float (f_a ** f_b) in
			  		Int_Val (res)
			  	end
			| _ -> raise (TypeError "pow: args not both ints")
		end
	| Or (arg1, arg2) ->
		begin
			let val1 = eval_expr env arg1 in
			let val2 = eval_expr env arg2 in
			match (val1, val2) with
			  (Bool_Val a, Bool_Val b) -> Bool_Val (a || b)
			| _ -> raise (TypeError "or: args not both bools")
		end
	| And (arg1, arg2) ->
		begin
			let val1 = eval_expr env arg1 in
			let val2 = eval_expr env arg2 in
			match (val1, val2) with
			  (Bool_Val a, Bool_Val b) -> Bool_Val (a && b)
			| _ -> raise (TypeError "and: args not both bools")
		end
	| Not (arg1) ->
		begin
			let val1 = eval_expr env arg1 in
			match val1 with
			  Bool_Val a -> Bool_Val (not a)
			| _ -> raise (TypeError "not: arg not bool")
		end
	| Greater (arg1, arg2) ->
		begin
			let val1 = eval_expr env arg1 in
			let val2 = eval_expr env arg2 in
			match (val1, val2) with
			  (Int_Val a, Int_Val b) -> Bool_Val (a > b)
			| _ -> raise (TypeError "greater: args not both ints")
		end
	| Less (arg1, arg2) ->
		begin
			let val1 = eval_expr env arg1 in
			let val2 = eval_expr env arg2 in
			match (val1, val2) with
			  (Int_Val a, Int_Val b) -> Bool_Val (a < b)
			| _ -> raise (TypeError "less: args not both ints")
		end
	| GreaterEqual (arg1, arg2) ->
		begin
			let val1 = eval_expr env arg1 in
			let val2 = eval_expr env arg2 in
			match (val1, val2) with
			  (Int_Val a, Int_Val b) -> Bool_Val (a >= b)
			| _ -> raise (TypeError "greaterequal: args not both ints")
		end
	| LessEqual (arg1, arg2) ->
		begin
			let val1 = eval_expr env arg1 in
			let val2 = eval_expr env arg2 in
			match (val1, val2) with
			  (Int_Val a, Int_Val b) -> Bool_Val (a <= b)
			| _ -> raise (TypeError "lessequal: args not both ints")
		end
	| Equal (arg1, arg2) ->
		begin
			let val1 = eval_expr env arg1 in
			let val2 = eval_expr env arg2 in
			match (val1, val2) with
			  (Int_Val a, Int_Val b) -> Bool_Val (a = b)
			| (Bool_Val a, Bool_Val b) -> Bool_Val (a = b)
			| _ -> raise (TypeError "equal: args not both of same type")
		end
	| NotEqual (arg1, arg2) ->
		begin
			let val1 = eval_expr env arg1 in
			let val2 = eval_expr env arg2 in
			match (val1, val2) with
			  (Int_Val a, Int_Val b) -> Bool_Val (not (a = b))
			| (Bool_Val a, Bool_Val b) -> Bool_Val (not (a = b))
			| _ -> raise (TypeError "notequal: args not both of same type")
		end
;;



let rec env_has env x = 
	match env with
	  [] -> false
	| head :: tail ->
		let (id, res) = head in
		if id = x then
			true
		else
			env_has tail x
;;

let rec update_env env id new_val = 
	match env with
	  [] -> []
	| (h_id, h_v) :: t ->
		if h_id = id then
			begin
				match (h_v, new_val) with
				  (Int_Val o_i, Int_Val n_i) -> (id, new_val) :: t
				| (Bool_Val o_b, Bool_Val n_b) -> (id, new_val) :: t
				| _ -> raise (TypeError "assign: types do not match")
			end
		else
			(h_id, h_v) :: (update_env t id new_val)
;;

let rec eval_stmt env s =
	match s with
	  NoOp -> env
	| Seq (curr, next) -> 
		begin
			let new_env = eval_stmt env curr in
			eval_stmt new_env next
		end
	| Declare (dt, id) ->
		begin
			if env_has env id then
				raise (DeclareError "declare: variable already exists")
			else begin
				let init_val = declare_new dt in
				(id, init_val) :: env
			end
		end
	| Assign (id, e) ->
		begin
			if not (env_has env id) then
				raise (DeclareError "assign: variable does not exist")
			else begin
				let res = eval_expr env e in
				update_env env id res
			end
		end
	| If (e, s1, s2) ->
		begin
			let res = eval_expr env e in
			match res with
			  Bool_Val a ->
			  	begin
			  		if a then
			  			eval_stmt env s1
			  		else
			  			eval_stmt env s2
			  	end
			 | Int_Val _ -> raise (TypeError "if: guard is not boolean")
		end
	| While (e, s) ->
		begin
			let res = eval_expr env e in
			match res with
			  Bool_Val a ->
					begin
						if a then
							let new_env = eval_stmt env s in
							eval_stmt new_env (While (e,s))
						else
							env
					end
			| Int_Val _ -> raise (TypeError "while: guard is not boolean")
		end
	| Print e ->
		begin
			let res = eval_expr env e in
			let str = (match res with
			  Int_Val a -> string_of_int a
			| Bool_Val b -> string_of_bool b) in
			let () = print_output_string (str ^ "\n") in
			env
		end
		
	
	
	
	
	
	
	
	
	
	
;;
