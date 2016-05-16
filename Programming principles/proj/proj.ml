open Syntax

exception RUNTIME_EXCEPTION of string
exception NOT_IMPLEMENTED
exception UNCAUGHT_EXCEPTION

(* this is for testing the lexer and the parser *)

type value_t = 
  | INT of int
  | BOOL of bool
  | NULL
  | CLOS of (var_t list * exp_t * ((var_t*value_t) ref list)) (* closure = lambda value *)
  | CLOS_MEM of (var_t list * exp_t * ((var_t*value_t) ref list) * (((var_t * value_t list), value_t) Hashtbl.t)) (* closure of memoizing lambda value *)
  | PAIR of value_t * value_t
  | MPAIR of value_t ref * value_t ref
  | VOID	       
  | UNDEF

exception EXCEPTION_RAISED of value_t

(* If value_to_string does not work well for your code, *)
(*  adjust this function manually to make it work *)
(* Content of mpair is hidden when printing *)
let rec value_to_string (v:value_t): string =
  match v with
  | INT n -> string_of_int n
  | BOOL b -> if b then "#t" else "#f"
  | NULL -> "'()"
  | CLOS _ -> "#<procedure>"
  | CLOS_MEM _ -> "#<procedure-memo>"
  | PAIR (a, b) -> "(cons " ^ (value_to_string a) ^ " " ^ (value_to_string b) ^ ")"
  | MPAIR (a, b) -> "(mcons ? ?)"
  | VOID -> "#<void>"
  | UNDEF -> "#<undef>"

let has_key (env : (var_t*value_t) ref list) (key: var_t) : bool = 
	List.exists (fun x -> if (fst !x = key) then true else false) env
let key_to_val (env : (var_t*value_t) ref list) (key: var_t) : value_t =
	let a = List.find (fun x -> (fst !x = key)) env in snd !a 
	
let insert_to_env (env: (var_t*value_t) ref list) (key: var_t) (value: value_t): (var_t*value_t) ref list =
	let newpair = ref (key, value) in 
	if has_key env key then List.map (fun x -> if ((fst !x) = key) then newpair else x) env 
	else let a = ref (key, value) in (a::env)

let dummy_env (env: (var_t*value_t) ref list) (keylist: (var_t*exp_t) list) : (var_t*value_t) ref list = 
	List.fold_left (fun x y -> insert_to_env x (fst y) UNDEF) env keylist

let insert_to_envmem (env: (var_t*value_t) ref list) (key: var_t) (value: value_t): (var_t*value_t) ref list =
	let newpair = (key, value) in 
	if has_key env key then let _ = List.iter (fun x -> if ((fst !x) = key) then x := newpair else x := !x) env in env
	else let a = ref (key, value) in (a::env)  
let rec dup_check lst = match lst with 
			| [] -> false 
			| hd::tl -> if (List.mem_assoc (fst hd) tl) then true else dup_check tl

let myeval (exp_string: string): value_t =
  let lexbuf = Lexing.from_string exp_string in
  let lexer () = Lexer.token lexbuf in
  let exp = Parser.parse lexer in
  let emptyenv = [] in 
  let rec myeval_aux (exp: Syntax.exp_t) (env : (var_t*value_t) ref list) (inhandle: int): value_t =
		match exp with 
		| CONST a -> (match a with 
								| CINT a -> INT a
								| CTRUE -> BOOL true
								| CFALSE -> BOOL false
								| CNULL -> NULL)
		| VAR x -> if has_key env x then (let a = key_to_val env x in if a = UNDEF then raise (RUNTIME_EXCEPTION "undefined identifier") else a) 
						else raise (RUNTIME_EXCEPTION "var must be assigned before applying")
		| IF(exp1, exp2, exp3) -> let exp = myeval_aux exp1 env inhandle in 
								(match exp with 
								| BOOL a -> if a == false then (myeval_aux exp3 env inhandle) else (myeval_aux exp2 env inhandle)
								| _ -> raise (RUNTIME_EXCEPTION "IF must accept boolean value as 1st assignment"))

		| CONS(exp1, exp2) -> PAIR ((myeval_aux exp1 env inhandle), (myeval_aux exp2 env inhandle))
		| CAR exp -> let expr = (myeval_aux exp env inhandle) in 
									(match expr with 
									| PAIR (a, b) -> a
									| _ -> raise (RUNTIME_EXCEPTION "CAR must accept pair"))
		| CDR exp ->  let expr = (myeval_aux exp env inhandle) in 
									(match expr with 
									| PAIR (a, b) -> b
									| _ -> raise (RUNTIME_EXCEPTION "CAR must accept pair"))
		| LAMBDA (varlist, exp) -> let rec dup_checker lst = match lst with 
					| [] -> false 
					| hd::tl -> (if List.mem hd tl then true else dup_checker tl) in if dup_checker varlist then raise (RUNTIME_EXCEPTION "duplicate identifier") else CLOS (varlist, exp, env)
		| APP (exp, explist) -> let expr = myeval_aux exp env inhandle in 
					(match expr with 
					| CLOS (varlist, ex, restoreden) ->
					(if (List.length varlist) = (List.length explist) then 
					let a = List.fold_left2 (fun x y z -> insert_to_env x y (myeval_aux z env inhandle)) restoreden varlist explist 									in myeval_aux ex a inhandle else raise (RUNTIME_EXCEPTION "var = exp"))							
					| _ -> raise (RUNTIME_EXCEPTION "APP must have lambda at 1st place"))
		| LET (bindlist, exp) -> (if dup_check bindlist then raise (RUNTIME_EXCEPTION "duplicate identifier") else 
					let a = List.fold_left (fun x y -> insert_to_env x (fst y) (myeval_aux (snd y) env inhandle)) env bindlist in myeval_aux exp a inhandle)
		| LETREC (bindlist, exp) ->(if dup_check bindlist then raise (RUNTIME_EXCEPTION "duplicate identifier") else 
					 let dummyenv = dummy_env env bindlist in
					 let a = List.fold_left (fun x y -> insert_to_envmem x (fst y) (myeval_aux (snd y) x inhandle)) dummyenv bindlist in myeval_aux exp a inhandle)
		| ADD (exp1, exp2) -> let result1 = (myeval_aux exp1 env inhandle) in
													let result2 = (myeval_aux exp2 env inhandle) in 
													(match result1 with
													| INT a -> (match result2 with
																			| INT b -> INT (a + b)
																			| _ -> raise (RUNTIME_EXCEPTION "ADD must have int"))
													| _ -> raise (RUNTIME_EXCEPTION "ADD must have pair"))
		| SUB (exp1, exp2) -> let result1 = (myeval_aux exp1 env inhandle) in
													let result2 = (myeval_aux exp2 env inhandle) in 
													(match result1 with
													| INT a -> (match result2 with
																			| INT b -> INT (a - b)
																			| _ -> raise (RUNTIME_EXCEPTION "SUB must have int"))
													| _ ->	raise (RUNTIME_EXCEPTION "SUB must have pair"))
		| MUL (exp1, exp2) -> let result1 = (myeval_aux exp1 env inhandle) in
										let result2 = (myeval_aux exp2 env inhandle) in 
										(match result1 with
										| INT a -> (match result2 with
											| INT b -> INT (a * b)
											| _ -> raise (RUNTIME_EXCEPTION "MUL must have int"))
										| _ ->	raise (RUNTIME_EXCEPTION "MUL must have pair"))

		| EQ (exp1, exp2) -> let result1 = (myeval_aux exp1 env inhandle) in
													let result2 = (myeval_aux exp2 env inhandle) in 
													(match result1 with
													| INT a ->( match result2 with
																			| INT b -> BOOL (a = b)
																			| _ -> raise (RUNTIME_EXCEPTION "EQ must have int"))
													| _ ->	raise (RUNTIME_EXCEPTION "EQ must have pair"))
		| LT (exp1, exp2) -> let result1 = (myeval_aux exp1 env inhandle) in
													let result2 = (myeval_aux exp2 env inhandle) in 
													(match result1 with
													| INT a -> (match result2 with
																			| INT b -> BOOL (a < b)
																			| _ -> raise (RUNTIME_EXCEPTION "LT must have int"))
													| _ ->	raise (RUNTIME_EXCEPTION "LT must have pair"))
		| GT (exp1, exp2) -> let result1 = (myeval_aux exp1 env inhandle) in
													let result2 = (myeval_aux exp2 env inhandle) in 
													(match result1 with
													| INT a -> (match result2 with
																			| INT b -> BOOL (a > b)
																			| _ -> raise (RUNTIME_EXCEPTION "GT must have int"))
													| _ ->	raise (RUNTIME_EXCEPTION "GT must have pair"))
		| MCONS (exp1, exp2) ->  let refa = ref (myeval_aux exp1 env inhandle) in 
														 let refb = ref (myeval_aux exp2 env inhandle) in MPAIR (refa, refb)
 	  	| MCAR exp -> let expr = (myeval_aux exp env inhandle) in 
									(match expr with 
									| MPAIR (a, b) -> !a
									| _ -> raise (RUNTIME_EXCEPTION "MCAR must accept mpair"))
		| MCDR exp -> let expr = (myeval_aux exp env inhandle) in 
									(match expr with 
									| MPAIR (a, b) -> !b
									| _ -> raise (RUNTIME_EXCEPTION "MCDR must accept mpair"))
   	        | SETMCAR (exp1, exp2) -> let expr = (myeval_aux exp1 env inhandle) in 
															(match expr with 
															| MPAIR (a, b) -> let _ = a := (myeval_aux exp2 env inhandle) in VOID
															| _ -> raise (RUNTIME_EXCEPTION "SETMCAR must accept pair"))
                | SETMCDR (exp1, exp2) -> let expr = (myeval_aux exp1 env inhandle) in 
															(match expr with 
															| MPAIR (a, b) -> let _ = b := (myeval_aux exp2 env inhandle) in VOID
															| _ -> raise (RUNTIME_EXCEPTION "SETMCDR must accept pair"))
		| RAISE exp -> (if (inhandle > 0) then raise (EXCEPTION_RAISED (myeval_aux exp env inhandle)) else raise UNCAUGHT_EXCEPTION)

		| HANDLERS(hdlist, exp) -> try (myeval_aux exp env (inhandle + 1)) 
						with (EXCEPTION_RAISED raisedexcep) -> 
							let rec excephandler lst expr = (match lst with 
							| [] -> raise (if inhandle = 0 then UNCAUGHT_EXCEPTION else (EXCEPTION_RAISED raisedexcep))
							| hd::tl ->(if (let lam = (myeval_aux (fst hd) env inhandle) in	(match lam with 
								| CLOS (varlist, ex, restoredenv) -> 
								(if (List.length varlist) = 1 then let a = List.fold_left2 (fun x y z -> insert_to_env x y z) restoredenv varlist [raisedexcep] in myeval_aux ex a inhandle else raise (RUNTIME_EXCEPTION "var = exp"))
								| _ -> raise (RUNTIME_EXCEPTION "HANDLER must have lambda in 1st place"))) = (BOOL false) 
								    then excephandler tl expr else (let lam = (myeval_aux (snd hd) env inhandle) in (match lam with 
												| CLOS (varlist, ex, restoredenv) -> 
												(if (List.length varlist) = 1 
												then let b = List.fold_left2 (fun x y z -> insert_to_env x y z) restoredenv varlist [raisedexcep] in myeval_aux ex b inhandle else raise (RUNTIME_EXCEPTION "var = exp"))
												| _ -> raise (RUNTIME_EXCEPTION "HANDLER must have lambda in 2nd place")))))
							in excephandler hdlist raisedexcep

 in myeval_aux exp emptyenv 0 

let myeval_memo (exp_string: string): value_t =
  let lexbuf = Lexing.from_string exp_string in
  let lexer () = Lexer.token lexbuf in
  let exp = Parser.parse lexer in
  let emptyenv = [] in
  let hashtbl = Hashtbl.create 1000 in  
  let rec myeval_aux (exp: Syntax.exp_t) (env : (var_t*value_t) ref list) (inhandle: int) (memtbl: (((var_t * value_t list), value_t) Hashtbl.t)): value_t =
		match exp with 
		| CONST a -> (match a with 
								| CINT a -> INT a
								| CTRUE -> BOOL true
								| CFALSE -> BOOL false
								| CNULL -> NULL)
		| VAR x -> if has_key env x then (let a = key_to_val env x in if a = UNDEF then raise (RUNTIME_EXCEPTION "undefined") else a) 
						else raise (RUNTIME_EXCEPTION "var must be assigned before applying")
		| IF(exp1, exp2, exp3) -> let exp = myeval_aux exp1 env inhandle memtbl in 
								(match exp with 
								| BOOL a -> if a == false then (myeval_aux exp3 env inhandle memtbl) else (myeval_aux exp2 env inhandle memtbl)
								| _ -> raise (RUNTIME_EXCEPTION "IF must accept boolean value as 1st assignment"))

		| CONS(exp1, exp2) -> PAIR ((myeval_aux exp1 env inhandle memtbl), (myeval_aux exp2 env inhandle memtbl))
		| CAR exp -> let expr = (myeval_aux exp env inhandle memtbl) in 
									(match expr with 
									| PAIR (a, b) -> a
									| _ -> raise (RUNTIME_EXCEPTION "CAR must accept pair"))
		| CDR exp ->  let expr = (myeval_aux exp env inhandle memtbl) in 
									(match expr with 
									| PAIR (a, b) -> b
									| _ -> raise (RUNTIME_EXCEPTION "CAR must accept pair"))
		| LAMBDA (varlist, exp) -> let rec dup_checker lst = match lst with 
					| [] -> false 
					| hd::tl -> (if List.mem hd tl then true else dup_checker tl) in if dup_checker varlist then raise (RUNTIME_EXCEPTION "duplicate identifier") else CLOS (varlist, exp, env)
		| APP (exp, explist) -> (match exp with 
					| VAR x -> let valist = List.map (fun x -> myeval_aux x env inhandle memtbl) explist in 
						   let hashkey = (x, valist) in 
						   (if Hashtbl.mem memtbl hashkey then Hashtbl.find memtbl hashkey else
							let expr = myeval_aux exp env inhandle memtbl in 
							(match expr with
							| CLOS (varlist, ex, restoreden) -> 
								(if (List.length varlist) = (List.length explist) then 
								let a = List.fold_left2 (fun x y z -> insert_to_env x y (myeval_aux z env inhandle memtbl)) restoreden varlist explist 
									in let b = myeval_aux ex a inhandle memtbl in 
									let _ = Hashtbl.add memtbl hashkey b in 
									b
									 else raise (RUNTIME_EXCEPTION "var = exp"))
							| _ -> raise (RUNTIME_EXCEPTION "APP must have lambda at 1st place")))
					| _ -> 	let expr = myeval_aux exp env inhandle memtbl in 
						(match expr with 
						| CLOS (varlist, ex, restoreden) ->
							(if (List.length varlist) = (List.length explist) then 
							let a = List.fold_left2 (fun x y z -> insert_to_env x y (myeval_aux z env inhandle memtbl)) restoreden varlist explist 												in myeval_aux ex a inhandle memtbl else raise (RUNTIME_EXCEPTION "var = exp"))							
						| _ -> raise (RUNTIME_EXCEPTION "APP must have lambda at 1st place")))
		
		| LET (bindlist, exp) -> (if dup_check bindlist then raise (RUNTIME_EXCEPTION "duplicate identifier") else 
					let a = List.fold_left (fun x y -> insert_to_env x (fst y) (myeval_aux (snd y) env inhandle memtbl)) env bindlist in myeval_aux exp a inhandle memtbl)
		| LETREC (bindlist, exp) ->(if dup_check bindlist then raise (RUNTIME_EXCEPTION "duplicate identifier") else 
					 let dummyenv = dummy_env env bindlist in
					 let a = List.fold_left (fun x y -> insert_to_envmem x (fst y) (myeval_aux (snd y) x inhandle memtbl)) dummyenv bindlist in myeval_aux exp a inhandle memtbl)
		| ADD (exp1, exp2) -> let result1 = (myeval_aux exp1 env inhandle memtbl) in
													let result2 = (myeval_aux exp2 env inhandle memtbl) in 
													(match result1 with
													| INT a -> (match result2 with
																			| INT b -> INT (a + b)
																			| _ -> raise (RUNTIME_EXCEPTION "ADD must have int"))
													| _ -> raise (RUNTIME_EXCEPTION "ADD must have pair"))
		| SUB (exp1, exp2) -> let result1 = (myeval_aux exp1 env inhandle memtbl) in
													let result2 = (myeval_aux exp2 env inhandle memtbl) in 
													(match result1 with
													| INT a -> (match result2 with
																			| INT b -> INT (a - b)
																			| _ -> raise (RUNTIME_EXCEPTION "SUB must have int"))
													| _ ->	raise (RUNTIME_EXCEPTION "SUB must have pair"))
		| MUL (exp1, exp2) -> let result1 = (myeval_aux exp1 env inhandle memtbl) in
										let result2 = (myeval_aux exp2 env inhandle memtbl) in 
										(match result1 with
										| INT a -> (match result2 with
											| INT b -> INT (a * b)
											| _ -> raise (RUNTIME_EXCEPTION "MUL must have int"))
										| _ ->	raise (RUNTIME_EXCEPTION "MUL must have pair"))

		| EQ (exp1, exp2) -> let result1 = (myeval_aux exp1 env inhandle memtbl) in
													let result2 = (myeval_aux exp2 env inhandle memtbl) in 
													(match result1 with
													| INT a ->( match result2 with
																			| INT b -> BOOL (a = b)
																			| _ -> raise (RUNTIME_EXCEPTION "EQ must have int"))
													| _ ->	raise (RUNTIME_EXCEPTION "EQ must have pair"))
		| LT (exp1, exp2) -> let result1 = (myeval_aux exp1 env inhandle memtbl) in
													let result2 = (myeval_aux exp2 env inhandle memtbl) in 
													(match result1 with
													| INT a -> (match result2 with
																			| INT b -> BOOL (a < b)
																			| _ -> raise (RUNTIME_EXCEPTION "LT must have int"))
													| _ ->	raise (RUNTIME_EXCEPTION "LT must have pair"))
		| GT (exp1, exp2) -> let result1 = (myeval_aux exp1 env inhandle memtbl) in
													let result2 = (myeval_aux exp2 env inhandle memtbl) in 
													(match result1 with
													| INT a -> (match result2 with
																			| INT b -> BOOL (a > b)
																			| _ -> raise (RUNTIME_EXCEPTION "GT must have int"))
													| _ ->	raise (RUNTIME_EXCEPTION "GT must have pair"))
		| MCONS (exp1, exp2) ->  let refa = ref (myeval_aux exp1 env inhandle memtbl) in 
														 let refb = ref (myeval_aux exp2 env inhandle memtbl) in MPAIR (refa, refb)
 	  	| MCAR exp -> let expr = (myeval_aux exp env inhandle memtbl) in 
									(match expr with 
									| MPAIR (a, b) -> !a
									| _ -> raise (RUNTIME_EXCEPTION "MCAR must accept mpair"))
		| MCDR exp -> let expr = (myeval_aux exp env inhandle memtbl) in 
									(match expr with 
									| MPAIR (a, b) -> !b
									| _ -> raise (RUNTIME_EXCEPTION "MCDR must accept mpair"))
   	        | SETMCAR (exp1, exp2) -> let expr = (myeval_aux exp1 env inhandle memtbl) in 
															(match expr with 
															| MPAIR (a, b) -> let _ = a := (myeval_aux exp2 env inhandle memtbl) in VOID
															| _ -> raise (RUNTIME_EXCEPTION "SETMCAR must accept pair"))
                | SETMCDR (exp1, exp2) -> let expr = (myeval_aux exp1 env inhandle memtbl) in 
															(match expr with 
															| MPAIR (a, b) -> let _ = b := (myeval_aux exp2 env inhandle memtbl) in VOID
															| _ -> raise (RUNTIME_EXCEPTION "SETMCDR must accept pair"))
		| RAISE exp -> (if (inhandle > 0) then raise (EXCEPTION_RAISED (myeval_aux exp env inhandle memtbl)) else raise UNCAUGHT_EXCEPTION)

		| HANDLERS(hdlist, exp) -> try (myeval_aux exp env (inhandle + 1) memtbl) 
						with (EXCEPTION_RAISED raisedexcep) -> 
							let rec excephandler lst expr = (match lst with 
							| [] -> raise (if inhandle = 0 then UNCAUGHT_EXCEPTION else (EXCEPTION_RAISED raisedexcep))
							| hd::tl ->(if (let lam = (myeval_aux (fst hd) env inhandle memtbl) in	(match lam with 
								| CLOS (varlist, ex, restoredenv) -> 
								(if (List.length varlist) = 1 then let a = List.fold_left2 (fun x y z -> insert_to_env x y z) restoredenv varlist [raisedexcep] in myeval_aux ex a inhandle memtbl else raise (RUNTIME_EXCEPTION "var = exp"))
								| _ -> raise (RUNTIME_EXCEPTION "HANDLER must have lambda in 1st place"))) = (BOOL false) 
								    then excephandler tl expr else (let lam = (myeval_aux (snd hd) env inhandle memtbl) in (match lam with 
												| CLOS (varlist, ex, restoredenv) -> 
												(if (List.length varlist) = 1 
												then let b = List.fold_left2 (fun x y z -> insert_to_env x y z) restoredenv varlist [raisedexcep] in myeval_aux ex b inhandle memtbl else raise (RUNTIME_EXCEPTION "var = exp"))
												| _ -> raise (RUNTIME_EXCEPTION "HANDLER must have lambda in 2nd place")))))
							in excephandler hdlist raisedexcep

 in myeval_aux exp emptyenv 0 hashtbl 
