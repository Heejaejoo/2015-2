open Syntax

exception RUNTIME_EXCEPTION of string
exception NOT_IMPLEMENTED
exception UNCAUGHT_EXCEPTION

(* this is for testing the lexer and the parser *)

type value_t = 
  | INT of int
  | BOOL of bool
  | NULL
  | CLOS of (var_t list * exp_t * ((var_t, value_t) Hashtbl.t)) (* closure = lambda value *)
  | CLOS_MEM of (var_t list * exp_t * ((var_t, value_t) Hashtbl.t)) (* closure of memoizing lambda value *)
  | PAIR of value_t * value_t
  | MPAIR of value_t * value_t
  | VOID	       

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

let myeval (exp_string: string): value_t =
  let lexbuf = Lexing.from_string exp_string in
  let lexer () = Lexer.token lexbuf in
  let exp = Parser.parse lexer in
	let hash = Hashtbl.create 1000 in 
  let rec myeval_aux (exp: Syntax.exp_t) (env: ('a, 'b) t): value_t =
		match exp with 
		| CONST a -> (match a with 
								| CINT a -> a
								| CTRUE -> BOOL true
								| CFALSE -> BOOL false
								| CNULL -> NULL)
		| VAR x -> if Hashtbl.mem env x then 
								Hashtbl.find env x else raise (RUNTIME_EXCEPTION "var must be assigned before applying")
		| IF(exp1, exp2, exp3) -> let exp = myeval_aux exp1 env in 
																match exp with 
																| BOOL a -> if a == false then (myeval_aux exp3 env) else (myeval_aux exp2 env)
																| _ -> raise (RUNTIME_EXCEPTION "IF must accept boolean value as 1st assignment")
		| CONS(exp1, exp2) -> PAIR ((myeval_aux exp1 env), (myeval_aux exp2 env))
		| CAR exp -> let expr = (myeval_aux exp env) in 
									(match expr with 
									| PAIR (a, b) -> a
									| _ -> raise (RUNTIME_EXCEPTION "CAR must accept pair"))
		| CDR exp ->  let expr = (myeval_aux exp env) in 
									(match expr with 
									| PAIR (a, b) -> b
									| _ -> raise (RUNTIME_EXCEPTION "CAR must accept pair"))
		| LAMBDA (varlist, exp) -> CLOS (varlist, exp, env)
		| APP (exp, explist) -> let expr = myeval_aux exp env in 
														match expr with 
														| CLOS (varlist, ex, restoreden) -> 
															let _ = List.iter2 (fun x y -> Hashtbl.add restoreden x (myeval_aux y env)) varlist explist in 
															myeval_aux ex restoreden													
														| _ -> raise (RUNTIME_EXCEPTION "APP must have lambda at 1st place")
		| LET (bindlist, exp) -> let _ = List.iter (fun x -> Hashtbl.add env (fst x) (myeval_aux (snd x) env)) bindlist in 
														myeval_aux exp env
		| LETREC (bindlist, exp) -> let _ = List.iter (fun x -> Hashtbl.add env (fst x) (myeval_aux (snd x) env)) bindlist in 
															myeval_aux exp env
		| ADD (exp1, exp2) -> let result1 = (myeval_aux exp1 env) in
													let result2 = (myeval_aux exp2 env) in 
													match result1 with
													| INT a -> match result2 with
																			| INT b -> INT (a + b)
																			| _ -> (RUNTIME_EXCEPTION "ADD must have int")
													| _ ->	(RUNTIME_EXCEPTION "CAR must have pair")
		| SUB (exp1, exp2) -> let result1 = (myeval_aux exp1 env) in
													let result2 = (myeval_aux exp2 env) in 
													match result1 with
													| INT a -> match result2 with
																			| INT b -> INT (a + b)
																			| _ -> (RUNTIME_EXCEPTION "ADD must have int")
													| _ ->	(RUNTIME_EXCEPTION "CAR must have pair")
		| MUL (exp1, exp2) -> let result1 = (myeval_aux exp1 env) in
													let result2 = (myeval_aux exp2 env) in 
													match result1 with
													| INT a -> match result2 with
																			| INT b -> INT (a * b)
																			| _ -> (RUNTIME_EXCEPTION "ADD must have int")
													| _ ->	(RUNTIME_EXCEPTION "CAR must have pair")

		| EQ (exp1, exp2) -> let result1 = (myeval_aux exp1 env) in
													let result2 = (myeval_aux exp2 env) in 
													match result1 with
													| INT a -> match result2 with
																			| INT b -> BOOL (a == b)
																			| _ -> (RUNTIME_EXCEPTION "ADD must have int")
													| _ ->	(RUNTIME_EXCEPTION "CAR must have pair")
		| LT (exp1, exp2) -> let result1 = (myeval_aux exp1 env) in
													let result2 = (myeval_aux exp2 env) in 
													match result1 with
													| INT a -> match result2 with
																			| INT b -> BOOL (a < b)
																			| _ -> (RUNTIME_EXCEPTION "ADD must have int")
													| _ ->	(RUNTIME_EXCEPTION "CAR must have pair")
		| GT (exp1, exp2) -> let result1 = (myeval_aux exp1 env) in
													let result2 = (myeval_aux exp2 env) in 
													match result1 with
													| INT a -> match result2 with
																			| INT b -> BOOL (a > b)
																			| _ -> (RUNTIME_EXCEPTION "ADD must have int")
													| _ ->	(RUNTIME_EXCEPTION "CAR must have pair")
 in myeval_aux exp hash
let myeval_memo (exp_string: string): value_t =
  raise (RUNTIME_EXCEPTION "CAR must have pair")

(* test like this:
let exp1 = "(if #t (car (cons 1 2)) 4)"
let v = myeval exp1
let _ = print_endline (value_to_string v)
 *)
