module type SKI = sig
  type liquid =
    | S
    | K
    | I
    | V of string (* varible *)
    | M of liquid * liquid (* mix of two liquids *)
  val react: liquid -> liquid
  val pprint: liquid -> string
end

module SkiLiquid : SKI = struct
  exception ETODO

  type liquid =
    | S
    | K
    | I
    | V of string (* varible *)
    | M of liquid * liquid (* mix of two liquids *)

  let rec pprint: liquid -> string =
    fun l -> match l with 
					| S -> "S"
					| K -> "K"
					| I -> "I"
					| V a -> a
					| M(a, b) -> "(" ^ (pprint a) ^ " " ^ (pprint b) ^ ")"

let rec react : liquid -> liquid = 
  let rec react_h: liquid -> liquid =
    let isS: liquid -> bool = 
			fun k -> match k with 
			| M(sol1, sol2) -> (match sol1 with 
													| M(sol11, sol12) -> (match sol11 with 
																								| M (sol21, sol22) -> (match sol21 with
																																			|S -> true
																																			|_ -> false)
																								| _ -> false)
													| _ -> false)
			| _ -> false
			in 
			let isK: liquid -> bool = 
				fun p -> match p with 
				| M(sol1, sol2) -> (match sol1 with 
														| M(sol11, sol12) -> (match sol11 with
																									| K -> true
																									| _ -> false)			
														| _ -> false)
				| _ -> false
				in 
				let isI: liquid -> bool =
					fun i -> match i with
					| M(sol1, sol2) -> (match sol1 with 
															| I -> true
															| _ -> false)					
					| _ -> false
					in
					fun l -> if isS l then (match l with 
																	| M(M(M(sol1, sol2), sol3), sol4) -> react_h (M(M(sol2, sol4),M(sol3, sol4)))
																	| _ -> l)
									 else if isK l then (match l with 
																	| M(M(sol1, sol2), sol3) -> react_h sol2
																	| _ -> l)
									 else if isI l then (match l with 
																	| M(sol1, sol2) -> react_h sol2
																	| _ -> l)
									 else match l with 
												| M(sol1, sol2) -> M((react_h sol1), (react_h  sol2))
												| _ -> l
		in fun l -> if pprint (react_h l) = pprint (react_h (react_h l)) then (react_h l)
						else react (react_h l) 
end
