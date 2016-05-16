module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyq: queue
  val enq: queue * element -> queue
  val deq: queue -> element * queue
end

module StringQ : Queue with type element = string = 
struct 
	exception EMPTY_Q
  type element = string
	type queue = string list * string list
 	let emptyq = ([], [])
	let enq qelem = 
		match qelem with 
		| ((a, b), c) -> (c::a, b)
	let deq q = 
		match q with
		| ([], []) -> raise EMPTY_Q
		| (a, []) -> let arev = List.rev a in 
									(List.hd arev, ([], List.tl arev))
		| (a, b) -> (List.hd b, (a, List.tl b))
end

module StringQQ : Queue with type element = StringQ.queue = 
struct 
  exception EMPTY_Q
	type element = StringQ.queue
	type queue = StringQ.queue list * StringQ.queue list
	let emptyq = ([], [])
		let enq qelem = 
		match qelem with 
		| ((a, b), c) -> (c::a, b)
	let deq q = 
		match q with
		| ([], []) -> raise EMPTY_Q
		| (a, []) -> let arev = List.rev a in 
									(List.hd arev, ([], List.tl arev))
		| (a, b) -> (List.hd b, (a, List.tl b))
end
