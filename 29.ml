(* Determine whether a given integer number is prime. (medium) *)
(*
	# is_prime 1;;
	- : bool = false
	# is_prime 7;;
	- : bool = true
	# is_prime 12;;
	- : bool = false
	# is_prime 17;;
	- : bool = true
*)

let is_prime n =
	let rec is_divisible_by_any f t n =
		if f > t then false
		else if (n mod f) = 0 then true
		else is_divisible_by_any (f+1) t n in
	let rec aux = function
		| _ when n < 1 -> failwith "is_prime"
		| 1 -> false
		| 2 -> true
		| 3 -> true
		| n -> not (is_divisible_by_any 2 (n/2) n) in
	aux n;;

is_prime 1;;
is_prime 2;;
is_prime 3;;
is_prime 4;;
is_prime 7;;
is_prime 12;;
is_prime 17;;
is_prime 97;;
is_prime 101;;
is_prime 100000;;