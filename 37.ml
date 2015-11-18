(*A list of prime numbers. (easy)

Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.*)
(*
	# List.length (all_primes 2 7920);;
	- : int = 1000
*)

let sieve n =
	let arr = Array.make (n+1) true in
	Array.set arr 0 false;
	Array.set arr 1 false;
	let rec mult_to_false arr x0 m n =
		if x0 > n then ()
		else (Array.set arr x0 false; mult_to_false arr (x0+m) m n) in
	let rec aux arr x n m =
		if x > n then ()
		else if Array.get arr x then (mult_to_false arr (x*2) x m; aux arr (x+1) n m)
		else aux arr (x+1) n m in
	aux arr 2 (n/2) n;
	arr;;

let primes_from_sieve s min max =
	let s = Array.sub s min (max-min) in
	let i = min in
	List.rev (snd (Array.fold_left (fun (n, l) x -> if x then (n+1, n::l) else (n+1, l)) (i, []) s));;

let all_primes min max =
	let s = sieve max in
	primes_from_sieve s min max;;

List.length (all_primes 2 7920);;