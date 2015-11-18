(* A list of Goldbach compositions. (medium)

Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000. *)
(*
	# goldbach_list 9 20;;
	- : (int * (int * int)) list =
	[(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
	 (20, (3, 17))]
	# goldbach_limit 1 2000 50;;
	- : (int * (int * int)) list =
	[(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
	 (1928, (61, 1867))]
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

let goldbach_sieve s ps n =
	let is_prime s p = if p < 0 then false else Array.get s p in
	let rec aux s ps n = match ps with
		| [] -> failwith "goldbach"
		| p :: ps when p > n -> failwith "goldbach"
		| p :: ps when is_prime s (n-p) -> (p, n-p)
		| p :: ps -> ((*print_int p; print_newline();*) aux s ps n) in
	match n with
		| n when n <= 2 -> raise (Invalid_argument "n not in domain")
		| n when n mod 2 = 1 -> raise (Invalid_argument "n not in domain")
		| n -> aux s ps n;;

let goldbach n =
	let s = sieve n in
	let ps = primes_from_sieve s 0 n in
	goldbach_sieve s ps n;;

let goldbach_list n m =
	let rec aux s ps acc n m = match m with
		| m when n > m -> List.rev acc
		| m -> aux s ps ((n, goldbach_sieve s ps n)::acc) (n+2) m in
	let n = if n mod 2 = 0 then n else n + 1 in
	let n = if n < 4 then 4 else n in
	let s = sieve m in
	let ps = primes_from_sieve s 0 m in
	aux s ps [] n m;;

let goldbach_limit n m limit =
	List.filter (fun (_, (x, _)) -> x > limit) (goldbach_list n m);;
