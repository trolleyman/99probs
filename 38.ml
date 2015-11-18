(* Goldbach's conjecture. (medium)

Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers. Write a function to find the two prime numbers that sum up to a given even integer. *)
(*
	# goldbach 28;;
	- : int * int = (5, 23)
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
		| p :: ps when is_prime s (n-p) -> (p, n-p)
		| p :: ps -> ((*print_int p; print_newline();*) aux s ps n) in
	match n with
		| n when n <= 2 -> raise Invalid_argument "n not in domain"
		| n when n mod 2 = 1 -> raise Invalid_argument "n not in domain"
		| n -> aux s ps n;;

let goldbach n =
	let s = sieve n in
	let ps = primes_from_sieve s 0 n in
	goldbach_sieve s ps n;;

let test_goldbach x0 x1 =
	let rec aux s ps x0 x1 max_a max_p =
		match x0 with
			| x0 when x0 > x1 -> (max_a, max_p)
			| x0 ->
				let (a, b) = goldbach_sieve s ps x0 in
				(*let _ = print_int x0; print_string " = "; print_int a; print_string " + "; print_int b; print_newline() in*)
				let (max_a, max_p) = if a > max_a then (a, x0) else (max_a, max_p) in
				aux s ps (x0+2) x1 max_a max_p in
	let _ = print_string "Calculating sieve... "; flush stdout in
	let s = sieve (x1+1) in
	let _ = print_endline "Done." in
	let _ = print_string "Caclulating primes from sieve... "; flush stdout in
	let ps = primes_from_sieve s 0 (x1+1) in
	let _ = print_endline "Done." in
	let (max_a, max_p) = aux s ps x0 x1 0 0 in
	let _ = print_string "a: "; print_int max_a; print_string ", p: "; print_int max_p; print_newline() in ();;

goldbach 28;;
